{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (filterM, forM_)
import Data.ByteString qualified as ByteString
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Database.PostgreSQL.Migrate
  ( EquivalentHistoryPolicy (AllowEquivalentHistory),
    HistoryImportError (..),
    HistoryImportOutcome (AlreadyImported, Imported),
    HistoryImportReport (HistoryImportReport, importResults),
    HistoryImportResult (importOutcome),
    HistoryValidationError (..),
    ImportOptions,
    MigrationOutcome (AlreadyApplied, AppliedNow),
    MigrationPlan,
    MigrationReport (results),
    MigrationResult (outcome),
    connectionProviderFromSettings,
    defaultImportOptions,
    defaultRunOptions,
    migrationPlan,
    runMigrationPlan,
    withEquivalentHistory,
  )
import Database.PostgreSQL.Migrate.History.HasqlMigration
  ( HasqlMigrationImportError (..),
    HasqlMigrationSourceConfig,
    defaultHasqlMigrationTable,
    hasqlMigrationSourceConfig,
    importHasqlMigrationHistory,
  )
import Database.PostgreSQL.Migrate.Internal
  ( ComponentDescription (..),
    PlanDescription (..),
    componentNameText,
    planDescription,
  )
import EphemeralPg
  ( connectionSettings,
    withCached,
  )
import Hasql.Connection qualified as Connection
import Hasql.Connection.Settings qualified as Settings
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session (Session)
import Hasql.Session qualified as Session
import Hasql.Statement (preparable)
import Hasql.Statement qualified as Statement
import Pgmq.Migration qualified as Migration
import Pgmq.Migration.History.HasqlMigration
  ( AlternativeHistoryPolicy (..),
    pgmqHasqlMigrationMappings,
    pgmqHasqlMigrationSourceConfig,
  )
import System.Directory (doesFileExist)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

main :: IO ()
main = do
  result <- withCached $ \db -> do
    let connSettings = connectionSettings db
    connResult <- Connection.acquire connSettings
    case connResult of
      Left err -> error $ "Failed to connect: " <> show err
      Right conn ->
        defaultMain (tests connSettings conn)
  case result of
    Left startErr -> error $ "Failed to start temp database: " <> show startErr
    Right () -> pure ()

tests :: Settings.Settings -> Connection.Connection -> TestTree
tests settings conn =
  testGroup
    "pgmq-migration"
    [ testGroup
        "native definition"
        [ testCase "baseline bytes equal vendored pgmq.sql" testNativePayload,
          testCase "component pgmq has one migration and no dependencies" testNativeComponent
        ],
      testGroup
        "native runner"
        [ testCase "fresh install applies once and is idempotent" (testNativeRunner settings conn)
        ],
      testGroup
        "history import"
        [ testCase "direct row imports without executing the target action" (testDirectHistoryImport settings conn),
          testCase "direct import rejects altered bytes, checksum, and duplicate rows" (testDirectHistoryRejections settings conn),
          testCase "two-step history requires explicit equivalent opt-in" (testEquivalentHistoryImport settings conn),
          testCase "two-step history rejects an incomplete PGMQ contract" (testEquivalentContractRejections settings conn)
        ]
    ]

testNativePayload :: IO ()
testNativePayload = do
  nativePath <- findFile ["pgmq-migration/migrations/0001-install-v1.11.0.sql", "migrations/0001-install-v1.11.0.sql"]
  vendorPath <- findFile ["vendor/pgmq/pgmq-extension/sql/pgmq.sql", "../vendor/pgmq/pgmq-extension/sql/pgmq.sql"]
  native <- ByteString.readFile nativePath
  vendored <- ByteString.readFile vendorPath
  native @?= vendored

testNativeComponent :: IO ()
testNativeComponent = do
  component <- either (assertFailure . show) pure Migration.pgmqMigrations
  plan <- either (assertFailure . show) pure (migrationPlan (component :| []))
  let PlanDescription components = planDescription plan
  case toList components of
    [ComponentDescription {name, dependencies, migrations}] -> do
      componentNameText name @?= "pgmq"
      dependencies @?= mempty
      length migrations @?= 1
    actual -> assertFailure ("unexpected native PGMQ plan: " <> show actual)

findFile :: [FilePath] -> IO FilePath
findFile candidates = do
  existing <- filterM doesFileExist candidates
  case existing of
    path : _ -> pure path
    [] -> assertFailure ("could not find any of: " <> show candidates) >> pure "."

-- | Reset the database to a clean state by dropping the pgmq schema
-- and migration tracking table
resetDb :: Connection.Connection -> IO ()
resetDb conn = do
  resetResult <- Connection.use conn resetSession
  case resetResult of
    Left err -> error $ "Failed to reset database: " <> show err
    Right () -> pure ()
  where
    resetSession :: Session ()
    resetSession = do
      Session.statement () dropPgmqSchema
      Session.statement () dropMigrationTable
      Session.statement () dropNativeMigrationSchema

    dropPgmqSchema :: Statement.Statement () ()
    dropPgmqSchema =
      preparable
        "DROP SCHEMA IF EXISTS pgmq CASCADE"
        Encoders.noParams
        Decoders.noResult

    dropMigrationTable :: Statement.Statement () ()
    dropMigrationTable =
      preparable
        "DROP TABLE IF EXISTS public.schema_migrations"
        Encoders.noParams
        Decoders.noResult

    dropNativeMigrationSchema :: Statement.Statement () ()
    dropNativeMigrationSchema =
      preparable
        "DROP SCHEMA IF EXISTS pgmigrate CASCADE"
        Encoders.noParams
        Decoders.noResult

withCleanDb :: Connection.Connection -> (Connection.Connection -> IO ()) -> IO ()
withCleanDb conn action = do
  resetDb conn
  action conn

testNativeRunner :: Settings.Settings -> Connection.Connection -> IO ()
testNativeRunner settings conn = withCleanDb conn $ \c -> do
  plan <- nativePlan
  first <- runMigrationPlan defaultRunOptions settings plan
  case first of
    Left err -> assertFailure ("fresh native migration failed: " <> show err)
    Right report -> (outcome <$> toList (results report)) @?= [AppliedNow]
  second <- runMigrationPlan defaultRunOptions settings plan
  case second of
    Left err -> assertFailure ("repeated native migration failed: " <> show err)
    Right report -> (outcome <$> toList (results report)) @?= [AlreadyApplied]
  functionExists c "pgmq.metrics_all()" >>= (@?= True)

testDirectHistoryImport :: Settings.Settings -> Connection.Connection -> IO ()
testDirectHistoryImport settings conn = withCleanDb conn $ \c -> do
  prepareDirectHistory settings c
  runSql c "DROP FUNCTION pgmq.metrics_all()"

  first <- runPolicyImport settings defaultImportOptions DirectFullInstallHistory
  historyOutcomes first @?= [Imported]
  functionExists c "pgmq.metrics_all()" >>= (@?= False)

  second <- runPolicyImport settings defaultImportOptions DirectFullInstallHistory
  historyOutcomes second @?= [AlreadyImported]

  plan <- nativePlan
  nativeRun <- runMigrationPlan defaultRunOptions settings plan
  case nativeRun of
    Left err -> assertFailure ("native runner failed after direct import: " <> show err)
    Right report -> (outcome <$> toList (results report)) @?= [AlreadyApplied]
  functionExists c "pgmq.metrics_all()" >>= (@?= False)

testDirectHistoryRejections :: Settings.Settings -> Connection.Connection -> IO ()
testDirectHistoryRejections settings conn = do
  withCleanDb conn $ \c -> do
    prepareDirectHistory settings c
    nativePath <- findFile ["pgmq-migration/migrations/0001-install-v1.11.0.sql", "migrations/0001-install-v1.11.0.sql"]
    payload <- (<> "\n-- altered") <$> ByteString.readFile nativePath
    let provider = connectionProviderFromSettings settings
    config <-
      either (assertFailure . show) pure $
        hasqlMigrationSourceConfig
          provider
          defaultHasqlMigrationTable
          (directLegacyFilename :| [])
          True
          (Map.singleton directLegacyFilename payload)
          []
          "test altered direct PGMQ payload"
    runImportWith settings defaultImportOptions DirectFullInstallHistory config
      >>= assertImportError (\case HasqlMigrationChecksumMismatch name _ _ -> name == directLegacyFilename; _ -> False)

  withCleanDb conn $ \c -> do
    prepareDirectHistory settings c
    runSql c "UPDATE public.schema_migrations SET checksum = 'altered' WHERE filename = 'pgmq_v1.11.0'"
    runPolicyImportEither settings defaultImportOptions DirectFullInstallHistory
      >>= assertImportError (\case HasqlMigrationChecksumMismatch name "altered" _ -> name == directLegacyFilename; _ -> False)

  withCleanDb conn $ \c -> do
    prepareDirectHistory settings c
    runSql c "INSERT INTO public.schema_migrations SELECT * FROM public.schema_migrations WHERE filename = 'pgmq_v1.11.0'"
    runPolicyImportEither settings defaultImportOptions DirectFullInstallHistory
      >>= assertImportError (\case HasqlMigrationDuplicateLedgerFilename name -> name == directLegacyFilename; _ -> False)

testEquivalentHistoryImport :: Settings.Settings -> Connection.Connection -> IO ()
testEquivalentHistoryImport settings conn = withCleanDb conn $ \c -> do
  prepareTwoStepHistory settings c
  runPolicyImportEither settings defaultImportOptions EquivalentTwoStepUpgradeHistory
    >>= assertImportError
      ( \case
          HasqlMigrationTargetImportFailed (HistoryImportValidationFailed (HistoryEquivalentStateDisallowed _)) -> True
          _ -> False
      )

  first <- runPolicyImport settings equivalentImportOptions EquivalentTwoStepUpgradeHistory
  historyOutcomes first @?= [Imported]
  second <- runPolicyImport settings equivalentImportOptions EquivalentTwoStepUpgradeHistory
  historyOutcomes second @?= [AlreadyImported]

  plan <- nativePlan
  nativeRun <- runMigrationPlan defaultRunOptions settings plan
  case nativeRun of
    Left err -> assertFailure ("native runner failed after equivalent import: " <> show err)
    Right report -> (outcome <$> toList (results report)) @?= [AlreadyApplied]

testEquivalentContractRejections :: Settings.Settings -> Connection.Connection -> IO ()
testEquivalentContractRejections settings conn =
  forM_ destructiveChanges $ \sql ->
    withCleanDb conn $ \c -> do
      prepareTwoStepHistory settings c
      runSql c sql
      runPolicyImportEither settings equivalentImportOptions EquivalentTwoStepUpgradeHistory
        >>= assertImportError
          ( \case
              HasqlMigrationTargetImportFailed (HistoryStateValidationFailed _ _) -> True
              _ -> False
          )
  where
    destructiveChanges =
      [ "DROP FUNCTION pgmq.send_topic(text,jsonb)",
        "DROP TYPE pgmq.metrics_result CASCADE",
        "DROP TABLE pgmq.topic_bindings CASCADE"
      ]

equivalentImportOptions :: ImportOptions
equivalentImportOptions =
  withEquivalentHistory AllowEquivalentHistory defaultImportOptions

directLegacyFilename :: FilePath
directLegacyFilename = "pgmq_v1.11.0"

nativePlan :: IO MigrationPlan
nativePlan = do
  component <- either (assertFailure . show) pure Migration.pgmqMigrations
  either (assertFailure . show) pure (migrationPlan (component :| []))

runPolicyImport ::
  Settings.Settings ->
  ImportOptions ->
  AlternativeHistoryPolicy ->
  IO HistoryImportReport
runPolicyImport settings options policy =
  runPolicyImportEither settings options policy >>= either (assertFailure . show) pure

runPolicyImportEither ::
  Settings.Settings ->
  ImportOptions ->
  AlternativeHistoryPolicy ->
  IO (Either HasqlMigrationImportError HistoryImportReport)
runPolicyImportEither settings options policy = do
  let provider = connectionProviderFromSettings settings
  config <- either (assertFailure . show) pure (pgmqHasqlMigrationSourceConfig provider policy)
  runImportWith settings options policy config

runImportWith ::
  Settings.Settings ->
  ImportOptions ->
  AlternativeHistoryPolicy ->
  HasqlMigrationSourceConfig ->
  IO (Either HasqlMigrationImportError HistoryImportReport)
runImportWith settings options policy config = do
  mappings <- either (assertFailure . show) pure (pgmqHasqlMigrationMappings policy)
  plan <- nativePlan
  let provider = connectionProviderFromSettings settings
  importHasqlMigrationHistory options config provider plan mappings

historyOutcomes :: HistoryImportReport -> [HistoryImportOutcome]
historyOutcomes HistoryImportReport {importResults} = importOutcome <$> toList importResults

assertImportError ::
  (HasqlMigrationImportError -> Bool) ->
  Either HasqlMigrationImportError HistoryImportReport ->
  IO ()
assertImportError predicate actual =
  case actual of
    Left err | predicate err -> pure ()
    Left err -> assertFailure ("unexpected history import error: " <> show err)
    Right report -> assertFailure ("expected history import failure, received: " <> show report)

prepareDirectHistory :: Settings.Settings -> Connection.Connection -> IO ()
prepareDirectHistory settings connection = do
  installNativeFixture settings
  runSql connection "DROP SCHEMA pgmigrate CASCADE"
  runSql connection legacyLedgerDefinition
  runSql
    connection
    "INSERT INTO public.schema_migrations (filename, checksum) VALUES ('pgmq_v1.11.0', '+qm4gAAF+A+99qM9BxGD0g==')"

prepareTwoStepHistory :: Settings.Settings -> Connection.Connection -> IO ()
prepareTwoStepHistory settings connection = do
  installNativeFixture settings
  runSql connection "DROP SCHEMA pgmigrate CASCADE"
  runSql connection legacyLedgerDefinition
  runSql
    connection
    ( "INSERT INTO public.schema_migrations (filename, checksum) VALUES "
        <> "('pgmq_v1.10.0_to_v1.10.1', 'C56QJtvtxB2pGcEHR82LFA=='), "
        <> "('pgmq_v1.10.1_to_v1.11.0', 'KMM7gGjkepkD1YA1hUCpEQ==')"
    )

installNativeFixture :: Settings.Settings -> IO ()
installNativeFixture settings = do
  plan <- nativePlan
  result <- runMigrationPlan defaultRunOptions settings plan
  case result of
    Left err -> assertFailure ("native fixture installation failed: " <> show err)
    Right _ -> pure ()

legacyLedgerDefinition :: Text
legacyLedgerDefinition =
  "CREATE TABLE public.schema_migrations "
    <> "(filename text NOT NULL, checksum text NOT NULL, "
    <> "executed_at timestamp without time zone NOT NULL DEFAULT now())"

runSql :: Connection.Connection -> Text -> IO ()
runSql connection sql = do
  result <- Connection.use connection (Session.script sql)
  case result of
    Left err -> assertFailure ("SQL fixture failed: " <> show err)
    Right () -> pure ()

functionExists :: Connection.Connection -> Text -> IO Bool
functionExists connection identity = do
  result <- Connection.use connection (Session.statement identity functionExistsStatement)
  case result of
    Left err -> assertFailure ("function inspection failed: " <> show err) >> pure False
    Right exists -> pure exists

functionExistsStatement :: Statement.Statement Text Bool
functionExistsStatement =
  preparable
    "SELECT pg_catalog.to_regprocedure($1) IS NOT NULL"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
