{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, try)
import Control.Monad (filterM, forM_)
import Data.ByteString qualified as ByteString
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Monoid (Last (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
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
    VerificationIssue (PendingMigration),
    VerificationReport (VerificationReport),
    connectionProviderFromSettings,
    defaultImportOptions,
    defaultRunOptions,
    migrationComponentFromEmbeddedSql,
    migrationId,
    migrationPlan,
    runMigrationPlan,
    verifyMigrationPlan,
    withEquivalentHistory,
  )
import Database.PostgreSQL.Migrate.History.HasqlMigration
  ( HasqlMigrationHistory (unselectedRows),
    HasqlMigrationImportError (..),
    HasqlMigrationRow (filename),
    HasqlMigrationSourceConfig,
    defaultHasqlMigrationTable,
    hasqlMigrationSourceConfig,
    importHasqlMigrationHistory,
    readHasqlMigrationHistory,
  )
import Database.PostgreSQL.Migrate.Internal
  ( ComponentDescription (..),
    PlanDescription (..),
    componentNameText,
    migrationIdName,
    migrationNameText,
    planDescription,
  )
-- MigrationDescription's 'migrationId' field would shadow the smart constructor
-- of the same name imported above, so it is reached through this alias only.
import Database.PostgreSQL.Migrate.Internal qualified as MigrateInternal
import EphemeralPg
  ( Config (temporaryRoot),
    connectionSettings,
    defaultCacheConfig,
    defaultConfig,
    withCachedConfig,
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
    SourceLedgerPolicy (..),
    pgmqHasqlMigrationMappings,
    pgmqHasqlMigrationSourceConfig,
    pgmqHasqlMigrationSourceConfigWithPolicy,
  )
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv)
import System.Posix.User (getEffectiveUserID)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

-- | Root directory for ephemeral PostgreSQL clusters.
--
-- ephemeral-pg reaps abandoned clusters at startup, but only within its own
-- temporary root. With 'temporaryRoot' unset that root is @$TMPDIR@, which
-- @nix develop@ makes unique per shell, so a run never reclaims what an earlier
-- session abandoned. Pinning one root across sessions keeps them reachable.
--
-- The path is keyed by effective uid. It is a fixed location created @0700@, so
-- a run under a different user -- the Nix build sandbox -- must not collide with
-- a directory it cannot write to.
ephemeralRoot :: IO FilePath
ephemeralRoot = do
  uid <- getEffectiveUserID
  pure ("/tmp/ephpg-pgmq-hs-" <> show uid)

-- | Cached-startup configuration pinned to 'ephemeralRoot'.
ephemeralConfig :: IO Config
ephemeralConfig = do
  root <- ephemeralRoot
  createDirectoryIfMissing True root
  pure defaultConfig {temporaryRoot = Last (Just root)}

main :: IO ()
main = do
  config <- ephemeralConfig
  result <- withCachedConfig config defaultCacheConfig $ \db -> do
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
        [ testCase "immutable history and upstream payload provenance" (testNativePayload conn),
          testCase "component pgmq lists the ledger in order and has no dependencies" testNativeComponent
        ],
      testGroup
        "native runner"
        [ testCase "fresh install applies once and is idempotent" (testNativeRunner settings conn),
          testCase "1.12 and 1.13 catalog convergence" (testConvergence conn),
          testCase "populated partition upgrades and spill recovery" (testPartitions settings conn)
        ],
      testGroup
        "history import"
        [ testCase "strict helper rejects a shared ledger before target writes" (testStrictSharedLedger settings conn),
          testCase "shared-ledger policy imports only PGMQ and applies the canary once" (testSharedLedgerImport settings conn),
          testCase "direct row imports without executing the target action" (testDirectHistoryImport settings conn),
          testCase "direct import rejects altered bytes, checksum, and duplicate rows" (testDirectHistoryRejections settings conn),
          testCase "two-step history requires explicit equivalent opt-in" (testEquivalentHistoryImport settings conn),
          testCase "two-step history rejects an incomplete PGMQ contract" (testEquivalentContractRejections settings conn)
        ]
    ]

testNativePayload :: Connection.Connection -> IO ()
testNativePayload conn = do
  forM_
    [ ("0001-install-v1.11.0.sql", "faa9b8800005f80fbdf6a33d071183d2"),
      ("0002-schema-management-comment.sql", "f3010777d47ec6d4f7d92a179bcf09b8"),
      ("0003-notify-crash-safety-and-locking.sql", "e0a66c96bd8811607440dbca0cb302ea")
    ]
    $ \(name, digest) -> do
      bytes <- readMigration name
      result <- Connection.use conn (Session.statement (Text.decodeUtf8 bytes) (preparable "SELECT md5($1)" (Encoders.param (Encoders.nonNullable Encoders.text)) (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))))
      either (assertFailure . show) (@?= digest) result
  first <- readVendor "pgmq--1.11.0--1.11.1.sql"
  second <- readVendor "pgmq--1.11.1--1.12.0.sql"
  readMigration "0004-upgrade-v1.12.0.sql" >>= (@?= first <> "\n" <> second)
  upstream <- readVendor "pgmq--1.12.0--1.13.0.sql"
  readMigration "0005-upgrade-v1.13.0.sql" >>= (@?= upstream)
  fixture <- Text.decodeUtf8 <$> readFixture "pgmq-1.12.0.sql"
  digest <- Connection.use conn (Session.statement fixture (preparable "SELECT encode(sha256(convert_to($1,'UTF8')),'hex')" (Encoders.param (Encoders.nonNullable Encoders.text)) (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))))
  either (assertFailure . show) (@?= "be087bfcb0ec5e65abb76610249750f2ec8dc757956125a1b40430ce95fc7f0f") digest

readMigration :: FilePath -> IO ByteString.ByteString
readMigration name = findFile ["pgmq-migration/migrations/" <> name, "migrations/" <> name] >>= ByteString.readFile

readVendor :: FilePath -> IO ByteString.ByteString
readVendor name = findFile ["vendor/pgmq/pgmq-extension/sql/" <> name, "../vendor/pgmq/pgmq-extension/sql/" <> name] >>= ByteString.readFile

readFixture :: FilePath -> IO ByteString.ByteString
readFixture name = findFile ["pgmq-migration/test/fixtures/" <> name, "test/fixtures/" <> name] >>= ByteString.readFile

testConvergence :: Connection.Connection -> IO ()
testConvergence conn = forM_ [False, True] $ \latest -> do
  resetDb conn
  names <- nativeMigrationNames
  forM_ (take (if latest then 6 else 4) names) $ \name ->
    readMigration (T.unpack name <> ".sql") >>= runSql conn . Text.decodeUtf8
  upgraded <- schemaSnapshot conn
  resetDb conn
  -- mori://pgmq/pgmq tag v1.12.0, 08ace4087dbf00e51704c5a3d9df2e15fd566127.
  -- pgmq-extension/sql/pgmq.sql; artifact URI pending. SHA256:
  -- be087bfcb0ec5e65abb76610249750f2ec8dc757956125a1b40430ce95fc7f0f
  freshSql <- if latest then readVendor "pgmq.sql" else readFixture "pgmq-1.12.0.sql"
  runSql conn (Text.decodeUtf8 freshSql)
  fresh <- schemaSnapshot conn
  let exceptions =
        [ "body:notify_queue_listeners()",
          "body:enable_notify_insert(text, integer)",
          if latest then "body:create_partitioned(text, text, text, integer)" else "body:create_partitioned(text, text, text)"
        ]
      normalized snapshot = foldr Map.delete snapshot exceptions
      differences a b =
        Map.keys (Map.differenceWith (\x y -> if x == y then Nothing else Just x) a b)
          <> Map.keys (Map.difference b a)
      actual = normalized upgraded
      expected = normalized fresh
  forM_ exceptions $ \key -> assertBool ("missing local override " <> T.unpack key) (Map.member key upgraded && Map.member key fresh)
  differences actual expected @?= []
  let grouped = "body:read_grouped_head(text, integer, integer)"
  assertBool "snapshot comparison detects missing grouped function" (grouped `elem` differences (Map.delete grouped actual) expected)
  assertBool "snapshot comparison detects altered body" (grouped `elem` differences (Map.insert grouped "changed" actual) expected)
  if latest
    then do
      let attribute = "column:metrics_result:8"
      assertBool "eighth metrics attribute exists" (Map.member attribute actual)
      assertBool "snapshot comparison detects altered metric" (attribute `elem` differences (Map.insert attribute "wrong type" actual) expected)
    else pure ()

schemaSnapshot :: Connection.Connection -> IO (Map.Map Text Text)
schemaSnapshot conn = do
  sql <- Text.decodeUtf8 <$> readFixture "schema-snapshot.sql"
  result <-
    Connection.use
      conn
      ( Session.statement
          ()
          ( preparable
              sql
              Encoders.noParams
              (Decoders.rowList ((,) <$> Decoders.column (Decoders.nonNullable Decoders.text) <*> Decoders.column (Decoders.nonNullable Decoders.text)))
          )
      )
  either (\err -> assertFailure (show err) >> pure Map.empty) (pure . Map.fromList) result

queryText :: Connection.Connection -> Text -> IO Text
queryText conn sql = do
  result <-
    Connection.use
      conn
      ( Session.statement
          ()
          ( Statement.unpreparable
              sql
              Encoders.noParams
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))
          )
      )
  either (\err -> assertFailure (show err) >> pure "") pure result

assertSql :: Connection.Connection -> Text -> IO ()
assertSql conn predicate = queryText conn ("SELECT (" <> predicate <> ")::text") >>= (@?= "true")

-- Install a prefix through the real ledger so populated upgrades also exercise
-- checksums, suffix selection and AlreadyApplied, not just raw SQL replay.
installPrefix :: Settings.Settings -> Int -> IO ()
installPrefix settings count = do
  names <- take count <$> nativeMigrationNames
  entries <-
    traverse
      ( \name -> do
          let file = T.unpack name <> ".sql"
          bytes <- readMigration file
          pure (file, bytes)
      )
      names
  case entries of
    [] -> assertFailure "empty migration prefix"
    first : rest -> do
      component <- either (assertFailure . show) pure (migrationComponentFromEmbeddedSql "pgmq" mempty (first :| rest))
      plan <- either (assertFailure . show) pure (migrationPlan (component :| []))
      result <- runMigrationPlan defaultRunOptions settings plan
      either (assertFailure . show) (const (pure ())) result

testPartitions :: Settings.Settings -> Connection.Connection -> IO ()
testPartitions settings conn = do
  required <- (== Just "1") <$> lookupEnv "PGMQ_REQUIRE_PARTMAN"
  available <- queryText conn "SELECT EXISTS (SELECT 1 FROM pg_available_extensions WHERE name='pg_partman')::text"
  if available /= "true"
    then
      if required
        then assertFailure "PGMQ_REQUIRE_PARTMAN=1 but pg_partman is unavailable"
        else putStrLn "SKIPPED: partition upgrade/recovery requires pg_partman"
    else do
      runSql conn "CREATE SCHEMA IF NOT EXISTS partman; CREATE EXTENSION IF NOT EXISTS pg_partman SCHEMA partman"
      version <- queryText conn "SELECT current_setting('server_version') || ' / pg_partman ' || extversion FROM pg_extension WHERE extname='pg_partman'"
      putStrLn ("Partition test versions: " <> T.unpack version)
      forM_ [3, 4] $ \prefix -> do
        resetDb conn
        -- Each reset discards registrations as well as fixture-owned tables.
        runSql conn "DROP EXTENSION pg_partman CASCADE; CREATE EXTENSION pg_partman SCHEMA partman"
        installPrefix settings prefix
        runSql conn "SELECT pgmq.create_partitioned('numeric_old','10','100'); SELECT pgmq.create_partitioned('time_old','1 day','30 days'); SELECT pgmq.create('ordinary_old')"
        runSql conn "SELECT pgmq.send('numeric_old',jsonb_build_object('n',n)) FROM generate_series(1,20) n; SELECT pgmq.send('time_old','{\"keep\":true}'); SELECT pgmq.send('ordinary_old','{\"keep\":true}'); SELECT pgmq.archive('numeric_old',1::bigint)"
        before <- queryText conn preservedDataSql
        installPrefix settings 6
        after <- queryText conn preservedDataSql
        after @?= before
        assertSql conn "(SELECT count(*)=2 FROM information_schema.columns WHERE table_schema='pgmq' AND table_name IN ('q_numeric_old','q_time_old') AND column_name='msg_id' AND identity_generation='BY DEFAULT')"
        assertSql conn "(SELECT identity_generation='ALWAYS' FROM information_schema.columns WHERE table_schema='pgmq' AND table_name='q_ordinary_old' AND column_name='msg_id')"
        assertSql conn "(SELECT is_identity='NO' FROM information_schema.columns WHERE table_schema='pgmq' AND table_name='a_numeric_old' AND column_name='msg_id')"
        installPrefix settings 6
        runSql conn "SELECT pgmq.create_partitioned('defaults','10','100'); SELECT pgmq.create_partitioned('spill','10','100',2); SELECT pgmq.create_partitioned('spill','10','100',2)"
        schema <- queryText conn "SELECT pgmq._get_pg_partman_schema()"
        let qualified = "\"" <> T.replace "\"" "\"\"" schema <> "\"."
        assertSql conn ("(SELECT count(*)=2 AND bool_and(premake=4) FROM " <> qualified <> "part_config WHERE parent_table IN ('pgmq.q_defaults','pgmq.a_defaults'))")
        assertSql conn ("(SELECT count(*)=2 AND bool_and(premake=2) FROM " <> qualified <> "part_config WHERE parent_table IN ('pgmq.q_spill','pgmq.a_spill'))")
        forM_ ["0", "-1"] $ \invalid -> do
          result <- Connection.use conn (Session.script ("SELECT pgmq.create_partitioned('invalid','10','100'," <> invalid <> ")"))
          case result of
            Left _ -> pure ()
            Right _ -> assertFailure "invalid premake succeeded"
          assertSql conn "to_regclass('pgmq.q_invalid') IS NULL AND NOT EXISTS (SELECT 1 FROM pgmq.meta WHERE queue_name='invalid')"
        -- Use independent connections; advisory locking plus guards must let both finish.
        let createConcurrent = do
              connection <- Connection.acquire settings >>= either (error . show) pure
              result <- try (runSql connection "SELECT pgmq.create_partitioned('concurrent','10','100',2)") :: IO (Either SomeException ())
              Connection.release connection
              pure result
        box <- newEmptyMVar
        _ <- forkIO (createConcurrent >>= putMVar box)
        right <- createConcurrent
        left <- takeMVar box
        forM_ [left, right] (either (assertFailure . show) pure)
        runSql conn ("UPDATE " <> qualified <> "part_config SET automatic_maintenance='off' WHERE parent_table LIKE 'pgmq.%'")
        runSql conn "SELECT pgmq.send('spill',jsonb_build_object('n',n)) FROM generate_series(1,100) n; SELECT pgmq.archive('spill',100::bigint); ANALYZE pgmq.q_spill_default; ANALYZE pgmq.a_spill_default"
        assertSql conn "(SELECT count(*)>0 FROM pgmq.q_spill_default) AND (SELECT count(*)=1 FROM pgmq.a_spill_default)"
        assertSql conn "(SELECT default_partition_length FROM pgmq.metrics('spill')) = (SELECT count(*) FROM pgmq.q_spill_default) + (SELECT count(*) FROM pgmq.a_spill_default)"
        assertSql conn "(SELECT default_partition_length IS NULL FROM pgmq.metrics('ordinary_old'))"
        runSql conn "SELECT pgmq.create_unlogged('unlogged');"
        assertSql conn "(SELECT default_partition_length IS NULL FROM pgmq.metrics('unlogged'))"
        spillBefore <- queryText conn spillDataSql
        -- Each CALL is its own simple-query request: the procedure commits internally.
        forM_ ["q_spill", "a_spill"] $ \parent ->
          runSql conn ("CALL " <> qualified <> "partition_data_proc('pgmq." <> parent <> "', p_wait := 0)")
        runSql conn ("SELECT " <> qualified <> "run_maintenance('pgmq.q_spill'); SELECT " <> qualified <> "run_maintenance('pgmq.a_spill')")
        runSql conn "ANALYZE pgmq.q_spill_default; ANALYZE pgmq.a_spill_default"
        queryText conn spillDataSql >>= (@?= spillBefore)
        assertSql conn "(SELECT count(*)=0 FROM pgmq.q_spill_default) AND (SELECT count(*)=0 FROM pgmq.a_spill_default) AND (SELECT default_partition_length=0 FROM pgmq.metrics('spill'))"
      resetDb conn
      runSql conn "DROP EXTENSION pg_partman CASCADE"

preservedDataSql :: Text
preservedDataSql = "SELECT jsonb_agg(to_jsonb(m) ORDER BY source,msg_id)::text FROM (SELECT 'numeric' source,msg_id,message FROM pgmq.q_numeric_old UNION ALL SELECT 'archive',msg_id,message FROM pgmq.a_numeric_old UNION ALL SELECT 'time',msg_id,message FROM pgmq.q_time_old UNION ALL SELECT 'ordinary',msg_id,message FROM pgmq.q_ordinary_old) m"

spillDataSql :: Text
spillDataSql = "SELECT jsonb_agg(to_jsonb(m) ORDER BY source,msg_id)::text FROM (SELECT 'queue' source,msg_id,message FROM pgmq.q_spill UNION ALL SELECT 'archive',msg_id,message FROM pgmq.a_spill) m"

-- | The one place the ledger is spelled out. Every other expectation in this
-- suite derives from 'nativeMigrationNames', so appending a migration means
-- reviewing this list and nothing else.
testNativeComponent :: IO ()
testNativeComponent = do
  component <- either (assertFailure . show) pure Migration.pgmqMigrations
  plan <- either (assertFailure . show) pure (migrationPlan (component :| []))
  let PlanDescription components = planDescription plan
  case toList components of
    [ComponentDescription {name, dependencies, migrations}] -> do
      componentNameText name @?= "pgmq"
      dependencies @?= mempty
      migrationNames migrations
        @?= [ "0001-install-v1.11.0",
              "0002-schema-management-comment",
              "0003-notify-crash-safety-and-locking",
              "0004-upgrade-v1.12.0",
              "0005-upgrade-v1.13.0",
              "0006-preserve-partitioned-reentry-v1.13.0"
            ]
    actual -> assertFailure ("unexpected native PGMQ plan: " <> show actual)

migrationNames :: (Foldable f) => f MigrateInternal.MigrationDescription -> [Text]
migrationNames descriptions =
  [ migrationNameText (migrationIdName (MigrateInternal.migrationId description))
  | description <- toList descriptions
  ]

-- | Migration names in the native PGMQ ledger, in manifest order.
nativeMigrationNames :: IO [Text]
nativeMigrationNames = do
  component <- either (assertFailure . show) pure Migration.pgmqMigrations
  plan <- either (assertFailure . show) pure (migrationPlan (component :| []))
  let PlanDescription components = planDescription plan
  case toList components of
    [ComponentDescription {migrations}] -> pure (migrationNames migrations)
    actual -> assertFailure ("unexpected native PGMQ plan: " <> show actual)

-- | Every migration a history import leaves unapplied. An import records the
-- immutable baseline only, so everything after it is still pending.
pendingAfterBaseline :: IO [VerificationIssue]
pendingAfterBaseline = do
  names <- nativeMigrationNames
  traverse pendingIssue (drop 1 names)
  where
    pendingIssue name =
      PendingMigration <$> either (assertFailure . show) pure (migrationId "pgmq" name)

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
  ledgerLength <- length <$> nativeMigrationNames
  first <- runMigrationPlan defaultRunOptions settings plan
  case first of
    Left err -> assertFailure ("fresh native migration failed: " <> show err)
    Right report -> (outcome <$> toList (results report)) @?= replicate ledgerLength AppliedNow
  second <- runMigrationPlan defaultRunOptions settings plan
  case second of
    Left err -> assertFailure ("repeated native migration failed: " <> show err)
    Right report -> (outcome <$> toList (results report)) @?= replicate ledgerLength AlreadyApplied
  functionExists c "pgmq.metrics_all()" >>= (@?= True)
  hasCanaryComment c >>= (@?= True)

testStrictSharedLedger :: Settings.Settings -> Connection.Connection -> IO ()
testStrictSharedLedger settings conn = withCleanDb conn $ \c -> do
  prepareSharedDirectHistory c
  runPolicyImportEither settings defaultImportOptions DirectFullInstallHistory
    >>= assertImportError
      (\case HasqlMigrationStrictSourceHasUnselected [name] -> name == unrelatedLegacyFilename; _ -> False)
  nativeLedgerTablesAbsent c >>= (@?= True)

testSharedLedgerImport :: Settings.Settings -> Connection.Connection -> IO ()
testSharedLedgerImport settings conn = withCleanDb conn $ \c -> do
  prepareSharedDirectHistory c
  runSql c "DROP FUNCTION pgmq.metrics_all()"
  let provider = connectionProviderFromSettings settings
  config <-
    either (assertFailure . show) pure $
      pgmqHasqlMigrationSourceConfigWithPolicy
        provider
        DirectFullInstallHistory
        AllowUnselectedSourceRows
  history <- readHasqlMigrationHistory config >>= either (assertFailure . show) pure
  (filename <$> unselectedRows history) @?= [unrelatedLegacyFilename]
  sourceBefore <- legacyLedgerSnapshot c

  first <- runImportWith settings defaultImportOptions DirectFullInstallHistory config >>= either (assertFailure . show) pure
  historyOutcomes first @?= [Imported]
  sourceAfter <- legacyLedgerSnapshot c
  sourceAfter @?= sourceBefore
  functionExists c "pgmq.metrics_all()" >>= (@?= False)

  assertNativeCanaryLifecycle settings c

testDirectHistoryImport :: Settings.Settings -> Connection.Connection -> IO ()
testDirectHistoryImport settings conn = withCleanDb conn $ \c -> do
  prepareDirectHistory c
  runSql c "DROP FUNCTION pgmq.metrics_all()"

  first <- runPolicyImport settings defaultImportOptions DirectFullInstallHistory
  historyOutcomes first @?= [Imported]
  functionExists c "pgmq.metrics_all()" >>= (@?= False)

  second <- runPolicyImport settings defaultImportOptions DirectFullInstallHistory
  historyOutcomes second @?= [AlreadyImported]

  plan <- nativePlan
  pending <- pendingAfterBaseline
  ledgerLength <- length <$> nativeMigrationNames
  beforeCanary <- verifyMigrationPlan defaultRunOptions settings plan
  case beforeCanary of
    Left err -> assertFailure ("native verify failed after direct import: " <> show err)
    Right (VerificationReport issues _ _ _) -> issues @?= pending
  nativeRun <- runMigrationPlan defaultRunOptions settings plan
  case nativeRun of
    Left err -> assertFailure ("native runner failed after direct import: " <> show err)
    Right report ->
      (outcome <$> toList (results report)) @?= AlreadyApplied : replicate (ledgerLength - 1) AppliedNow
  afterCanary <- verifyMigrationPlan defaultRunOptions settings plan
  case afterCanary of
    Left err -> assertFailure ("native verify failed after direct canary: " <> show err)
    Right (VerificationReport issues _ _ _) -> issues @?= []
  repeated <- runMigrationPlan defaultRunOptions settings plan
  case repeated of
    Left err -> assertFailure ("native rerun failed after direct canary: " <> show err)
    Right report -> (outcome <$> toList (results report)) @?= replicate ledgerLength AlreadyApplied
  functionExists c "pgmq.metrics_all()" >>= (@?= False)
  hasCanaryComment c >>= (@?= True)

testDirectHistoryRejections :: Settings.Settings -> Connection.Connection -> IO ()
testDirectHistoryRejections settings conn = do
  withCleanDb conn $ \c -> do
    prepareDirectHistory c
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
    prepareDirectHistory c
    runSql c "UPDATE public.schema_migrations SET checksum = 'altered' WHERE filename = 'pgmq_v1.11.0'"
    runPolicyImportEither settings defaultImportOptions DirectFullInstallHistory
      >>= assertImportError (\case HasqlMigrationChecksumMismatch name "altered" _ -> name == directLegacyFilename; _ -> False)

  withCleanDb conn $ \c -> do
    prepareDirectHistory c
    runSql c "INSERT INTO public.schema_migrations SELECT * FROM public.schema_migrations WHERE filename = 'pgmq_v1.11.0'"
    runPolicyImportEither settings defaultImportOptions DirectFullInstallHistory
      >>= assertImportError (\case HasqlMigrationDuplicateLedgerFilename name -> name == directLegacyFilename; _ -> False)

  withCleanDb conn $ \c -> do
    prepareSharedDirectHistory c
    runSql c "UPDATE public.schema_migrations SET checksum = 'altered' WHERE filename = 'pgmq_v1.11.0'"
    runPolicyImportWithSourcePolicyEither
      settings
      defaultImportOptions
      DirectFullInstallHistory
      AllowUnselectedSourceRows
      >>= assertImportError (\case HasqlMigrationChecksumMismatch name "altered" _ -> name == directLegacyFilename; _ -> False)
    nativeLedgerTablesAbsent c >>= (@?= True)

testEquivalentHistoryImport :: Settings.Settings -> Connection.Connection -> IO ()
testEquivalentHistoryImport settings conn = withCleanDb conn $ \c -> do
  prepareSharedTwoStepHistory c
  runPolicyImportWithSourcePolicyEither
    settings
    defaultImportOptions
    EquivalentTwoStepUpgradeHistory
    AllowUnselectedSourceRows
    >>= assertImportError
      ( \case
          HasqlMigrationTargetImportFailed (HistoryImportValidationFailed (HistoryEquivalentStateDisallowed _)) -> True
          _ -> False
      )

  first <- runPolicyImportWithSourcePolicy settings equivalentImportOptions EquivalentTwoStepUpgradeHistory AllowUnselectedSourceRows
  historyOutcomes first @?= [Imported]
  second <- runPolicyImportWithSourcePolicy settings equivalentImportOptions EquivalentTwoStepUpgradeHistory AllowUnselectedSourceRows
  historyOutcomes second @?= [AlreadyImported]

  plan <- nativePlan
  pending <- pendingAfterBaseline
  ledgerLength <- length <$> nativeMigrationNames
  beforeCanary <- verifyMigrationPlan defaultRunOptions settings plan
  case beforeCanary of
    Left err -> assertFailure ("native verify failed after equivalent import: " <> show err)
    Right (VerificationReport issues _ _ _) -> issues @?= pending
  nativeRun <- runMigrationPlan defaultRunOptions settings plan
  case nativeRun of
    Left err -> assertFailure ("native runner failed after equivalent import: " <> show err)
    Right report ->
      (outcome <$> toList (results report)) @?= AlreadyApplied : replicate (ledgerLength - 1) AppliedNow
  afterCanary <- verifyMigrationPlan defaultRunOptions settings plan
  case afterCanary of
    Left err -> assertFailure ("native verify failed after equivalent canary: " <> show err)
    Right (VerificationReport issues _ _ _) -> issues @?= []
  repeated <- runMigrationPlan defaultRunOptions settings plan
  case repeated of
    Left err -> assertFailure ("native rerun failed after equivalent canary: " <> show err)
    Right report -> (outcome <$> toList (results report)) @?= replicate ledgerLength AlreadyApplied
  hasCanaryComment c >>= (@?= True)

testEquivalentContractRejections :: Settings.Settings -> Connection.Connection -> IO ()
testEquivalentContractRejections settings conn =
  forM_ destructiveChanges $ \sql ->
    withCleanDb conn $ \c -> do
      prepareTwoStepHistory c
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

runPolicyImportWithSourcePolicy ::
  Settings.Settings ->
  ImportOptions ->
  AlternativeHistoryPolicy ->
  SourceLedgerPolicy ->
  IO HistoryImportReport
runPolicyImportWithSourcePolicy settings options policy sourceLedgerPolicy =
  runPolicyImportWithSourcePolicyEither settings options policy sourceLedgerPolicy
    >>= either (assertFailure . show) pure

runPolicyImportWithSourcePolicyEither ::
  Settings.Settings ->
  ImportOptions ->
  AlternativeHistoryPolicy ->
  SourceLedgerPolicy ->
  IO (Either HasqlMigrationImportError HistoryImportReport)
runPolicyImportWithSourcePolicyEither settings options policy sourceLedgerPolicy = do
  let provider = connectionProviderFromSettings settings
  config <-
    either (assertFailure . show) pure $
      pgmqHasqlMigrationSourceConfigWithPolicy provider policy sourceLedgerPolicy
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

prepareDirectHistory :: Connection.Connection -> IO ()
prepareDirectHistory connection = do
  installHistoricalSchema connection
  runSql connection legacyLedgerDefinition
  runSql
    connection
    "INSERT INTO public.schema_migrations (filename, checksum) VALUES ('pgmq_v1.11.0', '+qm4gAAF+A+99qM9BxGD0g==')"

prepareSharedDirectHistory :: Connection.Connection -> IO ()
prepareSharedDirectHistory connection = do
  prepareDirectHistory connection
  insertUnrelatedHistory connection

prepareTwoStepHistory :: Connection.Connection -> IO ()
prepareTwoStepHistory connection = do
  installHistoricalSchema connection
  runSql connection legacyLedgerDefinition
  runSql
    connection
    ( "INSERT INTO public.schema_migrations (filename, checksum) VALUES "
        <> "('pgmq_v1.10.0_to_v1.10.1', 'C56QJtvtxB2pGcEHR82LFA=='), "
        <> "('pgmq_v1.10.1_to_v1.11.0', 'KMM7gGjkepkD1YA1hUCpEQ==')"
    )

prepareSharedTwoStepHistory :: Connection.Connection -> IO ()
prepareSharedTwoStepHistory connection = do
  prepareTwoStepHistory connection
  insertUnrelatedHistory connection

unrelatedLegacyFilename :: FilePath
unrelatedLegacyFilename = "application_0001.sql"

insertUnrelatedHistory :: Connection.Connection -> IO ()
insertUnrelatedHistory connection =
  runSql
    connection
    "INSERT INTO public.schema_migrations (filename, checksum) VALUES ('application_0001.sql', 'application-checksum')"

installHistoricalSchema :: Connection.Connection -> IO ()
installHistoricalSchema connection = do
  path <- findFile ["pgmq-migration/migrations/0001-install-v1.11.0.sql", "migrations/0001-install-v1.11.0.sql"]
  payload <- ByteString.readFile path
  runSql connection (Text.decodeUtf8 payload)

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

legacyLedgerSnapshot :: Connection.Connection -> IO [(Text, Text)]
legacyLedgerSnapshot connection = do
  result <- Connection.use connection (Session.statement () legacyLedgerSnapshotStatement)
  case result of
    Left err -> assertFailure ("legacy ledger inspection failed: " <> show err) >> pure []
    Right rows -> pure rows

legacyLedgerSnapshotStatement :: Statement.Statement () [(Text, Text)]
legacyLedgerSnapshotStatement =
  preparable
    "SELECT filename, checksum FROM public.schema_migrations ORDER BY filename"
    Encoders.noParams
    ( Decoders.rowList
        ( (,)
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
        )
    )

nativeLedgerTablesAbsent :: Connection.Connection -> IO Bool
nativeLedgerTablesAbsent connection = do
  result <- Connection.use connection (Session.statement () nativeLedgerTablesAbsentStatement)
  case result of
    Left err -> assertFailure ("native ledger inspection failed: " <> show err) >> pure False
    Right absent -> pure absent

nativeLedgerTablesAbsentStatement :: Statement.Statement () Bool
nativeLedgerTablesAbsentStatement =
  preparable
    "SELECT to_regclass('pgmigrate.migrations') IS NULL AND to_regclass('pgmigrate.history_imports') IS NULL"
    Encoders.noParams
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))

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

hasCanaryComment :: Connection.Connection -> IO Bool
hasCanaryComment connection = do
  result <- Connection.use connection (Session.statement () canaryCommentStatement)
  case result of
    Left err -> assertFailure ("schema comment inspection failed: " <> show err) >> pure False
    Right matches -> pure matches

assertNativeCanaryLifecycle :: Settings.Settings -> Connection.Connection -> IO ()
assertNativeCanaryLifecycle settings connection = do
  plan <- nativePlan
  pending <- pendingAfterBaseline
  ledgerLength <- length <$> nativeMigrationNames
  beforeCanary <- verifyMigrationPlan defaultRunOptions settings plan
  case beforeCanary of
    Left err -> assertFailure ("native verify failed after shared-ledger import: " <> show err)
    Right (VerificationReport issues _ _ _) -> issues @?= pending
  nativeRun <- runMigrationPlan defaultRunOptions settings plan
  case nativeRun of
    Left err -> assertFailure ("native runner failed after shared-ledger import: " <> show err)
    Right report ->
      (outcome <$> toList (results report)) @?= AlreadyApplied : replicate (ledgerLength - 1) AppliedNow
  repeated <- runMigrationPlan defaultRunOptions settings plan
  case repeated of
    Left err -> assertFailure ("native rerun failed after shared-ledger import: " <> show err)
    Right report -> (outcome <$> toList (results report)) @?= replicate ledgerLength AlreadyApplied
  hasCanaryComment connection >>= (@?= True)
  assertSql connection "to_regprocedure('pgmq.read_grouped_head(text,integer,integer)') IS NOT NULL AND to_regprocedure('pgmq.read_grouped_head_with_poll(text,integer,integer,integer,integer)') IS NOT NULL AND to_regprocedure('pgmq.create_partitioned(text,text,text)') IS NULL AND to_regprocedure('pgmq.create_partitioned(text,text,text,integer)') IS NOT NULL"
  assertSql connection "(SELECT count(*)=8 FROM pg_attribute WHERE attrelid='pgmq.metrics_result'::regclass AND attnum>0 AND NOT attisdropped)"

canaryCommentStatement :: Statement.Statement () Bool
canaryCommentStatement =
  preparable
    "SELECT obj_description(to_regnamespace('pgmq'), 'pg_namespace') = 'Managed by pg-migrate component pgmq through 0002-schema-management-comment'"
    Encoders.noParams
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
