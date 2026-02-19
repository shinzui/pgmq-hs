{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (foldM)
import Data.List (isInfixOf)
import Data.Text.Encoding qualified as TE
import Database.Postgres.Temp
  ( StartError,
    toConnectionString,
    with,
  )
import Hasql.Connection qualified as Connection
import Hasql.Connection.Settings qualified as Settings
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Migration (MigrationCommand, MigrationError)
import Hasql.Session (Session)
import Hasql.Session qualified as Session
import Hasql.Statement (preparable)
import Hasql.Statement qualified as Statement
import Pgmq.Migration qualified as Migration
import Pgmq.Migration.Migrations.V1_9_0 qualified as V1_9_0
import Pgmq.Migration.Sessions qualified as Sessions
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "pgmq-migration"
    [ testGroup
        "fresh install"
        [ testCase "migrate on fresh database succeeds" testMigrateFresh,
          testCase "migrate is idempotent" testMigrateIdempotent,
          testCase "getMigrations returns applied migrations" testGetMigrations,
          testCase "version is v1.10.0" testVersion
        ],
      testGroup
        "upgrade"
        [ testCase "upgrade from v1.9.0 succeeds" testUpgradeFromV1_9_0,
          testCase "upgrade is idempotent" testUpgradeIdempotent,
          testCase "upgrade adds last_read_at column" testUpgradeAddsLastReadAt
        ]
    ]

-- | Helper to run a test with a temporary PostgreSQL database
withTempDb :: (Connection.Connection -> IO a) -> IO (Either StartError a)
withTempDb action = with $ \db -> do
  let connStr = toConnectionString db
      connSettings = Settings.connectionString (TE.decodeUtf8 connStr)
  connResult <- Connection.acquire connSettings
  case connResult of
    Left err -> error $ "Failed to connect: " <> show err
    Right conn -> action conn

testMigrateFresh :: IO ()
testMigrateFresh = do
  result <- withTempDb $ \conn -> do
    migResult <- Connection.use conn Migration.migrate
    case migResult of
      Left sessionErr -> assertFailure $ "Session error: " <> show sessionErr
      Right (Left migrationErr) -> assertFailure $ "Migration error: " <> show migrationErr
      Right (Right ()) -> pure ()
  case result of
    Left startErr -> assertFailure $ "Failed to start temp database: " <> show startErr
    Right () -> pure ()

testMigrateIdempotent :: IO ()
testMigrateIdempotent = do
  result <- withTempDb $ \conn -> do
    -- Run migration first time
    result1 <- Connection.use conn Migration.migrate
    case result1 of
      Left sessionErr -> assertFailure $ "First migration session error: " <> show sessionErr
      Right (Left migrationErr) -> assertFailure $ "First migration error: " <> show migrationErr
      Right (Right ()) -> pure ()

    -- Run migration second time - should succeed without error
    result2 <- Connection.use conn Migration.migrate
    case result2 of
      Left sessionErr -> assertFailure $ "Second migration session error: " <> show sessionErr
      Right (Left migrationErr) -> assertFailure $ "Second migration error: " <> show migrationErr
      Right (Right ()) -> pure ()
  case result of
    Left startErr -> assertFailure $ "Failed to start temp database: " <> show startErr
    Right () -> pure ()

testGetMigrations :: IO ()
testGetMigrations = do
  result <- withTempDb $ \conn -> do
    -- Run migrations first
    _ <- Connection.use conn Migration.migrate

    -- Get applied migrations
    migrationsResult <- Connection.use conn Migration.getMigrations
    case migrationsResult of
      Left sessionErr -> assertFailure $ "Session error: " <> show sessionErr
      Right appliedMigrations -> do
        -- Should have applied the initialization + 10 SQL migrations
        assertBool "Should have applied migrations" (length appliedMigrations >= 10)
  case result of
    Left startErr -> assertFailure $ "Failed to start temp database: " <> show startErr
    Right () -> pure ()

testVersion :: IO ()
testVersion =
  Migration.version @?= "v1.10.0"

-- | Helper to run a list of migrations
runMigrations :: [MigrationCommand] -> Session (Either MigrationError ())
runMigrations cmds = foldM runIfOk (Right ()) cmds
  where
    runIfOk :: Either MigrationError () -> MigrationCommand -> Session (Either MigrationError ())
    runIfOk (Left err) _ = pure (Left err)
    runIfOk (Right ()) cmd = Sessions.runMigrationSession cmd

-- | Install v1.9.0 schema
installV1_9_0 :: Session (Either MigrationError ())
installV1_9_0 = runMigrations V1_9_0.migrations

testUpgradeFromV1_9_0 :: IO ()
testUpgradeFromV1_9_0 = do
  result <- withTempDb $ \conn -> do
    -- First install v1.9.0 schema
    v190Result <- Connection.use conn installV1_9_0
    case v190Result of
      Left sessionErr -> assertFailure $ "v1.9.0 install session error: " <> show sessionErr
      Right (Left migrationErr) -> assertFailure $ "v1.9.0 install migration error: " <> show migrationErr
      Right (Right ()) -> pure ()

    -- Now run upgrade
    upgradeResult <- Connection.use conn Migration.upgrade
    case upgradeResult of
      Left sessionErr -> assertFailure $ "Upgrade session error: " <> show sessionErr
      Right (Left migrationErr) -> assertFailure $ "Upgrade migration error: " <> show migrationErr
      Right (Right ()) -> pure ()

    -- Verify upgrade migration was applied
    migrationsResult <- Connection.use conn Migration.getMigrations
    case migrationsResult of
      Left sessionErr -> assertFailure $ "getMigrations session error: " <> show sessionErr
      Right appliedMigrations -> do
        let migrationNames = map show appliedMigrations
            hasUpgrade = any ("v1.9.0_to_v1.10.0" `isInfixOf`) migrationNames
        assertBool "Should have applied upgrade migration" hasUpgrade
  case result of
    Left startErr -> assertFailure $ "Failed to start temp database: " <> show startErr
    Right () -> pure ()

testUpgradeIdempotent :: IO ()
testUpgradeIdempotent = do
  result <- withTempDb $ \conn -> do
    -- First install v1.9.0 schema
    v190Result <- Connection.use conn installV1_9_0
    case v190Result of
      Left sessionErr -> assertFailure $ "v1.9.0 install session error: " <> show sessionErr
      Right (Left migrationErr) -> assertFailure $ "v1.9.0 install migration error: " <> show migrationErr
      Right (Right ()) -> pure ()

    -- Run upgrade first time
    upgrade1Result <- Connection.use conn Migration.upgrade
    case upgrade1Result of
      Left sessionErr -> assertFailure $ "First upgrade session error: " <> show sessionErr
      Right (Left migrationErr) -> assertFailure $ "First upgrade migration error: " <> show migrationErr
      Right (Right ()) -> pure ()

    -- Run upgrade second time - should succeed
    upgrade2Result <- Connection.use conn Migration.upgrade
    case upgrade2Result of
      Left sessionErr -> assertFailure $ "Second upgrade session error: " <> show sessionErr
      Right (Left migrationErr) -> assertFailure $ "Second upgrade migration error: " <> show migrationErr
      Right (Right ()) -> pure ()
  case result of
    Left startErr -> assertFailure $ "Failed to start temp database: " <> show startErr
    Right () -> pure ()

testUpgradeAddsLastReadAt :: IO ()
testUpgradeAddsLastReadAt = do
  result <- withTempDb $ \conn -> do
    -- First install v1.9.0 schema
    v190Result <- Connection.use conn installV1_9_0
    case v190Result of
      Left sessionErr -> assertFailure $ "v1.9.0 install session error: " <> show sessionErr
      Right (Left migrationErr) -> assertFailure $ "v1.9.0 install migration error: " <> show migrationErr
      Right (Right ()) -> pure ()

    -- Create a queue to test the schema
    createQueueResult <- Connection.use conn createTestQueue
    case createQueueResult of
      Left sessionErr -> assertFailure $ "Create queue session error: " <> show sessionErr
      Right () -> pure ()

    -- Run upgrade
    upgradeResult <- Connection.use conn Migration.upgrade
    case upgradeResult of
      Left sessionErr -> assertFailure $ "Upgrade session error: " <> show sessionErr
      Right (Left migrationErr) -> assertFailure $ "Upgrade migration error: " <> show migrationErr
      Right (Right ()) -> pure ()

    -- Verify last_read_at column exists in the queue table
    checkResult <- Connection.use conn checkLastReadAtColumn
    case checkResult of
      Left sessionErr -> assertFailure $ "Check column session error: " <> show sessionErr
      Right hasColumn ->
        assertBool "Queue table should have last_read_at column after upgrade" hasColumn
  case result of
    Left startErr -> assertFailure $ "Failed to start temp database: " <> show startErr
    Right () -> pure ()

-- | Create a test queue using pgmq.create
createTestQueue :: Session ()
createTestQueue = Session.statement () createQueueStmt
  where
    createQueueStmt :: Statement.Statement () ()
    createQueueStmt =
      preparable sql encoder decoder
      where
        sql = "SELECT pgmq.create('test_upgrade_queue')"
        encoder = Encoders.noParams
        decoder = Decoders.noResult

-- | Check if last_read_at column exists in the test queue table
checkLastReadAtColumn :: Session Bool
checkLastReadAtColumn = Session.statement () checkColumnStmt
  where
    checkColumnStmt :: Statement.Statement () Bool
    checkColumnStmt =
      preparable sql encoder decoder
      where
        sql =
          "SELECT EXISTS ( \
          \  SELECT 1 FROM information_schema.columns \
          \  WHERE table_schema = 'pgmq' \
          \  AND table_name = 'q_test_upgrade_queue' \
          \  AND column_name = 'last_read_at' \
          \)"
        encoder = Encoders.noParams
        decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool))
