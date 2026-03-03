{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import EphemeralPg
  ( connectionSettings,
    withCached,
  )
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session (Session)
import Hasql.Session qualified as Session
import Hasql.Statement (preparable)
import Hasql.Statement qualified as Statement
import Pgmq.Migration qualified as Migration
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

main :: IO ()
main = do
  result <- withCached $ \db -> do
    let connSettings = connectionSettings db
    connResult <- Connection.acquire connSettings
    case connResult of
      Left err -> error $ "Failed to connect: " <> show err
      Right conn ->
        defaultMain (tests conn)
  case result of
    Left startErr -> error $ "Failed to start temp database: " <> show startErr
    Right () -> pure ()

tests :: Connection.Connection -> TestTree
tests conn =
  testGroup
    "pgmq-migration"
    [ testGroup
        "fresh install"
        [ testCase "migrate on fresh database succeeds" (testMigrateFresh conn),
          testCase "migrate is idempotent" (testMigrateIdempotent conn),
          testCase "getMigrations returns applied migrations" (testGetMigrations conn),
          testCase "version is v1.11.0" testVersion
        ],
      testGroup
        "upgrade"
        [ testCase "upgrade is idempotent" (testUpgradeIdempotent conn),
          testCase "upgrade after migrate succeeds" (testUpgradeAfterMigrate conn)
        ]
    ]

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

-- | Run a test with a clean database
withCleanDb :: Connection.Connection -> (Connection.Connection -> IO ()) -> IO ()
withCleanDb conn action = do
  resetDb conn
  action conn

testMigrateFresh :: Connection.Connection -> IO ()
testMigrateFresh conn = withCleanDb conn $ \c -> do
  migResult <- Connection.use c Migration.migrate
  case migResult of
    Left sessionErr -> assertFailure $ "Session error: " <> show sessionErr
    Right (Left migrationErr) -> assertFailure $ "Migration error: " <> show migrationErr
    Right (Right ()) -> pure ()

testMigrateIdempotent :: Connection.Connection -> IO ()
testMigrateIdempotent conn = withCleanDb conn $ \c -> do
  -- Run migration first time
  result1 <- Connection.use c Migration.migrate
  case result1 of
    Left sessionErr -> assertFailure $ "First migration session error: " <> show sessionErr
    Right (Left migrationErr) -> assertFailure $ "First migration error: " <> show migrationErr
    Right (Right ()) -> pure ()

  -- Run migration second time - should succeed without error
  result2 <- Connection.use c Migration.migrate
  case result2 of
    Left sessionErr -> assertFailure $ "Second migration session error: " <> show sessionErr
    Right (Left migrationErr) -> assertFailure $ "Second migration error: " <> show migrationErr
    Right (Right ()) -> pure ()

testGetMigrations :: Connection.Connection -> IO ()
testGetMigrations conn = withCleanDb conn $ \c -> do
  -- Run migrations first
  _ <- Connection.use c Migration.migrate

  -- Get applied migrations
  migrationsResult <- Connection.use c Migration.getMigrations
  case migrationsResult of
    Left sessionErr -> assertFailure $ "Session error: " <> show sessionErr
    Right appliedMigrations -> do
      -- Should have applied at least the pgmq_v1.11.0 SQL migration
      assertBool "Should have applied migrations" (length appliedMigrations >= 1)

testVersion :: IO ()
testVersion =
  Migration.version @?= "v1.11.0"

-- | Run upgrade twice after a fresh install - second run should be a no-op.
-- Upgrade migrations assume the pgmq schema already exists, so we install
-- the base schema first.
testUpgradeIdempotent :: Connection.Connection -> IO ()
testUpgradeIdempotent conn = withCleanDb conn $ \c -> do
  -- First install the base schema
  migResult <- Connection.use c Migration.migrate
  case migResult of
    Left sessionErr -> assertFailure $ "Migrate session error: " <> show sessionErr
    Right (Left migrationErr) -> assertFailure $ "Migrate error: " <> show migrationErr
    Right (Right ()) -> pure ()

  -- Run upgrade first time
  upgrade1Result <- Connection.use c Migration.upgrade
  case upgrade1Result of
    Left sessionErr -> assertFailure $ "First upgrade session error: " <> show sessionErr
    Right (Left migrationErr) -> assertFailure $ "First upgrade migration error: " <> show migrationErr
    Right (Right ()) -> pure ()

  -- Run upgrade second time - should succeed
  upgrade2Result <- Connection.use c Migration.upgrade
  case upgrade2Result of
    Left sessionErr -> assertFailure $ "Second upgrade session error: " <> show sessionErr
    Right (Left migrationErr) -> assertFailure $ "Second upgrade migration error: " <> show migrationErr
    Right (Right ()) -> pure ()

-- | Run migrate then upgrade - tests that CREATE OR REPLACE FUNCTION
-- makes re-applying safe
testUpgradeAfterMigrate :: Connection.Connection -> IO ()
testUpgradeAfterMigrate conn = withCleanDb conn $ \c -> do
  -- First do a fresh install
  migResult <- Connection.use c Migration.migrate
  case migResult of
    Left sessionErr -> assertFailure $ "Migrate session error: " <> show sessionErr
    Right (Left migrationErr) -> assertFailure $ "Migrate error: " <> show migrationErr
    Right (Right ()) -> pure ()

  -- Now run upgrade - should succeed (migrations are safe to re-apply)
  upgradeResult <- Connection.use c Migration.upgrade
  case upgradeResult of
    Left sessionErr -> assertFailure $ "Upgrade session error: " <> show sessionErr
    Right (Left migrationErr) -> assertFailure $ "Upgrade migration error: " <> show migrationErr
    Right (Right ()) -> pure ()
