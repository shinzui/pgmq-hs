{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text.Encoding qualified as TE
import Database.Postgres.Temp
  ( StartError,
    toConnectionString,
    with,
  )
import Hasql.Connection qualified as Connection
import Hasql.Connection.Setting qualified as Setting
import Hasql.Connection.Setting.Connection qualified as Connection.Setting
import Hasql.Session qualified as Session
import Pgmq.Migration qualified as Migration
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "pgmq-migration"
    [ testCase "migrate on fresh database succeeds" testMigrateFresh,
      testCase "migrate is idempotent" testMigrateIdempotent,
      testCase "getMigrations returns applied migrations" testGetMigrations,
      testCase "version is v1.10.0" testVersion
    ]

-- | Helper to run a test with a temporary PostgreSQL database
withTempDb :: (Connection.Connection -> IO a) -> IO (Either StartError a)
withTempDb action = with $ \db -> do
  let connStr = toConnectionString db
      connSettings = [Setting.connection (Connection.Setting.string (TE.decodeUtf8 connStr))]
  connResult <- Connection.acquire connSettings
  case connResult of
    Left err -> error $ "Failed to connect: " <> show err
    Right conn -> action conn

testMigrateFresh :: IO ()
testMigrateFresh = do
  result <- withTempDb $ \conn -> do
    migResult <- Session.run Migration.migrate conn
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
    result1 <- Session.run Migration.migrate conn
    case result1 of
      Left sessionErr -> assertFailure $ "First migration session error: " <> show sessionErr
      Right (Left migrationErr) -> assertFailure $ "First migration error: " <> show migrationErr
      Right (Right ()) -> pure ()

    -- Run migration second time - should succeed without error
    result2 <- Session.run Migration.migrate conn
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
    _ <- Session.run Migration.migrate conn

    -- Get applied migrations
    migrationsResult <- Session.run Migration.getMigrations conn
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
