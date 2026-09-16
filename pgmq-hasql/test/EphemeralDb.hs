{-# LANGUAGE OverloadedStrings #-}

-- | Test database infrastructure using ephemeral-pg
module EphemeralDb
  ( -- * Database setup
    withPgmqDb,
    withPgmqPool,

    -- * Test fixtures
    TestFixture (..),
    withTestFixture,

    -- * Re-exports
    Database,
    StartError,
  )
where

import Control.Monad (filterM, when)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid (Last (..))
import Data.Text qualified as T
import Data.Text.IO qualified as TextIO
import Data.Word (Word64)
import Database.PostgreSQL.Migrate
  ( defaultRunOptions,
    migrationPlan,
    runMigrationPlan,
  )
import EphemeralPg
  ( Config (temporaryRoot),
    Database,
    StartError,
    connectionSettings,
    defaultCacheConfig,
    defaultConfig,
    withCachedConfig,
  )
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as PoolConfig
import Hasql.Session qualified as Session
import Pgmq.Migration qualified as Migration
import Pgmq.Types (QueueName, parseQueueName)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv)
import System.Posix.User (getEffectiveUserID)
import System.Random (randomRIO)

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

-- | Run an action with a temporary PostgreSQL database that has pgmq schema installed.
-- The 'Database' handle is passed alongside the pool because tests that need a raw
-- libpq connection (LISTEN\/NOTIFY has no hasql API) need its connection string.
withPgmqDb :: (Pool.Pool -> Database -> IO a) -> IO (Either StartError a)
withPgmqDb action =
  ephemeralConfig >>= \config ->
    withCachedConfig config defaultCacheConfig $ \db -> do
      let connSettings = connectionSettings db
          poolConfig =
            PoolConfig.settings
              [ PoolConfig.size 3,
                PoolConfig.staticConnectionSettings connSettings
              ]
      pool <- Pool.acquire poolConfig
      version <- lookupEnv "PGMQ_TEST_SCHEMA_VERSION"
      case version of
        Just "1.12.0" -> do
          paths <- filterM doesFileExist ["test/fixtures/pgmq-1.12.0.sql", "pgmq-hasql/test/fixtures/pgmq-1.12.0.sql"]
          path <- case paths of
            candidate : _ -> pure candidate
            [] -> error "Missing packaged PGMQ 1.12.0 test fixture"
          sql <- TextIO.readFile path
          Pool.use pool (Session.script sql) >>= either (error . show) pure
        Nothing -> installNative connSettings
        Just "1.13.0" -> installNative connSettings
        Just invalid -> error ("Invalid PGMQ_TEST_SCHEMA_VERSION: " <> invalid)
      required <- (== Just "1") <$> lookupEnv "PGMQ_REQUIRE_PARTMAN"
      -- Required runs fail for absent or unusable pg_partman, not just missing metadata.
      partman <- Pool.use pool (Session.script "CREATE SCHEMA IF NOT EXISTS partman; CREATE EXTENSION IF NOT EXISTS pg_partman SCHEMA partman")
      case partman of
        Left err -> when required (error ("PGMQ_REQUIRE_PARTMAN=1: " <> show err))
        Right () -> pure ()
      action pool db
  where
    installNative connSettings = do
      component <- either (error . ("Invalid PGMQ migration component: " <>) . show) pure Migration.pgmqMigrations
      plan <- either (error . ("Invalid PGMQ migration plan: " <>) . show) pure (migrationPlan (component :| []))
      installResult <- runMigrationPlan defaultRunOptions connSettings plan
      either (error . ("Migration failed: " <>) . show) (const (pure ())) installResult

-- | Run an action with a connection pool to a temporary PostgreSQL database
-- The database will have the pgmq schema installed
withPgmqPool :: (Pool.Pool -> IO a) -> IO (Either StartError a)
withPgmqPool action = withPgmqDb (\pool _ -> action pool)

-- | Test fixture with isolated queue for a test
data TestFixture = TestFixture
  { pool :: !Pool.Pool,
    queueName :: !QueueName
  }

-- | Create an isolated test fixture with a random queue name
-- This allows tests to run in parallel without interfering with each other
withTestFixture :: Pool.Pool -> (TestFixture -> IO a) -> IO a
withTestFixture p action = do
  qName <- generateTestQueueName
  action TestFixture {pool = p, queueName = qName}

-- | Generate a random queue name for test isolation
generateTestQueueName :: IO QueueName
generateTestQueueName = do
  -- Property tests allocate hundreds of queues; a five-digit namespace collides.
  suffix <- randomRIO (0 :: Word64, maxBound)
  case parseQueueName ("test_queue_" <> T.pack (show suffix)) of
    Left err -> error $ "Failed to generate queue name: " <> show err
    Right name -> pure name
