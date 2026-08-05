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

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as T
import Data.Word (Word32)
import Database.PostgreSQL.Migrate
  ( defaultRunOptions,
    migrationPlan,
    runMigrationPlan,
  )
import EphemeralPg
  ( Database,
    StartError,
    connectionSettings,
    withCached,
  )
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as PoolConfig
import Pgmq.Migration qualified as Migration
import Pgmq.Types (QueueName, parseQueueName)
import System.Random (randomRIO)

-- | Run an action with a temporary PostgreSQL database that has pgmq schema installed.
-- The 'Database' handle is passed alongside the pool because tests that need a raw
-- libpq connection (LISTEN\/NOTIFY has no hasql API) need its connection string.
withPgmqDb :: (Pool.Pool -> Database -> IO a) -> IO (Either StartError a)
withPgmqDb action = withCached $ \db -> do
  let connSettings = connectionSettings db
      poolConfig =
        PoolConfig.settings
          [ PoolConfig.size 3,
            PoolConfig.staticConnectionSettings connSettings
          ]
  pool <- Pool.acquire poolConfig
  component <- either (error . ("Invalid PGMQ migration component: " <>) . show) pure Migration.pgmqMigrations
  plan <- either (error . ("Invalid PGMQ migration plan: " <>) . show) pure (migrationPlan (component :| []))
  installResult <- runMigrationPlan defaultRunOptions connSettings plan
  case installResult of
    Left migrationErr -> error $ "Migration failed: " <> show migrationErr
    Right _ -> action pool db

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
  suffix <- randomRIO (10000 :: Word32, 99999)
  case parseQueueName ("test_queue_" <> T.pack (show suffix)) of
    Left err -> error $ "Failed to generate queue name: " <> show err
    Right name -> pure name
