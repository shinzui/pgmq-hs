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
    StartError,
  )
where

import Data.Text qualified as T
import Data.Word (Word32)
import EphemeralPg
  ( StartError,
    connectionSettings,
    withCached,
  )
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as PoolConfig
import Pgmq.Migration qualified as Migration
import Pgmq.Types (QueueName, parseQueueName)
import System.Random (randomRIO)

-- | Run an action with a temporary PostgreSQL database that has pgmq schema installed
withPgmqDb :: (Pool.Pool -> IO a) -> IO (Either StartError a)
withPgmqDb action = withCached $ \db -> do
  let connSettings = connectionSettings db
      poolConfig =
        PoolConfig.settings
          [ PoolConfig.size 3,
            PoolConfig.staticConnectionSettings connSettings
          ]
  pool <- Pool.acquire poolConfig
  -- Install pgmq schema
  installResult <- Pool.use pool Migration.migrate
  case installResult of
    Left poolErr -> error $ "Failed to install pgmq schema: " <> show poolErr
    Right (Left migrationErr) -> error $ "Migration failed: " <> show migrationErr
    Right (Right ()) -> action pool

-- | Run an action with a connection pool to a temporary PostgreSQL database
-- The database will have the pgmq schema installed
withPgmqPool :: (Pool.Pool -> IO a) -> IO (Either StartError a)
withPgmqPool = withPgmqDb

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
