{-# LANGUAGE OverloadedStrings #-}

-- | Test database infrastructure using ephemeral-pg
module EphemeralDb
  ( -- * Database setup
    withPgmqDb,

    -- * Re-exports
    StartError,
  )
where

import EphemeralPg
  ( StartError,
    connectionSettings,
    withCached,
  )
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as PoolConfig
import Pgmq.Migration qualified as Migration

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
