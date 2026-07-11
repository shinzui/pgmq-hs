{-# LANGUAGE OverloadedStrings #-}

-- | Test database infrastructure using ephemeral-pg
module EphemeralDb
  ( -- * Database setup
    withPgmqDb,

    -- * Re-exports
    StartError,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Database.PostgreSQL.Migrate
  ( defaultRunOptions,
    migrationPlan,
    runMigrationPlan,
  )
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
  component <- either (error . ("Invalid PGMQ migration component: " <>) . show) pure Migration.pgmqMigrations
  plan <- either (error . ("Invalid PGMQ migration plan: " <>) . show) pure (migrationPlan (component :| []))
  installResult <- runMigrationPlan defaultRunOptions connSettings plan
  case installResult of
    Left migrationErr -> error $ "Migration failed: " <> show migrationErr
    Right _ -> action pool
