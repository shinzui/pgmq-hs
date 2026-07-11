{-# LANGUAGE OverloadedStrings #-}

-- | Test database infrastructure using ephemeral-pg.
--
-- This is a copy of pgmq-hasql\'s EphemeralDb helper, adjusted for
-- pgmq-effectful\'s test suite. Keeping a local copy avoids cross-package
-- test-helper sharing (which would require a shared test-helper library
-- or fragile cross-package hs-source-dirs).
module EphemeralDb
  ( withPgmqDb,
    withPgmqPool,
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

-- | Run an action with a temporary PostgreSQL database that has the
-- pgmq schema installed.
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

-- | Alias for 'withPgmqDb' kept for parallelism with pgmq-hasql.
withPgmqPool :: (Pool.Pool -> IO a) -> IO (Either StartError a)
withPgmqPool = withPgmqDb
