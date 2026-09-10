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

import Control.Monad (filterM, when)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text.IO qualified as TextIO
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
import Hasql.Session qualified as Session
import Pgmq.Migration qualified as Migration
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)

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
  version <- lookupEnv "PGMQ_TEST_SCHEMA_VERSION"
  case version of
    Just "1.12.0" -> do
      paths <- filterM doesFileExist ["test/fixtures/pgmq-1.12.0.sql", "pgmq-effectful/test/fixtures/pgmq-1.12.0.sql"]
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
  action pool
  where
    installNative connSettings = do
      component <- either (error . ("Invalid PGMQ migration component: " <>) . show) pure Migration.pgmqMigrations
      plan <- either (error . ("Invalid PGMQ migration plan: " <>) . show) pure (migrationPlan (component :| []))
      installResult <- runMigrationPlan defaultRunOptions connSettings plan
      either (error . ("Migration failed: " <>) . show) (const (pure ())) installResult

-- | Alias for 'withPgmqDb' kept for parallelism with pgmq-hasql.
withPgmqPool :: (Pool.Pool -> IO a) -> IO (Either StartError a)
withPgmqPool = withPgmqDb
