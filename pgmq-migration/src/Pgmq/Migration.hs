-- | PGMQ schema migration support
--
-- This module provides functions to install the PGMQ schema into PostgreSQL
-- without requiring the pgmq extension. It uses hasql-migration for tracking
-- applied migrations.
--
-- == Fresh Installation
--
-- For new projects, use 'migrate' to install the complete PGMQ schema:
--
-- @
-- import Hasql.Connection (acquire)
-- import Hasql.Session (run)
-- import Pgmq.Migration (migrate)
--
-- main :: IO ()
-- main = do
--   Right conn <- acquire "host=localhost dbname=mydb"
--   result <- run migrate conn
--   case result of
--     Right (Right ()) -> putStrLn "Migration successful"
--     Right (Left err) -> print err
--     Left sessionErr  -> print sessionErr
-- @
--
-- == Upgrading Existing Installations
--
-- For projects that previously installed PGMQ via this package (e.g., at v1.9.0),
-- use 'upgrade' to apply only the incremental changes:
--
-- @
-- import Hasql.Connection (acquire)
-- import Hasql.Session (run)
-- import Pgmq.Migration (upgrade)
--
-- main :: IO ()
-- main = do
--   Right conn <- acquire "host=localhost dbname=mydb"
--   result <- run upgrade conn
--   case result of
--     Right (Right ()) -> putStrLn "Upgrade successful"
--     Right (Left err) -> print err
--     Left sessionErr  -> print sessionErr
-- @
--
-- The hasql-migration library tracks which migrations have been applied,
-- so 'upgrade' will only apply migrations that haven't run yet.
--
-- == Which Function Should I Use?
--
-- * __New project, fresh database__: Use 'migrate'
-- * __Existing project using pgmq-migration__: Use 'upgrade'
-- * __Not sure__: Use 'upgrade' - it's safe on fresh databases too
--   (it will apply all needed migrations)
module Pgmq.Migration
  ( -- * Migration Operations
    migrate,
    upgrade,
    validate,
    getMigrations,

    -- * Migration Info
    version,
    migrations,
    upgradeMigrations,

    -- * Re-exports
    MigrationCommand,
    MigrationError (..),
    SchemaMigration (..),
  )
where

import Control.Monad (foldM)
import Hasql.Migration (MigrationCommand, MigrationError (..), SchemaMigration (..))
import Hasql.Migration qualified as Migration
import Hasql.Session (Session)
import Pgmq.Migration.Migrations qualified as Migrations
import Pgmq.Migration.Sessions qualified as Sessions

-- | Current version of the PGMQ schema
version :: String
version = Migrations.version

-- | Full migration commands for the current version.
--
-- Use this for fresh installations. Applies the complete schema.
migrations :: [MigrationCommand]
migrations = Migrations.migrations

-- | Incremental upgrade migrations.
--
-- Use this to upgrade existing installations that were set up via this package.
-- Contains only the delta migrations (e.g., v1.9.0 -> v1.10.0).
upgradeMigrations :: [MigrationCommand]
upgradeMigrations = Migrations.upgradeMigrations

-- | Run all PGMQ migrations for a fresh installation.
--
-- This function is idempotent - it will only run migrations that haven't
-- been applied yet. Returns 'Left' 'MigrationError' if a migration fails.
--
-- Use this for new projects with a fresh database.
migrate :: Session (Either MigrationError ())
migrate = runMigrations migrations

-- | Run upgrade migrations for existing installations.
--
-- This function applies only the incremental migrations needed to upgrade
-- from a previous version installed via this package (e.g., v1.9.0 -> v1.10.0).
--
-- It is idempotent and safe to run on any database - migrations that have
-- already been applied will be skipped.
--
-- Use this for existing projects that need to upgrade their PGMQ schema.
upgrade :: Session (Either MigrationError ())
upgrade = runMigrations upgradeMigrations

-- | Run a list of migrations
runMigrations :: [MigrationCommand] -> Session (Either MigrationError ())
runMigrations cmds = foldM runIfOk (Right ()) cmds
  where
    runIfOk :: Either MigrationError () -> MigrationCommand -> Session (Either MigrationError ())
    runIfOk (Left err) _ = pure (Left err)
    runIfOk (Right ()) cmd = Sessions.runMigrationSession cmd

-- | Validate all migrations without applying them
--
-- Checks that previously applied migrations haven't changed.
-- Returns a list of validation errors, or an empty list if validation passes.
validate :: Session [MigrationError]
validate = do
  results <- mapM validateOne validationCommands
  pure $ concat results
  where
    validationCommands = map Migration.MigrationValidation migrations

    validateOne :: MigrationCommand -> Session [MigrationError]
    validateOne cmd = do
      result <- Sessions.runMigrationSession cmd
      pure $ case result of
        Left err -> [err]
        Right () -> []

-- | Get all applied migrations
getMigrations :: Session [SchemaMigration]
getMigrations = Sessions.getMigrationsSession
