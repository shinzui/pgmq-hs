-- | PGMQ schema migration support
--
-- This module provides functions to install the PGMQ schema into PostgreSQL
-- without requiring the pgmq extension. It uses hasql-migration for tracking
-- applied migrations.
--
-- == Usage Example
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
module Pgmq.Migration
  ( -- * Migration Operations
    migrate,
    validate,
    getMigrations,

    -- * Migration Info
    version,
    migrations,

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

-- | All migration commands
migrations :: [MigrationCommand]
migrations = Migrations.migrations

-- | Run all PGMQ migrations
--
-- This function is idempotent - it will only run migrations that haven't
-- been applied yet. Returns 'Left' 'MigrationError' if a migration fails.
migrate :: Session (Either MigrationError ())
migrate = foldM runIfOk (Right ()) migrations
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
