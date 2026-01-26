-- | Transaction-level migration operations
module Pgmq.Migration.Transactions
  ( runMigrationWithSearchPath,
    getMigrationsTransaction,
  )
where

import Control.Monad (void)
import Data.Text (Text)
import Hasql.Migration (MigrationCommand, MigrationError, SchemaMigration)
import Hasql.Migration qualified as Migration
import Hasql.Transaction (Transaction)
import Hasql.Transaction qualified as Transaction
import Pgmq.Migration.Statements qualified as Statements

-- | Run a migration command, preserving and restoring the search path
runMigrationWithSearchPath :: Text -> MigrationCommand -> Transaction (Maybe MigrationError)
runMigrationWithSearchPath searchPath cmd = do
  -- Set the search path for this transaction
  void $ Transaction.statement searchPath Statements.setSearchPath
  -- Run the migration
  Migration.runMigration cmd

-- | Get all applied migrations
getMigrationsTransaction :: Transaction [SchemaMigration]
getMigrationsTransaction = Migration.getMigrations
