-- | Session-level migration operations
module Pgmq.Migration.Sessions
  ( runMigrationSession,
    getMigrationsSession,
  )
where

import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Text (Text)
import Hasql.Migration (MigrationCommand, MigrationError, SchemaMigration)
import Hasql.Session (Session)
import Hasql.Session qualified as Session
import Hasql.Transaction.Sessions qualified as Transaction.Sessions
import Pgmq.Migration.Statements qualified as Statements
import Pgmq.Migration.Transactions qualified as Transactions

-- | Run a single migration command in a session
-- Returns Left MigrationError on failure, Right () on success
runMigrationSession :: MigrationCommand -> Session (Either MigrationError ())
runMigrationSession cmd = runExceptT $ do
  -- Get the current search path
  searchPath <- ExceptT $ fmap Right $ Session.statement () Statements.getSearchPath
  -- Run the migration in a transaction with proper isolation
  result <- ExceptT $ fmap Right $ runMigrationTransaction searchPath cmd
  -- Convert Maybe MigrationError to Either
  case result of
    Nothing -> pure ()
    Just err -> ExceptT $ pure $ Left err

-- | Run migration in a transaction with serializable isolation
runMigrationTransaction :: Text -> MigrationCommand -> Session (Maybe MigrationError)
runMigrationTransaction searchPath cmd =
  Transaction.Sessions.transaction
    Transaction.Sessions.Serializable
    Transaction.Sessions.Write
    (Transactions.runMigrationWithSearchPath searchPath cmd)

-- | Get all applied migrations
getMigrationsSession :: Session [SchemaMigration]
getMigrationsSession =
  Transaction.Sessions.transaction
    Transaction.Sessions.ReadCommitted
    Transaction.Sessions.Read
    Transactions.getMigrationsTransaction
