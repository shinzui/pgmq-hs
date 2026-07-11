-- | Native pg-migrate component for installing PGMQ without the extension.
module Pgmq.Migration
  ( DefinitionError,
    MigrationComponent,
    pgmqMigrations,
  )
where

import Database.PostgreSQL.Migrate (DefinitionError, MigrationComponent)
import Pgmq.Migration.Internal.Definition (pgmqMigrations)
