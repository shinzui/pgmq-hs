{-# LANGUAGE TemplateHaskell #-}

module Pgmq.Migration.Internal.Definition
  ( embeddedMigrationEntries,
    pgmqMigrations,
  )
where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Database.PostgreSQL.Migrate
  ( DefinitionError,
    MigrationComponent,
    migrationComponentFromEmbeddedSql,
  )
import Database.PostgreSQL.Migrate.Embed (embedMigrationManifest)

embeddedMigrationEntries :: NonEmpty (FilePath, ByteString)
embeddedMigrationEntries =
  $(embedMigrationManifest "migrations/manifest")

pgmqMigrations :: Either DefinitionError MigrationComponent
pgmqMigrations =
  migrationComponentFromEmbeddedSql "pgmq" mempty embeddedMigrationEntries
