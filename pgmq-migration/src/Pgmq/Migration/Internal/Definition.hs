{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Database.PostgreSQL.Migrate.Embed.RecompilePlugin #-}

-- | GHC 9.12 cannot track the manifest's sibling SQL directory as a Template Haskell
-- dependency, so the plugin above forces this module to be reconsidered on every build.
-- Without it, adding or removing a SQL file leaves stale embedded bytes and skips strict
-- manifest membership validation.
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
