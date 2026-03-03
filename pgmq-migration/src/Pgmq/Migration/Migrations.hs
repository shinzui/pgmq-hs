-- | Aggregation of all PGMQ migrations
--
-- This module provides two migration paths:
--
-- == Fresh Installation
--
-- For new projects, use 'migrations' which installs the latest PGMQ schema directly:
--
-- @
-- import Pgmq.Migration.Migrations (migrations)
--
-- -- Apply migrations to a fresh database
-- runMigrations conn migrations
-- @
--
-- == Upgrading Existing Installations
--
-- For projects that previously installed PGMQ via this package, use 'upgradeMigrations'
-- to apply only the incremental changes:
--
-- @
-- import Pgmq.Migration.Migrations (upgradeMigrations)
--
-- -- Apply only upgrade migrations (v1.10.0 -> v1.10.1 -> v1.11.0)
-- runMigrations conn upgradeMigrations
-- @
--
-- The hasql-migration library tracks which migrations have been applied,
-- so running 'upgradeMigrations' on a database with v1.10.0 installed will
-- only apply the delta migrations needed to reach the current version.
--
-- == Compatibility Note
--
-- Existing deployments that ran the old hand-written upgrade migrations
-- (e.g., @pgmq_v1.9.0_to_v1.10.0_upgrade@, @pgmq_v1.10.0_to_v1.11.0_upgrade@)
-- have those names recorded in @schema_migrations@. The vendored migrations use
-- different names, so @hasql-migration@ will treat them as new, unapplied migrations.
-- This is correct — the vendored migrations re-apply upstream function definitions
-- using @CREATE OR REPLACE FUNCTION@ and @IF NOT EXISTS@ guards, which are safe
-- to re-run.
module Pgmq.Migration.Migrations
  ( version,
    migrations,
    upgradeMigrations,
  )
where

import Hasql.Migration (MigrationCommand (..))
import Pgmq.Migration.Migrations.V1_10_0_to_V1_10_1 qualified as V1_10_0_to_V1_10_1
import Pgmq.Migration.Migrations.V1_10_1_to_V1_11_0 qualified as V1_10_1_to_V1_11_0
import Pgmq.Migration.Migrations.V1_11_0 qualified as V1_11_0

-- | Current version of the PGMQ schema
version :: String
version = V1_11_0.version

-- | Full migration commands for the current version.
--
-- Use this for fresh installations. Applies the complete v1.11.0 schema.
migrations :: [MigrationCommand]
migrations = V1_11_0.migrations

-- | Incremental upgrade migrations.
--
-- Use this to upgrade existing installations that were set up via this package.
-- Contains only the delta migrations (v1.10.0 -> v1.10.1 -> v1.11.0).
--
-- The hasql-migration library will skip migrations that have already been applied,
-- so this is safe to run on any database regardless of its current version.
upgradeMigrations :: [MigrationCommand]
upgradeMigrations =
  [ MigrationInitialization
  ]
    ++ V1_10_0_to_V1_10_1.migrations
    ++ V1_10_1_to_V1_11_0.migrations
