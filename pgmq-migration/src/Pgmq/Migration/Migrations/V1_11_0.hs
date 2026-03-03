{-# LANGUAGE TemplateHaskell #-}

-- | PGMQ v1.11.0 migrations
--
-- Embeds the upstream pgmq.sql directly from the vendored git subtree.
-- This is the single source of truth for the full PGMQ schema.
module Pgmq.Migration.Migrations.V1_11_0
  ( version,
    migrations,
  )
where

import Data.FileEmbed (embedFile)
import Hasql.Migration (MigrationCommand (..))

-- | Version string for this migration set
version :: String
version = "v1.11.0"

-- | All migration commands for v1.11.0
--
-- Uses the upstream pgmq.sql directly. The file is already ordered correctly
-- (types before functions, tables before references) and PostgreSQL executes
-- it as one transaction.
migrations :: [MigrationCommand]
migrations =
  [ MigrationInitialization,
    MigrationScript "pgmq_v1.11.0" $(embedFile "vendor/pgmq/pgmq-extension/sql/pgmq.sql")
  ]
