{-# LANGUAGE TemplateHaskell #-}

-- | PGMQ v1.10.0 to v1.10.1 migration
--
-- Embeds the upstream pgmq--1.10.0--1.10.1.sql migration directly.
-- Changes: Function signature refinements, read_grouped LATERAL join optimization.
module Pgmq.Migration.Migrations.V1_10_0_to_V1_10_1
  ( migrations,
  )
where

import Data.FileEmbed (embedFile)
import Hasql.Migration (MigrationCommand (..))

-- | Migration commands to upgrade from v1.10.0 to v1.10.1
migrations :: [MigrationCommand]
migrations =
  [ MigrationScript
      "pgmq_v1.10.0_to_v1.10.1"
      $(embedFile "vendor/pgmq/pgmq-extension/sql/pgmq--1.10.0--1.10.1.sql")
  ]
