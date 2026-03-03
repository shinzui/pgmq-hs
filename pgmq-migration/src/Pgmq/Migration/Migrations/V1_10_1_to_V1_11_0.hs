{-# LANGUAGE TemplateHaskell #-}

-- | PGMQ v1.10.1 to v1.11.0 migration
--
-- Embeds the upstream pgmq--1.10.1--1.11.0.sql migration directly.
-- Changes: Topic routing, batch validation, notification throttle management.
module Pgmq.Migration.Migrations.V1_10_1_to_V1_11_0
  ( migrations,
  )
where

import Data.FileEmbed (embedFile)
import Hasql.Migration (MigrationCommand (..))

-- | Migration commands to upgrade from v1.10.1 to v1.11.0
migrations :: [MigrationCommand]
migrations =
  [ MigrationScript
      "pgmq_v1.10.1_to_v1.11.0"
      $(embedFile "vendor/pgmq/pgmq-extension/sql/pgmq--1.10.1--1.11.0.sql")
  ]
