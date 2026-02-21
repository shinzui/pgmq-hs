{-# LANGUAGE TemplateHaskell #-}

-- | PGMQ v1.10.0 to v1.11.0 migration
-- Changes: AMQP-like topic-based routing, notification management functions,
-- batch validation helpers, read_grouped fix
module Pgmq.Migration.Migrations.V1_10_0_to_V1_11_0
  ( migrations,
  )
where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Hasql.Migration (MigrationCommand (..))

-- | Migration commands to upgrade from v1.10.0 to v1.11.0
migrations :: [MigrationCommand]
migrations =
  [ MigrationScript "pgmq_v1.10.0_to_v1.11.0_upgrade" upgrade
  ]

upgrade :: ByteString
upgrade = $(embedFile "database/migrations/v1.10.0_to_v1.11.0.sql")
