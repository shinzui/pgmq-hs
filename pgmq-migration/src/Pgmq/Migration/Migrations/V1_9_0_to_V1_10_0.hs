{-# LANGUAGE TemplateHaskell #-}

-- | PGMQ v1.9.0 to v1.10.0 migration
-- Changes: Added last_read_at column to track when messages were last read
module Pgmq.Migration.Migrations.V1_9_0_to_V1_10_0
  ( migrations,
  )
where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Hasql.Migration (MigrationCommand (..))

-- | Migration commands to upgrade from v1.9.0 to v1.10.0
migrations :: [MigrationCommand]
migrations =
  [ MigrationScript "pgmq_v1.9.0_to_v1.10.0_upgrade" upgrade
  ]

upgrade :: ByteString
upgrade = $(embedFile "database/migrations/v1.9.0_to_v1.10.0.sql")
