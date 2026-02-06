{-# LANGUAGE TemplateHaskell #-}

-- | PGMQ v1.10.0 migrations
-- Main change: Added last_read_at column to track when messages were last read
module Pgmq.Migration.Migrations.V1_10_0
  ( version,
    migrations,
  )
where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Hasql.Migration (MigrationCommand (..))

-- | Version string for this migration set
version :: String
version = "v1.10.0"

-- | All migration commands for v1.10.0
migrations :: [MigrationCommand]
migrations =
  [ MigrationInitialization,
    MigrationScript "pgmq_v1.10.0_01_schema" schema,
    MigrationScript "pgmq_v1.10.0_02_tables" tables,
    MigrationScript "pgmq_v1.10.0_03_types" types,
    MigrationScript "pgmq_v1.10.0_04_core_functions" coreFunctions,
    MigrationScript "pgmq_v1.10.0_05_queue_management" queueManagement,
    MigrationScript "pgmq_v1.10.0_06_message_ops" messageOps,
    MigrationScript "pgmq_v1.10.0_07_metrics" metrics,
    MigrationScript "pgmq_v1.10.0_08_partitioning" partitioning,
    MigrationScript "pgmq_v1.10.0_09_notifications" notifications,
    MigrationScript "pgmq_v1.10.0_10_fifo" fifo
  ]

schema :: ByteString
schema = $(embedFile "database/v1.10.0/01_schema.sql")

tables :: ByteString
tables = $(embedFile "database/v1.10.0/02_tables.sql")

types :: ByteString
types = $(embedFile "database/v1.10.0/03_types.sql")

coreFunctions :: ByteString
coreFunctions = $(embedFile "database/v1.10.0/04_core_functions.sql")

queueManagement :: ByteString
queueManagement = $(embedFile "database/v1.10.0/05_queue_management.sql")

messageOps :: ByteString
messageOps = $(embedFile "database/v1.10.0/06_message_ops.sql")

metrics :: ByteString
metrics = $(embedFile "database/v1.10.0/07_metrics.sql")

partitioning :: ByteString
partitioning = $(embedFile "database/v1.10.0/08_partitioning.sql")

notifications :: ByteString
notifications = $(embedFile "database/v1.10.0/09_notifications.sql")

fifo :: ByteString
fifo = $(embedFile "database/v1.10.0/10_fifo.sql")
