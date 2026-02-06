-- | Aggregation of all PGMQ migrations
module Pgmq.Migration.Migrations
  ( version,
    migrations,
  )
where

import Hasql.Migration (MigrationCommand)
import Pgmq.Migration.Migrations.V1_10_0 qualified as V1_10_0

-- | Current version of the PGMQ schema
version :: String
version = V1_10_0.version

-- | All migration commands for the current version
migrations :: [MigrationCommand]
migrations = V1_10_0.migrations
