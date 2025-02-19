module DbTesting where

import Database.Postgres.Temp
  ( DB,
    cacheAction,
    cacheConfig,
    defaultConfig,
    -- verboseConfig,
    startConfig,
    stop,
    toConnectionString,
    withDbCache,
  )
import Hasql.Pool qualified as P
import Test.Tasty (withResource)
import Test.Tasty.Runners (TestTree)
