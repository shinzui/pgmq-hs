module BenchConfig
  ( BenchConfig (..),
    loadConfig,
    defaultConfig,
  )
where

import Data.ByteString.Char8 qualified as BS
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data BenchConfig = BenchConfig
  { connectionString :: !BS.ByteString,
    messageCount :: !Int,
    batchSizes :: ![Int],
    skipCleanup :: !Bool,
    enablePollBenchmarks :: !Bool,
    enableThroughputBenchmarks :: !Bool
  }
  deriving stock (Show)

defaultConfig :: BenchConfig
defaultConfig =
  BenchConfig
    { connectionString = "host=localhost port=5432 dbname=pgmq_bench",
      messageCount = 1000,
      batchSizes = [1, 10, 50, 100],
      skipCleanup = False,
      enablePollBenchmarks = False,
      enableThroughputBenchmarks = False
    }

loadConfig :: IO BenchConfig
loadConfig = do
  connStr <- lookupEnv "PG_CONNECTION_STRING"
  msgCount <- lookupEnv "BENCH_MESSAGE_COUNT"
  batchSizesStr <- lookupEnv "BENCH_BATCH_SIZES"
  skipClean <- lookupEnv "BENCH_SKIP_CLEANUP"
  enablePoll <- lookupEnv "BENCH_ENABLE_POLL"
  enableThroughput <- lookupEnv "BENCH_ENABLE_THROUGHPUT"

  pure
    BenchConfig
      { connectionString = maybe (defaultConfig.connectionString) BS.pack connStr,
        messageCount = fromMaybe (defaultConfig.messageCount) (msgCount >>= readMaybe),
        batchSizes = maybe (defaultConfig.batchSizes) parseBatchSizes batchSizesStr,
        skipCleanup = maybe False parseBool skipClean,
        enablePollBenchmarks = maybe False parseBool enablePoll,
        enableThroughputBenchmarks = maybe False parseBool enableThroughput
      }

parseBatchSizes :: String -> [Int]
parseBatchSizes s =
  let parts = Text.splitOn "," (Text.pack s)
   in mapMaybe (readMaybe . Text.unpack . Text.strip) parts
  where
    mapMaybe f = foldr (\x acc -> maybe acc (: acc) (f x)) []

parseBool :: String -> Bool
parseBool s = s `elem` ["true", "1", "yes", "True", "TRUE"]
