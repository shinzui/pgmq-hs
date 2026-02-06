module Main (main) where

import Bench.Ack (ackBenchmarks)
import Bench.LayerComparison (layerComparisonBenchmarks)
import Bench.Read (readBenchmarks)
import Bench.Send (sendBenchmarks)
import Bench.Throughput (throughputBenchmarks)
import BenchConfig (BenchConfig (..), loadConfig)
import BenchSetup (installPgmqSchema, withBenchPool)
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main = do
  config <- loadConfig
  putStrLn $ "pgmq-bench configuration:"
  putStrLn $ "  Connection: " <> show config.connectionString
  putStrLn $ "  Message count: " <> show config.messageCount
  putStrLn $ "  Batch sizes: " <> show config.batchSizes
  putStrLn $ "  Skip cleanup: " <> show config.skipCleanup
  putStrLn ""

  withBenchPool config.connectionString $ \pool -> do
    putStrLn "Installing pgmq schema..."
    installPgmqSchema pool
    putStrLn "Schema installed. Starting benchmarks...\n"

    defaultMain
      [ layerComparisonBenchmarks pool config,
        sendBenchmarks pool config,
        readBenchmarks pool config,
        ackBenchmarks pool config,
        throughputBenchmarks pool config
      ]
