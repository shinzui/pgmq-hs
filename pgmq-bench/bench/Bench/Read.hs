module Bench.Read
  ( readBenchmarks,
  )
where

import BenchConfig (BenchConfig (..))
import BenchSetup
  ( createBenchQueue,
    runEffectful,
    runSession,
    seedQueue,
    uniqueQueueName,
  )
import Data.Text qualified as Text
import Hasql.Pool (Pool)
import Hasql.Session (Session)
import Pgmq.Effectful.Effect qualified as Eff
import Pgmq.Hasql.Sessions qualified as Sessions
import Pgmq.Hasql.Statements.Types (ReadMessage (..), ReadWithPollMessage (..))
import Pgmq.Types (QueueName)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnfIO)

-- | All read operation benchmarks
readBenchmarks :: Pool -> BenchConfig -> Benchmark
readBenchmarks pool config =
  bgroup
    "read"
    ( [ singleReadBenchmarks pool config,
        batchReadBenchmarks pool config
      ]
        <> [readWithPollBenchmarks pool config | config.enablePollBenchmarks]
    )

-- | Single read benchmarks
singleReadBenchmarks :: Pool -> BenchConfig -> Benchmark
singleReadBenchmarks pool config =
  let qname = unsafePerformIO $ setupQueueWithMessages pool "read_single" 100
   in bgroup
        "single"
        [ bench "1-raw-sql" $ whnfIO $ runHasqlRead pool qname 1,
          bench "2-hasql" $ whnfIO $ runHasqlRead pool qname 1,
          bench "3-effectful" $ whnfIO $ runEffectfulRead pool qname 1
        ]

-- | Batch read benchmarks with varying sizes
batchReadBenchmarks :: Pool -> BenchConfig -> Benchmark
batchReadBenchmarks pool config =
  bgroup
    "batch-sizes"
    [ batchReadBenchmark pool "batch_10" 10,
      batchReadBenchmark pool "batch_50" 50,
      batchReadBenchmark pool "batch_100" 100
    ]

batchReadBenchmark :: Pool -> String -> Int -> Benchmark
batchReadBenchmark pool name size =
  let qname = unsafePerformIO $ setupQueueWithMessages pool name 200
   in bgroup
        name
        [ bench "1-raw-sql" $ whnfIO $ runHasqlRead pool qname size,
          bench "2-hasql" $ whnfIO $ runHasqlRead pool qname size,
          bench "3-effectful" $ whnfIO $ runEffectfulRead pool qname size
        ]

-- | Read with poll benchmarks
readWithPollBenchmarks :: Pool -> BenchConfig -> Benchmark
readWithPollBenchmarks pool config =
  let qname = unsafePerformIO $ setupQueueWithMessages pool "read_poll" 100
   in bgroup
        "with-poll"
        [ bench "poll_1s_10msg" $ whnfIO $ runHasqlReadWithPoll pool qname 10 1 100,
          bench "poll_5s_50msg" $ whnfIO $ runHasqlReadWithPoll pool qname 50 5 100
        ]

--------------------------------------------------------------------------------
-- Setup Helpers
--------------------------------------------------------------------------------

setupQueueWithMessages :: Pool -> String -> Int -> IO QueueName
setupQueueWithMessages pool suffix count = do
  qname <- uniqueQueueName ("read_" <> Text.pack suffix)
  createBenchQueue pool qname
  _ <- seedQueue pool qname count
  pure qname

--------------------------------------------------------------------------------
-- Hasql Runners
--------------------------------------------------------------------------------

runHasqlRead :: Pool -> QueueName -> Int -> IO ()
runHasqlRead pool qname batchSize = do
  let query = ReadMessage qname 30 (Just (fromIntegral batchSize)) Nothing
  _ <- runSessionOrFail pool (Sessions.readMessage query)
  pure ()

runHasqlReadWithPoll :: Pool -> QueueName -> Int -> Int -> Int -> IO ()
runHasqlReadWithPoll pool qname batchSize maxPollSecs pollIntervalMs = do
  let query =
        ReadWithPollMessage
          { queueName = qname,
            delay = 30,
            batchSize = Just (fromIntegral batchSize),
            maxPollSeconds = fromIntegral maxPollSecs,
            pollIntervalMs = fromIntegral pollIntervalMs,
            conditional = Nothing
          }
  _ <- runSessionOrFail pool (Sessions.readWithPoll query)
  pure ()

--------------------------------------------------------------------------------
-- Effectful Runners
--------------------------------------------------------------------------------

runEffectfulRead :: Pool -> QueueName -> Int -> IO ()
runEffectfulRead pool qname batchSize = do
  let query = ReadMessage qname 30 (Just (fromIntegral batchSize)) Nothing
  _ <- runEffectful pool (Eff.readMessage query)
  pure ()

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

runSessionOrFail :: Pool -> Session a -> IO a
runSessionOrFail pool session = do
  result <- runSession pool session
  case result of
    Left err -> error $ "Session failed: " <> show err
    Right a -> pure a
