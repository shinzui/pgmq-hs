module Bench.Send
  ( sendBenchmarks,
  )
where

import BenchConfig (BenchConfig (..))
import BenchSetup
  ( createBenchQueue,
    generatePayload,
    generatePayloads,
    runEffectful,
    runSession,
    uniqueQueueName,
  )
import Data.Aeson (object, (.=))
import Data.Text qualified as Text
import Hasql.Pool (Pool)
import Hasql.Session (Session)
import Pgmq.Effectful.Effect qualified as Eff
import Pgmq.Hasql.Sessions qualified as Sessions
import Pgmq.Hasql.Statements.Types (BatchSendMessage (..), SendMessage (..))
import Pgmq.Types (MessageBody (..), QueueName)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnfIO)

-- | All send operation benchmarks
sendBenchmarks :: Pool -> BenchConfig -> Benchmark
sendBenchmarks pool config =
  bgroup
    "send"
    [ singleSendBenchmarks pool config,
      batchSizeBenchmarks pool config,
      payloadSizeBenchmarks pool config
    ]

-- | Single send benchmarks
singleSendBenchmarks :: Pool -> BenchConfig -> Benchmark
singleSendBenchmarks pool config =
  let qname = unsafePerformIO $ setupQueue pool "send_single"
   in bgroup
        "single"
        [ bench "1-raw-sql" $ whnfIO $ runRawSend pool qname (generatePayload 1),
          bench "2-hasql" $ whnfIO $ runHasqlSend pool qname (generatePayload 1),
          bench "3-effectful" $ whnfIO $ runEffectfulSend pool qname (generatePayload 1)
        ]

-- | Batch size benchmarks (10, 50, 100)
batchSizeBenchmarks :: Pool -> BenchConfig -> Benchmark
batchSizeBenchmarks pool config =
  bgroup
    "batch-sizes"
    [ batchSizeBenchmark pool "batch_10" 10,
      batchSizeBenchmark pool "batch_50" 50,
      batchSizeBenchmark pool "batch_100" 100
    ]

batchSizeBenchmark :: Pool -> String -> Int -> Benchmark
batchSizeBenchmark pool name size =
  let qname = unsafePerformIO $ setupQueue pool name
   in bgroup
        name
        [ bench "1-raw-sql" $ whnfIO $ runRawBatchSend pool qname size,
          bench "2-hasql" $ whnfIO $ runHasqlBatchSend pool qname size,
          bench "3-effectful" $ whnfIO $ runEffectfulBatchSend pool qname size
        ]

-- | Payload size benchmarks
payloadSizeBenchmarks :: Pool -> BenchConfig -> Benchmark
payloadSizeBenchmarks pool config =
  bgroup
    "payload-sizes"
    [ payloadSizeBenchmark pool "small_100b" 100,
      payloadSizeBenchmark pool "medium_10kb" 10000,
      payloadSizeBenchmark pool "large_100kb" 100000
    ]

payloadSizeBenchmark :: Pool -> String -> Int -> Benchmark
payloadSizeBenchmark pool name size =
  let qname = unsafePerformIO $ setupQueue pool name
      payload = generatePayloadOfSize size
   in bgroup
        name
        [ bench "1-raw-sql" $ whnfIO $ runRawSend pool qname payload,
          bench "2-hasql" $ whnfIO $ runHasqlSend pool qname payload,
          bench "3-effectful" $ whnfIO $ runEffectfulSend pool qname payload
        ]

--------------------------------------------------------------------------------
-- Setup Helpers
--------------------------------------------------------------------------------

setupQueue :: Pool -> String -> IO QueueName
setupQueue pool suffix = do
  qname <- uniqueQueueName ("send_" <> Text.pack suffix)
  createBenchQueue pool qname
  pure qname

--------------------------------------------------------------------------------
-- Payload Generators
--------------------------------------------------------------------------------

generatePayloadOfSize :: Int -> MessageBody
generatePayloadOfSize size =
  MessageBody $
    object
      [ "id" .= (1 :: Int),
        "padding" .= replicate paddingLength 'x'
      ]
  where
    paddingLength = max 0 (size - 30)

--------------------------------------------------------------------------------
-- Raw SQL Runners (simplified - reuse from LayerComparison for full impl)
--------------------------------------------------------------------------------

runRawSend :: Pool -> QueueName -> MessageBody -> IO ()
runRawSend pool qname payload = do
  let msg = SendMessage qname payload Nothing
  _ <- runSessionOrFail pool (Sessions.sendMessage msg)
  pure ()

runRawBatchSend :: Pool -> QueueName -> Int -> IO ()
runRawBatchSend pool qname count = do
  let msg = BatchSendMessage qname (generatePayloads count) Nothing
  _ <- runSessionOrFail pool (Sessions.batchSendMessage msg)
  pure ()

--------------------------------------------------------------------------------
-- Hasql Runners
--------------------------------------------------------------------------------

runHasqlSend :: Pool -> QueueName -> MessageBody -> IO ()
runHasqlSend pool qname payload = do
  let msg = SendMessage qname payload Nothing
  _ <- runSessionOrFail pool (Sessions.sendMessage msg)
  pure ()

runHasqlBatchSend :: Pool -> QueueName -> Int -> IO ()
runHasqlBatchSend pool qname count = do
  let msg = BatchSendMessage qname (generatePayloads count) Nothing
  _ <- runSessionOrFail pool (Sessions.batchSendMessage msg)
  pure ()

--------------------------------------------------------------------------------
-- Effectful Runners
--------------------------------------------------------------------------------

runEffectfulSend :: Pool -> QueueName -> MessageBody -> IO ()
runEffectfulSend pool qname payload = do
  let msg = SendMessage qname payload Nothing
  _ <- runEffectful pool (Eff.sendMessage msg)
  pure ()

runEffectfulBatchSend :: Pool -> QueueName -> Int -> IO ()
runEffectfulBatchSend pool qname count = do
  let msg = BatchSendMessage qname (generatePayloads count) Nothing
  _ <- runEffectful pool (Eff.batchSendMessage msg)
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
