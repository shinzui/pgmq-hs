module Bench.Throughput
  ( throughputBenchmarks,
  )
where

import BenchConfig (BenchConfig (..))
import BenchSetup
  ( createBenchQueue,
    generatePayload,
    generatePayloads,
    purgeQueue,
    runEffectful,
    runSession,
    uniqueQueueName,
  )
import Control.Monad (forM_, replicateM_)
import Data.Text qualified as Text
import Data.Vector qualified as V
import Hasql.Pool (Pool)
import Hasql.Session (Session)
import Pgmq.Effectful.Effect qualified as Eff
import Pgmq.Hasql.Sessions qualified as Sessions
import Pgmq.Hasql.Statements.Types
  ( BatchMessageQuery (..),
    BatchSendMessage (..),
    PopMessage (..),
    ReadMessage (..),
    SendMessage (..),
  )
import Pgmq.Types (Message (..), QueueName)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnfIO)

-- | All throughput benchmarks
throughputBenchmarks :: Pool -> BenchConfig -> Benchmark
throughputBenchmarks pool config =
  bgroup
    "throughput"
    [ sendNMessagesBenchmarks pool config,
      fullCycleNMessagesBenchmarks pool config
    ]

-- | Send N messages benchmarks (sequential vs batch)
sendNMessagesBenchmarks :: Pool -> BenchConfig -> Benchmark
sendNMessagesBenchmarks pool config =
  let n = config.messageCount
   in bgroup
        "send_n"
        [ sendNSequentialBenchmark pool n,
          sendNBatchedBenchmark pool n
        ]

sendNSequentialBenchmark :: Pool -> Int -> Benchmark
sendNSequentialBenchmark pool n =
  let qname = unsafePerformIO $ setupQueue pool "send_n_seq"
   in bgroup
        "sequential"
        [ bench "hasql" $ whnfIO $ do
            replicateM_ n $ do
              let msg = SendMessage qname (generatePayload 1) Nothing
              runSessionOrFail pool (Sessions.sendMessage msg)
            purgeQueue pool qname,
          bench "effectful" $ whnfIO $ do
            replicateM_ n $ do
              let msg = SendMessage qname (generatePayload 1) Nothing
              runEffectful pool (Eff.sendMessage msg)
            purgeQueue pool qname
        ]

sendNBatchedBenchmark :: Pool -> Int -> Benchmark
sendNBatchedBenchmark pool n =
  let qname = unsafePerformIO $ setupQueue pool "send_n_batch"
   in bgroup
        "batched"
        [ bench "hasql_batch_100" $ whnfIO $ do
            let batches = n `div` 100
            replicateM_ batches $ do
              let msg = BatchSendMessage qname (generatePayloads 100) Nothing
              runSessionOrFail pool (Sessions.batchSendMessage msg)
            purgeQueue pool qname,
          bench "effectful_batch_100" $ whnfIO $ do
            let batches = n `div` 100
            replicateM_ batches $ do
              let msg = BatchSendMessage qname (generatePayloads 100) Nothing
              runEffectful pool (Eff.batchSendMessage msg)
            purgeQueue pool qname
        ]

-- | Full cycle N messages (send -> read -> delete)
fullCycleNMessagesBenchmarks :: Pool -> BenchConfig -> Benchmark
fullCycleNMessagesBenchmarks pool config =
  let n = min 100 config.messageCount -- Cap at 100 for full cycle
   in bgroup
        "full_cycle_n"
        [ fullCycleNSequential pool n,
          fullCycleNBatched pool n
        ]

fullCycleNSequential :: Pool -> Int -> Benchmark
fullCycleNSequential pool n =
  let qname = unsafePerformIO $ setupQueue pool "cycle_n_seq"
   in bgroup
        "sequential"
        [ bench "hasql" $ whnfIO $ do
            -- Send N messages
            forM_ [1 .. n] $ \i -> do
              let msg = SendMessage qname (generatePayload i) Nothing
              runSessionOrFail pool (Sessions.sendMessage msg)
            -- Pop all messages (atomic read + delete)
            let popQuery = PopMessage qname (Just (fromIntegral n))
            _ <- runSessionOrFail pool (Sessions.pop popQuery)
            pure ()
        ]

fullCycleNBatched :: Pool -> Int -> Benchmark
fullCycleNBatched pool n =
  let qname = unsafePerformIO $ setupQueue pool "cycle_n_batch"
   in bgroup
        "batched"
        [ bench "hasql" $ whnfIO $ do
            -- Send batch
            let sendMsg = BatchSendMessage qname (generatePayloads n) Nothing
            _ <- runSessionOrFail pool (Sessions.batchSendMessage sendMsg)
            -- Read batch
            let readQuery = ReadMessage qname 30 (Just (fromIntegral n)) Nothing
            msgs <- runSessionOrFail pool (Sessions.readMessage readQuery)
            -- Delete batch
            let msgIds = map (.messageId) (V.toList msgs)
            let deleteQuery = BatchMessageQuery qname msgIds
            _ <- runSessionOrFail pool (Sessions.batchDeleteMessages deleteQuery)
            pure (),
          bench "effectful" $ whnfIO $ do
            -- Send batch
            let sendMsg = BatchSendMessage qname (generatePayloads n) Nothing
            _ <- runEffectful pool (Eff.batchSendMessage sendMsg)
            -- Read batch
            let readQuery = ReadMessage qname 30 (Just (fromIntegral n)) Nothing
            msgs <- runEffectful pool (Eff.readMessage readQuery)
            -- Delete batch
            let msgIds = map (.messageId) (V.toList msgs)
            let deleteQuery = BatchMessageQuery qname msgIds
            _ <- runEffectful pool (Eff.batchDeleteMessages deleteQuery)
            pure ()
        ]

--------------------------------------------------------------------------------
-- Setup Helpers
--------------------------------------------------------------------------------

setupQueue :: Pool -> String -> IO QueueName
setupQueue pool suffix = do
  qname <- uniqueQueueName ("throughput_" <> Text.pack suffix)
  createBenchQueue pool qname
  pure qname

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

runSessionOrFail :: Pool -> Session a -> IO a
runSessionOrFail pool session = do
  result <- runSession pool session
  case result of
    Left err -> error $ "Session failed: " <> show err
    Right a -> pure a
