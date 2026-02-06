module Bench.Ack
  ( ackBenchmarks,
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
import Data.Text qualified as Text
import Hasql.Pool (Pool)
import Hasql.Session (Session)
import Pgmq.Effectful.Effect qualified as Eff
import Pgmq.Hasql.Sessions qualified as Sessions
import Pgmq.Hasql.Statements.Types
  ( BatchMessageQuery (..),
    BatchSendMessage (..),
    MessageQuery (..),
    SendMessage (..),
  )
import Pgmq.Types (MessageId, QueueName)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnfIO)

-- | All ack (delete/archive) operation benchmarks
ackBenchmarks :: Pool -> BenchConfig -> Benchmark
ackBenchmarks pool config =
  bgroup
    "ack"
    [ deleteBenchmarks pool config,
      archiveBenchmarks pool config
    ]

-- | Delete benchmarks
deleteBenchmarks :: Pool -> BenchConfig -> Benchmark
deleteBenchmarks pool config =
  bgroup
    "delete"
    [ deleteSingleBenchmarks pool,
      deleteBatchBenchmarks pool
    ]

deleteSingleBenchmarks :: Pool -> Benchmark
deleteSingleBenchmarks pool =
  let qname = unsafePerformIO $ setupQueue pool "delete_single"
   in bgroup
        "single"
        [ bench "1-raw-sql" $ whnfIO $ benchDeleteSingle pool qname,
          bench "2-hasql" $ whnfIO $ benchDeleteSingle pool qname,
          bench "3-effectful" $ whnfIO $ benchDeleteSingleEffectful pool qname
        ]

deleteBatchBenchmarks :: Pool -> Benchmark
deleteBatchBenchmarks pool =
  bgroup
    "batch"
    [ deleteBatchBenchmark pool "batch_10" 10,
      deleteBatchBenchmark pool "batch_50" 50,
      deleteBatchBenchmark pool "batch_100" 100
    ]

deleteBatchBenchmark :: Pool -> String -> Int -> Benchmark
deleteBatchBenchmark pool name size =
  let qname = unsafePerformIO $ setupQueue pool name
   in bgroup
        name
        [ bench "1-raw-sql" $ whnfIO $ benchDeleteBatch pool qname size,
          bench "2-hasql" $ whnfIO $ benchDeleteBatch pool qname size,
          bench "3-effectful" $ whnfIO $ benchDeleteBatchEffectful pool qname size
        ]

-- | Archive benchmarks
archiveBenchmarks :: Pool -> BenchConfig -> Benchmark
archiveBenchmarks pool config =
  bgroup
    "archive"
    [ archiveSingleBenchmarks pool,
      archiveBatchBenchmarks pool
    ]

archiveSingleBenchmarks :: Pool -> Benchmark
archiveSingleBenchmarks pool =
  let qname = unsafePerformIO $ setupQueue pool "archive_single"
   in bgroup
        "single"
        [ bench "1-raw-sql" $ whnfIO $ benchArchiveSingle pool qname,
          bench "2-hasql" $ whnfIO $ benchArchiveSingle pool qname,
          bench "3-effectful" $ whnfIO $ benchArchiveSingleEffectful pool qname
        ]

archiveBatchBenchmarks :: Pool -> Benchmark
archiveBatchBenchmarks pool =
  bgroup
    "batch"
    [ archiveBatchBenchmark pool "batch_10" 10,
      archiveBatchBenchmark pool "batch_50" 50,
      archiveBatchBenchmark pool "batch_100" 100
    ]

archiveBatchBenchmark :: Pool -> String -> Int -> Benchmark
archiveBatchBenchmark pool name size =
  let qname = unsafePerformIO $ setupQueue pool name
   in bgroup
        name
        [ bench "1-raw-sql" $ whnfIO $ benchArchiveBatch pool qname size,
          bench "2-hasql" $ whnfIO $ benchArchiveBatch pool qname size,
          bench "3-effectful" $ whnfIO $ benchArchiveBatchEffectful pool qname size
        ]

--------------------------------------------------------------------------------
-- Setup Helpers
--------------------------------------------------------------------------------

setupQueue :: Pool -> String -> IO QueueName
setupQueue pool suffix = do
  qname <- uniqueQueueName ("ack_" <> Text.pack suffix)
  createBenchQueue pool qname
  pure qname

-- | Send a message and return its ID (for delete/archive benchmarks)
sendTestMessage :: Pool -> QueueName -> IO MessageId
sendTestMessage pool qname = do
  let msg = SendMessage qname (generatePayload 1) Nothing
  runSessionOrFail pool (Sessions.sendMessage msg)

-- | Send multiple messages and return their IDs
sendTestMessages :: Pool -> QueueName -> Int -> IO [MessageId]
sendTestMessages pool qname count = do
  let msg = BatchSendMessage qname (generatePayloads count) Nothing
  runSessionOrFail pool (Sessions.batchSendMessage msg)

--------------------------------------------------------------------------------
-- Delete Benchmarks Implementation
--------------------------------------------------------------------------------

benchDeleteSingle :: Pool -> QueueName -> IO ()
benchDeleteSingle pool qname = do
  msgId <- sendTestMessage pool qname
  let query = MessageQuery qname msgId
  _ <- runSessionOrFail pool (Sessions.deleteMessage query)
  pure ()

benchDeleteSingleEffectful :: Pool -> QueueName -> IO ()
benchDeleteSingleEffectful pool qname = do
  msgId <- sendTestMessage pool qname
  let query = MessageQuery qname msgId
  _ <- runEffectful pool (Eff.deleteMessage query)
  pure ()

benchDeleteBatch :: Pool -> QueueName -> Int -> IO ()
benchDeleteBatch pool qname count = do
  msgIds <- sendTestMessages pool qname count
  let query = BatchMessageQuery qname msgIds
  _ <- runSessionOrFail pool (Sessions.batchDeleteMessages query)
  pure ()

benchDeleteBatchEffectful :: Pool -> QueueName -> Int -> IO ()
benchDeleteBatchEffectful pool qname count = do
  msgIds <- sendTestMessages pool qname count
  let query = BatchMessageQuery qname msgIds
  _ <- runEffectful pool (Eff.batchDeleteMessages query)
  pure ()

--------------------------------------------------------------------------------
-- Archive Benchmarks Implementation
--------------------------------------------------------------------------------

benchArchiveSingle :: Pool -> QueueName -> IO ()
benchArchiveSingle pool qname = do
  msgId <- sendTestMessage pool qname
  let query = MessageQuery qname msgId
  _ <- runSessionOrFail pool (Sessions.archiveMessage query)
  pure ()

benchArchiveSingleEffectful :: Pool -> QueueName -> IO ()
benchArchiveSingleEffectful pool qname = do
  msgId <- sendTestMessage pool qname
  let query = MessageQuery qname msgId
  _ <- runEffectful pool (Eff.archiveMessage query)
  pure ()

benchArchiveBatch :: Pool -> QueueName -> Int -> IO ()
benchArchiveBatch pool qname count = do
  msgIds <- sendTestMessages pool qname count
  let query = BatchMessageQuery qname msgIds
  _ <- runSessionOrFail pool (Sessions.batchArchiveMessages query)
  pure ()

benchArchiveBatchEffectful :: Pool -> QueueName -> Int -> IO ()
benchArchiveBatchEffectful pool qname count = do
  msgIds <- sendTestMessages pool qname count
  let query = BatchMessageQuery qname msgIds
  _ <- runEffectful pool (Eff.batchArchiveMessages query)
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
