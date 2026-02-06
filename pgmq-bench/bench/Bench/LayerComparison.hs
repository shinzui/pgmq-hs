module Bench.LayerComparison
  ( layerComparisonBenchmarks,
  )
where

import BenchConfig (BenchConfig (..))
import BenchSetup
  ( createBenchQueue,
    generatePayload,
    generatePayloads,
    runEffectful,
    runSession,
    seedQueue,
    uniqueQueueName,
  )
import Data.Functor.Contravariant ((>$<))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text qualified as Text
import Data.Vector qualified as V
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Pool (Pool)
import Hasql.Session (Session, statement)
import Hasql.Statement (Statement (..))
import Pgmq.Effectful.Effect qualified as Eff
import Pgmq.Hasql.Decoders (messageDecoder)
import Pgmq.Hasql.Sessions qualified as Sessions
import Pgmq.Hasql.Statements.Types
  ( BatchMessageQuery (..),
    BatchSendMessage (..),
    MessageQuery (..),
    PopMessage (..),
    ReadMessage (..),
    SendMessage (..),
  )
import Pgmq.Types (Message, MessageBody (..), MessageId (..), QueueName, queueNameToText)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnfIO)

-- | All layer comparison benchmarks
layerComparisonBenchmarks :: Pool -> BenchConfig -> Benchmark
layerComparisonBenchmarks pool config =
  bgroup
    "layer-comparison"
    [ sendSingleBenchmarks pool config,
      sendBatch100Benchmarks pool config,
      readBatch10Benchmarks pool config,
      deleteSingleBenchmarks pool config,
      popBatch10Benchmarks pool config,
      fullCycleBenchmarks pool config
    ]

-- | Send single message benchmarks across all layers
sendSingleBenchmarks :: Pool -> BenchConfig -> Benchmark
sendSingleBenchmarks pool config =
  let qname = unsafePerformIO $ setupQueue pool config "send_single"
   in bgroup
        "send-single"
        [ bench "1-raw-sql" $ whnfIO $ do
            let payload = generatePayload 1
            _ <- runRawSendSingle pool qname payload
            pure (),
          bench "2-hasql" $ whnfIO $ do
            let msg = SendMessage qname (generatePayload 1) Nothing
            _ <- runSessionOrFail pool (Sessions.sendMessage msg)
            pure (),
          bench "3-effectful" $ whnfIO $ do
            let msg = SendMessage qname (generatePayload 1) Nothing
            _ <- runEffectful pool (Eff.sendMessage msg)
            pure ()
        ]

-- | Send batch of 100 messages across all layers
sendBatch100Benchmarks :: Pool -> BenchConfig -> Benchmark
sendBatch100Benchmarks pool config =
  let qname = unsafePerformIO $ setupQueue pool config "send_batch_100"
   in bgroup
        "send-batch-100"
        [ bench "1-raw-sql" $ whnfIO $ do
            let payloads = generatePayloads 100
            _ <- runRawBatchSend pool qname payloads
            pure (),
          bench "2-hasql" $ whnfIO $ do
            let msg = BatchSendMessage qname (generatePayloads 100) Nothing
            _ <- runSessionOrFail pool (Sessions.batchSendMessage msg)
            pure (),
          bench "3-effectful" $ whnfIO $ do
            let msg = BatchSendMessage qname (generatePayloads 100) Nothing
            _ <- runEffectful pool (Eff.batchSendMessage msg)
            pure ()
        ]

-- | Read batch of 10 messages across all layers
readBatch10Benchmarks :: Pool -> BenchConfig -> Benchmark
readBatch10Benchmarks pool config =
  let qname = unsafePerformIO $ setupQueueWithMessages pool config "read_batch_10" 1000
   in bgroup
        "read-batch-10"
        [ bench "1-raw-sql" $ whnfIO $ runRawRead pool qname 10,
          bench "2-hasql" $ whnfIO $ do
            let query = ReadMessage qname 30 (Just 10) Nothing
            _ <- runSessionOrFail pool (Sessions.readMessage query)
            pure (),
          bench "3-effectful" $ whnfIO $ do
            let query = ReadMessage qname 30 (Just 10) Nothing
            _ <- runEffectful pool (Eff.readMessage query)
            pure ()
        ]

-- | Delete single message across all layers
deleteSingleBenchmarks :: Pool -> BenchConfig -> Benchmark
deleteSingleBenchmarks pool config =
  let qname = unsafePerformIO $ setupQueue pool config "delete_single"
   in bgroup
        "delete-single"
        [ bench "1-raw-sql" $ whnfIO $ do
            msgId <- runRawSendSingle pool qname (generatePayload 1)
            _ <- runRawDeleteSingle pool qname msgId
            pure (),
          bench "2-hasql" $ whnfIO $ do
            let sendMsg = SendMessage qname (generatePayload 1) Nothing
            msgId <- runSessionOrFail pool (Sessions.sendMessage sendMsg)
            let query = MessageQuery qname msgId
            _ <- runSessionOrFail pool (Sessions.deleteMessage query)
            pure (),
          bench "3-effectful" $ whnfIO $ do
            let sendMsg = SendMessage qname (generatePayload 1) Nothing
            msgId <- runEffectful pool (Eff.sendMessage sendMsg)
            let query = MessageQuery qname msgId
            _ <- runEffectful pool (Eff.deleteMessage query)
            pure ()
        ]

-- | Pop batch of 10 messages across all layers
popBatch10Benchmarks :: Pool -> BenchConfig -> Benchmark
popBatch10Benchmarks pool config =
  let qname = unsafePerformIO $ setupQueue pool config "pop_batch_10"
   in bgroup
        "pop-batch-10"
        [ bench "1-raw-sql" $ whnfIO $ do
            _ <- seedQueue pool qname 10
            runRawPop pool qname 10,
          bench "2-hasql" $ whnfIO $ do
            _ <- seedQueue pool qname 10
            let query = PopMessage qname (Just 10)
            _ <- runSessionOrFail pool (Sessions.pop query)
            pure (),
          bench "3-effectful" $ whnfIO $ do
            _ <- seedQueue pool qname 10
            let query = PopMessage qname (Just 10)
            _ <- runEffectful pool (Eff.pop query)
            pure ()
        ]

-- | Full cycle: send -> read -> delete across all layers
fullCycleBenchmarks :: Pool -> BenchConfig -> Benchmark
fullCycleBenchmarks pool config =
  let qname = unsafePerformIO $ setupQueue pool config "full_cycle"
   in bgroup
        "full-cycle"
        [ bench "1-raw-sql" $ whnfIO $ do
            let payload = generatePayload 1
            msgId <- runRawSendSingle pool qname payload
            _ <- runRawRead pool qname 1
            _ <- runRawDeleteSingle pool qname msgId
            pure (),
          bench "2-hasql" $ whnfIO $ do
            let sendMsg = SendMessage qname (generatePayload 1) Nothing
            msgId <- runSessionOrFail pool (Sessions.sendMessage sendMsg)
            let readQuery = ReadMessage qname 30 (Just 1) Nothing
            _ <- runSessionOrFail pool (Sessions.readMessage readQuery)
            let deleteQuery = MessageQuery qname msgId
            _ <- runSessionOrFail pool (Sessions.deleteMessage deleteQuery)
            pure (),
          bench "3-effectful" $ whnfIO $ do
            let sendMsg = SendMessage qname (generatePayload 1) Nothing
            msgId <- runEffectful pool (Eff.sendMessage sendMsg)
            let readQuery = ReadMessage qname 30 (Just 1) Nothing
            _ <- runEffectful pool (Eff.readMessage readQuery)
            let deleteQuery = MessageQuery qname msgId
            _ <- runEffectful pool (Eff.deleteMessage deleteQuery)
            pure ()
        ]

-- | Setup a queue for benchmarking
setupQueue :: Pool -> BenchConfig -> String -> IO QueueName
setupQueue pool _config suffix = do
  qname <- uniqueQueueName (Text.pack suffix <> "_layer")
  createBenchQueue pool qname
  pure qname

-- | Setup a queue with pre-seeded messages
setupQueueWithMessages :: Pool -> BenchConfig -> String -> Int -> IO QueueName
setupQueueWithMessages pool config suffix count = do
  qname <- setupQueue pool config suffix
  _ <- seedQueue pool qname count
  pure qname

-- | Run session or fail with error
runSessionOrFail :: Pool -> Session a -> IO a
runSessionOrFail pool session = do
  result <- runSession pool session
  case result of
    Left err -> error $ "Session failed: " <> show err
    Right a -> pure a

--------------------------------------------------------------------------------
-- Raw SQL Statements (Baseline)
--------------------------------------------------------------------------------

-- | Raw SQL for sending a single message
rawSendSingleStatement :: Statement (QueueName, MessageBody) MessageId
rawSendSingleStatement = Statement sql encoder decoder True
  where
    sql = "select * from pgmq.send($1, $2, 0)"
    encoder =
      (queueNameToText . fst >$< E.param (E.nonNullable E.text))
        <> ((.unMessageBody) . snd >$< E.param (E.nonNullable E.jsonb))
    decoder = D.singleRow (MessageId <$> D.column (D.nonNullable D.int8))

-- | Run raw send single
runRawSendSingle :: Pool -> QueueName -> MessageBody -> IO MessageId
runRawSendSingle pool qname payload =
  runSessionOrFail pool $ statement (qname, payload) rawSendSingleStatement

-- | Raw SQL for batch sending messages
rawBatchSendStatement :: Statement (QueueName, [MessageBody]) [MessageId]
rawBatchSendStatement = Statement sql encoder decoder True
  where
    sql = "select * from pgmq.send_batch($1, $2, 0)"
    encoder =
      (queueNameToText . fst >$< E.param (E.nonNullable E.text))
        <> (fmap (.unMessageBody) . snd >$< E.param (E.nonNullable (E.foldableArray (E.nonNullable E.jsonb))))
    decoder = D.rowList (MessageId <$> D.column (D.nonNullable D.int8))

-- | Run raw batch send
runRawBatchSend :: Pool -> QueueName -> [MessageBody] -> IO [MessageId]
runRawBatchSend pool qname payloads =
  runSessionOrFail pool $ statement (qname, payloads) rawBatchSendStatement

-- | Raw SQL for reading messages (with full message decoding for fair comparison)
rawReadStatement :: Statement (QueueName, Int) (V.Vector Message)
rawReadStatement = Statement sql encoder decoder True
  where
    sql = "select * from pgmq.read($1, 30, $2)"
    encoder =
      (queueNameToText . fst >$< E.param (E.nonNullable E.text))
        <> (fromIntegral . snd >$< E.param (E.nonNullable E.int4))
    decoder = D.rowVector messageDecoder

-- | Run raw read
runRawRead :: Pool -> QueueName -> Int -> IO ()
runRawRead pool qname batchSize = do
  _ <- runSessionOrFail pool $ statement (qname, batchSize) rawReadStatement
  pure ()

-- | Raw SQL for deleting a single message
rawDeleteSingleStatement :: Statement (QueueName, MessageId) Bool
rawDeleteSingleStatement = Statement sql encoder decoder True
  where
    sql = "select * from pgmq.delete($1, $2)"
    encoder =
      (queueNameToText . fst >$< E.param (E.nonNullable E.text))
        <> ((.unMessageId) . snd >$< E.param (E.nonNullable E.int8))
    decoder = D.singleRow (D.column (D.nonNullable D.bool))

-- | Run raw delete single
runRawDeleteSingle :: Pool -> QueueName -> MessageId -> IO Bool
runRawDeleteSingle pool qname msgId =
  runSessionOrFail pool $ statement (qname, msgId) rawDeleteSingleStatement

-- | Raw SQL for popping messages (with full message decoding for fair comparison)
rawPopStatement :: Statement (QueueName, Int) (V.Vector Message)
rawPopStatement = Statement sql encoder decoder True
  where
    sql = "select * from pgmq.pop($1, $2)"
    encoder =
      (queueNameToText . fst >$< E.param (E.nonNullable E.text))
        <> (fromIntegral . snd >$< E.param (E.nonNullable E.int4))
    decoder = D.rowVector messageDecoder

-- | Run raw pop
runRawPop :: Pool -> QueueName -> Int -> IO ()
runRawPop pool qname qty = do
  _ <- runSessionOrFail pool $ statement (qname, qty) rawPopStatement
  pure ()
