module RouterSpec (tests, mockEnv, lookupKey, errorCodeOf) where

import Control.Monad (forM_)
import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Time (UTCTime (..), fromGregorian)
import Data.Vector qualified as V
import Effectful (runEff)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (runError)
import Pgmq.Effectful (PgmqRuntimeError)
import Pgmq.Effectful.Effect (Pgmq (..))
import Pgmq.Hasql.Statements.Types qualified as H
import Pgmq.Inspect.Config (defaultInspectConfig)
import Pgmq.Inspect.Env (InspectEnv (..))
import Pgmq.Types (UnvalidatedQueue (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestServer

-- | An environment whose runner never opens a connection: the handler answers
-- the four operations the Milestone 1 routes need and errors on any other.
mockEnv :: InspectEnv
mockEnv = InspectEnv {runInspection = runMock, listenerSettings = Nothing}
  where
    runMock action =
      either (Left . snd) Right
        <$> runEff
          ( runError @PgmqRuntimeError
              ( interpret
                  ( \_ -> \case
                      ListQueuesUnvalidated -> pure [cannedQueue]
                      QueueMetricsUnvalidated name -> pure (cannedMetrics name)
                      AllQueueMetrics -> pure [cannedMetrics "orders"]
                      ListTopicBindings -> pure []
                      PeekMessages _ -> pure V.empty
                      PeekArchivedMessages _ -> pure V.empty
                      LookupMessage _ -> pure Nothing
                      LookupArchivedMessage _ -> pure Nothing
                      ListNotifyInsertThrottles -> pure []
                      TestRouting _ -> pure []
                      _ -> error "RouterSpec: operation not canned"
                  )
                  action
              )
          )
    epoch = UTCTime (fromGregorian 2026 1 1) 0
    cannedQueue = UnvalidatedQueue "orders" epoch False False
    cannedMetrics name = H.QueueMetrics name 3 (Just 1) (Just 9) 12 epoch 2 Nothing

tests :: TestTree
tests =
  testGroup
    "Router (mock interpreter, no database)"
    [ testCase "integer overflow and malformed input are rejected before database work" $
        withTestServer defaultInspectConfig mockEnv $ \port -> forM_ ["-1", "9223372036854775808", "18446744073709551617", replicate 128 '9', replicate 129 '1', "1x", "", "%2B1"] $ \raw -> do
          reply <- get port ("/queues/orders/messages?from=" <> raw)
          replyStatus reply @?= 400
          errorCodeOf reply >>= (@?= Aeson.String "invalid_cursor")
          one <- get port ("/queues/orders/messages/" <> if null raw then "abc" else raw)
          replyStatus one @?= 400,
      testCase "maximum int64 and large capped limit remain exact" $
        withTestServer defaultInspectConfig mockEnv $ \port -> do
          reply <- get port "/queues/orders/messages?from=9223372036854775807&limit=999999999999999999999999999999999999999"
          replyStatus reply @?= 200,
      testCase "GET / describes the service" $
        withTestServer defaultInspectConfig mockEnv $ \port -> do
          reply <- get port "/"
          replyStatus reply @?= 200
          body <- decodeReply reply
          lookupKey "service" body @?= Just (Aeson.String "pgmq-inspect")
          lookupKey "websocket_path" body @?= Just (Aeson.String "ws"),
      testCase "GET /queues lists the canned queue with pgmq column names" $
        withTestServer defaultInspectConfig mockEnv $ \port -> do
          reply <- get port "/queues"
          replyStatus reply @?= 200
          rows <- decodeReply reply
          map (lookupKey "queue_name") rows @?= [Just (Aeson.String "orders")],
      testCase "GET /queues/orders/metrics distinguishes total and visible depth" $
        withTestServer defaultInspectConfig mockEnv $ \port -> do
          body <- decodeReply =<< get port "/queues/orders/metrics"
          lookupKey "queue_length" body @?= Just (Aeson.Number 3)
          lookupKey "queue_visible_length" body @?= Just (Aeson.Number 2)
          lookupKey "default_partition_length" body @?= Just Aeson.Null,
      testCase "an unknown path is 404 not_found" $
        withTestServer defaultInspectConfig mockEnv $ \port -> do
          reply <- get port "/nope"
          replyStatus reply @?= 404
          code <- errorCodeOf reply
          code @?= "not_found",
      testCase "POST is 405 method_not_allowed" $
        withTestServer defaultInspectConfig mockEnv $ \port -> do
          reply <- getWith port "POST" [] "/queues"
          replyStatus reply @?= 405,
      testCase "limit=abc is 400 invalid_limit before any database work" $
        withTestServer defaultInspectConfig mockEnv $ \port -> do
          reply <- get port "/queues/orders/messages?limit=abc"
          replyStatus reply @?= 400
          code <- errorCodeOf reply
          code @?= "invalid_limit"
    ]

lookupKey :: Aeson.Key -> Value -> Maybe Value
lookupKey k = \case
  Aeson.Object o -> KM.lookup k o
  _ -> Nothing

errorCodeOf :: Reply -> IO Value
errorCodeOf reply = do
  body <- decodeReply reply
  maybe (fail "no error.code") pure (lookupKey "error" body >>= lookupKey "code")
