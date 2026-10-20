module WebSocketSpec (tests, databaseTests, withWs, sendClient, receive, awaitFrame, frameType, frameField) where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (forM_, void)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.List (isInfixOf)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import EphemeralPg qualified as Pg
import ErrorSpec (failingEnv, serverError)
import Hasql.Pool qualified as Pool
import HttpSpec (sendN, withQueue)
import Network.Wai.Handler.Warp qualified as Warp
import Network.WebSockets qualified as WS
import Pgmq.Hasql.Statements.Types qualified as H
import Pgmq.Inspect
import Pgmq.Inspect.Outbox (overflowFrame)
import Pgmq.Types (queueNameToText)
import RouterSpec (lookupKey, mockEnv)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.Golden (NoCreateFile (..), goldenVsString)
import Test.Tasty.HUnit
import TestServer

withWs :: Int -> (WS.Connection -> IO a) -> IO a
withWs port = WS.runClient "127.0.0.1" port "/ws"

sendClient :: WS.Connection -> ClientFrame -> IO ()
sendClient connection = WS.sendTextData connection . encode

receive :: WS.Connection -> IO Value
receive connection = do
  bytes <- timeout 2000000 (WS.receiveData connection :: IO LBS.ByteString) >>= maybe (assertFailure "WebSocket receive timed out") pure
  either (assertFailure . ("invalid server JSON: " <>)) pure (eitherDecode bytes)

frameField :: Text -> Value -> Maybe Value
frameField = lookupKey . Key.fromText

frameType :: Value -> Maybe Text
frameType value = case frameField "type" value of Just (String tag) -> Just tag; _ -> Nothing

awaitFrame :: Int -> WS.Connection -> (Value -> Bool) -> IO (Maybe Value)
awaitFrame budget connection predicate = timeout budget loop
  where
    loop = do
      bytes <- WS.receiveData connection :: IO LBS.ByteString
      value <- either (assertFailure . show) pure (eitherDecode bytes)
      if predicate value then pure value else loop

config :: InspectConfig
config = defaultInspectConfig {pollIntervalUs = 200000, metricsMinRefreshUs = 100000, notifyDebounceUs = 20000}

tests :: TestTree
tests =
  testGroup
    "WebSocket"
    [ goldenTests,
      testCase "ping and malformed JSON are answered through the writer" $
        withTestRuntime config mockEnv $ \port _ -> withWs port $ \connection -> do
          sendClient connection Ping
          receive connection >>= (\frame -> frameType frame @?= Just "pong")
          WS.sendTextData connection ("not json" :: Text)
          receive connection >>= (\frame -> frameField "code" frame @?= Just (String "invalid_frame")),
      testCase "invalid queue frames do not echo the client payload" $
        withTestRuntime config mockEnv $ \port _ -> withWs port $ \connection -> do
          sendClient connection (Subscribe (T.replicate 100 "SECRET"))
          frame <- receive connection
          frameField "code" frame @?= Just (String "invalid_frame")
          frameField "queue" frame @?= Just Null
          assertBool "no raw client payload" (not ("SECRET" `isInfixOf` LBS.unpack (encode frame))),
      testCase "unknown queue returns a queue-scoped error and the connection survives" $
        withTestRuntime config mockEnv $ \port _ -> withWs port $ \connection -> do
          sendClient connection (Subscribe "missing")
          frame <- receive connection
          frameField "code" frame @?= Just (String "queue_not_found")
          frameField "queue" frame @?= Just (String "missing")
          sendClient connection Ping
          receive connection >>= (\answer -> frameType answer @?= Just "pong"),
      testCase "per-connection subscription cap is enforced before another read" $
        withTestRuntime config {wsMaxSubscriptions = 1} mockEnv $ \port _ -> withWs port $ \connection -> do
          sendClient connection (Subscribe "orders")
          receive connection >>= (\frame -> frameType frame @?= Just "snapshot")
          sendClient connection (Subscribe "other")
          receive connection >>= (\frame -> frameField "code" frame @?= Just (String "too_many_subscriptions")),
      testCase "distinct queue cap is shared across connections" $
        withTestRuntime config {wsMaxQueues = 1} mockEnv $ \port _ -> withWs port $ \first -> withWs port $ \second -> do
          sendClient first (Subscribe "orders")
          void (receive first)
          sendClient second (Subscribe "other")
          receive second >>= (\frame -> frameField "code" frame @?= Just (String "too_many_queues")),
      testCase "connection cap rejects the extra upgrade" $
        withTestRuntime config {wsMaxConnections = 1} mockEnv $ \port _ -> withWs port $ \first -> do
          sendClient first Ping
          void (receive first)
          outcome <- try @WS.HandshakeException (withWs port (const (pure ())))
          case outcome of
            Left _ -> pure ()
            Right () -> assertFailure "extra connection was accepted",
      testCase "oversized frame is bounded before receiving its body, even with capacity one" $
        withTestRuntime config {wsMaxFrameBytes = 64, wsQueueCapacity = 1} mockEnv $ \port _ -> withWs port $ \connection -> do
          WS.sendTextData connection (T.replicate 1024 "x")
          frame <- receive connection
          frameField "code" frame @?= Just (String "frame_too_large")
          receive connection >>= (\bye -> frameType bye @?= Just "goodbye"),
      testCase "database diagnostics do not reach feed errors" $
        withTestRuntime config (failingEnv (serverError "23505")) $ \port _ -> withWs port $ \connection -> do
          sendClient connection (Subscribe "orders")
          frame <- receive connection
          frameField "code" frame @?= Just (String "database_error")
          assertBool "no SQL/parameters/server diagnostics" (not ("SECRET" `isInfixOf` LBS.unpack (encode frame))),
      testCase "goodbye precedes the server close" $
        withTestRuntime config mockEnv $ \port runtime -> withWs port $ \connection -> do
          sendClient connection (Subscribe "orders")
          void (receive connection)
          runtimeRequestShutdown runtime
          receive connection >>= (\frame -> frameType frame @?= Just "goodbye")
          closed <- try @WS.ConnectionException (WS.receiveData connection :: IO LBS.ByteString)
          case closed of
            Left (WS.CloseRequest _ _) -> pure ()
            other -> assertFailure (show other),
      testCase "cancelling the convenience runner drains goodbye before Warp exits" $ do
        started <- newEmptyMVar
        stop <- newEmptyMVar
        withAsync (withInspectServer defaultInspectServerConfig {bindPort = 0, inspect = config} mockEnv (\server -> putMVar started (serverPort server) >> takeMVar stop)) $ \owner -> do
          port <- takeMVar started
          withWs port $ \connection -> do
            sendClient connection Ping
            void (receive connection)
            cancel owner
            receive connection >>= (\frame -> frameType frame @?= Just "goodbye"),
      testCase "host-owned Warp may outlive the runtime without retaining feed tasks" $ do
        readyRuntime <- newEmptyMVar
        stop <- newEmptyMVar
        withAsync (withInspectRuntime config mockEnv (\runtime -> putMVar readyRuntime runtime >> takeMVar stop)) $ \owner -> do
          runtime <- takeMVar readyRuntime
          Warp.testWithApplication (pure (runtimeApplication runtime)) $ \port -> withWs port $ \connection -> do
            sendClient connection Ping
            void (receive connection)
            putMVar stop ()
            receive connection >>= (\frame -> frameType frame @?= Just "goodbye")
            finished <- timeout 2000000 (wait owner)
            finished @?= Just ()
    ]

goldenTests :: TestTree
goldenTests =
  localOption (NoCreateFile True) $
    testGroup
      "frame encodings"
      ( [goldenVsString name ("test/golden/" <> name <> ".json") (pure bytes) | (name, bytes) <- fixtures]
          <> [ testCase "client round trips and server encoder parity" $ do
                 forM_ [Subscribe "orders", Unsubscribe "orders", Ping] $ \frame -> eitherDecode (encode frame) @?= Right frame
                 forM_ serverFrames $ \frame -> eitherDecode (encode frame) @?= Right (toJSON frame)
             ]
      )
  where
    epoch = UTCTime (fromGregorian 2026 9 30) (12 * 3600)
    metrics = H.QueueMetrics "orders" 3 (Just 1) (Just 9) 12 epoch 2 Nothing
    serverFrames = [Snapshot "orders" metrics True, Update "orders" metrics NotifySource, Update "orders" metrics PollSource, Pong, overflowFrame, Goodbye]
    fixtures =
      zip ["ws-subscribe", "ws-unsubscribe", "ws-ping"] (map encode [Subscribe "orders", Unsubscribe "orders", Ping])
        <> zip ["ws-snapshot", "ws-update-notify", "ws-update-poll", "ws-pong", "ws-error", "ws-goodbye"] (map encode serverFrames)

databaseTests :: IO (Pool.Pool, Pg.Database) -> TestTree
databaseTests database =
  testGroup
    "WebSocket database feed"
    [ testCase "subscribe sends snapshot then authoritative poll-sourced updates" $ do
        (pool, _) <- database
        withQueue pool $ \queue -> withTestRuntime config (poolEnv pool Nothing) $ \port _ -> withWs port $ \connection -> do
          let name = queueNameToText queue
          sendClient connection (Subscribe name)
          snapshot <- receive connection
          frameType snapshot @?= Just "snapshot"
          frameField "push" snapshot @?= Just (Bool False)
          (frameField "metrics" snapshot >>= frameField "queue_length") @?= Just (Number 0)
          void (sendN pool queue 3)
          update <- awaitFrame 2000000 connection (\frame -> frameType frame == Just "update" && (frameField "metrics" frame >>= frameField "queue_length") == Just (Number 3))
          case update of
            Nothing -> assertFailure "poll failed to converge"
            Just frame -> frameField "source" frame @?= Just (String "poll"),
      testCase "nonempty idle queues emit age changes even without writes" $ do
        (pool, _) <- database
        withQueue pool $ \queue -> do
          void (sendN pool queue 1)
          withTestRuntime config (poolEnv pool Nothing) $ \port _ -> withWs port $ \connection -> do
            sendClient connection (Subscribe (queueNameToText queue))
            snapshot <- receive connection
            let initialAge = frameField "metrics" snapshot >>= frameField "oldest_msg_age_sec"
            update <- awaitFrame 2000000 connection (\frame -> frameType frame == Just "update" && (frameField "metrics" frame >>= frameField "oldest_msg_age_sec") /= initialAge)
            case update of
              Nothing -> assertFailure "idle age change was suppressed"
              Just frame -> frameField "source" frame @?= Just (String "poll")
    ]
