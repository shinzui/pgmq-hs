module LiveFeedSpec (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad (replicateM_, void)
import Data.Aeson (Value (..))
import Data.Text (Text)
import Data.Text qualified as T
import EphemeralDb (withPgmqDb)
import EphemeralPg qualified as Pg
import Hasql.Decoders qualified as D
import Hasql.Pool qualified as Pool
import Hasql.Session qualified as Session
import Hasql.Statement (unpreparable)
import HttpSpec (sendN, session, withQueue)
import Network.WebSockets qualified as WS
import Pgmq.Hasql.Sessions qualified as S
import Pgmq.Hasql.Statements.Types qualified as H
import Pgmq.Inspect
import Pgmq.Types (notifyChannelName, queueNameToText)
import RouterSpec (mockEnv)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import TestServer
import WebSocketSpec hiding (tests)

cfg :: InspectConfig
cfg = defaultInspectConfig {pollIntervalUs = 200000, metricsMinRefreshUs = 100000, notifyDebounceUs = 20000}

waitAcknowledged :: InspectRuntime -> Text -> IO ()
waitAcknowledged runtime channel = timeout 10000000 (atomically (runtimeListenerAcknowledged runtime channel >>= check)) >>= maybe (assertFailure "LISTEN not acknowledged") pure

hasLength :: Int -> Value -> Bool
hasLength n frame = (frameField "metrics" frame >>= frameField "queue_length") == Just (Number (fromIntegral n))

requireUpdate :: WS.Connection -> (Value -> Bool) -> IO Value
requireUpdate connection predicate = awaitFrame 2000000 connection (\frame -> frameType frame == Just "update" && predicate frame) >>= maybe (assertFailure "feed update timed out") pure

tests :: IO (Pool.Pool, Pg.Database) -> TestTree
tests database =
  testGroup
    "WebSocket listener-accelerated feed"
    [ testCase "registered push interest and notify updates work with polling far away" $ do
        (pool, db) <- database
        withQueue pool $ \queue -> do
          session pool (S.enableNotifyInsert (H.EnableNotifyInsert queue (Just 0)))
          withTestRuntime cfg {pollIntervalUs = 10000000} (poolEnv pool (Just (Pg.connectionSettings db))) $ \port runtime -> withWs port $ \connection -> do
            sendClient connection (Subscribe (queueNameToText queue))
            snapshot <- receive connection
            frameField "push" snapshot @?= Just (Bool True)
            waitAcknowledged runtime (notifyChannelName queue)
            void (sendN pool queue 1)
            frame <- requireUpdate connection (hasLength 1)
            frameField "source" frame @?= Just (String "notify"),
      testCase "notify burst coalesces and fans out one refresh to both clients" $ do
        (pool, db) <- database
        withQueue pool $ \queue -> do
          session pool (S.enableNotifyInsert (H.EnableNotifyInsert queue (Just 0)))
          withTestRuntime cfg {pollIntervalUs = 10000000} (poolEnv pool (Just (Pg.connectionSettings db))) $ \port runtime -> withWs port $ \a -> withWs port $ \b -> do
            sendClient a (Subscribe (queueNameToText queue))
            sendClient b (Subscribe (queueNameToText queue))
            void (receive a)
            void (receive b)
            waitAcknowledged runtime (notifyChannelName queue)
            replicateM_ 5 (void (sendN pool queue 1))
            first <- requireUpdate a (hasLength 5)
            second <- requireUpdate b (hasLength 5)
            frameField "source" first @?= Just (String "notify")
            first @?= second
            let countExtra n
                  | n >= 4 = pure n
                  | otherwise = awaitFrame 200000 a (\frame -> frameType frame == Just "update") >>= maybe (pure n) (const (countExtra (n + 1)))
            extra <- countExtra (0 :: Int)
            assertBool "fewer than five notify updates for the burst" (extra < 4),
      testCase "push means registration even without a trigger; poll remains authoritative" $ do
        (pool, db) <- database
        withQueue pool $ \queue ->
          withTestRuntime cfg (poolEnv pool (Just (Pg.connectionSettings db))) $ \port runtime -> withWs port $ \connection -> do
            sendClient connection (Subscribe (queueNameToText queue))
            snapshot <- receive connection
            frameField "push" snapshot @?= Just (Bool True)
            waitAcknowledged runtime (notifyChannelName queue)
            void (sendN pool queue 1)
            update <- requireUpdate connection (hasLength 1)
            frameField "source" update @?= Just (String "poll"),
      testCase "listener connection failure does not disable snapshots or readiness" $
        withTestRuntime cfg mockEnv {listenerSettings = Just "host=127.0.0.1 port=1 user=nobody dbname=nonexistent connect_timeout=1"} $ \port runtime -> withWs port $ \connection -> do
          sendClient connection (Subscribe "orders")
          snapshot <- receive connection
          frameField "push" snapshot @?= Just (Bool True)
          atomically (runtimeListenerHealthy runtime) >>= (@?= False)
          get port "/health/ready" >>= (\reply -> replyStatus reply @?= 200),
      testCase "foreign mixed-case names are poll-only with a configured listener" $ do
        result <- withPgmqDb $ \pool db -> do
          session pool (Session.script "select pgmq.create('Mixed_Case')")
          withTestRuntime cfg (poolEnv pool (Just (Pg.connectionSettings db))) $ \port _ -> withWs port $ \connection -> do
            sendClient connection (Subscribe "Mixed_Case")
            snapshot <- receive connection
            frameField "push" snapshot @?= Just (Bool False)
            session pool (Session.script "select pgmq.send('Mixed_Case', '{\"x\":1}'::jsonb)")
            update <- requireUpdate connection (hasLength 1)
            frameField "source" update @?= Just (String "poll")
        either (assertFailure . show) pure result,
      testCase "killing the LISTEN backend preserves convergence and resumes notify pushes" $ do
        -- Isolated cluster: terminating this runtime's listener cannot disrupt other cases.
        result <- withPgmqDb $ \pool db -> withQueue pool $ \queue -> do
          session pool (S.enableNotifyInsert (H.EnableNotifyInsert queue (Just 0)))
          withTestRuntime cfg (poolEnv pool (Just (Pg.connectionSettings db))) $ \port runtime -> withWs port $ \connection -> do
            sendClient connection (Subscribe (queueNameToText queue))
            void (receive connection)
            waitAcknowledged runtime (notifyChannelName queue)
            oldPid <- session pool (Session.statement () (unpreparable "select pid::int from pg_stat_activity where application_name = 'pgmq-inspect-listener'" mempty (D.singleRow (D.column (D.nonNullable D.int4)))))
            killed <- session pool (Session.statement () (unpreparable "select count(pg_terminate_backend(pid))::int from pg_stat_activity where application_name = 'pgmq-inspect-listener'" mempty (D.singleRow (D.column (D.nonNullable D.int4)))))
            assertBool "terminated a listener" (killed >= 1)
            void (sendN pool queue 1)
            void (requireUpdate connection (hasLength 1))
            let waitReplacement = do
                  replaced <- session pool (Session.statement () (unpreparable ("select count(*)::int from pg_stat_activity where application_name = 'pgmq-inspect-listener' and pid <> " <> T.pack (show oldPid)) mempty (D.singleRow (D.column (D.nonNullable D.int4)))))
                  if replaced > 0 then pure () else threadDelay 10000 >> waitReplacement
            timeout 10000000 waitReplacement >>= maybe (assertFailure "listener did not reconnect") pure
            waitAcknowledged runtime (notifyChannelName queue)
            void (sendN pool queue 1)
            update <- requireUpdate connection (\frame -> hasLength 2 frame && frameField "source" frame == Just (String "notify"))
            frameField "source" update @?= Just (String "notify")
        either (assertFailure . show) pure result
    ]
