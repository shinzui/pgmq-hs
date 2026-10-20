module RefreshSpec (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (forM_, replicateM, replicateM_, void)
import Data.Aeson (encode)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian)
import Data.Word (Word64)
import Effectful (liftIO)
import Effectful.Dispatch.Dynamic (interpret)
import Pgmq.Effectful.Effect qualified as E
import Pgmq.Hasql.Statements.Types qualified as H
import Pgmq.Inspect.Budget
import Pgmq.Inspect.Config
import Pgmq.Inspect.Env
import Pgmq.Inspect.Frames
import Pgmq.Inspect.Outbox
import Pgmq.Inspect.Refresh
import Pgmq.Types (UnvalidatedQueue (..))
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

cfg :: InspectConfig
cfg = defaultInspectConfig {pollIntervalUs = 200000, metricsMinRefreshUs = 100000, notifyDebounceUs = 20000}

epoch :: UTCTime
epoch = UTCTime (fromGregorian 2026 1 1) 0

metrics :: Text -> Int -> H.QueueMetrics
metrics name count = H.QueueMetrics name (fromIntegral count) Nothing Nothing (fromIntegral count) epoch (fromIntegral count) Nothing

data Probe = Probe
  {calls :: TVar (Map.Map Text Int), live :: TVar (Map.Map Text Int), peak :: TVar Int}

newProbe :: IO Probe
newProbe = Probe <$> newTVarIO Map.empty <*> newTVarIO Map.empty <*> newTVarIO 0

probeEnv :: Probe -> (Text -> Int -> IO H.QueueMetrics) -> InspectEnv
probeEnv probe readOne =
  InspectEnv
    ( runnerFromInterpreter
        ( interpret
            ( \_ -> \case
                E.ListQueuesUnvalidated -> pure [UnvalidatedQueue name epoch False False | name <- ["orders", "other"]]
                E.QueueMetricsUnvalidated name -> liftIO $ do
                  index <- atomically $ do
                    counts <- readTVar (calls probe)
                    let index = Map.findWithDefault 0 name counts + 1
                    writeTVar (calls probe) (Map.insert name index counts)
                    modifyTVar' (live probe) (Map.insertWith (+) name 1)
                    active <- sum . Map.elems <$> readTVar (live probe)
                    modifyTVar' (peak probe) (max active)
                    pure index
                  readOne name index `finally` atomically (modifyTVar' (live probe) (Map.adjust (subtract 1) name))
                _ -> error "RefreshSpec unexpected effect"
            )
        )
    )
    Nothing

await :: STM a -> IO a
await action = timeout 2000000 (atomically action) >>= maybe (assertFailure "coordinator barrier timed out") pure

next :: Subscriber -> IO ServerFrame
next subscriber = snd <$> await (takeOutgoing (subscriberOutbox subscriber))

callCount :: Probe -> Text -> STM Int
callCount probe name = Map.findWithDefault 0 name <$> readTVar (calls probe)

settled :: Probe -> MetricsCoordinator -> Text -> Int -> IO ()
settled probe coordinator name expected = await $ do
  n <- callCount probe name
  check (n == expected)
  coordinatorInFlight coordinator >>= check . (== 0)

withClock :: InspectConfig -> InspectEnv -> Maybe PushHooks -> (TVar Word64 -> MetricsCoordinator -> IO a) -> IO a
withClock config env hooks k = do
  clock <- newTVarIO 1000000
  withInspectionBudget config $ \budget -> withCoordinatorUsing (readTVarIO clock) config env budget hooks (k clock)

tests :: TestTree
tests =
  testGroup
    "WebSocket shared coordinator"
    [ testCase "100 subscriptions share one in-flight snapshot and one metrics read per poll" $ do
        probe <- newProbe
        started <- newEmptyMVar
        release <- newEmptyMVar
        let env =
              probeEnv
                probe
                ( \name n -> do
                    if n == 1 then putMVar started () >> takeMVar release else pure ()
                    pure (metrics name 0)
                )
        withClock cfg env Nothing $ \clock coordinator -> do
          subscribers <- replicateM 100 (newSubscriber 4)
          subscribeQueue coordinator (subscribers !! 0) "orders"
          takeMVar started
          forM_ (drop 1 subscribers) $ \subscriber -> subscribeQueue coordinator subscriber "orders"
          atomically (coordinatorSubscriberCount coordinator "orders") >>= (@?= 100)
          putMVar release ()
          forM_ subscribers $ \subscriber -> next subscriber >>= (\case Snapshot "orders" _ False -> pure (); frame -> assertFailure (show frame))
          settled probe coordinator "orders" 1
          -- Re-subscription reuses shared current metrics and cannot bypass the floor.
          replicateM_ 100 (subscribeQueue coordinator (subscribers !! 0) "orders")
          atomically (callCount probe "orders") >>= (@?= 1)
          atomically (writeTVar clock 1200000)
          settled probe coordinator "orders" 2
          forM_ (drop 1 subscribers) $ \subscriber -> atomically (queuedFrames (subscriberOutbox subscriber)) >>= (@?= 0),
      testCase "hints coalesce globally and obey the minimum refresh interval" $ do
        probe <- newProbe
        hint <- newEmptyMVar
        let hooks = PushHooks (\_ callback -> putMVar hint callback >> pure (pure ()))
        withClock cfg (probeEnv probe (\name _ -> pure (metrics name 0))) (Just hooks) $ \clock coordinator -> do
          a <- newSubscriber 4
          b <- newSubscriber 4
          subscribeQueue coordinator a "orders"
          subscribeQueue coordinator b "orders"
          next a >>= (\case Snapshot _ _ True -> pure (); frame -> assertFailure (show frame))
          void (next b)
          notify <- takeMVar hint
          atomically (replicateM_ 100 notify >> writeTVar clock 1050000)
          threadDelay 30000
          atomically (callCount probe "orders") >>= (@?= 1)
          atomically (writeTVar clock 1100000)
          forM_ [a, b] $ \subscriber -> next subscriber >>= (\case Update _ _ NotifySource -> pure (); frame -> assertFailure (show frame))
          settled probe coordinator "orders" 2,
      testCase "a slow queue cannot overlap itself or starve another queue" $ do
        probe <- newProbe
        started <- newEmptyMVar
        release <- newEmptyMVar
        let env =
              probeEnv
                probe
                ( \name n -> do
                    if name == "orders" && n == 1 then putMVar started () >> takeMVar release else pure ()
                    pure (metrics name n)
                )
        withClock cfg env Nothing $ \clock coordinator -> do
          a <- newSubscriber 8
          b <- newSubscriber 8
          subscribeQueue coordinator a "orders"
          takeMVar started
          subscribeQueue coordinator b "other"
          void (next b)
          atomically (writeTVar clock 1300000)
          void (next b)
          atomically (callCount probe "orders") >>= (@?= 1)
          atomically (readTVar (peak probe)) >>= (\n -> assertBool "global bound" (n <= maxConcurrentReads cfg))
          active <- atomically (Map.findWithDefault 0 "orders" <$> readTVar (live probe))
          active @?= 1
          putMVar release (),
      testCase "unsubscribe/re-subscribe discards stale completion and preserves refresh floor" $ do
        probe <- newProbe
        started <- newEmptyMVar
        release <- newEmptyMVar
        let env =
              probeEnv
                probe
                ( \name n -> do
                    if n == 1 then putMVar started () >> takeMVar release else pure ()
                    pure (metrics name n)
                )
        withClock cfg env Nothing $ \clock coordinator -> do
          subscriber <- newSubscriber 4
          subscribeQueue coordinator subscriber "orders"
          takeMVar started
          unsubscribeQueue coordinator subscriber "orders"
          subscribeQueue coordinator subscriber "orders"
          putMVar release ()
          settled probe coordinator "orders" 1
          atomically (queuedFrames (subscriberOutbox subscriber)) >>= (@?= 0)
          atomically (writeTVar clock 1200000)
          frame <- next subscriber
          case frame of
            Snapshot "orders" actual False -> encode actual @?= encode (metrics "orders" 2)
            _ -> assertFailure (show frame),
      testCase "HTTP-held admission reports busy and keeps a single bounded retry" $ do
        probe <- newProbe
        clock <- newTVarIO 1000000
        started <- newEmptyMVar
        release <- newEmptyMVar
        let config = cfg {maxConcurrentReads = 1}
        withInspectionBudget config $ \budget ->
          withAsync (withInspectionPermit budget (putMVar started () >> takeMVar release)) $ \held -> do
            takeMVar started
            withCoordinatorUsing (readTVarIO clock) config (probeEnv probe (\name n -> pure (metrics name n))) budget Nothing $ \coordinator -> do
              subscriber <- newSubscriber 4
              subscribeQueue coordinator subscriber "orders"
              next subscriber >>= (\case ErrorFrame "inspection_busy" _ _ -> pure (); frame -> assertFailure (show frame))
              atomically (callCount probe "orders") >>= (@?= 0)
              putMVar release ()
              void (wait held)
              atomically (writeTVar clock 1200000)
              next subscriber >>= (\case Snapshot _ _ _ -> pure (); frame -> assertFailure (show frame))
              settled probe coordinator "orders" 1
    ]
