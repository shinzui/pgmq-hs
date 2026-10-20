-- | Runtime-wide fair scheduling. Only fixed workers perform database work.
-- Notifications and missed polls coalesce; subscription count never multiplies scans.
module Pgmq.Inspect.Refresh
  ( MetricsCoordinator,
    PushHooks (..),
    Subscriber,
    newSubscriber,
    subscriberOutbox,
    withCoordinator,
    withCoordinatorUsing,
    subscribeQueue,
    unsubscribeQueue,
    releaseSubscriber,
    stopCoordinator,
    coordinatorQueueCount,
    coordinatorSubscriberCount,
    coordinatorInFlight,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (link, withAsync)
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forM_, forever, when)
import Data.Aeson (Value (..), toJSON)
import Data.Aeson.KeyMap qualified as KM
import Data.List (find, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Unique (Unique, newUnique)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Numeric.Natural (Natural)
import Pgmq.Effectful.Effect qualified as E
import Pgmq.Hasql.Statements.Types (QueueMetrics)
import Pgmq.Inspect.Budget
import Pgmq.Inspect.Config
import Pgmq.Inspect.Env
import Pgmq.Inspect.Error
import Pgmq.Inspect.Frames
import Pgmq.Inspect.Outbox
import Pgmq.Types

-- | Register one distinct eligible queue, returning its idempotent cleanup.
newtype PushHooks = PushHooks {registerPush :: Text -> STM () -> IO (IO ())}

data Subscriber = Subscriber
  { subscriberId :: !Unique,
    subscriberGenerations :: !(TVar (Map Text Generation)),
    subscriberFaultAt :: !(TVar (Maybe Word64)),
    subscriberOutbox :: !Outbox
  }

data Subscription = Subscription !Subscriber !Generation !Bool

-- Bool: initial snapshot has been atomically published.
data Entry = Entry
  { entryGeneration :: !Generation,
    entrySubscribers :: !(Map Unique Subscription),
    entryMetrics :: !(Maybe QueueMetrics),
    entryPushCleanup :: !(Maybe (IO ())),
    entryLastStart :: !(Maybe Word64),
    entryPollAt :: !Word64,
    entryDirtyAt :: !(Maybe Word64),
    entryFlight :: !(Maybe Unique)
  }

data MetricsCoordinator = MetricsCoordinator
  { coordinatorConfig :: !InspectConfig,
    coordinatorEnv :: !InspectEnv,
    coordinatorBudget :: !InspectionBudget,
    coordinatorPush :: !(Maybe PushHooks),
    coordinatorEntries :: !(TVar (Map Text Entry)),
    coordinatorClock :: !(TVar Word64),
    coordinatorStopped :: !(TVar Bool)
  }

data Job = Job !Text !Generation !Unique !(Map Unique Generation) !UpdateSource

newSubscriber :: Natural -> IO Subscriber
newSubscriber capacity = Subscriber <$> newUnique <*> newTVarIO Map.empty <*> newTVarIO Nothing <*> newOutbox capacity

withCoordinator :: InspectConfig -> InspectEnv -> InspectionBudget -> Maybe PushHooks -> (MetricsCoordinator -> IO a) -> IO a
withCoordinator = withCoordinatorUsing ((`div` 1000) <$> getMonotonicTimeNSec)

-- | Clock seam for deterministic scheduling and generation tests (microseconds).
withCoordinatorUsing :: IO Word64 -> InspectConfig -> InspectEnv -> InspectionBudget -> Maybe PushHooks -> (MetricsCoordinator -> IO a) -> IO a
withCoordinatorUsing clock cfg env budget hooks k = do
  now <- clock
  coordinator <- MetricsCoordinator cfg env budget hooks <$> newTVarIO Map.empty <*> newTVarIO now <*> newTVarIO False
  let ticker = forever $ clock >>= atomically . writeTVar (coordinatorClock coordinator) >> threadDelay 10000
      workers 0 action = action
      workers n action = withAsync (worker clock coordinator) $ \task -> link task >> workers (n - 1) action
      cleanup = do
        cleanups <- atomically $ do
          stopCoordinator coordinator
          entries <- readTVar (coordinatorEntries coordinator)
          writeTVar (coordinatorEntries coordinator) Map.empty
          pure [cleanup' | entry <- Map.elems entries, Just cleanup' <- [entryPushCleanup entry]]
        sequence_ cleanups
  (withAsync ticker $ \task -> link task >> workers (maxConcurrentReads cfg) (k coordinator `finally` atomically (stopCoordinator coordinator))) `finally` cleanup

stopCoordinator :: MetricsCoordinator -> STM ()
stopCoordinator = flip writeTVar True . coordinatorStopped

coordinatorQueueCount :: MetricsCoordinator -> STM Int
coordinatorQueueCount coordinator = length . filter (not . Map.null . entrySubscribers) . Map.elems <$> readTVar (coordinatorEntries coordinator)

coordinatorSubscriberCount :: MetricsCoordinator -> Text -> STM Int
coordinatorSubscriberCount coordinator name = maybe 0 (Map.size . entrySubscribers) . Map.lookup name <$> readTVar (coordinatorEntries coordinator)

coordinatorInFlight :: MetricsCoordinator -> STM Int
coordinatorInFlight coordinator = length . filter (isJust . entryFlight) . Map.elems <$> readTVar (coordinatorEntries coordinator)

-- Keep only bounded cooldown leases after unsubscribe, so unsubscribe/re-subscribe
-- cannot reset the minimum refresh interval. No metadata, metrics or listener remains.
collectIdle :: InspectConfig -> Word64 -> Map Text Entry -> Map Text Entry
collectIdle cfg now = Map.filter $ \entry -> not (Map.null (entrySubscribers entry)) || isJust (entryFlight entry) || not (cooled cfg now entry)

cooled :: InspectConfig -> Word64 -> Entry -> Bool
cooled cfg now entry = maybe True (\started -> now >= started && now - started >= fromIntegral (metricsMinRefreshUs cfg)) (entryLastStart entry)

subscribeQueue :: MetricsCoordinator -> Subscriber -> Text -> IO ()
subscribeQueue coordinator subscriber name = do
  generation <- newUnique
  atomically $ do
    stopped <- readTVar (coordinatorStopped coordinator)
    unlessStopped stopped $ do
      current <- readTVar (subscriberGenerations subscriber)
      now <- readTVar (coordinatorClock coordinator)
      entries <- collectIdle cfg now <$> readTVar (coordinatorEntries coordinator)
      let existing = Map.lookup name entries
          activeCount = length (filter (not . Map.null . entrySubscribers) (Map.elems entries))
          fresh = maybe True (Map.null . entrySubscribers) existing
          reject code message = enqueue (subscriberOutbox subscriber) (ErrorFrame code message (Just name))
      if T.null name || T.length name > 47 || T.any (== '\0') name
        then reject "invalid_frame" "Queue names must contain 1 to 47 characters without NUL."
        else
          if not (Map.member name current) && Map.size current >= wsMaxSubscriptions cfg
            then reject "too_many_subscriptions" "This connection has reached its subscription limit."
            else
              if fresh && activeCount >= wsMaxQueues cfg
                then reject "too_many_queues" "The runtime has reached its distinct queue limit."
                else
                  if not (isJust existing) && toInteger (Map.size entries) >= 2 * toInteger (wsMaxQueues cfg)
                    then reject "inspection_busy" "Queue refresh cooldown capacity is busy; retry the subscription."
                    else do
                      let entry = maybe (Entry generation Map.empty Nothing Nothing Nothing now Nothing Nothing) id existing
                          -- A generation tag invalidates queued frames from the previous subscription.
                          tag = (subscriberGenerations subscriber, name, generation)
                          active = isJust (entryMetrics entry)
                      clearQueue (subscriberOutbox subscriber) name
                      writeTVar (subscriberGenerations subscriber) (Map.insert name generation current)
                      forM_ (entryMetrics entry) $ \metrics -> enqueueTagged (subscriberOutbox subscriber) (Just tag) (Snapshot name metrics (isJust (entryPushCleanup entry)))
                      let updated = entry {entrySubscribers = Map.insert (subscriberId subscriber) (Subscription subscriber generation active) (entrySubscribers entry)}
                      writeTVar (coordinatorEntries coordinator) (Map.insert name updated entries)
  where
    cfg = coordinatorConfig coordinator
    unlessStopped True _ = pure ()
    unlessStopped False action = action

unsubscribeQueue :: MetricsCoordinator -> Subscriber -> Text -> IO ()
unsubscribeQueue coordinator subscriber name = do
  generation <- newUnique
  cleanup <- atomically $ do
    clearQueue (subscriberOutbox subscriber) name
    modifyTVar' (subscriberGenerations subscriber) (Map.delete name)
    entries <- readTVar (coordinatorEntries coordinator)
    case Map.lookup name entries of
      Nothing -> pure Nothing
      Just entry -> do
        let remaining = Map.delete (subscriberId subscriber) (entrySubscribers entry)
            empty = Map.null remaining
            updated =
              entry
                { entrySubscribers = remaining,
                  entryGeneration = if empty then generation else entryGeneration entry,
                  entryMetrics = if empty then Nothing else entryMetrics entry,
                  entryPushCleanup = if empty then Nothing else entryPushCleanup entry,
                  entryDirtyAt = if empty then Nothing else entryDirtyAt entry
                }
        writeTVar (coordinatorEntries coordinator) (Map.insert name updated entries)
        pure (if empty then entryPushCleanup entry else Nothing)
  sequence_ cleanup

releaseSubscriber :: MetricsCoordinator -> Subscriber -> IO ()
releaseSubscriber coordinator subscriber = do
  names <- Map.keys <$> readTVarIO (subscriberGenerations subscriber)
  mapM_ (unsubscribeQueue coordinator subscriber) names

worker :: IO Word64 -> MetricsCoordinator -> IO ()
worker clock coordinator = forever $ do
  token <- newUnique
  job@(Job name _ _ _ _) <- atomically (chooseJob coordinator token)
  now <- clock
  atomically $ modifyTVar' (coordinatorEntries coordinator) (Map.adjust (\entry -> entry {entryLastStart = Just now}) name)
  result <- try @SomeException $ withInspectionPermit (coordinatorBudget coordinator) (readMetrics coordinator name)
  outcome <- case result of
    Right Nothing -> pure (Left inspectionBusy)
    Right (Just value) -> pure value
    Left err
      | Just (_ :: SomeAsyncException) <- fromException err -> throwIO err
      | otherwise -> pure (Left (ErrorBody "database_error" "The database could not complete the inspection request." Nothing))
  case outcome of
    Right (queue, metrics) -> do
      ensurePush coordinator job queue
      atomically (completeJob coordinator job (Right metrics)) >>= sequence_
    Left err -> atomically (completeJob coordinator job (Left err)) >>= sequence_

chooseJob :: MetricsCoordinator -> Unique -> STM Job
chooseJob coordinator token = do
  stopped <- readTVar (coordinatorStopped coordinator)
  check (not stopped)
  now <- readTVar (coordinatorClock coordinator)
  entries <- readTVar (coordinatorEntries coordinator)
  let eligible (_, entry) = not (Map.null (entrySubscribers entry)) && not (isJust (entryFlight entry)) && cooled cfg now entry && due entry
      due entry = any (\(Subscription _ _ active) -> not active) (Map.elems (entrySubscribers entry)) || entryPollAt entry <= now || maybe False (\dirty -> now >= dirty && now - dirty >= fromIntegral (notifyDebounceUs cfg)) (entryDirtyAt entry)
      candidates = sortOn (\(name, entry) -> (entryLastStart entry, name)) (filter eligible (Map.toList entries))
  case candidates of
    [] -> retry
    (name, entry) : _ -> do
      let source = if entryPollAt entry <= now || not (isJust (entryMetrics entry)) then PollSource else NotifySource
          subscribers = Map.map (\(Subscription _ generation _) -> generation) (entrySubscribers entry)
          updated =
            entry
              { entryFlight = Just token,
                entryLastStart = Just now,
                entryDirtyAt = Nothing,
                entryPollAt = if source == PollSource then now + fromIntegral (pollIntervalUs cfg) else entryPollAt entry
              }
      writeTVar (coordinatorEntries coordinator) (Map.insert name updated entries)
      pure (Job name (entryGeneration entry) token subscribers source)
  where
    cfg = coordinatorConfig coordinator

readMetrics :: MetricsCoordinator -> Text -> IO (Either ErrorBody (UnvalidatedQueue, QueueMetrics))
readMetrics coordinator name = do
  outcome <- runInspection env $ do
    queues <- E.listQueuesUnvalidated
    case find ((== name) . unvalidatedName) queues of
      Nothing -> pure (Left (queueNotFound name))
      Just queue -> Right . (queue,) <$> E.queueMetricsUnvalidated name
  case outcome of
    Right value -> pure value
    Left err | sqlState err == Just "42P01" -> do
      checked <- runInspection env E.listQueuesUnvalidated
      pure $ case checked of
        Right queues | not (any ((== name) . unvalidatedName) queues) -> Left (queueNotFound name)
        _ -> Left (snd (classifyRuntimeError err))
    Left err -> pure (Left (snd (classifyRuntimeError err)))
  where
    env = coordinatorEnv coordinator

ensurePush :: MetricsCoordinator -> Job -> UnvalidatedQueue -> IO ()
ensurePush coordinator (Job name generation token _ _) queue =
  case (coordinatorPush coordinator, parseQueueName name) of
    (Just hooks, Right validated) | not (unvalidatedIsPartitioned queue) -> mask $ \restore -> do
      needed <- atomically $ do
        entries <- readTVar (coordinatorEntries coordinator)
        pure $ case Map.lookup name entries of
          Just entry -> entryGeneration entry == generation && entryFlight entry == Just token && not (isJust (entryPushCleanup entry)) && not (Map.null (entrySubscribers entry))
          _ -> False
      when needed $ do
        cleanup <- registerPush hooks (notifyChannelName validated) (markDirty coordinator name generation)
        retained <- atomically $ do
          stopped <- readTVar (coordinatorStopped coordinator)
          entries <- readTVar (coordinatorEntries coordinator)
          case Map.lookup name entries of
            Just entry | not stopped && entryGeneration entry == generation && entryFlight entry == Just token && not (Map.null (entrySubscribers entry)) -> do
              writeTVar (coordinatorEntries coordinator) (Map.insert name entry {entryPushCleanup = Just cleanup} entries)
              pure True
            _ -> pure False
        if retained then pure () else restore cleanup
    _ -> pure ()

markDirty :: MetricsCoordinator -> Text -> Generation -> STM ()
markDirty coordinator name generation = do
  now <- readTVar (coordinatorClock coordinator)
  modifyTVar' (coordinatorEntries coordinator) (Map.adjust (\entry -> if entryGeneration entry == generation then entry {entryDirtyAt = Just (maybe now id (entryDirtyAt entry))} else entry) name)

completeJob :: MetricsCoordinator -> Job -> Either ErrorBody QueueMetrics -> STM (Maybe (IO ()))
completeJob coordinator (Job name generation token participants source) outcome = do
  stopped <- readTVar (coordinatorStopped coordinator)
  entries <- readTVar (coordinatorEntries coordinator)
  now <- readTVar (coordinatorClock coordinator)
  case Map.lookup name entries of
    Just entry | entryFlight entry == Just token -> do
      let eligible subscriber current = maybe True (== current) (Map.lookup (subscriberId subscriber) participants)
          publish (Subscription subscriber current active) = do
            let tag = (subscriberGenerations subscriber, name, current)
            case outcome of
              Left err
                | errorCode err == "queue_not_found" -> pure ()
                | otherwise -> do
                    lastFault <- readTVar (subscriberFaultAt subscriber)
                    let transient = errorCode err `elem` ["inspection_busy", "inspection_timeout", "database_unavailable"]
                        elapsed = maybe True (\prior -> now >= prior && now - prior >= fromIntegral (pollIntervalUs (coordinatorConfig coordinator))) lastFault
                    when (not transient || elapsed) $ do
                      writeTVar (subscriberFaultAt subscriber) (Just now)
                      enqueueTagged (subscriberOutbox subscriber) (Just tag) (ErrorFrame (errorCode err) (errorMessage err) (Just name))
              Right metrics
                | not active -> enqueueTagged (subscriberOutbox subscriber) (Just tag) (Snapshot name metrics (isJust (entryPushCleanup entry)))
                | source == NotifySource || maybe True ((/= fingerprint metrics) . fingerprint) (entryMetrics entry) -> enqueueTagged (subscriberOutbox subscriber) (Just tag) (Update name metrics source)
                | otherwise -> pure ()
      when (not stopped && entryGeneration entry == generation) $
        forM_ (Map.elems (entrySubscribers entry)) $
          \sub@(Subscription subscriber current _) -> when (eligible subscriber current) (publish sub)
      let valid = not stopped && entryGeneration entry == generation
          successful = case outcome of Right _ -> True; _ -> False
          subscribers = Map.map (\(Subscription subscriber current active) -> if valid && successful && eligible subscriber current then Subscription subscriber current True else Subscription subscriber current active) (entrySubscribers entry)
          updated =
            entry
              { entryFlight = Nothing,
                entrySubscribers = subscribers,
                entryMetrics = if valid then either (const (entryMetrics entry)) Just outcome else entryMetrics entry,
                entryDirtyAt = case outcome of Left _ | valid -> Just now; _ -> entryDirtyAt entry
              }
      -- Confirmed disappearance removes the subscription, but never a global 42P01.
      if valid && either ((== "queue_not_found") . errorCode) (const False) outcome
        then do
          forM_ (Map.elems subscribers) $ \(Subscription subscriber current _) -> modifyTVar' (subscriberGenerations subscriber) (\gs -> if Map.lookup name gs == Just current then Map.delete name gs else gs)
          -- Errors above remain visible even though the generation is removed.
          forM_ (Map.elems subscribers) $ \(Subscription subscriber _ _) -> enqueue (subscriberOutbox subscriber) (ErrorFrame "queue_not_found" "No queue with that name exists." (Just name))
          writeTVar (coordinatorEntries coordinator) (Map.insert name updated {entrySubscribers = Map.empty, entryMetrics = Nothing, entryDirtyAt = Nothing, entryPushCleanup = Nothing} entries)
          pure (entryPushCleanup entry)
        else writeTVar (coordinatorEntries coordinator) (Map.insert name updated entries) >> pure Nothing
    _ -> pure Nothing

fingerprint :: QueueMetrics -> Value
fingerprint metrics = case toJSON metrics of Object fields -> Object (KM.delete "scrape_time" fields); value -> value
