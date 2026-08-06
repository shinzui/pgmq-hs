-- | The backend-agnostic reconciliation core shared by "Pgmq.Config" (which
-- runs it in 'Hasql.Session.Session') and "Pgmq.Config.Effectful" (which runs
-- it in the @Pgmq@ effect). The logic lives here exactly once; the two public
-- modules only supply a 'ReconcileOps' record wiring the database calls.
--
-- This module is internal: it is listed under @other-modules@ in
-- @pgmq-config.cabal@ and is not part of the package's public API.
module Pgmq.Config.Reconcile
  ( ReconcileOps (..),
    ensureQueuesReportWith,
  )
where

import Control.Lens ((^.))
import Data.Generics.Labels ()
import Data.Int (Int32)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import GHC.Generics (Generic)
import Pgmq.Config.Types
import Pgmq.Hasql.Statements.Types qualified as StmtTypes
import Pgmq.Types
  ( NotifyInsertThrottle,
    QueueName,
    TopicBinding,
    TopicPattern,
    UnvalidatedQueue,
    queueNameToText,
    topicPatternToText,
  )

-- | The database operations the reconciler needs, abstracted over the carrier
-- monad so one implementation serves both the 'Hasql.Session.Session' and
-- @Pgmq@-effect entry points.
data ReconcileOps m = ReconcileOps
  { -- | Deliberately the /unvalidated/ listing: the reconciler only compares
    -- observed names against declared ones, and re-validating them would make
    -- one foreign queue whose name 'Pgmq.Types.parseQueueName' rejects fail the
    -- whole reconcile at application startup.
    listQueuesUnvalidated :: m [UnvalidatedQueue],
    listTopicBindings :: m [TopicBinding],
    listNotifyInsertThrottles :: m [NotifyInsertThrottle],
    createQueue :: QueueName -> m (),
    createUnloggedQueue :: QueueName -> m (),
    createPartitionedQueue :: StmtTypes.CreatePartitionedQueue -> m (),
    enableNotifyInsert :: StmtTypes.EnableNotifyInsert -> m (),
    createFifoIndex :: QueueName -> m (),
    bindTopic :: StmtTypes.BindTopic -> m (),
    -- | Queues that already carry their FIFO headers index, read from the
    -- @pg_indexes@ catalog. pgmq has no index-existence function, so without
    -- this the reconciler cannot say truthfully whether it created one.
    listFifoIndexQueueNames :: m [T.Text],
    -- | The reconciler's only mutation of already-existing state: bring a
    -- throttle row's interval in line with the declared one.
    updateNotifyInsert :: StmtTypes.UpdateNotifyInsert -> m ()
  }
  deriving stock (Generic)

-- | Reconcile the declared configs against existing state, returning a report
-- of actions taken. Queries existing state first and skips operations that are
-- already satisfied.
ensureQueuesReportWith ::
  (Monad m) =>
  ReconcileOps m ->
  [QueueConfig] ->
  m [ReconcileAction]
ensureQueuesReportWith ops configs = do
  existingQueues <- ops ^. #listQueuesUnvalidated
  existingBindings <- ops ^. #listTopicBindings
  existingThrottles <- ops ^. #listNotifyInsertThrottles
  existingFifoIndexes <- ops ^. #listFifoIndexQueueNames

  let existingQueuesByName =
        Map.fromList [(q ^. #unvalidatedName, q) | q <- existingQueues]
      existingBindingSet =
        Set.fromList
          [ (b ^. #bindingQueueName, topicPatternToText (b ^. #bindingPattern))
          | b <- existingBindings
          ]
      -- The interval is kept, not just the name: dropping it is what made
      -- declared-versus-stored throttle drift invisible.
      existingNotifyByName =
        Map.fromList
          [(t ^. #throttleQueueName, t ^. #throttleIntervalMs) | t <- existingThrottles]
      -- The catalog reports the lowercased physical form pgmq derives table
      -- names from. Declared names are lowercase-only (parseQueueName rejects
      -- anything else), so matching them textually is exact.
      existingFifoSet = Set.fromList existingFifoIndexes

  concat
    <$> traverse
      (reconcileQueue ops existingQueuesByName existingBindingSet existingNotifyByName existingFifoSet)
      configs

-- | Reconcile a single queue config against existing state, returning actions taken.
reconcileQueue ::
  (Monad m) =>
  ReconcileOps m ->
  Map.Map T.Text UnvalidatedQueue ->
  Set.Set (T.Text, T.Text) ->
  Map.Map T.Text Int32 ->
  Set.Set T.Text ->
  QueueConfig ->
  m [ReconcileAction]
reconcileQueue ops existingQueues existingBindings existingNotify existingFifo cfg = do
  let qn = cfg ^. #queueName
      qnText = queueNameToText qn
      declaredType = cfg ^. #queueType

  -- Queue creation, or — when the queue is already there — a shape comparison.
  -- Drift is reported and never repaired: the only way to change a queue's type
  -- is to drop and recreate it, which would destroy its messages.
  queueAction <-
    case Map.lookup qnText existingQueues of
      Just observed ->
        let observedType = observedQueueType observed
         in pure
              [ if declaredShape declaredType == observedType
                  then SkippedQueue qn
                  else DetectedQueueTypeDrift qn declaredType observedType
              ]
      Nothing -> do
        case declaredType of
          StandardQueue ->
            (ops ^. #createQueue) qn
          UnloggedQueue ->
            (ops ^. #createUnloggedQueue) qn
          PartitionedQueue pc ->
            (ops ^. #createPartitionedQueue)
              StmtTypes.CreatePartitionedQueue
                { queueName = qn,
                  partitionInterval = pc ^. #partitionInterval,
                  retentionInterval = pc ^. #retentionInterval
                }
        pure [CreatedQueue qn declaredType]

  -- Notification. A missing row is enabled; a row whose interval already
  -- matches is left strictly alone (re-enabling would reset last_notified_at);
  -- a row whose interval differs is updated in place.
  notifyAction <- case cfg ^. #notifyInsert of
    Nothing -> pure []
    Just nc ->
      let declaredMs = fromMaybe defaultThrottleMs (nc ^. #throttleMs)
       in case Map.lookup qnText existingNotify of
            Nothing -> do
              (ops ^. #enableNotifyInsert)
                StmtTypes.EnableNotifyInsert
                  { queueName = qn,
                    throttleIntervalMs = nc ^. #throttleMs
                  }
              pure [EnabledNotify qn (nc ^. #throttleMs)]
            Just observedMs
              | observedMs == declaredMs -> pure [SkippedNotify qn]
              | otherwise -> do
                  (ops ^. #updateNotifyInsert)
                    StmtTypes.UpdateNotifyInsert
                      { queueName = qn,
                        throttleIntervalMs = declaredMs
                      }
                  pure [UpdatedNotifyThrottle qn observedMs declaredMs]

  -- FIFO index. The catalog snapshot makes the report truthful; the underlying
  -- pgmq.create_fifo_index is CREATE INDEX IF NOT EXISTS, so losing a race with
  -- a concurrent replica degrades to a no-op rather than an error.
  fifoAction <-
    if cfg ^. #fifoIndex
      then
        if Set.member qnText existingFifo
          then pure [SkippedFifoIndex qn]
          else do
            (ops ^. #createFifoIndex) qn
            pure [CreatedFifoIndex qn]
      else pure []

  -- Topic bindings
  bindingActions <- concat <$> traverse (reconcileBinding ops qn qnText existingBindings) (cfg ^. #topicBindings)

  pure (queueAction ++ notifyAction ++ fifoAction ++ bindingActions)

-- | The shape a declared config asks for, reduced to what @pgmq.list_queues()@
-- can actually report. Partition interval and retention are deliberately
-- dropped here: the listing does not expose them, so they are not drift-checked.
declaredShape :: QueueType -> ObservedQueueType
declaredShape StandardQueue = ObservedStandard
declaredShape UnloggedQueue = ObservedUnlogged
declaredShape (PartitionedQueue _) = ObservedPartitioned

-- | The shape an observed queue actually has, from the two booleans
-- @pgmq.list_queues()@ reports.
observedQueueType :: UnvalidatedQueue -> ObservedQueueType
observedQueueType q
  | q ^. #unvalidatedIsPartitioned = ObservedPartitioned
  | q ^. #unvalidatedIsUnlogged = ObservedUnlogged
  | otherwise = ObservedStandard

-- | Reconcile a single topic binding.
reconcileBinding ::
  (Monad m) =>
  ReconcileOps m ->
  QueueName ->
  T.Text ->
  Set.Set (T.Text, T.Text) ->
  TopicPattern ->
  m [ReconcileAction]
reconcileBinding ops qn qnText existingBindings pat =
  let patText = topicPatternToText pat
   in if Set.member (qnText, patText) existingBindings
        then pure [SkippedTopicBinding qn pat]
        else do
          (ops ^. #bindTopic)
            StmtTypes.BindTopic
              { topicPattern = pat,
                queueName = qn
              }
          pure [BoundTopic qn pat]
