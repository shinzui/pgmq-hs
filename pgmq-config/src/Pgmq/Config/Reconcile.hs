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
import Data.Set qualified as Set
import Data.Text qualified as T
import GHC.Generics (Generic)
import Pgmq.Config.Types
import Pgmq.Hasql.Statements.Types qualified as StmtTypes
import Pgmq.Types
  ( NotifyInsertThrottle,
    Queue,
    QueueName,
    TopicBinding,
    TopicPattern,
    queueNameToText,
    topicPatternToText,
  )

-- | The database operations the reconciler needs, abstracted over the carrier
-- monad so one implementation serves both the 'Hasql.Session.Session' and
-- @Pgmq@-effect entry points.
data ReconcileOps m = ReconcileOps
  { listQueues :: m [Queue],
    listTopicBindings :: m [TopicBinding],
    listNotifyInsertThrottles :: m [NotifyInsertThrottle],
    createQueue :: QueueName -> m (),
    createUnloggedQueue :: QueueName -> m (),
    createPartitionedQueue :: StmtTypes.CreatePartitionedQueue -> m (),
    enableNotifyInsert :: StmtTypes.EnableNotifyInsert -> m (),
    createFifoIndex :: QueueName -> m (),
    bindTopic :: StmtTypes.BindTopic -> m ()
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
  existingQueues <- ops ^. #listQueues
  existingBindings <- ops ^. #listTopicBindings
  existingThrottles <- ops ^. #listNotifyInsertThrottles

  let existingQueueNames = Set.fromList (map (\q -> q ^. #name) existingQueues)
      existingBindingSet =
        Set.fromList
          [ (b ^. #bindingQueueName, topicPatternToText (b ^. #bindingPattern))
          | b <- existingBindings
          ]
      existingNotifySet = Set.fromList (map (\t -> t ^. #throttleQueueName) existingThrottles)

  concat <$> traverse (reconcileQueue ops existingQueueNames existingBindingSet existingNotifySet) configs

-- | Reconcile a single queue config against existing state, returning actions taken.
reconcileQueue ::
  (Monad m) =>
  ReconcileOps m ->
  Set.Set QueueName ->
  Set.Set (T.Text, T.Text) ->
  Set.Set T.Text ->
  QueueConfig ->
  m [ReconcileAction]
reconcileQueue ops existingQueues existingBindings existingNotify cfg = do
  let qn = cfg ^. #queueName
      qnText = queueNameToText qn

  -- Queue creation
  queueAction <-
    if Set.member qn existingQueues
      then pure [SkippedQueue qn]
      else do
        case cfg ^. #queueType of
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
        pure [CreatedQueue qn (cfg ^. #queueType)]

  -- Notification
  notifyAction <- case cfg ^. #notifyInsert of
    Nothing -> pure []
    Just nc ->
      if Set.member qnText existingNotify
        then pure [SkippedNotify qn]
        else do
          (ops ^. #enableNotifyInsert)
            StmtTypes.EnableNotifyInsert
              { queueName = qn,
                throttleIntervalMs = nc ^. #throttleMs
              }
          pure [EnabledNotify qn (nc ^. #throttleMs)]

  -- FIFO index — no way to query if index exists, so always apply (idempotent)
  fifoAction <-
    if cfg ^. #fifoIndex
      then do
        (ops ^. #createFifoIndex) qn
        pure [CreatedFifoIndex qn]
      else pure []

  -- Topic bindings
  bindingActions <- concat <$> traverse (reconcileBinding ops qn qnText existingBindings) (cfg ^. #topicBindings)

  pure (queueAction ++ notifyAction ++ fifoAction ++ bindingActions)

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
