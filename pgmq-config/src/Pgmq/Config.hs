module Pgmq.Config
  ( -- * Queue Configuration Types
    QueueConfig (..),
    QueueType (..),
    PartitionConfig (..),
    NotifyConfig (..),

    -- * Smart Constructors
    standardQueue,
    unloggedQueue,
    partitionedQueue,

    -- * Modifiers
    withNotifyInsert,
    withFifoIndex,
    withTopicBinding,

    -- * Reconciliation
    ensureQueues,
    ensureQueuesWithPool,

    -- * Reconciliation with Report
    ReconcileAction (..),
    ensureQueuesReport,
  )
where

import Control.Lens ((^.))
import Data.Foldable (for_)
import Data.Generics.Labels ()
import Data.Set qualified as Set
import Data.Text qualified as T
import Hasql.Pool qualified as Pool
import Hasql.Session (Session)
import Pgmq.Config.Types
import Pgmq.Hasql.Sessions qualified as Sessions
import Pgmq.Hasql.Statements.Types qualified as StmtTypes
import Pgmq.Types
  ( QueueName,
    TopicPattern,
    queueNameToText,
    topicPatternToText,
  )

-- | Ensure all declared queues exist with the desired settings.
-- This is idempotent — safe to call on every application startup.
-- Operations are additive only: queues not in the config are left untouched.
ensureQueues :: [QueueConfig] -> Session ()
ensureQueues configs =
  for_ configs applyQueueConfig

-- | Convenience wrapper that runs 'ensureQueues' against a connection pool.
ensureQueuesWithPool :: Pool.Pool -> [QueueConfig] -> IO (Either Pool.UsageError ())
ensureQueuesWithPool pool configs =
  Pool.use pool (ensureQueues configs)

-- | Like 'ensureQueues', but returns a report of actions taken.
-- Queries existing state first and skips operations that are already satisfied.
ensureQueuesReport :: [QueueConfig] -> Session [ReconcileAction]
ensureQueuesReport configs = do
  existingQueues <- Sessions.listQueues
  existingBindings <- Sessions.listTopicBindings
  existingThrottles <- Sessions.listNotifyInsertThrottles

  let existingQueueNames = Set.fromList (map (\q -> q ^. #name) existingQueues)
      existingBindingSet =
        Set.fromList
          [ (b ^. #bindingQueueName, topicPatternToText (b ^. #bindingPattern))
          | b <- existingBindings
          ]
      existingNotifySet = Set.fromList (map (\t -> t ^. #throttleQueueName) existingThrottles)

  concat <$> traverse (reconcileQueue existingQueueNames existingBindingSet existingNotifySet) configs

-- | Apply a single queue config without checking existing state.
applyQueueConfig :: QueueConfig -> Session ()
applyQueueConfig cfg = do
  let qn = cfg ^. #queueName
  -- Create the queue
  case cfg ^. #queueType of
    StandardQueue ->
      Sessions.createQueue qn
    UnloggedQueue ->
      Sessions.createUnloggedQueue qn
    PartitionedQueue pc ->
      Sessions.createPartitionedQueue
        StmtTypes.CreatePartitionedQueue
          { queueName = qn,
            partitionInterval = pc ^. #partitionInterval,
            retentionInterval = pc ^. #retentionInterval
          }

  -- Enable notifications if configured
  for_ (cfg ^. #notifyInsert) $ \nc ->
    Sessions.enableNotifyInsert
      StmtTypes.EnableNotifyInsert
        { queueName = qn,
          throttleIntervalMs = nc ^. #throttleMs
        }

  -- Create FIFO index if requested
  if cfg ^. #fifoIndex
    then Sessions.createFifoIndex qn
    else pure ()

  -- Bind topic patterns
  for_ (cfg ^. #topicBindings) $ \pat ->
    Sessions.bindTopic
      StmtTypes.BindTopic
        { topicPattern = pat,
          queueName = qn
        }

-- | Reconcile a single queue config against existing state, returning actions taken.
reconcileQueue ::
  Set.Set QueueName ->
  Set.Set (T.Text, T.Text) ->
  Set.Set T.Text ->
  QueueConfig ->
  Session [ReconcileAction]
reconcileQueue existingQueues existingBindings existingNotify cfg = do
  let qn = cfg ^. #queueName
      qnText = queueNameToText qn

  -- Queue creation
  queueAction <-
    if Set.member qn existingQueues
      then pure [SkippedQueue qn]
      else do
        case cfg ^. #queueType of
          StandardQueue ->
            Sessions.createQueue qn
          UnloggedQueue ->
            Sessions.createUnloggedQueue qn
          PartitionedQueue pc ->
            Sessions.createPartitionedQueue
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
          Sessions.enableNotifyInsert
            StmtTypes.EnableNotifyInsert
              { queueName = qn,
                throttleIntervalMs = nc ^. #throttleMs
              }
          pure [EnabledNotify qn (nc ^. #throttleMs)]

  -- FIFO index — no way to query if index exists, so always apply (idempotent)
  fifoAction <-
    if cfg ^. #fifoIndex
      then do
        Sessions.createFifoIndex qn
        pure [CreatedFifoIndex qn]
      else pure []

  -- Topic bindings
  bindingActions <- concat <$> traverse (reconcileBinding qn qnText existingBindings) (cfg ^. #topicBindings)

  pure (queueAction ++ notifyAction ++ fifoAction ++ bindingActions)

-- | Reconcile a single topic binding.
reconcileBinding ::
  QueueName ->
  T.Text ->
  Set.Set (T.Text, T.Text) ->
  TopicPattern ->
  Session [ReconcileAction]
reconcileBinding qn qnText existingBindings pat =
  let patText = topicPatternToText pat
   in if Set.member (qnText, patText) existingBindings
        then pure [SkippedTopicBinding qn pat]
        else do
          Sessions.bindTopic
            StmtTypes.BindTopic
              { topicPattern = pat,
                queueName = qn
              }
          pure [BoundTopic qn pat]
