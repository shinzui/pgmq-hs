module Pgmq.Config.Effectful
  ( -- * Reconciliation
    ensureQueuesEff,

    -- * Reconciliation with Report
    ensureQueuesReportEff,
  )
where

import Control.Lens ((^.))
import Data.Generics.Labels ()
import Data.Set qualified as Set
import Data.Text qualified as T
import Effectful (Eff, (:>))
import Pgmq.Config.Types
import Pgmq.Effectful.Effect qualified as Eff
import Pgmq.Hasql.Statements.Types qualified as StmtTypes
import Pgmq.Types
  ( QueueName,
    TopicPattern,
    queueNameToText,
    topicPatternToText,
  )

-- | Ensure all declared queues exist using the Pgmq effect.
ensureQueuesEff :: (Eff.Pgmq :> es) => [QueueConfig] -> Eff es ()
ensureQueuesEff configs =
  mapM_ applyQueueConfigEff configs

-- | Like 'ensureQueuesEff', but returns a report of actions taken.
ensureQueuesReportEff :: (Eff.Pgmq :> es) => [QueueConfig] -> Eff es [ReconcileAction]
ensureQueuesReportEff configs = do
  existingQueues <- Eff.listQueues
  existingBindings <- Eff.listTopicBindings
  existingThrottles <- Eff.listNotifyInsertThrottles

  let existingQueueNames = Set.fromList (map (\q -> q ^. #name) existingQueues)
      existingBindingSet =
        Set.fromList
          [ (b ^. #bindingQueueName, topicPatternToText (b ^. #bindingPattern))
          | b <- existingBindings
          ]
      existingNotifySet = Set.fromList (map (\t -> t ^. #throttleQueueName) existingThrottles)

  concat <$> traverse (reconcileQueueEff existingQueueNames existingBindingSet existingNotifySet) configs

-- | Apply a single queue config without checking existing state.
applyQueueConfigEff :: (Eff.Pgmq :> es) => QueueConfig -> Eff es ()
applyQueueConfigEff cfg = do
  let qn = cfg ^. #queueName
  case cfg ^. #queueType of
    StandardQueue ->
      Eff.createQueue qn
    UnloggedQueue ->
      Eff.createUnloggedQueue qn
    PartitionedQueue pc ->
      Eff.createPartitionedQueue
        StmtTypes.CreatePartitionedQueue
          { queueName = qn,
            partitionInterval = pc ^. #partitionInterval,
            retentionInterval = pc ^. #retentionInterval
          }

  case cfg ^. #notifyInsert of
    Nothing -> pure ()
    Just nc ->
      Eff.enableNotifyInsert
        StmtTypes.EnableNotifyInsert
          { queueName = qn,
            throttleIntervalMs = nc ^. #throttleMs
          }

  if cfg ^. #fifoIndex
    then Eff.createFifoIndex qn
    else pure ()

  mapM_
    ( \pat ->
        Eff.bindTopic
          StmtTypes.BindTopic
            { topicPattern = pat,
              queueName = qn
            }
    )
    (cfg ^. #topicBindings)

-- | Reconcile a single queue config against existing state.
reconcileQueueEff ::
  (Eff.Pgmq :> es) =>
  Set.Set QueueName ->
  Set.Set (T.Text, T.Text) ->
  Set.Set T.Text ->
  QueueConfig ->
  Eff es [ReconcileAction]
reconcileQueueEff existingQueues existingBindings existingNotify cfg = do
  let qn = cfg ^. #queueName
      qnText = queueNameToText qn

  queueAction <-
    if Set.member qn existingQueues
      then pure [SkippedQueue qn]
      else do
        case cfg ^. #queueType of
          StandardQueue ->
            Eff.createQueue qn
          UnloggedQueue ->
            Eff.createUnloggedQueue qn
          PartitionedQueue pc ->
            Eff.createPartitionedQueue
              StmtTypes.CreatePartitionedQueue
                { queueName = qn,
                  partitionInterval = pc ^. #partitionInterval,
                  retentionInterval = pc ^. #retentionInterval
                }
        pure [CreatedQueue qn (cfg ^. #queueType)]

  notifyAction <- case cfg ^. #notifyInsert of
    Nothing -> pure []
    Just nc ->
      if Set.member qnText existingNotify
        then pure [SkippedNotify qn]
        else do
          Eff.enableNotifyInsert
            StmtTypes.EnableNotifyInsert
              { queueName = qn,
                throttleIntervalMs = nc ^. #throttleMs
              }
          pure [EnabledNotify qn (nc ^. #throttleMs)]

  fifoAction <-
    if cfg ^. #fifoIndex
      then do
        Eff.createFifoIndex qn
        pure [CreatedFifoIndex qn]
      else pure []

  bindingActions <- concat <$> traverse (reconcileBindingEff qn qnText existingBindings) (cfg ^. #topicBindings)

  pure (queueAction ++ notifyAction ++ fifoAction ++ bindingActions)

-- | Reconcile a single topic binding.
reconcileBindingEff ::
  (Eff.Pgmq :> es) =>
  QueueName ->
  T.Text ->
  Set.Set (T.Text, T.Text) ->
  TopicPattern ->
  Eff es [ReconcileAction]
reconcileBindingEff qn qnText existingBindings pat =
  let patText = topicPatternToText pat
   in if Set.member (qnText, patText) existingBindings
        then pure [SkippedTopicBinding qn pat]
        else do
          Eff.bindTopic
            StmtTypes.BindTopic
              { topicPattern = pat,
                queueName = qn
              }
          pure [BoundTopic qn pat]
