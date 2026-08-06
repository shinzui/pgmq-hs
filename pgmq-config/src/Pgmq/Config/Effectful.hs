module Pgmq.Config.Effectful
  ( -- * Reconciliation
    ensureQueuesEff,

    -- * Reconciliation with Report
    ensureQueuesReportEff,
  )
where

import Effectful (Eff, (:>))
import Pgmq.Config.Reconcile (ReconcileOps (..), ensureQueuesReportWith)
import Pgmq.Config.Types
import Pgmq.Effectful.Effect qualified as Eff

-- | The @Pgmq@-effect-backed wiring of the reconciliation operations.
effectfulOps :: (Eff.Pgmq :> es) => ReconcileOps (Eff es)
effectfulOps =
  ReconcileOps
    { listQueuesUnvalidated = Eff.listQueuesUnvalidated,
      listTopicBindings = Eff.listTopicBindings,
      listNotifyInsertThrottles = Eff.listNotifyInsertThrottles,
      createQueue = Eff.createQueue,
      createUnloggedQueue = Eff.createUnloggedQueue,
      createPartitionedQueue = Eff.createPartitionedQueue,
      enableNotifyInsert = Eff.enableNotifyInsert,
      createFifoIndex = Eff.createFifoIndex,
      bindTopic = Eff.bindTopic
    }

-- | Ensure all declared queues exist using the Pgmq effect.
--
-- Queries existing queues, topic bindings, and notification throttles first,
-- and only issues mutating calls for items that are missing. Safe to call on
-- every application startup: a second run on an unchanged config is a no-op
-- modulo the three list queries.
--
-- Operations are additive only: queues not in the config are left untouched.
ensureQueuesEff :: (Eff.Pgmq :> es) => [QueueConfig] -> Eff es ()
ensureQueuesEff configs = () <$ ensureQueuesReportEff configs

-- | Like 'ensureQueuesEff', but returns a report of actions taken.
ensureQueuesReportEff :: (Eff.Pgmq :> es) => [QueueConfig] -> Eff es [ReconcileAction]
ensureQueuesReportEff = ensureQueuesReportWith effectfulOps
