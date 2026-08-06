-- | The reconciler of "Pgmq.Config", run over the @Pgmq@ effect from
-- pgmq-effectful instead of a @Hasql.Session.Session@.
--
-- Declaration types and the report type live in "Pgmq.Config.Types"; the
-- reconciliation contract is documented on 'Pgmq.Config.ensureQueues'. This
-- module is only built when the package's @effectful@ flag is on (it is by
-- default).
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
      bindTopic = Eff.bindTopic,
      listFifoIndexQueueNames = Eff.listFifoIndexQueueNames,
      updateNotifyInsert = Eff.updateNotifyInsert
    }

-- | Create whatever the declared configs call for that does not exist yet,
-- through the @Pgmq@ effect.
--
-- This runs the very same reconciler as 'Pgmq.Config.ensureQueues', over the
-- effect instead of a @Session@, so the contract is identical and is documented
-- once, there. In short: reconciliation is additive — it creates missing
-- queues, notification settings, FIFO indexes, and topic bindings, and never
-- drops or converts anything — with one deliberate exception, a declared
-- notification throttle interval that differs from the stored one, which is
-- updated in place. Queue-type drift is reported, not repaired.
--
-- Read 'Pgmq.Config.ensureQueues' before relying on this at startup: it covers
-- the throttle update's @last_notified_at@ side effect, what is deliberately
-- left unchecked, and the concurrent multi-replica caveat (SQLSTATE 42710 on
-- stock upstream-1.11.0 extension installs).
ensureQueuesEff :: (Eff.Pgmq :> es) => [QueueConfig] -> Eff es ()
ensureQueuesEff configs = () <$ ensureQueuesReportEff configs

-- | Like 'ensureQueuesEff', but returns a report of what was done.
--
-- Same report shape as 'Pgmq.Config.ensureQueuesReport': one action per
-- decision, one queue-existence action per config, and every @Skipped@ action
-- meaning no statement was issued.
ensureQueuesReportEff :: (Eff.Pgmq :> es) => [QueueConfig] -> Eff es [ReconcileAction]
ensureQueuesReportEff = ensureQueuesReportWith effectfulOps
