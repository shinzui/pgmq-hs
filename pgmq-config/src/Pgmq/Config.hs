-- | Declare a pgmq queue topology as Haskell values and create whatever is
-- missing, in one call at application startup.
--
-- Build configs with 'standardQueue', 'unloggedQueue', or 'partitionedQueue'
-- and refine them with 'withNotifyInsert', 'withFifoIndex', and
-- 'withTopicBinding'; then hand the list to 'ensureQueues' (or
-- 'ensureQueuesWithPool'), or to 'ensureQueuesReport' when you want to see what
-- was done. 'ensureQueues' documents the reconciliation contract in full —
-- what is created, the one case in which existing state is mutated, what is
-- reported as drift instead of repaired, and the concurrent-startup caveat.
--
-- The same reconciler is available over the @Pgmq@ effect in
-- "Pgmq.Config.Effectful".
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
    ObservedQueueType (..),
    ensureQueuesReport,

    -- * Defaults
    defaultThrottleMs,
  )
where

import Hasql.Pool qualified as Pool
import Hasql.Session (Session)
import Pgmq.Config.Reconcile (ReconcileOps (..), ensureQueuesReportWith)
import Pgmq.Config.Types
import Pgmq.Hasql.Sessions qualified as Sessions

-- | The 'Session'-backed wiring of the reconciliation operations.
sessionOps :: ReconcileOps Session
sessionOps =
  ReconcileOps
    { listQueuesUnvalidated = Sessions.listQueuesUnvalidated,
      listTopicBindings = Sessions.listTopicBindings,
      listNotifyInsertThrottles = Sessions.listNotifyInsertThrottles,
      createQueue = Sessions.createQueue,
      createUnloggedQueue = Sessions.createUnloggedQueue,
      createPartitionedQueue = Sessions.createPartitionedQueue,
      enableNotifyInsert = Sessions.enableNotifyInsert,
      createFifoIndex = Sessions.createFifoIndex,
      bindTopic = Sessions.bindTopic,
      listFifoIndexQueueNames = Sessions.listFifoIndexQueueNames,
      updateNotifyInsert = Sessions.updateNotifyInsert
    }

-- | Create whatever the declared configs call for that does not exist yet.
--
-- Reconciliation is /additive/. It snapshots existing queues, topic bindings,
-- notification throttles, and FIFO indexes with four read-only queries, then
-- issues mutating calls only for the pieces that are missing: it creates
-- queues, enables insert notifications, creates FIFO indexes, and binds topic
-- patterns. It never drops, converts, or disables anything, and a queue that
-- exists in the database but not in the config is left completely alone.
--
-- There is exactly one exception, and it is deliberate: if a config declares a
-- notification throttle interval that differs from the one stored in the
-- database, the stored value is updated in place via
-- @pgmq.update_notify_insert@ and reported as
-- 'Pgmq.Config.Types.UpdatedNotifyThrottle'. That update also resets the
-- throttle's @last_notified_at@ to the epoch, so the next insert on that queue
-- raises a notification immediately — once, after a genuine configuration
-- change. A throttle whose interval already matches is not touched at all; in
-- particular it is not re-enabled, because re-enabling would reset the same
-- timestamp on every startup. @throttleMs = Nothing@ compares equal to
-- 'defaultThrottleMs', so a defaulted config does not flap.
--
-- What is /not/ reconciled:
--
-- * A queue whose observed shape (standard, unlogged, or partitioned)
--   contradicts the declared one is reported as
--   'Pgmq.Config.Types.DetectedQueueTypeDrift' and nothing is mutated.
--   Changing a queue's type means dropping and recreating it, destroying every
--   message it holds; that is an operator's decision, not a startup task.
--
-- * A partitioned queue's partition interval and retention interval are never
--   compared, because @pgmq.list_queues()@ does not report them. Only the
--   three-way shape is checked.
--
-- Calling this on every application startup is the intended usage, and a second
-- run against an unchanged config issues no mutations at all. One caveat about
-- /concurrent/ startups: queue creation and topic binding are serialized
-- server-side and converge, but enabling insert notifications on a
-- brand-new queue from two replicas at once can fail one of them with SQLSTATE
-- 42710 (@duplicate_object@) when pgmq was installed as the stock upstream
-- 1.11.0 extension. Databases installed through this repository's
-- @pgmq-migration@ package are free of that race — its migration
-- @0003-notify-crash-safety-and-locking.sql@ takes a per-queue advisory lock
-- inside @enable_notify_insert@. On an extension install, either retry the
-- reconcile (every operation is convergent, so a retry succeeds) or serialize
-- startup reconciliation across replicas. FIFO index creation has the same
-- narrow shape of race and is likewise harmless on retry, since the underlying
-- statement is @CREATE INDEX IF NOT EXISTS@.
--
-- Reconciliation is not wrapped in a transaction: each call autocommits, so a
-- failure part-way leaves the work done so far in place and the next run
-- continues from there.
ensureQueues :: [QueueConfig] -> Session ()
ensureQueues configs = () <$ ensureQueuesReport configs

-- | Convenience wrapper that runs 'ensureQueues' against a connection pool.
ensureQueuesWithPool :: Pool.Pool -> [QueueConfig] -> IO (Either Pool.UsageError ())
ensureQueuesWithPool pool configs =
  Pool.use pool (ensureQueues configs)

-- | Like 'ensureQueues', but returns a report of what was done.
--
-- The contract is identical — see 'ensureQueues' for the full description of
-- what is and is not reconciled, and for the concurrent-startup caveat. The
-- report contains one action per decision the reconciler made, in declaration
-- order: exactly one queue-existence action per config
-- ('Pgmq.Config.Types.CreatedQueue', 'Pgmq.Config.Types.SkippedQueue', or
-- 'Pgmq.Config.Types.DetectedQueueTypeDrift'), then the notify, FIFO, and
-- topic-binding actions for that config. Every @Skipped@ action means no
-- statement was issued.
ensureQueuesReport :: [QueueConfig] -> Session [ReconcileAction]
ensureQueuesReport = ensureQueuesReportWith sessionOps
