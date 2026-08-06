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

import Hasql.Pool qualified as Pool
import Hasql.Session (Session)
import Pgmq.Config.Reconcile (ReconcileOps (..), ensureQueuesReportWith)
import Pgmq.Config.Types
import Pgmq.Hasql.Sessions qualified as Sessions

-- | The 'Session'-backed wiring of the reconciliation operations.
sessionOps :: ReconcileOps Session
sessionOps =
  ReconcileOps
    { listQueues = Sessions.listQueues,
      listTopicBindings = Sessions.listTopicBindings,
      listNotifyInsertThrottles = Sessions.listNotifyInsertThrottles,
      createQueue = Sessions.createQueue,
      createUnloggedQueue = Sessions.createUnloggedQueue,
      createPartitionedQueue = Sessions.createPartitionedQueue,
      enableNotifyInsert = Sessions.enableNotifyInsert,
      createFifoIndex = Sessions.createFifoIndex,
      bindTopic = Sessions.bindTopic
    }

-- | Ensure all declared queues exist with the desired settings.
--
-- Queries existing queues, topic bindings, and notification throttles first,
-- and only issues mutating calls for items that are missing. Safe to call on
-- every application startup: a second run on an unchanged config is a no-op
-- modulo the three list queries.
--
-- Operations are additive only: queues not in the config are left untouched.
ensureQueues :: [QueueConfig] -> Session ()
ensureQueues configs = () <$ ensureQueuesReport configs

-- | Convenience wrapper that runs 'ensureQueues' against a connection pool.
ensureQueuesWithPool :: Pool.Pool -> [QueueConfig] -> IO (Either Pool.UsageError ())
ensureQueuesWithPool pool configs =
  Pool.use pool (ensureQueues configs)

-- | Like 'ensureQueues', but returns a report of actions taken.
-- Queries existing state first and skips operations that are already satisfied.
ensureQueuesReport :: [QueueConfig] -> Session [ReconcileAction]
ensureQueuesReport = ensureQueuesReportWith sessionOps
