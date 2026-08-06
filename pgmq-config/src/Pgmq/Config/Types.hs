-- | The declarative vocabulary: what a queue topology looks like as Haskell
-- values, and what a reconciliation run reports back.
--
-- t'QueueConfig' is the declaration; 'ReconcileAction' is the report. Everything
-- here is re-exported from "Pgmq.Config", which is the module to import.
module Pgmq.Config.Types
  ( -- * Queue Configuration
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

    -- * Reconciliation Report
    ReconcileAction (..),
    ObservedQueueType (..),

    -- * Defaults
    defaultThrottleMs,
  )
where

import Control.Lens ((%~), (&))
import Data.Generics.Labels ()
import Data.Int (Int32)
import Data.Text (Text)
import GHC.Generics (Generic)
import Pgmq.Types (QueueName, TopicPattern)

-- | Describes the desired state of a single pgmq queue.
data QueueConfig = QueueConfig
  { queueName :: !QueueName,
    queueType :: !QueueType,
    notifyInsert :: !(Maybe NotifyConfig),
    fifoIndex :: !Bool,
    topicBindings :: ![TopicPattern]
  }
  deriving stock (Generic, Show)

-- | The type of queue to create.
data QueueType
  = -- | A standard queue with write-ahead logging.
    StandardQueue
  | -- | An unlogged queue — faster writes, but data is lost on crash.
    UnloggedQueue
  | -- | A partitioned queue for high-throughput scenarios.
    PartitionedQueue !PartitionConfig
  deriving stock (Show)

-- | Configuration for a partitioned queue.
data PartitionConfig = PartitionConfig
  { partitionInterval :: !Text,
    retentionInterval :: !Text
  }
  deriving stock (Generic, Show)

-- | Configuration for insert notifications (LISTEN/NOTIFY).
data NotifyConfig = NotifyConfig
  { -- | Minimum milliseconds between notifications. Nothing uses the documented
    -- pgmq default (250 ms), applied via COALESCE in the pgmq-hasql statement so
    -- SQL NULL never reaches the function.
    throttleMs :: !(Maybe Int32)
  }
  deriving stock (Generic, Show)

-- | The queue shape actually observed in the database.
--
-- @pgmq.list_queues()@ reports two booleans per queue, partitioned and
-- unlogged, which describe exactly these three states. It reports nothing about
-- a partitioned queue's interval or retention settings, so those are not
-- drift-checked — see 'DetectedQueueTypeDrift'.
data ObservedQueueType
  = ObservedStandard
  | ObservedUnlogged
  | ObservedPartitioned
  deriving stock (Eq, Show)

-- | An action taken (or skipped) during queue reconciliation.
data ReconcileAction
  = -- | The queue did not exist and was created with the declared type.
    CreatedQueue !QueueName !QueueType
  | -- | No throttle row existed, so insert notifications were enabled with the
    -- declared interval ('Nothing' meaning 'defaultThrottleMs').
    EnabledNotify !QueueName !(Maybe Int32)
  | -- | The FIFO headers index did not exist and was created.
    CreatedFifoIndex !QueueName
  | -- | The topic binding did not exist and was created.
    BoundTopic !QueueName !TopicPattern
  | -- | A queue with this name already existed and its observed type matches
    -- what was declared. Nothing was issued.
    SkippedQueue !QueueName
  | -- | A throttle row already existed with the declared interval. Nothing was
    -- issued — in particular the row was /not/ re-enabled, which would reset
    -- its @last_notified_at@.
    SkippedNotify !QueueName
  | -- | The FIFO headers index already existed; nothing was issued.
    SkippedFifoIndex !QueueName
  | -- | The topic binding already existed; nothing was issued.
    SkippedTopicBinding !QueueName !TopicPattern
  | -- | The declared throttle interval differed from the database row, so the
    -- row was updated in place via @pgmq.update_notify_insert@. Fields: queue,
    -- observed interval, declared interval (in milliseconds).
    --
    -- This is the reconciler's only mutation of already-existing state. The
    -- update also resets the throttle's @last_notified_at@ to the epoch, so the
    -- next insert on that queue notifies immediately; that is a property of
    -- @pgmq.update_notify_insert@ itself, and it happens at most once per real
    -- configuration change.
    UpdatedNotifyThrottle !QueueName !Int32 !Int32
  | -- | The queue exists but its observed shape contradicts the declared one.
    -- Fields: queue, declared type, observed type.
    --
    -- Nothing was mutated and nothing will be: converting a queue between
    -- standard, unlogged, and partitioned means dropping and recreating it,
    -- destroying every message it holds, which a startup reconciler must never
    -- do. Resolving the drift is an operator decision. This action replaces
    -- 'SkippedQueue' for the queue it concerns, so the report still carries
    -- exactly one queue-existence action per declared config.
    --
    -- Only the three-way shape is compared. A declared 'PartitionedQueue'
    -- against an observed partitioned queue matches regardless of its interval
    -- and retention settings, because @pgmq.list_queues()@ does not report
    -- them.
    DetectedQueueTypeDrift !QueueName !QueueType !ObservedQueueType
  deriving stock (Show)

-- | The throttle interval pgmq applies when none is given: 250 milliseconds.
--
-- A t'NotifyConfig' whose @throttleMs@ is 'Nothing' means \"use this value\".
-- The pgmq-hasql enable statement supplies it with a SQL @coalesce($2, 250)@,
-- and @pgmq.enable_notify_insert@ declares the same figure as its parameter
-- default, so a 'Nothing' config and a stored 250 agree and reconciliation does
-- not flap between them.
defaultThrottleMs :: Int32
defaultThrottleMs = 250

-- | Create a standard queue configuration with no extras.
standardQueue :: QueueName -> QueueConfig
standardQueue qn =
  QueueConfig
    { queueName = qn,
      queueType = StandardQueue,
      notifyInsert = Nothing,
      fifoIndex = False,
      topicBindings = []
    }

-- | Create an unlogged queue configuration (faster, no WAL, lost on crash).
unloggedQueue :: QueueName -> QueueConfig
unloggedQueue qn =
  (standardQueue qn) {queueType = UnloggedQueue}

-- | Create a partitioned queue configuration.
partitionedQueue :: QueueName -> PartitionConfig -> QueueConfig
partitionedQueue qn pc =
  (standardQueue qn) {queueType = PartitionedQueue pc}

-- | Enable LISTEN/NOTIFY on message insert.
withNotifyInsert :: Maybe Int32 -> QueueConfig -> QueueConfig
withNotifyInsert ms cfg =
  cfg {notifyInsert = Just NotifyConfig {throttleMs = ms}}

-- | Add a FIFO index for strict message ordering.
withFifoIndex :: QueueConfig -> QueueConfig
withFifoIndex cfg = cfg {fifoIndex = True}

-- | Bind a topic pattern for AMQP-style routing.
withTopicBinding :: TopicPattern -> QueueConfig -> QueueConfig
withTopicBinding pat cfg =
  cfg & #topicBindings %~ (++ [pat])
