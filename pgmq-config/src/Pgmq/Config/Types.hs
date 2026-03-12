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
  { -- | Minimum milliseconds between notifications. Nothing uses pgmq default (250ms).
    throttleMs :: !(Maybe Int32)
  }
  deriving stock (Generic, Show)

-- | An action taken (or skipped) during queue reconciliation.
data ReconcileAction
  = CreatedQueue !QueueName !QueueType
  | EnabledNotify !QueueName !(Maybe Int32)
  | CreatedFifoIndex !QueueName
  | BoundTopic !QueueName !TopicPattern
  | SkippedQueue !QueueName
  | SkippedNotify !QueueName
  | SkippedFifoIndex !QueueName
  | SkippedTopicBinding !QueueName !TopicPattern
  deriving stock (Show)

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
