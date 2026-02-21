module Pgmq.Hasql.Statements.Types
  ( SendMessage (..),
    SendMessageForLater (..),
    BatchSendMessage (..),
    BatchSendMessageForLater (..),
    SendMessageWithHeaders (..),
    SendMessageWithHeadersForLater (..),
    BatchSendMessageWithHeaders (..),
    BatchSendMessageWithHeadersForLater (..),
    ReadMessage (..),
    PopMessage (..),
    MessageQuery (..),
    BatchMessageQuery (..),
    VisibilityTimeoutQuery (..),
    BatchVisibilityTimeoutQuery (..),
    -- Timestamp-based VT types (pgmq 1.10.0+)
    VisibilityTimeoutAtQuery (..),
    BatchVisibilityTimeoutAtQuery (..),
    ReadWithPollMessage (..),
    EnableNotifyInsert (..),
    CreatePartitionedQueue (..),
    QueueMetrics (..),
    -- FIFO read types (pgmq 1.8.0+)
    ReadGrouped (..),
    ReadGroupedWithPoll (..),
    -- Topic types (pgmq 1.11.0+)
    BindTopic (..),
    UnbindTopic (..),
    SendTopic (..),
    SendTopicWithHeaders (..),
    BatchSendTopic (..),
    BatchSendTopicForLater (..),
    BatchSendTopicWithHeaders (..),
    BatchSendTopicWithHeadersForLater (..),
    UpdateNotifyInsert (..),
  )
where

import Data.Aeson (Value)
import Pgmq.Hasql.Prelude
import Pgmq.Types (MessageBody, MessageHeaders, MessageId, QueueName, RoutingKey, TopicPattern)

type Delay = Int32

data SendMessage = SendMessage
  { queueName :: !QueueName,
    messageBody :: !MessageBody,
    delay :: !(Maybe Delay)
  }
  deriving stock (Generic)

data VisibilityTimeoutQuery = VisibilityTimeoutQuery
  { queueName :: !QueueName,
    messageId :: !MessageId,
    visibilityTimeoutOffset :: !Int32
  }
  deriving stock (Generic)

-- | Batch visibility timeout update (pgmq 1.8.0+)
data BatchVisibilityTimeoutQuery = BatchVisibilityTimeoutQuery
  { queueName :: !QueueName,
    messageIds :: ![MessageId],
    visibilityTimeoutOffset :: !Int32
  }
  deriving stock (Generic)

-- | Set visibility timeout to absolute timestamp (pgmq 1.10.0+)
data VisibilityTimeoutAtQuery = VisibilityTimeoutAtQuery
  { queueName :: !QueueName,
    messageId :: !MessageId,
    visibilityTime :: !UTCTime
  }
  deriving stock (Generic)

-- | Batch set visibility timeout to absolute timestamp (pgmq 1.10.0+)
data BatchVisibilityTimeoutAtQuery = BatchVisibilityTimeoutAtQuery
  { queueName :: !QueueName,
    messageIds :: ![MessageId],
    visibilityTime :: !UTCTime
  }
  deriving stock (Generic)

data BatchMessageQuery = BatchMessageQuery
  { queueName :: !QueueName,
    messageIds :: ![MessageId]
  }
  deriving stock (Generic)

data MessageQuery = MessageQuery
  { queueName :: !QueueName,
    messageId :: !MessageId
  }
  deriving stock (Generic)

data SendMessageForLater = SendMessageForLater
  { queueName :: !QueueName,
    messageBody :: !MessageBody,
    scheduledAt :: !UTCTime
  }
  deriving stock (Generic)

data BatchSendMessage = BatchSendMessage
  { queueName :: !QueueName,
    messageBodies :: ![MessageBody],
    delay :: !(Maybe Delay)
  }
  deriving stock (Generic)

data BatchSendMessageForLater = BatchSendMessageForLater
  { queueName :: !QueueName,
    messageBodies :: ![MessageBody],
    scheduledAt :: !UTCTime
  }
  deriving stock (Generic)

-- | Send message with headers (pgmq 1.5.0+)
data SendMessageWithHeaders = SendMessageWithHeaders
  { queueName :: !QueueName,
    messageBody :: !MessageBody,
    messageHeaders :: !MessageHeaders,
    delay :: !(Maybe Delay)
  }
  deriving stock (Generic)

-- | Send message with headers for later (pgmq 1.5.0+)
data SendMessageWithHeadersForLater = SendMessageWithHeadersForLater
  { queueName :: !QueueName,
    messageBody :: !MessageBody,
    messageHeaders :: !MessageHeaders,
    scheduledAt :: !UTCTime
  }
  deriving stock (Generic)

-- | Batch send messages with headers (pgmq 1.5.0+)
data BatchSendMessageWithHeaders = BatchSendMessageWithHeaders
  { queueName :: !QueueName,
    messageBodies :: ![MessageBody],
    messageHeaders :: ![MessageHeaders],
    delay :: !(Maybe Delay)
  }
  deriving stock (Generic)

-- | Batch send messages with headers for later (pgmq 1.5.0+)
data BatchSendMessageWithHeadersForLater = BatchSendMessageWithHeadersForLater
  { queueName :: !QueueName,
    messageBodies :: ![MessageBody],
    messageHeaders :: ![MessageHeaders],
    scheduledAt :: !UTCTime
  }
  deriving stock (Generic)

-- | Parameters for reading messages from a queue
-- Note: conditional field added in pgmq 1.5.0
data ReadMessage = ReadMessage
  { queueName :: !QueueName,
    delay :: !Delay,
    batchSize :: !(Maybe Int32),
    -- | Optional JSONB filter (pgmq 1.5.0+)
    conditional :: !(Maybe Value)
  }
  deriving stock (Generic)

data ReadWithPollMessage = ReadWithPollMessage
  { queueName :: !QueueName,
    delay :: !Delay,
    batchSize :: !(Maybe Int32),
    maxPollSeconds :: !Int32,
    pollIntervalMs :: !Int32,
    conditional :: !(Maybe Value)
  }
  deriving stock (Generic)

-- | Parameters for popping messages from a queue (pgmq 1.7.0+)
data PopMessage = PopMessage
  { queueName :: !QueueName,
    -- | Number of messages to pop (Nothing = default 1)
    qty :: !(Maybe Int32)
  }
  deriving stock (Generic)

-- | Enable queue notifications (pgmq 1.7.0+, throttling in 1.8.0+)
data EnableNotifyInsert = EnableNotifyInsert
  { queueName :: !QueueName,
    -- | Minimum ms between notifications (Nothing = default 250ms)
    throttleIntervalMs :: !(Maybe Int32)
  }
  deriving stock (Generic)

data CreatePartitionedQueue = CreatePartitionedQueue
  { queueName :: !QueueName,
    partitionInterval :: !Text,
    retentionInterval :: !Text
  }
  deriving stock (Generic)

-- | Queue metrics returned by pgmq.metrics() and pgmq.metrics_all()
-- Note: queueVisibleLength added in pgmq 1.5.0
data QueueMetrics = QueueMetrics
  { queueName :: !Text,
    queueLength :: !Int64,
    newestMsgAgeSec :: !(Maybe Int32),
    oldestMsgAgeSec :: !(Maybe Int32),
    totalMessages :: !Int64,
    scrapeTime :: !UTCTime,
    -- | Count of messages available for reading (pgmq 1.5.0+)
    queueVisibleLength :: !Int64
  }
  deriving stock (Generic, Show)

-- | Parameters for FIFO grouped read (pgmq 1.8.0+)
-- Used for both read_grouped and read_grouped_rr functions.
-- Note: conditional parameter was removed in pgmq 1.9.0 (commit 9e9c3dc)
data ReadGrouped = ReadGrouped
  { queueName :: !QueueName,
    visibilityTimeout :: !Int32,
    qty :: !Int32
  }
  deriving stock (Generic)

-- | Parameters for FIFO grouped read with polling (pgmq 1.8.0+)
-- Used for both read_grouped_with_poll and read_grouped_rr_with_poll functions.
-- Note: conditional parameter was removed in pgmq 1.9.0 (commit 9e9c3dc)
data ReadGroupedWithPoll = ReadGroupedWithPoll
  { queueName :: !QueueName,
    visibilityTimeout :: !Int32,
    qty :: !Int32,
    maxPollSeconds :: !Int32,
    pollIntervalMs :: !Int32
  }
  deriving stock (Generic)

-- | Bind a topic pattern to a queue (pgmq 1.11.0+)
data BindTopic = BindTopic
  { topicPattern :: !TopicPattern,
    queueName :: !QueueName
  }
  deriving stock (Generic)

-- | Unbind a topic pattern from a queue (pgmq 1.11.0+)
data UnbindTopic = UnbindTopic
  { topicPattern :: !TopicPattern,
    queueName :: !QueueName
  }
  deriving stock (Generic)

-- | Send a message via topic routing (pgmq 1.11.0+)
data SendTopic = SendTopic
  { routingKey :: !RoutingKey,
    messageBody :: !MessageBody,
    delay :: !(Maybe Delay)
  }
  deriving stock (Generic)

-- | Send a message via topic routing with headers (pgmq 1.11.0+)
data SendTopicWithHeaders = SendTopicWithHeaders
  { routingKey :: !RoutingKey,
    messageBody :: !MessageBody,
    messageHeaders :: !MessageHeaders,
    delay :: !(Maybe Delay)
  }
  deriving stock (Generic)

-- | Batch send messages via topic routing (pgmq 1.11.0+)
data BatchSendTopic = BatchSendTopic
  { routingKey :: !RoutingKey,
    messageBodies :: ![MessageBody],
    delay :: !(Maybe Delay)
  }
  deriving stock (Generic)

-- | Batch send messages via topic routing for later (pgmq 1.11.0+)
data BatchSendTopicForLater = BatchSendTopicForLater
  { routingKey :: !RoutingKey,
    messageBodies :: ![MessageBody],
    scheduledAt :: !UTCTime
  }
  deriving stock (Generic)

-- | Batch send messages via topic routing with headers (pgmq 1.11.0+)
data BatchSendTopicWithHeaders = BatchSendTopicWithHeaders
  { routingKey :: !RoutingKey,
    messageBodies :: ![MessageBody],
    messageHeaders :: ![MessageHeaders],
    delay :: !(Maybe Delay)
  }
  deriving stock (Generic)

-- | Batch send messages via topic routing with headers for later (pgmq 1.11.0+)
data BatchSendTopicWithHeadersForLater = BatchSendTopicWithHeadersForLater
  { routingKey :: !RoutingKey,
    messageBodies :: ![MessageBody],
    messageHeaders :: ![MessageHeaders],
    scheduledAt :: !UTCTime
  }
  deriving stock (Generic)

-- | Update notification throttle interval (pgmq 1.11.0+)
data UpdateNotifyInsert = UpdateNotifyInsert
  { queueName :: !QueueName,
    throttleIntervalMs :: !Int32
  }
  deriving stock (Generic)
