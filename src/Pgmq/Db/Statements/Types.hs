module Pgmq.Db.Statements.Types
  ( SendMessage (..),
    SendMessageForLater (..),
    BatchSendMessage (..),
    BatchSendMessageForLater (..),
    SendMessageWithHeaders (..),
    SendMessageWithHeadersForLater (..),
    BatchSendMessageWithHeaders (..),
    BatchSendMessageWithHeadersForLater (..),
    ReadMessage (..),
    MessageQuery (..),
    BatchMessageQuery (..),
    VisibilityTimeoutQuery (..),
    ReadWithPollMessage (..),
    CreatePartitionedQueue (..),
    QueueMetrics (..),
  )
where

import Data.Aeson (Value)
import Pgmq.Prelude
import Pgmq.Types (MessageBody, MessageHeaders, MessageId, QueueName)

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

data ReadMessage = ReadMessage
  { queueName :: !QueueName,
    delay :: !Delay,
    batchSize :: !(Maybe Int32)
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
