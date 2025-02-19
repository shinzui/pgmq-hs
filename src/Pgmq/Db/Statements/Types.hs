module Pgmq.Db.Statements.Types
  ( SendMessage (..),
    SendMessageForLater (..),
    BatchSendMessage (..),
    BatchSendMessageForLater (..),
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
import Pgmq.Types (MessageBody, MessageId, QueueName)

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

data QueueMetrics = QueueMetrics
  { queueName :: !Text,
    queueLength :: !Int64,
    newestMsgAgeSec :: !(Maybe Int32),
    oldestMsgAgeSec :: !(Maybe Int32),
    totalMessages :: !Int64,
    scrapeTime :: !UTCTime
  }
  deriving stock (Generic, Show)
