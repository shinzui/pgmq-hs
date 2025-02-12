module Pgmq.Db.Statements.Types where

import Pgmq.Prelude
import Pgmq.Types (MessageBody, QueueName)

type Delay = Int32

data QueueMessage = QueueMessage
  { queueName :: !QueueName,
    messageBody :: !MessageBody,
    delay :: !(Maybe Delay)
  }
  deriving stock (Generic)

data QueueMessageForLater = QueueMessageForLater
  { queueName :: !QueueName,
    messageBody :: !MessageBody,
    scheduledAt :: !UTCTime
  }
  deriving stock (Generic)
