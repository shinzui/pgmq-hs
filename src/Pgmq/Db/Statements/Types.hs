module Pgmq.Db.Statements.Types
  ( SendMessage (..),
    SendMessageForLater (..),
  )
where

import Pgmq.Prelude
import Pgmq.Types (MessageBody, QueueName)

type Delay = Int32

data SendMessage = SendMessage
  { queueName :: !QueueName,
    messageBody :: !MessageBody,
    delay :: !(Maybe Delay)
  }
  deriving stock (Generic)

data SendMessageForLater = SendMessageForLater
  { queueName :: !QueueName,
    messageBody :: !MessageBody,
    scheduledAt :: !UTCTime
  }
  deriving stock (Generic)
