module Pgmq.Types where

import Data.Aeson (Value)
import Pgmq.Prelude

newtype MessageBody = MessageBody {unMessageBody :: Value}
  deriving newtype (Eq, Ord, FromJSON, ToJSON)
  deriving stock (Show, Generic)

newtype MessageId = MessageId {unMessageId :: Int64}
  deriving newtype (Eq, Ord, FromJSON, ToJSON)
  deriving stock (Show, Generic)

data Message = Message
  { messageId :: !MessageId,
    visibilityTime :: !UTCTime,
    enqueuedAt :: !UTCTime,
    readCount :: !Int32,
    body :: !MessageBody
  }
  deriving stock (Eq, Generic, Show)
