module Pgmq.Types
  ( MessageBody (..),
    MessageId (..),
    Message (..),
    QueueName,
    parseQueueName,
    queueNameToText,
  )
where

import Data.Aeson (Value)
import Data.Char (isAlphaNum, isAscii)
import Data.Text qualified as T
import Pgmq.Prelude

newtype MessageBody = MessageBody {unMessageBody :: Value}
  deriving newtype (Eq, Ord, FromJSON, ToJSON)
  deriving stock (Show, Generic)

newtype MessageId = MessageId {unMessageId :: Int64}
  deriving newtype (Eq, Ord, FromJSON, ToJSON)
  deriving stock (Show, Generic)

-- | https://tembo.io/pgmq/api/sql/types/
data Message = Message
  { messageId :: !MessageId,
    visibilityTime :: !UTCTime,
    enqueuedAt :: !UTCTime,
    readCount :: !Int64,
    body :: !MessageBody
  }
  deriving stock (Eq, Generic, Show)

newtype QueueName = QueueName Text
  deriving newtype (Eq, Ord, FromJSON, ToJSON)
  deriving stock (Show, Generic)

queueNameToText :: QueueName -> Text
queueNameToText (QueueName t) = t

newtype PgmqError = InvalidQueueName Text
  deriving stock (Show, Generic)

-- Adopted from https://github.com/tembo-io/pgmq/blob/e4d4b84bf302df77be2d1f877c5cf8ef8861bfc7/pgmq-rs/src/util.rs#L94
parseQueueName :: Text -> Either PgmqError QueueName
parseQueueName t
  | not isShortEnough = Left $ InvalidQueueName "The queue name is too long."
  | not hasValidCharacters = Left $ InvalidQueueName "The queue name contains invalid characters."
  | otherwise = Right $ QueueName t
  where
    isShortEnough = T.length t <= maxQueueNameLength
    hasValidCharacters = T.all isValidChar t
    isValidChar c = (isAscii c && isAlphaNum c) || c == '_'

    -- PostgreSQL identifier length information
    -- https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-SYNTAX-IDENTIFIERS
    maxIdentifierLength = 63 -- PostgreSQL truncates beyond this length
    longestPrefix :: Text = "archived_at_idx_"
    maxQueueNameLength = maxIdentifierLength - T.length longestPrefix
