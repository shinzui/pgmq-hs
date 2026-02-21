{-# LANGUAGE TemplateHaskellQuotes #-}

module Pgmq.Types
  ( MessageBody (..),
    MessageHeaders (..),
    MessageId (..),
    Message (..),
    Queue (..),
    QueueName,
    parseQueueName,
    queueNameToText,
    PgmqError (..),

    -- * Topic Routing (pgmq 1.11.0+)
    RoutingKey,
    parseRoutingKey,
    routingKeyToText,
    TopicPattern,
    parseTopicPattern,
    topicPatternToText,
    TopicBinding (..),
    RoutingMatch (..),
    TopicSendResult (..),
    NotifyInsertThrottle (..),
  )
where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Char (isAlphaNum, isAscii)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift (..))

newtype MessageBody = MessageBody {unMessageBody :: Value}
  deriving newtype (Eq, Ord, FromJSON, ToJSON)
  deriving stock (Show, Generic)

-- | Message headers for metadata (routing, tracing, etc.)
-- Added in pgmq 1.5.0
newtype MessageHeaders = MessageHeaders {unMessageHeaders :: Value}
  deriving newtype (Eq, Ord, FromJSON, ToJSON)
  deriving stock (Show, Generic)

newtype MessageId = MessageId {unMessageId :: Int64}
  deriving newtype (Eq, Ord, FromJSON, ToJSON)
  deriving stock (Show, Generic)

data Queue = Queue
  { name :: !QueueName,
    createdAt :: !UTCTime,
    isPartitioned :: !Bool,
    isUnlogged :: !Bool
  }
  deriving stock (Eq, Generic, Show)

-- | https://tembo.io/pgmq/api/sql/types/
-- Note: headers field added in pgmq 1.5.0
-- Note: lastReadAt field added in pgmq 1.10.0
data Message = Message
  { messageId :: !MessageId,
    visibilityTime :: !UTCTime,
    enqueuedAt :: !UTCTime,
    lastReadAt :: !(Maybe UTCTime),
    readCount :: !Int64,
    body :: !MessageBody,
    headers :: !(Maybe Value)
  }
  deriving stock (Eq, Generic, Show)

newtype QueueName = QueueName Text
  deriving newtype (Eq, Ord, FromJSON, ToJSON)
  deriving stock (Show, Generic)

instance Lift QueueName where
  lift (QueueName t) = [|QueueName t|]
  liftTyped (QueueName t) = [||QueueName t||]

queueNameToText :: QueueName -> Text
queueNameToText (QueueName t) = t

data PgmqError
  = InvalidQueueName Text
  | InvalidRoutingKey Text
  | InvalidTopicPattern Text
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

-- | A validated routing key for topic-based message routing (pgmq 1.11.0+)
-- Routing keys are dot-separated segments of alphanumeric characters, hyphens,
-- and underscores. Max 255 characters. No wildcards allowed.
newtype RoutingKey = RoutingKey Text
  deriving newtype (Eq, Ord, FromJSON, ToJSON)
  deriving stock (Show, Generic)

parseRoutingKey :: Text -> Either PgmqError RoutingKey
parseRoutingKey t
  | T.null t = Left $ InvalidRoutingKey "Routing key cannot be empty."
  | T.length t > 255 = Left $ InvalidRoutingKey "Routing key exceeds 255 characters."
  | not (T.all isValidChar t) = Left $ InvalidRoutingKey "Routing key contains invalid characters."
  | otherwise = Right $ RoutingKey t
  where
    isValidChar c = (isAscii c && isAlphaNum c) || c == '.' || c == '-' || c == '_'

routingKeyToText :: RoutingKey -> Text
routingKeyToText (RoutingKey t) = t

-- | A topic pattern for binding to queues (pgmq 1.11.0+)
-- Patterns support wildcards: '*' matches one segment, '#' matches zero or more.
newtype TopicPattern = TopicPattern Text
  deriving newtype (Eq, Ord, FromJSON, ToJSON)
  deriving stock (Show, Generic)

parseTopicPattern :: Text -> Either PgmqError TopicPattern
parseTopicPattern t
  | T.null t = Left $ InvalidTopicPattern "Topic pattern cannot be empty."
  | T.length t > 255 = Left $ InvalidTopicPattern "Topic pattern exceeds 255 characters."
  | otherwise = Right $ TopicPattern t

topicPatternToText :: TopicPattern -> Text
topicPatternToText (TopicPattern t) = t

-- | A topic binding record returned by list_topic_bindings (pgmq 1.11.0+)
data TopicBinding = TopicBinding
  { bindingPattern :: !TopicPattern,
    bindingQueueName :: !Text,
    bindingBoundAt :: !UTCTime,
    bindingCompiledRegex :: !Text
  }
  deriving stock (Eq, Generic, Show)

-- | A routing match result from test_routing (pgmq 1.11.0+)
data RoutingMatch = RoutingMatch
  { matchPattern :: !TopicPattern,
    matchQueueName :: !Text,
    matchCompiledRegex :: !Text
  }
  deriving stock (Eq, Generic, Show)

-- | Result row from send_batch_topic (pgmq 1.11.0+)
data TopicSendResult = TopicSendResult
  { sentToQueue :: !Text,
    sentMessageId :: !MessageId
  }
  deriving stock (Eq, Generic, Show)

-- | Notification throttle settings returned by list_notify_insert_throttles (pgmq 1.11.0+)
data NotifyInsertThrottle = NotifyInsertThrottle
  { throttleQueueName :: !Text,
    throttleIntervalMs :: !Int32,
    throttleLastNotifiedAt :: !UTCTime
  }
  deriving stock (Eq, Generic, Show)
