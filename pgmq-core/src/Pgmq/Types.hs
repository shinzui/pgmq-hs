{-# LANGUAGE TemplateHaskellQuotes #-}

module Pgmq.Types
  ( MessageBody (..),
    MessageHeaders (..),
    MessageId (..),
    Message (..),
    ArchivedMessage (..),
    Queue (..),
    UnvalidatedQueue (..),
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

    -- * Notifications (pgmq 1.11.0+)
    NotifyInsertThrottle (..),
    notifyChannelName,
  )
where

import Data.Aeson (Encoding, FromJSON (..), KeyValue, Series, ToJSON (..), Value, object, pairs, withObject, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Char (isAlphaNum, isAscii, isDigit, isLower)
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

-- | A row of @pgmq.list_queues()@ with the queue name left unvalidated.
--
-- Queues are created by every client that shares the database, and the
-- server accepts names 'parseQueueName' rejects (its only check is length).
-- This shape exists so state inspection — notably pgmq-config's reconciler —
-- can observe such foreign queues without failing to decode them.
-- 'unvalidatedName' may therefore hold any server-accepted name; do not feed
-- it into APIs expecting a validated 'QueueName' without going through
-- 'parseQueueName'.
data UnvalidatedQueue = UnvalidatedQueue
  { unvalidatedName :: !Text,
    unvalidatedCreatedAt :: !UTCTime,
    unvalidatedIsPartitioned :: !Bool,
    unvalidatedIsUnlogged :: !Bool
  }
  deriving stock (Eq, Generic, Show)

-- | https://pgmq.github.io/pgmq/api/sql/types/
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

-- | A row of an archive table @pgmq.a_\<queue\>@: the message exactly as it was
-- when @archiveMessage@ (or @pgmq.archive@ called by any client) moved it out of
-- the queue table, plus the archival timestamp the archive table stamped.
--
-- Archive tables are only ever read by the non-destructive inspection reads
-- (@peekArchivedMessages@, @lookupArchivedMessage@ in pgmq-hasql); pgmq itself
-- never reads them back. See @docs/design/019-non-destructive-inspection-reads.md@.
data ArchivedMessage = ArchivedMessage
  { archivedMessage :: !Message,
    archivedAt :: !UTCTime
  }
  deriving stock (Eq, Generic, Show)

newtype QueueName = QueueName Text
  deriving newtype (Eq, Ord, ToJSON)
  deriving stock (Show, Generic)

instance Lift QueueName where
  lift (QueueName t) = [|QueueName t|]
  liftTyped (QueueName t) = [||QueueName t||]

-- | Validates via 'parseQueueName', so JSON- and config-loaded names get
-- exactly the same checks as programmatic construction. A derived instance
-- would bypass the smart constructor entirely.
instance FromJSON QueueName where
  parseJSON = Aeson.withText "QueueName" $ \t ->
    either (fail . show) pure (parseQueueName t)

queueNameToText :: QueueName -> Text
queueNameToText (QueueName t) = t

data PgmqError
  = InvalidQueueName Text
  | InvalidRoutingKey Text
  | InvalidTopicPattern Text
  deriving stock (Show, Generic)

-- | Parse a queue name: non-empty, at most 47 characters, drawn from lowercase
-- ASCII letters, digits, and underscore only.
--
-- Lowercase-only is a correctness requirement, not a style choice. pgmq's SQL
-- lowercases /physical/ table names (@pgmq.format_table_name@) but stores the
-- caller's /original/ casing in @pgmq.meta@, and the notification trigger looks
-- up the /lowercased/ name extracted from the physical table. A mixed-case name
-- therefore aliases: @MyQueue@ and @myqueue@ are two metadata identities
-- sharing one physical table (interleaved messages; dropping either destroys
-- the other's data), and notification throttles configured under a mixed-case
-- name are never matched by the trigger. Rejecting anything but lowercase makes
-- all three representations agree. Names are deliberately not normalized:
-- silent lowercasing would re-introduce the aliasing against pre-existing
-- mixed-case metadata and make the parsed name disagree with what the caller
-- wrote.
--
-- Upgrade note: a database that already contains mixed-case rows in
-- @pgmq.meta@ will fail @listQueues@ decoding under this stricter parser (the
-- decoder re-validates names read back from the database). Run the mixed-case
-- remediation described in @docs/design/016-queue-name-validation.md@ before
-- upgrading such a deployment.
--
-- Length check adopted from
-- https://github.com/tembo-io/pgmq/blob/e4d4b84bf302df77be2d1f877c5cf8ef8861bfc7/pgmq-rs/src/util.rs#L94
parseQueueName :: Text -> Either PgmqError QueueName
parseQueueName t
  | T.null t = Left $ InvalidQueueName "The queue name is empty."
  | not isShortEnough = Left $ InvalidQueueName "The queue name is too long."
  | not hasValidCharacters =
      Left $
        InvalidQueueName
          "The queue name contains invalid characters (allowed: lowercase ASCII letters, digits, underscore)."
  | otherwise = Right $ QueueName t
  where
    isShortEnough = T.length t <= maxQueueNameLength
    hasValidCharacters = T.all isValidChar t
    isValidChar c = (isAscii c && (isLower c || isDigit c)) || c == '_'

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

-- | The LISTEN\/NOTIFY channel on which pgmq raises insert notifications for a
-- queue, once @pgmq.enable_notify_insert@ has installed the trigger. The format
-- is @pgmq.q_\<lowercased queue name\>.INSERT@: the physical table name (the
-- @q_@ prefix, lowercased by pgmq's @format_table_name@) bracketed by the
-- @pgmq.@ schema tag and the trigger operation.
--
-- Because the name contains dots, LISTEN requires it double-quoted:
--
-- > LISTEN "pgmq.q_myqueue.INSERT"
--
-- NOTIFY is fire-and-forget. Notifications are not queued for disconnected
-- listeners, and a configured throttle interval suppresses them by design.
-- Consumers must keep a poll fallback regardless of LISTEN.
notifyChannelName :: QueueName -> Text
notifyChannelName q = "pgmq.q_" <> T.toLower (queueNameToText q) <> ".INSERT"

-- JSON wire encodings ---------------------------------------------------------

-- | Encode fields directly as a 'Series', preserving nested record encoders
-- and written key order. The shared 'KeyValue' field lists also build 'Value'
-- objects for 'toJSON', without making 'toEncoding' pass through those objects.
encodeFields :: [Series] -> Encoding
encodeFields = pairs . mconcat

-- | Wire keys are the @pgmq.list_queues()@ column names. The encoding is a
-- published compatibility surface; see @docs/design/020-json-wire-encodings.md@.
-- 'Queue' and 'UnvalidatedQueue' encode identically; only decoding differs.
queueFields :: (KeyValue e kv) => Queue -> [kv]
queueFields q =
  [ "queue_name" .= name q,
    "is_partitioned" .= isPartitioned q,
    "is_unlogged" .= isUnlogged q,
    "created_at" .= createdAt q
  ]

-- | JSON wire contract: @docs/design/020-json-wire-encodings.md@.
instance ToJSON Queue where
  toJSON = object . queueFields
  toEncoding = encodeFields . queueFields

-- | Decoding validates @queue_name@ through 'parseQueueName' (the 'FromJSON'
-- 'QueueName' instance), so a foreign or mixed-case name fails here and must
-- be decoded as 'UnvalidatedQueue' instead.
-- | JSON wire contract: @docs/design/020-json-wire-encodings.md@.
instance FromJSON Queue where
  parseJSON = withObject "Queue" $ \o ->
    Queue
      <$> o .: "queue_name"
      <*> o .: "created_at"
      <*> o .: "is_partitioned"
      <*> o .: "is_unlogged"

-- | Same wire shape as 'Queue'; see @docs/design/020-json-wire-encodings.md@.
unvalidatedQueueFields :: (KeyValue e kv) => UnvalidatedQueue -> [kv]
unvalidatedQueueFields q =
  [ "queue_name" .= unvalidatedName q,
    "is_partitioned" .= unvalidatedIsPartitioned q,
    "is_unlogged" .= unvalidatedIsUnlogged q,
    "created_at" .= unvalidatedCreatedAt q
  ]

-- | JSON wire contract: @docs/design/020-json-wire-encodings.md@.
instance ToJSON UnvalidatedQueue where
  toJSON = object . unvalidatedQueueFields
  toEncoding = encodeFields . unvalidatedQueueFields

-- | Decodes any server-accepted name; nothing is validated.
-- | JSON wire contract: @docs/design/020-json-wire-encodings.md@.
instance FromJSON UnvalidatedQueue where
  parseJSON = withObject "UnvalidatedQueue" $ \o ->
    UnvalidatedQueue
      <$> o .: "queue_name"
      <*> o .: "created_at"
      <*> o .: "is_partitioned"
      <*> o .: "is_unlogged"

-- | Wire keys are the @pgmq.message_record@ column names: @msg_id@, @read_ct@,
-- @enqueued_at@, @last_read_at@, @vt@, @message@, @headers@. Optional columns
-- are always present and encode as @null@. The encoding is a published
-- compatibility surface; see @docs/design/020-json-wire-encodings.md@.
messageFields :: (KeyValue e kv) => Message -> [kv]
messageFields m =
  [ "msg_id" .= messageId m,
    "read_ct" .= readCount m,
    "enqueued_at" .= enqueuedAt m,
    "last_read_at" .= lastReadAt m,
    "vt" .= visibilityTime m,
    "message" .= body m,
    "headers" .= headers m
  ]

-- | JSON wire contract: @docs/design/020-json-wire-encodings.md@.
instance ToJSON Message where
  toJSON = object . messageFields
  toEncoding = encodeFields . messageFields

-- | A missing @last_read_at@ or @headers@ key decodes as 'Nothing', as does
-- an explicit @null@.
-- | JSON wire contract: @docs/design/020-json-wire-encodings.md@.
instance FromJSON Message where
  parseJSON = withObject "Message" $ \o ->
    Message
      <$> o .: "msg_id"
      <*> o .: "vt"
      <*> o .: "enqueued_at"
      <*> o .:? "last_read_at"
      <*> o .: "read_ct"
      <*> o .: "message"
      <*> o .:? "headers"

-- | The 'Message' encoding with one more key, @archived_at@, flattened into
-- the same object. See @docs/design/020-json-wire-encodings.md@.
archivedMessageFields :: (KeyValue e kv) => ArchivedMessage -> [kv]
archivedMessageFields a =
  messageFields (archivedMessage a) <> ["archived_at" .= archivedAt a]

-- | JSON wire contract: @docs/design/020-json-wire-encodings.md@.
instance ToJSON ArchivedMessage where
  toJSON = object . archivedMessageFields
  toEncoding = encodeFields . archivedMessageFields

-- | JSON wire contract: @docs/design/020-json-wire-encodings.md@.
instance FromJSON ArchivedMessage where
  parseJSON = withObject "ArchivedMessage" $ \o ->
    ArchivedMessage
      <$> parseJSON (Aeson.Object o)
      <*> o .: "archived_at"

-- | Wire keys are the @pgmq.list_topic_bindings()@ columns. Encode-only; see
-- @docs/design/020-json-wire-encodings.md@.
topicBindingFields :: (KeyValue e kv) => TopicBinding -> [kv]
topicBindingFields b =
  [ "pattern" .= bindingPattern b,
    "queue_name" .= bindingQueueName b,
    "bound_at" .= bindingBoundAt b,
    "compiled_regex" .= bindingCompiledRegex b
  ]

-- | JSON wire contract: @docs/design/020-json-wire-encodings.md@.
instance ToJSON TopicBinding where
  toJSON = object . topicBindingFields
  toEncoding = encodeFields . topicBindingFields

-- | Wire keys are the @pgmq.test_routing()@ columns. Encode-only.
routingMatchFields :: (KeyValue e kv) => RoutingMatch -> [kv]
routingMatchFields m =
  [ "pattern" .= matchPattern m,
    "queue_name" .= matchQueueName m,
    "compiled_regex" .= matchCompiledRegex m
  ]

-- | JSON wire contract: @docs/design/020-json-wire-encodings.md@.
instance ToJSON RoutingMatch where
  toJSON = object . routingMatchFields
  toEncoding = encodeFields . routingMatchFields

-- | Wire keys are the @pgmq.send_batch_topic()@ result columns. Encode-only.
topicSendResultFields :: (KeyValue e kv) => TopicSendResult -> [kv]
topicSendResultFields r =
  [ "queue_name" .= sentToQueue r,
    "msg_id" .= sentMessageId r
  ]

-- | JSON wire contract: @docs/design/020-json-wire-encodings.md@.
instance ToJSON TopicSendResult where
  toJSON = object . topicSendResultFields
  toEncoding = encodeFields . topicSendResultFields

-- | Wire keys are the @pgmq.list_notify_insert_throttles()@ columns. Encode-only.
notifyInsertThrottleFields :: (KeyValue e kv) => NotifyInsertThrottle -> [kv]
notifyInsertThrottleFields t =
  [ "queue_name" .= throttleQueueName t,
    "throttle_interval_ms" .= throttleIntervalMs t,
    "last_notified_at" .= throttleLastNotifiedAt t
  ]

-- | JSON wire contract: @docs/design/020-json-wire-encodings.md@.
instance ToJSON NotifyInsertThrottle where
  toJSON = object . notifyInsertThrottleFields
  toEncoding = encodeFields . notifyInsertThrottleFields
