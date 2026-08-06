module Pgmq.Hasql.Decoders
  ( messageDecoder,
    messageIdDecoder,
    queueDecoder,
    unvalidatedQueueDecoder,
    queueMetricsDecoder,
    -- Topic decoders (pgmq 1.11.0+)
    topicBindingDecoder,
    routingMatchDecoder,
    topicSendResultDecoder,
    notifyInsertThrottleDecoder,
  )
where

import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Hasql.Decoders qualified as D
import Pgmq.Hasql.Statements.Types (QueueMetrics (..))
import Pgmq.Types
  ( Message (..),
    MessageBody (..),
    MessageId (..),
    NotifyInsertThrottle (..),
    Queue (..),
    RoutingMatch (..),
    TopicBinding (..),
    TopicSendResult (..),
    UnvalidatedQueue (..),
    parseQueueName,
    parseTopicPattern,
  )

-- | Decoder for pgmq.message_record type
-- Column order matches pgmq SQL: msg_id, read_ct, enqueued_at, last_read_at, vt, message, headers
--
-- The @message@ column is nullable in the queue table, and
-- @pgmq.send(queue, NULL::jsonb)@ is legal SQL any non-Haskell producer can
-- issue. A SQL NULL body decodes as JSON @null@ (@MessageBody Aeson.Null@) —
-- an accepted conflation with an explicitly-sent JSON @null@ body, since both
-- mean \"no usable payload\". Requiring a non-null cell here would instead
-- fail the whole batch at decode, after the read statement had already bumped
-- @vt@ and @read_ct@ for every message in it, leaving an invisible poison row.
messageDecoder :: D.Row Message
messageDecoder =
  ( \msgId readCt enqueuedAt lastReadAt vt body headers ->
      Message
        { messageId = msgId,
          visibilityTime = vt,
          enqueuedAt = enqueuedAt,
          lastReadAt = lastReadAt,
          readCount = fromIntegral readCt,
          body = body,
          headers = headers
        }
  )
    <$> messageIdDecoder -- msg_id
    <*> D.column (D.nonNullable D.int4) -- read_ct (INTEGER -> Int32)
    <*> D.column (D.nonNullable D.timestamptz) -- enqueued_at
    <*> D.column (D.nullable D.timestamptz) -- last_read_at
    <*> D.column (D.nonNullable D.timestamptz) -- vt
    <*> (MessageBody . fromMaybe Aeson.Null <$> D.column (D.nullable D.jsonb)) -- message (SQL NULL -> JSON null)
    <*> D.column (D.nullable D.jsonb) -- headers

messageIdDecoder :: D.Row MessageId
messageIdDecoder = MessageId <$> D.column (D.nonNullable D.int8)

-- | Decoder for pgmq.queue_record type
-- Column order: queue_name (varchar), is_partitioned (bool), is_unlogged (bool), created_at (timestamptz)
queueDecoder :: D.Row Queue
queueDecoder =
  (\name isPartitioned isUnlogged createdAt -> Queue name createdAt isPartitioned isUnlogged)
    <$> D.column (D.nonNullable $ D.refine (first (pack . show) . parseQueueName) D.varchar)
    <*> D.column (D.nonNullable D.bool)
    <*> D.column (D.nonNullable D.bool)
    <*> D.column (D.nonNullable D.timestamptz)

-- | Like 'queueDecoder' but with the queue name left as plain text.
--
-- The server's only queue-name check is length, so any client sharing the
-- database can create a name 'parseQueueName' rejects. 'queueDecoder' refines
-- that column and therefore fails the entire listing on one such row; this
-- decoder does not, so state inspection can observe foreign queues.
--
-- Column order matches 'queueDecoder': queue_name (varchar), is_partitioned
-- (bool), is_unlogged (bool), created_at (timestamptz).
unvalidatedQueueDecoder :: D.Row UnvalidatedQueue
unvalidatedQueueDecoder =
  ( \name isPartitioned isUnlogged createdAt ->
      UnvalidatedQueue name createdAt isPartitioned isUnlogged
  )
    <$> D.column (D.nonNullable D.varchar)
    <*> D.column (D.nonNullable D.bool)
    <*> D.column (D.nonNullable D.bool)
    <*> D.column (D.nonNullable D.timestamptz)

queueMetricsDecoder :: D.Row QueueMetrics
queueMetricsDecoder =
  QueueMetrics
    <$> D.column (D.nonNullable D.text)
    <*> D.column (D.nonNullable D.int8)
    <*> D.column (D.nullable D.int4)
    <*> D.column (D.nullable D.int4)
    <*> D.column (D.nonNullable D.int8)
    <*> D.column (D.nonNullable D.timestamptz)
    <*> D.column (D.nonNullable D.int8) -- queue_visible_length (pgmq 1.5.0+)

-- | Decoder for topic binding records (pgmq 1.11.0+)
-- Column order: pattern, queue_name, bound_at, compiled_regex
topicBindingDecoder :: D.Row TopicBinding
topicBindingDecoder =
  TopicBinding
    <$> D.column (D.nonNullable $ D.refine (first (pack . show) . parseTopicPattern) D.text)
    <*> D.column (D.nonNullable D.text)
    <*> D.column (D.nonNullable D.timestamptz)
    <*> D.column (D.nonNullable D.text)

-- | Decoder for routing match results (pgmq 1.11.0+)
-- Column order: pattern, queue_name, compiled_regex
routingMatchDecoder :: D.Row RoutingMatch
routingMatchDecoder =
  RoutingMatch
    <$> D.column (D.nonNullable $ D.refine (first (pack . show) . parseTopicPattern) D.text)
    <*> D.column (D.nonNullable D.text)
    <*> D.column (D.nonNullable D.text)

-- | Decoder for topic send results (pgmq 1.11.0+)
-- Column order: queue_name, msg_id
topicSendResultDecoder :: D.Row TopicSendResult
topicSendResultDecoder =
  TopicSendResult
    <$> D.column (D.nonNullable D.text)
    <*> (MessageId <$> D.column (D.nonNullable D.int8))

-- | Decoder for notification throttle settings (pgmq 1.11.0+)
-- Column order: queue_name, throttle_interval_ms, last_notified_at
notifyInsertThrottleDecoder :: D.Row NotifyInsertThrottle
notifyInsertThrottleDecoder =
  NotifyInsertThrottle
    <$> D.column (D.nonNullable D.text)
    <*> D.column (D.nonNullable D.int4)
    <*> D.column (D.nonNullable D.timestamptz)
