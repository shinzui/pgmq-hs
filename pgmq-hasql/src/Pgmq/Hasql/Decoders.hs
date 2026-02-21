module Pgmq.Hasql.Decoders
  ( messageDecoder,
    messageIdDecoder,
    queueDecoder,
    queueMetricsDecoder,
    -- Topic decoders (pgmq 1.11.0+)
    topicBindingDecoder,
    routingMatchDecoder,
    topicSendResultDecoder,
    notifyInsertThrottleDecoder,
  )
where

import Data.Bifunctor (first)
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
    parseQueueName,
    parseTopicPattern,
  )

-- | Decoder for pgmq.message_record type
-- Column order matches pgmq SQL: msg_id, read_ct, enqueued_at, last_read_at, vt, message, headers
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
    <*> (MessageBody <$> D.column (D.nonNullable D.jsonb)) -- message
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
