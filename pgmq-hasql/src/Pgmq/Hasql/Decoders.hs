module Pgmq.Hasql.Decoders
  ( messageDecoder,
    messageIdDecoder,
    queueDecoder,
    queueMetricsDecoder,
  )
where

import Data.Bifunctor (first)
import Data.Text (pack)
import Hasql.Decoders qualified as D
import Pgmq.Hasql.Statements.Types (QueueMetrics (..))
import Pgmq.Types (Message (..), MessageBody (..), MessageId (..), Queue (..), parseQueueName)

-- | Decoder for pgmq.message_record type
-- Column order matches pgmq SQL: msg_id, read_ct, enqueued_at, last_read_at, vt, message, headers
messageDecoder :: D.Row Message
messageDecoder = do
  msgId <- messageIdDecoder -- msg_id
  readCt <- D.column (D.nonNullable D.int4) -- read_ct (INTEGER -> Int32)
  enqueuedAt <- D.column (D.nonNullable D.timestamptz) -- enqueued_at
  lastReadAt <- D.column (D.nullable D.timestamptz) -- last_read_at
  vt <- D.column (D.nonNullable D.timestamptz) -- vt
  body <- MessageBody <$> D.column (D.nonNullable D.jsonb) -- message
  headers <- D.column (D.nullable D.jsonb) -- headers
  pure $
    Message
      { messageId = msgId,
        visibilityTime = vt,
        enqueuedAt = enqueuedAt,
        lastReadAt = lastReadAt,
        readCount = fromIntegral readCt,
        body = body,
        headers = headers
      }

messageIdDecoder :: D.Row MessageId
messageIdDecoder = MessageId <$> D.column (D.nonNullable D.int8)

queueDecoder :: D.Row Queue
queueDecoder =
  Queue
    <$> D.column (D.nonNullable $ D.refine (first (pack . show) . parseQueueName) D.text)
    <*> D.column (D.nonNullable D.timestamptz)
    <*> D.column (D.nonNullable D.bool)
    <*> D.column (D.nonNullable D.bool)

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
