module Pgmq.Db.Decoders
  ( messageDecoder,
    messageIdDecoder,
    queueDecoder,
    queueMetricsDecoder,
  )
where

import Data.Bifunctor (first)
import Data.Text (pack)
import Hasql.Decoders qualified as D
import Pgmq.Db.Statements.Types (QueueMetrics (..))
import Pgmq.Types (Message (..), MessageBody (..), MessageId (..), Queue (..), parseQueueName)

messageDecoder :: D.Row Message
messageDecoder =
  Message
    <$> messageIdDecoder
    <*> D.column (D.nonNullable D.timestamptz)
    <*> D.column (D.nonNullable D.timestamptz)
    <*> D.column (D.nonNullable D.int8)
    <*> (MessageBody <$> D.column (D.nonNullable D.jsonb))
    <*> D.column (D.nullable D.jsonb) -- headers (pgmq 1.5.0+)

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
