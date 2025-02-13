module Pgmq.Db.Decoders
  ( messageDecoder,
    messageIdDecoder,
    queueDecoder,
  )
where

import Data.Bifunctor (first)
import Data.Text (pack)
import Hasql.Decoders qualified as D
import Pgmq.Types (Message (..), MessageBody (..), MessageId (..), Queue (..), parseQueueName)

messageDecoder :: D.Row Message
messageDecoder =
  Message
    <$> messageIdDecoder
    <*> D.column (D.nonNullable D.timestamptz)
    <*> D.column (D.nonNullable D.timestamptz)
    <*> D.column (D.nonNullable D.int8)
    <*> (MessageBody <$> D.column (D.nonNullable D.jsonb))

messageIdDecoder :: D.Row MessageId
messageIdDecoder = MessageId <$> D.column (D.nonNullable D.int8)

queueDecoder :: D.Row Queue
queueDecoder =
  Queue
    <$> D.column (D.nonNullable $ D.refine (first (pack . show) . parseQueueName) D.text)
    <*> D.column (D.nonNullable D.timestamptz)
    <*> D.column (D.nonNullable D.bool)
    <*> D.column (D.nonNullable D.bool)
