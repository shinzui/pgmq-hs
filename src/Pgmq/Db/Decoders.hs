module Pgmq.Db.Decoders
  ( messageDecoder,
    messageIdDecoder,
  )
where

import Hasql.Decoders qualified as D
import Pgmq.Types (Message (..), MessageBody (..), MessageId (..))

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
