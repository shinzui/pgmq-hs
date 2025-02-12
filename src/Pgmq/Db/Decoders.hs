module Pgmq.Db.Decoders (messageDecoder) where

import Hasql.Decoders qualified as D
import Pgmq.Types (Message (..), MessageBody (..), MessageId (..))

messageDecoder :: D.Row Message
messageDecoder =
  Message
    <$> messageIdRow
    <*> D.column (D.nonNullable D.timestamptz)
    <*> D.column (D.nonNullable D.timestamptz)
    <*> D.column (D.nonNullable D.int8)
    <*> (MessageBody <$> D.column (D.nonNullable D.jsonb))

messageIdRow :: D.Row MessageId
messageIdRow = MessageId <$> D.column (D.nonNullable D.int8)
