module Pgmq.Db.Decoders (messageDecoder) where

import Hasql.Decoders qualified as D
import Pgmq.Types (Message (..), MessageBody (..), MessageId (..))

messageDecoder :: D.Row Message
messageDecoder =
  Message
    <$> (MessageId <$> D.column (D.nonNullable D.int8))
    <*> D.column (D.nonNullable D.timestamptz)
    <*> D.column (D.nonNullable D.timestamptz)
    <*> D.column (D.nonNullable D.int8)
    <*> (MessageBody <$> D.column (D.nonNullable D.jsonb))
