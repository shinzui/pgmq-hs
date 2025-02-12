module Pgmq.Db.Statements.Message where

import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Statement (Statement (..))
import Pgmq.Db.Decoders (messageIdDecoder)
import Pgmq.Db.Encoders (sendMessageEncoder, sendMessageForLaterEncoder)
import Pgmq.Db.Statements.Types (SendMessage, SendMessageForLater)
import Pgmq.Types (MessageId)

-- https://tembo.io/pgmq/api/sql/functions/#send
sendMessage :: Statement SendMessage MessageId
sendMessage = Statement sql sendMessageEncoder decoder True
  where
    sql = "select * from pgmq.send($1, $2, $3)"
    decoder = D.singleRow messageIdDecoder

-- https://tembo.io/pgmq/api/sql/functions/#send
sendMessageForLater :: Statement SendMessageForLater MessageId
sendMessageForLater = Statement sql sendMessageForLaterEncoder decoder True
  where
    sql = "select * from pgmq.send($1, $2, $3)"
    decoder = D.singleRow messageIdDecoder
