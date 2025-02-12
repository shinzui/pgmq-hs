module Pgmq.Db.Statements.Message where

import Hasql.Decoders qualified as D
import Hasql.Statement (Statement (..))
import Pgmq.Db.Decoders (messageIdDecoder)
import Pgmq.Db.Encoders (batchSendMessageEncoder, batchSendMessageForLaterEncoder, sendMessageEncoder, sendMessageForLaterEncoder)
import Pgmq.Db.Statements.Types (BatchSendMessage, BatchSendMessageForLater, SendMessage, SendMessageForLater)
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

-- | https://tembo.io/pgmq/api/sql/functions/#send_batch
batchSendMessage :: Statement BatchSendMessage [MessageId]
batchSendMessage = Statement sql batchSendMessageEncoder decoder True
  where
    sql = "select * from pgmq.send_batch($1, $2, $3)"
    decoder = D.rowList messageIdDecoder

-- | https://tembo.io/pgmq/api/sql/functions/#send_batch
batchSendMessageForLater :: Statement BatchSendMessageForLater [MessageId]
batchSendMessageForLater = Statement sql batchSendMessageForLaterEncoder decoder True
  where
    sql = "select * from pgmq.send_batch($1, $2, $3)"
    decoder = D.rowList messageIdDecoder
