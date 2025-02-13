module Pgmq.Db.Statements.Message where

import Hasql.Decoders qualified as D
import Hasql.Statement (Statement (..))
import Pgmq.Db.Decoders (messageDecoder, messageIdDecoder)
import Pgmq.Db.Encoders
  ( batchMessageQueryEncoder,
    batchSendMessageEncoder,
    batchSendMessageForLaterEncoder,
    messageQueryEncoder,
    readMessageEncoder,
    sendMessageEncoder,
    sendMessageForLaterEncoder,
  )
import Pgmq.Db.Statements.Types
  ( BatchMessageQuery,
    BatchSendMessage,
    BatchSendMessageForLater,
    MessageQuery,
    ReadMessage,
    SendMessage,
    SendMessageForLater,
  )
import Pgmq.Prelude
import Pgmq.Types (Message, MessageId)

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

-- | https://tembo.io/pgmq/api/sql/functions/#read
readMessage :: Statement ReadMessage (Vector Message)
readMessage = Statement sql readMessageEncoder decoder True
  where
    sql = "select * from pgmq.read($1,$2,$3)"
    decoder = D.rowVector messageDecoder

-- | https://tembo.io/pgmq/api/sql/functions/#delete-single
deleteMessage :: Statement MessageQuery Bool
deleteMessage = Statement sql messageQueryEncoder decoder True
  where
    sql = "select pgmq.delete($1,$2)"
    decoder = D.singleRow (D.column (D.nonNullable D.bool))

-- | https://tembo.io/pgmq/api/sql/functions/#delete-batch
batchDeleteMessages :: Statement BatchMessageQuery [MessageId]
batchDeleteMessages = Statement sql batchMessageQueryEncoder decoder True
  where
    sql = "select pgmq.delete($1,$2)"
    decoder = D.rowList messageIdDecoder
