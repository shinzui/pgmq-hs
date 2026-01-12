module Pgmq.Hasql.Statements.Message
  ( sendMessage,
    sendMessageForLater,
    batchSendMessage,
    batchSendMessageForLater,
    sendMessageWithHeaders,
    sendMessageWithHeadersForLater,
    batchSendMessageWithHeaders,
    batchSendMessageWithHeadersForLater,
    readMessage,
    deleteMessage,
    batchDeleteMessages,
    archiveMessage,
    batchArchiveMessages,
    deleteAllMessagesFromQueue,
    changeVisibilityTimeout,
    batchChangeVisibilityTimeout,
    readWithPoll,
    pop,
  )
where

import Hasql.Decoders qualified as D
import Hasql.Statement (Statement (..))
import Pgmq.Hasql.Decoders (messageDecoder, messageIdDecoder)
import Pgmq.Hasql.Encoders
  ( batchMessageQueryEncoder,
    batchSendMessageEncoder,
    batchSendMessageForLaterEncoder,
    batchSendMessageWithHeadersEncoder,
    batchSendMessageWithHeadersForLaterEncoder,
    batchVisibilityTimeoutQueryEncoder,
    messageQueryEncoder,
    popMessageEncoder,
    queueNameEncoder,
    readMessageEncoder,
    readWithPollEncoder,
    sendMessageEncoder,
    sendMessageForLaterEncoder,
    sendMessageWithHeadersEncoder,
    sendMessageWithHeadersForLaterEncoder,
    visibilityTimeoutQueryEncoder,
  )
import Pgmq.Hasql.Prelude
import Pgmq.Hasql.Statements.Types
  ( BatchMessageQuery,
    BatchSendMessage,
    BatchSendMessageForLater,
    BatchSendMessageWithHeaders,
    BatchSendMessageWithHeadersForLater,
    BatchVisibilityTimeoutQuery,
    MessageQuery,
    PopMessage,
    ReadMessage,
    ReadWithPollMessage,
    SendMessage,
    SendMessageForLater,
    SendMessageWithHeaders,
    SendMessageWithHeadersForLater,
    VisibilityTimeoutQuery,
  )
import Pgmq.Types (Message, MessageId, QueueName)

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

-- | Send a message with headers (pgmq 1.5.0+)
-- https://tembo.io/pgmq/api/sql/functions/#send
sendMessageWithHeaders :: Statement SendMessageWithHeaders MessageId
sendMessageWithHeaders = Statement sql sendMessageWithHeadersEncoder decoder True
  where
    sql = "select * from pgmq.send($1, $2, $3, $4)"
    decoder = D.singleRow messageIdDecoder

-- | Send a message with headers for later (pgmq 1.5.0+)
-- https://tembo.io/pgmq/api/sql/functions/#send
sendMessageWithHeadersForLater :: Statement SendMessageWithHeadersForLater MessageId
sendMessageWithHeadersForLater = Statement sql sendMessageWithHeadersForLaterEncoder decoder True
  where
    sql = "select * from pgmq.send($1, $2, $3, $4)"
    decoder = D.singleRow messageIdDecoder

-- | Send a batch of messages with headers (pgmq 1.5.0+)
-- https://tembo.io/pgmq/api/sql/functions/#send_batch
batchSendMessageWithHeaders :: Statement BatchSendMessageWithHeaders [MessageId]
batchSendMessageWithHeaders = Statement sql batchSendMessageWithHeadersEncoder decoder True
  where
    sql = "select * from pgmq.send_batch($1, $2, $3, $4)"
    decoder = D.rowList messageIdDecoder

-- | Send a batch of messages with headers for later (pgmq 1.5.0+)
-- https://tembo.io/pgmq/api/sql/functions/#send_batch
batchSendMessageWithHeadersForLater :: Statement BatchSendMessageWithHeadersForLater [MessageId]
batchSendMessageWithHeadersForLater = Statement sql batchSendMessageWithHeadersForLaterEncoder decoder True
  where
    sql = "select * from pgmq.send_batch($1, $2, $3, $4)"
    decoder = D.rowList messageIdDecoder

-- | https://tembo.io/pgmq/api/sql/functions/#read
-- Note: conditional parameter added in pgmq 1.5.0
readMessage :: Statement ReadMessage (Vector Message)
readMessage = Statement sql readMessageEncoder decoder True
  where
    sql = "select * from pgmq.read($1,$2,$3,$4)"
    decoder = D.rowVector messageDecoder

-- | https://tembo.io/pgmq/api/sql/functions/#delete-single
deleteMessage :: Statement MessageQuery Bool
deleteMessage = Statement sql messageQueryEncoder decoder True
  where
    sql = "select * from pgmq.delete($1,$2)"
    decoder = D.singleRow (D.column (D.nonNullable D.bool))

-- | https://tembo.io/pgmq/api/sql/functions/#delete-batch
batchDeleteMessages :: Statement BatchMessageQuery [MessageId]
batchDeleteMessages = Statement sql batchMessageQueryEncoder decoder True
  where
    sql = "select * from pgmq.delete($1,$2)"
    decoder = D.rowList messageIdDecoder

-- | https://tembo.io/pgmq/api/sql/functions/#archive-single
archiveMessage :: Statement MessageQuery Bool
archiveMessage = Statement sql messageQueryEncoder decoder True
  where
    sql = "select * from pgmq.archive($1,$2)"
    decoder = D.singleRow (D.column (D.nonNullable D.bool))

-- | https://tembo.io/pgmq/api/sql/functions/#archive-batch
batchArchiveMessages :: Statement BatchMessageQuery [MessageId]
batchArchiveMessages = Statement sql batchMessageQueryEncoder decoder True
  where
    sql = "select * from pgmq.archive($1,$2)"
    decoder = D.rowList messageIdDecoder

-- | Permanently deletes all messages in a queue. Returns the number of messages that were deleted.
-- | https://tembo.io/pgmq/api/sql/functions/#purge_queue
deleteAllMessagesFromQueue :: Statement QueueName Int64
deleteAllMessagesFromQueue = Statement sql queueNameEncoder decoder True
  where
    sql = "select * from pgmq.purge_queue($1)"
    decoder = D.singleRow $ D.column $ D.nonNullable D.int8

-- | Sets the visibility timeout of a message to a specified time duration in the future. Returns the record of the message that was updated.
-- | https://tembo.io/pgmq/api/sql/functions/#set_vt
changeVisibilityTimeout :: Statement VisibilityTimeoutQuery Message
changeVisibilityTimeout = Statement sql visibilityTimeoutQueryEncoder decoder True
  where
    sql = "select * from pgmq.set_vt($1,$2,$3)"
    decoder = D.singleRow messageDecoder

-- | Batch update visibility timeout for multiple messages (pgmq 1.8.0+)
-- | https://tembo.io/pgmq/api/sql/functions/#set_vt
batchChangeVisibilityTimeout :: Statement BatchVisibilityTimeoutQuery (Vector Message)
batchChangeVisibilityTimeout = Statement sql batchVisibilityTimeoutQueryEncoder decoder True
  where
    sql = "select * from pgmq.set_vt($1,$2,$3)"
    decoder = D.rowVector messageDecoder

-- | https://tembo.io/pgmq/api/sql/functions/#read_with_poll
readWithPoll :: Statement ReadWithPollMessage (Vector Message)
readWithPoll = Statement sql readWithPollEncoder decoder True
  where
    sql = "select * from pgmq.read_with_poll($1,$2,$3,$4,$5,$6)"
    decoder = D.rowVector messageDecoder

-- | Pop messages from queue (atomic read + delete)
-- https://tembo.io/pgmq/api/sql/functions/#pop
-- Note: qty parameter added in pgmq 1.7.0
pop :: Statement PopMessage (Vector Message)
pop = Statement sql popMessageEncoder decoder True
  where
    sql = "select * from pgmq.pop($1,$2)"
    decoder = D.rowVector messageDecoder
