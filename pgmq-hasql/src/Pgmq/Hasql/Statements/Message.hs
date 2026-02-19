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
    -- Timestamp-based VT functions (pgmq 1.10.0+)
    setVisibilityTimeoutAt,
    batchSetVisibilityTimeoutAt,
    readWithPoll,
    pop,
    -- FIFO read functions (pgmq 1.8.0+)
    readGrouped,
    readGroupedWithPoll,
    -- Round-robin FIFO functions (pgmq 1.9.0+)
    readGroupedRoundRobin,
    readGroupedRoundRobinWithPoll,
  )
where

import Hasql.Decoders qualified as D
import Hasql.Statement (Statement, preparable)
import Pgmq.Hasql.Decoders (messageDecoder, messageIdDecoder)
import Pgmq.Hasql.Encoders
  ( batchMessageQueryEncoder,
    batchSendMessageEncoder,
    batchSendMessageForLaterEncoder,
    batchSendMessageWithHeadersEncoder,
    batchSendMessageWithHeadersForLaterEncoder,
    batchVisibilityTimeoutAtQueryEncoder,
    batchVisibilityTimeoutQueryEncoder,
    messageQueryEncoder,
    popMessageEncoder,
    queueNameEncoder,
    readGroupedEncoder,
    readGroupedWithPollEncoder,
    readMessageEncoder,
    readWithPollEncoder,
    sendMessageEncoder,
    sendMessageForLaterEncoder,
    sendMessageWithHeadersEncoder,
    sendMessageWithHeadersForLaterEncoder,
    visibilityTimeoutAtQueryEncoder,
    visibilityTimeoutQueryEncoder,
  )
import Pgmq.Hasql.Prelude
import Pgmq.Hasql.Statements.Types
  ( BatchMessageQuery,
    BatchSendMessage,
    BatchSendMessageForLater,
    BatchSendMessageWithHeaders,
    BatchSendMessageWithHeadersForLater,
    BatchVisibilityTimeoutAtQuery,
    BatchVisibilityTimeoutQuery,
    MessageQuery,
    PopMessage,
    ReadGrouped,
    ReadGroupedWithPoll,
    ReadMessage,
    ReadWithPollMessage,
    SendMessage,
    SendMessageForLater,
    SendMessageWithHeaders,
    SendMessageWithHeadersForLater,
    VisibilityTimeoutAtQuery,
    VisibilityTimeoutQuery,
  )
import Pgmq.Types (Message, MessageId, QueueName)

-- https://tembo.io/pgmq/api/sql/functions/#send
-- Note: coalesce handles null delay to ensure correct function overload resolution
sendMessage :: Statement SendMessage MessageId
sendMessage = preparable sql sendMessageEncoder decoder
  where
    sql = "select * from pgmq.send($1, $2, coalesce($3, 0))"
    decoder = D.singleRow messageIdDecoder

-- https://tembo.io/pgmq/api/sql/functions/#send
sendMessageForLater :: Statement SendMessageForLater MessageId
sendMessageForLater = preparable sql sendMessageForLaterEncoder decoder
  where
    sql = "select * from pgmq.send($1, $2, $3)"
    decoder = D.singleRow messageIdDecoder

-- | https://tembo.io/pgmq/api/sql/functions/#send_batch
-- Note: coalesce handles null delay to ensure correct function overload resolution
batchSendMessage :: Statement BatchSendMessage [MessageId]
batchSendMessage = preparable sql batchSendMessageEncoder decoder
  where
    sql = "select * from pgmq.send_batch($1, $2, coalesce($3, 0))"
    decoder = D.rowList messageIdDecoder

-- | https://tembo.io/pgmq/api/sql/functions/#send_batch
batchSendMessageForLater :: Statement BatchSendMessageForLater [MessageId]
batchSendMessageForLater = preparable sql batchSendMessageForLaterEncoder decoder
  where
    sql = "select * from pgmq.send_batch($1, $2, $3)"
    decoder = D.rowList messageIdDecoder

-- | Send a message with headers (pgmq 1.5.0+)
-- https://tembo.io/pgmq/api/sql/functions/#send
-- Note: coalesce handles null delay to ensure correct function overload resolution
sendMessageWithHeaders :: Statement SendMessageWithHeaders MessageId
sendMessageWithHeaders = preparable sql sendMessageWithHeadersEncoder decoder
  where
    sql = "select * from pgmq.send($1, $2, $3, coalesce($4, 0))"
    decoder = D.singleRow messageIdDecoder

-- | Send a message with headers for later (pgmq 1.5.0+)
-- https://tembo.io/pgmq/api/sql/functions/#send
sendMessageWithHeadersForLater :: Statement SendMessageWithHeadersForLater MessageId
sendMessageWithHeadersForLater = preparable sql sendMessageWithHeadersForLaterEncoder decoder
  where
    sql = "select * from pgmq.send($1, $2, $3, $4)"
    decoder = D.singleRow messageIdDecoder

-- | Send a batch of messages with headers (pgmq 1.5.0+)
-- https://tembo.io/pgmq/api/sql/functions/#send_batch
-- Note: coalesce handles null delay to ensure correct function overload resolution
batchSendMessageWithHeaders :: Statement BatchSendMessageWithHeaders [MessageId]
batchSendMessageWithHeaders = preparable sql batchSendMessageWithHeadersEncoder decoder
  where
    sql = "select * from pgmq.send_batch($1, $2, $3, coalesce($4, 0))"
    decoder = D.rowList messageIdDecoder

-- | Send a batch of messages with headers for later (pgmq 1.5.0+)
-- https://tembo.io/pgmq/api/sql/functions/#send_batch
batchSendMessageWithHeadersForLater :: Statement BatchSendMessageWithHeadersForLater [MessageId]
batchSendMessageWithHeadersForLater = preparable sql batchSendMessageWithHeadersForLaterEncoder decoder
  where
    sql = "select * from pgmq.send_batch($1, $2, $3, $4)"
    decoder = D.rowList messageIdDecoder

-- | https://tembo.io/pgmq/api/sql/functions/#read
-- Note: conditional parameter added in pgmq 1.5.0
-- We use the 3-param version since the 4-param version fails with NULL conditional
-- (message @> NULL = NULL, not TRUE, so no rows match).
-- To use conditional filtering, use readMessageConditional instead.
readMessage :: Statement ReadMessage (Vector Message)
readMessage = preparable sql readMessageEncoder decoder
  where
    sql = "select * from pgmq.read($1,$2,$3)"
    decoder = D.rowVector messageDecoder

-- | https://tembo.io/pgmq/api/sql/functions/#delete-single
deleteMessage :: Statement MessageQuery Bool
deleteMessage = preparable sql messageQueryEncoder decoder
  where
    sql = "select * from pgmq.delete($1,$2)"
    decoder = D.singleRow (D.column (D.nonNullable D.bool))

-- | https://tembo.io/pgmq/api/sql/functions/#delete-batch
batchDeleteMessages :: Statement BatchMessageQuery [MessageId]
batchDeleteMessages = preparable sql batchMessageQueryEncoder decoder
  where
    sql = "select * from pgmq.delete($1,$2)"
    decoder = D.rowList messageIdDecoder

-- | https://tembo.io/pgmq/api/sql/functions/#archive-single
archiveMessage :: Statement MessageQuery Bool
archiveMessage = preparable sql messageQueryEncoder decoder
  where
    sql = "select * from pgmq.archive($1,$2)"
    decoder = D.singleRow (D.column (D.nonNullable D.bool))

-- | https://tembo.io/pgmq/api/sql/functions/#archive-batch
batchArchiveMessages :: Statement BatchMessageQuery [MessageId]
batchArchiveMessages = preparable sql batchMessageQueryEncoder decoder
  where
    sql = "select * from pgmq.archive($1,$2)"
    decoder = D.rowList messageIdDecoder

-- | Permanently deletes all messages in a queue. Returns the number of messages that were deleted.
-- | https://tembo.io/pgmq/api/sql/functions/#purge_queue
deleteAllMessagesFromQueue :: Statement QueueName Int64
deleteAllMessagesFromQueue = preparable sql queueNameEncoder decoder
  where
    sql = "select * from pgmq.purge_queue($1)"
    decoder = D.singleRow $ D.column $ D.nonNullable D.int8

-- | Sets the visibility timeout of a message to a specified time duration in the future. Returns the record of the message that was updated.
-- | https://tembo.io/pgmq/api/sql/functions/#set_vt
changeVisibilityTimeout :: Statement VisibilityTimeoutQuery Message
changeVisibilityTimeout = preparable sql visibilityTimeoutQueryEncoder decoder
  where
    sql = "select * from pgmq.set_vt($1,$2,$3)"
    decoder = D.singleRow messageDecoder

-- | Batch update visibility timeout for multiple messages (pgmq 1.8.0+)
-- | https://tembo.io/pgmq/api/sql/functions/#set_vt
batchChangeVisibilityTimeout :: Statement BatchVisibilityTimeoutQuery (Vector Message)
batchChangeVisibilityTimeout = preparable sql batchVisibilityTimeoutQueryEncoder decoder
  where
    sql = "select * from pgmq.set_vt($1,$2,$3)"
    decoder = D.rowVector messageDecoder

-- | Set visibility timeout to an absolute timestamp (pgmq 1.10.0+)
-- | https://tembo.io/pgmq/api/sql/functions/#set_vt
setVisibilityTimeoutAt :: Statement VisibilityTimeoutAtQuery Message
setVisibilityTimeoutAt = preparable sql visibilityTimeoutAtQueryEncoder decoder
  where
    sql = "select * from pgmq.set_vt($1,$2,$3)"
    decoder = D.singleRow messageDecoder

-- | Batch set visibility timeout to an absolute timestamp (pgmq 1.10.0+)
-- | https://tembo.io/pgmq/api/sql/functions/#set_vt
batchSetVisibilityTimeoutAt :: Statement BatchVisibilityTimeoutAtQuery (Vector Message)
batchSetVisibilityTimeoutAt = preparable sql batchVisibilityTimeoutAtQueryEncoder decoder
  where
    sql = "select * from pgmq.set_vt($1,$2,$3)"
    decoder = D.rowVector messageDecoder

-- | https://tembo.io/pgmq/api/sql/functions/#read_with_poll
readWithPoll :: Statement ReadWithPollMessage (Vector Message)
readWithPoll = preparable sql readWithPollEncoder decoder
  where
    sql = "select * from pgmq.read_with_poll($1,$2,$3,$4,$5,$6)"
    decoder = D.rowVector messageDecoder

-- | Pop messages from queue (atomic read + delete)
-- https://tembo.io/pgmq/api/sql/functions/#pop
-- Note: qty parameter added in pgmq 1.7.0
pop :: Statement PopMessage (Vector Message)
pop = preparable sql popMessageEncoder decoder
  where
    sql = "select * from pgmq.pop($1,$2)"
    decoder = D.rowVector messageDecoder

-- | FIFO read - fills batch from same message group (pgmq 1.8.0+)
-- Messages are grouped by the x-pgmq-group header.
-- https://tembo.io/pgmq/api/sql/functions/#read_grouped
readGrouped :: Statement ReadGrouped (Vector Message)
readGrouped = preparable sql readGroupedEncoder decoder
  where
    sql = "select * from pgmq.read_grouped($1,$2,$3)"
    decoder = D.rowVector messageDecoder

-- | FIFO read with polling - fills batch from same message group (pgmq 1.8.0+)
-- https://tembo.io/pgmq/api/sql/functions/#read_grouped_with_poll
readGroupedWithPoll :: Statement ReadGroupedWithPoll (Vector Message)
readGroupedWithPoll = preparable sql readGroupedWithPollEncoder decoder
  where
    sql = "select * from pgmq.read_grouped_with_poll($1,$2,$3,$4,$5)"
    decoder = D.rowVector messageDecoder

-- | Round-robin FIFO read - fair distribution across message groups (pgmq 1.9.0+)
-- Uses layered round-robin algorithm for fairness.
-- https://tembo.io/pgmq/api/sql/functions/#read_grouped_rr
readGroupedRoundRobin :: Statement ReadGrouped (Vector Message)
readGroupedRoundRobin = preparable sql readGroupedEncoder decoder
  where
    sql = "select * from pgmq.read_grouped_rr($1,$2,$3)"
    decoder = D.rowVector messageDecoder

-- | Round-robin FIFO read with polling (pgmq 1.9.0+)
-- https://tembo.io/pgmq/api/sql/functions/#read_grouped_rr_with_poll
readGroupedRoundRobinWithPoll :: Statement ReadGroupedWithPoll (Vector Message)
readGroupedRoundRobinWithPoll = preparable sql readGroupedWithPollEncoder decoder
  where
    sql = "select * from pgmq.read_grouped_rr_with_poll($1,$2,$3,$4,$5)"
    decoder = D.rowVector messageDecoder
