module Pgmq.Hasql.Sessions
  ( createQueue,
    dropQueue,
    createPartitionedQueue,
    createUnloggedQueue,
    detachArchive,
    enableNotifyInsert,
    disableNotifyInsert,
    sendMessage,
    sendMessageForLater,
    batchSendMessage,
    batchSendMessageForLater,
    sendMessageWithHeaders,
    sendMessageWithHeadersForLater,
    batchSendMessageWithHeaders,
    batchSendMessageWithHeadersForLater,
    deleteMessage,
    batchDeleteMessages,
    archiveMessage,
    batchArchiveMessages,
    deleteAllMessagesFromQueue,
    changeVisibilityTimeout,
    batchChangeVisibilityTimeout,
    listQueues,
    pop,
    queueMetrics,
    allQueueMetrics,
    readMessage,
    readWithPoll,
  )
where

import Hasql.Session (Session, statement)
import Pgmq.Hasql.Prelude
import Pgmq.Hasql.Statements qualified as Stmt
import Pgmq.Hasql.Statements.Message qualified as Msg
import Pgmq.Hasql.Statements.Types
  ( BatchMessageQuery,
    BatchSendMessage,
    BatchSendMessageForLater,
    BatchSendMessageWithHeaders,
    BatchSendMessageWithHeadersForLater,
    BatchVisibilityTimeoutQuery,
    CreatePartitionedQueue,
    EnableNotifyInsert,
    MessageQuery,
    PopMessage,
    QueueMetrics,
    ReadMessage,
    ReadWithPollMessage,
    SendMessage,
    SendMessageForLater,
    SendMessageWithHeaders,
    SendMessageWithHeadersForLater,
    VisibilityTimeoutQuery,
  )
import Pgmq.Types (Message, MessageId, Queue, QueueName)

createQueue :: QueueName -> Session ()
createQueue q = statement q Stmt.createQueue

dropQueue :: QueueName -> Session Bool
dropQueue q = statement q Stmt.dropQueue

sendMessage :: SendMessage -> Session MessageId
sendMessage msg = statement msg Msg.sendMessage

sendMessageForLater :: SendMessageForLater -> Session MessageId
sendMessageForLater msg = statement msg Msg.sendMessageForLater

batchSendMessage :: BatchSendMessage -> Session [MessageId]
batchSendMessage msgs = statement msgs Msg.batchSendMessage

batchSendMessageForLater :: BatchSendMessageForLater -> Session [MessageId]
batchSendMessageForLater msgs = statement msgs Msg.batchSendMessageForLater

-- | Send a message with headers (pgmq 1.5.0+)
sendMessageWithHeaders :: SendMessageWithHeaders -> Session MessageId
sendMessageWithHeaders msg = statement msg Msg.sendMessageWithHeaders

-- | Send a message with headers for later (pgmq 1.5.0+)
sendMessageWithHeadersForLater :: SendMessageWithHeadersForLater -> Session MessageId
sendMessageWithHeadersForLater msg = statement msg Msg.sendMessageWithHeadersForLater

-- | Send a batch of messages with headers (pgmq 1.5.0+)
batchSendMessageWithHeaders :: BatchSendMessageWithHeaders -> Session [MessageId]
batchSendMessageWithHeaders msgs = statement msgs Msg.batchSendMessageWithHeaders

-- | Send a batch of messages with headers for later (pgmq 1.5.0+)
batchSendMessageWithHeadersForLater :: BatchSendMessageWithHeadersForLater -> Session [MessageId]
batchSendMessageWithHeadersForLater msgs = statement msgs Msg.batchSendMessageWithHeadersForLater

deleteMessage :: MessageQuery -> Session Bool
deleteMessage msg = statement msg Msg.deleteMessage

batchDeleteMessages :: BatchMessageQuery -> Session [MessageId]
batchDeleteMessages msgs = statement msgs Msg.batchDeleteMessages

archiveMessage :: MessageQuery -> Session Bool
archiveMessage msg = statement msg Msg.archiveMessage

batchArchiveMessages :: BatchMessageQuery -> Session [MessageId]
batchArchiveMessages msgs = statement msgs Msg.batchArchiveMessages

deleteAllMessagesFromQueue :: QueueName -> Session Int64
deleteAllMessagesFromQueue qname = statement qname Msg.deleteAllMessagesFromQueue

changeVisibilityTimeout :: VisibilityTimeoutQuery -> Session Message
changeVisibilityTimeout query = statement query Msg.changeVisibilityTimeout

-- | Batch update visibility timeout (pgmq 1.8.0+)
batchChangeVisibilityTimeout :: BatchVisibilityTimeoutQuery -> Session (Vector Message)
batchChangeVisibilityTimeout query = statement query Msg.batchChangeVisibilityTimeout

listQueues :: Session [Queue]
listQueues = statement () Stmt.listQueues

createPartitionedQueue :: CreatePartitionedQueue -> Session ()
createPartitionedQueue q = statement q Stmt.createPartitionedQueue

createUnloggedQueue :: QueueName -> Session ()
createUnloggedQueue q = statement q Stmt.createUnloggedQueue

{-# DEPRECATED detachArchive "detach_archive is a no-op in pgmq and will be removed in pgmq 2.0" #-}
detachArchive :: QueueName -> Session ()
detachArchive q = statement q Stmt.detachArchive

-- | Enable insert notifications for a queue (pgmq 1.7.0+)
enableNotifyInsert :: EnableNotifyInsert -> Session ()
enableNotifyInsert config = statement config Stmt.enableNotifyInsert

-- | Disable insert notifications for a queue
disableNotifyInsert :: QueueName -> Session ()
disableNotifyInsert q = statement q Stmt.disableNotifyInsert

-- | Pop messages from queue (pgmq 1.7.0+)
pop :: PopMessage -> Session (Vector Message)
pop query = statement query Msg.pop

queueMetrics :: QueueName -> Session QueueMetrics
queueMetrics q = statement q Stmt.queueMetrics

allQueueMetrics :: Session [QueueMetrics]
allQueueMetrics = statement () Stmt.allQueueMetrics

readMessage :: ReadMessage -> Session (Vector Message)
readMessage query = statement query Stmt.readMessage

readWithPoll :: ReadWithPollMessage -> Session (Vector Message)
readWithPoll query = statement query Stmt.readWithPoll
