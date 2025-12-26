module Pgmq.Db.Sessions
  ( createQueue,
    dropQueue,
    createPartitionedQueue,
    createUnloggedQueue,
    detachArchive,
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
    listQueues,
    pop,
    queueMetrics,
    allQueueMetrics,
    readMessage,
    readWithPoll,
  )
where

import Hasql.Session (Session, statement)
import Pgmq.Db.Statements qualified as Db
import Pgmq.Db.Statements.Message qualified as Msg
import Pgmq.Db.Statements.Types
  ( BatchMessageQuery,
    BatchSendMessage,
    BatchSendMessageForLater,
    BatchSendMessageWithHeaders,
    BatchSendMessageWithHeadersForLater,
    CreatePartitionedQueue,
    MessageQuery,
    QueueMetrics,
    ReadMessage,
    ReadWithPollMessage,
    SendMessage,
    SendMessageForLater,
    SendMessageWithHeaders,
    SendMessageWithHeadersForLater,
    VisibilityTimeoutQuery,
  )
import Pgmq.Prelude
import Pgmq.Types (Message, MessageId, Queue, QueueName)

createQueue :: QueueName -> Session ()
createQueue q = statement q Db.createQueue

dropQueue :: QueueName -> Session Bool
dropQueue q = statement q Db.dropQueue

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

listQueues :: Session [Queue]
listQueues = statement () Db.listQueues

createPartitionedQueue :: CreatePartitionedQueue -> Session ()
createPartitionedQueue q = statement q Db.createPartitionedQueue

createUnloggedQueue :: QueueName -> Session ()
createUnloggedQueue q = statement q Db.createUnloggedQueue

detachArchive :: QueueName -> Session ()
detachArchive q = statement q Db.detachArchive

pop :: QueueName -> Session Message
pop q = statement q Msg.pop

queueMetrics :: QueueName -> Session QueueMetrics
queueMetrics q = statement q Db.queueMetrics

allQueueMetrics :: Session [QueueMetrics]
allQueueMetrics = statement () Db.allQueueMetrics

readMessage :: ReadMessage -> Session (Vector Message)
readMessage query = statement query Db.readMessage

readWithPoll :: ReadWithPollMessage -> Session (Vector Message)
readWithPoll query = statement query Db.readWithPoll
