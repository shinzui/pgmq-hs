module Pgmq.Db.Transactions
  ( createQueue,
    dropQueue,
    createPartitionedQueue,
    createUnloggedQueue,
    detachArchive,
    sendMessage,
    sendMessageForLater,
    batchSendMessage,
    batchSendMessageForLater,
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
  )
where

import Hasql.Session qualified as S
import Hasql.Transaction
import Hasql.Transaction.Sessions
import Pgmq.Db.Statements qualified as Db
import Pgmq.Db.Statements.Message qualified as Msg
import Pgmq.Db.Statements.Types
  ( BatchMessageQuery,
    BatchSendMessage,
    BatchSendMessageForLater,
    CreatePartitionedQueue,
    MessageQuery,
    QueueMetrics,
    SendMessage,
    SendMessageForLater,
    VisibilityTimeoutQuery,
  )
import Pgmq.Prelude
import Pgmq.Types (Message, MessageId, Queue, QueueName)

createQueue :: QueueName -> S.Session ()
createQueue q =
  transaction Serializable Write $
    statement q Db.createQueue

dropQueue :: QueueName -> S.Session Bool
dropQueue q =
  transaction Serializable Write $
    statement q Db.dropQueue

sendMessage :: SendMessage -> S.Session MessageId
sendMessage msg =
  transaction Serializable Write $
    statement msg Msg.sendMessage

sendMessageForLater :: SendMessageForLater -> S.Session MessageId
sendMessageForLater msg =
  transaction Serializable Write $
    statement msg Msg.sendMessageForLater

batchSendMessage :: BatchSendMessage -> S.Session [MessageId]
batchSendMessage msgs =
  transaction Serializable Write $
    statement msgs Msg.batchSendMessage

batchSendMessageForLater :: BatchSendMessageForLater -> S.Session [MessageId]
batchSendMessageForLater msgs =
  transaction Serializable Write $
    statement msgs Msg.batchSendMessageForLater

deleteMessage :: MessageQuery -> S.Session Bool
deleteMessage msg =
  transaction Serializable Write $
    statement msg Msg.deleteMessage

batchDeleteMessages :: BatchMessageQuery -> S.Session [MessageId]
batchDeleteMessages msgs =
  transaction Serializable Write $
    statement msgs Msg.batchDeleteMessages

archiveMessage :: MessageQuery -> S.Session Bool
archiveMessage msg =
  transaction Serializable Write $
    statement msg Msg.archiveMessage

batchArchiveMessages :: BatchMessageQuery -> S.Session [MessageId]
batchArchiveMessages msgs =
  transaction Serializable Write $
    statement msgs Msg.batchArchiveMessages

deleteAllMessagesFromQueue :: QueueName -> S.Session Int64
deleteAllMessagesFromQueue qname =
  transaction Serializable Write $
    statement qname Msg.deleteAllMessagesFromQueue

changeVisibilityTimeout :: VisibilityTimeoutQuery -> S.Session Message
changeVisibilityTimeout query =
  transaction Serializable Write $
    statement query Msg.changeVisibilityTimeout

listQueues :: S.Session [Queue]
listQueues =
  transaction Serializable Read $
    statement () Db.listQueues

createPartitionedQueue :: CreatePartitionedQueue -> S.Session ()
createPartitionedQueue q =
  transaction Serializable Write $
    statement q Db.createPartitionedQueue

createUnloggedQueue :: QueueName -> S.Session ()
createUnloggedQueue q =
  transaction Serializable Write $
    statement q Db.createUnloggedQueue

detachArchive :: QueueName -> S.Session ()
detachArchive q =
  transaction Serializable Write $
    statement q Db.detachArchive

pop :: QueueName -> S.Session Message
pop q =
  transaction Serializable Write $
    statement q Msg.pop

queueMetrics :: QueueName -> S.Session QueueMetrics
queueMetrics q =
  transaction Serializable Read $
    statement q Db.queueMetrics

allQueueMetrics :: S.Session [QueueMetrics]
allQueueMetrics =
  transaction Serializable Read $
    statement () Db.allQueueMetrics
