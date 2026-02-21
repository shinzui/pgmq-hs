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
    -- Timestamp-based VT functions (pgmq 1.10.0+)
    setVisibilityTimeoutAt,
    batchSetVisibilityTimeoutAt,
    listQueues,
    pop,
    queueMetrics,
    allQueueMetrics,
    readMessage,
    readWithPoll,
    -- FIFO read functions (pgmq 1.8.0+)
    readGrouped,
    readGroupedWithPoll,
    -- Round-robin FIFO functions (pgmq 1.9.0+)
    readGroupedRoundRobin,
    readGroupedRoundRobinWithPoll,
    -- FIFO index functions (pgmq 1.8.0+)
    createFifoIndex,
    createFifoIndexesAll,
    -- Topic management (pgmq 1.11.0+)
    bindTopic,
    unbindTopic,
    validateRoutingKey,
    validateTopicPattern,
    testRouting,
    listTopicBindings,
    listTopicBindingsForQueue,
    -- Topic sending (pgmq 1.11.0+)
    sendTopic,
    sendTopicWithHeaders,
    batchSendTopic,
    batchSendTopicForLater,
    batchSendTopicWithHeaders,
    batchSendTopicWithHeadersForLater,
    -- Notification management (pgmq 1.11.0+)
    listNotifyInsertThrottles,
    updateNotifyInsert,
  )
where

import Hasql.Session (Session, statement)
import Pgmq.Hasql.Prelude
import Pgmq.Hasql.Statements qualified as Stmt
import Pgmq.Hasql.Statements.Message qualified as Msg
import Pgmq.Hasql.Statements.TopicManagement qualified as Topic
import Pgmq.Hasql.Statements.Types
  ( BatchMessageQuery,
    BatchSendMessage,
    BatchSendMessageForLater,
    BatchSendMessageWithHeaders,
    BatchSendMessageWithHeadersForLater,
    BatchSendTopic,
    BatchSendTopicForLater,
    BatchSendTopicWithHeaders,
    BatchSendTopicWithHeadersForLater,
    BatchVisibilityTimeoutAtQuery,
    BatchVisibilityTimeoutQuery,
    BindTopic,
    CreatePartitionedQueue,
    EnableNotifyInsert,
    MessageQuery,
    PopMessage,
    QueueMetrics,
    ReadGrouped,
    ReadGroupedWithPoll,
    ReadMessage,
    ReadWithPollMessage,
    SendMessage,
    SendMessageForLater,
    SendMessageWithHeaders,
    SendMessageWithHeadersForLater,
    SendTopic,
    SendTopicWithHeaders,
    UnbindTopic,
    UpdateNotifyInsert,
    VisibilityTimeoutAtQuery,
    VisibilityTimeoutQuery,
  )
import Pgmq.Types
  ( Message,
    MessageId,
    NotifyInsertThrottle,
    Queue,
    QueueName,
    RoutingKey,
    RoutingMatch,
    TopicBinding,
    TopicPattern,
    TopicSendResult,
  )

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

-- | Set visibility timeout to an absolute timestamp (pgmq 1.10.0+)
setVisibilityTimeoutAt :: VisibilityTimeoutAtQuery -> Session Message
setVisibilityTimeoutAt query = statement query Msg.setVisibilityTimeoutAt

-- | Batch set visibility timeout to an absolute timestamp (pgmq 1.10.0+)
batchSetVisibilityTimeoutAt :: BatchVisibilityTimeoutAtQuery -> Session (Vector Message)
batchSetVisibilityTimeoutAt query = statement query Msg.batchSetVisibilityTimeoutAt

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

-- | FIFO read - fills batch from same message group (pgmq 1.8.0+)
readGrouped :: ReadGrouped -> Session (Vector Message)
readGrouped query = statement query Msg.readGrouped

-- | FIFO read with polling (pgmq 1.8.0+)
readGroupedWithPoll :: ReadGroupedWithPoll -> Session (Vector Message)
readGroupedWithPoll query = statement query Msg.readGroupedWithPoll

-- | Round-robin FIFO read (pgmq 1.9.0+)
readGroupedRoundRobin :: ReadGrouped -> Session (Vector Message)
readGroupedRoundRobin query = statement query Msg.readGroupedRoundRobin

-- | Round-robin FIFO read with polling (pgmq 1.9.0+)
readGroupedRoundRobinWithPoll :: ReadGroupedWithPoll -> Session (Vector Message)
readGroupedRoundRobinWithPoll query = statement query Msg.readGroupedRoundRobinWithPoll

-- | Create FIFO index for a queue (pgmq 1.8.0+)
createFifoIndex :: QueueName -> Session ()
createFifoIndex q = statement q Stmt.createFifoIndex

-- | Create FIFO indexes for all queues (pgmq 1.8.0+)
createFifoIndexesAll :: Session ()
createFifoIndexesAll = statement () Stmt.createFifoIndexesAll

-- Topic Management (pgmq 1.11.0+)

-- | Bind a topic pattern to a queue (pgmq 1.11.0+)
bindTopic :: BindTopic -> Session ()
bindTopic params = statement params Topic.bindTopic

-- | Unbind a topic pattern from a queue (pgmq 1.11.0+)
unbindTopic :: UnbindTopic -> Session Bool
unbindTopic params = statement params Topic.unbindTopic

-- | Validate a routing key (pgmq 1.11.0+)
validateRoutingKey :: RoutingKey -> Session Bool
validateRoutingKey key = statement key Topic.validateRoutingKey

-- | Validate a topic pattern (pgmq 1.11.0+)
validateTopicPattern :: TopicPattern -> Session Bool
validateTopicPattern pat = statement pat Topic.validateTopicPattern

-- | Test which queues a routing key would match (pgmq 1.11.0+)
testRouting :: RoutingKey -> Session [RoutingMatch]
testRouting key = statement key Topic.testRouting

-- | List all topic bindings (pgmq 1.11.0+)
listTopicBindings :: Session [TopicBinding]
listTopicBindings = statement () Topic.listTopicBindings

-- | List topic bindings for a specific queue (pgmq 1.11.0+)
listTopicBindingsForQueue :: QueueName -> Session [TopicBinding]
listTopicBindingsForQueue q = statement q Topic.listTopicBindingsForQueue

-- Topic Sending (pgmq 1.11.0+)

-- | Send a message via topic routing (pgmq 1.11.0+)
sendTopic :: SendTopic -> Session Int32
sendTopic msg = statement msg Msg.sendTopic

-- | Send a message via topic routing with headers (pgmq 1.11.0+)
sendTopicWithHeaders :: SendTopicWithHeaders -> Session Int32
sendTopicWithHeaders msg = statement msg Msg.sendTopicWithHeaders

-- | Batch send messages via topic routing (pgmq 1.11.0+)
batchSendTopic :: BatchSendTopic -> Session [TopicSendResult]
batchSendTopic msgs = statement msgs Msg.batchSendTopic

-- | Batch send messages via topic routing for later (pgmq 1.11.0+)
batchSendTopicForLater :: BatchSendTopicForLater -> Session [TopicSendResult]
batchSendTopicForLater msgs = statement msgs Msg.batchSendTopicForLater

-- | Batch send messages via topic routing with headers (pgmq 1.11.0+)
batchSendTopicWithHeaders :: BatchSendTopicWithHeaders -> Session [TopicSendResult]
batchSendTopicWithHeaders msgs = statement msgs Msg.batchSendTopicWithHeaders

-- | Batch send messages via topic routing with headers for later (pgmq 1.11.0+)
batchSendTopicWithHeadersForLater :: BatchSendTopicWithHeadersForLater -> Session [TopicSendResult]
batchSendTopicWithHeadersForLater msgs = statement msgs Msg.batchSendTopicWithHeadersForLater

-- Notification Management (pgmq 1.11.0+)

-- | List all notification insert throttle settings (pgmq 1.11.0+)
listNotifyInsertThrottles :: Session [NotifyInsertThrottle]
listNotifyInsertThrottles = statement () Stmt.listNotifyInsertThrottles

-- | Update the throttle interval for a queue's insert notifications (pgmq 1.11.0+)
updateNotifyInsert :: UpdateNotifyInsert -> Session ()
updateNotifyInsert params = statement params Stmt.updateNotifyInsert
