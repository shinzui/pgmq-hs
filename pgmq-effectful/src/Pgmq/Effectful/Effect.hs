module Pgmq.Effectful.Effect
  ( -- * Effect
    Pgmq (..),

    -- * Queue Management
    createQueue,
    dropQueue,
    createPartitionedQueue,
    createUnloggedQueue,
    detachArchive,

    -- ** Notifications (pgmq 1.7.0+)
    enableNotifyInsert,
    disableNotifyInsert,

    -- ** FIFO Index (pgmq 1.8.0+)
    createFifoIndex,
    createFifoIndexesAll,

    -- * Message Operations
    sendMessage,
    sendMessageForLater,
    batchSendMessage,
    batchSendMessageForLater,

    -- ** With Headers (pgmq 1.5.0+)
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

    -- ** FIFO Read (pgmq 1.8.0+)
    readGrouped,
    readGroupedWithPoll,

    -- ** Round-Robin FIFO Read (pgmq 1.9.0+)
    readGroupedRoundRobin,
    readGroupedRoundRobinWithPoll,

    -- * Queue Observability
    listQueues,
    queueMetrics,
    allQueueMetrics,
  )
where

import Data.Int (Int64)
import Data.Vector (Vector)
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (send)
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
    ReadGrouped,
    ReadGroupedWithPoll,
    ReadMessage,
    ReadWithPollMessage,
    SendMessage,
    SendMessageForLater,
    SendMessageWithHeaders,
    SendMessageWithHeadersForLater,
    VisibilityTimeoutQuery,
  )
import Pgmq.Types (Message, MessageId, Queue, QueueName)

-- | Effect for pgmq message queue operations.
data Pgmq :: Effect where
  -- Queue Management
  CreateQueue :: QueueName -> Pgmq m ()
  DropQueue :: QueueName -> Pgmq m Bool
  CreatePartitionedQueue :: CreatePartitionedQueue -> Pgmq m ()
  CreateUnloggedQueue :: QueueName -> Pgmq m ()
  DetachArchive :: QueueName -> Pgmq m ()
  EnableNotifyInsert :: EnableNotifyInsert -> Pgmq m ()
  DisableNotifyInsert :: QueueName -> Pgmq m ()
  CreateFifoIndex :: QueueName -> Pgmq m ()
  CreateFifoIndexesAll :: Pgmq m ()
  -- Message Operations
  SendMessage :: SendMessage -> Pgmq m MessageId
  SendMessageForLater :: SendMessageForLater -> Pgmq m MessageId
  BatchSendMessage :: BatchSendMessage -> Pgmq m [MessageId]
  BatchSendMessageForLater :: BatchSendMessageForLater -> Pgmq m [MessageId]
  SendMessageWithHeaders :: SendMessageWithHeaders -> Pgmq m MessageId
  SendMessageWithHeadersForLater :: SendMessageWithHeadersForLater -> Pgmq m MessageId
  BatchSendMessageWithHeaders :: BatchSendMessageWithHeaders -> Pgmq m [MessageId]
  BatchSendMessageWithHeadersForLater :: BatchSendMessageWithHeadersForLater -> Pgmq m [MessageId]
  ReadMessage :: ReadMessage -> Pgmq m (Vector Message)
  DeleteMessage :: MessageQuery -> Pgmq m Bool
  BatchDeleteMessages :: BatchMessageQuery -> Pgmq m [MessageId]
  ArchiveMessage :: MessageQuery -> Pgmq m Bool
  BatchArchiveMessages :: BatchMessageQuery -> Pgmq m [MessageId]
  DeleteAllMessagesFromQueue :: QueueName -> Pgmq m Int64
  ChangeVisibilityTimeout :: VisibilityTimeoutQuery -> Pgmq m Message
  BatchChangeVisibilityTimeout :: BatchVisibilityTimeoutQuery -> Pgmq m (Vector Message)
  ReadWithPoll :: ReadWithPollMessage -> Pgmq m (Vector Message)
  Pop :: PopMessage -> Pgmq m (Vector Message)
  -- FIFO Read (pgmq 1.8.0+)
  ReadGrouped :: ReadGrouped -> Pgmq m (Vector Message)
  ReadGroupedWithPoll :: ReadGroupedWithPoll -> Pgmq m (Vector Message)
  -- Round-robin FIFO Read (pgmq 1.9.0+)
  ReadGroupedRoundRobin :: ReadGrouped -> Pgmq m (Vector Message)
  ReadGroupedRoundRobinWithPoll :: ReadGroupedWithPoll -> Pgmq m (Vector Message)
  -- Queue Observability
  ListQueues :: Pgmq m [Queue]
  QueueMetrics :: QueueName -> Pgmq m QueueMetrics
  AllQueueMetrics :: Pgmq m [QueueMetrics]

type instance DispatchOf Pgmq = 'Dynamic

-- Queue Management

createQueue :: (Pgmq :> es) => QueueName -> Eff es ()
createQueue = send . CreateQueue

dropQueue :: (Pgmq :> es) => QueueName -> Eff es Bool
dropQueue = send . DropQueue

createPartitionedQueue :: (Pgmq :> es) => CreatePartitionedQueue -> Eff es ()
createPartitionedQueue = send . CreatePartitionedQueue

createUnloggedQueue :: (Pgmq :> es) => QueueName -> Eff es ()
createUnloggedQueue = send . CreateUnloggedQueue

{-# DEPRECATED detachArchive "detach_archive is a no-op in pgmq and will be removed in pgmq 2.0" #-}
detachArchive :: (Pgmq :> es) => QueueName -> Eff es ()
detachArchive = send . DetachArchive

enableNotifyInsert :: (Pgmq :> es) => EnableNotifyInsert -> Eff es ()
enableNotifyInsert = send . EnableNotifyInsert

disableNotifyInsert :: (Pgmq :> es) => QueueName -> Eff es ()
disableNotifyInsert = send . DisableNotifyInsert

-- | Create FIFO index for a queue (pgmq 1.8.0+)
createFifoIndex :: (Pgmq :> es) => QueueName -> Eff es ()
createFifoIndex = send . CreateFifoIndex

-- | Create FIFO indexes for all queues (pgmq 1.8.0+)
createFifoIndexesAll :: (Pgmq :> es) => Eff es ()
createFifoIndexesAll = send CreateFifoIndexesAll

-- Message Operations

sendMessage :: (Pgmq :> es) => SendMessage -> Eff es MessageId
sendMessage = send . SendMessage

sendMessageForLater :: (Pgmq :> es) => SendMessageForLater -> Eff es MessageId
sendMessageForLater = send . SendMessageForLater

batchSendMessage :: (Pgmq :> es) => BatchSendMessage -> Eff es [MessageId]
batchSendMessage = send . BatchSendMessage

batchSendMessageForLater :: (Pgmq :> es) => BatchSendMessageForLater -> Eff es [MessageId]
batchSendMessageForLater = send . BatchSendMessageForLater

sendMessageWithHeaders :: (Pgmq :> es) => SendMessageWithHeaders -> Eff es MessageId
sendMessageWithHeaders = send . SendMessageWithHeaders

sendMessageWithHeadersForLater :: (Pgmq :> es) => SendMessageWithHeadersForLater -> Eff es MessageId
sendMessageWithHeadersForLater = send . SendMessageWithHeadersForLater

batchSendMessageWithHeaders :: (Pgmq :> es) => BatchSendMessageWithHeaders -> Eff es [MessageId]
batchSendMessageWithHeaders = send . BatchSendMessageWithHeaders

batchSendMessageWithHeadersForLater :: (Pgmq :> es) => BatchSendMessageWithHeadersForLater -> Eff es [MessageId]
batchSendMessageWithHeadersForLater = send . BatchSendMessageWithHeadersForLater

readMessage :: (Pgmq :> es) => ReadMessage -> Eff es (Vector Message)
readMessage = send . ReadMessage

deleteMessage :: (Pgmq :> es) => MessageQuery -> Eff es Bool
deleteMessage = send . DeleteMessage

batchDeleteMessages :: (Pgmq :> es) => BatchMessageQuery -> Eff es [MessageId]
batchDeleteMessages = send . BatchDeleteMessages

archiveMessage :: (Pgmq :> es) => MessageQuery -> Eff es Bool
archiveMessage = send . ArchiveMessage

batchArchiveMessages :: (Pgmq :> es) => BatchMessageQuery -> Eff es [MessageId]
batchArchiveMessages = send . BatchArchiveMessages

deleteAllMessagesFromQueue :: (Pgmq :> es) => QueueName -> Eff es Int64
deleteAllMessagesFromQueue = send . DeleteAllMessagesFromQueue

changeVisibilityTimeout :: (Pgmq :> es) => VisibilityTimeoutQuery -> Eff es Message
changeVisibilityTimeout = send . ChangeVisibilityTimeout

batchChangeVisibilityTimeout :: (Pgmq :> es) => BatchVisibilityTimeoutQuery -> Eff es (Vector Message)
batchChangeVisibilityTimeout = send . BatchChangeVisibilityTimeout

readWithPoll :: (Pgmq :> es) => ReadWithPollMessage -> Eff es (Vector Message)
readWithPoll = send . ReadWithPoll

pop :: (Pgmq :> es) => PopMessage -> Eff es (Vector Message)
pop = send . Pop

-- FIFO Read (pgmq 1.8.0+)

-- | FIFO read - fills batch from same message group (pgmq 1.8.0+)
readGrouped :: (Pgmq :> es) => ReadGrouped -> Eff es (Vector Message)
readGrouped = send . ReadGrouped

-- | FIFO read with polling (pgmq 1.8.0+)
readGroupedWithPoll :: (Pgmq :> es) => ReadGroupedWithPoll -> Eff es (Vector Message)
readGroupedWithPoll = send . ReadGroupedWithPoll

-- | Round-robin FIFO read (pgmq 1.9.0+)
readGroupedRoundRobin :: (Pgmq :> es) => ReadGrouped -> Eff es (Vector Message)
readGroupedRoundRobin = send . ReadGroupedRoundRobin

-- | Round-robin FIFO read with polling (pgmq 1.9.0+)
readGroupedRoundRobinWithPoll :: (Pgmq :> es) => ReadGroupedWithPoll -> Eff es (Vector Message)
readGroupedRoundRobinWithPoll = send . ReadGroupedRoundRobinWithPoll

-- Queue Observability

listQueues :: (Pgmq :> es) => Eff es [Queue]
listQueues = send ListQueues

queueMetrics :: (Pgmq :> es) => QueueName -> Eff es QueueMetrics
queueMetrics = send . QueueMetrics

allQueueMetrics :: (Pgmq :> es) => Eff es [QueueMetrics]
allQueueMetrics = send AllQueueMetrics
