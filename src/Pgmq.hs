module Pgmq
  ( -- * Queue Management
    createQueue,
    dropQueue,
    createPartitionedQueue,
    createUnloggedQueue,
    detachArchive, -- DEPRECATED: no-op, will be removed in pgmq 2.0

    -- ** Notifications (pgmq 1.7.0+)
    enableNotifyInsert,
    disableNotifyInsert,

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
    batchChangeVisibilityTimeout, -- pgmq 1.8.0+
    listQueues,
    readWithPoll,
    pop,
    queueMetrics,
    allQueueMetrics,

    -- * Types
    MessageBody (..),
    MessageHeaders (..),
    MessageId (..),
    Message (..),
    Queue (..),
    QueueName,
    SendMessage (..),
    SendMessageForLater (..),
    BatchSendMessage (..),
    BatchSendMessageForLater (..),

    -- ** With Headers (pgmq 1.5.0+)
    SendMessageWithHeaders (..),
    SendMessageWithHeadersForLater (..),
    BatchSendMessageWithHeaders (..),
    BatchSendMessageWithHeadersForLater (..),
    ReadMessage (..),
    PopMessage (..),
    EnableNotifyInsert (..), -- pgmq 1.7.0+
    MessageQuery (..),
    BatchMessageQuery (..),
    VisibilityTimeoutQuery (..),
    BatchVisibilityTimeoutQuery (..), -- pgmq 1.8.0+
    ReadWithPollMessage (..),
    CreatePartitionedQueue (..),
    QueueMetrics (..),

    -- * Queue Name Utilities
    parseQueueName,
    queueNameToText,
  )
where

import Pgmq.Db.Sessions
  ( allQueueMetrics,
    archiveMessage,
    batchArchiveMessages,
    batchChangeVisibilityTimeout,
    batchDeleteMessages,
    batchSendMessage,
    batchSendMessageForLater,
    batchSendMessageWithHeaders,
    batchSendMessageWithHeadersForLater,
    changeVisibilityTimeout,
    createPartitionedQueue,
    createQueue,
    createUnloggedQueue,
    deleteAllMessagesFromQueue,
    deleteMessage,
    detachArchive,
    disableNotifyInsert,
    dropQueue,
    enableNotifyInsert,
    listQueues,
    pop,
    queueMetrics,
    readMessage,
    readWithPoll,
    sendMessage,
    sendMessageForLater,
    sendMessageWithHeaders,
    sendMessageWithHeadersForLater,
  )
import Pgmq.Db.Statements.Types
  ( BatchMessageQuery (..),
    BatchSendMessage (..),
    BatchSendMessageForLater (..),
    BatchSendMessageWithHeaders (..),
    BatchSendMessageWithHeadersForLater (..),
    BatchVisibilityTimeoutQuery (..),
    CreatePartitionedQueue (..),
    EnableNotifyInsert (..),
    MessageQuery (..),
    PopMessage (..),
    QueueMetrics (..),
    ReadMessage (..),
    ReadWithPollMessage (..),
    SendMessage (..),
    SendMessageForLater (..),
    SendMessageWithHeaders (..),
    SendMessageWithHeadersForLater (..),
    VisibilityTimeoutQuery (..),
  )
import Pgmq.Types
  ( Message (..),
    MessageBody (..),
    MessageHeaders (..),
    MessageId (..),
    Queue (..),
    QueueName,
    parseQueueName,
    queueNameToText,
  )
