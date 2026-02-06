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

    -- ** Timestamp-based VT (pgmq 1.10.0+)
    setVisibilityTimeoutAt,
    batchSetVisibilityTimeoutAt,
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

    -- ** Timestamp-based VT types (pgmq 1.10.0+)
    VisibilityTimeoutAtQuery (..),
    BatchVisibilityTimeoutAtQuery (..),
    ReadWithPollMessage (..),
    CreatePartitionedQueue (..),
    QueueMetrics (..),

    -- * Queue Name Utilities
    parseQueueName,
    queueNameToText,
  )
where

import Pgmq.Hasql.Sessions
  ( allQueueMetrics,
    archiveMessage,
    batchArchiveMessages,
    batchChangeVisibilityTimeout,
    batchDeleteMessages,
    batchSendMessage,
    batchSendMessageForLater,
    batchSendMessageWithHeaders,
    batchSendMessageWithHeadersForLater,
    batchSetVisibilityTimeoutAt,
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
    setVisibilityTimeoutAt,
  )
import Pgmq.Hasql.Statements.Types
  ( BatchMessageQuery (..),
    BatchSendMessage (..),
    BatchSendMessageForLater (..),
    BatchSendMessageWithHeaders (..),
    BatchSendMessageWithHeadersForLater (..),
    BatchVisibilityTimeoutAtQuery (..),
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
    VisibilityTimeoutAtQuery (..),
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
