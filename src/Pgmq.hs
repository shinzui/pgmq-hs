{-# LANGUAGE QuasiQuotes #-}

module Pgmq
  ( -- * Queue Management
    createQueue,
    dropQueue,
    createPartitionedQueue,
    createUnloggedQueue,
    detachArchive,

    -- * Message Operations
    sendMessage,
    sendMessageForLater,
    batchSendMessage,
    batchSendMessageForLater,
    readMessage,
    deleteMessage,
    batchDeleteMessages,
    archiveMessage,
    batchArchiveMessages,
    deleteAllMessagesFromQueue,
    changeVisibilityTimeout,
    listQueues,
    readWithPoll,
    pop,
    queueMetrics,
    allQueueMetrics,

    -- * Types
    MessageBody (..),
    MessageId (..),
    Message (..),
    Queue (..),
    QueueName,
    SendMessage (..),
    SendMessageForLater (..),
    BatchSendMessage (..),
    BatchSendMessageForLater (..),
    ReadMessage (..),
    MessageQuery (..),
    BatchMessageQuery (..),
    VisibilityTimeoutQuery (..),
    ReadWithPollMessage (..),
    CreatePartitionedQueue (..),
    QueueMetrics (..),

    -- * Queue Name Utilities
    parseQueueName,
    queueNameToText,
  )
where

import Pgmq.Db.Sessions (readMessage, readWithPoll)
import Pgmq.Db.Statements.Types
  ( BatchMessageQuery (..),
    BatchSendMessage (..),
    BatchSendMessageForLater (..),
    CreatePartitionedQueue (..),
    MessageQuery (..),
    QueueMetrics (..),
    ReadMessage (..),
    ReadWithPollMessage (..),
    SendMessage (..),
    SendMessageForLater (..),
    VisibilityTimeoutQuery (..),
  )
import Pgmq.Db.Transactions
  ( allQueueMetrics,
    archiveMessage,
    batchArchiveMessages,
    batchDeleteMessages,
    batchSendMessage,
    batchSendMessageForLater,
    changeVisibilityTimeout,
    createPartitionedQueue,
    createQueue,
    createUnloggedQueue,
    deleteAllMessagesFromQueue,
    deleteMessage,
    detachArchive,
    dropQueue,
    listQueues,
    pop,
    queueMetrics,
    sendMessage,
    sendMessageForLater,
  )
import Pgmq.Types
  ( Message (..),
    MessageBody (..),
    MessageId (..),
    Queue (..),
    QueueName,
    parseQueueName,
    queueNameToText,
  )
