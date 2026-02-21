module Pgmq.Effectful
  ( -- * Effect
    Pgmq,

    -- * Interpreters
    runPgmq,
    PgmqError (..),

    -- ** Traced Interpreters
    runPgmqTraced,
    runPgmqTracedWith,
    TracingConfig (..),
    defaultTracingConfig,

    -- ** Traced Operations
    sendMessageTraced,
    readMessageWithContext,
    MessageWithContext,

    -- ** Telemetry Utilities
    injectTraceContext,
    extractTraceContext,
    mergeTraceHeaders,
    TraceHeaders,

    -- * Queue Management
    createQueue,
    dropQueue,
    createPartitionedQueue,
    createUnloggedQueue,
    detachArchive,

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
    batchChangeVisibilityTimeout,
    readWithPoll,
    pop,

    -- * Topic Routing (pgmq 1.11.0+)

    -- ** Topic Management
    bindTopic,
    unbindTopic,
    validateRoutingKey,
    validateTopicPattern,
    testRouting,
    listTopicBindings,
    listTopicBindingsForQueue,

    -- ** Topic Sending
    sendTopic,
    sendTopicWithHeaders,
    batchSendTopic,
    batchSendTopicForLater,
    batchSendTopicWithHeaders,
    batchSendTopicWithHeadersForLater,

    -- ** Notification Management
    listNotifyInsertThrottles,
    updateNotifyInsert,

    -- * Queue Observability
    listQueues,
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
    EnableNotifyInsert (..),
    MessageQuery (..),
    BatchMessageQuery (..),
    VisibilityTimeoutQuery (..),
    BatchVisibilityTimeoutQuery (..),
    ReadWithPollMessage (..),
    CreatePartitionedQueue (..),
    QueueMetrics (..),

    -- ** Topic types (pgmq 1.11.0+)
    RoutingKey,
    parseRoutingKey,
    routingKeyToText,
    TopicPattern,
    parseTopicPattern,
    topicPatternToText,
    TopicBinding (..),
    RoutingMatch (..),
    TopicSendResult (..),
    NotifyInsertThrottle (..),
    BindTopic (..),
    UnbindTopic (..),
    SendTopic (..),
    SendTopicWithHeaders (..),
    BatchSendTopic (..),
    BatchSendTopicForLater (..),
    BatchSendTopicWithHeaders (..),
    BatchSendTopicWithHeadersForLater (..),
    UpdateNotifyInsert (..),

    -- * Queue Name Utilities
    parseQueueName,
    queueNameToText,
  )
where

import Pgmq.Effectful.Effect
  ( Pgmq,
    allQueueMetrics,
    archiveMessage,
    batchArchiveMessages,
    batchChangeVisibilityTimeout,
    batchDeleteMessages,
    batchSendMessage,
    batchSendMessageForLater,
    batchSendMessageWithHeaders,
    batchSendMessageWithHeadersForLater,
    batchSendTopic,
    batchSendTopicForLater,
    batchSendTopicWithHeaders,
    batchSendTopicWithHeadersForLater,
    bindTopic,
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
    listNotifyInsertThrottles,
    listQueues,
    listTopicBindings,
    listTopicBindingsForQueue,
    pop,
    queueMetrics,
    readMessage,
    readWithPoll,
    sendMessage,
    sendMessageForLater,
    sendMessageWithHeaders,
    sendMessageWithHeadersForLater,
    sendTopic,
    sendTopicWithHeaders,
    testRouting,
    unbindTopic,
    updateNotifyInsert,
    validateRoutingKey,
    validateTopicPattern,
  )
import Pgmq.Effectful.Interpreter (PgmqError (..), runPgmq)
import Pgmq.Effectful.Interpreter.Traced
  ( TracingConfig (..),
    defaultTracingConfig,
    runPgmqTraced,
    runPgmqTracedWith,
  )
import Pgmq.Effectful.Telemetry
  ( TraceHeaders,
    extractTraceContext,
    injectTraceContext,
    mergeTraceHeaders,
  )
import Pgmq.Effectful.Traced
  ( MessageWithContext,
    readMessageWithContext,
    sendMessageTraced,
  )
import Pgmq.Hasql.Statements.Types
  ( BatchMessageQuery (..),
    BatchSendMessage (..),
    BatchSendMessageForLater (..),
    BatchSendMessageWithHeaders (..),
    BatchSendMessageWithHeadersForLater (..),
    BatchSendTopic (..),
    BatchSendTopicForLater (..),
    BatchSendTopicWithHeaders (..),
    BatchSendTopicWithHeadersForLater (..),
    BatchVisibilityTimeoutQuery (..),
    BindTopic (..),
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
    SendTopic (..),
    SendTopicWithHeaders (..),
    UnbindTopic (..),
    UpdateNotifyInsert (..),
    VisibilityTimeoutQuery (..),
  )
import Pgmq.Types
  ( Message (..),
    MessageBody (..),
    MessageHeaders (..),
    MessageId (..),
    NotifyInsertThrottle (..),
    Queue (..),
    QueueName,
    RoutingKey,
    RoutingMatch (..),
    TopicBinding (..),
    TopicPattern,
    TopicSendResult (..),
    parseQueueName,
    parseRoutingKey,
    parseTopicPattern,
    queueNameToText,
    routingKeyToText,
    topicPatternToText,
  )
