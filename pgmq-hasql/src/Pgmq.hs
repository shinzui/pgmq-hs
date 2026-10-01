module Pgmq
  ( -- * Queue Management
    createQueue,
    dropQueue,
    createPartitionedQueue,
    createPartitionedQueueWithPremake,
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
    listQueuesUnvalidated,
    listFifoIndexQueueNames,
    readWithPoll,
    pop,
    queueMetrics,
    allQueueMetrics,

    -- * FIFO / Grouped Reads

    -- | Head reads (PGMQ 1.12+) lease at most one absolute head per group.
    -- An invisible head blocks its group; expiry makes that same head eligible.
    -- Polling occupies a database connection. Leases do not guarantee exactly-once processing.
    readGrouped,
    readGroupedWithPoll,
    readGroupedRoundRobin,
    readGroupedRoundRobinWithPoll,
    readGroupedHead,
    readGroupedHeadWithPoll,
    ReadGrouped (..),
    ReadGroupedWithPoll (..),

    -- * Non-destructive Inspection

    -- | Reads that observe a queue or its archive without leasing anything:
    -- @vt@ and @read_ct@ are untouched. They accept any server-accepted name
    -- (pass 'queueNameToText' for a validated one) and page by exclusive
    -- @msg_id@ cursor, never @OFFSET@. See
    -- @docs/design/019-non-destructive-inspection-reads.md@.
    peekMessages,
    peekArchivedMessages,
    lookupMessage,
    lookupArchivedMessage,
    queueMetricsUnvalidated,
    PeekMessages (..),
    LookupMessage (..),

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

    -- * Types
    MessageBody (..),
    MessageHeaders (..),
    MessageId (..),
    Message (..),
    ArchivedMessage (..),
    Queue (..),
    UnvalidatedQueue (..),
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
    notifyChannelName,
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
    batchSendTopic,
    batchSendTopicForLater,
    batchSendTopicWithHeaders,
    batchSendTopicWithHeadersForLater,
    batchSetVisibilityTimeoutAt,
    bindTopic,
    changeVisibilityTimeout,
    createPartitionedQueue,
    createPartitionedQueueWithPremake,
    createQueue,
    createUnloggedQueue,
    deleteAllMessagesFromQueue,
    deleteMessage,
    detachArchive,
    disableNotifyInsert,
    dropQueue,
    enableNotifyInsert,
    listFifoIndexQueueNames,
    listNotifyInsertThrottles,
    listQueues,
    listQueuesUnvalidated,
    listTopicBindings,
    listTopicBindingsForQueue,
    lookupArchivedMessage,
    lookupMessage,
    peekArchivedMessages,
    peekMessages,
    pop,
    queueMetrics,
    queueMetricsUnvalidated,
    readGrouped,
    readGroupedHead,
    readGroupedHeadWithPoll,
    readGroupedRoundRobin,
    readGroupedRoundRobinWithPoll,
    readGroupedWithPoll,
    readMessage,
    readWithPoll,
    sendMessage,
    sendMessageForLater,
    sendMessageWithHeaders,
    sendMessageWithHeadersForLater,
    sendTopic,
    sendTopicWithHeaders,
    setVisibilityTimeoutAt,
    testRouting,
    unbindTopic,
    updateNotifyInsert,
    validateRoutingKey,
    validateTopicPattern,
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
    BatchVisibilityTimeoutAtQuery (..),
    BatchVisibilityTimeoutQuery (..),
    BindTopic (..),
    CreatePartitionedQueue (..),
    EnableNotifyInsert (..),
    LookupMessage (..),
    MessageQuery (..),
    PeekMessages (..),
    PopMessage (..),
    QueueMetrics (..),
    ReadGrouped (..),
    ReadGroupedWithPoll (..),
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
    VisibilityTimeoutAtQuery (..),
    VisibilityTimeoutQuery (..),
  )
import Pgmq.Types
  ( ArchivedMessage (..),
    Message (..),
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
    UnvalidatedQueue (..),
    notifyChannelName,
    parseQueueName,
    parseRoutingKey,
    parseTopicPattern,
    queueNameToText,
    routingKeyToText,
    topicPatternToText,
  )
