module Pgmq.Hasql.Encoders
  ( queueNameValue,
    sendMessageEncoder,
    sendMessageForLaterEncoder,
    batchSendMessageEncoder,
    batchSendMessageForLaterEncoder,
    sendMessageWithHeadersEncoder,
    sendMessageWithHeadersForLaterEncoder,
    batchSendMessageWithHeadersEncoder,
    batchSendMessageWithHeadersForLaterEncoder,
    messageIdValue,
    messageHeadersValue,
    readMessageEncoder,
    popMessageEncoder,
    messageQueryEncoder,
    batchMessageQueryEncoder,
    queueNameEncoder,
    visibilityTimeoutQueryEncoder,
    batchVisibilityTimeoutQueryEncoder,
    -- Timestamp-based VT encoders (pgmq 1.10.0+)
    visibilityTimeoutAtQueryEncoder,
    batchVisibilityTimeoutAtQueryEncoder,
    enableNotifyInsertEncoder,
    readWithPollEncoder,
    createPartitionedQueueEncoder,
    -- FIFO encoders (pgmq 1.8.0+)
    readGroupedEncoder,
    readGroupedWithPollEncoder,
    -- Topic encoders (pgmq 1.11.0+)
    routingKeyValue,
    topicPatternValue,
    bindTopicEncoder,
    unbindTopicEncoder,
    sendTopicEncoder,
    sendTopicWithHeadersEncoder,
    batchSendTopicEncoder,
    batchSendTopicForLaterEncoder,
    batchSendTopicWithHeadersEncoder,
    batchSendTopicWithHeadersForLaterEncoder,
    updateNotifyInsertEncoder,
  )
where

import Data.Generics.Product (HasField')
import Hasql.Encoders qualified as E
import Pgmq.Hasql.Prelude
import Pgmq.Hasql.Statements.Types
import Pgmq.Types
  ( MessageBody (..),
    MessageHeaders (..),
    MessageId (..),
    QueueName,
    RoutingKey,
    TopicPattern,
    queueNameToText,
    routingKeyToText,
    topicPatternToText,
  )

queueNameEncoder :: E.Params QueueName
queueNameEncoder = E.param (E.nonNullable queueNameValue)

queueNameValue :: E.Value QueueName
queueNameValue = queueNameToText >$< E.text

messageBodyValue :: E.Value MessageBody
messageBodyValue = unMessageBody >$< E.jsonb

messageIdValue :: E.Value MessageId
messageIdValue = unMessageId >$< E.int8

messageHeadersValue :: E.Value MessageHeaders
messageHeadersValue = unMessageHeaders >$< E.jsonb

-- | Common encoder for queue message fields
commonSendMessageFields :: (HasField' "queueName" a QueueName, HasField' "messageBody" a MessageBody) => E.Params a
commonSendMessageFields =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #messageBody >$< E.param (E.nonNullable messageBodyValue))

sendMessageEncoder :: E.Params SendMessage
sendMessageEncoder =
  commonSendMessageFields
    <> (view #delay >$< E.param (E.nullable E.int4))

sendMessageForLaterEncoder :: E.Params SendMessageForLater
sendMessageForLaterEncoder =
  commonSendMessageFields
    <> (view #scheduledAt >$< E.param (E.nonNullable E.timestamptz))

-- | Common encoder for batch message fields
commonBatchSendMessageFields :: (HasField' "queueName" a QueueName, HasField' "messageBodies" a [MessageBody]) => E.Params a
commonBatchSendMessageFields =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #messageBodies >$< E.param (E.nonNullable (E.array (E.dimension foldl' (E.element (E.nonNullable messageBodyValue))))))

batchSendMessageEncoder :: E.Params BatchSendMessage
batchSendMessageEncoder =
  commonBatchSendMessageFields
    <> (view #delay >$< E.param (E.nullable E.int4))

batchSendMessageForLaterEncoder :: E.Params BatchSendMessageForLater
batchSendMessageForLaterEncoder =
  commonBatchSendMessageFields
    <> (view #scheduledAt >$< E.param (E.nonNullable E.timestamptz))

-- | Encoder for SendMessageWithHeaders (pgmq 1.5.0+)
-- SQL: pgmq.send(queue_name, msg, headers, delay)
sendMessageWithHeadersEncoder :: E.Params SendMessageWithHeaders
sendMessageWithHeadersEncoder =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #messageBody >$< E.param (E.nonNullable messageBodyValue))
    <> (view #messageHeaders >$< E.param (E.nonNullable messageHeadersValue))
    <> (view #delay >$< E.param (E.nullable E.int4))

-- | Encoder for SendMessageWithHeadersForLater (pgmq 1.5.0+)
-- SQL: pgmq.send(queue_name, msg, headers, timestamp)
sendMessageWithHeadersForLaterEncoder :: E.Params SendMessageWithHeadersForLater
sendMessageWithHeadersForLaterEncoder =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #messageBody >$< E.param (E.nonNullable messageBodyValue))
    <> (view #messageHeaders >$< E.param (E.nonNullable messageHeadersValue))
    <> (view #scheduledAt >$< E.param (E.nonNullable E.timestamptz))

-- | Encoder for BatchSendMessageWithHeaders (pgmq 1.5.0+)
-- SQL: pgmq.send_batch(queue_name, msgs[], headers[], delay)
batchSendMessageWithHeadersEncoder :: E.Params BatchSendMessageWithHeaders
batchSendMessageWithHeadersEncoder =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #messageBodies >$< E.param (E.nonNullable (E.array (E.dimension foldl' (E.element (E.nonNullable messageBodyValue))))))
    <> (view #messageHeaders >$< E.param (E.nonNullable (E.array (E.dimension foldl' (E.element (E.nonNullable messageHeadersValue))))))
    <> (view #delay >$< E.param (E.nullable E.int4))

-- | Encoder for BatchSendMessageWithHeadersForLater (pgmq 1.5.0+)
-- SQL: pgmq.send_batch(queue_name, msgs[], headers[], timestamp)
batchSendMessageWithHeadersForLaterEncoder :: E.Params BatchSendMessageWithHeadersForLater
batchSendMessageWithHeadersForLaterEncoder =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #messageBodies >$< E.param (E.nonNullable (E.array (E.dimension foldl' (E.element (E.nonNullable messageBodyValue))))))
    <> (view #messageHeaders >$< E.param (E.nonNullable (E.array (E.dimension foldl' (E.element (E.nonNullable messageHeadersValue))))))
    <> (view #scheduledAt >$< E.param (E.nonNullable E.timestamptz))

-- | Encoder for the 3-param pgmq.read (without conditional filter)
readMessageEncoder :: E.Params ReadMessage
readMessageEncoder =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #delay >$< E.param (E.nonNullable E.int4))
    <> (view #batchSize >$< E.param (E.nullable E.int4))

-- | Encoder for PopMessage (pgmq 1.7.0+)
popMessageEncoder :: E.Params PopMessage
popMessageEncoder =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #qty >$< E.param (E.nullable E.int4))

messageQueryEncoder :: E.Params MessageQuery
messageQueryEncoder =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #messageId >$< E.param (E.nonNullable messageIdValue))

batchMessageQueryEncoder :: E.Params BatchMessageQuery
batchMessageQueryEncoder =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #messageIds >$< E.param (E.nonNullable (E.array (E.dimension foldl' (E.element (E.nonNullable messageIdValue))))))

visibilityTimeoutQueryEncoder :: E.Params VisibilityTimeoutQuery
visibilityTimeoutQueryEncoder =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #messageId >$< E.param (E.nonNullable messageIdValue))
    <> (view #visibilityTimeoutOffset >$< E.param (E.nonNullable E.int4))

-- | Encoder for BatchVisibilityTimeoutQuery (pgmq 1.8.0+)
batchVisibilityTimeoutQueryEncoder :: E.Params BatchVisibilityTimeoutQuery
batchVisibilityTimeoutQueryEncoder =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #messageIds >$< E.param (E.nonNullable (E.array (E.dimension foldl' (E.element (E.nonNullable messageIdValue))))))
    <> (view #visibilityTimeoutOffset >$< E.param (E.nonNullable E.int4))

-- | Encoder for VisibilityTimeoutAtQuery (pgmq 1.10.0+)
-- SQL: pgmq.set_vt(queue_name, msg_id, timestamp)
visibilityTimeoutAtQueryEncoder :: E.Params VisibilityTimeoutAtQuery
visibilityTimeoutAtQueryEncoder =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #messageId >$< E.param (E.nonNullable messageIdValue))
    <> (view #visibilityTime >$< E.param (E.nonNullable E.timestamptz))

-- | Encoder for BatchVisibilityTimeoutAtQuery (pgmq 1.10.0+)
-- SQL: pgmq.set_vt(queue_name, msg_ids[], timestamp)
batchVisibilityTimeoutAtQueryEncoder :: E.Params BatchVisibilityTimeoutAtQuery
batchVisibilityTimeoutAtQueryEncoder =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #messageIds >$< E.param (E.nonNullable (E.array (E.dimension foldl' (E.element (E.nonNullable messageIdValue))))))
    <> (view #visibilityTime >$< E.param (E.nonNullable E.timestamptz))

-- | Encoder for EnableNotifyInsert (pgmq 1.7.0+)
enableNotifyInsertEncoder :: E.Params EnableNotifyInsert
enableNotifyInsertEncoder =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #throttleIntervalMs >$< E.param (E.nullable E.int4))

readWithPollEncoder :: E.Params ReadWithPollMessage
readWithPollEncoder =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #delay >$< E.param (E.nonNullable E.int4))
    <> (view #batchSize >$< E.param (E.nullable E.int4))
    <> (view #maxPollSeconds >$< E.param (E.nonNullable E.int4))
    <> (view #pollIntervalMs >$< E.param (E.nonNullable E.int4))
    <> (view #conditional >$< E.param (E.nullable E.jsonb))

createPartitionedQueueEncoder :: E.Params CreatePartitionedQueue
createPartitionedQueueEncoder =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #partitionInterval >$< E.param (E.nonNullable E.text))
    <> (view #retentionInterval >$< E.param (E.nonNullable E.text))

-- | Encoder for ReadGrouped (pgmq 1.8.0+)
-- Used for read_grouped and read_grouped_rr
readGroupedEncoder :: E.Params ReadGrouped
readGroupedEncoder =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #visibilityTimeout >$< E.param (E.nonNullable E.int4))
    <> (view #qty >$< E.param (E.nonNullable E.int4))

-- | Encoder for ReadGroupedWithPoll (pgmq 1.8.0+)
-- Used for read_grouped_with_poll and read_grouped_rr_with_poll
readGroupedWithPollEncoder :: E.Params ReadGroupedWithPoll
readGroupedWithPollEncoder =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #visibilityTimeout >$< E.param (E.nonNullable E.int4))
    <> (view #qty >$< E.param (E.nonNullable E.int4))
    <> (view #maxPollSeconds >$< E.param (E.nonNullable E.int4))
    <> (view #pollIntervalMs >$< E.param (E.nonNullable E.int4))

-- Topic encoders (pgmq 1.11.0+)

routingKeyValue :: E.Value RoutingKey
routingKeyValue = routingKeyToText >$< E.text

topicPatternValue :: E.Value TopicPattern
topicPatternValue = topicPatternToText >$< E.text

bindTopicEncoder :: E.Params BindTopic
bindTopicEncoder =
  (view #topicPattern >$< E.param (E.nonNullable topicPatternValue))
    <> (view #queueName >$< E.param (E.nonNullable queueNameValue))

unbindTopicEncoder :: E.Params UnbindTopic
unbindTopicEncoder =
  (view #topicPattern >$< E.param (E.nonNullable topicPatternValue))
    <> (view #queueName >$< E.param (E.nonNullable queueNameValue))

-- | Encoder for SendTopic (pgmq 1.11.0+)
-- SQL: pgmq.send_topic(routing_key, msg, coalesce(delay, 0))
sendTopicEncoder :: E.Params SendTopic
sendTopicEncoder =
  (view #routingKey >$< E.param (E.nonNullable routingKeyValue))
    <> (view #messageBody >$< E.param (E.nonNullable messageBodyValue))
    <> (view #delay >$< E.param (E.nullable E.int4))

-- | Encoder for SendTopicWithHeaders (pgmq 1.11.0+)
-- SQL: pgmq.send_topic(routing_key, msg, headers, coalesce(delay, 0))
sendTopicWithHeadersEncoder :: E.Params SendTopicWithHeaders
sendTopicWithHeadersEncoder =
  (view #routingKey >$< E.param (E.nonNullable routingKeyValue))
    <> (view #messageBody >$< E.param (E.nonNullable messageBodyValue))
    <> (view #messageHeaders >$< E.param (E.nonNullable messageHeadersValue))
    <> (view #delay >$< E.param (E.nullable E.int4))

-- | Encoder for BatchSendTopic (pgmq 1.11.0+)
-- SQL: pgmq.send_batch_topic(routing_key, msgs[], coalesce(delay, 0))
batchSendTopicEncoder :: E.Params BatchSendTopic
batchSendTopicEncoder =
  (view #routingKey >$< E.param (E.nonNullable routingKeyValue))
    <> (view #messageBodies >$< E.param (E.nonNullable (E.array (E.dimension foldl' (E.element (E.nonNullable messageBodyValue))))))
    <> (view #delay >$< E.param (E.nullable E.int4))

-- | Encoder for BatchSendTopicForLater (pgmq 1.11.0+)
-- SQL: pgmq.send_batch_topic(routing_key, msgs[], timestamp)
batchSendTopicForLaterEncoder :: E.Params BatchSendTopicForLater
batchSendTopicForLaterEncoder =
  (view #routingKey >$< E.param (E.nonNullable routingKeyValue))
    <> (view #messageBodies >$< E.param (E.nonNullable (E.array (E.dimension foldl' (E.element (E.nonNullable messageBodyValue))))))
    <> (view #scheduledAt >$< E.param (E.nonNullable E.timestamptz))

-- | Encoder for BatchSendTopicWithHeaders (pgmq 1.11.0+)
-- SQL: pgmq.send_batch_topic(routing_key, msgs[], headers[], coalesce(delay, 0))
batchSendTopicWithHeadersEncoder :: E.Params BatchSendTopicWithHeaders
batchSendTopicWithHeadersEncoder =
  (view #routingKey >$< E.param (E.nonNullable routingKeyValue))
    <> (view #messageBodies >$< E.param (E.nonNullable (E.array (E.dimension foldl' (E.element (E.nonNullable messageBodyValue))))))
    <> (view #messageHeaders >$< E.param (E.nonNullable (E.array (E.dimension foldl' (E.element (E.nonNullable messageHeadersValue))))))
    <> (view #delay >$< E.param (E.nullable E.int4))

-- | Encoder for BatchSendTopicWithHeadersForLater (pgmq 1.11.0+)
-- SQL: pgmq.send_batch_topic(routing_key, msgs[], headers[], timestamp)
batchSendTopicWithHeadersForLaterEncoder :: E.Params BatchSendTopicWithHeadersForLater
batchSendTopicWithHeadersForLaterEncoder =
  (view #routingKey >$< E.param (E.nonNullable routingKeyValue))
    <> (view #messageBodies >$< E.param (E.nonNullable (E.array (E.dimension foldl' (E.element (E.nonNullable messageBodyValue))))))
    <> (view #messageHeaders >$< E.param (E.nonNullable (E.array (E.dimension foldl' (E.element (E.nonNullable messageHeadersValue))))))
    <> (view #scheduledAt >$< E.param (E.nonNullable E.timestamptz))

-- | Encoder for UpdateNotifyInsert (pgmq 1.11.0+)
-- SQL: pgmq.update_notify_insert(queue_name, throttle_interval_ms)
updateNotifyInsertEncoder :: E.Params UpdateNotifyInsert
updateNotifyInsertEncoder =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #throttleIntervalMs >$< E.param (E.nonNullable E.int4))
