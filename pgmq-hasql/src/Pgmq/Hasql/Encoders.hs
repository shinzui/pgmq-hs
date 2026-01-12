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
    enableNotifyInsertEncoder,
    readWithPollEncoder,
    createPartitionedQueueEncoder,
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
    queueNameToText,
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

readMessageEncoder :: E.Params ReadMessage
readMessageEncoder =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #delay >$< E.param (E.nonNullable E.int4))
    <> (view #batchSize >$< E.param (E.nullable E.int4))
    <> (view #conditional >$< E.param (E.nullable E.jsonb)) -- pgmq 1.5.0+

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
