module Pgmq.Db.Encoders
  ( queueNameValue,
    sendMessageEncoder,
    sendMessageForLaterEncoder,
    batchSendMessageEncoder,
    batchSendMessageForLaterEncoder,
    messageIdValue,
    readMessageEncoder,
    messageQueryEncoder,
    batchMessageQueryEncoder,
    queueNameEncoder,
  )
where

import Data.Foldable (foldl')
import Data.Generics.Product (HasField')
import Hasql.Encoders qualified as E
import Pgmq.Db.Statements.Types
import Pgmq.Prelude
import Pgmq.Types
  ( MessageBody (..),
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

readMessageEncoder :: E.Params ReadMessage
readMessageEncoder =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #delay >$< E.param (E.nonNullable E.int4))
    <> (view #batchSize >$< E.param (E.nullable E.int4))

messageQueryEncoder :: E.Params MessageQuery
messageQueryEncoder =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #messageId >$< E.param (E.nonNullable messageIdValue))

batchMessageQueryEncoder :: E.Params BatchMessageQuery
batchMessageQueryEncoder =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #messageIds >$< E.param (E.nonNullable (E.array (E.dimension foldl' (E.element (E.nonNullable messageIdValue))))))
