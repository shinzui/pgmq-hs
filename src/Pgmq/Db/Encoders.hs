module Pgmq.Db.Encoders
  ( queueNameValue,
    sendMessageEncoder,
    sendMessageForLaterEncoder,
    messageIdValue,
  )
where

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
