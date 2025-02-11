module Pgmq.Db.Encoders (queueNameValue) where

import Hasql.Encoders qualified as E
import Pgmq.Prelude
import Pgmq.Types (Message (..), MessageBody (..), MessageId (..), QueueName, queueNameToText)

queueNameValue :: E.Value QueueName
queueNameValue = queueNameToText >$< E.text
