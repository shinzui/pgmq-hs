module Pgmq.Db.Transactions (createQueue, dropQueue) where

import Hasql.Session qualified as S
import Hasql.Transaction
import Hasql.Transaction.Sessions
import Pgmq.Db.Statements qualified as Db
import Pgmq.Types (QueueName)

createQueue :: QueueName -> S.Session ()
createQueue q =
  transaction Serializable Write $
    statement q Db.createQueue

dropQueue :: QueueName -> S.Session Bool
dropQueue q =
  transaction Serializable Write $
    statement q Db.dropQueue
