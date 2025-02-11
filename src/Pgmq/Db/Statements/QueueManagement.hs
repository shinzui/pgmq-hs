module Pgmq.Db.Statements.QueueManagement (createQueue, dropQueue) where

import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Statement (Statement (..))
import Pgmq.Db.Encoders (queueNameValue)
import Pgmq.Types (QueueName)

queuNameEncoder :: E.Params QueueName
queuNameEncoder = E.param (E.nonNullable queueNameValue)

-- https://tembo.io/pgmq/api/sql/functions/#create
createQueue :: Statement QueueName ()
createQueue = Statement sql queuNameEncoder D.noResult True
  where
    sql = "select from pgmq.create($1)"

-- https://tembo.io/pgmq/api/sql/functions/#drop_queue
dropQueue :: Statement QueueName Bool
dropQueue = Statement sql queuNameEncoder decoder True
  where
    sql = "select * from pgmq.drop_queue($1)"
    decoder = D.singleRow (D.column (D.nonNullable D.bool))
