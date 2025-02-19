module Pgmq.Db.Statements.QueueManagement
  ( createQueue,
    dropQueue,
    createPartitionedQueue,
    createUnloggedQueue,
    detachArchive,
  )
where

import Hasql.Decoders qualified as D
import Hasql.Statement (Statement (..))
import Pgmq.Db.Encoders (createPartitionedQueueEncoder, queueNameEncoder)
import Pgmq.Db.Statements.Types (CreatePartitionedQueue)
import Pgmq.Types (QueueName)

-- https://tembo.io/pgmq/api/sql/functions/#create
createQueue :: Statement QueueName ()
createQueue = Statement sql queueNameEncoder D.noResult True
  where
    sql = "select from pgmq.create($1)"

-- https://tembo.io/pgmq/api/sql/functions/#drop_queue
dropQueue :: Statement QueueName Bool
dropQueue = Statement sql queueNameEncoder decoder True
  where
    sql = "select * from pgmq.drop_queue($1)"
    decoder = D.singleRow (D.column (D.nonNullable D.bool))

-- | https://tembo.io/pgmq/api/sql/functions/#create_partitioned
createPartitionedQueue :: Statement CreatePartitionedQueue ()
createPartitionedQueue = Statement sql createPartitionedQueueEncoder D.noResult True
  where
    sql = "select from pgmq.create_partitioned($1,$2,$3)"

-- | https://tembo.io/pgmq/api/sql/functions/#create_unlogged
createUnloggedQueue :: Statement QueueName ()
createUnloggedQueue = Statement sql queueNameEncoder D.noResult True
  where
    sql = "select from pgmq.create_unlogged($1)"

-- | https://tembo.io/pgmq/api/sql/functions/#detach_archive
detachArchive :: Statement QueueName ()
detachArchive = Statement sql queueNameEncoder D.noResult True
  where
    sql = "select from pgmq.detach_archive($1)"
