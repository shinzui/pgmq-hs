module Pgmq.Hasql.Statements.QueueManagement
  ( createQueue,
    dropQueue,
    createPartitionedQueue,
    createUnloggedQueue,
    detachArchive,
    enableNotifyInsert,
    disableNotifyInsert,
    -- FIFO index functions (pgmq 1.8.0+)
    createFifoIndex,
    createFifoIndexesAll,
  )
where

import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Statement (Statement (..))
import Pgmq.Hasql.Encoders (createPartitionedQueueEncoder, enableNotifyInsertEncoder, queueNameEncoder)
import Pgmq.Hasql.Statements.Types (CreatePartitionedQueue, EnableNotifyInsert)
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

-- | DEPRECATED: detach_archive is a no-op in pgmq and will be removed in pgmq 2.0
-- https://tembo.io/pgmq/api/sql/functions/#detach_archive
detachArchive :: Statement QueueName ()
detachArchive = Statement sql queueNameEncoder D.noResult True
  where
    sql = "select from pgmq.detach_archive($1)"

-- | Enable insert notifications for a queue (pgmq 1.7.0+)
-- Notifications are sent via PostgreSQL LISTEN/NOTIFY to channel pgmq_<queue_name>
enableNotifyInsert :: Statement EnableNotifyInsert ()
enableNotifyInsert = Statement sql enableNotifyInsertEncoder D.noResult True
  where
    sql = "select from pgmq.enable_notify_insert($1, $2)"

-- | Disable insert notifications for a queue
disableNotifyInsert :: Statement QueueName ()
disableNotifyInsert = Statement sql queueNameEncoder D.noResult True
  where
    sql = "select from pgmq.disable_notify_insert($1)"

-- | Create FIFO index for a queue (pgmq 1.8.0+)
-- Improves performance for FIFO read operations.
createFifoIndex :: Statement QueueName ()
createFifoIndex = Statement sql queueNameEncoder D.noResult True
  where
    sql = "select from pgmq.create_fifo_index($1)"

-- | Create FIFO indexes for all queues (pgmq 1.8.0+)
createFifoIndexesAll :: Statement () ()
createFifoIndexesAll = Statement sql E.noParams D.noResult True
  where
    sql = "select from pgmq.create_fifo_indexes_all()"
