module Pgmq.Hasql.Statements.QueueManagement
  ( createQueue,
    dropQueue,
    createPartitionedQueue,
    createPartitionedQueueWithPremake,
    createUnloggedQueue,
    detachArchive,
    enableNotifyInsert,
    disableNotifyInsert,
    -- FIFO index functions (pgmq 1.8.0+)
    createFifoIndex,
    createFifoIndexesAll,
    -- Notification management (pgmq 1.11.0+)
    listNotifyInsertThrottles,
    updateNotifyInsert,
  )
where

import Data.Int (Int32)
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Statement (Statement, preparable)
import Pgmq.Hasql.Decoders (notifyInsertThrottleDecoder)
import Pgmq.Hasql.Encoders (createPartitionedQueueEncoder, createPartitionedQueueWithPremakeEncoder, enableNotifyInsertEncoder, queueNameEncoder, updateNotifyInsertEncoder)
import Pgmq.Hasql.Statements.Types (CreatePartitionedQueue, EnableNotifyInsert, UpdateNotifyInsert)
import Pgmq.Types (NotifyInsertThrottle, QueueName)

-- https://pgmq.github.io/pgmq/api/sql/functions/#create
createQueue :: Statement QueueName ()
createQueue = preparable sql queueNameEncoder D.noResult
  where
    sql = "select from pgmq.create($1)"

-- https://pgmq.github.io/pgmq/api/sql/functions/#drop_queue
dropQueue :: Statement QueueName Bool
dropQueue = preparable sql queueNameEncoder decoder
  where
    sql = "select * from pgmq.drop_queue($1)"
    decoder = D.singleRow (D.column (D.nonNullable D.bool))

-- | https://pgmq.github.io/pgmq/api/sql/functions/#create_partitioned
createPartitionedQueue :: Statement CreatePartitionedQueue ()
createPartitionedQueue = preparable sql createPartitionedQueueEncoder D.noResult
  where
    sql = "select from pgmq.create_partitioned($1,$2,$3)"

-- | Create with an explicit premake count (pgmq 1.13.0+).
-- Counts below one are server errors; no fallback is attempted on older servers.
createPartitionedQueueWithPremake :: Statement (CreatePartitionedQueue, Int32) ()
createPartitionedQueueWithPremake = preparable sql createPartitionedQueueWithPremakeEncoder D.noResult
  where
    sql = "select from pgmq.create_partitioned($1,$2,$3,$4)"

-- | https://pgmq.github.io/pgmq/api/sql/functions/#create_unlogged
createUnloggedQueue :: Statement QueueName ()
createUnloggedQueue = preparable sql queueNameEncoder D.noResult
  where
    sql = "select from pgmq.create_unlogged($1)"

-- | DEPRECATED: detach_archive is a no-op in pgmq and will be removed in pgmq 2.0
-- https://pgmq.github.io/pgmq/api/sql/functions/#detach_archive
detachArchive :: Statement QueueName ()
detachArchive = preparable sql queueNameEncoder D.noResult
  where
    sql = "select from pgmq.detach_archive($1)"

-- | Enable insert notifications for a queue (pgmq 1.7.0+)
--
-- Notifications are sent via PostgreSQL LISTEN\/NOTIFY on the channel computed by
-- 'Pgmq.Types.notifyChannelName' — @pgmq.q_\<lowercased queue name\>.INSERT@. The
-- name contains dots, so LISTEN requires it double-quoted. Use the helper rather
-- than assembling the name by hand.
--
-- NOTIFY is fire-and-forget: notifications are not queued for disconnected
-- listeners, and a configured throttle interval suppresses them by design. Every
-- consumer needs a poll fallback in addition to LISTEN.
enableNotifyInsert :: Statement EnableNotifyInsert ()
enableNotifyInsert = preparable sql enableNotifyInsertEncoder D.noResult
  where
    -- The coalesce makes "Nothing = 250ms" true. throttle_interval_ms is NOT NULL
    -- with a column DEFAULT, but a column DEFAULT does not apply to an explicitly
    -- supplied NULL, so a bound SQL NULL raised SQLSTATE 23502 on every call.
    sql = "select from pgmq.enable_notify_insert($1, coalesce($2, 250))"

-- | Disable insert notifications for a queue
disableNotifyInsert :: Statement QueueName ()
disableNotifyInsert = preparable sql queueNameEncoder D.noResult
  where
    sql = "select from pgmq.disable_notify_insert($1)"

-- | Create FIFO index for a queue (pgmq 1.8.0+)
-- Improves performance for FIFO read operations.
createFifoIndex :: Statement QueueName ()
createFifoIndex = preparable sql queueNameEncoder D.noResult
  where
    sql = "select from pgmq.create_fifo_index($1)"

-- | Create FIFO indexes for all queues (pgmq 1.8.0+)
createFifoIndexesAll :: Statement () ()
createFifoIndexesAll = preparable sql E.noParams D.noResult
  where
    sql = "select from pgmq.create_fifo_indexes_all()"

-- | List all notification insert throttle settings (pgmq 1.11.0+)
listNotifyInsertThrottles :: Statement () [NotifyInsertThrottle]
listNotifyInsertThrottles = preparable sql E.noParams decoder
  where
    sql = "select * from pgmq.list_notify_insert_throttles()"
    decoder = D.rowList notifyInsertThrottleDecoder

-- | Update the throttle interval for a queue's insert notifications (pgmq 1.11.0+)
updateNotifyInsert :: Statement UpdateNotifyInsert ()
updateNotifyInsert = preparable sql updateNotifyInsertEncoder D.noResult
  where
    sql = "select from pgmq.update_notify_insert($1, $2)"
