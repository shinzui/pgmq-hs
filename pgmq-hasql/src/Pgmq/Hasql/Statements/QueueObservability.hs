module Pgmq.Hasql.Statements.QueueObservability
  ( listQueues,
    listQueuesUnvalidated,
    listFifoIndexQueueNames,
    queueMetrics,
    allQueueMetrics,
  )
where

import Data.Text (Text)
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Statement (Statement, preparable)
import Pgmq.Hasql.Decoders (queueDecoder, queueMetricsDecoder, unvalidatedQueueDecoder)
import Pgmq.Hasql.Encoders (queueNameEncoder)
import Pgmq.Hasql.Statements.Types (QueueMetrics)
import Pgmq.Types (Queue, QueueName, UnvalidatedQueue)

-- | List all queues that currently exist
-- | https://pgmq.github.io/pgmq/api/sql/functions/#list_queues
listQueues :: Statement () [Queue]
listQueues = preparable sql E.noParams decoder
  where
    sql = "select * from pgmq.list_queues()"
    decoder = D.rowList queueDecoder

-- | Like 'listQueues' but with names left unvalidated, so rows created by
-- other clients with names 'Pgmq.Types.parseQueueName' rejects still decode.
-- | https://pgmq.github.io/pgmq/api/sql/functions/#list_queues
listQueuesUnvalidated :: Statement () [UnvalidatedQueue]
listQueuesUnvalidated = preparable sql E.noParams decoder
  where
    sql = "select * from pgmq.list_queues()"
    decoder = D.rowList unvalidatedQueueDecoder

-- | Queue names (in the lowercased physical form pgmq derives table names from)
-- that already carry the FIFO headers index @q_\<name\>_fifo_idx@.
--
-- Unlike every other statement in this module this reads a PostgreSQL catalog
-- view rather than calling a @pgmq.*@ function, because pgmq exposes no
-- index-existence query: @pgmq.create_fifo_index@ delegates to
-- @CREATE INDEX IF NOT EXISTS@ and reports nothing back. A caller that wants to
-- say truthfully whether it created an index has to look in @pg_indexes@.
listFifoIndexQueueNames :: Statement () [Text]
listFifoIndexQueueNames = preparable sql E.noParams decoder
  where
    sql =
      "select substring(indexname from '^q_(.*)_fifo_idx$')::text \
      \from pg_indexes \
      \where schemaname = 'pgmq' and indexname ~ '^q_.*_fifo_idx$'"
    decoder = D.rowList (D.column (D.nonNullable D.text))

-- | https://pgmq.github.io/pgmq/api/sql/functions/#metrics
queueMetrics :: Statement QueueName QueueMetrics
queueMetrics = preparable sql queueNameEncoder decoder
  where
    sql = "select * from pgmq.metrics($1)"
    decoder = D.singleRow queueMetricsDecoder

-- | https://pgmq.github.io/pgmq/api/sql/functions/#metrics_all
allQueueMetrics :: Statement () [QueueMetrics]
allQueueMetrics = preparable sql E.noParams decoder
  where
    sql = "select * from pgmq.metrics_all()"
    decoder = D.rowList queueMetricsDecoder
