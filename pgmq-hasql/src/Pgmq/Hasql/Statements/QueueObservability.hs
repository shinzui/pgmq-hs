module Pgmq.Hasql.Statements.QueueObservability
  ( listQueues,
    queueMetrics,
    allQueueMetrics,
  )
where

import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Statement (Statement, preparable)
import Pgmq.Hasql.Decoders (queueDecoder, queueMetricsDecoder)
import Pgmq.Hasql.Encoders (queueNameEncoder)
import Pgmq.Hasql.Statements.Types (QueueMetrics)
import Pgmq.Types (Queue, QueueName)

-- | List all queues that currently exist
-- | https://tembo.io/pgmq/api/sql/functions/#list_queues
listQueues :: Statement () [Queue]
listQueues = preparable sql E.noParams decoder
  where
    sql = "select * from pgmq.list_queues()"
    decoder = D.rowList queueDecoder

-- | https://tembo.io/pgmq/api/sql/functions/#metrics
queueMetrics :: Statement QueueName QueueMetrics
queueMetrics = preparable sql queueNameEncoder decoder
  where
    sql = "select * from pgmq.metrics($1)"
    decoder = D.singleRow queueMetricsDecoder

-- | https://tembo.io/pgmq/api/sql/functions/#metrics_all
allQueueMetrics :: Statement () [QueueMetrics]
allQueueMetrics = preparable sql E.noParams decoder
  where
    sql = "select * from pgmq.metrics_all()"
    decoder = D.rowList queueMetricsDecoder
