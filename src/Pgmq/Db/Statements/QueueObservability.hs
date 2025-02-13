module Pgmq.Db.Statements.QueueObservability where

import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Statement (Statement (..))
import Pgmq.Db.Decoders (queueDecoder)
import Pgmq.Prelude
import Pgmq.Types (Queue)

-- | List all queues that currently exist
-- | https://tembo.io/pgmq/api/sql/functions/#list_queues
listQueues :: Statement () [Queue]
listQueues = Statement sql E.noParams decoder True
  where
    sql = "select * from pgmq.list_queues()"
    decoder = D.rowList queueDecoder
