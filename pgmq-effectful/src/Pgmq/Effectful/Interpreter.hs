module Pgmq.Effectful.Interpreter
  ( -- * Interpreters
    runPgmq,

    -- * Error Types
    PgmqError (..),
  )
where

import Effectful (Eff, IOE, (:>))
import Effectful qualified
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Hasql.Pool (Pool, UsageError)
import Hasql.Pool qualified as Pool
import Hasql.Session qualified
import Pgmq.Effectful.Effect (Pgmq (..))
import Pgmq.Hasql.Sessions qualified as Sessions

-- | Error type for pgmq operations.
newtype PgmqError = PgmqPoolError UsageError
  deriving stock (Show)

-- | Run the Pgmq effect using a connection pool.
-- Errors are thrown via the 'Error' effect.
runPgmq ::
  (IOE :> es, Error PgmqError :> es) =>
  Pool ->
  Eff (Pgmq : es) a ->
  Eff es a
runPgmq pool = interpret $ \_ -> \case
  -- Queue Management
  CreateQueue q -> runSession pool $ Sessions.createQueue q
  DropQueue q -> runSession pool $ Sessions.dropQueue q
  CreatePartitionedQueue q -> runSession pool $ Sessions.createPartitionedQueue q
  CreateUnloggedQueue q -> runSession pool $ Sessions.createUnloggedQueue q
  DetachArchive q -> runSession pool $ Sessions.detachArchive q
  EnableNotifyInsert config -> runSession pool $ Sessions.enableNotifyInsert config
  DisableNotifyInsert q -> runSession pool $ Sessions.disableNotifyInsert q
  CreateFifoIndex q -> runSession pool $ Sessions.createFifoIndex q
  CreateFifoIndexesAll -> runSession pool Sessions.createFifoIndexesAll
  -- Message Operations
  SendMessage msg -> runSession pool $ Sessions.sendMessage msg
  SendMessageForLater msg -> runSession pool $ Sessions.sendMessageForLater msg
  BatchSendMessage msgs -> runSession pool $ Sessions.batchSendMessage msgs
  BatchSendMessageForLater msgs -> runSession pool $ Sessions.batchSendMessageForLater msgs
  SendMessageWithHeaders msg -> runSession pool $ Sessions.sendMessageWithHeaders msg
  SendMessageWithHeadersForLater msg -> runSession pool $ Sessions.sendMessageWithHeadersForLater msg
  BatchSendMessageWithHeaders msgs -> runSession pool $ Sessions.batchSendMessageWithHeaders msgs
  BatchSendMessageWithHeadersForLater msgs -> runSession pool $ Sessions.batchSendMessageWithHeadersForLater msgs
  ReadMessage query -> runSession pool $ Sessions.readMessage query
  DeleteMessage query -> runSession pool $ Sessions.deleteMessage query
  BatchDeleteMessages query -> runSession pool $ Sessions.batchDeleteMessages query
  ArchiveMessage query -> runSession pool $ Sessions.archiveMessage query
  BatchArchiveMessages query -> runSession pool $ Sessions.batchArchiveMessages query
  DeleteAllMessagesFromQueue q -> runSession pool $ Sessions.deleteAllMessagesFromQueue q
  ChangeVisibilityTimeout query -> runSession pool $ Sessions.changeVisibilityTimeout query
  BatchChangeVisibilityTimeout query -> runSession pool $ Sessions.batchChangeVisibilityTimeout query
  ReadWithPoll query -> runSession pool $ Sessions.readWithPoll query
  Pop query -> runSession pool $ Sessions.pop query
  -- FIFO Read (pgmq 1.8.0+)
  ReadGrouped query -> runSession pool $ Sessions.readGrouped query
  ReadGroupedWithPoll query -> runSession pool $ Sessions.readGroupedWithPoll query
  -- Round-robin FIFO Read (pgmq 1.9.0+)
  ReadGroupedRoundRobin query -> runSession pool $ Sessions.readGroupedRoundRobin query
  ReadGroupedRoundRobinWithPoll query -> runSession pool $ Sessions.readGroupedRoundRobinWithPoll query
  -- Queue Observability
  ListQueues -> runSession pool Sessions.listQueues
  QueueMetrics q -> runSession pool $ Sessions.queueMetrics q
  AllQueueMetrics -> runSession pool Sessions.allQueueMetrics

-- Internal helper
runSession ::
  (IOE :> es, Error PgmqError :> es) =>
  Pool ->
  Hasql.Session.Session a ->
  Eff es a
runSession pool session = do
  result <- Effectful.liftIO $ Pool.use pool session
  case result of
    Left err -> throwError $ PgmqPoolError err
    Right a -> pure a
