module Pgmq.Effectful.Interpreter
  ( -- * Interpreters
    runPgmq,

    -- * Error Types
    PgmqRuntimeError (..),
    fromUsageError,

    -- * Legacy Error Type (deprecated; remove in a future release)
    PgmqError (..),
  )
where

import Control.Exception (Exception)
import Effectful (Eff, IOE, (:>))
import Effectful qualified
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import GHC.Generics (Generic)
import Hasql.Errors qualified as HasqlErrors
import Hasql.Pool (Pool, UsageError)
import Hasql.Pool qualified as Pool
import Hasql.Session qualified
import Pgmq.Effectful.Effect (Pgmq (..))
import Pgmq.Hasql.Sessions qualified as Sessions

-- | Structured runtime error for pgmq-effectful operations.
--
-- Constructors mirror 'Hasql.Pool.UsageError' so the mapping is direct.
-- The inner 'HasqlErrors.ConnectionError' and 'HasqlErrors.SessionError'
-- types are sum types with detailed constructors; see the Hasql
-- documentation for their full shapes.
data PgmqRuntimeError
  = -- | Timed out waiting for a connection from the pool.
    PgmqAcquisitionTimeout
  | -- | Failed to establish a connection to PostgreSQL.
    PgmqConnectionError HasqlErrors.ConnectionError
  | -- | Error during session execution (SQL statement, decoding, etc.).
    PgmqSessionError HasqlErrors.SessionError
  deriving stock (Show, Eq, Generic)

instance Exception PgmqRuntimeError

-- | Convert hasql-pool's 'UsageError' into the structured
-- 'PgmqRuntimeError'.
fromUsageError :: UsageError -> PgmqRuntimeError
fromUsageError = \case
  Pool.AcquisitionTimeoutUsageError -> PgmqAcquisitionTimeout
  Pool.ConnectionUsageError e -> PgmqConnectionError e
  Pool.SessionUsageError e -> PgmqSessionError e

-- | Legacy error type. Retained for one release cycle to ease migration.
-- Prefer 'PgmqRuntimeError' for new code.
newtype PgmqError = PgmqPoolError UsageError
  deriving stock (Show)

-- | Run the Pgmq effect using a connection pool.
-- Errors are thrown via the 'Error' effect as 'PgmqRuntimeError'.
runPgmq ::
  (IOE :> es, Error PgmqRuntimeError :> es) =>
  Pool ->
  Eff (Pgmq : es) a ->
  Eff es a
runPgmq pool = interpret $ \_ -> \case
  -- Queue Management
  CreateQueue q -> runSession pool $ Sessions.createQueue q
  DropQueue q -> runSession pool $ Sessions.dropQueue q
  CreatePartitionedQueue q -> runSession pool $ Sessions.createPartitionedQueue q
  CreateUnloggedQueue q -> runSession pool $ Sessions.createUnloggedQueue q
  DetachArchive _q -> pure ()
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
  -- Timestamp-based VT (pgmq 1.10.0+)
  SetVisibilityTimeoutAt query -> runSession pool $ Sessions.setVisibilityTimeoutAt query
  BatchSetVisibilityTimeoutAt query -> runSession pool $ Sessions.batchSetVisibilityTimeoutAt query
  ReadWithPoll query -> runSession pool $ Sessions.readWithPoll query
  Pop query -> runSession pool $ Sessions.pop query
  -- FIFO Read (pgmq 1.8.0+)
  ReadGrouped query -> runSession pool $ Sessions.readGrouped query
  ReadGroupedWithPoll query -> runSession pool $ Sessions.readGroupedWithPoll query
  -- Round-robin FIFO Read (pgmq 1.9.0+)
  ReadGroupedRoundRobin query -> runSession pool $ Sessions.readGroupedRoundRobin query
  ReadGroupedRoundRobinWithPoll query -> runSession pool $ Sessions.readGroupedRoundRobinWithPoll query
  -- Topic Management (pgmq 1.11.0+)
  BindTopic params -> runSession pool $ Sessions.bindTopic params
  UnbindTopic params -> runSession pool $ Sessions.unbindTopic params
  ValidateRoutingKey key -> runSession pool $ Sessions.validateRoutingKey key
  ValidateTopicPattern pat -> runSession pool $ Sessions.validateTopicPattern pat
  TestRouting key -> runSession pool $ Sessions.testRouting key
  ListTopicBindings -> runSession pool Sessions.listTopicBindings
  ListTopicBindingsForQueue q -> runSession pool $ Sessions.listTopicBindingsForQueue q
  -- Topic Sending (pgmq 1.11.0+)
  SendTopic msg -> runSession pool $ Sessions.sendTopic msg
  SendTopicWithHeaders msg -> runSession pool $ Sessions.sendTopicWithHeaders msg
  BatchSendTopic msgs -> runSession pool $ Sessions.batchSendTopic msgs
  BatchSendTopicForLater msgs -> runSession pool $ Sessions.batchSendTopicForLater msgs
  BatchSendTopicWithHeaders msgs -> runSession pool $ Sessions.batchSendTopicWithHeaders msgs
  BatchSendTopicWithHeadersForLater msgs -> runSession pool $ Sessions.batchSendTopicWithHeadersForLater msgs
  -- Notification Management (pgmq 1.11.0+)
  ListNotifyInsertThrottles -> runSession pool Sessions.listNotifyInsertThrottles
  UpdateNotifyInsert params -> runSession pool $ Sessions.updateNotifyInsert params
  -- Queue Observability
  ListQueues -> runSession pool Sessions.listQueues
  QueueMetrics q -> runSession pool $ Sessions.queueMetrics q
  AllQueueMetrics -> runSession pool Sessions.allQueueMetrics

-- Internal helper
runSession ::
  (IOE :> es, Error PgmqRuntimeError :> es) =>
  Pool ->
  Hasql.Session.Session a ->
  Eff es a
runSession pool session = do
  result <- Effectful.liftIO $ Pool.use pool session
  case result of
    Left err -> throwError $ fromUsageError err
    Right a -> pure a
