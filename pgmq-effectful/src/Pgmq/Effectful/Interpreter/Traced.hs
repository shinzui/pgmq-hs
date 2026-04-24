-- | OpenTelemetry-instrumented interpreter for the Pgmq effect.
--
-- This module provides a traced version of the Pgmq interpreter that
-- creates OpenTelemetry spans for all PGMQ operations.
--
-- == OpenTelemetry Semantic Conventions
--
-- Spans emitted by this interpreter follow OpenTelemetry
-- [Semantic Conventions v1.24](https://github.com/open-telemetry/semantic-conventions/tree/v1.24.0)
-- for messaging clients and database clients.
--
-- For every operation the interpreter emits:
--
-- * @db.system = "postgresql"@
-- * @db.operation = "pgmq.<fn>"@ (for example @"pgmq.send"@, @"pgmq.read"@,
--   @"pgmq.archive"@).
--
-- For /messaging/ operations (the publish and receive families) the
-- interpreter additionally emits:
--
-- * @messaging.system = "pgmq"@
-- * @messaging.operation@ — one of @"publish"@ (every @send*@ variant,
--   including topic sends) or @"receive"@ (every @read*@ and @pop@
--   variant). The @"process"@ operation is intentionally not emitted
--   here — it belongs to the consumer, which should open its own
--   @process@ span around application-level handling of a received
--   message (see 'Pgmq.Effectful.Traced.readMessageWithContext').
-- * @messaging.destination.name@ — the queue name, or for topic sends
--   the routing key (which is the logical destination for pgmq topics).
--
-- == Span Names
--
-- Span names follow the v1.24 @"<operation> <destination>"@ form:
--
-- * Messaging spans: @"publish my-queue"@, @"receive my-queue"@.
-- * Lifecycle and observability spans: @"pgmq.archive my-queue"@,
--   @"pgmq.set_vt my-queue"@, @"pgmq.list_queues"@ (no destination).
--
-- == Span Kinds
--
-- * 'OTel.Producer': every @send*@ / @send_topic*@ variant.
-- * 'OTel.Consumer': every @read*@ variant and @pop@.
-- * 'OTel.Internal': lifecycle (create, drop, archive, delete, set_vt,
--   purge, bind_topic, …) and observability (metrics, list_queues,
--   list_topic_bindings, validate_*).
--
-- == Usage
--
-- @
-- import OpenTelemetry.Trace qualified as OTel
-- import Pgmq.Effectful.Interpreter.Traced
--
-- main :: IO ()
-- main = do
--   tracerProvider <- OTel.getGlobalTracerProvider
--   let tracer = OTel.makeTracer tracerProvider "my-app" OTel.tracerOptions
--   pool <- ...
--
--   runEff
--     . runError @PgmqRuntimeError
--     . runPgmqTraced pool tracer
--     $ do
--       createQueue "my-queue"
--       sendMessage $ SendMessage "my-queue" (MessageBody "hello") Nothing
-- @
module Pgmq.Effectful.Interpreter.Traced
  ( -- * Interpreters
    runPgmqTraced,
    runPgmqTracedWith,

    -- * Configuration
    TracingConfig (..),
    defaultTracingConfig,
  )
where

import Control.Monad (when)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import Effectful qualified
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Hasql.Pool (Pool, UsageError)
import Hasql.Pool qualified as Pool
import Hasql.Session qualified
import OpenTelemetry.Attributes (Attribute (..), PrimitiveAttribute (..))
import OpenTelemetry.Attributes.Map (AttributeMap, insertByKey)
import OpenTelemetry.Attributes.Map qualified as AttrMap
import OpenTelemetry.Trace.Core qualified as OTel
import Pgmq.Effectful.Effect (Pgmq (..))
import Pgmq.Effectful.Interpreter
  ( PgmqRuntimeError,
    fromUsageError,
  )
import Pgmq.Effectful.Telemetry
import Pgmq.Hasql.Sessions qualified as Sessions
import Pgmq.Hasql.Statements.Types qualified as Types
import Pgmq.Types (MessageId (..), QueueName, RoutingKey, queueNameToText, routingKeyToText)

-- | Configuration for OpenTelemetry tracing.
data TracingConfig = TracingConfig
  { -- | The OpenTelemetry tracer to use
    tracer :: !OTel.Tracer,
    -- | Whether to record exceptions on spans
    recordExceptions :: !Bool,
    -- | Whether to include message bodies in spans (may contain PII)
    includeMessageBodies :: !Bool
  }

-- | Create a default tracing configuration.
defaultTracingConfig :: OTel.Tracer -> TracingConfig
defaultTracingConfig t =
  TracingConfig
    { tracer = t,
      recordExceptions = True,
      includeMessageBodies = False
    }

-- | Describes how a 'Pgmq' operation maps onto OpenTelemetry semantic
-- conventions v1.24.
data OpInfo = OpInfo
  { -- | The pgmq SQL function being invoked (@"pgmq.send"@ etc.).
    opDbFunction :: !Text,
    -- | The v1.24 @messaging.operation@ verb, if any. One of
    -- @"publish"@ or @"receive"@; @Nothing@ for lifecycle/observability
    -- operations.
    opMessagingKind :: !(Maybe Text),
    -- | The span kind.
    opSpanKind :: !OTel.SpanKind,
    -- | The logical destination (queue name, or the routing key for
    -- topic sends). @Nothing@ for operations without a per-queue scope
    -- (for example @list_queues@, @metrics_all@).
    opDestination :: !(Maybe Text),
    -- | A pre-known message id (for per-message operations like delete,
    -- archive, set_vt).
    opMessageId :: !(Maybe MessageId),
    -- | Batch size, for batch operations.
    opBatchCount :: !(Maybe Int)
  }

-- | An 'OpInfo' with no messaging verb, no destination, no message id,
-- and no batch count.
defaultOpInfo :: Text -> OTel.SpanKind -> OpInfo
defaultOpInfo fn kind =
  OpInfo
    { opDbFunction = fn,
      opMessagingKind = Nothing,
      opSpanKind = kind,
      opDestination = Nothing,
      opMessageId = Nothing,
      opBatchCount = Nothing
    }

-- | Run the Pgmq effect with OpenTelemetry instrumentation.
runPgmqTraced ::
  (IOE :> es, Error PgmqRuntimeError :> es) =>
  Pool ->
  OTel.Tracer ->
  Eff (Pgmq : es) a ->
  Eff es a
runPgmqTraced pool t = runPgmqTracedWith pool (defaultTracingConfig t)

-- | Run the Pgmq effect with custom tracing configuration.
runPgmqTracedWith ::
  (IOE :> es, Error PgmqRuntimeError :> es) =>
  Pool ->
  TracingConfig ->
  Eff (Pgmq : es) a ->
  Eff es a
runPgmqTracedWith pool config = interpret $ \_ -> \case
  -- Queue Management (Internal spans)
  CreateQueue q ->
    withTracedOp config pool (queueOp "pgmq.create" OTel.Internal q) $
      Sessions.createQueue q
  DropQueue q ->
    withTracedOp config pool (queueOp "pgmq.drop_queue" OTel.Internal q) $
      Sessions.dropQueue q
  CreatePartitionedQueue pq@(Types.CreatePartitionedQueue qn _ _) ->
    withTracedOp config pool (queueOp "pgmq.create_partitioned" OTel.Internal qn) $
      Sessions.createPartitionedQueue pq
  CreateUnloggedQueue q ->
    withTracedOp config pool (queueOp "pgmq.create_unlogged" OTel.Internal q) $
      Sessions.createUnloggedQueue q
  DetachArchive _q ->
    pure ()
  EnableNotifyInsert cfg@(Types.EnableNotifyInsert qn _) ->
    withTracedOp config pool (queueOp "pgmq.enable_notify_insert" OTel.Internal qn) $
      Sessions.enableNotifyInsert cfg
  DisableNotifyInsert q ->
    withTracedOp config pool (queueOp "pgmq.disable_notify_insert" OTel.Internal q) $
      Sessions.disableNotifyInsert q
  CreateFifoIndex q ->
    withTracedOp config pool (queueOp "pgmq.create_fifo_index" OTel.Internal q) $
      Sessions.createFifoIndex q
  CreateFifoIndexesAll ->
    withTracedOp config pool (defaultOpInfo "pgmq.create_fifo_indexes_all" OTel.Internal) $
      Sessions.createFifoIndexesAll
  -- Message Operations - Send (Producer spans, messaging.operation=publish)
  SendMessage msg@(Types.SendMessage qn _ _) ->
    withTracedOp config pool (publishOp "pgmq.send" qn) $
      Sessions.sendMessage msg
  SendMessageForLater msg@(Types.SendMessageForLater qn _ _) ->
    withTracedOp config pool (publishOp "pgmq.send" qn) $
      Sessions.sendMessageForLater msg
  BatchSendMessage msg@(Types.BatchSendMessage qn bodies _) ->
    withTracedOp config pool (publishBatchOp "pgmq.send_batch" qn (length bodies)) $
      Sessions.batchSendMessage msg
  BatchSendMessageForLater msg@(Types.BatchSendMessageForLater qn bodies _) ->
    withTracedOp config pool (publishBatchOp "pgmq.send_batch" qn (length bodies)) $
      Sessions.batchSendMessageForLater msg
  SendMessageWithHeaders msg@(Types.SendMessageWithHeaders qn _ _ _) ->
    withTracedOp config pool (publishOp "pgmq.send" qn) $
      Sessions.sendMessageWithHeaders msg
  SendMessageWithHeadersForLater msg@(Types.SendMessageWithHeadersForLater qn _ _ _) ->
    withTracedOp config pool (publishOp "pgmq.send" qn) $
      Sessions.sendMessageWithHeadersForLater msg
  BatchSendMessageWithHeaders msg@(Types.BatchSendMessageWithHeaders qn bodies _ _) ->
    withTracedOp config pool (publishBatchOp "pgmq.send_batch" qn (length bodies)) $
      Sessions.batchSendMessageWithHeaders msg
  BatchSendMessageWithHeadersForLater msg@(Types.BatchSendMessageWithHeadersForLater qn bodies _ _) ->
    withTracedOp config pool (publishBatchOp "pgmq.send_batch" qn (length bodies)) $
      Sessions.batchSendMessageWithHeadersForLater msg
  -- Message Operations - Read (Consumer spans, messaging.operation=receive)
  ReadMessage query@(Types.ReadMessage qn _ _ _) ->
    withTracedOp config pool (receiveOp "pgmq.read" qn) $
      Sessions.readMessage query
  ReadWithPoll query@(Types.ReadWithPollMessage qn _ _ _ _ _) ->
    withTracedOp config pool (receiveOp "pgmq.read_with_poll" qn) $
      Sessions.readWithPoll query
  Pop query@(Types.PopMessage qn _) ->
    withTracedOp config pool (receiveOp "pgmq.pop" qn) $
      Sessions.pop query
  -- FIFO Read (Consumer spans)
  ReadGrouped query@(Types.ReadGrouped qn _ _) ->
    withTracedOp config pool (receiveOp "pgmq.read_grouped" qn) $
      Sessions.readGrouped query
  ReadGroupedWithPoll query@(Types.ReadGroupedWithPoll qn _ _ _ _) ->
    withTracedOp config pool (receiveOp "pgmq.read_grouped_with_poll" qn) $
      Sessions.readGroupedWithPoll query
  -- Round-robin FIFO Read (Consumer spans)
  ReadGroupedRoundRobin query@(Types.ReadGrouped qn _ _) ->
    withTracedOp config pool (receiveOp "pgmq.read_grouped_rr" qn) $
      Sessions.readGroupedRoundRobin query
  ReadGroupedRoundRobinWithPoll query@(Types.ReadGroupedWithPoll qn _ _ _ _) ->
    withTracedOp config pool (receiveOp "pgmq.read_grouped_rr_with_poll" qn) $
      Sessions.readGroupedRoundRobinWithPoll query
  -- Message Lifecycle (Internal spans)
  DeleteMessage query@(Types.MessageQuery qn msgId) ->
    withTracedOp config pool ((queueOp "pgmq.delete" OTel.Internal qn) {opMessageId = Just msgId}) $
      Sessions.deleteMessage query
  BatchDeleteMessages query@(Types.BatchMessageQuery qn msgIds) ->
    withTracedOp config pool ((queueOp "pgmq.delete" OTel.Internal qn) {opBatchCount = Just (length msgIds)}) $
      Sessions.batchDeleteMessages query
  ArchiveMessage query@(Types.MessageQuery qn msgId) ->
    withTracedOp config pool ((queueOp "pgmq.archive" OTel.Internal qn) {opMessageId = Just msgId}) $
      Sessions.archiveMessage query
  BatchArchiveMessages query@(Types.BatchMessageQuery qn msgIds) ->
    withTracedOp config pool ((queueOp "pgmq.archive" OTel.Internal qn) {opBatchCount = Just (length msgIds)}) $
      Sessions.batchArchiveMessages query
  DeleteAllMessagesFromQueue q ->
    withTracedOp config pool (queueOp "pgmq.purge_queue" OTel.Internal q) $
      Sessions.deleteAllMessagesFromQueue q
  ChangeVisibilityTimeout query@(Types.VisibilityTimeoutQuery qn msgId _) ->
    withTracedOp config pool ((queueOp "pgmq.set_vt" OTel.Internal qn) {opMessageId = Just msgId}) $
      Sessions.changeVisibilityTimeout query
  BatchChangeVisibilityTimeout query@(Types.BatchVisibilityTimeoutQuery qn msgIds _) ->
    withTracedOp config pool ((queueOp "pgmq.set_vt" OTel.Internal qn) {opBatchCount = Just (length msgIds)}) $
      Sessions.batchChangeVisibilityTimeout query
  -- Timestamp-based VT (pgmq 1.10.0+)
  SetVisibilityTimeoutAt query@(Types.VisibilityTimeoutAtQuery qn msgId _) ->
    withTracedOp config pool ((queueOp "pgmq.set_vt" OTel.Internal qn) {opMessageId = Just msgId}) $
      Sessions.setVisibilityTimeoutAt query
  BatchSetVisibilityTimeoutAt query@(Types.BatchVisibilityTimeoutAtQuery qn msgIds _) ->
    withTracedOp config pool ((queueOp "pgmq.set_vt" OTel.Internal qn) {opBatchCount = Just (length msgIds)}) $
      Sessions.batchSetVisibilityTimeoutAt query
  -- Topic Management (Internal spans, pgmq 1.11.0+)
  BindTopic params@(Types.BindTopic _ qn) ->
    withTracedOp config pool (queueOp "pgmq.bind_topic" OTel.Internal qn) $
      Sessions.bindTopic params
  UnbindTopic params@(Types.UnbindTopic _ qn) ->
    withTracedOp config pool (queueOp "pgmq.unbind_topic" OTel.Internal qn) $
      Sessions.unbindTopic params
  ValidateRoutingKey key ->
    withTracedOp config pool (defaultOpInfo "pgmq.validate_routing_key" OTel.Internal) $
      Sessions.validateRoutingKey key
  ValidateTopicPattern pat ->
    withTracedOp config pool (defaultOpInfo "pgmq.validate_topic_pattern" OTel.Internal) $
      Sessions.validateTopicPattern pat
  TestRouting key ->
    withTracedOp config pool (defaultOpInfo "pgmq.test_routing" OTel.Internal) $
      Sessions.testRouting key
  ListTopicBindings ->
    withTracedOp config pool (defaultOpInfo "pgmq.list_topic_bindings" OTel.Internal) $
      Sessions.listTopicBindings
  ListTopicBindingsForQueue q ->
    withTracedOp config pool (queueOp "pgmq.list_topic_bindings" OTel.Internal q) $
      Sessions.listTopicBindingsForQueue q
  -- Topic Sending (Producer spans, pgmq 1.11.0+)
  SendTopic msg@(Types.SendTopic rk _ _) ->
    withTracedOp config pool (publishTopicOp "pgmq.send_topic" rk) $
      Sessions.sendTopic msg
  SendTopicWithHeaders msg@(Types.SendTopicWithHeaders rk _ _ _) ->
    withTracedOp config pool (publishTopicOp "pgmq.send_topic" rk) $
      Sessions.sendTopicWithHeaders msg
  BatchSendTopic msg@(Types.BatchSendTopic rk bodies _) ->
    withTracedOp config pool (publishTopicBatchOp "pgmq.send_batch_topic" rk (length bodies)) $
      Sessions.batchSendTopic msg
  BatchSendTopicForLater msg@(Types.BatchSendTopicForLater rk bodies _) ->
    withTracedOp config pool (publishTopicBatchOp "pgmq.send_batch_topic" rk (length bodies)) $
      Sessions.batchSendTopicForLater msg
  BatchSendTopicWithHeaders msg@(Types.BatchSendTopicWithHeaders rk bodies _ _) ->
    withTracedOp config pool (publishTopicBatchOp "pgmq.send_batch_topic" rk (length bodies)) $
      Sessions.batchSendTopicWithHeaders msg
  BatchSendTopicWithHeadersForLater msg@(Types.BatchSendTopicWithHeadersForLater rk bodies _ _) ->
    withTracedOp config pool (publishTopicBatchOp "pgmq.send_batch_topic" rk (length bodies)) $
      Sessions.batchSendTopicWithHeadersForLater msg
  -- Notification Management (Internal spans, pgmq 1.11.0+)
  ListNotifyInsertThrottles ->
    withTracedOp config pool (defaultOpInfo "pgmq.list_notify_insert_throttles" OTel.Internal) $
      Sessions.listNotifyInsertThrottles
  UpdateNotifyInsert params@(Types.UpdateNotifyInsert qn _) ->
    withTracedOp config pool (queueOp "pgmq.update_notify_insert" OTel.Internal qn) $
      Sessions.updateNotifyInsert params
  -- Queue Observability (Internal spans)
  ListQueues ->
    withTracedOp config pool (defaultOpInfo "pgmq.list_queues" OTel.Internal) $
      Sessions.listQueues
  QueueMetrics q ->
    withTracedOp config pool (queueOp "pgmq.metrics" OTel.Internal q) $
      Sessions.queueMetrics q
  AllQueueMetrics ->
    withTracedOp config pool (defaultOpInfo "pgmq.metrics_all" OTel.Internal) $
      Sessions.allQueueMetrics

-- ---------------------------------------------------------------------
-- OpInfo constructors
-- ---------------------------------------------------------------------

-- | 'OpInfo' for a non-messaging operation scoped to a single queue.
queueOp :: Text -> OTel.SpanKind -> QueueName -> OpInfo
queueOp fn kind qn = (defaultOpInfo fn kind) {opDestination = Just (queueNameToText qn)}

-- | 'OpInfo' for a @publish@ (send) on a queue.
publishOp :: Text -> QueueName -> OpInfo
publishOp fn qn =
  (queueOp fn OTel.Producer qn) {opMessagingKind = Just "publish"}

-- | 'OpInfo' for a batch @publish@ on a queue.
publishBatchOp :: Text -> QueueName -> Int -> OpInfo
publishBatchOp fn qn n = (publishOp fn qn) {opBatchCount = Just n}

-- | 'OpInfo' for a topic send (publish to a routing key).
publishTopicOp :: Text -> RoutingKey -> OpInfo
publishTopicOp fn rk =
  (defaultOpInfo fn OTel.Producer)
    { opMessagingKind = Just "publish",
      opDestination = Just (routingKeyToText rk)
    }

-- | 'OpInfo' for a batch topic send.
publishTopicBatchOp :: Text -> RoutingKey -> Int -> OpInfo
publishTopicBatchOp fn rk n = (publishTopicOp fn rk) {opBatchCount = Just n}

-- | 'OpInfo' for a @receive@ (read, pop) on a queue.
receiveOp :: Text -> QueueName -> OpInfo
receiveOp fn qn =
  (queueOp fn OTel.Consumer qn) {opMessagingKind = Just "receive"}

-- ---------------------------------------------------------------------
-- Span assembly
-- ---------------------------------------------------------------------

-- | Span name: @"<messaging.operation> <destination>"@ for messaging
-- spans, else @"<db.operation> <destination>"@; if there is no
-- destination, the operation name alone.
spanNameFor :: OpInfo -> Text
spanNameFor info =
  let op = case info.opMessagingKind of
        Just mk -> mk
        Nothing -> info.opDbFunction
   in case info.opDestination of
        Just d -> op <> " " <> d
        Nothing -> op

operationAttributes :: OpInfo -> AttributeMap
operationAttributes info =
  let base =
        insertByKey db_system ("postgresql" :: Text)
          . insertByKey db_operation info.opDbFunction
      withMsgKind = case info.opMessagingKind of
        Just mk ->
          insertByKey messaging_system ("pgmq" :: Text)
            . insertByKey messaging_operation mk
        Nothing -> id
      withDest = case info.opDestination of
        Just d -> insertByKey messaging_destination_name d
        Nothing -> id
      withMsgId = case info.opMessageId of
        Just (MessageId mid) ->
          insertByKey messaging_message_id (T.pack (show mid))
        Nothing -> id
      withCount = case info.opBatchCount of
        Just n -> insertByKey messaging_batch_messageCount (fromIntegral n :: Int64)
        Nothing -> id
   in (withMsgId . withCount . withDest . withMsgKind . base) mempty

withTracedOp ::
  (IOE :> es, Error PgmqRuntimeError :> es) =>
  TracingConfig ->
  Pool ->
  OpInfo ->
  Hasql.Session.Session a ->
  Eff es a
withTracedOp config pool info session = do
  let args =
        OTel.addAttributesToSpanArguments
          (operationAttributes info)
          OTel.defaultSpanArguments {OTel.kind = info.opSpanKind}
  result <-
    Effectful.liftIO $
      OTel.inSpan' config.tracer (spanNameFor info) args $ \s ->
        runSessionIO config s pool session
  throwOnLeft result

-- ---------------------------------------------------------------------
-- Session runner and error recording
-- ---------------------------------------------------------------------

-- | Rethrow a runtime error via the 'Error' effect when present.
throwOnLeft ::
  (Error PgmqRuntimeError :> es) =>
  Either PgmqRuntimeError a ->
  Eff es a
throwOnLeft = \case
  Left err -> throwError err
  Right a -> pure a

runSessionIO ::
  TracingConfig ->
  OTel.Span ->
  Pool ->
  Hasql.Session.Session a ->
  IO (Either PgmqRuntimeError a)
runSessionIO config s pool session = do
  result <- Pool.use pool session
  case result of
    Left err -> do
      when config.recordExceptions $ recordUsageError s err
      OTel.setStatus s (OTel.Error $ T.pack $ show err)
      pure $ Left $ fromUsageError err
    Right a -> do
      OTel.setStatus s OTel.Ok
      pure $ Right a

recordUsageError :: OTel.Span -> UsageError -> IO ()
recordUsageError s err = do
  OTel.addEvent s $
    OTel.NewEvent
      { OTel.newEventName = "exception",
        OTel.newEventTimestamp = Nothing,
        OTel.newEventAttributes =
          AttrMap.fromList
            [ ("exception.type", AttributeValue $ TextAttribute "UsageError"),
              ("exception.message", AttributeValue $ TextAttribute $ T.pack $ show err)
            ]
      }
