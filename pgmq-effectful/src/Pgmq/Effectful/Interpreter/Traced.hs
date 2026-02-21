-- | OpenTelemetry-instrumented interpreter for the Pgmq effect.
--
-- This module provides a traced version of the Pgmq interpreter that
-- creates OpenTelemetry spans for all PGMQ operations.
--
-- == Span Kinds
--
-- * Producer spans: Queue creation, message send operations
-- * Consumer spans: Message read operations
-- * Internal spans: Message lifecycle (delete, archive, visibility timeout)
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
--     . runError @PgmqError
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
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import Effectful qualified
import Effectful.Dispatch.Dynamic (interpret)
import Hasql.Pool (Pool, UsageError)
import Hasql.Pool qualified as Pool
import Hasql.Session qualified
import OpenTelemetry.Attributes (Attribute (..), PrimitiveAttribute (..))
import OpenTelemetry.Attributes.Map qualified as AttrMap
import OpenTelemetry.Trace.Core qualified as OTel
import Pgmq.Effectful.Effect (Pgmq (..))
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

-- | Run the Pgmq effect with OpenTelemetry instrumentation.
runPgmqTraced ::
  (IOE :> es) =>
  Pool ->
  OTel.Tracer ->
  Eff (Pgmq : es) a ->
  Eff es a
runPgmqTraced pool t = runPgmqTracedWith pool (defaultTracingConfig t)

-- | Run the Pgmq effect with custom tracing configuration.
runPgmqTracedWith ::
  (IOE :> es) =>
  Pool ->
  TracingConfig ->
  Eff (Pgmq : es) a ->
  Eff es a
runPgmqTracedWith pool config = interpret $ \_ -> \case
  -- Queue Management (Producer spans)
  CreateQueue q ->
    withTracedSession config "pgmq create_queue" OTel.Producer q pool $
      Sessions.createQueue q
  DropQueue q ->
    withTracedSession config "pgmq drop_queue" OTel.Producer q pool $
      Sessions.dropQueue q
  CreatePartitionedQueue pq@(Types.CreatePartitionedQueue qn _ _) ->
    withTracedSession config "pgmq create_partitioned_queue" OTel.Producer qn pool $
      Sessions.createPartitionedQueue pq
  CreateUnloggedQueue q ->
    withTracedSession config "pgmq create_unlogged_queue" OTel.Producer q pool $
      Sessions.createUnloggedQueue q
  DetachArchive _q ->
    pure ()
  EnableNotifyInsert cfg@(Types.EnableNotifyInsert qn _) ->
    withTracedSession config "pgmq enable_notify_insert" OTel.Internal qn pool $
      Sessions.enableNotifyInsert cfg
  DisableNotifyInsert q ->
    withTracedSession config "pgmq disable_notify_insert" OTel.Internal q pool $
      Sessions.disableNotifyInsert q
  CreateFifoIndex q ->
    withTracedSession config "pgmq create_fifo_index" OTel.Internal q pool $
      Sessions.createFifoIndex q
  CreateFifoIndexesAll ->
    withTracedSessionNoQueue config "pgmq create_fifo_indexes_all" OTel.Internal pool $
      Sessions.createFifoIndexesAll
  -- Message Operations - Send (Producer spans)
  SendMessage msg@(Types.SendMessage qn _ _) ->
    withTracedSession config "pgmq send" OTel.Producer qn pool $
      Sessions.sendMessage msg
  SendMessageForLater msg@(Types.SendMessageForLater qn _ _) ->
    withTracedSession config "pgmq send" OTel.Producer qn pool $
      Sessions.sendMessageForLater msg
  BatchSendMessage msg@(Types.BatchSendMessage qn bodies _) ->
    withTracedSessionWithCount config "pgmq send_batch" OTel.Producer qn (length bodies) pool $
      Sessions.batchSendMessage msg
  BatchSendMessageForLater msg@(Types.BatchSendMessageForLater qn bodies _) ->
    withTracedSessionWithCount config "pgmq send_batch" OTel.Producer qn (length bodies) pool $
      Sessions.batchSendMessageForLater msg
  SendMessageWithHeaders msg@(Types.SendMessageWithHeaders qn _ _ _) ->
    withTracedSession config "pgmq send" OTel.Producer qn pool $
      Sessions.sendMessageWithHeaders msg
  SendMessageWithHeadersForLater msg@(Types.SendMessageWithHeadersForLater qn _ _ _) ->
    withTracedSession config "pgmq send" OTel.Producer qn pool $
      Sessions.sendMessageWithHeadersForLater msg
  BatchSendMessageWithHeaders msg@(Types.BatchSendMessageWithHeaders qn bodies _ _) ->
    withTracedSessionWithCount config "pgmq send_batch" OTel.Producer qn (length bodies) pool $
      Sessions.batchSendMessageWithHeaders msg
  BatchSendMessageWithHeadersForLater msg@(Types.BatchSendMessageWithHeadersForLater qn bodies _ _) ->
    withTracedSessionWithCount config "pgmq send_batch" OTel.Producer qn (length bodies) pool $
      Sessions.batchSendMessageWithHeadersForLater msg
  -- Message Operations - Read (Consumer spans)
  ReadMessage query@(Types.ReadMessage qn _ _ _) ->
    withTracedSession config "pgmq read" OTel.Consumer qn pool $
      Sessions.readMessage query
  ReadWithPoll query@(Types.ReadWithPollMessage qn _ _ _ _ _) ->
    withTracedSession config "pgmq read_with_poll" OTel.Consumer qn pool $
      Sessions.readWithPoll query
  Pop query@(Types.PopMessage qn _) ->
    withTracedSession config "pgmq pop" OTel.Consumer qn pool $
      Sessions.pop query
  -- FIFO Read (Consumer spans)
  ReadGrouped query@(Types.ReadGrouped qn _ _) ->
    withTracedSession config "pgmq read_grouped" OTel.Consumer qn pool $
      Sessions.readGrouped query
  ReadGroupedWithPoll query@(Types.ReadGroupedWithPoll qn _ _ _ _) ->
    withTracedSession config "pgmq read_grouped_with_poll" OTel.Consumer qn pool $
      Sessions.readGroupedWithPoll query
  -- Round-robin FIFO Read (Consumer spans)
  ReadGroupedRoundRobin query@(Types.ReadGrouped qn _ _) ->
    withTracedSession config "pgmq read_grouped_rr" OTel.Consumer qn pool $
      Sessions.readGroupedRoundRobin query
  ReadGroupedRoundRobinWithPoll query@(Types.ReadGroupedWithPoll qn _ _ _ _) ->
    withTracedSession config "pgmq read_grouped_rr_with_poll" OTel.Consumer qn pool $
      Sessions.readGroupedRoundRobinWithPoll query
  -- Message Lifecycle (Internal spans)
  DeleteMessage query@(Types.MessageQuery qn msgId) ->
    withTracedSessionWithMsgId config "pgmq delete" OTel.Internal qn msgId pool $
      Sessions.deleteMessage query
  BatchDeleteMessages query@(Types.BatchMessageQuery qn msgIds) ->
    withTracedSessionWithCount config "pgmq delete_batch" OTel.Internal qn (length msgIds) pool $
      Sessions.batchDeleteMessages query
  ArchiveMessage query@(Types.MessageQuery qn msgId) ->
    withTracedSessionWithMsgId config "pgmq archive" OTel.Internal qn msgId pool $
      Sessions.archiveMessage query
  BatchArchiveMessages query@(Types.BatchMessageQuery qn msgIds) ->
    withTracedSessionWithCount config "pgmq archive_batch" OTel.Internal qn (length msgIds) pool $
      Sessions.batchArchiveMessages query
  DeleteAllMessagesFromQueue q ->
    withTracedSession config "pgmq purge_queue" OTel.Internal q pool $
      Sessions.deleteAllMessagesFromQueue q
  ChangeVisibilityTimeout query@(Types.VisibilityTimeoutQuery qn msgId _) ->
    withTracedSessionWithMsgId config "pgmq set_vt" OTel.Internal qn msgId pool $
      Sessions.changeVisibilityTimeout query
  BatchChangeVisibilityTimeout query@(Types.BatchVisibilityTimeoutQuery qn msgIds _) ->
    withTracedSessionWithCount config "pgmq set_vt_batch" OTel.Internal qn (length msgIds) pool $
      Sessions.batchChangeVisibilityTimeout query
  -- Timestamp-based VT (pgmq 1.10.0+)
  SetVisibilityTimeoutAt query@(Types.VisibilityTimeoutAtQuery qn msgId _) ->
    withTracedSessionWithMsgId config "pgmq set_vt_at" OTel.Internal qn msgId pool $
      Sessions.setVisibilityTimeoutAt query
  BatchSetVisibilityTimeoutAt query@(Types.BatchVisibilityTimeoutAtQuery qn msgIds _) ->
    withTracedSessionWithCount config "pgmq set_vt_at_batch" OTel.Internal qn (length msgIds) pool $
      Sessions.batchSetVisibilityTimeoutAt query
  -- Topic Management (Internal spans, pgmq 1.11.0+)
  BindTopic params@(Types.BindTopic _ qn) ->
    withTracedSession config "pgmq bind_topic" OTel.Internal qn pool $
      Sessions.bindTopic params
  UnbindTopic params@(Types.UnbindTopic _ qn) ->
    withTracedSession config "pgmq unbind_topic" OTel.Internal qn pool $
      Sessions.unbindTopic params
  ValidateRoutingKey key ->
    withTracedSessionWithRoutingKey config "pgmq validate_routing_key" OTel.Internal key pool $
      Sessions.validateRoutingKey key
  ValidateTopicPattern _pat ->
    withTracedSessionNoQueue config "pgmq validate_topic_pattern" OTel.Internal pool $
      Sessions.validateTopicPattern _pat
  TestRouting key ->
    withTracedSessionWithRoutingKey config "pgmq test_routing" OTel.Internal key pool $
      Sessions.testRouting key
  ListTopicBindings ->
    withTracedSessionNoQueue config "pgmq list_topic_bindings" OTel.Internal pool $
      Sessions.listTopicBindings
  ListTopicBindingsForQueue q ->
    withTracedSession config "pgmq list_topic_bindings" OTel.Internal q pool $
      Sessions.listTopicBindingsForQueue q
  -- Topic Sending (Producer spans, pgmq 1.11.0+)
  SendTopic msg@(Types.SendTopic rk _ _) ->
    withTracedSessionWithRoutingKey config "pgmq send_topic" OTel.Producer rk pool $
      Sessions.sendTopic msg
  SendTopicWithHeaders msg@(Types.SendTopicWithHeaders rk _ _ _) ->
    withTracedSessionWithRoutingKey config "pgmq send_topic" OTel.Producer rk pool $
      Sessions.sendTopicWithHeaders msg
  BatchSendTopic msg@(Types.BatchSendTopic rk bodies _) ->
    withTracedSessionWithRoutingKeyAndCount config "pgmq send_batch_topic" OTel.Producer rk (length bodies) pool $
      Sessions.batchSendTopic msg
  BatchSendTopicForLater msg@(Types.BatchSendTopicForLater rk bodies _) ->
    withTracedSessionWithRoutingKeyAndCount config "pgmq send_batch_topic" OTel.Producer rk (length bodies) pool $
      Sessions.batchSendTopicForLater msg
  BatchSendTopicWithHeaders msg@(Types.BatchSendTopicWithHeaders rk bodies _ _) ->
    withTracedSessionWithRoutingKeyAndCount config "pgmq send_batch_topic" OTel.Producer rk (length bodies) pool $
      Sessions.batchSendTopicWithHeaders msg
  BatchSendTopicWithHeadersForLater msg@(Types.BatchSendTopicWithHeadersForLater rk bodies _ _) ->
    withTracedSessionWithRoutingKeyAndCount config "pgmq send_batch_topic" OTel.Producer rk (length bodies) pool $
      Sessions.batchSendTopicWithHeadersForLater msg
  -- Notification Management (Internal spans, pgmq 1.11.0+)
  ListNotifyInsertThrottles ->
    withTracedSessionNoQueue config "pgmq list_notify_insert_throttles" OTel.Internal pool $
      Sessions.listNotifyInsertThrottles
  UpdateNotifyInsert params@(Types.UpdateNotifyInsert qn _) ->
    withTracedSession config "pgmq update_notify_insert" OTel.Internal qn pool $
      Sessions.updateNotifyInsert params
  -- Queue Observability (Internal spans)
  ListQueues ->
    withTracedSessionNoQueue config "pgmq list_queues" OTel.Internal pool $
      Sessions.listQueues
  QueueMetrics q ->
    withTracedSession config "pgmq metrics" OTel.Internal q pool $
      Sessions.queueMetrics q
  AllQueueMetrics ->
    withTracedSessionNoQueue config "pgmq metrics_all" OTel.Internal pool $
      Sessions.allQueueMetrics

-- Internal helpers

withTracedSession ::
  (IOE :> es) =>
  TracingConfig ->
  Text ->
  OTel.SpanKind ->
  QueueName ->
  Pool ->
  Hasql.Session.Session a ->
  Eff es a
withTracedSession config spanName kind queueName pool session = do
  let args = OTel.defaultSpanArguments {OTel.kind = kind}
  Effectful.liftIO $ OTel.inSpan' config.tracer spanName args $ \s -> do
    addQueueAttributes s queueName
    runSessionIO config s pool session

withTracedSessionNoQueue ::
  (IOE :> es) =>
  TracingConfig ->
  Text ->
  OTel.SpanKind ->
  Pool ->
  Hasql.Session.Session a ->
  Eff es a
withTracedSessionNoQueue config spanName kind pool session = do
  let args = OTel.defaultSpanArguments {OTel.kind = kind}
  Effectful.liftIO $ OTel.inSpan' config.tracer spanName args $ \s -> do
    addBaseAttributes s
    runSessionIO config s pool session

withTracedSessionWithMsgId ::
  (IOE :> es) =>
  TracingConfig ->
  Text ->
  OTel.SpanKind ->
  QueueName ->
  MessageId ->
  Pool ->
  Hasql.Session.Session a ->
  Eff es a
withTracedSessionWithMsgId config spanName kind queueName msgId pool session = do
  let args = OTel.defaultSpanArguments {OTel.kind = kind}
  Effectful.liftIO $ OTel.inSpan' config.tracer spanName args $ \s -> do
    addQueueAttributes s queueName
    addMessageIdAttribute s msgId
    runSessionIO config s pool session

withTracedSessionWithCount ::
  (IOE :> es) =>
  TracingConfig ->
  Text ->
  OTel.SpanKind ->
  QueueName ->
  Int ->
  Pool ->
  Hasql.Session.Session a ->
  Eff es a
withTracedSessionWithCount config spanName kind queueName count pool session = do
  let args = OTel.defaultSpanArguments {OTel.kind = kind}
  Effectful.liftIO $ OTel.inSpan' config.tracer spanName args $ \s -> do
    addQueueAttributes s queueName
    addBatchCountAttribute s count
    runSessionIO config s pool session

runSessionIO ::
  TracingConfig ->
  OTel.Span ->
  Pool ->
  Hasql.Session.Session a ->
  IO a
runSessionIO config s pool session = do
  result <- Pool.use pool session
  case result of
    Left err -> do
      when config.recordExceptions $ recordUsageError s err
      OTel.setStatus s (OTel.Error $ T.pack $ show err)
      fail $ "PgmqPoolError: " <> show err
    Right a -> do
      OTel.setStatus s OTel.Ok
      pure a

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

addQueueAttributes :: OTel.Span -> QueueName -> IO ()
addQueueAttributes s queueName = do
  addBaseAttributes s
  OTel.addAttribute s messagingDestinationName (queueNameToText queueName)

addBaseAttributes :: OTel.Span -> IO ()
addBaseAttributes s = do
  OTel.addAttribute s messagingSystem ("pgmq" :: Text)
  OTel.addAttribute s dbSystem ("postgresql" :: Text)

addMessageIdAttribute :: OTel.Span -> MessageId -> IO ()
addMessageIdAttribute s (MessageId msgId) =
  OTel.addAttribute s messagingMessageId (T.pack $ show msgId)

addBatchCountAttribute :: OTel.Span -> Int -> IO ()
addBatchCountAttribute s count =
  OTel.addAttribute s messagingBatchMessageCount count

addRoutingKeyAttribute :: OTel.Span -> RoutingKey -> IO ()
addRoutingKeyAttribute s rk =
  OTel.addAttribute s messagingRoutingKey (routingKeyToText rk)

withTracedSessionWithRoutingKey ::
  (IOE :> es) =>
  TracingConfig ->
  Text ->
  OTel.SpanKind ->
  RoutingKey ->
  Pool ->
  Hasql.Session.Session a ->
  Eff es a
withTracedSessionWithRoutingKey config spanName kind routingKey pool session = do
  let args = OTel.defaultSpanArguments {OTel.kind = kind}
  Effectful.liftIO $ OTel.inSpan' config.tracer spanName args $ \s -> do
    addBaseAttributes s
    addRoutingKeyAttribute s routingKey
    runSessionIO config s pool session

withTracedSessionWithRoutingKeyAndCount ::
  (IOE :> es) =>
  TracingConfig ->
  Text ->
  OTel.SpanKind ->
  RoutingKey ->
  Int ->
  Pool ->
  Hasql.Session.Session a ->
  Eff es a
withTracedSessionWithRoutingKeyAndCount config spanName kind routingKey count pool session = do
  let args = OTel.defaultSpanArguments {OTel.kind = kind}
  Effectful.liftIO $ OTel.inSpan' config.tracer spanName args $ \s -> do
    addBaseAttributes s
    addRoutingKeyAttribute s routingKey
    addBatchCountAttribute s count
    runSessionIO config s pool session
