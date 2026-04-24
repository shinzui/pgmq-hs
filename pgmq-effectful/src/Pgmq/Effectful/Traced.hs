-- | Traced operations for PGMQ with automatic trace-context propagation.
--
-- Higher-level helpers layered on top of the Pgmq effect:
--
-- * 'sendMessageTraced' writes the current trace context onto the
--   outgoing message headers using the tracer provider's /configured/
--   propagator (W3C Trace Context by default; swap in B3, Datadog, or
--   any other propagator by configuring the provider).
-- * 'readMessageWithContext' extracts the trace context from each
--   received message and returns an 'OTel.Context' suitable for
--   starting a child @process@ span that links back to the producer's
--   trace.
--
-- == Usage
--
-- @
-- -- Producer
-- provider <- OTel.getGlobalTracerProvider
-- sendMessageTraced provider queueName body Nothing
--
-- -- Consumer
-- messagesWithCtx <- readMessageWithContext provider readQuery
-- forM_ messagesWithCtx $ \\(msg, parentCtx) ->
--   -- parentCtx carries the propagated parent context
--   OTel.inSpan tracer \"process\" OTel.defaultSpanArguments
--     (processMessage msg)
-- @
module Pgmq.Effectful.Traced
  ( -- * Traced Send Operations
    sendMessageTraced,

    -- * Context-aware Read Operations
    readMessageWithContext,
    MessageWithContext,
  )
where

import Data.Aeson (Value (..))
import Data.Vector (Vector)
import Data.Vector qualified as V
import Effectful (Eff, IOE, (:>))
import Effectful qualified
import OpenTelemetry.Context qualified as Ctxt
import OpenTelemetry.Context.ThreadLocal qualified as CtxtLocal
import OpenTelemetry.Trace.Core qualified as OTel
import Pgmq.Effectful.Effect (Pgmq, readMessage, sendMessageWithHeaders)
import Pgmq.Effectful.Telemetry
  ( extractTraceContext,
    injectTraceContext,
    jsonToTraceHeaders,
    mergeTraceHeaders,
  )
import Pgmq.Hasql.Statements.Types qualified as Types
import Pgmq.Types

-- | A message paired with the trace context extracted from its
-- headers. Use 'OpenTelemetry.Context.ThreadLocal.attachContext' (or
-- pass it to an @inSpan''@-shaped primitive that lets you set the
-- parent) to start a @process@ span linked to the producer's trace.
type MessageWithContext = (Message, Ctxt.Context)

-- | Send a message with trace context injected into its headers.
--
-- The current 'Ctxt.Context' is fetched via
-- 'OpenTelemetry.Context.ThreadLocal.getContext' and handed to the
-- tracer provider's configured propagator, which writes the propagated
-- fields (traceparent/tracestate for W3C, x-b3-* for B3, etc.) onto
-- the message headers. Any user-supplied headers win against
-- propagator-written ones (the merge is additive).
sendMessageTraced ::
  (Pgmq :> es, IOE :> es) =>
  OTel.TracerProvider ->
  QueueName ->
  MessageBody ->
  Maybe Value ->
  Eff es MessageId
sendMessageTraced provider queueName body existingHeaders = do
  ctx <- Effectful.liftIO CtxtLocal.getContext
  traceHeaders <- injectTraceContext provider ctx
  let mergedHeaders = MessageHeaders $ mergeTraceHeaders traceHeaders existingHeaders
  sendMessageWithHeaders $
    Types.SendMessageWithHeaders
      { queueName = queueName,
        messageBody = body,
        messageHeaders = mergedHeaders,
        delay = Nothing
      }

-- | Read messages and extract trace context from their headers.
--
-- The returned 'Ctxt.Context' for each message is the
-- 'Ctxt.empty' context updated with whatever fields the tracer
-- provider's propagator was able to parse from the headers — so a
-- caller can open a child @process@ span linked to the producer's
-- trace without knowing which propagator is in use.
readMessageWithContext ::
  (Pgmq :> es, IOE :> es) =>
  OTel.TracerProvider ->
  Types.ReadMessage ->
  Eff es (Vector MessageWithContext)
readMessageWithContext provider readQuery = do
  messages <- readMessage readQuery
  Effectful.liftIO $
    V.forM messages $ \msg -> do
      parentCtx <- case msg.headers of
        Just hdrs -> extractTraceContext provider (jsonToTraceHeaders hdrs) Ctxt.empty
        Nothing -> pure Ctxt.empty
      pure (msg, parentCtx)
