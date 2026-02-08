-- | Traced operations for PGMQ with automatic trace context propagation.
--
-- This module provides higher-level operations that automatically inject
-- W3C Trace Context into message headers (for producers) and extract
-- trace context from received messages (for consumers).
--
-- == Usage
--
-- @
-- -- Producer: Send with trace context
-- sendMessageTraced tracer queueName body Nothing
--
-- -- Consumer: Read with trace context extraction
-- messagesWithCtx <- readMessageWithContext readQuery
-- forM_ messagesWithCtx $ \(msg, maybeParentCtx) ->
--   -- maybeParentCtx contains the SpanContext from the producer
--   processMessage msg maybeParentCtx
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
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Vector (Vector)
import Data.Vector qualified as V
import Effectful (Eff, IOE, (:>))
import Effectful qualified
import OpenTelemetry.Context qualified as Ctxt
import OpenTelemetry.Context.ThreadLocal qualified as CtxtLocal
import OpenTelemetry.Trace.Core qualified as OTel
import Pgmq.Effectful.Effect (Pgmq, readMessage, sendMessageWithHeaders)
import Pgmq.Effectful.Telemetry
import Pgmq.Hasql.Statements.Types qualified as Types
import Pgmq.Types

-- | A message paired with its extracted trace context (if present).
type MessageWithContext = (Message, Maybe OTel.SpanContext)

-- | Send a message with trace context automatically injected into headers.
--
-- This creates traceparent and tracestate headers from the current span
-- context, which can be extracted by consumers to link traces.
--
-- If existing headers are provided, the trace headers are merged in.
sendMessageTraced ::
  (Pgmq :> es, IOE :> es) =>
  OTel.Tracer ->
  QueueName ->
  MessageBody ->
  Maybe Value ->
  Eff es MessageId
sendMessageTraced _tracer queueName body existingHeaders = do
  -- Get current span context and inject into headers
  ctx <- Effectful.liftIO CtxtLocal.getContext
  traceHeaders <- case Ctxt.lookupSpan ctx of
    Just s -> Effectful.liftIO $ injectTraceContext s
    Nothing -> pure []

  let mergedHeaders = MessageHeaders $ mergeTraceHeaders traceHeaders existingHeaders

  sendMessageWithHeaders $
    Types.SendMessageWithHeaders
      { queueName = queueName,
        messageBody = body,
        messageHeaders = mergedHeaders,
        delay = Nothing
      }

-- | Read messages and extract trace context from headers.
--
-- Returns messages paired with their extracted SpanContext (if present).
-- The SpanContext can be used to create a child span that links to the
-- producer's trace.
--
-- @
-- messagesWithCtx <- readMessageWithContext readQuery
-- forM_ messagesWithCtx $ \(msg, maybeParentCtx) ->
--   inSpan' tracer "process" (linkToParent maybeParentCtx) $ do
--     processMessage msg
-- @
readMessageWithContext ::
  (Pgmq :> es) =>
  Types.ReadMessage ->
  Eff es (Vector MessageWithContext)
readMessageWithContext readQuery = do
  messages <- readMessage readQuery
  pure $ V.map extractContext messages
  where
    extractContext :: Message -> MessageWithContext
    extractContext msg =
      let ctx = extractTraceContextFromMessage msg
       in (msg, ctx)

    extractTraceContextFromMessage :: Message -> Maybe OTel.SpanContext
    extractTraceContextFromMessage msg = do
      hdrs <- msg.headers
      traceHeaders <- parseTraceHeaders hdrs
      extractTraceContext traceHeaders

    parseTraceHeaders :: Value -> Maybe TraceHeaders
    parseTraceHeaders (Object obj) = do
      traceparent <- KM.lookup (Key.fromText "traceparent") obj >>= asText
      let tracestate = KM.lookup (Key.fromText "tracestate") obj >>= asText
      pure $
        catMaybes
          [ Just ("traceparent", TE.encodeUtf8 traceparent),
            ("tracestate",) . TE.encodeUtf8 <$> tracestate
          ]
    parseTraceHeaders _ = Nothing

    asText :: Value -> Maybe Text
    asText (String t) = Just t
    asText _ = Nothing

    catMaybes :: [Maybe a] -> [a]
    catMaybes = foldr (\x acc -> maybe acc (: acc) x) []
