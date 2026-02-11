-- | OpenTelemetry utilities for PGMQ tracing.
--
-- This module provides:
--
-- * W3C Trace Context propagation via message headers
-- * OpenTelemetry semantic conventions for messaging systems
module Pgmq.Effectful.Telemetry
  ( -- * Trace Context Operations
    injectTraceContext,
    extractTraceContext,
    mergeTraceHeaders,
    TraceHeaders,

    -- * Semantic Conventions
    messagingSystem,
    messagingOperationType,
    messagingDestinationName,
    messagingMessageId,
    messagingBatchMessageCount,
    dbSystem,
    dbOperationName,
  )
where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import OpenTelemetry.Propagator.W3CTraceContext qualified as W3C
import OpenTelemetry.Trace.Core qualified as OTel

-- | Headers for W3C Trace Context propagation.
-- Contains traceparent and optionally tracestate.
type TraceHeaders = [(ByteString, ByteString)]

-- | Inject trace context into message headers (for producers).
-- Creates traceparent and tracestate headers from current span context.
injectTraceContext :: OTel.Span -> IO TraceHeaders
injectTraceContext otelSpan = do
  (traceparent, tracestate) <- W3C.encodeSpanContext otelSpan
  pure
    [ ("traceparent", traceparent),
      ("tracestate", tracestate)
    ]

-- | Extract trace context from message headers (for consumers).
-- Returns SpanContext that can be used as parent link.
extractTraceContext :: TraceHeaders -> Maybe OTel.SpanContext
extractTraceContext headers = do
  let traceparent = lookup "traceparent" headers
      tracestate = lookup "tracestate" headers
  W3C.decodeSpanContext traceparent tracestate

-- | Merge trace headers into existing PGMQ message headers.
-- Preserves existing headers while adding trace context.
mergeTraceHeaders :: TraceHeaders -> Maybe Value -> Value
mergeTraceHeaders traceHeaders existingHeaders =
  let traceObj =
        Object $
          KM.fromList
            [ (Key.fromText "traceparent", toJsonValue $ lookup "traceparent" traceHeaders),
              (Key.fromText "tracestate", toJsonValue $ lookup "tracestate" traceHeaders)
            ]
   in case existingHeaders of
        Just (Object obj) -> Object $ KM.union (toKeyMap traceObj) obj
        Just v -> Object $ KM.fromList [(Key.fromText "_original", v), (Key.fromText "_trace", traceObj)]
        Nothing -> traceObj
  where
    toJsonValue :: Maybe ByteString -> Value
    toJsonValue = maybe Null (String . TE.decodeUtf8)

    toKeyMap :: Value -> KM.KeyMap Value
    toKeyMap (Object o) = o
    toKeyMap _ = KM.empty

-- OpenTelemetry Semantic Conventions for Messaging
-- See: https://opentelemetry.io/docs/specs/semconv/messaging/

-- | The messaging system identifier.
-- Value: "pgmq"
messagingSystem :: Text
messagingSystem = "messaging.system"

-- | The type of messaging operation.
-- Values: "send", "receive", "process"
messagingOperationType :: Text
messagingOperationType = "messaging.operation.type"

-- | The destination queue name.
messagingDestinationName :: Text
messagingDestinationName = "messaging.destination.name"

-- | The unique message identifier.
messagingMessageId :: Text
messagingMessageId = "messaging.message.id"

-- | Number of messages in a batch operation.
messagingBatchMessageCount :: Text
messagingBatchMessageCount = "messaging.batch.message_count"

-- | The database system.
-- Value: "postgresql"
dbSystem :: Text
dbSystem = "db.system"

-- | The database operation name.
dbOperationName :: Text
dbOperationName = "db.operation.name"
