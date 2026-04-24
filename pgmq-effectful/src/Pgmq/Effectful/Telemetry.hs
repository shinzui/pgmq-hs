-- | OpenTelemetry utilities for PGMQ tracing.
--
-- This module provides:
--
-- * W3C Trace Context propagation via message headers
-- * Re-exports of typed 'AttributeKey' values from
--   "OpenTelemetry.SemanticConventions" for the v1.24 attributes used
--   by the pgmq instrumentation.
--
-- The semantic-conventions names mirror OpenTelemetry specification
-- v1.24 as generated into
-- @hs-opentelemetry-semantic-conventions@ 0.1.0.0. Prefer using the
-- typed keys over raw strings so a generator bump or spec revision can
-- be picked up centrally.
module Pgmq.Effectful.Telemetry
  ( -- * Trace Context Operations
    injectTraceContext,
    extractTraceContext,
    mergeTraceHeaders,
    TraceHeaders,

    -- * Semantic Convention Keys (re-exported from

    -- "OpenTelemetry.SemanticConventions")
    messaging_system,
    messaging_operation,
    messaging_destination_name,
    messaging_message_id,
    messaging_batch_messageCount,
    db_system,
    db_operation,
  )
where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.Text.Encoding qualified as TE
import OpenTelemetry.Propagator.W3CTraceContext qualified as W3C
import OpenTelemetry.SemanticConventions
  ( db_operation,
    db_system,
    messaging_batch_messageCount,
    messaging_destination_name,
    messaging_message_id,
    messaging_operation,
    messaging_system,
  )
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
