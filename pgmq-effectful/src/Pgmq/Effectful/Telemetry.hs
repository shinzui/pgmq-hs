-- | OpenTelemetry utilities for PGMQ tracing.
--
-- This module provides:
--
-- * Trace-context propagation through pgmq message headers, using the
--   tracer provider's /configured/ propagator (W3C Trace Context by
--   default, but any propagator installed on the provider will work —
--   B3, Datadog, …). The pgmq header payload is a JSON object whose
--   keys are header names (lower-cased) and whose values are strings.
-- * Re-exports of typed 'AttributeKey' values from
--   "OpenTelemetry.SemanticConventions" for the attributes used by the
--   pgmq instrumentation.
--
-- The semantic-conventions names mirror OpenTelemetry specification
-- v1.40 as generated into @hs-opentelemetry-semantic-conventions@
-- 1.40.0.0. The traced interpreter uses
-- @OTEL_SEMCONV_STABILITY_OPT_IN@ to choose old, stable, or duplicate
-- database and messaging operation attributes.
module Pgmq.Effectful.Telemetry
  ( -- * Trace Context Propagation
    injectTraceContext,
    extractTraceContext,
    traceHeadersToJson,
    jsonToTraceHeaders,
    mergeTraceHeaders,
    TraceHeaders,

    -- * Semantic Convention Keys (re-exported from

    -- "OpenTelemetry.SemanticConventions")
    messaging_system,
    messaging_operation,
    messaging_destination_name,
    messaging_message_id,
    messaging_batch_messageCount,
    messaging_operation_name,
    messaging_operation_type,
    db_system,
    db_operation,
    db_system_name,
    db_operation_name,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.CaseInsensitive qualified as CI
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Types (RequestHeaders)
import OpenTelemetry.Context qualified as Ctxt
import OpenTelemetry.Propagator
  ( TextMap,
    TextMapPropagator,
    emptyTextMap,
    extract,
    inject,
    textMapFromList,
    textMapToList,
  )
import OpenTelemetry.SemanticConventions
  ( db_operation,
    db_operation_name,
    db_system,
    db_system_name,
    messaging_batch_messageCount,
    messaging_destination_name,
    messaging_message_id,
    messaging_operation,
    messaging_operation_name,
    messaging_operation_type,
    messaging_system,
  )
import OpenTelemetry.Trace.Core
  ( TracerProvider,
    getTracerProviderPropagators,
  )

-- | Carrier for propagated trace context. Same shape as
-- @http-types@' @RequestHeaders@ — @[(HeaderName, ByteString)]@, where
-- @HeaderName@ is case-insensitive — because that is the carrier type
-- used by every propagator in the @hs-opentelemetry@ ecosystem.
type TraceHeaders = RequestHeaders

-- | Inject trace context into carrier headers, using the tracer
-- provider's configured propagator.
--
-- Callers typically pass a 'Ctxt.Context' obtained from
-- 'OpenTelemetry.Context.ThreadLocal.getContext' (with any current
-- span inserted) so the propagator can write the current span's
-- context onto the carrier.
injectTraceContext ::
  (MonadIO m) =>
  TracerProvider ->
  Ctxt.Context ->
  m TraceHeaders
injectTraceContext provider ctxt =
  let propagator = getTracerProviderPropagators provider
   in injectWith propagator ctxt

-- | Extract trace context from carrier headers, using the tracer
-- provider's configured propagator. Returns an updated 'Ctxt.Context'
-- carrying the extracted span context (if any) — suitable for starting
-- a child \"process\" span linked to the producer's trace.
extractTraceContext ::
  (MonadIO m) =>
  TracerProvider ->
  TraceHeaders ->
  Ctxt.Context ->
  m Ctxt.Context
extractTraceContext provider headers ctxt =
  let propagator = getTracerProviderPropagators provider
   in extract propagator (traceHeadersToTextMap headers) ctxt

-- | Propagator-generic inject wrapper (mainly useful for tests that
-- want to pin the propagator explicitly).
injectWith ::
  (MonadIO m) =>
  TextMapPropagator ->
  Ctxt.Context ->
  m TraceHeaders
injectWith propagator ctxt =
  liftIO $ textMapToTraceHeaders <$> inject propagator ctxt emptyTextMap

traceHeadersToTextMap :: TraceHeaders -> TextMap
traceHeadersToTextMap =
  textMapFromList
    . fmap
      ( \(name, value) ->
          ( TE.decodeUtf8 (CI.foldedCase name),
            TE.decodeUtf8 value
          )
      )

textMapToTraceHeaders :: TextMap -> TraceHeaders
textMapToTraceHeaders =
  fmap
    ( \(name, value) ->
        ( CI.mk (TE.encodeUtf8 name),
          TE.encodeUtf8 value
        )
    )
    . textMapToList

-- | Encode carrier headers as a JSON object for storage in pgmq
-- message headers (jsonb). Header names are lower-cased (via
-- case-insensitive folding) so the on-wire representation is stable
-- regardless of the original case the propagator emitted.
traceHeadersToJson :: TraceHeaders -> Value
traceHeadersToJson = Object . traceHeadersKeyMap

-- | Decode a JSON object of trace headers back into the carrier
-- shape. Non-object inputs and non-string values are ignored.
jsonToTraceHeaders :: Value -> TraceHeaders
jsonToTraceHeaders = \case
  Object obj ->
    [ (CI.mk (TE.encodeUtf8 (Key.toText k)), TE.encodeUtf8 v)
    | (k, String v) <- KM.toList obj
    ]
  _ -> []

-- | Merge propagated trace headers into an existing pgmq headers
-- value. Existing keys win — we never overwrite user-supplied headers.
mergeTraceHeaders :: TraceHeaders -> Maybe Value -> Value
mergeTraceHeaders traceHeaders existingHeaders =
  let traceObj = traceHeadersKeyMap traceHeaders
   in case existingHeaders of
        Just (Object obj) -> Object (KM.union obj traceObj)
        Just v ->
          Object
            ( KM.fromList
                [ (Key.fromText (T.pack "_original"), v),
                  (Key.fromText (T.pack "_trace"), Object traceObj)
                ]
            )
        Nothing -> Object traceObj

traceHeadersKeyMap :: TraceHeaders -> KM.KeyMap Value
traceHeadersKeyMap =
  KM.fromList
    . fmap
      ( \(name, value) ->
          ( Key.fromText (TE.decodeUtf8 (CI.foldedCase name)),
            String (TE.decodeUtf8 value)
          )
      )
