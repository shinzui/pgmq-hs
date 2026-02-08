# pgmq-hs OpenTelemetry Instrumentation Plan

This document outlines the plan for adding OpenTelemetry instrumentation to pgmq-hs.

## Goal

Enable distributed tracing for PGMQ operations with:
- Producer spans for message send operations
- Consumer spans for message read operations
- W3C Trace Context propagation via message headers
- Database operation instrumentation

## Current State

- No OpenTelemetry instrumentation
- Message headers support exists (pgmq 1.5.0+) - ideal for trace context propagation
- Uses effectful for effect system
- Three-layer architecture: Statements → Sessions → Effectful Interpreter

---

## Implementation Plan

### 1. Add OpenTelemetry Dependencies

**File: `pgmq-effectful/pgmq-effectful.cabal`**

```cabal
build-depends:
  -- existing deps...
  , hs-opentelemetry-api ^>=0.3
  , hs-opentelemetry-propagator-w3c ^>=0.1
```

### 2. Create Telemetry Module

**New file: `pgmq-effectful/src/Pgmq/Effectful/Telemetry.hs`**

```haskell
module Pgmq.Effectful.Telemetry
  ( -- * Trace Context Operations
    injectTraceContext
  , extractTraceContext
  , mergeTraceHeaders
  , TraceHeaders

    -- * Semantic Conventions
  , attrMessagingSystem
  , attrMessagingOperationType
  , attrMessagingDestinationName
  , attrMessagingMessageId
  , attrMessagingBatchMessageCount
  , attrDbSystem
  , attrDbOperationName
  ) where

import Data.Aeson (Value(..), object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.Text (Text)
import OpenTelemetry.Context qualified as OTel
import OpenTelemetry.Propagator.W3CTraceContext qualified as W3C
import OpenTelemetry.Trace qualified as OTel

-- | Headers for W3C Trace Context propagation
type TraceHeaders = [(ByteString, ByteString)]

-- | Inject trace context into message headers (for producers)
-- Creates traceparent and tracestate headers from current span
injectTraceContext :: OTel.Span -> IO TraceHeaders
injectTraceContext span = do
  let propagator = W3C.w3cTraceContextPropagator
  ctx <- OTel.getContext
  let ctxWithSpan = OTel.insertSpan span ctx
  pure $ W3C.inject propagator ctxWithSpan

-- | Extract trace context from message headers (for consumers)
-- Returns SpanContext that can be used as parent
extractTraceContext :: TraceHeaders -> Maybe OTel.SpanContext
extractTraceContext headers = do
  let propagator = W3C.w3cTraceContextPropagator
  let ctx = W3C.extract propagator headers OTel.empty
  span <- OTel.lookupSpan ctx
  pure $ OTel.spanContext span

-- | Merge trace headers into existing PGMQ message headers
mergeTraceHeaders :: TraceHeaders -> Maybe Value -> Value
mergeTraceHeaders traceHeaders existingHeaders =
  let traceObj = object
        [ "traceparent" .= lookup "traceparent" traceHeaders
        , "tracestate" .= lookup "tracestate" traceHeaders
        ]
  in case existingHeaders of
    Just (Object obj) -> Object $ KM.union (toKeyMap traceObj) obj
    Just v -> object ["_original" .= v, "_trace" .= traceObj]
    Nothing -> traceObj
  where
    toKeyMap (Object o) = o
    toKeyMap _ = KM.empty

-- OpenTelemetry Semantic Conventions for Messaging
attrMessagingSystem :: Text
attrMessagingSystem = "messaging.system"

attrMessagingOperationType :: Text
attrMessagingOperationType = "messaging.operation.type"

attrMessagingDestinationName :: Text
attrMessagingDestinationName = "messaging.destination.name"

attrMessagingMessageId :: Text
attrMessagingMessageId = "messaging.message.id"

attrMessagingBatchMessageCount :: Text
attrMessagingBatchMessageCount = "messaging.batch.message_count"

attrDbSystem :: Text
attrDbSystem = "db.system"

attrDbOperationName :: Text
attrDbOperationName = "db.operation.name"
```

### 3. Create Instrumented Interpreter

**New file: `pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs`**

```haskell
module Pgmq.Effectful.Interpreter.Traced
  ( runPgmqTraced
  , runPgmqTracedWith
  , TracingConfig(..)
  , defaultTracingConfig
  ) where

import Effectful
import Effectful.Error.Static
import Hasql.Pool (Pool)
import OpenTelemetry.Trace qualified as OTel
import Pgmq.Effectful.Effect (Pgmq(..))
import Pgmq.Effectful.Telemetry

data TracingConfig = TracingConfig
  { tracer :: !OTel.Tracer
  , recordExceptions :: !Bool
  , includeMessageBodies :: !Bool  -- Careful: may contain PII
  }

defaultTracingConfig :: OTel.Tracer -> TracingConfig
defaultTracingConfig tracer = TracingConfig
  { tracer = tracer
  , recordExceptions = True
  , includeMessageBodies = False
  }

-- | Run Pgmq effect with OpenTelemetry instrumentation
runPgmqTraced ::
  (IOE :> es, Error PgmqError :> es) =>
  Pool ->
  OTel.Tracer ->
  Eff (Pgmq : es) a ->
  Eff es a
runPgmqTraced pool tracer = runPgmqTracedWith pool (defaultTracingConfig tracer)

-- | Run Pgmq effect with custom tracing configuration
runPgmqTracedWith ::
  (IOE :> es, Error PgmqError :> es) =>
  Pool ->
  TracingConfig ->
  Eff (Pgmq : es) a ->
  Eff es a
runPgmqTracedWith pool config = interpret $ \_ -> \case
  -- Queue Management (Producer spans)
  CreateQueue queueName ->
    withSpan config "pgmq.create_queue" OTel.Producer $ \span -> do
      addQueueAttributes span queueName
      runSession pool $ Sessions.createQueue queueName

  -- Message Send Operations (Producer spans)
  SendMessage msg ->
    withSpan config "pgmq.send" OTel.Producer $ \span -> do
      addQueueAttributes span msg.queueName
      result <- runSession pool $ Sessions.sendMessage msg
      OTel.addAttribute span attrMessagingMessageId (show result)
      pure result

  SendMessageWithHeaders msg ->
    withSpan config "pgmq.send" OTel.Producer $ \span -> do
      addQueueAttributes span msg.queueName
      result <- runSession pool $ Sessions.sendMessageWithHeaders msg
      OTel.addAttribute span attrMessagingMessageId (show result)
      pure result

  BatchSendMessage msg ->
    withSpan config "pgmq.send_batch" OTel.Producer $ \span -> do
      addQueueAttributes span msg.queueName
      addAttribute span attrMessagingBatchMessageCount (length msg.messages)
      runSession pool $ Sessions.batchSendMessage msg

  -- Message Read Operations (Consumer spans)
  ReadMessage msg ->
    withSpan config "pgmq.read" OTel.Consumer $ \span -> do
      addQueueAttributes span msg.queueName
      addAttribute span "pgmq.batch_size" msg.batchSize
      addAttribute span "pgmq.visibility_timeout" msg.visibilityTimeout
      result <- runSession pool $ Sessions.readMessage msg
      addAttribute span attrMessagingBatchMessageCount (length result)
      pure result

  ReadWithPoll msg ->
    withSpan config "pgmq.read_poll" OTel.Consumer $ \span -> do
      addQueueAttributes span msg.queueName
      addAttribute span "pgmq.poll_timeout" msg.pollTimeout
      result <- runSession pool $ Sessions.readWithPoll msg
      addAttribute span attrMessagingBatchMessageCount (length result)
      pure result

  -- Message Lifecycle Operations (Internal spans)
  DeleteMessage msg ->
    withSpan config "pgmq.delete" OTel.Internal $ \span -> do
      addQueueAttributes span msg.queueName
      addAttribute span attrMessagingMessageId (show msg.messageId)
      runSession pool $ Sessions.deleteMessage msg

  ArchiveMessage msg ->
    withSpan config "pgmq.archive" OTel.Internal $ \span -> do
      addQueueAttributes span msg.queueName
      addAttribute span attrMessagingMessageId (show msg.messageId)
      runSession pool $ Sessions.archiveMessage msg

  ChangeVisibilityTimeout msg ->
    withSpan config "pgmq.change_vt" OTel.Internal $ \span -> do
      addQueueAttributes span msg.queueName
      addAttribute span attrMessagingMessageId (show msg.messageId)
      addAttribute span "pgmq.visibility_timeout" msg.visibilityTimeout
      runSession pool $ Sessions.changeVisibilityTimeout msg

  -- Metrics Operations
  QueueMetrics queueName ->
    withSpan config "pgmq.metrics" OTel.Internal $ \span -> do
      addQueueAttributes span queueName
      runSession pool $ Sessions.queueMetrics queueName

  -- ... other operations follow same pattern

-- Helper to add common queue attributes
addQueueAttributes :: OTel.Span -> QueueName -> IO ()
addQueueAttributes span (QueueName name) = do
  OTel.addAttribute span attrMessagingSystem ("pgmq" :: Text)
  OTel.addAttribute span attrDbSystem ("postgresql" :: Text)
  OTel.addAttribute span attrMessagingDestinationName name

-- Wrapper for creating spans with error handling
withSpan ::
  (IOE :> es, Error PgmqError :> es) =>
  TracingConfig ->
  Text ->
  OTel.SpanKind ->
  (OTel.Span -> Eff es a) ->
  Eff es a
withSpan config spanName kind action = do
  liftIO $ OTel.inSpan' config.tracer spanName (OTel.defaultSpanArguments { OTel.kind = kind }) $ \span -> do
    result <- try $ runEff $ action span
    case result of
      Left err -> do
        when config.recordExceptions $ OTel.recordException span [] Nothing err
        OTel.setStatus span (OTel.Error $ show err)
        throwError err
      Right val -> do
        OTel.setStatus span OTel.Ok
        pure val
```

### 4. Add Traced Send with Context Injection

**New file: `pgmq-effectful/src/Pgmq/Effectful/Traced.hs`**

```haskell
module Pgmq.Effectful.Traced
  ( -- * Traced Send Operations
    sendMessageTraced
  , sendMessageWithHeadersTraced
  , batchSendMessageTraced

    -- * Context-aware Read Operations
  , readMessageWithContext
  ) where

import Data.Aeson (Value)
import Effectful
import OpenTelemetry.Trace qualified as OTel
import Pgmq.Effectful.Effect
import Pgmq.Effectful.Telemetry
import Pgmq.Types

-- | Send a message with trace context automatically injected into headers
-- This creates a Producer span and injects traceparent/tracestate headers
sendMessageTraced ::
  (Pgmq :> es, IOE :> es) =>
  OTel.Tracer ->
  QueueName ->
  Value ->         -- Message body
  Maybe Value ->   -- Existing headers (will be merged)
  Eff es MessageId
sendMessageTraced tracer queueName body existingHeaders = do
  OTel.inSpan tracer "pgmq.send" OTel.defaultSpanArguments { OTel.kind = OTel.Producer } $ do
    span <- OTel.getCurrentSpan
    traceHeaders <- liftIO $ injectTraceContext span
    let mergedHeaders = mergeTraceHeaders traceHeaders existingHeaders
    sendMessageWithHeaders $ SendMessageWithHeaders
      { queueName = queueName
      , body = MessageBody body
      , headers = MessageHeaders mergedHeaders
      , delay = 0
      }

-- | Read messages and extract trace context from headers
-- Returns messages paired with their extracted SpanContext (if present)
readMessageWithContext ::
  (Pgmq :> es) =>
  ReadMessage ->
  Eff es (Vector (Message, Maybe OTel.SpanContext))
readMessageWithContext readMsg = do
  messages <- readMessage readMsg
  pure $ fmap extractContext messages
  where
    extractContext msg =
      let ctx = extractTraceContextFromMessage msg
      in (msg, ctx)

    extractTraceContextFromMessage :: Message -> Maybe OTel.SpanContext
    extractTraceContextFromMessage msg = do
      headers <- msg.headers
      traceHeaders <- parseTraceHeaders headers
      extractTraceContext traceHeaders

    parseTraceHeaders :: Value -> Maybe TraceHeaders
    parseTraceHeaders (Object obj) = do
      traceparent <- KM.lookup "traceparent" obj >>= asText
      let tracestate = KM.lookup "tracestate" obj >>= asText
      pure $ catMaybes
        [ Just ("traceparent", encodeUtf8 traceparent)
        , ("tracestate",) . encodeUtf8 <$> tracestate
        ]
    parseTraceHeaders _ = Nothing
```

### 5. Export Modules

**Update: `pgmq-effectful/src/Pgmq/Effectful.hs`**

```haskell
module Pgmq.Effectful
  ( -- * Core Effect
    Pgmq
  , PgmqError(..)

    -- * Interpreters
  , runPgmq
  , runPgmqTraced      -- NEW
  , runPgmqTracedWith  -- NEW

    -- * Traced Operations
  , sendMessageTraced  -- NEW
  , readMessageWithContext  -- NEW

    -- * Telemetry Utilities
  , injectTraceContext  -- NEW
  , extractTraceContext -- NEW
  , TraceHeaders        -- NEW

    -- * Re-exports
  , module Pgmq.Types
  ) where
```

### 6. GHC 9.12 Compatibility

**Update: `cabal.project`** (in pgmq-hs repo root)

The hs-opentelemetry packages on Hackage don't yet have version bounds that support GHC 9.12 (base 4.21). However, the packages compile fine with GHC 9.12 when using the main branch from GitHub and relaxing some dependency constraints.

```cabal
-- hs-opentelemetry from GitHub (main branch for GHC 9.12 support)
source-repository-package
  type: git
  location: https://github.com/iand675/hs-opentelemetry
  tag: adc464b0a45e56a983fa1441be6e432b50c29e0e
  subdir: api

source-repository-package
  type: git
  location: https://github.com/iand675/hs-opentelemetry
  tag: adc464b0a45e56a983fa1441be6e432b50c29e0e
  subdir: propagators/w3c

-- Allow newer for proto-lens packages (GHC 9.12 support)
-- Only needed if using hs-opentelemetry-otlp
allow-newer:
  proto-lens:base,
  proto-lens:ghc-prim,
  proto-lens-runtime:base,
  proto-lens-protobuf-types:base,
  proto-lens-protobuf-types:ghc-prim
```

---

## Testing Strategy

### Unit Tests
- Trace context injection produces valid W3C headers
- Trace context extraction parses valid headers correctly
- Invalid/missing headers handled gracefully (returns Nothing)
- Header merging preserves existing headers

### Integration Tests
- Producer span created with correct attributes
- Consumer span created with correct attributes
- Batch operations show correct message counts
- Error spans show exception details

---

## Semantic Conventions

Following [OpenTelemetry Messaging Semantic Conventions](https://opentelemetry.io/docs/specs/semconv/messaging/):

| Attribute | Value | Description |
|-----------|-------|-------------|
| `messaging.system` | `"pgmq"` | The messaging system |
| `messaging.destination.name` | queue name | Target queue |
| `messaging.message.id` | message ID | Unique message identifier |
| `messaging.batch.message_count` | count | Number of messages in batch |
| `messaging.operation.type` | `"send"` / `"receive"` | Operation type |
| `db.system` | `"postgresql"` | Underlying database |

---

## References

- [OpenTelemetry Messaging Semantic Conventions](https://opentelemetry.io/docs/specs/semconv/messaging/)
- [W3C Trace Context Specification](https://www.w3.org/TR/trace-context/)
- [hs-opentelemetry GitHub](https://github.com/iand675/hs-opentelemetry)
- [OpenTelemetry OTLP Exporter Configuration](https://opentelemetry.io/docs/languages/sdk-configuration/otlp-exporter/)
