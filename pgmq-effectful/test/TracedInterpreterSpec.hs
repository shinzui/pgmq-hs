{-# LANGUAGE OverloadedStrings #-}

module TracedInterpreterSpec (tests) where

import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Exception (bracket)
import Control.Monad (unless)
import Data.HashMap.Strict qualified as HM
import Data.IORef (IORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Effectful (runEff)
import Effectful.Error.Static (runError)
import Hasql.Pool qualified as Pool
import OpenTelemetry.Attributes qualified as Attrs
import OpenTelemetry.Context qualified as Ctxt
import OpenTelemetry.Context.ThreadLocal qualified as CtxtLocal
import OpenTelemetry.Exporter.InMemory.Span qualified as InMemoryExporter
import OpenTelemetry.Propagator.W3CTraceContext qualified as W3C
import OpenTelemetry.Trace.Core qualified as OTel
import OpenTelemetry.Trace.Id.Generator.Default (defaultIdGenerator)
import OpenTelemetry.Util (appendOnlyBoundedCollectionValues)
import Pgmq.Effectful
  ( MessageBody (..),
    MessageId (..),
    MessageQuery (..),
    PgmqRuntimeError (..),
    QueueName,
    ReadMessage (..),
    SendMessage (..),
    createQueue,
    deleteMessage,
    parseQueueName,
    queueNameToText,
    readMessage,
    runPgmqTraced,
    sendMessage,
    sendMessageTraced,
  )
import Pgmq.Effectful.Telemetry (extractTraceContext, jsonToTraceHeaders)
import Pgmq.Types qualified as Pgmq
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: Pool.Pool -> TestTree
tests pool =
  testGroup
    "Traced interpreter"
    [ testGroup
        "Error propagation"
        [ testCase "statement error surfaces PgmqSessionError via Error channel" $ do
            (tracer, _provider, _spansRef) <- setupTracer
            bogus <- mkQueueName "queue_that_does_not_exist_xyz2"
            result <-
              runEff
                . runError @PgmqRuntimeError
                . runPgmqTraced pool tracer
                $ deleteMessage
                  MessageQuery
                    { queueName = bogus,
                      messageId = MessageId 1
                    }
            case result of
              Left (_cs, PgmqSessionError _) -> pure ()
              Left (_cs, other) ->
                assertFailure $
                  "Expected PgmqSessionError, got " <> show other
              Right _ ->
                assertFailure
                  "Expected an error, got success"
        ],
      testGroup
        "semantic conventions"
        [ testCase "publish emits messaging.* + db.* attributes" $
            withSemconvOptIn "" $ do
              (tracer, _provider, spansRef) <- setupTracer
              queue <- mkUniqueQueue "publish_attrs"
              result <-
                runEff . runError @PgmqRuntimeError . runPgmqTraced pool tracer $ do
                  createQueue queue
                  sendMessage
                    SendMessage
                      { queueName = queue,
                        messageBody = MessageBody "hello",
                        delay = Nothing
                      }
              _ <- assertRight result
              spans <- readIORef spansRef
              publishSpans <- spansWithFirstWord "publish" spans
              unless (length publishSpans == 1) $
                spanNames spans >>= \names ->
                  assertFailure $
                    "Expected exactly one publish span, got "
                      <> show names
              s <- singleSpan publishSpans "publish"
              actualSpanName <- spanName s
              assertEqual
                "span name carries destination"
                ("publish " <> queueNameToText queue)
                actualSpanName
              assertSpanKindProducer s
              assertAttrText s "messaging.system" "pgmq"
              assertAttrText s "messaging.operation" "publish"
              assertAttrText s "messaging.destination.name" (queueNameToText queue)
              assertAttrText s "db.system" "postgresql"
              assertAttrText s "db.operation" "pgmq.send",
          testCase "receive emits messaging.* + db.* attributes" $
            withSemconvOptIn "" $ do
              (tracer, _provider, spansRef) <- setupTracer
              queue <- mkUniqueQueue "receive_attrs"
              result <-
                runEff . runError @PgmqRuntimeError . runPgmqTraced pool tracer $ do
                  createQueue queue
                  _ <-
                    sendMessage
                      SendMessage
                        { queueName = queue,
                          messageBody = MessageBody "one",
                          delay = Nothing
                        }
                  readMessage
                    ReadMessage
                      { queueName = queue,
                        delay = 30,
                        batchSize = Just 1,
                        conditional = Nothing
                      }
              _ <- assertRight result
              spans <- readIORef spansRef
              receiveSpans <- spansWithFirstWord "receive" spans
              unless (length receiveSpans == 1) $
                spanNames spans >>= \names ->
                  assertFailure $
                    "Expected exactly one receive span, got "
                      <> show names
              s <- singleSpan receiveSpans "receive"
              actualSpanName <- spanName s
              assertEqual
                "span name carries destination"
                ("receive " <> queueNameToText queue)
                actualSpanName
              assertSpanKindConsumer s
              assertAttrText s "messaging.system" "pgmq"
              assertAttrText s "messaging.operation" "receive"
              assertAttrText s "messaging.destination.name" (queueNameToText queue)
              assertAttrText s "db.system" "postgresql"
              assertAttrText s "db.operation" "pgmq.read",
          testCase "stable opt-in emits stable messaging and database attributes" $
            withSemconvOptIn "messaging,database" $ do
              (tracer, _provider, spansRef) <- setupTracer
              queue <- mkUniqueQueue "stable_attrs"
              result <-
                runEff . runError @PgmqRuntimeError . runPgmqTraced pool tracer $ do
                  createQueue queue
                  sendMessage
                    SendMessage
                      { queueName = queue,
                        messageBody = MessageBody "hello",
                        delay = Nothing
                      }
              _ <- assertRight result
              spans <- readIORef spansRef
              publishSpans <- spansWithFirstWord "publish" spans
              s <- singleSpan publishSpans "publish"
              assertAttrText s "messaging.system" "pgmq"
              assertNoAttr s "messaging.operation"
              assertAttrText s "messaging.operation.name" "publish"
              assertAttrText s "messaging.operation.type" "publish"
              assertAttrText s "messaging.destination.name" (queueNameToText queue)
              assertNoAttr s "db.system"
              assertNoAttr s "db.operation"
              assertAttrText s "db.system.name" "postgresql"
              assertAttrText s "db.operation.name" "pgmq.send",
          testCase "dup opt-in emits old and stable messaging and database attributes" $
            withSemconvOptIn "messaging/dup,database/dup" $ do
              (tracer, _provider, spansRef) <- setupTracer
              queue <- mkUniqueQueue "dup_attrs"
              result <-
                runEff . runError @PgmqRuntimeError . runPgmqTraced pool tracer $ do
                  createQueue queue
                  sendMessage
                    SendMessage
                      { queueName = queue,
                        messageBody = MessageBody "hello",
                        delay = Nothing
                      }
              _ <- assertRight result
              spans <- readIORef spansRef
              publishSpans <- spansWithFirstWord "publish" spans
              s <- singleSpan publishSpans "publish"
              assertAttrText s "messaging.operation" "publish"
              assertAttrText s "messaging.operation.name" "publish"
              assertAttrText s "messaging.operation.type" "publish"
              assertAttrText s "db.system" "postgresql"
              assertAttrText s "db.operation" "pgmq.send"
              assertAttrText s "db.system.name" "postgresql"
              assertAttrText s "db.operation.name" "pgmq.send",
          testCase "error path records exception event and Error status" $ do
            (tracer, _provider, spansRef) <- setupTracer
            bogus <- mkQueueName "queue_that_does_not_exist_xyz3"
            _ <-
              runEff
                . runError @PgmqRuntimeError
                . runPgmqTraced pool tracer
                $ deleteMessage
                  MessageQuery {queueName = bogus, messageId = MessageId 1}
            spans <- readIORef spansRef
            s <- case spans of
              [only] -> pure only
              _ ->
                spanNames spans >>= \names ->
                  assertFailure
                    ( "Expected exactly one span, got "
                        <> show names
                    )
                    >> fail ""
            hot <- readIORef (OTel.spanHot s)
            let events =
                  V.toList
                    (appendOnlyBoundedCollectionValues (OTel.hotEvents hot))
                hasExceptionEvent =
                  any ((==) "exception" . OTel.eventName) events
            assertBool "span has exception event" hasExceptionEvent
            case OTel.hotStatus hot of
              OTel.Error desc ->
                assertBool
                  ( "span status description should be a short non-PII label, got "
                      <> T.unpack desc
                  )
                  ("pool.session.statement" `T.isPrefixOf` desc)
              other ->
                assertFailure $ "expected Error status, got " <> show other,
          testCase "W3C traceparent round-trips through message headers" $ do
            (tracer, provider, _spansRef) <- setupTracer
            queue <- mkUniqueQueue "w3c_roundtrip"
            parentSpan <-
              OTel.createSpan
                tracer
                Ctxt.empty
                "parent"
                OTel.defaultSpanArguments
            parentTraceId <- OTel.traceId <$> OTel.getSpanContext parentSpan
            _ <- CtxtLocal.attachContext (Ctxt.insertSpan parentSpan Ctxt.empty)
            result <-
              runEff . runError @PgmqRuntimeError . runPgmqTraced pool tracer $ do
                createQueue queue
                _ <- sendMessageTraced provider queue (MessageBody "payload") Nothing
                readMessage
                  ReadMessage
                    { queueName = queue,
                      delay = 30,
                      batchSize = Just 1,
                      conditional = Nothing
                    }
            OTel.endSpan parentSpan Nothing
            msgs <- assertRight result
            msg <- case V.toList msgs of
              [m] -> pure m
              other ->
                assertFailure ("expected 1 message, got " <> show (length other))
                  >> fail ""
            case Pgmq.headers msg of
              Nothing ->
                assertFailure
                  "expected trace headers on received message, got none"
              Just hdrs -> do
                ctx <-
                  extractTraceContext provider (jsonToTraceHeaders hdrs) Ctxt.empty
                case Ctxt.lookupSpan ctx of
                  Nothing ->
                    assertFailure $
                      "propagator did not extract a span from headers: " <> show hdrs
                  Just extracted -> do
                    extractedTraceId <- OTel.traceId <$> OTel.getSpanContext extracted
                    assertEqual
                      "trace id round-trips via message headers"
                      parentTraceId
                      extractedTraceId
        ]
    ]

-- ---------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------

-- | Build a tracer, tracer provider (with in-memory list processor and
-- W3C Trace Context propagator), and a handle to the exported spans.
setupTracer :: IO (OTel.Tracer, OTel.TracerProvider, IORef [OTel.ImmutableSpan])
setupTracer = do
  (processor, spansRef) <- InMemoryExporter.inMemoryListExporter
  let opts =
        OTel.emptyTracerProviderOptions
          { OTel.tracerProviderOptionsIdGenerator = defaultIdGenerator,
            OTel.tracerProviderOptionsPropagators = W3C.w3cTraceContextPropagator
          }
  provider <- OTel.createTracerProvider [processor] opts
  let tracer =
        OTel.makeTracer provider "pgmq-effectful-test" OTel.tracerOptions
  pure (tracer, provider, spansRef)

mkQueueName :: String -> IO QueueName
mkQueueName raw =
  case parseQueueName (T.pack raw) of
    Right q -> pure q
    Left err ->
      assertFailure
        ("could not build queue name " <> show raw <> ": " <> show err)
        >> fail ""

mkUniqueQueue :: String -> IO QueueName
mkUniqueQueue prefix = do
  suffix <- randomRIO (100000 :: Int, 999999)
  mkQueueName (prefix <> "_" <> show suffix)

firstWord :: Text -> Text
firstWord = T.takeWhile (/= ' ')

spanName :: OTel.ImmutableSpan -> IO Text
spanName s = OTel.hotName <$> readIORef (OTel.spanHot s)

spanNames :: [OTel.ImmutableSpan] -> IO [Text]
spanNames = traverse spanName

spansWithFirstWord :: Text -> [OTel.ImmutableSpan] -> IO [OTel.ImmutableSpan]
spansWithFirstWord word spans =
  fmap
    (fmap fst . filter ((==) word . firstWord . snd))
    (traverse (\s -> do name <- spanName s; pure (s, name)) spans)

assertRight :: (Show a) => Either a b -> IO b
assertRight = \case
  Right b -> pure b
  Left e ->
    assertFailure ("expected Right, got Left " <> show e) >> fail ""

singleSpan :: [OTel.ImmutableSpan] -> String -> IO OTel.ImmutableSpan
singleSpan spans label = case spans of
  [s] -> pure s
  _ ->
    spanNames spans >>= \names ->
      assertFailure
        ( "expected exactly one "
            <> label
            <> " span, got "
            <> show names
        )
        >> fail ""

-- | 'OTel.SpanKind' has no 'Eq' instance, so assert via pattern.
assertSpanKindProducer :: OTel.ImmutableSpan -> IO ()
assertSpanKindProducer s = case OTel.spanKind s of
  OTel.Producer -> pure ()
  other -> assertFailure ("expected Producer span kind, got " <> show other)

assertSpanKindConsumer :: OTel.ImmutableSpan -> IO ()
assertSpanKindConsumer s = case OTel.spanKind s of
  OTel.Consumer -> pure ()
  other -> assertFailure ("expected Consumer span kind, got " <> show other)

-- | Assert a span carries the given attribute as a 'TextAttribute' with
-- the given value. Produces a readable failure message with the full
-- attribute map when the assertion fails.
assertAttrText :: OTel.ImmutableSpan -> Text -> Text -> IO ()
assertAttrText s key expected = do
  hot <- readIORef (OTel.spanHot s)
  let attrMap = Attrs.getAttributeMap (OTel.hotAttributes hot)
  case HM.lookup key attrMap of
    Just (Attrs.AttributeValue (Attrs.TextAttribute actual)) ->
      assertEqual ("attribute " <> T.unpack key) expected actual
    other ->
      assertFailure $
        "expected text attribute "
          <> T.unpack key
          <> "="
          <> T.unpack expected
          <> "; got "
          <> show other
          <> " in "
          <> show attrMap

assertNoAttr :: OTel.ImmutableSpan -> Text -> IO ()
assertNoAttr s key = do
  hot <- readIORef (OTel.spanHot s)
  let attrMap = Attrs.getAttributeMap (OTel.hotAttributes hot)
  case HM.lookup key attrMap of
    Nothing -> pure ()
    Just actual ->
      assertFailure $
        "expected no attribute "
          <> T.unpack key
          <> "; got "
          <> show actual
          <> " in "
          <> show attrMap

withSemconvOptIn :: String -> IO a -> IO a
withSemconvOptIn value action =
  bracket
    (takeMVar semconvEnvLock *> (lookupEnv var <* setEnv var value))
    restore
    (const action)
  where
    var = "OTEL_SEMCONV_STABILITY_OPT_IN"
    restore = \case
      Nothing -> unsetEnv var *> putMVar semconvEnvLock ()
      Just old -> setEnv var old *> putMVar semconvEnvLock ()

semconvEnvLock :: MVar ()
semconvEnvLock = unsafePerformIO (newMVar ())
{-# NOINLINE semconvEnvLock #-}
