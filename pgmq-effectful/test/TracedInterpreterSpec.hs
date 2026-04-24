{-# LANGUAGE OverloadedStrings #-}

module TracedInterpreterSpec (tests) where

import Effectful (runEff)
import Effectful.Error.Static (runError)
import Hasql.Pool qualified as Pool
import OpenTelemetry.Trace.Core qualified as OTel
import Pgmq.Effectful
  ( MessageId (..),
    MessageQuery (..),
    PgmqRuntimeError (..),
    deleteMessage,
    parseQueueName,
    runPgmqTraced,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

tests :: Pool.Pool -> TestTree
tests pool =
  testGroup
    "Traced interpreter error propagation"
    [ testCase "statement error surfaces PgmqSessionError via Error channel" $ do
        tracer <- noopTracer
        bogus <- case parseQueueName "queue_that_does_not_exist_xyz2" of
          Right q -> pure q
          Left err -> assertFailure ("could not build test queue name: " <> show err) >> fail ""
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
    ]
  where
    -- \| Build a no-op tracer. We do not install any SpanProcessor, so spans are
    -- started and ended but never exported. That is sufficient to exercise the
    -- traced interpreter\'s error-propagation path without depending on the
    -- hs-opentelemetry-sdk package.
    noopTracer :: IO OTel.Tracer
    noopTracer = do
      tp <- OTel.createTracerProvider [] OTel.emptyTracerProviderOptions
      pure $
        OTel.makeTracer
          tp
          "pgmq-effectful-test"
          OTel.tracerOptions
