{-# LANGUAGE OverloadedStrings #-}

-- | Tests for queue metrics operations
module MetricsSpec (tests) where

import Data.Aeson (object, (.=))
import Data.Text (Text)
import EphemeralDb (TestFixture (..), withTestFixture)
import Hasql.Pool qualified as Pool
import Pgmq.Hasql.Sessions qualified as Sessions
import Pgmq.Hasql.Statements.Types (QueueMetrics (..))
import Pgmq.Hasql.Statements.Types qualified as Types
import Pgmq.Types (MessageBody (..), QueueName, parseQueueName, queueNameToText)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))
import TestUtils (assertRight, assertSession, cleanupQueue)

-- | All metrics tests
tests :: Pool.Pool -> TestTree
tests p =
  testGroup
    "Metrics"
    [ testQueueMetrics p,
      testQueueMetricsEmpty p,
      testAllQueueMetrics p
    ]

-- | Test queueMetrics returns correct metrics
testQueueMetrics :: Pool.Pool -> TestTree
testQueueMetrics p = testCase "queueMetrics returns queue statistics" $ do
  withTestFixture p $ \TestFixture {pool, queueName = qName} -> do
    assertSession pool (Sessions.createQueue qName)
    -- Send some messages
    let msg = makeSendMessage qName
    _ <- assertSession pool (Sessions.sendMessage msg)
    _ <- assertSession pool (Sessions.sendMessage msg)
    _ <- assertSession pool (Sessions.sendMessage msg)
    -- Get metrics
    metrics <- assertSession pool (Sessions.queueMetrics qName)
    -- Verify metrics
    queueLength metrics @?= 3
    totalMessages metrics @?= 3
    queueVisibleLength metrics @?= 3
    assertEqual "Queue name should match" (queueNameToText qName) (metricsQueueName metrics)
    cleanupQueue pool qName

-- | Test queueMetrics for empty queue
testQueueMetricsEmpty :: Pool.Pool -> TestTree
testQueueMetricsEmpty p = testCase "queueMetrics works for empty queue" $ do
  withTestFixture p $ \TestFixture {pool, queueName = qName} -> do
    assertSession pool (Sessions.createQueue qName)
    -- Get metrics for empty queue
    metrics <- assertSession pool (Sessions.queueMetrics qName)
    queueLength metrics @?= 0
    totalMessages metrics @?= 0
    queueVisibleLength metrics @?= 0
    cleanupQueue pool qName

-- | Test allQueueMetrics returns all queues
testAllQueueMetrics :: Pool.Pool -> TestTree
testAllQueueMetrics p = testCase "allQueueMetrics returns metrics for all queues" $ do
  queueName1 <- assertRight $ parseQueueName "test_metrics_q1"
  queueName2 <- assertRight $ parseQueueName "test_metrics_q2"
  -- Create two queues
  assertSession p (Sessions.createQueue queueName1)
  assertSession p (Sessions.createQueue queueName2)
  -- Send a message to the first queue
  let msg = makeSendMessage queueName1
  _ <- assertSession p (Sessions.sendMessage msg)
  -- Get all metrics
  allMetrics <- assertSession p Sessions.allQueueMetrics
  -- Verify both queues are in the results
  let queueNames = map metricsQueueName allMetrics
  assertBool "Queue 1 should be in metrics" (queueNameToText queueName1 `elem` queueNames)
  assertBool "Queue 2 should be in metrics" (queueNameToText queueName2 `elem` queueNames)
  -- Verify queue 1 has the correct message count
  let q1Metrics = filter (\m -> metricsQueueName m == queueNameToText queueName1) allMetrics
  case q1Metrics of
    [m] -> queueLength m @?= 1
    _ -> assertBool "Queue 1 should have metrics" False
  -- Cleanup
  cleanupQueue p queueName1
  cleanupQueue p queueName2

-- | Helper to extract queue name from metrics (avoids ambiguity)
metricsQueueName :: QueueMetrics -> Text
metricsQueueName QueueMetrics {queueName = qn} = qn

-- | Helper to create SendMessage (avoids field ambiguity)
makeSendMessage :: QueueName -> Types.SendMessage
makeSendMessage qName =
  Types.SendMessage
    { Types.queueName = qName,
      Types.messageBody = MessageBody (object ["test" .= (1 :: Int)]),
      Types.delay = Nothing
    }
