{-# LANGUAGE OverloadedStrings #-}

-- | Tests for queue metrics operations
module MetricsSpec (tests) where

import Control.Exception (finally)
import Data.Aeson (object, (.=))
import Data.Int (Int64)
import Data.Text (Text)
import EphemeralDb (TestFixture (..), withPgmqPool, withTestFixture)
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Pool qualified as Pool
import Hasql.Session qualified as Session
import Hasql.Statement (preparable)
import Pgmq.Hasql.Sessions qualified as Sessions
import Pgmq.Hasql.Statements.Types (QueueMetrics (..))
import Pgmq.Hasql.Statements.Types qualified as Types
import Pgmq.Types (MessageBody (..), QueueName, parseQueueName, queueNameToText)
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase, (@?=))
import TestUtils (assertRight, assertSession, cleanupQueue)

-- | All metrics tests
tests :: TestTree
tests =
  testGroup
    "Metrics"
    [ testQueueMetrics,
      testQueueMetricsEmpty,
      testAllQueueMetrics,
      testUnloggedMetrics,
      testPartitionMetrics
    ]

-- | Test queueMetrics returns correct metrics
testQueueMetrics :: TestTree
testQueueMetrics = testCase "queueMetrics returns queue statistics" $ withMetricsPool $ \p -> do
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
    defaultPartitionLength metrics @?= Nothing
    queueLength metrics @?= 3
    totalMessages metrics @?= 3
    queueVisibleLength metrics @?= 3
    assertEqual "Queue name should match" (queueNameToText qName) (metricsQueueName metrics)
    cleanupQueue pool qName

-- | Test queueMetrics for empty queue
testQueueMetricsEmpty :: TestTree
testQueueMetricsEmpty = testCase "queueMetrics works for empty queue" $ withMetricsPool $ \p -> do
  withTestFixture p $ \TestFixture {pool, queueName = qName} -> do
    assertSession pool (Sessions.createQueue qName)
    -- Get metrics for empty queue
    metrics <- assertSession pool (Sessions.queueMetrics qName)
    defaultPartitionLength metrics @?= Nothing
    queueLength metrics @?= 0
    totalMessages metrics @?= 0
    queueVisibleLength metrics @?= 0
    cleanupQueue pool qName

-- | Test allQueueMetrics returns all queues
testAllQueueMetrics :: TestTree
testAllQueueMetrics = testCase "allQueueMetrics returns metrics for all queues" $ withMetricsPool $ \p -> do
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
    [m] -> do
      queueLength m @?= 1
      defaultPartitionLength m @?= Nothing
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

testUnloggedMetrics :: TestTree
testUnloggedMetrics = testCase "unlogged queues have no default-partition metric" $ withMetricsPool $ \p ->
  withTestFixture p $ \TestFixture {pool, queueName = qName} -> flip finally (cleanupQueue pool qName) $ do
    assertSession pool (Sessions.createUnloggedQueue qName)
    assertDefaultMetric pool qName Nothing

testPartitionMetrics :: TestTree
testPartitionMetrics = testCase "default-partition estimates include archived spills on both metrics APIs" $ withMetricsPool $ \p -> do
  available <-
    assertSession p $
      Session.statement () $
        preparable "select exists (select from pg_extension where extname = 'pg_partman')" E.noParams (D.singleRow (D.column (D.nonNullable D.bool)))
  required <- (== Just "1") <$> lookupEnv "PGMQ_REQUIRE_PARTMAN"
  if not available
    then
      if required
        then assertFailure "PGMQ_REQUIRE_PARTMAN=1 but pg_partman is not installed"
        else putStrLn "    SKIPPED: pg_partman is not installed"
    else withTestFixture p $ \TestFixture {pool, queueName = qName} -> flip finally (cleanupQueue pool qName) $ do
      stock <- (== Just "1.12.0") <$> lookupEnv "PGMQ_TEST_SCHEMA_VERSION"
      let request = Types.CreatePartitionedQueue qName "10" "100"
          q = queueNameToText qName
          analyze = Session.script ("ANALYZE pgmq.q_" <> q <> "_default; ANALYZE pgmq.a_" <> q <> "_default")
      if stock
        then assertSession pool (Sessions.createPartitionedQueue request)
        else assertSession pool (Sessions.createPartitionedQueueWithPremake request 2)
      assertSession pool analyze
      assertDefaultMetric pool qName (if stock then Nothing else Just 0)
      -- Sending far beyond the premade partitions forces queue and archive spills.
      ids <-
        assertSession
          pool
          ( Sessions.batchSendMessage
              ( Types.BatchSendMessage
                  qName
                  [MessageBody (object ["n" .= n]) | n <- [1 .. 100 :: Int]]
                  Nothing
              )
          )
      let spilled = ids !! 99
      archived <- assertSession pool (Sessions.archiveMessage (Types.MessageQuery qName spilled))
      archived @?= True
      assertSession pool analyze
      (queueRows, archiveRows) <-
        assertSession pool $
          Session.statement () $
            preparable
              ("select (select count(*) from pgmq.q_" <> q <> "_default), (select count(*) from pgmq.a_" <> q <> "_default)")
              E.noParams
              (D.singleRow ((,) <$> D.column (D.nonNullable D.int8) <*> D.column (D.nonNullable D.int8)))
      assertBool "queue contains spilled messages" (queueRows > 0)
      archiveRows @?= 1
      assertDefaultMetric pool qName (if stock then Nothing else Just (queueRows + archiveRows))

assertDefaultMetric :: Pool.Pool -> QueueName -> Maybe Int64 -> IO ()
assertDefaultMetric pool qName expected = do
  single <- assertSession pool (Sessions.queueMetrics qName)
  defaultPartitionLength single @?= expected
  allMetrics <- assertSession pool Sessions.allQueueMetrics
  let matching = filter ((== queueNameToText qName) . metricsQueueName) allMetrics
  assertEqual "exactly one metrics_all row" 1 (length matching)
  map defaultPartitionLength matching @?= [expected]

-- metrics_all enumerates every queue, so unrelated concurrent queue drops can
-- invalidate its table lookup. Each case needs a stable database-wide fixture.
withMetricsPool :: (Pool.Pool -> IO ()) -> IO ()
withMetricsPool action = assertRight =<< withPgmqPool (\pool -> action pool `finally` Pool.release pool)
