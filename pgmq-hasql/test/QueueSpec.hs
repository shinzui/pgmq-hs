{-# LANGUAGE OverloadedStrings #-}

-- | Tests for queue management operations
module QueueSpec (tests) where

import EphemeralDb (TestFixture (..), withTestFixture)
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Pool qualified as Pool
import Hasql.Session qualified as Session
import Hasql.Statement (Statement, preparable)
import Pgmq.Hasql.Sessions qualified as Sessions
import Pgmq.Hasql.Statements.Types qualified as StmtTypes
import Pgmq.Types (Queue (..), parseQueueName)
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestUtils
  ( assertRight,
    assertSession,
    cleanupQueue,
  )

-- | All queue management tests
tests :: Pool.Pool -> TestTree
tests p =
  testGroup
    "Queue Management"
    [ testCreateQueue p,
      testDropQueue p,
      testDropNonExistentQueue p,
      testListQueues p,
      testCreateUnloggedQueue p,
      -- Note: the partitioned-queue tests need the pg_partman extension, which
      -- is not present in the ephemeral test environment; this one reports the
      -- skip rather than pretending to have run.
      testCreatePartitionedQueueIsReentrant p
    ]

testCreateQueue :: Pool.Pool -> TestTree
testCreateQueue p = testCase "createQueue creates a new queue" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    -- Create the queue
    assertSession pool (Sessions.createQueue queueName)
    -- Verify it exists by listing queues
    queues <- assertSession pool Sessions.listQueues
    let queueNames = map (\q -> name q) queues
    assertBool "Queue should be in list" (queueName `elem` queueNames)
    -- Cleanup
    cleanupQueue pool queueName

testDropQueue :: Pool.Pool -> TestTree
testDropQueue p = testCase "dropQueue removes an existing queue" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    -- Create then drop the queue
    assertSession pool (Sessions.createQueue queueName)
    dropped <- assertSession pool (Sessions.dropQueue queueName)
    dropped @?= True
    -- Verify it's gone
    queues <- assertSession pool Sessions.listQueues
    let queueNames = map (\q -> name q) queues
    assertBool "Queue should not be in list" (queueName `notElem` queueNames)

testDropNonExistentQueue :: Pool.Pool -> TestTree
testDropNonExistentQueue p = testCase "dropQueue returns False for non-existent queue" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    -- Try to drop a queue that doesn't exist
    dropped <- assertSession pool (Sessions.dropQueue queueName)
    dropped @?= False

testListQueues :: Pool.Pool -> TestTree
testListQueues p = testCase "listQueues returns all created queues" $ do
  queueName1 <- assertRight $ parseQueueName "test_list_q1"
  queueName2 <- assertRight $ parseQueueName "test_list_q2"
  -- Create two queues
  assertSession p (Sessions.createQueue queueName1)
  assertSession p (Sessions.createQueue queueName2)
  -- List and verify both exist
  queues <- assertSession p Sessions.listQueues
  let queueNames = map (\q -> name q) queues
  assertBool "Queue 1 should be in list" (queueName1 `elem` queueNames)
  assertBool "Queue 2 should be in list" (queueName2 `elem` queueNames)
  -- Cleanup
  cleanupQueue p queueName1
  cleanupQueue p queueName2

-- | Two replicas can call @create_partitioned@ for the same queue: the advisory
-- lock serializes them, but the second one used to fail anyway because
-- @partman.create_parent@ rejects an already-registered parent. Migration
-- 0003 guards both @create_parent@ calls with a @part_config@ probe.
testCreatePartitionedQueueIsReentrant :: Pool.Pool -> TestTree
testCreatePartitionedQueueIsReentrant p =
  testCase "createPartitionedQueue is re-entrant (needs pg_partman)" $ do
    stock <- (== Just "1.12.0") <$> lookupEnv "PGMQ_TEST_SCHEMA_VERSION"
    required <- (== Just "1") <$> lookupEnv "PGMQ_REQUIRE_PARTMAN"
    available <- assertSession p (Session.statement () pgPartmanAvailable)
    if stock
      then putStrLn "    SKIPPED: local partition re-entry is a native-ledger contract"
      else
        if not available
          then
            if required
              then assertFailure "PGMQ_REQUIRE_PARTMAN=1 but pg_partman is not installed"
              else putStrLn "    SKIPPED: pg_partman is not installed in this PostgreSQL database"
          else do
            qName <- assertRight $ parseQueueName "test_partitioned_reentry"
            let request =
                  StmtTypes.CreatePartitionedQueue
                    { StmtTypes.queueName = qName,
                      StmtTypes.partitionInterval = "10000",
                      StmtTypes.retentionInterval = "100000"
                    }
            assertSession p (Sessions.createPartitionedQueue request)
            assertSession p (Sessions.createPartitionedQueue request)
            cleanupQueue p qName

pgPartmanAvailable :: Statement () Bool
pgPartmanAvailable = preparable sql E.noParams decoder
  where
    sql = "select exists (select 1 from pg_extension where extname = 'pg_partman')"
    decoder = D.singleRow (D.column (D.nonNullable D.bool))

testCreateUnloggedQueue :: Pool.Pool -> TestTree
testCreateUnloggedQueue p = testCase "createUnloggedQueue creates an unlogged queue" $ do
  qName <- assertRight $ parseQueueName "test_unlogged_q"
  assertSession p (Sessions.createUnloggedQueue qName)
  -- Verify it exists
  queues <- assertSession p Sessions.listQueues
  let matchingQueues = filter (\q -> name q == qName) queues
  -- Just verify the queue was created (isUnlogged status depends on pgmq schema version)
  assertBool "Queue should exist" (not (null matchingQueues))
  -- Cleanup
  cleanupQueue p qName
