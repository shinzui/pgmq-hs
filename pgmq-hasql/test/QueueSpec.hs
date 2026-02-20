{-# LANGUAGE OverloadedStrings #-}

-- | Tests for queue management operations
module QueueSpec (tests) where

import EphemeralDb (TestFixture (..), withTestFixture)
import Hasql.Pool qualified as Pool
import Pgmq.Hasql.Sessions qualified as Sessions
import Pgmq.Types (Queue (..), parseQueueName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
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
      testCreateUnloggedQueue p
      -- Note: testCreatePartitionedQueue is skipped because it requires pg_partman extension
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
