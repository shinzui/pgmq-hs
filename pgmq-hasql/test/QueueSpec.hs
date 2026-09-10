{-# LANGUAGE OverloadedStrings #-}

-- | Tests for queue management operations
module QueueSpec (tests) where

import Control.Exception (finally)
import Control.Monad (forM_)
import Data.Int (Int32)
import Data.List (isInfixOf)
import Data.Text qualified
import EphemeralDb (TestFixture (..), withPgmqPool, withTestFixture)
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Pool qualified as Pool
import Hasql.Session qualified as Session
import Hasql.Statement (Statement, preparable)
import Pgmq.Hasql.Sessions qualified as Sessions
import Pgmq.Hasql.Statements.Types qualified as StmtTypes
import Pgmq.Types (Queue (..), parseQueueName, queueNameToText)
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
      testCreatePartitionedQueueIsReentrant p,
      partitionCompatibilityTests p
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

partitionCompatibilityTests :: Pool.Pool -> TestTree
partitionCompatibilityTests p =
  testGroup
    "PartitionCompatibility"
    [ testCase "legacy creation keeps default premake on both versions" $
        withPartman p $
          withTestFixture p $ \TestFixture {pool, queueName} -> flip finally (cleanupQueue pool queueName) $ do
            assertSession pool (Sessions.createPartitionedQueue (StmtTypes.CreatePartitionedQueue queueName "10" "100"))
            counts <- premakeCounts pool (queueNameToText queueName)
            counts @?= [4, 4],
      testCase "explicit premake controls both parents or reports unsupported 1.12" $
        withPartman p $
          withTestFixture p $ \TestFixture {pool, queueName} -> flip finally (cleanupQueue pool queueName) $ do
            stock <- (== Just "1.12.0") <$> lookupEnv "PGMQ_TEST_SCHEMA_VERSION"
            let request = StmtTypes.CreatePartitionedQueue queueName "10" "100"
            if stock
              then do
                result <- Pool.use pool (Sessions.createPartitionedQueueWithPremake request 2)
                case result of
                  Left err -> assertBool "undefined function SQLSTATE" ("42883" `isInfixOf` show err)
                  Right () -> assertFailure "1.12 must not silently discard explicit premake"
                assertNoPartitionQueue pool (queueNameToText queueName)
              else do
                assertSession pool (Sessions.createPartitionedQueueWithPremake request 2)
                counts <- premakeCounts pool (queueNameToText queueName)
                counts @?= [2, 2]
                assertSession pool (Sessions.createPartitionedQueueWithPremake request 2),
      testCase "zero and negative premake fail without creating objects" $
        forM_ [0, -1] $ \count -> do
          -- A failed Parse on the pinned Hasql leaves a prepared-statement
          -- cache entry. Isolate each unsupported-signature check so its first
          -- server error is observed instead of a later missing-prepared error.
          result <- withPgmqPool $ \isolated -> withPartman isolated $
            withTestFixture isolated $ \TestFixture {pool, queueName} -> flip finally (cleanupQueue pool queueName) $ do
              stock <- (== Just "1.12.0") <$> lookupEnv "PGMQ_TEST_SCHEMA_VERSION"
              creation <- Pool.use pool (Sessions.createPartitionedQueueWithPremake (StmtTypes.CreatePartitionedQueue queueName "10" "100") count)
              case creation of
                Left err ->
                  assertBool
                    ("documented database error: " <> show err)
                    ((if stock then "42883" else "premake must be at least 1") `isInfixOf` show err)
                Right () -> assertFailure "invalid premake unexpectedly succeeded"
              assertNoPartitionQueue pool (queueNameToText queueName)
          assertRight result
    ]

withPartman :: Pool.Pool -> IO () -> IO ()
withPartman pool action = do
  available <- assertSession pool (Session.statement () pgPartmanAvailable)
  required <- (== Just "1") <$> lookupEnv "PGMQ_REQUIRE_PARTMAN"
  if available
    then action
    else
      if required
        then assertFailure "PGMQ_REQUIRE_PARTMAN=1 but pg_partman is not installed"
        else putStrLn "    SKIPPED: pg_partman is not installed"

premakeCounts :: Pool.Pool -> Data.Text.Text -> IO [Int32]
premakeCounts pool queue =
  assertSession pool $
    Session.statement queue $
      preparable
        "select premake from partman.part_config where parent_table in ('pgmq.q_' || $1, 'pgmq.a_' || $1) order by parent_table"
        (E.param (E.nonNullable E.text))
        (D.rowList (D.column (D.nonNullable D.int4)))

assertNoPartitionQueue :: Pool.Pool -> Data.Text.Text -> IO ()
assertNoPartitionQueue pool queue = do
  absent <-
    assertSession pool $
      Session.statement queue $
        preparable
          "select not exists (select from pgmq.meta where queue_name = $1) and to_regclass('pgmq.q_' || $1) is null and to_regclass('pgmq.a_' || $1) is null and not exists (select from partman.part_config where parent_table in ('pgmq.q_' || $1, 'pgmq.a_' || $1))"
          (E.param (E.nonNullable E.text))
          (D.singleRow (D.column (D.nonNullable D.bool)))
  assertBool "failed creation leaves no metadata, tables or parent registrations" absent
