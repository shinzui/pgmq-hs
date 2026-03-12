{-# LANGUAGE OverloadedStrings #-}

module ConfigSpec
  ( tests,
  )
where

import Control.Lens ((^.))
import Data.Generics.Labels ()
import Data.Text qualified as T
import Data.Word (Word32)
import Hasql.Pool qualified as Pool
import Hasql.Session qualified
import Pgmq.Config
import Pgmq.Hasql.Sessions qualified as Sessions
import Pgmq.Types (QueueName, parseQueueName, parseTopicPattern, queueNameToText, topicPatternToText)
import System.Random (randomRIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

tests :: Pool.Pool -> TestTree
tests pool =
  testGroup
    "Pgmq.Config"
    [ testEnsureQueuesCreatesStandard pool,
      testEnsureQueuesCreatesUnlogged pool,
      testEnsureQueuesIdempotent pool,
      testEnsureQueuesIncremental pool,
      testEnsureQueuesWithNotify pool,
      testEnsureQueuesWithFifo pool,
      testEnsureQueuesWithTopicBinding pool
    ]

-- | Helper to run a session and fail on error
runSession :: Pool.Pool -> Hasql.Session.Session a -> IO a
runSession p session = do
  result <- Pool.use p session
  case result of
    Left err -> assertFailure $ "Session failed: " <> show err
    Right a -> pure a

-- | Generate a random queue name for test isolation
genQueueName :: IO QueueName
genQueueName = do
  suffix <- randomRIO (10000 :: Word32, 99999)
  case parseQueueName ("cfg_test_" <> T.pack (show suffix)) of
    Left err -> error $ "Failed to generate queue name: " <> show err
    Right qn -> pure qn

-- | Clean up a queue (ignore errors)
cleanupQueue :: Pool.Pool -> QueueName -> IO ()
cleanupQueue p qn = do
  _ <- Pool.use p (Sessions.dropQueue qn)
  pure ()

testEnsureQueuesCreatesStandard :: Pool.Pool -> TestTree
testEnsureQueuesCreatesStandard pool = testCase "creates a standard queue" $ do
  qn <- genQueueName
  let configs = [standardQueue qn]
  actions <- runSession pool (ensureQueuesReport configs)
  assertBool "should have CreatedQueue action" $
    any isCreatedQueue actions
  -- Verify queue exists
  queues <- runSession pool Sessions.listQueues
  assertBool "queue should be in list" $
    any (\q -> (q ^. #name) == qn) queues
  cleanupQueue pool qn

testEnsureQueuesCreatesUnlogged :: Pool.Pool -> TestTree
testEnsureQueuesCreatesUnlogged pool = testCase "creates an unlogged queue" $ do
  qn <- genQueueName
  let configs = [unloggedQueue qn]
  actions <- runSession pool (ensureQueuesReport configs)
  assertBool "should have CreatedQueue action" $
    any isCreatedQueue actions
  -- Verify it's unlogged
  queues <- runSession pool Sessions.listQueues
  let mq = filter (\q -> (q ^. #name) == qn) queues
  case mq of
    [q] -> (q ^. #isUnlogged) @?= True
    _ -> assertFailure "queue not found"
  cleanupQueue pool qn

testEnsureQueuesIdempotent :: Pool.Pool -> TestTree
testEnsureQueuesIdempotent pool = testCase "is idempotent (second run skips)" $ do
  qn <- genQueueName
  let configs = [standardQueue qn]
  -- First run: creates
  actions1 <- runSession pool (ensureQueuesReport configs)
  assertBool "first run should create" $
    any isCreatedQueue actions1
  -- Second run: skips
  actions2 <- runSession pool (ensureQueuesReport configs)
  assertBool "second run should skip" $
    all isSkipped actions2
  cleanupQueue pool qn

testEnsureQueuesIncremental :: Pool.Pool -> TestTree
testEnsureQueuesIncremental pool = testCase "creates only new queues incrementally" $ do
  qn1 <- genQueueName
  qn2 <- genQueueName
  -- First run: create one queue
  _ <- runSession pool (ensureQueuesReport [standardQueue qn1])
  -- Second run: add a second queue
  actions <- runSession pool (ensureQueuesReport [standardQueue qn1, standardQueue qn2])
  -- qn1 should be skipped, qn2 should be created
  let qn1Actions = filter (actionForQueue qn1) actions
      qn2Actions = filter (actionForQueue qn2) actions
  assertBool "existing queue should be skipped" $
    all isSkipped qn1Actions
  assertBool "new queue should be created" $
    any isCreatedQueue qn2Actions
  cleanupQueue pool qn1
  cleanupQueue pool qn2

testEnsureQueuesWithNotify :: Pool.Pool -> TestTree
testEnsureQueuesWithNotify pool = testCase "enables notify insert" $ do
  qn <- genQueueName
  let configs = [withNotifyInsert (Just 500) (standardQueue qn)]
  actions <- runSession pool (ensureQueuesReport configs)
  assertBool "should have EnabledNotify action" $
    any isEnabledNotify actions
  -- Second run should skip notify
  actions2 <- runSession pool (ensureQueuesReport configs)
  assertBool "second run should skip notify" $
    any isSkippedNotify actions2
  cleanupQueue pool qn

testEnsureQueuesWithFifo :: Pool.Pool -> TestTree
testEnsureQueuesWithFifo pool = testCase "creates FIFO index" $ do
  qn <- genQueueName
  let configs = [withFifoIndex (standardQueue qn)]
  actions <- runSession pool (ensureQueuesReport configs)
  assertBool "should have CreatedFifoIndex action" $
    any isFifoIndex actions
  cleanupQueue pool qn

testEnsureQueuesWithTopicBinding :: Pool.Pool -> TestTree
testEnsureQueuesWithTopicBinding pool = testCase "binds topic pattern" $ do
  qn <- genQueueName
  pat <- case parseTopicPattern "orders.*" of
    Left err -> assertFailure ("Failed to parse topic pattern: " <> show err) >> error "unreachable"
    Right p -> pure p
  let configs = [withTopicBinding pat (standardQueue qn)]
  actions <- runSession pool (ensureQueuesReport configs)
  assertBool "should have BoundTopic action" $
    any isBoundTopic actions
  -- Verify binding exists
  bindings <- runSession pool Sessions.listTopicBindings
  assertBool "binding should exist" $
    any
      ( \b ->
          (b ^. #bindingQueueName) == queueNameToText qn
            && topicPatternToText (b ^. #bindingPattern) == "orders.*"
      )
      bindings
  -- Second run should skip
  actions2 <- runSession pool (ensureQueuesReport configs)
  assertBool "second run should skip topic binding" $
    any isSkippedTopicBinding actions2
  cleanupQueue pool qn

-- Helpers for checking action types

isCreatedQueue :: ReconcileAction -> Bool
isCreatedQueue (CreatedQueue _ _) = True
isCreatedQueue _ = False

isSkipped :: ReconcileAction -> Bool
isSkipped (SkippedQueue _) = True
isSkipped (SkippedNotify _) = True
isSkipped (SkippedFifoIndex _) = True
isSkipped (SkippedTopicBinding _ _) = True
isSkipped _ = False

isSkippedTopicBinding :: ReconcileAction -> Bool
isSkippedTopicBinding (SkippedTopicBinding _ _) = True
isSkippedTopicBinding _ = False

isEnabledNotify :: ReconcileAction -> Bool
isEnabledNotify (EnabledNotify _ _) = True
isEnabledNotify _ = False

isSkippedNotify :: ReconcileAction -> Bool
isSkippedNotify (SkippedNotify _) = True
isSkippedNotify _ = False

isFifoIndex :: ReconcileAction -> Bool
isFifoIndex (CreatedFifoIndex _) = True
isFifoIndex _ = False

isBoundTopic :: ReconcileAction -> Bool
isBoundTopic (BoundTopic _ _) = True
isBoundTopic _ = False

actionForQueue :: QueueName -> ReconcileAction -> Bool
actionForQueue qn (CreatedQueue q _) = q == qn
actionForQueue qn (SkippedQueue q) = q == qn
actionForQueue qn (EnabledNotify q _) = q == qn
actionForQueue qn (SkippedNotify q) = q == qn
actionForQueue qn (CreatedFifoIndex q) = q == qn
actionForQueue qn (SkippedFifoIndex q) = q == qn
actionForQueue qn (BoundTopic q _) = q == qn
actionForQueue qn (SkippedTopicBinding q _) = q == qn
