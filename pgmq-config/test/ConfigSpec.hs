{-# LANGUAGE OverloadedStrings #-}

module ConfigSpec
  ( tests,
  )
where

import Control.Lens ((^.))
import Data.Generics.Labels ()
import Data.Int (Int32)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Word (Word32)
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Pool qualified as Pool
import Hasql.Session qualified
import Hasql.Statement (Statement, preparable)
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
      testEnsureQueuesWithNotifyDefault pool,
      testEnsureQueuesWithFifo pool,
      testEnsureQueuesSkipsExistingFifo pool,
      testEnsureQueuesUpdatesDriftedThrottle pool,
      testEnsureQueuesReportsQueueTypeDrift pool,
      testEnsureQueuesWithTopicBinding pool,
      testEnsureQueuesIsTrulyIdempotent pool,
      testEnsureQueuesSilentIdempotent pool,
      testEnsureQueuesSilentIncremental pool
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

-- | A queue configured with @withNotifyInsert Nothing@ must reconcile cleanly,
-- twice.
--
-- @Nothing@ is documented as "use the pgmq default (250 ms)". Before the fix
-- the pgmq-hasql statement bound an SQL NULL for the throttle interval, which
-- @pgmq.enable_notify_insert@ inserted into a @NOT NULL@ column — a column
-- DEFAULT does not apply to an explicitly supplied NULL — so the call always
-- raised SQLSTATE 23502. Reconciliation is not one transaction: each statement
-- autocommits, so the queue creation stuck while the notify enable failed,
-- and every subsequent startup failed the same way forever.
testEnsureQueuesWithNotifyDefault :: Pool.Pool -> TestTree
testEnsureQueuesWithNotifyDefault pool = testCase "enables notify insert with the default throttle" $ do
  qn <- genQueueName
  let configs = [withNotifyInsert Nothing (standardQueue qn)]
  actions <- runSession pool (ensureQueuesReport configs)
  assertBool "first run should have EnabledNotify action" $
    any isEnabledNotify actions
  -- Second run must be a clean skip, proving the first run actually recorded
  -- a throttle row rather than failing.
  actions2 <- runSession pool (ensureQueuesReport configs)
  assertBool "second run should skip notify" $
    any isSkippedNotify actions2
  -- The recorded interval must be the documented 250ms default.
  throttles <- runSession pool Sessions.listNotifyInsertThrottles
  let mine = filter (\t -> (t ^. #throttleQueueName) == queueNameToText qn) throttles
  case mine of
    [t] -> (t ^. #throttleIntervalMs) @?= 250
    _ -> assertFailure $ "expected exactly one throttle row, got " <> show (length mine)
  cleanupQueue pool qn

testEnsureQueuesWithFifo :: Pool.Pool -> TestTree
testEnsureQueuesWithFifo pool = testCase "creates FIFO index" $ do
  qn <- genQueueName
  let configs = [withFifoIndex (standardQueue qn)]
  actions <- runSession pool (ensureQueuesReport configs)
  assertBool "should have CreatedFifoIndex action" $
    any isFifoIndex actions
  cleanupQueue pool qn

-- | The FIFO action must report what actually happened.
--
-- @pgmq.create_fifo_index@ is @CREATE INDEX IF NOT EXISTS@ and tells the caller
-- nothing, so the reconciler used to report 'CreatedFifoIndex' on every run
-- forever and 'SkippedFifoIndex' was unreachable dead code. The reconciler now
-- snapshots @pg_indexes@ first, so the second run skips the call outright.
testEnsureQueuesSkipsExistingFifo :: Pool.Pool -> TestTree
testEnsureQueuesSkipsExistingFifo pool = testCase "second run skips the existing FIFO index" $ do
  qn <- genQueueName
  let configs = [withFifoIndex (standardQueue qn)]
  actions1 <- runSession pool (ensureQueuesReport configs)
  assertBool
    ("first run should create the FIFO index, got " <> show actions1)
    (any isFifoIndex actions1)
  actions2 <- runSession pool (ensureQueuesReport configs)
  assertBool
    ("second run should skip the FIFO index, got " <> show actions2)
    (any isSkippedFifoIndex actions2)
  assertBool
    ("second run must not claim to have created it, got " <> show actions2)
    (not (any isFifoIndex actions2))
  cleanupQueue pool qn

-- | A declared throttle interval that differs from the stored one is applied.
--
-- This is the reconciler's single mutation of already-existing state. Before
-- the fix the declared value was compared only for presence, so changing it in
-- the config had no effect on the database and the report said 'SkippedNotify'.
testEnsureQueuesUpdatesDriftedThrottle :: Pool.Pool -> TestTree
testEnsureQueuesUpdatesDriftedThrottle pool = testCase "updates a drifted notify throttle" $ do
  qn <- genQueueName
  _ <- runSession pool (ensureQueuesReport [withNotifyInsert (Just 250) (standardQueue qn)])
  -- Redeclare with a different interval: the row must be brought in line.
  actions <- runSession pool (ensureQueuesReport [withNotifyInsert (Just 500) (standardQueue qn)])
  assertBool
    ("expected UpdatedNotifyThrottle " <> show qn <> " 250 500, got " <> show actions)
    (any (isUpdatedThrottle qn 250 500) actions)
  throttleFor pool qn >>= (@?= 500)
  -- A third run at the now-current value is a clean skip, so the update
  -- converges instead of firing on every startup.
  actions3 <- runSession pool (ensureQueuesReport [withNotifyInsert (Just 500) (standardQueue qn)])
  assertBool
    ("third run should skip notify, got " <> show actions3)
    (any isSkippedNotify actions3)
  cleanupQueue pool qn

-- | A declared queue type contradicting the live queue is reported, not fixed.
--
-- Converting a queue's type means dropping and recreating it, destroying every
-- message it holds, so the reconciler surfaces the contradiction and leaves the
-- decision to an operator. Before the fix it silently reported 'SkippedQueue'.
testEnsureQueuesReportsQueueTypeDrift :: Pool.Pool -> TestTree
testEnsureQueuesReportsQueueTypeDrift pool = testCase "reports queue-type drift without mutating" $ do
  qn <- genQueueName
  _ <- runSession pool (ensureQueuesReport [standardQueue qn])
  actions <- runSession pool (ensureQueuesReport [unloggedQueue qn])
  assertBool
    ("expected DetectedQueueTypeDrift for " <> show qn <> ", got " <> show actions)
    (any (isQueueTypeDrift qn ObservedStandard) actions)
  assertBool
    ("drift must replace the plain skip, got " <> show actions)
    (not (any isSkippedQueue actions))
  -- The queue is untouched: still standard, still there.
  queues <- runSession pool Sessions.listQueues
  case filter (\q -> (q ^. #name) == qn) queues of
    [q] -> do
      (q ^. #isUnlogged) @?= False
      (q ^. #isPartitioned) @?= False
    other -> assertFailure $ "expected exactly one queue row, got " <> show (length other)
  cleanupQueue pool qn

-- | The stored throttle interval for a queue.
throttleFor :: Pool.Pool -> QueueName -> IO Int32
throttleFor pool qn = do
  throttles <- runSession pool Sessions.listNotifyInsertThrottles
  case filter (\t -> (t ^. #throttleQueueName) == queueNameToText qn) throttles of
    [t] -> pure (t ^. #throttleIntervalMs)
    other -> assertFailure $ "expected exactly one throttle row, got " <> show (length other)

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

isSkippedFifoIndex :: ReconcileAction -> Bool
isSkippedFifoIndex (SkippedFifoIndex _) = True
isSkippedFifoIndex _ = False

isSkippedQueue :: ReconcileAction -> Bool
isSkippedQueue (SkippedQueue _) = True
isSkippedQueue _ = False

isUpdatedThrottle :: QueueName -> Int32 -> Int32 -> ReconcileAction -> Bool
isUpdatedThrottle qn observed declared (UpdatedNotifyThrottle q o d) =
  q == qn && o == observed && d == declared
isUpdatedThrottle _ _ _ _ = False

isQueueTypeDrift :: QueueName -> ObservedQueueType -> ReconcileAction -> Bool
isQueueTypeDrift qn observed (DetectedQueueTypeDrift q _ o) = q == qn && o == observed
isQueueTypeDrift _ _ _ = False

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
actionForQueue qn (UpdatedNotifyThrottle q _ _) = q == qn
actionForQueue qn (DetectedQueueTypeDrift q _ _) = q == qn

-- | Silent-variant version of 'testEnsureQueuesIdempotent': call 'ensureQueues'
-- twice with the same standard-queue config and confirm the queue exists exactly
-- once afterwards.
testEnsureQueuesSilentIdempotent :: Pool.Pool -> TestTree
testEnsureQueuesSilentIdempotent pool = testCase "silent ensureQueues is idempotent" $ do
  qn <- genQueueName
  let configs = [standardQueue qn]
  runSession pool (ensureQueues configs)
  runSession pool (ensureQueues configs)
  queues <- runSession pool Sessions.listQueues
  let matching = filter (\q -> (q ^. #name) == qn) queues
  length matching @?= 1
  cleanupQueue pool qn

-- | Silent-variant version of 'testEnsureQueuesIncremental': after an initial
-- single-queue reconcile, a second reconcile that adds a new queue must leave
-- both queues in place.
testEnsureQueuesSilentIncremental :: Pool.Pool -> TestTree
testEnsureQueuesSilentIncremental pool = testCase "silent ensureQueues adds new queues incrementally" $ do
  qn1 <- genQueueName
  qn2 <- genQueueName
  runSession pool (ensureQueues [standardQueue qn1])
  runSession pool (ensureQueues [standardQueue qn1, standardQueue qn2])
  queues <- runSession pool Sessions.listQueues
  let names = map (^. #name) queues
  assertBool "qn1 should exist" (qn1 `elem` names)
  assertBool "qn2 should exist" (qn2 `elem` names)
  cleanupQueue pool qn1
  cleanupQueue pool qn2

-- | Assert that calling the silent 'ensureQueues' a second time for a queue with
-- notify-insert configured does NOT reset 'last_notified_at' to the epoch.
--
-- Under the pre-fix behaviour, 'ensureQueues' unconditionally calls
-- 'pgmq.enable_notify_insert', which first runs 'pgmq.disable_notify_insert'
-- (DELETE FROM pgmq.notify_insert_throttle) and then re-inserts the row with
-- the default 'last_notified_at = to_timestamp(0)'. After the fix, the second
-- call queries existing state and skips the notify mutation entirely, so a
-- timestamp bumped between the two calls is preserved.
testEnsureQueuesIsTrulyIdempotent :: Pool.Pool -> TestTree
testEnsureQueuesIsTrulyIdempotent pool = testCase "ensureQueues is truly idempotent for notify" $ do
  qn <- genQueueName
  let configs = [withNotifyInsert (Just 500) (standardQueue qn)]
  -- First run creates the queue and enables notify. Fresh throttle row has
  -- last_notified_at = to_timestamp(0) (the table default).
  runSession pool (ensureQueues configs)
  -- Bump last_notified_at to a recent timestamp so a reset would be observable.
  bumped <- runSession pool (Hasql.Session.statement (queueNameToText qn) bumpLastNotifiedAtStmt)
  assertBool "bumped timestamp should be post-epoch" (bumped > epochUtc)
  -- Second run must be a no-op for notify (i.e., must not rewrite the row).
  runSession pool (ensureQueues configs)
  after <- runSession pool (Hasql.Session.statement (queueNameToText qn) pgNotifyLastAt)
  assertBool
    ( "last_notified_at must not be reset by second ensureQueues; was "
        <> show after
    )
    (after > epochUtc)
  after @?= bumped
  cleanupQueue pool qn

-- | Read 'last_notified_at' for a queue. One-off statement for tests only.
pgNotifyLastAt :: Statement T.Text UTCTime
pgNotifyLastAt = preparable sql encoder decoder
  where
    sql = "SELECT last_notified_at FROM pgmq.notify_insert_throttle WHERE queue_name = $1"
    encoder = E.param (E.nonNullable E.text)
    decoder = D.singleRow (D.column (D.nonNullable D.timestamptz))

-- | Bump 'last_notified_at' to clock_timestamp() and return the new value.
-- Used to observe whether a subsequent ensureQueues resets it.
bumpLastNotifiedAtStmt :: Statement T.Text UTCTime
bumpLastNotifiedAtStmt = preparable sql encoder decoder
  where
    sql =
      "UPDATE pgmq.notify_insert_throttle \
      \SET last_notified_at = clock_timestamp() \
      \WHERE queue_name = $1 RETURNING last_notified_at"
    encoder = E.param (E.nonNullable E.text)
    decoder = D.singleRow (D.column (D.nonNullable D.timestamptz))

-- | Postgres epoch as a UTCTime, i.e. 'to_timestamp(0)'.
epochUtc :: UTCTime
epochUtc = read "1970-01-01 00:00:00 UTC"
