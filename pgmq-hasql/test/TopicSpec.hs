{-# LANGUAGE OverloadedStrings #-}

-- | Tests for topic routing operations (pgmq 1.11.0+):
-- - bind/unbind topic
-- - send_topic / send_topic with headers
-- - batch send_topic (immediate, delayed, with headers)
-- - validate_routing_key / validate_topic_pattern
-- - test_routing
-- - list_topic_bindings
-- - list_notify_insert_throttles / update_notify_insert
module TopicSpec (tests) where

import Data.Aeson (object, (.=))
import Data.Text qualified as T
import Data.Time.Clock (addUTCTime, getCurrentTime)
import EphemeralDb (TestFixture (..), withTestFixture)
import Hasql.Pool qualified as Pool
import Pgmq.Hasql.Sessions qualified as Sessions
import Pgmq.Hasql.Statements.Types
  ( BatchSendTopic (..),
    BatchSendTopicForLater (..),
    BatchSendTopicWithHeaders (..),
    BatchSendTopicWithHeadersForLater (..),
    BindTopic (..),
    EnableNotifyInsert (..),
    SendTopic (..),
    SendTopicWithHeaders (..),
    UnbindTopic (..),
    UpdateNotifyInsert (..),
  )
import Pgmq.Types
  ( MessageBody (..),
    MessageHeaders (..),
    QueueName,
    RoutingKey,
    TopicPattern,
    parseRoutingKey,
    parseTopicPattern,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import TestUtils (assertSession, cleanupQueue)

-- | All topic operation tests
tests :: Pool.Pool -> TestTree
tests p =
  testGroup
    "Topic Operations (pgmq 1.11.0+)"
    [ testBindUnbindTopic p,
      testSendTopic p,
      testSendTopicWithHeaders p,
      testBatchSendTopic p,
      testBatchSendTopicForLater p,
      testBatchSendTopicWithHeaders p,
      testBatchSendTopicWithHeadersForLater p,
      testTestRouting p,
      testValidateRoutingKey p,
      testValidateTopicPattern p,
      testListTopicBindings p,
      testListTopicBindingsForQueue p,
      testNotifyInsertThrottle p
    ]

mkRoutingKey :: T.Text -> RoutingKey
mkRoutingKey t = case parseRoutingKey t of
  Left err -> error $ "Invalid routing key: " <> show err
  Right rk -> rk

mkTopicPattern :: T.Text -> TopicPattern
mkTopicPattern t = case parseTopicPattern t of
  Left err -> error $ "Invalid topic pattern: " <> show err
  Right tp -> tp

-- | Helper to unbind a pattern and then cleanup the queue
unbindAndCleanup :: Pool.Pool -> TopicPattern -> QueueName -> IO ()
unbindAndCleanup pool pat qn = do
  _ <-
    assertSession pool $
      Sessions.unbindTopic
        UnbindTopic {topicPattern = pat, queueName = qn}
  cleanupQueue pool qn

-- | Test bind and unbind topic
testBindUnbindTopic :: Pool.Pool -> TestTree
testBindUnbindTopic p = testCase "bindTopic and unbindTopic" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    -- Bind a pattern
    let pat = mkTopicPattern "bind-test.#"
    assertSession pool $
      Sessions.bindTopic
        BindTopic {topicPattern = pat, queueName = queueName}
    -- Verify binding exists for this queue
    bindings <- assertSession pool $ Sessions.listTopicBindingsForQueue queueName
    assertEqual "Should have 1 binding for queue" 1 (length bindings)
    -- Unbind
    removed <-
      assertSession pool $
        Sessions.unbindTopic
          UnbindTopic {topicPattern = pat, queueName = queueName}
    assertBool "Unbind should return True" removed
    -- Unbind again should return False
    removedAgain <-
      assertSession pool $
        Sessions.unbindTopic
          UnbindTopic {topicPattern = pat, queueName = queueName}
    assertBool "Second unbind should return False" (not removedAgain)
    cleanupQueue pool queueName

-- | Test sendTopic routes to matching queues
testSendTopic :: Pool.Pool -> TestTree
testSendTopic p = testCase "sendTopic routes message to matching queues" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    -- Use a unique pattern prefix to avoid matching stale bindings
    let pat = mkTopicPattern "sendtest.#"
    assertSession pool $
      Sessions.bindTopic
        BindTopic {topicPattern = pat, queueName = queueName}
    -- Send via topic with routing key matching only our pattern
    matchedCount <-
      assertSession pool $
        Sessions.sendTopic
          SendTopic
            { routingKey = mkRoutingKey "sendtest.error",
              messageBody = MessageBody (object ["level" .= ("error" :: String)]),
              delay = Nothing
            }
    assertEqual "Should match 1 queue" 1 matchedCount
    unbindAndCleanup pool pat queueName

-- | Test sendTopicWithHeaders
testSendTopicWithHeaders :: Pool.Pool -> TestTree
testSendTopicWithHeaders p = testCase "sendTopicWithHeaders routes with headers" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    let pat = mkTopicPattern "hdrstest.#"
    assertSession pool $
      Sessions.bindTopic
        BindTopic {topicPattern = pat, queueName = queueName}
    matchedCount <-
      assertSession pool $
        Sessions.sendTopicWithHeaders
          SendTopicWithHeaders
            { routingKey = mkRoutingKey "hdrstest.user.login",
              messageBody = MessageBody (object ["event" .= ("login" :: String)]),
              messageHeaders = MessageHeaders (object ["source" .= ("auth" :: String)]),
              delay = Nothing
            }
    assertEqual "Should match 1 queue" 1 matchedCount
    unbindAndCleanup pool pat queueName

-- | Test batchSendTopic
testBatchSendTopic :: Pool.Pool -> TestTree
testBatchSendTopic p = testCase "batchSendTopic routes batch to matching queues" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    let pat = mkTopicPattern "batchtest.*"
    assertSession pool $
      Sessions.bindTopic
        BindTopic {topicPattern = pat, queueName = queueName}
    results <-
      assertSession pool $
        Sessions.batchSendTopic
          BatchSendTopic
            { routingKey = mkRoutingKey "batchtest.error",
              messageBodies =
                [ MessageBody (object ["msg" .= (1 :: Int)]),
                  MessageBody (object ["msg" .= (2 :: Int)])
                ],
              delay = Nothing
            }
    assertEqual "Should return 2 results (2 msgs to 1 queue)" 2 (length results)
    unbindAndCleanup pool pat queueName

-- | Test batchSendTopicForLater with a scheduled time
testBatchSendTopicForLater :: Pool.Pool -> TestTree
testBatchSendTopicForLater p = testCase "batchSendTopicForLater schedules batch via topic" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    let pat = mkTopicPattern "batchlater.*"
    assertSession pool $
      Sessions.bindTopic
        BindTopic {topicPattern = pat, queueName = queueName}
    futureTime <- addUTCTime 60 <$> getCurrentTime
    results <-
      assertSession pool $
        Sessions.batchSendTopicForLater
          BatchSendTopicForLater
            { routingKey = mkRoutingKey "batchlater.info",
              messageBodies =
                [ MessageBody (object ["msg" .= (1 :: Int)]),
                  MessageBody (object ["msg" .= (2 :: Int)])
                ],
              scheduledAt = futureTime
            }
    assertEqual "Should return 2 results (2 msgs to 1 queue)" 2 (length results)
    unbindAndCleanup pool pat queueName

-- | Test batchSendTopicWithHeaders
testBatchSendTopicWithHeaders :: Pool.Pool -> TestTree
testBatchSendTopicWithHeaders p = testCase "batchSendTopicWithHeaders routes batch with headers" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    let pat = mkTopicPattern "batchhdrs.*"
    assertSession pool $
      Sessions.bindTopic
        BindTopic {topicPattern = pat, queueName = queueName}
    results <-
      assertSession pool $
        Sessions.batchSendTopicWithHeaders
          BatchSendTopicWithHeaders
            { routingKey = mkRoutingKey "batchhdrs.warn",
              messageBodies =
                [ MessageBody (object ["msg" .= (1 :: Int)]),
                  MessageBody (object ["msg" .= (2 :: Int)])
                ],
              messageHeaders =
                [ MessageHeaders (object ["priority" .= ("high" :: String)]),
                  MessageHeaders (object ["priority" .= ("low" :: String)])
                ],
              delay = Nothing
            }
    assertEqual "Should return 2 results (2 msgs to 1 queue)" 2 (length results)
    unbindAndCleanup pool pat queueName

-- | Test batchSendTopicWithHeadersForLater
testBatchSendTopicWithHeadersForLater :: Pool.Pool -> TestTree
testBatchSendTopicWithHeadersForLater p = testCase "batchSendTopicWithHeadersForLater schedules batch with headers" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    let pat = mkTopicPattern "batchhdrslater.*"
    assertSession pool $
      Sessions.bindTopic
        BindTopic {topicPattern = pat, queueName = queueName}
    futureTime <- addUTCTime 60 <$> getCurrentTime
    results <-
      assertSession pool $
        Sessions.batchSendTopicWithHeadersForLater
          BatchSendTopicWithHeadersForLater
            { routingKey = mkRoutingKey "batchhdrslater.error",
              messageBodies =
                [ MessageBody (object ["msg" .= (1 :: Int)]),
                  MessageBody (object ["msg" .= (2 :: Int)])
                ],
              messageHeaders =
                [ MessageHeaders (object ["source" .= ("app" :: String)]),
                  MessageHeaders (object ["source" .= ("sys" :: String)])
                ],
              scheduledAt = futureTime
            }
    assertEqual "Should return 2 results (2 msgs to 1 queue)" 2 (length results)
    unbindAndCleanup pool pat queueName

-- | Test validateRoutingKey
testValidateRoutingKey :: Pool.Pool -> TestTree
testValidateRoutingKey p = testCase "validateRoutingKey validates on server" $ do
  withTestFixture p $ \TestFixture {pool} -> do
    result <- assertSession pool $ Sessions.validateRoutingKey (mkRoutingKey "orders.created")
    assertBool "Valid routing key should return True" result

-- | Test validateTopicPattern
testValidateTopicPattern :: Pool.Pool -> TestTree
testValidateTopicPattern p = testCase "validateTopicPattern validates on server" $ do
  withTestFixture p $ \TestFixture {pool} -> do
    result <- assertSession pool $ Sessions.validateTopicPattern (mkTopicPattern "orders.#")
    assertBool "Valid topic pattern should return True" result

-- | Test testRouting dry-run
testTestRouting :: Pool.Pool -> TestTree
testTestRouting p = testCase "testRouting shows matching patterns" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    -- Bind two patterns that both match "routetest.error"
    let pat1 = mkTopicPattern "routetest.#"
        pat2 = mkTopicPattern "routetest.*"
    assertSession pool $
      Sessions.bindTopic
        BindTopic {topicPattern = pat1, queueName = queueName}
    assertSession pool $
      Sessions.bindTopic
        BindTopic {topicPattern = pat2, queueName = queueName}
    -- Test routing - both patterns should match
    matches <- assertSession pool $ Sessions.testRouting (mkRoutingKey "routetest.error")
    assertEqual "Should match 2 patterns" 2 (length matches)
    -- Cleanup
    _ <-
      assertSession pool $
        Sessions.unbindTopic
          UnbindTopic {topicPattern = pat1, queueName = queueName}
    unbindAndCleanup pool pat2 queueName

-- | Test listTopicBindings
testListTopicBindings :: Pool.Pool -> TestTree
testListTopicBindings p = testCase "listTopicBindings returns all bindings" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    let pat = mkTopicPattern "listtest.#"
    assertSession pool $
      Sessions.bindTopic
        BindTopic {topicPattern = pat, queueName = queueName}
    bindings <- assertSession pool Sessions.listTopicBindings
    assertBool "Should have at least 1 binding" (not $ null bindings)
    unbindAndCleanup pool pat queueName

-- | Test listTopicBindingsForQueue
testListTopicBindingsForQueue :: Pool.Pool -> TestTree
testListTopicBindingsForQueue p = testCase "listTopicBindingsForQueue returns queue-specific bindings" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    let pat1 = mkTopicPattern "listqtest.events.#"
        pat2 = mkTopicPattern "listqtest.logs.*"
    assertSession pool $
      Sessions.bindTopic
        BindTopic {topicPattern = pat1, queueName = queueName}
    assertSession pool $
      Sessions.bindTopic
        BindTopic {topicPattern = pat2, queueName = queueName}
    bindings <- assertSession pool $ Sessions.listTopicBindingsForQueue queueName
    assertEqual "Should have 2 bindings for queue" 2 (length bindings)
    -- Cleanup
    _ <-
      assertSession pool $
        Sessions.unbindTopic
          UnbindTopic {topicPattern = pat1, queueName = queueName}
    unbindAndCleanup pool pat2 queueName

-- | Test listNotifyInsertThrottles and updateNotifyInsert
testNotifyInsertThrottle :: Pool.Pool -> TestTree
testNotifyInsertThrottle p = testCase "listNotifyInsertThrottles and updateNotifyInsert" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    -- Enable notifications first
    assertSession pool $
      Sessions.enableNotifyInsert
        EnableNotifyInsert {queueName = queueName, throttleIntervalMs = Just 100}
    -- List throttles
    throttles <- assertSession pool Sessions.listNotifyInsertThrottles
    assertBool "Should have at least 1 throttle entry" (not $ null throttles)
    -- Update throttle
    assertSession pool $
      Sessions.updateNotifyInsert
        UpdateNotifyInsert {queueName = queueName, throttleIntervalMs = 500}
    -- Verify update
    throttlesAfter <- assertSession pool Sessions.listNotifyInsertThrottles
    assertBool "Should still have throttle entries" (not $ null throttlesAfter)
    cleanupQueue pool queueName
