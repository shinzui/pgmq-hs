{-# LANGUAGE OverloadedStrings #-}

-- | Tests for advanced message operations:
-- - pop with qty
-- - batch set_vt
-- - read_with_poll
-- - FIFO index functions
-- - read_grouped functions
module AdvancedOpsSpec (tests) where

import Control.Concurrent (threadDelay)
import Data.Aeson (object, (.=))
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Data.Vector qualified as V
import Hasql.Pool qualified as Pool
import Pgmq.Hasql.Sessions qualified as Sessions
import Pgmq.Hasql.Statements.Types
  ( BatchMessageQuery (..),
    BatchSendMessage (..),
    BatchSendMessageWithHeaders (..),
    BatchVisibilityTimeoutAtQuery (..),
    BatchVisibilityTimeoutQuery (..),
    PopMessage (..),
    ReadGrouped (..),
    ReadGroupedWithPoll (..),
    ReadMessage (..),
    ReadWithPollMessage (..),
    SendMessage (..),
    VisibilityTimeoutAtQuery (..),
  )
import Pgmq.Types (MessageBody (..), MessageHeaders (..), MessageId (..))
import Pgmq.Types qualified as PgmqTypes
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import TestUtils (assertSession, cleanupQueue)
import TmpPostgres (TestFixture (..), withTestFixture)

-- | All advanced operation tests
tests :: Pool.Pool -> TestTree
tests p =
  testGroup
    "Advanced Operations"
    [ testPopSingle p,
      testPopBatch p,
      testBatchChangeVisibilityTimeout p,
      testSetVisibilityTimeoutAt p,
      testBatchSetVisibilityTimeoutAt p,
      testReadWithPoll p,
      testReadWithPollEmpty p,
      testCreateFifoIndex p,
      testReadGrouped p,
      testReadGroupedRoundRobin p
    ]

-- | Test pop with default qty (single message)
testPopSingle :: Pool.Pool -> TestTree
testPopSingle p = testCase "pop returns and deletes single message" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    -- Send messages
    let msgs =
          BatchSendMessage
            { queueName = queueName,
              messageBodies =
                [ MessageBody (object ["pop" .= (1 :: Int)]),
                  MessageBody (object ["pop" .= (2 :: Int)])
                ],
              delay = Nothing
            }
    _ <- assertSession pool (Sessions.batchSendMessage msgs)
    -- Pop single message
    let popQuery = PopMessage {queueName = queueName, qty = Just 1}
    popped <- assertSession pool (Sessions.pop popQuery)
    assertEqual "Should pop 1 message" 1 (V.length popped)
    -- Verify only 1 message remains
    let readQuery =
          ReadMessage
            { queueName = queueName,
              delay = 30,
              batchSize = Just 10,
              conditional = Nothing
            }
    remaining <- assertSession pool (Sessions.readMessage readQuery)
    assertEqual "Should have 1 message remaining" 1 (V.length remaining)
    cleanupQueue pool queueName

-- | Test pop with qty > 1 (batch pop)
testPopBatch :: Pool.Pool -> TestTree
testPopBatch p = testCase "pop with qty > 1 returns multiple messages" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    -- Send 5 messages
    let msgs =
          BatchSendMessage
            { queueName = queueName,
              messageBodies = [MessageBody (object ["pop" .= i]) | i <- [1 .. 5 :: Int]],
              delay = Nothing
            }
    _ <- assertSession pool (Sessions.batchSendMessage msgs)
    -- Pop 3 messages
    let popQuery = PopMessage {queueName = queueName, qty = Just 3}
    popped <- assertSession pool (Sessions.pop popQuery)
    assertEqual "Should pop 3 messages" 3 (V.length popped)
    -- Verify 2 messages remain
    let readQuery =
          ReadMessage
            { queueName = queueName,
              delay = 30,
              batchSize = Just 10,
              conditional = Nothing
            }
    remaining <- assertSession pool (Sessions.readMessage readQuery)
    assertEqual "Should have 2 messages remaining" 2 (V.length remaining)
    cleanupQueue pool queueName

-- | Test batchChangeVisibilityTimeout
testBatchChangeVisibilityTimeout :: Pool.Pool -> TestTree
testBatchChangeVisibilityTimeout p = testCase "batchChangeVisibilityTimeout updates multiple messages" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    -- Send messages
    let msgs =
          BatchSendMessage
            { queueName = queueName,
              messageBodies =
                [ MessageBody (object ["vt" .= (1 :: Int)]),
                  MessageBody (object ["vt" .= (2 :: Int)]),
                  MessageBody (object ["vt" .= (3 :: Int)])
                ],
              delay = Nothing
            }
    msgIds <- assertSession pool (Sessions.batchSendMessage msgs)
    -- Read messages to set VT
    let readQuery =
          ReadMessage
            { queueName = queueName,
              delay = 5,
              batchSize = Just 10,
              conditional = Nothing
            }
    _ <- assertSession pool (Sessions.readMessage readQuery)
    -- Change VT for all messages
    let vtQuery =
          BatchVisibilityTimeoutQuery
            { queueName = queueName,
              messageIds = msgIds,
              visibilityTimeoutOffset = 60
            }
    updated <- assertSession pool (Sessions.batchChangeVisibilityTimeout vtQuery)
    assertEqual "Should update 3 messages" 3 (V.length updated)
    cleanupQueue pool queueName

-- | Test setVisibilityTimeoutAt (pgmq 1.10.0+)
testSetVisibilityTimeoutAt :: Pool.Pool -> TestTree
testSetVisibilityTimeoutAt p = testCase "setVisibilityTimeoutAt sets VT to absolute timestamp" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    -- Send a message
    let msg =
          SendMessage
            { queueName = queueName,
              messageBody = MessageBody (object ["vt_at" .= ("test" :: String)]),
              delay = Nothing
            }
    msgId <- assertSession pool (Sessions.sendMessage msg)
    -- Read message to set initial VT
    let readQuery =
          ReadMessage
            { queueName = queueName,
              delay = 5,
              batchSize = Just 1,
              conditional = Nothing
            }
    _ <- assertSession pool (Sessions.readMessage readQuery)
    -- Set VT to 60 seconds in the future using absolute timestamp
    futureTime <- addUTCTime 60 <$> getCurrentTime
    let vtQuery =
          VisibilityTimeoutAtQuery
            { queueName = queueName,
              messageId = msgId,
              visibilityTime = futureTime
            }
    updated <- assertSession pool (Sessions.setVisibilityTimeoutAt vtQuery)
    assertEqual "Should return the updated message" msgId (PgmqTypes.messageId updated)
    cleanupQueue pool queueName

-- | Test batchSetVisibilityTimeoutAt (pgmq 1.10.0+)
testBatchSetVisibilityTimeoutAt :: Pool.Pool -> TestTree
testBatchSetVisibilityTimeoutAt p = testCase "batchSetVisibilityTimeoutAt sets VT for multiple messages" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    -- Send messages
    let msgs =
          BatchSendMessage
            { queueName = queueName,
              messageBodies =
                [ MessageBody (object ["vt_at" .= (1 :: Int)]),
                  MessageBody (object ["vt_at" .= (2 :: Int)]),
                  MessageBody (object ["vt_at" .= (3 :: Int)])
                ],
              delay = Nothing
            }
    msgIds <- assertSession pool (Sessions.batchSendMessage msgs)
    -- Read messages to set initial VT
    let readQuery =
          ReadMessage
            { queueName = queueName,
              delay = 5,
              batchSize = Just 10,
              conditional = Nothing
            }
    _ <- assertSession pool (Sessions.readMessage readQuery)
    -- Set VT to 120 seconds in the future using absolute timestamp
    futureTime <- addUTCTime 120 <$> getCurrentTime
    let vtQuery =
          BatchVisibilityTimeoutAtQuery
            { queueName = queueName,
              messageIds = msgIds,
              visibilityTime = futureTime
            }
    updated <- assertSession pool (Sessions.batchSetVisibilityTimeoutAt vtQuery)
    assertEqual "Should update 3 messages" 3 (V.length updated)
    cleanupQueue pool queueName

-- | Test readWithPoll - messages available immediately
testReadWithPoll :: Pool.Pool -> TestTree
testReadWithPoll p = testCase "readWithPoll returns messages immediately when available" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    -- Send a message
    let msg =
          SendMessage
            { queueName = queueName,
              messageBody = MessageBody (object ["poll" .= ("test" :: String)]),
              delay = Nothing
            }
    _ <- assertSession pool (Sessions.sendMessage msg)
    -- Poll for messages
    let pollQuery =
          ReadWithPollMessage
            { queueName = queueName,
              delay = 30,
              batchSize = Just 1,
              maxPollSeconds = 5,
              pollIntervalMs = 100,
              conditional = Nothing
            }
    messages <- assertSession pool (Sessions.readWithPoll pollQuery)
    assertEqual "Should read 1 message" 1 (V.length messages)
    cleanupQueue pool queueName

-- | Test readWithPoll - empty queue with short timeout
testReadWithPollEmpty :: Pool.Pool -> TestTree
testReadWithPollEmpty p = testCase "readWithPoll returns empty when queue is empty" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    -- Poll on empty queue with short timeout
    let pollQuery =
          ReadWithPollMessage
            { queueName = queueName,
              delay = 30,
              batchSize = Just 1,
              maxPollSeconds = 1, -- Short timeout
              pollIntervalMs = 100,
              conditional = Nothing
            }
    messages <- assertSession pool (Sessions.readWithPoll pollQuery)
    assertEqual "Should return empty" 0 (V.length messages)
    cleanupQueue pool queueName

-- | Test createFifoIndex
testCreateFifoIndex :: Pool.Pool -> TestTree
testCreateFifoIndex p = testCase "createFifoIndex creates GIN index on headers" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    -- Create FIFO index
    assertSession pool (Sessions.createFifoIndex queueName)
    -- Verify by sending a message with headers (index should be used)
    let msg =
          BatchSendMessageWithHeaders
            { queueName = queueName,
              messageBodies = [MessageBody (object ["fifo" .= ("test" :: String)])],
              messageHeaders = [MessageHeaders (object ["x-pgmq-group" .= ("group1" :: String)])],
              delay = Nothing
            }
    msgIds <- assertSession pool (Sessions.batchSendMessageWithHeaders msg)
    assertEqual "Should send 1 message" 1 (length msgIds)
    cleanupQueue pool queueName

-- | Test readGrouped - fills batch from same message group
testReadGrouped :: Pool.Pool -> TestTree
testReadGrouped p = testCase "readGrouped fills batch from same message group" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    -- Create FIFO index for better performance
    assertSession pool (Sessions.createFifoIndex queueName)
    -- Send messages from multiple groups
    let msgs =
          BatchSendMessageWithHeaders
            { queueName = queueName,
              messageBodies =
                [ MessageBody (object ["msg" .= (1 :: Int)]),
                  MessageBody (object ["msg" .= (2 :: Int)]),
                  MessageBody (object ["msg" .= (3 :: Int)]),
                  MessageBody (object ["msg" .= (4 :: Int)])
                ],
              messageHeaders =
                [ MessageHeaders (object ["x-pgmq-group" .= ("groupA" :: String)]),
                  MessageHeaders (object ["x-pgmq-group" .= ("groupA" :: String)]),
                  MessageHeaders (object ["x-pgmq-group" .= ("groupB" :: String)]),
                  MessageHeaders (object ["x-pgmq-group" .= ("groupB" :: String)])
                ],
              delay = Nothing
            }
    _ <- assertSession pool (Sessions.batchSendMessageWithHeaders msgs)
    -- Read grouped - should prefer filling from one group
    let readQuery =
          ReadGrouped
            { queueName = queueName,
              visibilityTimeout = 30,
              qty = 2
            }
    messages <- assertSession pool (Sessions.readGrouped readQuery)
    assertEqual "Should read 2 messages" 2 (V.length messages)
    -- Delete the messages
    let msgIds = V.toList $ V.map PgmqTypes.messageId messages
    _ <-
      assertSession pool $
        Sessions.batchDeleteMessages
          BatchMessageQuery {queueName = queueName, messageIds = msgIds}
    cleanupQueue pool queueName

-- | Test readGroupedRoundRobin - interleaves across groups
testReadGroupedRoundRobin :: Pool.Pool -> TestTree
testReadGroupedRoundRobin p = testCase "readGroupedRoundRobin interleaves across groups" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    -- Create FIFO index
    assertSession pool (Sessions.createFifoIndex queueName)
    -- Send messages from 3 groups
    let msgs =
          BatchSendMessageWithHeaders
            { queueName = queueName,
              messageBodies =
                [ MessageBody (object ["msg" .= (1 :: Int)]),
                  MessageBody (object ["msg" .= (2 :: Int)]),
                  MessageBody (object ["msg" .= (3 :: Int)]),
                  MessageBody (object ["msg" .= (4 :: Int)]),
                  MessageBody (object ["msg" .= (5 :: Int)]),
                  MessageBody (object ["msg" .= (6 :: Int)])
                ],
              messageHeaders =
                [ MessageHeaders (object ["x-pgmq-group" .= ("groupA" :: String)]),
                  MessageHeaders (object ["x-pgmq-group" .= ("groupB" :: String)]),
                  MessageHeaders (object ["x-pgmq-group" .= ("groupC" :: String)]),
                  MessageHeaders (object ["x-pgmq-group" .= ("groupA" :: String)]),
                  MessageHeaders (object ["x-pgmq-group" .= ("groupB" :: String)]),
                  MessageHeaders (object ["x-pgmq-group" .= ("groupC" :: String)])
                ],
              delay = Nothing
            }
    _ <- assertSession pool (Sessions.batchSendMessageWithHeaders msgs)
    -- Read round-robin - should interleave groups
    let readQuery =
          ReadGrouped
            { queueName = queueName,
              visibilityTimeout = 30,
              qty = 3
            }
    messages <- assertSession pool (Sessions.readGroupedRoundRobin readQuery)
    assertEqual "Should read 3 messages" 3 (V.length messages)
    -- Clean up
    let msgIds = V.toList $ V.map PgmqTypes.messageId messages
    _ <-
      assertSession pool $
        Sessions.batchDeleteMessages
          BatchMessageQuery {queueName = queueName, messageIds = msgIds}
    cleanupQueue pool queueName
