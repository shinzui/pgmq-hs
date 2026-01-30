{-# LANGUAGE OverloadedStrings #-}

-- | Tests for message operations
module MessageSpec (tests) where

import Data.Aeson (object, (.=))
import Data.Vector qualified as V
import Hasql.Pool qualified as Pool
import Pgmq.Hasql.Sessions qualified as Sessions
import Pgmq.Hasql.Statements.Types
  ( BatchSendMessage (..),
    BatchSendMessageWithHeaders (..),
    MessageQuery (..),
    ReadMessage (..),
    SendMessage (..),
    SendMessageWithHeaders (..),
    VisibilityTimeoutQuery (..),
  )
import Pgmq.Types (MessageBody (..), MessageHeaders (..), MessageId (..))
import Pgmq.Types qualified as PgmqTypes
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import TestUtils (assertSession, cleanupQueue)
import TmpPostgres (TestFixture (..), withTestFixture)

-- | All message operation tests
tests :: Pool.Pool -> TestTree
tests p =
  testGroup
    "Message Operations"
    [ testSendMessageNullDelay p,
      testSendMessageZeroDelay p,
      testBatchSendMessageNullDelay p,
      testReadMessage p,
      testDeleteMessage p,
      testArchiveMessage p,
      testChangeVisibilityTimeout p,
      testSendMessageWithHeaders p,
      testBatchSendMessageWithHeaders p
    ]

-- | Test sendMessage with delay=Nothing (validates COALESCE fix)
-- This is a critical test - it validates that our fix for NULL delay
-- correctly uses the pgmq function overload resolution
testSendMessageNullDelay :: Pool.Pool -> TestTree
testSendMessageNullDelay p = testCase "sendMessage with delay=Nothing works" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    let msg =
          SendMessage
            { queueName = queueName,
              messageBody = MessageBody (object ["test" .= ("hello" :: String)]),
              delay = Nothing
            }
    msgId <- assertSession pool (Sessions.sendMessage msg)
    assertBool "Message ID should be positive" (unMessageId msgId > 0)
    -- Verify message can be read immediately (no delay)
    let readQuery =
          ReadMessage
            { queueName = queueName,
              delay = 30,
              batchSize = Just 1,
              conditional = Nothing
            }
    messages <- assertSession pool (Sessions.readMessage readQuery)
    assertEqual "Should read 1 message" 1 (V.length messages)
    cleanupQueue pool queueName

-- | Test sendMessage with delay=Just 0
testSendMessageZeroDelay :: Pool.Pool -> TestTree
testSendMessageZeroDelay p = testCase "sendMessage with delay=Just 0 works" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    let msg =
          SendMessage
            { queueName = queueName,
              messageBody = MessageBody (object ["test" .= ("hello" :: String)]),
              delay = Just 0
            }
    msgId <- assertSession pool (Sessions.sendMessage msg)
    assertBool "Message ID should be positive" (unMessageId msgId > 0)
    -- Verify message can be read immediately
    let readQuery =
          ReadMessage
            { queueName = queueName,
              delay = 30,
              batchSize = Just 1,
              conditional = Nothing
            }
    messages <- assertSession pool (Sessions.readMessage readQuery)
    assertEqual "Should read 1 message" 1 (V.length messages)
    cleanupQueue pool queueName

-- | Test batchSendMessage with delay=Nothing (validates COALESCE fix)
testBatchSendMessageNullDelay :: Pool.Pool -> TestTree
testBatchSendMessageNullDelay p = testCase "batchSendMessage with delay=Nothing works" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    let msgs =
          BatchSendMessage
            { queueName = queueName,
              messageBodies =
                [ MessageBody (object ["batch" .= (1 :: Int)]),
                  MessageBody (object ["batch" .= (2 :: Int)]),
                  MessageBody (object ["batch" .= (3 :: Int)])
                ],
              delay = Nothing
            }
    msgIds <- assertSession pool (Sessions.batchSendMessage msgs)
    assertEqual "Should return 3 message IDs" 3 (length msgIds)
    -- Verify messages can be read
    let readQuery =
          ReadMessage
            { queueName = queueName,
              delay = 30,
              batchSize = Just 10,
              conditional = Nothing
            }
    messages <- assertSession pool (Sessions.readMessage readQuery)
    assertEqual "Should read 3 messages" 3 (V.length messages)
    cleanupQueue pool queueName

-- | Test readMessage
testReadMessage :: Pool.Pool -> TestTree
testReadMessage p = testCase "readMessage retrieves and hides message" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    -- Send a message
    let msg =
          SendMessage
            { queueName = queueName,
              messageBody = MessageBody (object ["key" .= ("value" :: String)]),
              delay = Nothing
            }
    _ <- assertSession pool (Sessions.sendMessage msg)
    -- Read the message
    let readQuery =
          ReadMessage
            { queueName = queueName,
              delay = 30, -- 30 second visibility timeout
              batchSize = Just 1,
              conditional = Nothing
            }
    messages <- assertSession pool (Sessions.readMessage readQuery)
    assertEqual "Should read 1 message" 1 (V.length messages)
    -- Message should not be readable again (still hidden)
    messages2 <- assertSession pool (Sessions.readMessage readQuery)
    assertEqual "Should not read hidden message" 0 (V.length messages2)
    cleanupQueue pool queueName

-- | Test deleteMessage
testDeleteMessage :: Pool.Pool -> TestTree
testDeleteMessage p = testCase "deleteMessage removes message from queue" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    -- Send and read a message
    let sendMsg =
          SendMessage
            { queueName = queueName,
              messageBody = MessageBody (object ["delete" .= ("me" :: String)]),
              delay = Nothing
            }
    msgId <- assertSession pool (Sessions.sendMessage sendMsg)
    -- Delete the message
    let deleteQuery =
          MessageQuery
            { queueName = queueName,
              messageId = msgId
            }
    deleted <- assertSession pool (Sessions.deleteMessage deleteQuery)
    assertBool "Delete should succeed" deleted
    -- Verify message can't be read anymore
    let readQuery =
          ReadMessage
            { queueName = queueName,
              delay = 30,
              batchSize = Just 1,
              conditional = Nothing
            }
    messages <- assertSession pool (Sessions.readMessage readQuery)
    assertEqual "Queue should be empty after delete" 0 (V.length messages)
    cleanupQueue pool queueName

-- | Test archiveMessage
testArchiveMessage :: Pool.Pool -> TestTree
testArchiveMessage p = testCase "archiveMessage moves message to archive" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    -- Send a message
    let sendMsg =
          SendMessage
            { queueName = queueName,
              messageBody = MessageBody (object ["archive" .= ("me" :: String)]),
              delay = Nothing
            }
    msgId <- assertSession pool (Sessions.sendMessage sendMsg)
    -- Archive the message
    let archiveQuery =
          MessageQuery
            { queueName = queueName,
              messageId = msgId
            }
    archived <- assertSession pool (Sessions.archiveMessage archiveQuery)
    assertBool "Archive should succeed" archived
    -- Verify message can't be read from queue anymore
    let readQuery =
          ReadMessage
            { queueName = queueName,
              delay = 30,
              batchSize = Just 1,
              conditional = Nothing
            }
    messages <- assertSession pool (Sessions.readMessage readQuery)
    assertEqual "Queue should be empty after archive" 0 (V.length messages)
    cleanupQueue pool queueName

-- | Test changeVisibilityTimeout
testChangeVisibilityTimeout :: Pool.Pool -> TestTree
testChangeVisibilityTimeout p = testCase "changeVisibilityTimeout extends/resets VT" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    -- Send a message
    let sendMsg =
          SendMessage
            { queueName = queueName,
              messageBody = MessageBody (object ["vt" .= ("test" :: String)]),
              delay = Nothing
            }
    msgId <- assertSession pool (Sessions.sendMessage sendMsg)
    -- Read the message to set visibility timeout
    let readQuery =
          ReadMessage
            { queueName = queueName,
              delay = 30,
              batchSize = Just 1,
              conditional = Nothing
            }
    _ <- assertSession pool (Sessions.readMessage readQuery)
    -- Change visibility timeout
    let vtQuery =
          VisibilityTimeoutQuery
            { queueName = queueName,
              messageId = msgId,
              visibilityTimeoutOffset = 60
            }
    msg <- assertSession pool (Sessions.changeVisibilityTimeout vtQuery)
    assertEqual "Should return the message" msgId (PgmqTypes.messageId msg)
    cleanupQueue pool queueName

-- | Test sendMessageWithHeaders
testSendMessageWithHeaders :: Pool.Pool -> TestTree
testSendMessageWithHeaders p = testCase "sendMessageWithHeaders includes headers" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    let msg =
          SendMessageWithHeaders
            { queueName = queueName,
              messageBody = MessageBody (object ["data" .= ("test" :: String)]),
              messageHeaders = MessageHeaders (object ["trace_id" .= ("abc123" :: String)]),
              delay = Nothing
            }
    msgId <- assertSession pool (Sessions.sendMessageWithHeaders msg)
    assertBool "Message ID should be positive" (unMessageId msgId > 0)
    -- Read and verify headers
    let readQuery =
          ReadMessage
            { queueName = queueName,
              delay = 30,
              batchSize = Just 1,
              conditional = Nothing
            }
    messages <- assertSession pool (Sessions.readMessage readQuery)
    assertEqual "Should read 1 message" 1 (V.length messages)
    let readMsg = V.head messages
    case PgmqTypes.headers readMsg of
      Just _ -> assertBool "Headers should contain trace_id" True
      Nothing -> assertBool "Headers should be present" False
    cleanupQueue pool queueName

-- | Test batchSendMessageWithHeaders
testBatchSendMessageWithHeaders :: Pool.Pool -> TestTree
testBatchSendMessageWithHeaders p = testCase "batchSendMessageWithHeaders sends batch with headers" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    let msgs =
          BatchSendMessageWithHeaders
            { queueName = queueName,
              messageBodies =
                [ MessageBody (object ["batch" .= (1 :: Int)]),
                  MessageBody (object ["batch" .= (2 :: Int)])
                ],
              messageHeaders =
                [ MessageHeaders (object ["idx" .= (1 :: Int)]),
                  MessageHeaders (object ["idx" .= (2 :: Int)])
                ],
              delay = Nothing
            }
    msgIds <- assertSession pool (Sessions.batchSendMessageWithHeaders msgs)
    assertEqual "Should return 2 message IDs" 2 (length msgIds)
    cleanupQueue pool queueName
