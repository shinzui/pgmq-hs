{-# LANGUAGE OverloadedStrings #-}

-- | Field-specific decoder validation tests
-- These tests verify each decoded field contains semantically correct values
-- This catches column swap bugs (e.g., reading read_ct into msg_id)
module DecoderValidationSpec (tests) where

import Data.Aeson (Value, object, (.=))
import Data.Time (addUTCTime, getCurrentTime)
import Data.Vector qualified as V
import EphemeralDb (TestFixture (..), withTestFixture)
import Hasql.Pool qualified as Pool
import Hasql.Session (Session)
import Pgmq.Hasql.Sessions qualified as Sessions
import Pgmq.Hasql.Statements.Types
  ( ReadMessage (..),
    SendMessage (..),
    SendMessageWithHeaders (..),
    VisibilityTimeoutQuery (..),
  )
import Pgmq.Types
  ( Message (..),
    MessageBody (..),
    MessageHeaders (..),
    QueueName,
    unMessageId,
  )
import Pgmq.Types qualified
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import TestUtils (assertSession, cleanupQueue)

-- | All decoder validation tests
tests :: Pool.Pool -> TestTree
tests p =
  testGroup
    "Decoder Field Validation"
    [ testMessageIdIsPositive p,
      testReadCountIncrements p,
      testEnqueuedAtBeforeRead p,
      testLastReadAtUpdates p,
      testVisibilityTimeAfterRead p,
      testBodyMatchesSent p,
      testHeadersMatchSent p,
      testNullHeadersWhenNotSent p
    ]

-- | Test that messageId is always positive
testMessageIdIsPositive :: Pool.Pool -> TestTree
testMessageIdIsPositive p = testCase "messageId is positive" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)

    -- Send message
    let sendMsg = mkSendMessage queueName (object ["test" .= ("positive_id" :: String)])
    _ <- assertSession pool (Sessions.sendMessage sendMsg)

    -- Read message
    messages <- assertSession pool (mkReadMessage queueName)
    assertEqual "Should have 1 message" 1 (V.length messages)

    let msg = V.head messages
    assertBool
      "messageId should be positive (> 0)"
      (unMessageId (Pgmq.Types.messageId msg) > 0)
    cleanupQueue pool queueName

-- | Test that readCount increments on each read
testReadCountIncrements :: Pool.Pool -> TestTree
testReadCountIncrements p = testCase "readCount increments on each read" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)

    -- Send message
    let sendMsg = mkSendMessage queueName (object ["test" .= ("read_count" :: String)])
    msgId <- assertSession pool (Sessions.sendMessage sendMsg)

    -- First read
    messages1 <- assertSession pool (mkReadMessage queueName)
    assertEqual "Should have 1 message" 1 (V.length messages1)
    let msg1 = V.head messages1
    assertEqual "readCount should be 1 after first read" 1 (readCount msg1)

    -- Make message visible again via set_vt
    let vtQuery =
          VisibilityTimeoutQuery
            { queueName = queueName,
              messageId = msgId,
              visibilityTimeoutOffset = 0 -- Make visible immediately
            }
    _ <- assertSession pool (Sessions.changeVisibilityTimeout vtQuery)

    -- Second read
    messages2 <- assertSession pool (mkReadMessage queueName)
    assertEqual "Should have 1 message" 1 (V.length messages2)
    let msg2 = V.head messages2
    assertEqual "readCount should be 2 after second read" 2 (readCount msg2)

    cleanupQueue pool queueName

-- | Test that enqueuedAt is before read time
testEnqueuedAtBeforeRead :: Pool.Pool -> TestTree
testEnqueuedAtBeforeRead p = testCase "enqueuedAt is before read time" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    beforeEnqueue <- getCurrentTime
    assertSession pool (Sessions.createQueue queueName)

    -- Send message
    let sendMsg = mkSendMessage queueName (object ["test" .= ("enqueued_at" :: String)])
    _ <- assertSession pool (Sessions.sendMessage sendMsg)

    afterEnqueue <- getCurrentTime

    -- Read message
    messages <- assertSession pool (mkReadMessage queueName)
    let msg = V.head messages

    -- enqueuedAt should be between beforeEnqueue and afterEnqueue
    assertBool
      "enqueuedAt should be after test start"
      (enqueuedAt msg >= beforeEnqueue)
    assertBool
      "enqueuedAt should be before read"
      (enqueuedAt msg <= afterEnqueue)

    cleanupQueue pool queueName

-- | Test that lastReadAt is set after reading
testLastReadAtUpdates :: Pool.Pool -> TestTree
testLastReadAtUpdates p = testCase "lastReadAt is set after reading" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)

    -- Send message
    let sendMsg = mkSendMessage queueName (object ["test" .= ("last_read_at" :: String)])
    _ <- assertSession pool (Sessions.sendMessage sendMsg)

    beforeRead <- getCurrentTime

    -- First read
    messages1 <- assertSession pool (mkReadMessage queueName)
    let msg1 = V.head messages1

    afterRead <- getCurrentTime

    -- lastReadAt should be Just and between beforeRead and afterRead
    case lastReadAt msg1 of
      Nothing -> assertBool "lastReadAt should be set after first read" False
      Just lra -> do
        assertBool
          "lastReadAt should be >= beforeRead"
          (lra >= addUTCTime (-1) beforeRead) -- Allow 1 second tolerance
        assertBool
          "lastReadAt should be <= afterRead"
          (lra <= addUTCTime 1 afterRead) -- Allow 1 second tolerance
    cleanupQueue pool queueName

-- | Test that visibilityTime is in future after read with delay
testVisibilityTimeAfterRead :: Pool.Pool -> TestTree
testVisibilityTimeAfterRead p = testCase "visibilityTime is in future after read" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)

    -- Send message
    let sendMsg = mkSendMessage queueName (object ["test" .= ("vt" :: String)])
    _ <- assertSession pool (Sessions.sendMessage sendMsg)

    beforeRead <- getCurrentTime

    -- Read message with 60 second visibility timeout
    let readQuery =
          ReadMessage
            { queueName = queueName,
              delay = 60, -- 60 second visibility timeout
              batchSize = Just 1,
              conditional = Nothing
            }
    messages <- assertSession pool (Sessions.readMessage readQuery)
    let msg = V.head messages

    -- visibilityTime should be at least 55 seconds in the future
    let expectedMinVt = addUTCTime 55 beforeRead
    assertBool
      "visibilityTime should be at least 55 seconds in future"
      (visibilityTime msg >= expectedMinVt)

    cleanupQueue pool queueName

-- | Test that body exactly matches what was sent
testBodyMatchesSent :: Pool.Pool -> TestTree
testBodyMatchesSent p = testCase "body exactly matches sent message" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)

    -- Send message with specific body
    let sentBody =
          object
            [ "string_field" .= ("test value" :: String),
              "int_field" .= (42 :: Int),
              "bool_field" .= True,
              "nested" .= object ["inner" .= ("nested value" :: String)]
            ]
    let sendMsg = mkSendMessage queueName sentBody
    _ <- assertSession pool (Sessions.sendMessage sendMsg)

    -- Read message
    messages <- assertSession pool (mkReadMessage queueName)
    let msg = V.head messages
    let receivedBody = unMessageBody (Pgmq.Types.body msg)

    assertEqual "body should exactly match sent message" sentBody receivedBody

    cleanupQueue pool queueName

-- | Test that headers exactly match what was sent
testHeadersMatchSent :: Pool.Pool -> TestTree
testHeadersMatchSent p = testCase "headers exactly match sent message" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)

    -- Send message with headers
    let sentBody = object ["data" .= ("test" :: String)]
    let sentHeaders =
          object
            [ "trace_id" .= ("abc-123-xyz" :: String),
              "priority" .= (1 :: Int),
              "routing_key" .= ("queue.important" :: String)
            ]
    let sendMsg =
          SendMessageWithHeaders
            { queueName = queueName,
              messageBody = MessageBody sentBody,
              messageHeaders = MessageHeaders sentHeaders,
              delay = Nothing
            }
    _ <- assertSession pool (Sessions.sendMessageWithHeaders sendMsg)

    -- Read message
    messages <- assertSession pool (mkReadMessage queueName)
    let msg = V.head messages

    assertEqual "headers should exactly match sent message" (Just sentHeaders) (Pgmq.Types.headers msg)

    cleanupQueue pool queueName

-- | Test that headers are null when not sent
testNullHeadersWhenNotSent :: Pool.Pool -> TestTree
testNullHeadersWhenNotSent p = testCase "headers are null when not sent" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)

    -- Send message without headers
    let sendMsg = mkSendMessage queueName (object ["test" .= ("no_headers" :: String)])
    _ <- assertSession pool (Sessions.sendMessage sendMsg)

    -- Read message
    messages <- assertSession pool (mkReadMessage queueName)
    let msg = V.head messages

    assertEqual "headers should be Nothing when not sent" Nothing (Pgmq.Types.headers msg)

    cleanupQueue pool queueName

-- Helper functions

mkSendMessage :: QueueName -> Value -> SendMessage
mkSendMessage qName bodyVal =
  SendMessage
    { queueName = qName,
      messageBody = MessageBody bodyVal,
      delay = Nothing
    }

mkReadMessage :: QueueName -> Session (V.Vector Message)
mkReadMessage qName =
  Sessions.readMessage
    ReadMessage
      { queueName = qName,
        delay = 30,
        batchSize = Just 1,
        conditional = Nothing
      }
