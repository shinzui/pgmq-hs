{-# LANGUAGE OverloadedStrings #-}

-- | Decoder tests for all pgmq functions that return messages
-- Each test verifies that the decoder works correctly for a specific function
module AllFunctionsDecoderSpec (tests) where

import Data.Aeson (Value, object, (.=))
import Data.Time (addUTCTime, getCurrentTime)
import Data.Vector qualified as V
import Hasql.Pool qualified as Pool
import Pgmq.Hasql.Sessions qualified as Sessions
import Pgmq.Hasql.Statements.Types
  ( BatchVisibilityTimeoutQuery (..),
    PopMessage (..),
    ReadMessage (..),
    ReadWithPollMessage (..),
    SendMessage (..),
    VisibilityTimeoutQuery (..),
  )
import Pgmq.Types (Message (..), MessageBody (..), QueueName, unMessageId)
import Pgmq.Types qualified
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import TestUtils (assertSession, cleanupQueue)
import TmpPostgres (TestFixture (..), withTestFixture)

-- | All per-function decoder tests
tests :: Pool.Pool -> TestTree
tests p =
  testGroup
    "All Functions Decoder Tests"
    [ testReadDecoder p,
      testPopDecoder p,
      testSetVtDecoder p,
      testBatchSetVtDecoder p,
      testReadWithPollDecoder p
    ]

-- | Test pgmq.read() decoder
testReadDecoder :: Pool.Pool -> TestTree
testReadDecoder p = testCase "read() returns correctly decoded message" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)

    -- Send message with known body
    let sentBody = object ["function" .= ("read" :: String), "value" .= (123 :: Int)]
    _ <- assertSession pool (Sessions.sendMessage (mkSendMessage queueName sentBody))

    -- Read via pgmq.read
    let readQuery =
          ReadMessage
            { queueName = queueName,
              delay = 30,
              batchSize = Just 1,
              conditional = Nothing
            }
    messages <- assertSession pool (Sessions.readMessage readQuery)

    assertEqual "Should return 1 message" 1 (V.length messages)
    let msg = V.head messages
    assertValidMessage msg sentBody

    cleanupQueue pool queueName

-- | Test pgmq.pop() decoder
testPopDecoder :: Pool.Pool -> TestTree
testPopDecoder p = testCase "pop() returns correctly decoded message" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)

    -- Send message
    let sentBody = object ["function" .= ("pop" :: String), "value" .= (456 :: Int)]
    _ <- assertSession pool (Sessions.sendMessage (mkSendMessage queueName sentBody))

    -- Pop message
    let popQuery = PopMessage {queueName = queueName, qty = Just 1}
    messages <- assertSession pool (Sessions.pop popQuery)

    assertEqual "Should return 1 message" 1 (V.length messages)
    let msg = V.head messages
    assertValidMessage msg sentBody

    -- Verify message is deleted (pop removes from queue)
    let readQuery =
          ReadMessage
            { queueName = queueName,
              delay = 30,
              batchSize = Just 1,
              conditional = Nothing
            }
    messagesAfter <- assertSession pool (Sessions.readMessage readQuery)
    assertEqual "Queue should be empty after pop" 0 (V.length messagesAfter)

    cleanupQueue pool queueName

-- | Test pgmq.set_vt() decoder
testSetVtDecoder :: Pool.Pool -> TestTree
testSetVtDecoder p = testCase "set_vt() returns correctly decoded message" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)

    -- Send message
    let sentBody = object ["function" .= ("set_vt" :: String)]
    msgId <- assertSession pool (Sessions.sendMessage (mkSendMessage queueName sentBody))

    beforeSetVt <- getCurrentTime

    -- Change visibility timeout
    let vtQuery =
          VisibilityTimeoutQuery
            { queueName = queueName,
              messageId = msgId,
              visibilityTimeoutOffset = 60
            }
    msg <- assertSession pool (Sessions.changeVisibilityTimeout vtQuery)

    -- Verify message fields
    assertBool "messageId should be positive" (unMessageId (Pgmq.Types.messageId msg) > 0)
    assertEqual "messageId should match" msgId (Pgmq.Types.messageId msg)
    assertEqual "body should match" sentBody (unMessageBody (body msg))

    -- Visibility time should be ~60 seconds in the future
    let expectedMinVt = addUTCTime 55 beforeSetVt
    assertBool "visibilityTime should be in future" (visibilityTime msg >= expectedMinVt)

    cleanupQueue pool queueName

-- | Test batch set_vt decoder
testBatchSetVtDecoder :: Pool.Pool -> TestTree
testBatchSetVtDecoder p = testCase "batch set_vt returns correctly decoded messages" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)

    -- Send two messages
    let body1 = object ["batch_vt" .= (1 :: Int)]
    let body2 = object ["batch_vt" .= (2 :: Int)]
    msgId1 <- assertSession pool (Sessions.sendMessage (mkSendMessage queueName body1))
    msgId2 <- assertSession pool (Sessions.sendMessage (mkSendMessage queueName body2))

    -- Batch change visibility timeout
    let batchVtQuery =
          BatchVisibilityTimeoutQuery
            { queueName = queueName,
              messageIds = [msgId1, msgId2],
              visibilityTimeoutOffset = 45
            }
    messages <- assertSession pool (Sessions.batchChangeVisibilityTimeout batchVtQuery)

    assertEqual "Should return 2 messages" 2 (V.length messages)

    -- Verify each message is valid
    let msg1 = V.head messages
    let msg2 = messages V.! 1
    assertBool "first messageId should be positive" (unMessageId (Pgmq.Types.messageId msg1) > 0)
    assertBool "second messageId should be positive" (unMessageId (Pgmq.Types.messageId msg2) > 0)

    cleanupQueue pool queueName

-- | Test read_with_poll decoder
testReadWithPollDecoder :: Pool.Pool -> TestTree
testReadWithPollDecoder p = testCase "read_with_poll returns correctly decoded message" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)

    -- Send message first
    let sentBody = object ["function" .= ("read_with_poll" :: String)]
    _ <- assertSession pool (Sessions.sendMessage (mkSendMessage queueName sentBody))

    -- Read with poll (should return immediately since message exists)
    let pollQuery =
          ReadWithPollMessage
            { queueName = queueName,
              delay = 30,
              batchSize = Just 1,
              maxPollSeconds = 1,
              pollIntervalMs = 100,
              conditional = Nothing
            }
    messages <- assertSession pool (Sessions.readWithPoll pollQuery)

    assertEqual "Should return 1 message" 1 (V.length messages)
    let msg = V.head messages
    assertValidMessage msg sentBody

    cleanupQueue pool queueName

-- Helper functions

mkSendMessage :: QueueName -> Value -> SendMessage
mkSendMessage qName bodyVal =
  SendMessage
    { queueName = qName,
      messageBody = MessageBody bodyVal,
      delay = Nothing
    }

-- | Assert that a message has valid fields
assertValidMessage :: Message -> Value -> IO ()
assertValidMessage msg expectedBody = do
  assertBool "messageId should be positive" (unMessageId (Pgmq.Types.messageId msg) > 0)
  assertBool "readCount should be >= 0" (readCount msg >= 0)
  assertEqual "body should match expected" expectedBody (unMessageBody (body msg))
