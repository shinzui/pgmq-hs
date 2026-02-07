{-# LANGUAGE OverloadedStrings #-}

-- | Property-based round-trip tests
-- Verifies that data survives encode -> database -> decode cycles
module RoundTripSpec (tests) where

import Data.Vector qualified as V
import Generators (genMessageBody, genMessageHeaders)
import Hasql.Pool qualified as Pool
import Hedgehog (annotateShow, forAll, (===))
import Hedgehog qualified as H
import Pgmq.Hasql.Sessions qualified as Sessions
import Pgmq.Hasql.Statements.Types
  ( ReadMessage (..),
    SendMessage (..),
    SendMessageWithHeaders (..),
  )
import Pgmq.Types (Message (..), MessageBody (..), MessageHeaders (..), unMessageId)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import TestUtils (assertSession, cleanupQueue)
import TmpPostgres (TestFixture (..), withTestFixture)

-- | All round-trip property tests
tests :: Pool.Pool -> TestTree
tests p =
  testGroup
    "Round-Trip Properties"
    [ testProperty "message body survives round-trip" (propMessageBodyRoundTrip p),
      testProperty "message with headers survives round-trip" (propMessageWithHeadersRoundTrip p),
      testProperty "timestamp ordering is preserved" (propTimestampOrdering p)
    ]

-- | Property: message body survives send/read round-trip
propMessageBodyRoundTrip :: Pool.Pool -> H.Property
propMessageBodyRoundTrip p = H.property $ do
  body <- forAll genMessageBody
  result <- H.evalIO $ withTestFixture p $ \TestFixture {pool, queueName} -> do
    -- Create queue
    assertSession pool (Sessions.createQueue queueName)

    -- Send message with random body
    let sendMsg =
          SendMessage
            { queueName = queueName,
              messageBody = body,
              delay = Nothing
            }
    _ <- assertSession pool (Sessions.sendMessage sendMsg)

    -- Read message back
    let readQuery =
          ReadMessage
            { queueName = queueName,
              delay = 30,
              batchSize = Just 1,
              conditional = Nothing
            }
    messages <- assertSession pool (Sessions.readMessage readQuery)
    cleanupQueue pool queueName
    pure messages

  -- Verify body is preserved
  annotateShow result
  H.assert (V.length result == 1)
  let msg = V.head result
  unMessageBody (body :: MessageBody) === unMessageBody (Pgmq.Types.body msg)

-- | Property: message with headers survives round-trip
propMessageWithHeadersRoundTrip :: Pool.Pool -> H.Property
propMessageWithHeadersRoundTrip p = H.property $ do
  body <- forAll genMessageBody
  hdrs <- forAll genMessageHeaders
  result <- H.evalIO $ withTestFixture p $ \TestFixture {pool, queueName} -> do
    -- Create queue
    assertSession pool (Sessions.createQueue queueName)

    -- Send message with headers
    let sendMsg =
          SendMessageWithHeaders
            { queueName = queueName,
              messageBody = body,
              messageHeaders = hdrs,
              delay = Nothing
            }
    _ <- assertSession pool (Sessions.sendMessageWithHeaders sendMsg)

    -- Read message back
    let readQuery =
          ReadMessage
            { queueName = queueName,
              delay = 30,
              batchSize = Just 1,
              conditional = Nothing
            }
    messages <- assertSession pool (Sessions.readMessage readQuery)
    cleanupQueue pool queueName
    pure messages

  -- Verify body and headers are preserved
  annotateShow result
  H.assert (V.length result == 1)
  let msg = V.head result
  unMessageBody (body :: MessageBody) === unMessageBody (Pgmq.Types.body msg)
  Just (unMessageHeaders hdrs) === Pgmq.Types.headers msg

-- | Property: timestamps maintain ordering (enqueued_at <= vt when read)
propTimestampOrdering :: Pool.Pool -> H.Property
propTimestampOrdering p = H.property $ do
  body <- forAll genMessageBody
  result <- H.evalIO $ withTestFixture p $ \TestFixture {pool, queueName} -> do
    -- Create queue
    assertSession pool (Sessions.createQueue queueName)

    -- Send message
    let sendMsg =
          SendMessage
            { queueName = queueName,
              messageBody = body,
              delay = Nothing
            }
    _ <- assertSession pool (Sessions.sendMessage sendMsg)

    -- Read message with visibility timeout
    let readQuery =
          ReadMessage
            { queueName = queueName,
              delay = 30, -- 30 second visibility timeout
              batchSize = Just 1,
              conditional = Nothing
            }
    messages <- assertSession pool (Sessions.readMessage readQuery)
    cleanupQueue pool queueName
    pure messages

  -- Verify timestamp ordering
  H.assert (V.length result == 1)
  let msg = V.head result
  -- enqueued_at should be before visibility_time (vt is set to future after read)
  H.assert (enqueuedAt msg < visibilityTime msg)
  -- Message ID should be positive
  H.assert (unMessageId (messageId msg) > 0)
  -- Read count should be 1 after first read
  readCount msg === 1
