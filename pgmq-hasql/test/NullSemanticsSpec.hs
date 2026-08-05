{-# LANGUAGE OverloadedStrings #-}

-- | Tests that pin the meaning of @Nothing@ for every optional parameter that
-- reaches PostgreSQL as a bound SQL NULL.
--
-- Background a reader needs: a plpgsql parameter DEFAULT applies only when the
-- argument is /omitted/ from the call. A bound SQL NULL is a supplied argument,
-- so it silently overrides the DEFAULT. Combined with @LIMIT NULL@ meaning
-- @LIMIT ALL@ in PostgreSQL, an optional batch size encoded as a nullable
-- parameter turns "no preference" into "the whole queue". These tests assert
-- the documented behaviour instead: @Nothing@ means the documented default and
-- never widens the scope of an operation.
module NullSemanticsSpec (tests) where

import Data.Aeson (object, (.=))
import Data.Text (Text)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Data.Vector qualified as V
import EphemeralDb (TestFixture (..), withTestFixture)
import Hasql.Pool qualified as Pool
import Pgmq.Hasql.Sessions qualified as Sessions
import Pgmq.Hasql.Statements.Types
  ( BatchSendMessage (..),
    EnableNotifyInsert (..),
    PopMessage (..),
    QueueMetrics (..),
    ReadMessage (..),
    ReadWithPollMessage (..),
    SendMessage (..),
    VisibilityTimeoutAtQuery (..),
    VisibilityTimeoutQuery (..),
  )
import Pgmq.Types (MessageBody (..), MessageId (..), queueNameToText)
import Pgmq.Types qualified as PgmqTypes
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import TestUtils (assertJust, assertSession, cleanupQueue)

-- | All NULL-parameter semantics tests
tests :: Pool.Pool -> TestTree
tests p =
  testGroup
    "NULL Parameter Semantics"
    [ testPopNothingDoesNotDrainQueue p,
      testReadNothingDoesNotLeaseQueue p,
      testReadWithPollNothingDoesNotLeaseQueue p,
      testConditionalFiltersWhenJust p,
      testEnableNotifyInsertNothingUsesDefault p,
      testSetVtOnMissingRow p
    ]

-- | Send @n@ distinct messages to a queue and return nothing useful; the bodies
-- are irrelevant beyond being distinguishable in a failure message.
seedMessages :: Pool.Pool -> PgmqTypes.QueueName -> Int -> IO ()
seedMessages pool queueName n = do
  _ <-
    assertSession pool $
      Sessions.batchSendMessage
        BatchSendMessage
          { queueName = queueName,
            messageBodies = [MessageBody (object ["seq" .= i]) | i <- [1 .. n]],
            delay = Nothing
          }
  pure ()

-- | @pop@ with @qty = Nothing@ must pop exactly one message.
--
-- Before the fix this popped — and therefore permanently deleted — every
-- visible message in the queue, because the NULL @qty@ became @LIMIT ALL@ in
-- the DELETE-returning CTE inside @pgmq.pop@. There is no visibility-timeout
-- safety net for @pop@: the rows are gone.
testPopNothingDoesNotDrainQueue :: Pool.Pool -> TestTree
testPopNothingDoesNotDrainQueue p = testCase "pop with qty = Nothing pops exactly one message" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    seedMessages pool queueName 5
    popped <- assertSession pool (Sessions.pop PopMessage {queueName = queueName, qty = Nothing})
    assertEqual "Should pop exactly 1 message" 1 (V.length popped)
    metrics <- assertSession pool (Sessions.queueMetrics queueName)
    assertEqual "Should leave 4 messages in the queue" 4 (queueLength metrics)
    cleanupQueue pool queueName

-- | @read@ with @batchSize = Nothing@ must read exactly one message.
--
-- Before the fix the NULL batch size became @LIMIT ALL@, so a single call
-- leased the entire queue: every row had its visibility timeout pushed forward
-- and its read count incremented, hiding the whole queue from every other
-- consumer for the duration of the timeout.
testReadNothingDoesNotLeaseQueue :: Pool.Pool -> TestTree
testReadNothingDoesNotLeaseQueue p = testCase "read with batchSize = Nothing reads exactly one message" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    seedMessages pool queueName 5
    first <-
      assertSession pool $
        Sessions.readMessage
          ReadMessage
            { queueName = queueName,
              delay = 30,
              batchSize = Nothing,
              conditional = Nothing
            }
    assertEqual "Should read exactly 1 message" 1 (V.length first)
    -- Only one row may have been leased, so four remain immediately visible.
    second <-
      assertSession pool $
        Sessions.readMessage
          ReadMessage
            { queueName = queueName,
              delay = 30,
              batchSize = Just 10,
              conditional = Nothing
            }
    assertEqual "Should leave 4 messages unleased" 4 (V.length second)
    cleanupQueue pool queueName

-- | @readWithPoll@ shares the @LIMIT NULL@ hazard with @read@ and must behave
-- identically for @batchSize = Nothing@.
testReadWithPollNothingDoesNotLeaseQueue :: Pool.Pool -> TestTree
testReadWithPollNothingDoesNotLeaseQueue p = testCase "readWithPoll with batchSize = Nothing reads exactly one message" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    seedMessages pool queueName 5
    first <-
      assertSession pool $
        Sessions.readWithPoll
          ReadWithPollMessage
            { queueName = queueName,
              delay = 30,
              batchSize = Nothing,
              maxPollSeconds = 1,
              pollIntervalMs = 100,
              conditional = Nothing
            }
    assertEqual "Should read exactly 1 message" 1 (V.length first)
    second <-
      assertSession pool $
        Sessions.readWithPoll
          ReadWithPollMessage
            { queueName = queueName,
              delay = 30,
              batchSize = Just 10,
              maxPollSeconds = 1,
              pollIntervalMs = 100,
              conditional = Nothing
            }
    assertEqual "Should leave 4 messages unleased" 4 (V.length second)
    cleanupQueue pool queueName

-- | The @conditional@ field on 'ReadMessage' must actually filter.
--
-- @conditional@ is a JSONB containment filter: a message is returned only when
-- its body contains the given object (SQL @message \@> conditional@).
-- @Nothing@ means "no filtering".
--
-- Before the fix the field was never encoded — the statement bound only three
-- parameters — so a @Just@ filter was silently ignored and every visible
-- message came back.
testConditionalFiltersWhenJust :: Pool.Pool -> TestTree
testConditionalFiltersWhenJust p = testCase "conditional filters when Just and is neutral when Nothing" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    _ <-
      assertSession pool $
        Sessions.batchSendMessage
          BatchSendMessage
            { queueName = queueName,
              messageBodies =
                [ MessageBody (object ["kind" .= ("a" :: Text)]),
                  MessageBody (object ["kind" .= ("b" :: Text)])
                ],
              delay = Nothing
            }
    -- delay = 0 keeps both messages immediately visible for the second read.
    filtered <-
      assertSession pool $
        Sessions.readMessage
          ReadMessage
            { queueName = queueName,
              delay = 0,
              batchSize = Just 10,
              conditional = Just (object ["kind" .= ("a" :: Text)])
            }
    assertEqual "Filtered read should return only the matching message" 1 (V.length filtered)
    case V.toList filtered of
      [msg] ->
        assertEqual
          "Filtered read should return the 'a' message"
          (object ["kind" .= ("a" :: Text)])
          (unMessageBody (PgmqTypes.body msg))
      _ -> assertFailure "Filtered read should return exactly one message"
    unfiltered <-
      assertSession pool $
        Sessions.readMessage
          ReadMessage
            { queueName = queueName,
              delay = 0,
              batchSize = Just 10,
              conditional = Nothing
            }
    assertEqual "Unfiltered read should return both messages" 2 (V.length unfiltered)
    cleanupQueue pool queueName

-- | @enableNotifyInsert@ with @throttleIntervalMs = Nothing@ must install the
-- documented 250 ms throttle.
--
-- Before the fix the bound SQL NULL was inserted straight into
-- @pgmq.notify_insert_throttle.throttle_interval_ms@, which is @NOT NULL@; a
-- column DEFAULT does not apply to an explicitly supplied NULL, so the call
-- raised SQLSTATE 23502 every single time. Because the reconciler runs each
-- statement in its own transaction, the queue creation had already committed,
-- so the failure repeated on every application startup forever.
testEnableNotifyInsertNothingUsesDefault :: Pool.Pool -> TestTree
testEnableNotifyInsertNothingUsesDefault p = testCase "enableNotifyInsert with Nothing applies the 250ms default" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    assertSession pool $
      Sessions.enableNotifyInsert
        EnableNotifyInsert {queueName = queueName, throttleIntervalMs = Nothing}
    throttles <- assertSession pool Sessions.listNotifyInsertThrottles
    let mine = filter (\t -> PgmqTypes.throttleQueueName t == queueNameToText queueName) throttles
    case mine of
      [t] ->
        assertEqual
          "Throttle interval should be the documented 250ms default"
          250
          (PgmqTypes.throttleIntervalMs t)
      _ -> assertFailure $ "Expected exactly one throttle row for the queue, got " <> show (length mine)
    cleanupQueue pool queueName

-- | Setting a visibility timeout on a message that no longer exists must be an
-- ordinary, reportable outcome rather than a session failure.
--
-- @pgmq.set_vt@ is @RETURNS SETOF@ and yields zero rows for an absent
-- @msg_id@. Decoding that with a single-row decoder produces hasql's
-- @UnexpectedRowCountStatementError@ wrapped in a @StatementSessionError@ —
-- the same shape a genuine infrastructure failure has — so a caller extending
-- a lease could not distinguish "someone else already deleted this message"
-- from "the database is broken".
--
-- Both functions therefore return @Maybe Message@: @Nothing@ for an absent
-- row, @Just@ for a live one, and a session error only for a genuine failure.
testSetVtOnMissingRow :: Pool.Pool -> TestTree
testSetVtOnMissingRow p = testCase "set_vt on a raced-away row returns Nothing" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    assertSession pool (Sessions.createQueue queueName)
    let missingId = MessageId 999999
    futureTime <- addUTCTime 60 <$> getCurrentTime
    missingChanged <-
      assertSession pool $
        Sessions.changeVisibilityTimeout
          VisibilityTimeoutQuery
            { queueName = queueName,
              messageId = missingId,
              visibilityTimeoutOffset = 60
            }
    assertEqual "changeVisibilityTimeout on a missing message should be Nothing" Nothing (fmap PgmqTypes.messageId missingChanged)
    missingSetAt <-
      assertSession pool $
        Sessions.setVisibilityTimeoutAt
          VisibilityTimeoutAtQuery
            { queueName = queueName,
              messageId = missingId,
              visibilityTime = futureTime
            }
    assertEqual "setVisibilityTimeoutAt on a missing message should be Nothing" Nothing (fmap PgmqTypes.messageId missingSetAt)
    -- An existing message must still be updated and returned.
    msgId <-
      assertSession pool $
        Sessions.sendMessage
          SendMessage
            { queueName = queueName,
              messageBody = MessageBody (object ["vt" .= ("present" :: Text)]),
              delay = Nothing
            }
    changed <-
      assertJust
        =<< assertSession
          pool
          ( Sessions.changeVisibilityTimeout
              VisibilityTimeoutQuery
                { queueName = queueName,
                  messageId = msgId,
                  visibilityTimeoutOffset = 60
                }
          )
    assertEqual "changeVisibilityTimeout should return the message" msgId (PgmqTypes.messageId changed)
    setAt <-
      assertJust
        =<< assertSession
          pool
          ( Sessions.setVisibilityTimeoutAt
              VisibilityTimeoutAtQuery
                { queueName = queueName,
                  messageId = msgId,
                  visibilityTime = futureTime
                }
          )
    assertEqual "setVisibilityTimeoutAt should return the message" msgId (PgmqTypes.messageId setAt)
    cleanupQueue pool queueName
