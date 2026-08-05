{-# LANGUAGE OverloadedStrings #-}

-- | PGH-11: a SQL NULL message body must not poison every read batch.
--
-- The queue table's @message@ column is nullable and
-- @select pgmq.send('q', null::jsonb)@ is legal SQL, so any non-Haskell
-- producer (psql, another language's client, a trigger) can insert a NULL
-- body. The decoder required a non-null body, so every batch containing such a
-- row failed at decode — /after/ the read statement had already bumped @vt@ and
-- @read_ct@ for the whole batch, because the statement succeeded and only its
-- result failed to decode. The row could not be seen, read, or archived through
-- the Haskell client, and it re-poisoned every batch each time its visibility
-- timeout lapsed.
--
-- The fix decodes SQL NULL as JSON @null@ (@MessageBody Aeson.Null@), an
-- accepted conflation with an explicitly-sent JSON @null@ body: both mean "no
-- usable payload", and the poison row becomes visible, identifiable, and
-- archivable through the normal API.
module NullBodySpec (tests) where

import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.Int (Int64)
import Data.Text (Text)
import Data.Vector qualified as V
import EphemeralDb (TestFixture (..), withTestFixture)
import Hasql.Decoders qualified as D
import Hasql.Pool qualified as Pool
import Hasql.Session (Session, statement)
import Hasql.Statement (unpreparable)
import Pgmq.Hasql.Sessions qualified as Sessions
import Pgmq.Hasql.Statements.Types
  ( BatchSendMessage (..),
    MessageQuery (..),
    ReadMessage (..),
  )
import Pgmq.Types (MessageBody (..), queueNameToText)
import Pgmq.Types qualified as PgmqTypes
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import TestUtils (assertSession, cleanupQueue, runSession)

tests :: Pool.Pool -> TestTree
tests p =
  testGroup
    "NULL Message Body (PGH-11)"
    [ testNullBodyBatchReadsFully p,
      testNullBodyArchivable p,
      testReadCtBumpedRegardless p
    ]

-- | Seed two well-formed messages through the API, then insert the poison row
-- the way any non-Haskell producer would: raw SQL. The queue name comes from
-- the fixture generator, so splicing it into the SQL text is safe.
seedWithPoison :: Pool.Pool -> PgmqTypes.QueueName -> IO ()
seedWithPoison pool queueName = do
  assertSession pool (Sessions.createQueue queueName)
  _ <-
    assertSession pool $
      Sessions.batchSendMessage
        BatchSendMessage
          { queueName = queueName,
            messageBodies = [MessageBody (object ["seq" .= i]) | i <- [1 :: Int, 2]],
            delay = Nothing
          }
  _ <-
    assertSession pool $
      rawIds ("select pgmq.send('" <> queueNameToText queueName <> "', null::jsonb)")
  pure ()

-- | A batch containing the NULL-bodied row must read fully, with the poison
-- row surfacing as JSON @null@. Red before the decoder fix: the whole batch
-- failed with a decode error on the NULL cell.
testNullBodyBatchReadsFully :: Pool.Pool -> TestTree
testNullBodyBatchReadsFully p = testCase "a batch containing a NULL body reads fully" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    seedWithPoison pool queueName
    msgs <-
      assertSession pool $
        Sessions.readMessage
          ReadMessage
            { queueName = queueName,
              delay = 30,
              batchSize = Just 10,
              conditional = Nothing
            }
    assertEqual "All three messages read, poison row included" 3 (V.length msgs)
    let nullBodied = [m | m <- V.toList msgs, unMessageBody (PgmqTypes.body m) == Aeson.Null]
    assertEqual "Exactly one message surfaces as JSON null" 1 (length nullBodied)
    cleanupQueue pool queueName

-- | The poison row must be identifiable and archivable through the normal API
-- — the dead-letter path a consumer actually needs. Red before the fix: the
-- row could not even be read to learn its id.
testNullBodyArchivable :: Pool.Pool -> TestTree
testNullBodyArchivable p = testCase "the NULL-bodied row can be archived through the normal API" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    seedWithPoison pool queueName
    msgs <-
      assertSession pool $
        Sessions.readMessage
          ReadMessage
            { queueName = queueName,
              delay = 30,
              batchSize = Just 10,
              conditional = Nothing
            }
    poisonId <-
      case [PgmqTypes.messageId m | m <- V.toList msgs, unMessageBody (PgmqTypes.body m) == Aeson.Null] of
        [msgId] -> pure msgId
        other -> assertFailure $ "Expected exactly one NULL-bodied message, got " <> show (length other)
    archived <-
      assertSession pool $
        Sessions.archiveMessage MessageQuery {queueName = queueName, messageId = poisonId}
    assertBool "archiveMessage reports success for the poison row" archived
    remaining <- assertSession pool (rawCount ("select count(*) from pgmq.q_" <> queueNameToText queueName))
    assertEqual "The two well-formed messages remain queued" 2 remaining
    archivedCount <- assertSession pool (rawCount ("select count(*) from pgmq.a_" <> queueNameToText queueName))
    assertEqual "The poison row landed in the archive" 1 archivedCount
    cleanupQueue pool queueName

-- | The read statement bumps @read_ct@ and @vt@ for the whole batch whether or
-- not the client manages to decode the result. Before the decoder fix this is
-- what made the NULL body a poison row rather than a mere error: the failed
-- call still consumed a read attempt for every batch-mate and hid the whole
-- batch for the visibility timeout, over and over. This test passes before and
-- after the fix; before, it documents the damage the failed call left behind.
testReadCtBumpedRegardless :: Pool.Pool -> TestTree
testReadCtBumpedRegardless p = testCase "read_ct is bumped for the whole batch even when decode fails" $ do
  withTestFixture p $ \TestFixture {pool, queueName} -> do
    seedWithPoison pool queueName
    -- Deliberately ignore the outcome: Left (decode failure) before the fix,
    -- Right afterwards. The server-side damage is identical.
    _ <-
      runSession pool $
        Sessions.readMessage
          ReadMessage
            { queueName = queueName,
              delay = 30,
              batchSize = Just 10,
              conditional = Nothing
            }
    readCts <-
      assertSession pool $
        rawCounts ("select read_ct::int8 from pgmq.q_" <> queueNameToText queueName <> " order by msg_id")
    assertEqual "All three rows consumed a read attempt" [1, 1, 1] (V.toList readCts)
    cleanupQueue pool queueName

-- Raw statement helpers -------------------------------------------------------

rawIds :: Text -> Session (V.Vector Int64)
rawIds sqlText = statement () (unpreparable sqlText mempty (D.rowVector (D.column (D.nonNullable D.int8))))

rawCount :: Text -> Session Int64
rawCount sqlText = statement () (unpreparable sqlText mempty (D.singleRow (D.column (D.nonNullable D.int8))))

rawCounts :: Text -> Session (V.Vector Int64)
rawCounts sqlText = statement () (unpreparable sqlText mempty (D.rowVector (D.column (D.nonNullable D.int8))))
