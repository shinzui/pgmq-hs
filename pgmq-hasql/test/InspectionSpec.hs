{-# LANGUAGE OverloadedStrings #-}

-- | IR-1: reads that observe a queue or archive without leasing anything.
--
-- The non-destructive guarantee is asserted two ways: the raw cells every
-- lease touches (vt, read_ct, last_read_at) are byte-identical before and
-- after a sequence of peeks, and a consumer polling the queue concurrently
-- with a loop of peeks still receives every message exactly once with
-- read_ct = 1. Paging is keyset-only: the statements' SQL contains no OFFSET,
-- and paging by the last id seen visits every message in a quiescent fixture
-- exactly once. Late commits and late archival require a first-page refresh.
module InspectionSpec (tests) where

import Control.Concurrent (forkFinally, killThread, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, bracket, finally, mask, throwIO, try)
import Control.Monad (forM_, replicateM_, void)
import Data.Aeson (object, (.=))
import Data.Int (Int32, Int64)
import Data.List (sort)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Vector qualified as V
import EphemeralDb (Database, TestFixture (..), withTestFixture)
import EphemeralPg qualified as Pg
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Pool qualified as Pool
import Hasql.Session (Session, statement)
import Hasql.Session qualified as Session
import Hasql.Statement (preparable, toSql, unpreparable)
import Pgmq.Hasql.Sessions qualified as Sessions
import Pgmq.Hasql.Statements.Inspection qualified as Inspect
import Pgmq.Hasql.Statements.Types
  ( BatchMessageQuery (..),
    BatchSendMessage (..),
    CreatePartitionedQueue (..),
    LookupMessage (LookupMessage),
    PeekMessages (PeekMessages),
    QueueMetrics (..),
    ReadMessage (..),
    SendMessage (..),
  )
import Pgmq.Types (ArchivedMessage (..), Message (..), MessageBody (..), MessageId (..), QueueName, queueNameToText)
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase, (@?=))
import TestUtils (assertSession, cleanupQueue)

tests :: Pool.Pool -> Database -> TestTree
tests p db =
  testGroup
    "Inspection (non-destructive reads)"
    [ testPeekLeavesRowsUntouched p,
      testPeekDoesNotDisturbConsumer p,
      testConsumerFailure,
      testLateCommit p db,
      testLateArchive p,
      testKeysetPagingVisitsEachOnce p,
      testNoOffset,
      testArchiveReads p,
      testLookups p,
      testLimitSemantics p,
      testMissingQueue p,
      testMetricsParity p,
      testPartitionedQueue p
    ]

-- | The cells a lease would change, for every row, in id order.
rowImages :: Text -> Session [(Int64, Int32, Maybe UTCTime, UTCTime)]
rowImages table = statement () (unpreparable sql mempty decoder)
  where
    -- Test queue names are generated from [a-z0-9_], so splicing is safe here.
    sql = "select msg_id, read_ct, last_read_at, vt from pgmq.q_" <> table <> " order by msg_id"
    decoder =
      D.rowList
        ( (,,,)
            <$> D.column (D.nonNullable D.int8)
            <*> D.column (D.nonNullable D.int4)
            <*> D.column (D.nullable D.timestamptz)
            <*> D.column (D.nonNullable D.timestamptz)
        )

sendN :: Pool.Pool -> QueueName -> Int -> IO [MessageId]
sendN pool q n =
  assertSession pool $
    Sessions.batchSendMessage
      BatchSendMessage
        { queueName = q,
          messageBodies = [MessageBody (object ["n" .= i]) | i <- [1 .. n]],
          delay = Nothing
        }

peek :: Pool.Pool -> QueueName -> Maybe MessageId -> Int32 -> IO [Message]
peek pool q cursor n =
  V.toList <$> assertSession pool (Sessions.peekMessages (PeekMessages (queueNameToText q) cursor n))

lastId :: [Message] -> IO MessageId
lastId [] = assertFailure "expected a non-empty page"
lastId page = pure (messageId (last page))

testPeekLeavesRowsUntouched :: Pool.Pool -> TestTree
testPeekLeavesRowsUntouched p = testCase "peek leaves vt, read_ct, and last_read_at byte-identical" $
  withTestFixture p $ \TestFixture {pool, queueName} -> flip finally (cleanupQueue pool queueName) $ do
    assertSession pool (Sessions.createQueue queueName)
    _ <- sendN pool queueName 3
    -- Two more with a delay, so vt differs between rows.
    _ <- assertSession pool (Sessions.sendMessage (SendMessage queueName (MessageBody "late") (Just 60)))
    _ <- assertSession pool (Sessions.sendMessage (SendMessage queueName (MessageBody "later") (Just 120)))
    -- Lease one through the real read so a non-trivial read_ct and last_read_at exist.
    leased <- assertSession pool (Sessions.readMessage (ReadMessage queueName 30 (Just 1) Nothing))
    V.length leased @?= 1
    before <- assertSession pool (rowImages (queueNameToText queueName))
    replicateM_ 5 $ do
      first <- peek pool queueName Nothing 2
      cursor <- lastId first
      void (peek pool queueName (Just cursor) 50)
    _ <- assertSession pool (Sessions.lookupMessage (LookupMessage (queueNameToText queueName) (messageId (V.head leased))))
    after <- assertSession pool (rowImages (queueNameToText queueName))
    assertEqual "rows are identical after peeks and lookups" before after
    length before @?= 5

testPeekDoesNotDisturbConsumer :: Pool.Pool -> TestTree
testPeekDoesNotDisturbConsumer p = testCase "a concurrently polling consumer sees every message exactly once" $
  withTestFixture p $ \TestFixture {pool, queueName} -> flip finally (cleanupQueue pool queueName) $ do
    assertSession pool (Sessions.createQueue queueName)
    sent <- sendN pool queueName 50
    -- The bracketed worker propagates failures and is cancelled if peeking fails.
    let consume seen empties
          | Set.size seen >= 50 || empties >= (200 :: Int) = pure seen
          | otherwise = do
              batch <- assertSession pool (Sessions.readMessage (ReadMessage queueName 60 (Just 5) Nothing))
              if V.null batch
                then threadDelay 5_000 >> consume seen (empties + 1)
                else consume (foldr (Set.insert . messageId) seen (V.toList batch)) empties
    seen <-
      withWorker (consume Set.empty 0) $
        replicateM_ 200 (void (peek pool queueName Nothing 50))
    assertEqual "the consumer received exactly the fifty sent ids" (Set.fromList sent) seen
    rows <- assertSession pool (rowImages (queueNameToText queueName))
    length rows @?= 50
    forM_ rows $ \(mid, readCt, _, _) ->
      assertEqual ("read_ct of " <> show mid <> " was bumped only by the consumer") 1 readCt
    -- Everything is leased for sixty seconds now; a further read returns nothing,
    -- which a destructive "peek" would have made impossible to predict.
    more <- assertSession pool (Sessions.readMessage (ReadMessage queueName 60 (Just 50) Nothing))
    V.length more @?= 0

-- | Base-only structured concurrency: every exit cancels the worker, and its
-- exception is rethrown in the parent rather than leaving an empty result MVar.
withWorker :: IO a -> IO () -> IO a
withWorker work during = mask $ \restore -> do
  result <- newEmptyMVar
  bracket
    (forkFinally (restore work) (putMVar result))
    killThread
    (\_ -> restore (during >> takeMVar result >>= either throwIO pure))

testConsumerFailure :: TestTree
testConsumerFailure = testCase "consumer exceptions reach the parent promptly" $ do
  outcome <-
    timeout 1_000_000 $
      try @IOException (withWorker (ioError (userError "forced consumer failure")) (pure ()))
  case outcome of
    Just (Left err) -> assertBool "original worker failure" ("forced consumer failure" `T.isInfixOf` T.pack (show err))
    Just (Right ()) -> assertFailure "worker failure was swallowed"
    Nothing -> assertFailure "parent hung waiting for a failed consumer"

testLateCommit :: Pool.Pool -> Database -> TestTree
testLateCommit p db = testCase "a late lower-id commit needs a first-page refresh" $
  withTestFixture p $ \TestFixture {pool, queueName} -> flip finally (cleanupQueue pool queueName) $ do
    assertSession pool (Sessions.createQueue queueName)
    -- Hold the first producer transaction on its own connection. The pool's
    -- second producer commits before the first, without timing-based sleeps.
    bracket
      (Connection.acquire (Pg.connectionSettings db) >>= either (assertFailure . show) pure)
      Connection.release
      $ \producer -> do
        let run :: Session a -> IO a
            run session = Connection.use producer session >>= either (assertFailure . show) pure
        run (Session.script "BEGIN")
        older <- run (Sessions.sendMessage (SendMessage queueName (MessageBody "older") Nothing))
        newer <- assertSession pool (Sessions.sendMessage (SendMessage queueName (MessageBody "newer") Nothing))
        assertBool "allocation order differs from commit order" (older < newer)
        first <- peek pool queueName Nothing 10
        map messageId first @?= [newer]
        run (Session.script "COMMIT")
        continued <- peek pool queueName (Just newer) 10
        map messageId continued @?= []
        refreshed <- peek pool queueName Nothing 10
        map messageId refreshed @?= [older, newer]

testLateArchive :: Pool.Pool -> TestTree
testLateArchive p = testCase "archiving an older id needs a first-page archive refresh" $
  withTestFixture p $ \TestFixture {pool, queueName} -> flip finally (cleanupQueue pool queueName) $ do
    assertSession pool (Sessions.createQueue queueName)
    ids <- sendN pool queueName 2
    (older, newer) <- case ids of
      [a, b] -> pure (a, b)
      other -> assertFailure ("expected two ids: " <> show other)
    let q = queueNameToText queueName
        archive mid = void (assertSession pool (Sessions.batchArchiveMessages (BatchMessageQuery queueName [mid])))
        page cursor = map (messageId . archivedMessage) . V.toList <$> assertSession pool (Sessions.peekArchivedMessages (PeekMessages q cursor 10))
    archive newer
    page Nothing >>= (@?= [newer])
    archive older
    page (Just newer) >>= (@?= [])
    page Nothing >>= (@?= [older, newer])

testKeysetPagingVisitsEachOnce :: Pool.Pool -> TestTree
testKeysetPagingVisitsEachOnce p = testCase "paging by the last id visits every message exactly once" $
  withTestFixture p $ \TestFixture {pool, queueName} -> flip finally (cleanupQueue pool queueName) $ do
    assertSession pool (Sessions.createQueue queueName)
    sent <- concat <$> mapM (const (sendN pool queueName 100)) [1 .. 10 :: Int]
    length sent @?= 1000
    before <- assertSession pool (rowImages (queueNameToText queueName))
    let go cursor pages acc = do
          page <- peek pool queueName cursor 7
          let acc' = acc <> map messageId page
          if length page < 7
            then pure (pages + 1 :: Int, acc')
            else do
              next <- lastId page
              go (Just next) (pages + 1) acc'
    (pages, visited) <- go Nothing 0 []
    assertEqual "143 pages of at most seven" 143 pages
    assertEqual "every id exactly once, in order" (sort sent) visited
    assertEqual "no duplicates" (length visited) (Set.size (Set.fromList visited))
    after <- assertSession pool (rowImages (queueNameToText queueName))
    assertEqual "paging modified nothing" before after

testNoOffset :: TestTree
testNoOffset = testCase "inspection statements never use OFFSET" $ do
  let sqls =
        [ toSql (Inspect.peekStatement "q_x"),
          toSql (Inspect.peekArchivedStatement "a_x"),
          toSql (Inspect.lookupStatement "q_x"),
          toSql (Inspect.lookupArchivedStatement "a_x")
        ]
  forM_ sqls $ \sql -> do
    assertBool ("no offset in " <> T.unpack sql) (not ("offset" `T.isInfixOf` T.toLower sql))
    assertBool ("quoted identifier in " <> T.unpack sql) ("pgmq.\"" `T.isInfixOf` sql)
  assertBool "pages are ordered by msg_id" ("order by msg_id asc limit $2" `T.isInfixOf` toSql (Inspect.peekStatement "q_x"))
  Inspect.quoteIdentifier "q_odd-name" @?= "\"q_odd-name\""
  Inspect.quoteIdentifier "we\"ird" @?= "\"we\"\"ird\""

testArchiveReads :: Pool.Pool -> TestTree
testArchiveReads p = testCase "archive reads return archived messages with their archival timestamp" $
  withTestFixture p $ \TestFixture {pool, queueName} -> flip finally (cleanupQueue pool queueName) $ do
    assertSession pool (Sessions.createQueue queueName)
    sent <- sendN pool queueName 3
    archived <- assertSession pool (Sessions.batchArchiveMessages (BatchMessageQuery queueName (take 2 sent)))
    sort archived @?= sort (take 2 sent)
    let q = queueNameToText queueName
    rows <- V.toList <$> assertSession pool (Sessions.peekArchivedMessages (PeekMessages q Nothing 10))
    map (messageId . archivedMessage) rows @?= take 2 sent
    forM_ rows $ \row ->
      assertBool "archived_at is not before enqueued_at" (archivedAt row >= enqueuedAt (archivedMessage row))
    live <- peek pool queueName Nothing 10
    map messageId live @?= drop 2 sent
    -- The archive pages by the same exclusive cursor.
    second <- V.toList <$> assertSession pool (Sessions.peekArchivedMessages (PeekMessages q (Just (sent !! 0)) 10))
    map (messageId . archivedMessage) second @?= [sent !! 1]

testLookups :: Pool.Pool -> TestTree
testLookups p = testCase "lookup finds a present message and answers Nothing for an absent one, in both tables" $
  withTestFixture p $ \TestFixture {pool, queueName} -> flip finally (cleanupQueue pool queueName) $ do
    assertSession pool (Sessions.createQueue queueName)
    sent <- sendN pool queueName 2
    (liveId, archivedId) <- case sent of
      [a, b] -> pure (a, b)
      other -> assertFailure ("expected two ids, got " <> show other)
    _ <- assertSession pool (Sessions.batchArchiveMessages (BatchMessageQuery queueName [archivedId]))
    let q = queueNameToText queueName
    live <- assertSession pool (Sessions.lookupMessage (LookupMessage q liveId))
    fmap messageId live @?= Just liveId
    gone <- assertSession pool (Sessions.lookupMessage (LookupMessage q archivedId))
    fmap messageId gone @?= Nothing
    inArchive <- assertSession pool (Sessions.lookupArchivedMessage (LookupMessage q archivedId))
    fmap (messageId . archivedMessage) inArchive @?= Just archivedId
    notInArchive <- assertSession pool (Sessions.lookupArchivedMessage (LookupMessage q liveId))
    fmap (messageId . archivedMessage) notInArchive @?= Nothing
    never <- assertSession pool (Sessions.lookupMessage (LookupMessage q (MessageId 999_999_999)))
    fmap messageId never @?= Nothing

testLimitSemantics :: Pool.Pool -> TestTree
testLimitSemantics p = testCase "limit bounds the page and a cursor excludes its own row" $
  withTestFixture p $ \TestFixture {pool, queueName} -> flip finally (cleanupQueue pool queueName) $ do
    assertSession pool (Sessions.createQueue queueName)
    sent <- sendN pool queueName 5
    three <- peek pool queueName Nothing 3
    map messageId three @?= take 3 sent
    rest <- peek pool queueName (Just (sent !! 2)) 3
    map messageId rest @?= drop 3 sent
    none <- peek pool queueName (Just (sent !! 4)) 3
    map messageId none @?= []
    zero <- peek pool queueName Nothing 0
    map messageId zero @?= []

testMissingQueue :: Pool.Pool -> TestTree
testMissingQueue p = testCase "a missing queue fails with undefined_table (42P01)" $ do
  result <- Pool.use p (Sessions.peekMessages (PeekMessages "no_such_queue_for_inspection" Nothing 1))
  case result of
    Right _ -> assertFailure "peeking a missing queue should fail"
    Left err -> assertBool ("expected 42P01, got " <> show err) ("42P01" `T.isInfixOf` T.pack (show err))

testMetricsParity :: Pool.Pool -> TestTree
testMetricsParity p = testCase "queueMetricsUnvalidated matches queueMetrics for a validated name" $
  withTestFixture p $ \TestFixture {pool, queueName} -> flip finally (cleanupQueue pool queueName) $ do
    assertSession pool (Sessions.createQueue queueName)
    _ <- sendN pool queueName 4
    typed <- assertSession pool (Sessions.queueMetrics queueName)
    lenient <- assertSession pool (Sessions.queueMetricsUnvalidated (queueNameToText queueName))
    queueLength lenient @?= queueLength typed
    queueVisibleLength lenient @?= queueVisibleLength typed
    totalMessages lenient @?= totalMessages typed
    defaultPartitionLength lenient @?= defaultPartitionLength typed
    queueLength lenient @?= 4

-- | A partitioned queue's physical table is a partitioned parent; a SELECT on
-- it sees every partition, so the reads need no special case. Needs pg_partman.
testPartitionedQueue :: Pool.Pool -> TestTree
testPartitionedQueue p = testCase "peek and lookup work on a partitioned queue (needs pg_partman)" $
  withPartman p $
    withTestFixture p $ \TestFixture {pool, queueName} -> flip finally (cleanupQueue pool queueName) $ do
      assertSession pool (Sessions.createPartitionedQueue (CreatePartitionedQueue queueName "10" "100"))
      sent <- sendN pool queueName 12
      page <- peek pool queueName Nothing 20
      map messageId page @?= sent
      one <- assertSession pool (Sessions.lookupMessage (LookupMessage (queueNameToText queueName) (sent !! 11)))
      fmap messageId one @?= Just (sent !! 11)
      -- The interval-10 archive also spans multiple leaves and supports the
      -- same first/late cursor and by-id paths as the larger diagnostic fixture.
      archivedIds <- assertSession pool (Sessions.batchArchiveMessages (BatchMessageQuery queueName sent))
      sort archivedIds @?= sent
      let q = queueNameToText queueName
          archivePage cursor = map (messageId . archivedMessage) . V.toList <$> assertSession pool (Sessions.peekArchivedMessages (PeekMessages q cursor 5))
      archivePage Nothing >>= (@?= take 5 sent)
      archivePage (Just (sent !! 10)) >>= (@?= drop 11 sent)
      found <- assertSession pool (Sessions.lookupArchivedMessage (LookupMessage q (sent !! 11)))
      fmap (messageId . archivedMessage) found @?= Just (sent !! 11)
      missing <- assertSession pool (Sessions.lookupArchivedMessage (LookupMessage q (MessageId 999999)))
      fmap (messageId . archivedMessage) missing @?= Nothing

withPartman :: Pool.Pool -> IO () -> IO ()
withPartman pool action = do
  available <-
    assertSession pool $
      statement () $
        preparable
          "select exists (select 1 from pg_extension where extname = 'pg_partman')"
          E.noParams
          (D.singleRow (D.column (D.nonNullable D.bool)))
  required <- (== Just "1") <$> lookupEnv "PGMQ_REQUIRE_PARTMAN"
  if available
    then action
    else
      if required
        then assertFailure "PGMQ_REQUIRE_PARTMAN=1 but pg_partman is not installed"
        else putStrLn "    SKIPPED: pg_partman is not installed"
