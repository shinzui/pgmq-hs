{-# LANGUAGE OverloadedStrings #-}

-- | Complete projections on real upstream layouts, without installing indexes.
-- Set PGMQ_INSPECTION_PLANS to an absolute directory to retain JSON EXPLAINs.
module InspectionCostSpec (tests) where

import Control.Exception (finally)
import Control.Monad (forM_, when)
import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import EphemeralDb (TestFixture (..), withTestFixture)
import Hasql.Decoders qualified as D
import Hasql.Pool qualified as Pool
import Hasql.Session (Session, statement)
import Hasql.Session qualified as Session
import Hasql.Statement (toSql, unpreparable)
import Pgmq.Hasql.Sessions qualified as Sessions
import Pgmq.Hasql.Statements.Inspection qualified as Inspect
import Pgmq.Hasql.Statements.Types (CreatePartitionedQueue (..), LookupMessage (LookupMessage), PeekMessages (PeekMessages))
import Pgmq.Types (ArchivedMessage (..), Message (..), MessageId (..), queueNameToText)
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestUtils (assertSession, cleanupQueue)

tests :: Pool.Pool -> TestTree
tests pool =
  testGroup "Inspection cost and partition layouts" $
    map (layoutTest pool) ["ordinary", "unlogged", "numeric", "time"]

layoutTest :: Pool.Pool -> Text -> TestTree
layoutTest p layout = testCase (T.unpack layout <> ": 100000 live and archive rows, complete projections") $ do
  available <- assertSession p (scalarBool "select exists (select from pg_extension where extname = 'pg_partman')")
  required <- (== Just "1") <$> lookupEnv "PGMQ_REQUIRE_PARTMAN"
  let partitioned = layout == "numeric" || layout == "time"
  if partitioned && not available
    then if required then assertFailure "required pg_partman is absent" else putStrLn "    SKIPPED: pg_partman is not installed"
    else withTestFixture p $ \TestFixture {pool, queueName} -> flip finally (cleanupQueue pool queueName) $ do
      case layout of
        "ordinary" -> assertSession pool (Sessions.createQueue queueName)
        "unlogged" -> assertSession pool (Sessions.createUnloggedQueue queueName)
        "numeric" -> assertSession pool (Sessions.createPartitionedQueue (CreatePartitionedQueue queueName "10000" "1000000"))
        "time" -> assertSession pool (Sessions.createPartitionedQueue (CreatePartitionedQueue queueName "1 day" "1000 days"))
        _ -> assertFailure "unknown test layout"
      let q = queueNameToText queueName
      forM_ ["q", "a"] $ \prefix -> do
        table <- assertSession pool (statement (q, prefix) Inspect.formatTableName)
        let qualified = "pgmq." <> Inspect.quoteIdentifier table
            timestamp = if layout == "time" then "date_trunc('day', now()) + case n % 3 when 0 then interval '-1 day' when 1 then interval '0 days' else interval '100 years' end" else "now()"
            columns = "msg_id, vt, enqueued_at, message, headers" <> if prefix == "a" then ", archived_at" else ""
            values = "n, now(), " <> timestamp <> ", jsonb_build_object('n', n, 'body', repeat('x', 128)), '{\"fixture\":true}'::jsonb" <> if prefix == "a" then ", " <> timestamp else ""
        assertSession pool $
          Session.script $
            "insert into " <> qualified <> " (" <> columns <> ") overriding system value select " <> values <> " from generate_series(1, 100000) n; analyze " <> qualified
        when partitioned $ do
          (leaves, hasDefault) <-
            assertSession pool $
              statement () $
                unpreparable
                  ("select count(distinct t.tableoid), bool_or(pg_get_expr(c.relpartbound, c.oid) = 'DEFAULT') from " <> qualified <> " t join pg_class c on c.oid = t.tableoid")
                  mempty
                  (D.singleRow ((,) <$> D.column (D.nonNullable D.int8) <*> D.column (D.nonNullable D.bool)))
          assertBool "rows occupy multiple leaves" (leaves >= (3 :: Int64))
          assertBool "rows occupy the default partition" hasDefault
        verifyReads pool q prefix
        let pageSql = if prefix == "q" then toSql (Inspect.peekStatement table) else toSql (Inspect.peekArchivedStatement table)
            lookupSql = if prefix == "q" then toSql (Inspect.lookupStatement table) else toSql (Inspect.lookupArchivedStatement table)
            queries :: [(Text, Text)]
            queries =
              [ ("first", T.replace "$2" "50" (T.replace "$1" "NULL" pageSql)),
                ("late", T.replace "$2" "50" (T.replace "$1" "99950" pageSql)),
                ("present", T.replace "$1" "99999" lookupSql),
                ("absent", T.replace "$1" "100001" lookupSql)
              ]
        plans <-
          mapM
            ( \(label, sql) -> do
                plan <- assertSession pool (explain sql)
                pure (object ["case" .= label, "sql" .= sql, "explain" .= plan])
            )
            queries
        output <- lookupEnv "PGMQ_INSPECTION_PLANS"
        case output of
          Nothing -> pure ()
          Just directory -> do
            createDirectoryIfMissing True directory
            version <- maybe "1.13.0" id <$> lookupEnv "PGMQ_TEST_SCHEMA_VERSION"
            postgres <- assertSession pool (scalarText "select version()")
            partman <- assertSession pool (scalarText "select coalesce((select extversion from pg_extension where extname = 'pg_partman'), 'absent')")
            Aeson.encodeFile (directory <> "/" <> version <> "-" <> T.unpack layout <> "-" <> T.unpack prefix <> ".json") $
              object ["schema" .= version, "postgres" .= postgres, "pg_partman" .= partman, "layout" .= layout, "table" .= table, "rows" .= (100000 :: Int), "plans" .= plans]

verifyReads :: Pool.Pool -> Text -> Text -> IO ()
verifyReads pool queue prefix = do
  let page cursor =
        if prefix == "q"
          then map messageId . V.toList <$> assertSession pool (Sessions.peekMessages (PeekMessages queue cursor 50))
          else map (messageId . archivedMessage) . V.toList <$> assertSession pool (Sessions.peekArchivedMessages (PeekMessages queue cursor 50))
      lookupId mid =
        if prefix == "q"
          then fmap messageId <$> assertSession pool (Sessions.lookupMessage (LookupMessage queue mid))
          else fmap (messageId . archivedMessage) <$> assertSession pool (Sessions.lookupArchivedMessage (LookupMessage queue mid))
  page Nothing >>= (@?= map MessageId [1 .. 50])
  page (Just (MessageId 99950)) >>= (@?= map MessageId [99951 .. 100000])
  lookupId (MessageId 99999) >>= (@?= Just (MessageId 99999))
  lookupId (MessageId 100001) >>= (@?= Nothing)

explain :: Text -> Session Value
explain sql = statement () $ unpreparable ("explain (analyze, buffers, format json) " <> sql) mempty (D.singleRow (D.column (D.nonNullable D.json)))

scalarBool :: Text -> Session Bool
scalarBool sql = statement () (unpreparable sql mempty (D.singleRow (D.column (D.nonNullable D.bool))))

scalarText :: Text -> Session Text
scalarText sql = statement () (unpreparable sql mempty (D.singleRow (D.column (D.nonNullable D.text))))
