{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Schema introspection tests
-- Verifies that pgmq.message_record type matches decoder expectations
module SchemaSpec (tests) where

import Data.Text (Text)
import Data.Vector qualified as V
import Hasql.Decoders qualified as D
import Hasql.Pool qualified as Pool
import Hasql.Session (Session, statement)
import Hasql.Statement (Statement (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import TestUtils (assertSession)

-- | All schema introspection tests
tests :: Pool.Pool -> TestTree
tests p =
  testGroup
    "Schema Introspection"
    [ testMessageRecordColumnOrder p,
      testMessageRecordColumnTypes p
    ]

-- | Expected column order for pgmq.message_record
-- This must match the order in Pgmq.Hasql.Decoders.messageDecoder
expectedMessageRecordColumns :: [(Text, Text)]
expectedMessageRecordColumns =
  [ ("msg_id", "int8"),
    ("read_ct", "int4"),
    ("enqueued_at", "timestamptz"),
    ("last_read_at", "timestamptz"),
    ("vt", "timestamptz"),
    ("message", "jsonb"),
    ("headers", "jsonb")
  ]

-- | Query to get column order of pgmq.message_record type
-- Returns columns in their attribute order
messageRecordColumnsQuery :: Statement () (V.Vector (Text, Text))
messageRecordColumnsQuery =
  Statement
    sql
    mempty
    decoder
    True
  where
    sql =
      "SELECT a.attname::text, t.typname::text \
      \FROM pg_type typ \
      \JOIN pg_namespace ns ON typ.typnamespace = ns.oid \
      \JOIN pg_attribute a ON a.attrelid = typ.typrelid \
      \JOIN pg_type t ON a.atttypid = t.oid \
      \WHERE ns.nspname = 'pgmq' \
      \  AND typ.typname = 'message_record' \
      \  AND a.attnum > 0 \
      \ORDER BY a.attnum"

    decoder :: D.Result (V.Vector (Text, Text))
    decoder = D.rowVector $ (,) <$> D.column (D.nonNullable D.text) <*> D.column (D.nonNullable D.text)

-- | Query columns of pgmq.message_record
getMessageRecordColumns :: Session (V.Vector (Text, Text))
getMessageRecordColumns = statement () messageRecordColumnsQuery

-- | Test that pgmq.message_record columns match expected order
-- This catches schema drift where columns are reordered or renamed
testMessageRecordColumnOrder :: Pool.Pool -> TestTree
testMessageRecordColumnOrder p = testCase "message_record column order matches decoder" $ do
  columns <- assertSession p getMessageRecordColumns
  let actualColumns = V.toList columns
      expectedColNames = map fst expectedMessageRecordColumns
      actualColNames = map fst actualColumns

  assertEqual
    "Column order must match decoder expectations (msg_id, read_ct, enqueued_at, last_read_at, vt, message, headers)"
    expectedColNames
    actualColNames

-- | Test that pgmq.message_record column types match expected types
-- This catches type changes that would cause decoding failures
testMessageRecordColumnTypes :: Pool.Pool -> TestTree
testMessageRecordColumnTypes p = testCase "message_record column types match decoder" $ do
  columns <- assertSession p getMessageRecordColumns
  let actualColumns = V.toList columns

  assertEqual
    "Column types must match decoder expectations"
    expectedMessageRecordColumns
    actualColumns
