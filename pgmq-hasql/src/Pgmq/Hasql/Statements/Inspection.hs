-- | Non-destructive inspection statements over queue and archive tables.
--
-- Upstream pgmq offers no read that does not lease (@pgmq.read@,
-- @read_with_poll@, and @pop@ all set @vt@ and bump @read_ct@), no read of an
-- archive table, and no fetch by id. These statements are plain @SELECT@s over
-- the tables the upstream schema defines; they shadow no @pgmq.*@ function and
-- add no migration, which is exactly the kind of hand-written statement
-- @docs/design/012-vendor-upstream-pgmq-sql.md@ permits. None of them modifies
-- a row.
--
-- The physical table is resolved on the server by 'formatTableName' (upstream's
-- @pgmq.format_table_name@, which lowercases and rejects @$@, @;@, @--@, and
-- @'@) and spliced into the SQL as a double-quoted identifier by
-- 'quoteIdentifier'. Because the SQL text differs per table, every builder uses
-- 'unpreparable': hasql caches prepared statements by SQL text, and a prepared
-- statement per queue would grow the per-connection cache without bound.
--
-- Pages are keyset pages: @msg_id > cursor@, ordered by @msg_id@, bounded by
-- @LIMIT@, never @OFFSET@. See @docs/design/019-non-destructive-inspection-reads.md@.
module Pgmq.Hasql.Statements.Inspection
  ( formatTableName,
    quoteIdentifier,
    peekStatement,
    peekArchivedStatement,
    lookupStatement,
    lookupArchivedStatement,
  )
where

import Data.Text qualified as T
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Statement (Statement, preparable, unpreparable)
import Pgmq.Hasql.Decoders (archivedMessageDecoder, messageDecoder)
import Pgmq.Hasql.Encoders (messageIdValue)
import Pgmq.Hasql.Prelude
import Pgmq.Types (ArchivedMessage, Message, MessageId)

-- | @select pgmq.format_table_name($1, $2)@: the physical table name for a
-- queue name and a prefix (@"q"@ for the queue table, @"a"@ for the archive).
-- Raises the server's own error for names containing @$@, @;@, @--@, or @'@.
formatTableName :: Statement (Text, Text) Text
formatTableName = preparable sql encoder decoder
  where
    sql = "select pgmq.format_table_name($1, $2)"
    encoder =
      (fst >$< E.param (E.nonNullable E.text))
        <> (snd >$< E.param (E.nonNullable E.text))
    decoder = D.singleRow (D.column (D.nonNullable D.text))

-- | Double-quote a SQL identifier, doubling any embedded double quote, so a
-- physical table name such as @q_odd-name@ or @q_myqueue@ can be spliced into
-- SQL text safely.
quoteIdentifier :: Text -> Text
quoteIdentifier ident = "\"" <> T.replace "\"" "\"\"" ident <> "\""

-- | The seven columns 'messageDecoder' expects, in its order.
messageColumns :: Text
messageColumns = "msg_id, read_ct, enqueued_at, last_read_at, vt, message, headers"

-- | 'messageColumns' followed by the archive table's @archived_at@.
archivedColumns :: Text
archivedColumns = messageColumns <> ", archived_at"

pageEncoder :: E.Params (Maybe MessageId, Int32)
pageEncoder =
  (fst >$< E.param (E.nullable messageIdValue))
    <> (snd >$< E.param (E.nonNullable E.int4))

messageIdEncoder :: E.Params MessageId
messageIdEncoder = E.param (E.nonNullable messageIdValue)

-- | A keyset page of a queue table. The argument is the already-resolved
-- physical table name (from 'formatTableName' with prefix @"q"@). Returns at
-- most @limit@ rows with @msg_id@ strictly greater than the cursor, in
-- ascending @msg_id@ order, and modifies nothing.
peekStatement :: Text -> Statement (Maybe MessageId, Int32) (Vector Message)
peekStatement table = unpreparable (pageSql messageColumns table) pageEncoder (D.rowVector messageDecoder)

-- | A keyset page of an archive table (from 'formatTableName' with prefix
-- @"a"@), each row carrying @archived_at@. Same paging rules as
-- 'peekStatement'.
peekArchivedStatement :: Text -> Statement (Maybe MessageId, Int32) (Vector ArchivedMessage)
peekArchivedStatement table = unpreparable (pageSql archivedColumns table) pageEncoder (D.rowVector archivedMessageDecoder)

-- | One row of a queue table by id, or 'Nothing'. Modifies nothing.
lookupStatement :: Text -> Statement MessageId (Maybe Message)
lookupStatement table = unpreparable (lookupSql messageColumns table) messageIdEncoder (D.rowMaybe messageDecoder)

-- | One row of an archive table by id, or 'Nothing'.
lookupArchivedStatement :: Text -> Statement MessageId (Maybe ArchivedMessage)
lookupArchivedStatement table = unpreparable (lookupSql archivedColumns table) messageIdEncoder (D.rowMaybe archivedMessageDecoder)

-- | @$1@ is the exclusive cursor (nullable), @$2@ the page size. The explicit
-- null test keeps the no-cursor case independent of where ids start.
pageSql :: Text -> Text -> Text
pageSql columns table =
  "select "
    <> columns
    <> " from pgmq."
    <> quoteIdentifier table
    <> " where ($1::bigint is null or msg_id > $1) order by msg_id asc limit $2"

lookupSql :: Text -> Text -> Text
lookupSql columns table =
  "select " <> columns <> " from pgmq." <> quoteIdentifier table <> " where msg_id = $1"
