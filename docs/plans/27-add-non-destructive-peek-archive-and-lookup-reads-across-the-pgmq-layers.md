---
id: 27
slug: add-non-destructive-peek-archive-and-lookup-reads-across-the-pgmq-layers
title: "Add non-destructive peek, archive, and lookup reads across the pgmq layers"
kind: exec-plan
created_at: 2026-10-01T00:15:41Z
intention: "intention_01m3tcw9vmeeftdtqj53d1d6nb"
master_plan: "docs/masterplans/7-expose-non-destructive-queue-inspection-through-typed-reads-json-codecs-and-the-pgmq-inspect-sister-package.md"
provenance:
  created_by:
    model: "claude-fable-5-1"
    harness: "claude-code"
    at: 2026-10-01T00:15:41Z
---

# Add non-destructive peek, archive, and lookup reads across the pgmq layers

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.
If durable project context changes, update or create ADRs in docs/adr/ in the same change.


## Purpose / Big Picture

Every read pgmq-hs offers today takes a lease. `readMessage`, `readWithPoll`, `pop`, and the
six grouped reads all call upstream SQL functions that set `vt` (the visibility timeout: the
moment a message becomes readable again) into the future and increment `read_ct` (the read
counter: how many times a message has been handed to a consumer) as a side effect of returning
rows. An operator who "just looks at" a queue through any of them steals every returned message
from the real consumers for the duration of the visibility timeout and inflates a counter that
retry and dead-letter policies depend on. There is also no way to read an archive table back
(`archiveMessage` writes into `pgmq.a_<queue>` and nothing reads it), and no way to fetch one
message by its id.

Improvement request `IR-1` (`docs/improvement-requests/expose-non-destructive-queue-inspection-reads.md`),
filed from the keiro runtime UI initiative, asks for three reads that observe without
disturbing: a bounded keyset-paged peek over a queue table, the same over an archive table, and
a fetch-by-id in each. This plan implements them through every layer of the family, following
the repository's layering recipe: a hand-written SQL statement in `pgmq-hasql`, a session over
it, a re-export from the `Pgmq` umbrella module, a constructor on the `Pgmq` effect in
`pgmq-effectful`, and a case in each of the two interpreters (plain and OpenTelemetry-traced).
It also adds one small companion, `queueMetricsUnvalidated`, which runs the existing metrics
projection for a queue named by plain text, because the HTTP surface the parent MasterPlan
builds later must report metrics for queues whose names this library's validator rejects.

After this plan, a user can do this against a queue that a consumer is actively polling, and
the consumer will never notice:

```haskell
import Pgmq
import Hasql.Pool qualified as Pool

-- Peek at the first fifty messages without leasing any of them.
Right page <- Pool.use pool (peekMessages (PeekMessages "orders" Nothing 50))
-- Continue from the last id seen; the cursor is exclusive.
Right next <- Pool.use pool (peekMessages (PeekMessages "orders" (Just (messageId (V.last page))) 50))
-- Browse what has been archived, with the archival timestamp.
Right archived <- Pool.use pool (peekArchivedMessages (PeekMessages "orders" Nothing 50))
-- Fetch one message by id; Nothing means it is not there (deleted, archived, or never sent).
Right found <- Pool.use pool (lookupMessage (LookupMessage "orders" (MessageId 42)))
```

The same five operations are available through the `Pgmq` effect (`Pgmq.Effectful.peekMessages`
and friends) under both `runPgmq` and `runPgmqTraced`, so an effect program can inspect a queue
and a mock interpreter can answer those inspections without a database. The proof that the
reads are non-destructive is a test that reads the raw `vt`, `read_ct`, and `last_read_at`
cells of every row before and after a sequence of peeks and asserts they are identical, and a
second test in which a consumer polling the queue concurrently with a loop of peeks still
receives every message exactly once with `read_ct` equal to one.


## Progress

- [ ] M1: `ArchivedMessage` added to `pgmq-core/src/Pgmq/Types.hs` and exported; the integration rule for its JSON instances applied (instances and golden file added if the policy from plan 28 (`docs/plans/28-provide-stable-json-codecs-for-the-inspection-facing-records.md`) is already present, otherwise deferred)
- [ ] M1: `PeekMessages` and `LookupMessage` added to `pgmq-hasql/src/Pgmq/Hasql/Statements/Types.hs`
- [ ] M1: `archivedMessageDecoder` added to `pgmq-hasql/src/Pgmq/Hasql/Decoders.hs`
- [ ] M1: `pgmq-hasql/src/Pgmq/Hasql/Statements/Inspection.hs` created with `formatTableName`, `quoteIdentifier`, and the four `unpreparable` statement builders; listed in the cabal file and re-exported from `Pgmq.Hasql.Statements`
- [ ] M1: `queueMetricsUnvalidated` added to `pgmq-hasql/src/Pgmq/Hasql/Statements/QueueObservability.hs`
- [ ] M1: five sessions added to `pgmq-hasql/src/Pgmq/Hasql/Sessions.hs` and exported, with the records and `ArchivedMessage`, from `pgmq-hasql/src/Pgmq.hs`
- [ ] M1: `pgmq-hasql/test/InspectionSpec.hs` written, wired into `Main.hs` and the cabal `other-modules`, red before the statements exist and green after; covers byte-identical rows, the concurrent consumer, exactly-once paging, archive timestamps, lookups, limit semantics, the missing-queue error, no `OFFSET`, metrics parity, and a partitioned queue under the partman guard
- [ ] M1: `nix fmt` clean; `cabal test pgmq-hasql:pgmq-hasql-test` green natively and with `PGMQ_TEST_SCHEMA_VERSION=1.12.0`; committed
- [ ] M2: five constructors on `Pgmq` in `pgmq-effectful/src/Pgmq/Effectful/Effect.hs` with smart constructors
- [ ] M2: cases in `pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs` and `pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs` (with the `queueOpText` helper and the four `pgmq.peek`/`pgmq.lookup` labels)
- [ ] M2: re-exports from `pgmq-effectful/src/Pgmq/Effectful.hs`; compile witnesses added to both `UmbrellaExportsSpec` modules
- [ ] M2: `pgmq-hasql/test/InspectionForeignNameSpec.hs` written on a dedicated instance, proving the mixed-case and hyphenated paths through every new read; wired into `Main.hs` and the cabal file
- [ ] M2: `featureTests` in `pgmq-effectful/test/TracedInterpreterSpec.hs` extended with the inspection case for both interpreters, including span assertions when traced
- [ ] M2: `nix fmt` clean; `cabal test all` green; committed
- [ ] M3: `docs/design/019-non-destructive-inspection-reads.md` written
- [ ] M3: Haddocks on every new type, statement, session, and effect operation state the contract
- [ ] M3: capability record written under `docs/capabilities/` with the handle `okf id next` returned; `index.md` and `log.md` updated; `just docs-check` green
- [ ] M3: `Unreleased` sections appended to `CHANGELOG.md`, `pgmq-core/CHANGELOG.md`, `pgmq-hasql/CHANGELOG.md`, and `pgmq-effectful/CHANGELOG.md`, the effectful one naming the breaking constructor additions
- [ ] M3: `IR-1` set to `completed` with `completedAt` and `resolution`; bundle log appended; committed


## Surprises & Discoveries

Document unexpected behaviors, bugs, optimizations, or insights discovered during
implementation. Provide concise evidence.

(None yet.)


## Decision Log

- Decision: The inspection reads take the queue name as plain `Text` (field
  `unvalidatedQueueName`), not as `QueueName`.
  Rationale: `IR-1` requires the reads to work for foreign and mixed-case names that
  `parseQueueName` rejects, because an inspection surface sees whatever exists in the database.
  A caller holding a validated `QueueName` passes `queueNameToText`. The MasterPlan's
  Integration Points fix this shape for every consumer.
  Date: 2026-09-30
- Decision: Resolve the physical table name on the server with `pgmq.format_table_name` in a
  first statement, then run the read as a second, `unpreparable` statement whose SQL embeds the
  double-quoted identifier.
  Rationale: upstream's function is the single source of truth for how a queue name becomes a
  table name (it lowercases and rejects `$`, `;`, `--`, and `'`); duplicating that rule in
  Haskell would drift. The read's SQL text differs per table, and hasql caches prepared
  statements per distinct SQL text, so a prepared statement per queue would grow the cache
  without bound on a server inspecting many queues. The extra round-trip runs inside the same
  session on the same pooled connection and is acceptable for an inspection path.
  Date: 2026-09-30
- Decision: `ArchivedMessage` nests a `Message` and adds `archivedAt`, rather than repeating
  the seven message fields with new names.
  Rationale: the decoder is `messageDecoder` followed by one more column; `pgmq-core` does not
  enable `DuplicateRecordFields`, so a flat record would need eight prefixed field names. The
  JSON encoding (the concern of plan 28, `docs/plans/28-provide-stable-json-codecs-for-the-inspection-facing-records.md`, or this plan's under the integration rule) flattens it so
  the wire shape is one object.
  Date: 2026-09-30
- Decision: The cursor predicate is `($1::bigint is null or msg_id > $1)`, and the page size is
  passed straight to `LIMIT`.
  Rationale: an explicit null test keeps the "no cursor" case honest instead of relying on
  `coalesce($1, 0)` and the assumption that ids start above zero; passing the limit through
  means a caller's non-positive limit gets PostgreSQL's own behaviour (zero rows for `0`, an
  error for a negative value), which the HTTP layer validates before it ever reaches here.
  Date: 2026-09-30
- Decision: Add `queueMetricsUnvalidated :: Text -> Session QueueMetrics` (and its effect
  constructor) in this plan rather than in the sister-package plan.
  Rationale: it belongs to the same lenient-name family as the four reads, reuses the metrics
  projection verbatim, and the MasterPlan assigns every library-layer read of this initiative to
  this plan so the sister package adds no statement of its own.
  Date: 2026-09-30
- Decision: The traced interpreter labels the new operations with this library's own names
  (`pgmq.peek`, `pgmq.peek_archive`, `pgmq.lookup_message`, `pgmq.lookup_archived_message`)
  and reuses `pgmq.metrics` for the lenient metrics read.
  Rationale: no upstream function backs the four reads, exactly as for the catalog-backed
  `pgmq.list_fifo_indexes`, so a `pgmq.<function>` label would be a lie; the lenient metrics
  read does call `pgmq.metrics`.
  Date: 2026-09-30


## Outcomes & Retrospective

Summarize outcomes, gaps, and lessons learned at major milestones or at completion.
Compare the result against the original purpose. Before marking the plan complete,
distill durable project context from the Decision Log, Surprises & Discoveries, and
this section into docs/adr/. Keep task-local execution details here.

(To be filled during and after implementation.)


## Context and Orientation

### The repository and its layering recipe

`pgmq-hs` is a multi-package Cabal project; `cabal.project` lists `pgmq-core`, `pgmq-hasql`,
`pgmq-effectful`, `pgmq-migration`, `pgmq-config`, and `pgmq-bench`. The toolchain comes from
Nix: run every build and test command inside `nix develop`, which provides GHC 9.12.4, cabal, a
PostgreSQL server binary the test suites start themselves, and `nix fmt` (fourmolu for Haskell,
cabal-gild for cabal files). The pre-commit hook runs the formatter and rejects an unformatted
commit, so run `nix fmt` before every commit. Nothing needs an external database.

This plan touches three packages, and every operation it adds passes through each of them in
the same order, which the repository calls its layering recipe:

1. `pgmq-core/src/Pgmq/Types.hs` holds the plain domain types (`Message`, `MessageId`, `Queue`,
   `UnvalidatedQueue`, `QueueName`, …) with no database dependency.
2. `pgmq-hasql/src/Pgmq/Hasql/Statements/*.hs` holds one `Hasql.Statement.Statement` per
   operation: SQL text plus a parameter encoder (from `Pgmq.Hasql.Encoders`) and a row decoder
   (from `Pgmq.Hasql.Decoders`). Argument records live in
   `pgmq-hasql/src/Pgmq/Hasql/Statements/Types.hs`. `Pgmq.Hasql.Statements` re-exports every
   statement module.
3. `pgmq-hasql/src/Pgmq/Hasql/Sessions.hs` wraps each statement in a `Hasql.Session.Session`.
4. `pgmq-hasql/src/Pgmq.hs` is the flat umbrella module a consumer imports; it re-exports the
   sessions, the argument records, and the core types.
5. `pgmq-effectful/src/Pgmq/Effectful/Effect.hs` declares the `Pgmq` effect as a GADT with one
   constructor per operation and one smart constructor (`send . Constructor`) each.
6. `pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs` (`runPgmq`) and
   `pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs` (`runPgmqTraced`) each have one
   case per constructor; the plain one calls the session through `runSession`, the traced one
   wraps the same session in an OpenTelemetry span through `withTracedOp`.
7. `pgmq-effectful/src/Pgmq/Effectful.hs` is the effect-side umbrella.

A `Session` is hasql's unit of work on one connection; `Hasql.Pool.use pool session` checks out a
pooled connection, runs the session, and returns `Either UsageError a`. A `Statement params
result` is built with `Hasql.Statement.preparable` (hasql asks the server to prepare it once per
connection and caches it by SQL text) or `Hasql.Statement.unpreparable` (sent as plain text every
time; the right choice for SQL whose text varies). Both take `Text` SQL, an `Encoders.Params`,
and a `Decoders.Result`. `Hasql.Statement.toSql :: Statement params result -> Text` returns the
SQL text, which this plan's tests use to pin the absence of `OFFSET`. These names were verified
against the pinned hasql source (`cabal.project` pins commit
`aa3d6ae499e187c291422443f221f9f486c43a9e`; the mori corpus copy is at
`mori registry show hasql/hasql --full`, file `hasql/src/library/Hasql/Statement.hs`, which
exports `Statement`, `preparable`, `unpreparable`, `refineResult`, and `toSql`).

### Terms used in this plan

A *lease* is what every existing read takes: it sets a message's `vt` into the future so other
consumers cannot see it, and bumps `read_ct`. A *non-destructive read* modifies no row at all. A
*keyset page* is a page defined by "the rows whose `msg_id` is greater than this cursor, in
`msg_id` order, at most this many"; it is stable under concurrent inserts and deletes, unlike an
`OFFSET` page, which shifts when rows before it disappear. An *exclusive cursor* is a cursor
whose own row is not part of the page. A *foreign name* is a queue name created by some other
client that `Pgmq.Types.parseQueueName` would reject (anything outside `[a-z0-9_]{1,47}`); a
*mixed-case name* is the most common foreign name and the one with the sharpest aliasing hazards
(design note 016). The *physical table* of a queue is `pgmq.q_<lowercased name>`; its archive
is `pgmq.a_<lowercased name>`. *ephemeral-pg* is the library the test suites use to start a
throwaway PostgreSQL. An *OKF bundle* is a directory of Markdown records with YAML frontmatter
validated by the `okf` command against a pinned profile; `docs/capabilities` and
`docs/improvement-requests` are two such bundles.

### What the tables look like and why the existing reads lease

The schema comes from the vendored upstream SQL at
`vendor/pgmq/pgmq-extension/sql/pgmq.sql` (PGMQ 1.13.0, installed natively by `pgmq-migration`).
`pgmq.create_non_partitioned` creates the queue table and its archive:

```sql
CREATE TABLE IF NOT EXISTS pgmq.q_<name> (
    msg_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    read_ct INT DEFAULT 0 NOT NULL,
    enqueued_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
    last_read_at TIMESTAMP WITH TIME ZONE,
    vt TIMESTAMP WITH TIME ZONE NOT NULL,
    message JSONB,
    headers JSONB
);

CREATE TABLE IF NOT EXISTS pgmq.a_<name> (
  msg_id BIGINT PRIMARY KEY,
  read_ct INT DEFAULT 0 NOT NULL,
  enqueued_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
  last_read_at TIMESTAMP WITH TIME ZONE,
  archived_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
  vt TIMESTAMP WITH TIME ZONE NOT NULL,
  message JSONB,
  headers JSONB
);
```

Partitioned queues (`pgmq.create_partitioned`) create the same columns on a parent table that
pg_partman splits into child partitions; a `SELECT` against the parent sees every partition, so
the reads in this plan work on partitioned queues without special handling. `pgmq.archive`
deletes from the queue table and inserts the same `msg_id`, `vt`, `read_ct`, `enqueued_at`,
`last_read_at`, `message`, and `headers` into the archive table, whose `archived_at` defaults to
`now()`.

The name-to-table rule is one upstream function, which this plan calls rather than copies:

```sql
CREATE FUNCTION pgmq.format_table_name(queue_name text, prefix text)
RETURNS TEXT AS $$
BEGIN
    IF queue_name ~ '\$|;|--|'''
    THEN
        RAISE EXCEPTION 'queue name contains invalid characters: $, ;, --, or \''';
    END IF;
    RETURN lower(prefix || '_' || queue_name);
END;
$$ LANGUAGE plpgsql;
```

The only other server-side check on a name is `pgmq.validate_queue_name`, which rejects names
longer than 47 characters. So the server accepts `MyQueue`, `odd-name`, and `Ünïcode`; this
library's `parseQueueName` rejects all three, for the aliasing reasons design note 016 records.

Every upstream read leases. `pgmq.read` is representative: it selects visible ids, then
`UPDATE ... SET vt = clock_timestamp() + <vt>, read_ct = read_ct + 1, last_read_at = clock_timestamp()`,
and returns the updated rows. Upstream provides no function that selects without updating, no
function that reads an archive table, and no function that fetches one message by id. Design
note 012 (`docs/design/012-vendor-upstream-pgmq-sql.md`) permits hand-written statements for
exactly this case: reads upstream has no equivalent for, written as plain `SELECT`s over tables
the upstream schema defines, shadowing no `pgmq.*` function and adding no migration.

### The code this plan builds on, as it is today

`pgmq-hasql/src/Pgmq/Hasql/Decoders.hs` decodes a message row in the column order the upstream
functions return, which is also the order this plan's `SELECT`s will list:

```haskell
-- | Decoder for pgmq.message_record type
-- Column order matches pgmq SQL: msg_id, read_ct, enqueued_at, last_read_at, vt, message, headers
messageDecoder :: D.Row Message
messageDecoder =
  ( \msgId readCt enqueuedAt lastReadAt vt body headers ->
      Message
        { messageId = msgId,
          visibilityTime = vt,
          enqueuedAt = enqueuedAt,
          lastReadAt = lastReadAt,
          readCount = fromIntegral readCt,
          body = body,
          headers = headers
        }
  )
    <$> messageIdDecoder -- msg_id
    <*> D.column (D.nonNullable D.int4) -- read_ct (INTEGER -> Int32)
    <*> D.column (D.nonNullable D.timestamptz) -- enqueued_at
    <*> D.column (D.nullable D.timestamptz) -- last_read_at
    <*> D.column (D.nonNullable D.timestamptz) -- vt
    <*> (MessageBody . fromMaybe Aeson.Null <$> D.column (D.nullable D.jsonb)) -- message (SQL NULL -> JSON null)
    <*> D.column (D.nullable D.jsonb) -- headers
```

`pgmq-hasql/src/Pgmq/Hasql/Encoders.hs` exports `messageIdValue :: E.Value MessageId`
(`unMessageId >$< E.int8`) and `queueNameEncoder :: E.Params QueueName`, which this plan reuses.
The module imports `Pgmq.Hasql.Prelude`, which re-exports `Text`, `Int32`, `Int64`, `Vector`,
`Generic`, `(>$<)`, and all of `Control.Lens` with `generic-lens` labels; the statement modules
import the same prelude.

`pgmq-hasql/src/Pgmq/Hasql/Statements/QueueObservability.hs` already has the lenient listing and
the metrics projection this plan generalises:

```haskell
listQueuesUnvalidated :: Statement () [UnvalidatedQueue]
listQueuesUnvalidated = preparable sql E.noParams decoder
  where
    sql = "select * from pgmq.list_queues()"
    decoder = D.rowList unvalidatedQueueDecoder

-- | Metrics with a stable projection on pgmq 1.12 and 1.13.
-- The JSON record lookup preserves SQL NULL for the missing 1.12 attribute.
queueMetrics :: Statement QueueName QueueMetrics
queueMetrics = preparable sql queueNameEncoder decoder
  where
    sql =
      "select m.queue_name, m.queue_length, m.newest_msg_age_sec, m.oldest_msg_age_sec, \
      \m.total_messages, m.scrape_time, m.queue_visible_length, \
      \(to_jsonb(m)->>'default_partition_length')::bigint \
      \from pgmq.metrics($1) as m"
    decoder = D.singleRow queueMetricsDecoder
```

`queueMetrics` is typed over `QueueName`, so it cannot be called for a foreign name even though
`pgmq.metrics(text)` accepts one; `queueMetricsUnvalidated` closes that gap with the same SQL and
a `text` parameter.

`pgmq-hasql/src/Pgmq/Hasql/Sessions.hs` imports `Hasql.Session (Session, statement)` and wraps
every statement as `name args = statement args Stmt.name`. `pgmq-hasql/src/Pgmq.hs` re-exports
the sessions in sectioned export lists (`-- * Queue Management`, `-- * Message Operations`, …)
and the argument records under `-- * Types`.

`pgmq-effectful/src/Pgmq/Effectful/Effect.hs` declares `data Pgmq :: Effect where` with
constructors such as `ReadMessage :: ReadMessage -> Pgmq m (Vector Message)` and
`QueueMetrics :: QueueName -> Pgmq m QueueMetrics`, importing the argument records by type only
(`import Pgmq.Hasql.Statements.Types (… ReadMessage, …)` without `(..)`) so the record
constructors never clash with the GADT constructors of the same name. Each smart constructor is
`readMessage = send . ReadMessage`. The plain interpreter's cases are
`ReadMessage query -> runSession pool $ Sessions.readMessage query`. The traced interpreter
matches the record constructor through the qualified alias to pull the destination out,
`ReadMessage query@(Types.ReadMessage qn _ _ _) -> withTracedOp config pool (receiveOp "pgmq.read" qn) $ Sessions.readMessage query`,
and builds its span description with these helpers:

```haskell
-- | 'OpInfo' for a non-messaging operation scoped to a single queue.
queueOp :: Text -> OTel.SpanKind -> QueueName -> OpInfo
queueOp fn kind qn = (defaultOpInfo fn kind) {opDestination = Just (queueNameToText qn)}
```

and, for the one existing operation that no `pgmq.*` function backs:

```haskell
  -- No pgmq function backs this one: it reads the pg_indexes catalog view, so
  -- the span carries this library's own label rather than a pgmq.* name.
  ListFifoIndexQueueNames ->
    withTracedOp config pool (defaultOpInfo "pgmq.list_fifo_indexes" OTel.Internal) $
      Sessions.listFifoIndexQueueNames
```

`pgmq-effectful` enables `OverloadedRecordDot` and `NoFieldSelectors` in its
`default-extensions`; follow the style of the file you are editing (`info.opMessagingKind`,
record-update syntax for `OpInfo`). `pgmq-core` and `pgmq-hasql` do not enable them; `pgmq-hasql`
enables `DuplicateRecordFields`, `NamedFieldPuns`, and `OverloadedLabels`, and field access there
is by plain selector or by `view #field` (the project convention is `generic-lens` plus `lens`;
never add `OverloadedRecordDot` to a package that lacks it).

`isTransient` in `pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs` is untouched by this plan.
The reads surface failures exactly as every other operation does: a `PgmqSessionError` wrapping
hasql's `SessionError`, through the one shared `fromUsageError`, under both interpreters.

### The lenient-name path and the mixed-case hazard

`parseQueueName` rejects uppercase because pgmq's SQL keeps three views of a name that agree only
for lowercase input: the physical table is lowercased by `format_table_name`, `pgmq.meta` stores
the caller's original casing, and the notification trigger looks up the lowercased name. The
consequences (`MyQueue` and `myqueue` interleaving in one table; a throttle row the trigger never
matches) are documented in `docs/design/016-queue-name-validation.md` and demonstrated live by
`pgmq-hasql/test/AliasingSpec.hs`. The reads in this plan do not create, rename, or configure
anything, so they introduce no new hazard; they simply show what is there, which is what an
inspection surface must do. They must, however, be tested on a *dedicated* PostgreSQL instance
when they construct mixed-case metadata, because a single mixed-case row in `pgmq.meta` makes the
typed `listQueues` fail to decode for every test sharing that database; `AliasingSpec.hs` and
`MixedCaseRemediationSpec.hs` already follow that rule with `withResource acquireDb releaseDb`,
and `InspectionForeignNameSpec.hs` copies their plumbing.

### Test infrastructure this plan builds on

`pgmq-hasql/test/EphemeralDb.hs` exports `withPgmqDb :: (Pool.Pool -> Database -> IO a) -> IO (Either StartError a)`
and `withPgmqPool`, which start a cached ephemeral PostgreSQL pinned to the stable per-uid root
`/tmp/ephpg-pgmq-hs-<uid>`, install the pgmq schema (the full native ledger by default, or the
packaged stock 1.12.0 fixture when `PGMQ_TEST_SCHEMA_VERSION=1.12.0`), try to install
`pg_partman` (failing when `PGMQ_REQUIRE_PARTMAN=1` and it is absent), and hand back a pool of
size three. It also exports `ephemeralConfig :: IO Config` for specs that start their own
cluster, and `TestFixture (..)` with `withTestFixture :: Pool.Pool -> (TestFixture -> IO a) -> IO a`,
which generates a random lowercase queue name (`test_queue_<random Word64>`) for isolation.
`pgmq-hasql/test/TestUtils.hs` exports `assertSession :: Pool.Pool -> Session a -> IO a` (fails
the test on `Left`), `assertSessionFails`, `cleanupQueue :: Pool.Pool -> QueueName -> IO ()`,
`assertRight`, and `assertJust`. `pgmq-hasql/test/Main.hs` starts one shared database and lists
every spec; `AliasingSpec.tests` and `MixedCaseRemediationSpec.tests` take no pool because they
start their own instance. The suite is registered in `pgmq-hasql/pgmq-hasql.cabal` under
`test-suite pgmq-hasql-test`, whose `other-modules` must name every test module; its
`build-depends` already include `hasql`, `hasql-pool`, `vector`, `text`, `time`, `random`,
`ephemeral-pg`, `pg-migrate`, and `pgmq-migration`, and the suite is built with `-threaded`.

`QueueSpec.hs` shows the partman guard this plan copies: a `pgPartmanAvailable` statement
(`select exists (select 1 from pg_extension where extname = 'pg_partman')`) and a `withPartman`
wrapper that runs the action when the extension is present, fails when
`PGMQ_REQUIRE_PARTMAN=1` says it must be, and prints `SKIPPED` otherwise. The partman shell is
`nix develop .#partman`, which provides PostgreSQL with pg_partman and sets
`PGMQ_REQUIRE_PARTMAN=1`.

`pgmq-effectful/test/Main.hs` starts one shared pool for `PlainInterpreterSpec` and
`TracedInterpreterSpec`; `TracedInterpreterSpec.featureTests :: Bool -> TestTree` holds the cases
both interpreters run (the plain spec includes `plainFeatureTests = featureTests False`), each on
its own database through `isolated`, and provides `setupTracer`, `mkUniqueQueue`,
`spansWithFirstWord`, `singleSpan`, `spanName`, `assertAttrText`, `withSemconvOptIn`, and
`session`. `UmbrellaExportsSpec.hs` in each package is a compile witness that imports only the
public umbrella; it is listed in `other-modules` and imported as `UmbrellaExportsSpec ()` by
`Main.hs`.

Two toolchain facts cost time if forgotten: GNU `sed` from the Nix profile shadows BSD `sed`, so
`sed -i '' …` fails (use `sed -i -e …` or edit with a tool); and the session scratchpad path is
too long for a Unix socket, so never point a scratch `postgres` at it.

### Documents that must change

`docs/design/` holds numbered design notes up to `018`; the MasterPlan pre-assigns `019` to this
plan (`020` and `021` belong to plans 28 and 29). `docs/capabilities/` is an OKF bundle under the
`coordination.capabilities` profile (okf-profiles v0.9.0); its records carry `capabilityId`,
`provider`, `status` (`shipped`), `stability` (`experimental`), `since` (a released version or
`unreleased`), `packages`, `interface`, and `evidence`, and its `index.md` and `log.md` must be
updated with every new record. The next free handle is read with
`okf id next docs/capabilities --profile docs/capabilities/profile.dhall CAP`; plan 28 runs in
parallel and may take the same number first, so never hardcode it. `just docs-check` validates
every bundle. `docs/improvement-requests/expose-non-destructive-queue-inspection-reads.md` is
`IR-1` with `status: proposed`; the profile allows `completed`, which requires `completedAt`
(RFC 3339 UTC) and recommends `resolution`, and the bundle's `log.md` is append-only through
`okf log add`. The root `CHANGELOG.md` and the three package changelogs have `## 0.6.1.1`
as their top entry and no `Unreleased` section yet; this plan adds one to each. No version is
bumped: the family's next lockstep release carries every `Unreleased` section (see the
MasterPlan's release note).

### ADRs consulted

`docs/adr/` holds four plain-Markdown records (no profiled bundle):

- [docs/adr/queue-inspection-surface-boundary-and-wire-contract.md](../adr/queue-inspection-surface-boundary-and-wire-contract.md)
  records, for this initiative, that the reads accept any server-accepted name, resolve the
  table through `pgmq.format_table_name`, never lease, never use `OFFSET`, return a typed
  `Nothing` for a missing message and the server's `42P01` for a missing queue, and that
  adding the effect constructors makes the family's next release a major one.
- [docs/adr/pgmq-1.12-1.13-compatibility.md](../adr/pgmq-1.12-1.13-compatibility.md) fixes the
  supported server range, the `QueueMetrics` projection with the nullable eighth column that
  `queueMetricsUnvalidated` reuses verbatim, and the test-isolation rule for metrics.
- [docs/adr/fifo-native-overrides-and-index-upgrade-boundary.md](../adr/fifo-native-overrides-and-index-upgrade-boundary.md)
  forbids overriding upstream function bodies; this plan adds statements, not functions.
- [docs/adr/haskell-dependency-bounds-and-nix-pin-policy.md](../adr/haskell-dependency-bounds-and-nix-pin-policy.md)
  is not affected: no library dependency changes.

Design notes [docs/design/012-vendor-upstream-pgmq-sql.md](../design/012-vendor-upstream-pgmq-sql.md)
(hand-written statements are permitted for reads upstream lacks) and
[docs/design/016-queue-name-validation.md](../design/016-queue-name-validation.md) (the
lenient path) carry the contracts above. Across repositories,
`mori://shinzui/keiro-ui/okf/adrs/concepts/ADR-1` assigns queue-level views, including
non-destructive peek, archive browsing, and message-by-id, to pgmq-hs, and
`mori://shinzui/keiro-ui/okf/adrs/concepts/ADR-3` requires every live view to rest on an
authoritative read path, which these reads are.


## Plan of Work

### Milestone 1: the statements, sessions, and umbrella, proven non-destructive

Scope: `pgmq-core` gains `ArchivedMessage`; `pgmq-hasql` gains the two argument records, the
archive decoder, the `Inspection` statement module, the lenient metrics statement, five
sessions, and the umbrella re-exports; a new `InspectionSpec` proves every property `IR-1`
names at the session layer. At the end, `Pool.use pool (peekMessages …)` works and the suite
shows that a peek changes nothing.

Start with the type. In `pgmq-core/src/Pgmq/Types.hs` add `ArchivedMessage` after `Message` and
export `ArchivedMessage (..)` after `Message (..)`. Apply the integration rule: open the file and
look for a `ToJSON Message` instance and a design-note reference to
`docs/design/020-json-wire-encodings.md` in its Haddock. If both are present, plan 28 has landed
and you add the `ArchivedMessage` instances and golden file described in Concrete Steps; if
neither is present, add only the type and record in the Decision Log that the codec is deferred
to plan 28.

Then the argument records in `pgmq-hasql/src/Pgmq/Hasql/Statements/Types.hs`: `PeekMessages`
(name, optional exclusive cursor, limit) and `LookupMessage` (name, id), both `Generic`. Add
`archivedMessageDecoder` to `pgmq-hasql/src/Pgmq/Hasql/Decoders.hs`. Create
`pgmq-hasql/src/Pgmq/Hasql/Statements/Inspection.hs` with `formatTableName`, `quoteIdentifier`,
and the four builders, add it to `exposed-modules` in `pgmq-hasql/pgmq-hasql.cabal`, and
re-export it from `pgmq-hasql/src/Pgmq/Hasql/Statements.hs`. Add `queueMetricsUnvalidated` to
`QueueObservability.hs`, sharing the projection SQL with `queueMetrics`. Add the five sessions to
`Sessions.hs` and export them, with `PeekMessages (..)`, `LookupMessage (..)`, and
`ArchivedMessage (..)`, from `Pgmq.hs` under a new `-- * Non-destructive Inspection` section.

Write `pgmq-hasql/test/InspectionSpec.hs` before the sessions compile if you like red-first, or
immediately after; either way it must fail without the statements and pass with them. Register
it in `Main.hs` and in the cabal `other-modules`. Run the suite natively, on the stock 1.12
fixture, and in the partman shell.

Acceptance: `cabal test pgmq-hasql:pgmq-hasql-test` passes with the `Inspection` group green;
the "byte-identical" and "concurrent consumer" cases demonstrate non-destruction; the paging
case visits 1000 ids exactly once in 143 pages of at most 7; `toSql` of every builder contains
no `offset`.

### Milestone 2: the effect, both interpreters, and the lenient-name evidence

Scope: `pgmq-effectful` gains five constructors, five smart constructors, five plain cases, five
traced cases with a new `queueOpText` helper, and umbrella re-exports; the foreign-name spec
proves the lenient path on a dedicated instance; the shared feature tests exercise the reads
under both interpreters and assert the spans under the traced one; both compile witnesses
grow.

In `Effect.hs` add the constructors under a `-- Non-destructive inspection` comment and the
smart constructors under `-- Non-destructive Inspection` in the export list. In
`Interpreter.hs` add the five `runSession` cases. In `Interpreter/Traced.hs` add `queueOpText`,
refactor `queueOp` to call it, and add the five traced cases. In `Pgmq/Effectful.hs` export the
five operations, the two records, and `ArchivedMessage`.

Write `pgmq-hasql/test/InspectionForeignNameSpec.hs` on a dedicated instance (copy the
`acquireDb`/`releaseDb` plumbing from `MixedCaseRemediationSpec.hs`, which uses
`ephemeralConfig`), creating a mixed-case and a hyphenated queue through raw
`select pgmq.create(...)`, sending through raw `pgmq.send`, archiving through raw
`pgmq.archive`, and reading everything back through the new sessions with the raw names.
Register it in `Main.hs` (no pool argument) and the cabal file.

Extend `featureTests` in `pgmq-effectful/test/TracedInterpreterSpec.hs` with one inspection case
that runs under both interpreters and, when traced, asserts the `pgmq.peek` and
`pgmq.lookup_message` spans. Add the witnesses to both `UmbrellaExportsSpec.hs` files.

Acceptance: `cabal test all` passes; the foreign-name group shows peeks, lookups, and metrics
succeeding for names `parseQueueName` rejects; the traced run shows a span named
`pgmq.peek <queue>` of kind `Internal` with `db.operation` `pgmq.peek` and a lookup span
carrying `messaging.message.id`.

### Milestone 3: state the contract and close the request

Scope: design note 019, Haddocks, a capability record, changelog sections, and the `IR-1`
closure. Nothing in the code changes except documentation comments.

Write `docs/design/019-non-destructive-inspection-reads.md`. Check every Haddock named in
Concrete Steps. Allocate the capability handle with `okf id next`, write the record, update the
bundle index and log, and run `just docs-check`. Append `Unreleased` sections to the four
changelogs, the effectful one stating that `Pgmq` gained constructors. Set `IR-1` to
`completed`, append the bundle log, and run `just docs-check` again.

Acceptance: `just docs-check` passes with the new capability and the completed request; the
four changelogs each have an `Unreleased` entry; `okf show docs/improvement-requests IR-1`
reports `completed` (or the equivalent `okf` listing shows the new status).


## Concrete Steps

All commands run from the repository root
`/Users/shinzui/Keikaku/bokuno/libraries/pgmq-hs-project/pgmq-hs` inside `nix develop`
(either enter the shell once with `nix develop` or prefix each command with
`nix develop --command`).

### M1: the core type

Edit `pgmq-core/src/Pgmq/Types.hs`. In the export list, after `Message (..),` add
`ArchivedMessage (..),`. After the `Message` declaration add:

```haskell
-- | A row of an archive table @pgmq.a_\<queue\>@: the message exactly as it was
-- when 'archiveMessage' (or @pgmq.archive@ called by any client) moved it out of
-- the queue table, plus the archival timestamp the archive table stamped.
--
-- Archive tables are only ever read by the non-destructive inspection reads
-- (@peekArchivedMessages@, @lookupArchivedMessage@ in pgmq-hasql); pgmq itself
-- never reads them back. See @docs/design/019-non-destructive-inspection-reads.md@.
data ArchivedMessage = ArchivedMessage
  { archivedMessage :: !Message,
    archivedAt :: !UTCTime
  }
  deriving stock (Eq, Generic, Show)
```

Now apply the integration rule. Run:

```bash
grep -n "instance ToJSON Message\|020-json-wire-encodings" pgmq-core/src/Pgmq/Types.hs
```

If the grep prints nothing, plan 28 has not landed: stop here for this file, and add to this
plan's Decision Log "ArchivedMessage JSON instances deferred to plan 28 (policy not yet
present at <date>)". If the grep prints both lines, plan 28 has landed and this plan owns the
`ArchivedMessage` codec. Add, directly after the type, hand-written instances that flatten the
message and follow the policy (snake_case pgmq column names; `Maybe` fields present and
`null`; no Generic deriving):

```haskell
-- | Wire shape (see @docs/design/020-json-wire-encodings.md@): the flattened
-- 'Message' encoding plus @archived_at@. Published encodings are frozen; fields
-- are only ever added.
instance ToJSON ArchivedMessage where
  toJSON (ArchivedMessage msg at) =
    Aeson.object
      [ "msg_id" .= messageId msg,
        "read_ct" .= readCount msg,
        "enqueued_at" .= enqueuedAt msg,
        "last_read_at" .= lastReadAt msg,
        "vt" .= visibilityTime msg,
        "message" .= body msg,
        "headers" .= headers msg,
        "archived_at" .= at
      ]

instance FromJSON ArchivedMessage where
  parseJSON = Aeson.withObject "ArchivedMessage" $ \o -> do
    msg <-
      Message
        <$> o .: "msg_id"
        <*> o .: "vt"
        <*> o .: "enqueued_at"
        <*> o .: "last_read_at"
        <*> o .: "read_ct"
        <*> o .: "message"
        <*> o .: "headers"
    ArchivedMessage msg <$> o .: "archived_at"
```

(`Message`'s positional field order is `messageId`, `visibilityTime`, `enqueuedAt`,
`lastReadAt`, `readCount`, `body`, `headers`; the parser above applies them in that order.
Import `(.:)` and `(.=)` from `Data.Aeson` if plan 28 did not already.) Then add a golden test
beside plan 28's: in `pgmq-core/test/`, following whatever file plan 28 created for the other
records (its name is listed in `pgmq-core/pgmq-core.cabal` under `other-modules`), add a case
that encodes a fixed `ArchivedMessage` and compares it with `goldenVsString` against
`pgmq-core/test/golden/archived-message.json`, and a round-trip case asserting
`decode (encode x) == Just x`. The fixed value:

```haskell
fixedArchived :: ArchivedMessage
fixedArchived =
  ArchivedMessage
    { archivedMessage =
        Message
          { messageId = MessageId 42,
            visibilityTime = read "2026-01-02 03:04:05 UTC",
            enqueuedAt = read "2026-01-02 03:00:00 UTC",
            lastReadAt = Just (read "2026-01-02 03:02:00 UTC"),
            readCount = 2,
            body = MessageBody (Aeson.object ["order" Aeson..= (7 :: Int)]),
            headers = Nothing
          },
      archivedAt = read "2026-01-02 03:05:00 UTC"
    }
```

whose golden file is exactly:

```json
{"msg_id":42,"read_ct":2,"enqueued_at":"2026-01-02T03:00:00Z","last_read_at":"2026-01-02T03:02:00Z","vt":"2026-01-02T03:04:05Z","message":{"order":7},"headers":null,"archived_at":"2026-01-02T03:05:00Z"}
```

(aeson's `object` orders keys by its internal map, not by insertion; if the first run of the
golden test shows a different key order, accept the produced order into the golden file. The
property being pinned is the set of keys and their values, and once the file exists any drift
fails the test.)

### M1: the argument records

Edit `pgmq-hasql/src/Pgmq/Hasql/Statements/Types.hs`. Add to the export list, before the
`-- Topic types` group:

```haskell
    -- Non-destructive inspection (no upstream function)
    PeekMessages (..),
    LookupMessage (..),
```

and at the end of the module:

```haskell
-- | A keyset page over a queue table (@pgmq.q_\<name\>@) or, through
-- @peekArchivedMessages@, an archive table (@pgmq.a_\<name\>@).
--
-- 'unvalidatedQueueName' is any name the server accepts, including names
-- 'Pgmq.Types.parseQueueName' rejects: an inspection surface must show what
-- exists. Pass 'Pgmq.Types.queueNameToText' when you hold a validated name.
-- The physical table is resolved by @pgmq.format_table_name@ on the server.
--
-- 'afterMessageId' is an /exclusive/ cursor: the page starts strictly after
-- it; 'Nothing' starts at the beginning. 'limit' is passed straight to SQL
-- @LIMIT@, so it must be positive (@0@ returns nothing; a negative value is a
-- server error). Pages are ordered by @msg_id@ ascending and never use
-- @OFFSET@, so paging is stable while rows before the cursor are consumed.
data PeekMessages = PeekMessages
  { unvalidatedQueueName :: !Text,
    afterMessageId :: !(Maybe MessageId),
    limit :: !Int32
  }
  deriving stock (Generic)

-- | One message by id in a queue table or, through @lookupArchivedMessage@, an
-- archive table. The name rules are those of 'PeekMessages'.
data LookupMessage = LookupMessage
  { unvalidatedQueueName :: !Text,
    messageId :: !MessageId
  }
  deriving stock (Generic)
```

`Text`, `Int32`, and `Generic` come from `Pgmq.Hasql.Prelude`, already imported; `MessageId` is
already imported from `Pgmq.Types`.

### M1: the archive decoder

Edit `pgmq-hasql/src/Pgmq/Hasql/Decoders.hs`. Export `archivedMessageDecoder` after
`messageIdDecoder`, add `ArchivedMessage (..)` to the `Pgmq.Types` import, and add after
`messageIdDecoder`:

```haskell
-- | Decoder for an archive-table row projected as the seven message columns in
-- 'messageDecoder' order followed by @archived_at@. The inspection statements
-- list the columns explicitly in exactly this order; the archive table's own
-- column order (which puts @archived_at@ before @vt@) is irrelevant.
archivedMessageDecoder :: D.Row ArchivedMessage
archivedMessageDecoder =
  ArchivedMessage
    <$> messageDecoder
    <*> D.column (D.nonNullable D.timestamptz) -- archived_at
```

### M1: the Inspection statement module

Create `pgmq-hasql/src/Pgmq/Hasql/Statements/Inspection.hs`:

```haskell
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

pageEncoder :: E.Params (Maybe MessageId, Int32)
pageEncoder =
  (fst >$< E.param (E.nullable messageIdValue))
    <> (snd >$< E.param (E.nonNullable E.int4))

messageIdEncoder :: E.Params MessageId
messageIdEncoder = E.param (E.nonNullable messageIdValue)

-- | A keyset page of a queue table. The argument is the already-resolved
-- physical table name (from 'formatTableName' with prefix @"q"@).
peekStatement :: Text -> Statement (Maybe MessageId, Int32) (Vector Message)
peekStatement table = unpreparable sql pageEncoder (D.rowVector messageDecoder)
  where
    sql = pageSql messageColumns table

-- | A keyset page of an archive table, each row carrying @archived_at@.
peekArchivedStatement :: Text -> Statement (Maybe MessageId, Int32) (Vector ArchivedMessage)
peekArchivedStatement table = unpreparable sql pageEncoder (D.rowVector archivedMessageDecoder)
  where
    sql = pageSql (messageColumns <> ", archived_at") table

-- | One row of a queue table by id, or 'Nothing'.
lookupStatement :: Text -> Statement MessageId (Maybe Message)
lookupStatement table = unpreparable sql messageIdEncoder (D.rowMaybe messageDecoder)
  where
    sql = lookupSql messageColumns table

-- | One row of an archive table by id, or 'Nothing'.
lookupArchivedStatement :: Text -> Statement MessageId (Maybe ArchivedMessage)
lookupArchivedStatement table = unpreparable sql messageIdEncoder (D.rowMaybe archivedMessageDecoder)
  where
    sql = lookupSql (messageColumns <> ", archived_at") table

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
```

Edit `pgmq-hasql/pgmq-hasql.cabal`: in the library's `exposed-modules`, add
`Pgmq.Hasql.Statements.Inspection` after `Pgmq.Hasql.Statements.Message` (cabal-gild will
re-sort on `nix fmt`). Edit `pgmq-hasql/src/Pgmq/Hasql/Statements.hs` to add
`module Pgmq.Hasql.Statements.Inspection,` to the export list and
`import Pgmq.Hasql.Statements.Inspection` to the imports.

### M1: the lenient metrics statement

Edit `pgmq-hasql/src/Pgmq/Hasql/Statements/QueueObservability.hs`. Export
`queueMetricsUnvalidated` after `queueMetrics`. Hoist the projection out of `queueMetrics` so
both statements share it byte for byte:

```haskell
-- | The metrics projection with a stable shape on pgmq 1.12 and 1.13. The JSON
-- record lookup preserves SQL NULL for the attribute 1.12 lacks.
queueMetricsSql :: Text
queueMetricsSql =
  "select m.queue_name, m.queue_length, m.newest_msg_age_sec, m.oldest_msg_age_sec, \
  \m.total_messages, m.scrape_time, m.queue_visible_length, \
  \(to_jsonb(m)->>'default_partition_length')::bigint \
  \from pgmq.metrics($1) as m"

-- | Metrics with a stable projection on pgmq 1.12 and 1.13.
-- | https://pgmq.github.io/pgmq/api/sql/functions/#metrics
queueMetrics :: Statement QueueName QueueMetrics
queueMetrics = preparable queueMetricsSql queueNameEncoder (D.singleRow queueMetricsDecoder)

-- | 'queueMetrics' for a queue named by plain text: any name the server
-- accepts, including names 'Pgmq.Types.parseQueueName' rejects, so an
-- inspection surface can report metrics for foreign queues. Same SQL, same
-- decoder, same nullable eighth column.
queueMetricsUnvalidated :: Statement Text QueueMetrics
queueMetricsUnvalidated = preparable queueMetricsSql (E.param (E.nonNullable E.text)) (D.singleRow queueMetricsDecoder)
```

(`Text` is already imported from `Data.Text` in that module; `E` is `Hasql.Encoders`.)

### M1: the sessions and the umbrella

Edit `pgmq-hasql/src/Pgmq/Hasql/Sessions.hs`. Add to the export list, before the
`-- FIFO read functions` comment:

```haskell
    -- Non-destructive inspection (no upstream function)
    peekMessages,
    peekArchivedMessages,
    lookupMessage,
    lookupArchivedMessage,
    queueMetricsUnvalidated,
```

Add `import Pgmq.Hasql.Statements.Inspection qualified as Inspect`, change the
`Pgmq.Hasql.Statements.Types` import to include `LookupMessage (..)` and `PeekMessages (..)`
(the other names stay constructor-less), and add `ArchivedMessage` to the `Pgmq.Types` import.
Then, after `allQueueMetrics`:

```haskell
-- | Metrics for a queue named by plain text, including names
-- 'Pgmq.Types.parseQueueName' rejects. Same projection as 'queueMetrics'.
queueMetricsUnvalidated :: Text -> Session QueueMetrics
queueMetricsUnvalidated q = statement q Stmt.queueMetricsUnvalidated

-- Non-destructive inspection ---------------------------------------------------
--
-- Each read resolves the physical table on the server first, then runs an
-- unprepared SELECT against it. Neither statement modifies a row: vt, read_ct,
-- and last_read_at are exactly what they were. A missing queue surfaces as the
-- server's undefined_table error (SQLSTATE 42P01); a missing message is Nothing.

-- | A keyset page of a queue table, newest-id-last, without leasing anything.
peekMessages :: PeekMessages -> Session (Vector Message)
peekMessages PeekMessages {unvalidatedQueueName, afterMessageId, limit} = do
  table <- statement (unvalidatedQueueName, "q") Inspect.formatTableName
  statement (afterMessageId, limit) (Inspect.peekStatement table)

-- | A keyset page of an archive table, each row with its archival timestamp.
peekArchivedMessages :: PeekMessages -> Session (Vector ArchivedMessage)
peekArchivedMessages PeekMessages {unvalidatedQueueName, afterMessageId, limit} = do
  table <- statement (unvalidatedQueueName, "a") Inspect.formatTableName
  statement (afterMessageId, limit) (Inspect.peekArchivedStatement table)

-- | One message of a queue table by id, or 'Nothing' when it is not there
-- (deleted, archived, popped, or never sent).
lookupMessage :: LookupMessage -> Session (Maybe Message)
lookupMessage LookupMessage {unvalidatedQueueName, messageId} = do
  table <- statement (unvalidatedQueueName, "q") Inspect.formatTableName
  statement messageId (Inspect.lookupStatement table)

-- | One message of an archive table by id, or 'Nothing'.
lookupArchivedMessage :: LookupMessage -> Session (Maybe ArchivedMessage)
lookupArchivedMessage LookupMessage {unvalidatedQueueName, messageId} = do
  table <- statement (unvalidatedQueueName, "a") Inspect.formatTableName
  statement messageId (Inspect.lookupArchivedStatement table)
```

Edit `pgmq-hasql/src/Pgmq.hs`. Add a section to the export list after the
`-- * FIFO / Grouped Reads` section:

```haskell
    -- * Non-destructive Inspection

    -- | Reads that observe a queue or its archive without leasing anything:
    -- @vt@ and @read_ct@ are untouched. They accept any server-accepted name
    -- (pass 'queueNameToText' for a validated one) and page by exclusive
    -- @msg_id@ cursor, never @OFFSET@. See
    -- @docs/design/019-non-destructive-inspection-reads.md@.
    peekMessages,
    peekArchivedMessages,
    lookupMessage,
    lookupArchivedMessage,
    queueMetricsUnvalidated,
    PeekMessages (..),
    LookupMessage (..),
```

add `ArchivedMessage (..),` after `Message (..),` under `-- * Types`, and extend the three
import lists (`Pgmq.Hasql.Sessions`, `Pgmq.Hasql.Statements.Types`, `Pgmq.Types`) with the
new names.

Build to check the library compiles:

```bash
cabal build pgmq-core pgmq-hasql
```

### M1: the session-layer spec

Create `pgmq-hasql/test/InspectionSpec.hs`. The whole file:

```haskell
{-# LANGUAGE OverloadedStrings #-}

-- | IR-1: reads that observe a queue or archive without leasing anything.
--
-- The non-destructive guarantee is asserted two ways: the raw cells every
-- lease touches (vt, read_ct, last_read_at) are byte-identical before and
-- after a sequence of peeks, and a consumer polling the queue concurrently
-- with a loop of peeks still receives every message exactly once with
-- read_ct = 1. Paging is keyset-only: the statements' SQL contains no OFFSET,
-- and paging by the last id seen visits every message exactly once.
module InspectionSpec (tests) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (finally)
import Control.Monad (forM_, replicateM_, unless, void)
import Data.Aeson (object, (.=))
import Data.Int (Int32, Int64)
import Data.List (sort)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Vector qualified as V
import EphemeralDb (TestFixture (..), withTestFixture)
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Pool qualified as Pool
import Hasql.Session (Session, statement)
import Hasql.Statement (preparable, toSql, unpreparable)
import Pgmq.Hasql.Sessions qualified as Sessions
import Pgmq.Hasql.Statements.Inspection qualified as Inspect
import Pgmq.Hasql.Statements.Types
  ( BatchMessageQuery (..),
    BatchSendMessage (..),
    LookupMessage (..),
    PeekMessages (..),
    QueueMetrics (..),
    ReadMessage (..),
    SendMessage (..),
  )
import Pgmq.Types (ArchivedMessage (..), Message (..), MessageBody (..), MessageId (..), QueueName, queueNameToText)
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase, (@?=))
import TestUtils (assertSession, cleanupQueue)

tests :: Pool.Pool -> TestTree
tests p =
  testGroup
    "Inspection (non-destructive reads)"
    [ testPeekLeavesRowsUntouched p,
      testPeekDoesNotDisturbConsumer p,
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
      _ <- peek pool queueName (Just (messageId (last first))) 50
      pure ()
    _ <- assertSession pool (Sessions.lookupMessage (LookupMessage (queueNameToText queueName) (messageId (V.head leased))))
    after <- assertSession pool (rowImages (queueNameToText queueName))
    assertEqual "rows are identical after peeks and lookups" before after
    length before @?= 5

testPeekDoesNotDisturbConsumer :: Pool.Pool -> TestTree
testPeekDoesNotDisturbConsumer p = testCase "a concurrently polling consumer sees every message exactly once" $
  withTestFixture p $ \TestFixture {pool, queueName} -> flip finally (cleanupQueue pool queueName) $ do
    assertSession pool (Sessions.createQueue queueName)
    sent <- sendN pool queueName 50
    done <- newEmptyMVar
    -- The consumer leases in batches of five until it has seen fifty ids or
    -- gives up after two hundred empty polls (which would mean peeks stole work).
    _ <- forkIO $ do
      let loop seen empties
            | Set.size seen >= 50 || empties >= (200 :: Int) = pure seen
            | otherwise = do
                batch <- assertSession pool (Sessions.readMessage (ReadMessage queueName 60 (Just 5) Nothing))
                if V.null batch
                  then threadDelay 5_000 >> loop seen (empties + 1)
                  else loop (foldr (Set.insert . messageId) seen (V.toList batch)) empties
      seen <- loop Set.empty 0
      putMVar done seen
    -- Meanwhile, peek relentlessly.
    replicateM_ 200 (void (peek pool queueName Nothing 50))
    seen <- takeMVar done
    assertEqual "the consumer received exactly the fifty sent ids" (Set.fromList sent) seen
    rows <- assertSession pool (rowImages (queueNameToText queueName))
    forM_ rows $ \(mid, readCt, _, _) ->
      assertEqual ("read_ct of " <> show mid <> " was bumped only by the consumer") 1 readCt
    -- Everything is leased for sixty seconds now; a further read returns nothing,
    -- which a destructive "peek" would have made impossible to predict.
    more <- assertSession pool (Sessions.readMessage (ReadMessage queueName 60 (Just 50) Nothing))
    V.length more @?= 0

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
            else go (Just (messageId (last page))) (pages + 1) acc'
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
    rows <- V.toList <$> assertSession pool (Sessions.peekArchivedMessages (PeekMessages (queueNameToText queueName) Nothing 10))
    map (messageId . archivedMessage) rows @?= take 2 sent
    forM_ rows $ \row ->
      assertBool "archived_at is not before enqueued_at" (archivedAt row >= enqueuedAt (archivedMessage row))
    live <- peek pool queueName Nothing 10
    map messageId live @?= drop 2 sent
    -- The archive pages by the same exclusive cursor.
    second <- V.toList <$> assertSession pool (Sessions.peekArchivedMessages (PeekMessages (queueNameToText queueName) (Just (head sent)) 10))
    map (messageId . archivedMessage) second @?= [sent !! 1]

testLookups :: Pool.Pool -> TestTree
testLookups p = testCase "lookup finds a present message and answers Nothing for an absent one, in both tables" $
  withTestFixture p $ \TestFixture {pool, queueName} -> flip finally (cleanupQueue pool queueName) $ do
    assertSession pool (Sessions.createQueue queueName)
    [liveId, archivedId] <- sendN pool queueName 2
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
    none <- peek pool queueName (Just (last sent)) 3
    none @?= []
    zero <- peek pool queueName Nothing 0
    zero @?= []

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
      assertSession pool (Sessions.createPartitionedQueue (Pgmq.Hasql.Statements.Types.CreatePartitionedQueue queueName "10" "100"))
      sent <- sendN pool queueName 12
      page <- peek pool queueName Nothing 20
      map messageId page @?= sent
      one <- assertSession pool (Sessions.lookupMessage (LookupMessage (queueNameToText queueName) (sent !! 11)))
      fmap messageId one @?= Just (sent !! 11)

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
  unless (available || not required) $ assertFailure "PGMQ_REQUIRE_PARTMAN=1 but pg_partman is not installed"
  if available then action else putStrLn "    SKIPPED: pg_partman is not installed"
```

(Import `CreatePartitionedQueue (..)` from `Pgmq.Hasql.Statements.Types` instead of the
fully qualified spelling in `testPartitionedQueue` if you prefer; the fully qualified form
compiles because the module is imported.) Note that `sendN` relies on `pgmq.send_batch`
returning ids in insertion order, which `batchSendMessage` already depends on elsewhere, and
that in `testPeekDoesNotDisturbConsumer` the consumer leases for sixty seconds so the final
read can be asserted empty even if the test machine is slow.

Register the module: in `pgmq-hasql/test/Main.hs` add `import InspectionSpec qualified` and
`InspectionSpec.tests pool,` to the shared-pool list (for example after
`MessageSpec.tests pool,`); in `pgmq-hasql/pgmq-hasql.cabal` add `InspectionSpec` to the test
suite's `other-modules`.

Run the suite three ways:

```bash
nix fmt
cabal test pgmq-hasql:pgmq-hasql-test --test-show-details=direct
PGMQ_TEST_SCHEMA_VERSION=1.12.0 cabal test pgmq-hasql:pgmq-hasql-test --test-show-details=direct
nix develop .#partman --command cabal test pgmq-hasql:pgmq-hasql-test --test-show-details=direct
```

Expected, in each run, a group like:

```text
  Inspection (non-destructive reads)
    peek leaves vt, read_ct, and last_read_at byte-identical:               OK (0.12s)
    a concurrently polling consumer sees every message exactly once:        OK (0.41s)
    paging by the last id visits every message exactly once:                OK (0.63s)
    inspection statements never use OFFSET:                                 OK
    archive reads return archived messages with their archival timestamp:   OK (0.09s)
    lookup finds a present message and answers Nothing for an absent one, in both tables: OK (0.08s)
    limit bounds the page and a cursor excludes its own row:                OK (0.07s)
    a missing queue fails with undefined_table (42P01):                     OK (0.02s)
    queueMetricsUnvalidated matches queueMetrics for a validated name:      OK (0.06s)
    peek and lookup work on a partitioned queue (needs pg_partman):         OK (0.30s)
```

with the last line reading `SKIPPED: pg_partman is not installed` outside the partman shell.
To see the suite red first, stash the `Inspection.hs` module and the session bodies: the spec
fails to compile, which is the red state for a compile-time contract; the behavioural red state
is reproduced by temporarily pointing `peekStatement` at `pgmq.read` (which leases) and watching
`testPeekLeavesRowsUntouched` report differing `vt` values.

Commit:

```text
feat(hasql): add non-destructive peek, archive, and lookup reads

Four hand-written SELECTs over the queue and archive tables, resolved
through pgmq.format_table_name and paged by exclusive msg_id cursor,
plus queueMetricsUnvalidated for queues named by plain text. None of
them leases: vt, read_ct, and last_read_at are byte-identical before
and after, and a concurrently polling consumer still receives every
message exactly once. pgmq-core gains ArchivedMessage.

MasterPlan: docs/masterplans/7-expose-non-destructive-queue-inspection-through-typed-reads-json-codecs-and-the-pgmq-inspect-sister-package.md
ExecPlan: docs/plans/27-add-non-destructive-peek-archive-and-lookup-reads-across-the-pgmq-layers.md
Intention: intention_01m3tcw9vmeeftdtqj53d1d6nb
```

### M2: the effect

Edit `pgmq-effectful/src/Pgmq/Effectful/Effect.hs`. In the export list add, after the
`-- * Queue Observability` group:

```haskell
    -- * Non-destructive Inspection
    peekMessages,
    peekArchivedMessages,
    lookupMessage,
    lookupArchivedMessage,
    queueMetricsUnvalidated,
```

Add `LookupMessage` and `PeekMessages` (types only, no `(..)`) to the
`Pgmq.Hasql.Statements.Types` import and `ArchivedMessage` to the `Pgmq.Types` import. In the
GADT, after `AllQueueMetrics :: Pgmq m [QueueMetrics]`, add:

```haskell
  -- Non-destructive inspection (no upstream function; design note 019)
  PeekMessages :: PeekMessages -> Pgmq m (Vector Message)
  PeekArchivedMessages :: PeekMessages -> Pgmq m (Vector ArchivedMessage)
  LookupMessage :: LookupMessage -> Pgmq m (Maybe Message)
  LookupArchivedMessage :: LookupMessage -> Pgmq m (Maybe ArchivedMessage)
  QueueMetricsUnvalidated :: Text -> Pgmq m QueueMetrics
```

and at the end of the module:

```haskell
-- Non-destructive Inspection

-- | A keyset page of a queue table without leasing anything: @vt@ and
-- @read_ct@ are untouched. Accepts any server-accepted name; pass
-- 'Pgmq.Types.queueNameToText' for a validated one. Pages by exclusive
-- @msg_id@ cursor, never @OFFSET@. A missing queue is a 'PgmqSessionError'
-- carrying SQLSTATE @42P01@.
peekMessages :: (Pgmq :> es) => PeekMessages -> Eff es (Vector Message)
peekMessages = send . PeekMessages

-- | A keyset page of an archive table, each row with its archival timestamp.
peekArchivedMessages :: (Pgmq :> es) => PeekMessages -> Eff es (Vector ArchivedMessage)
peekArchivedMessages = send . PeekArchivedMessages

-- | One message by id, or 'Nothing' when it is not in the queue table.
lookupMessage :: (Pgmq :> es) => LookupMessage -> Eff es (Maybe Message)
lookupMessage = send . LookupMessage

-- | One archived message by id, or 'Nothing' when it is not in the archive.
lookupArchivedMessage :: (Pgmq :> es) => LookupMessage -> Eff es (Maybe ArchivedMessage)
lookupArchivedMessage = send . LookupArchivedMessage

-- | 'queueMetrics' for a queue named by plain text, including names
-- 'Pgmq.Types.parseQueueName' rejects.
queueMetricsUnvalidated :: (Pgmq :> es) => Text -> Eff es QueueMetrics
queueMetricsUnvalidated = send . QueueMetricsUnvalidated
```

Adding constructors to the exported `Pgmq (..)` GADT is a breaking change under the Package
Versioning Policy: an interpreter that matches every constructor (a mock in a consumer's test
suite, for example) now has five missing cases. The plain and traced interpreters in this
repository are updated below; the changelog in M3 must say so.

### M2: the plain interpreter

Edit `pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs`. After the `AllQueueMetrics` case:

```haskell
  -- Non-destructive inspection
  PeekMessages args -> runSession pool $ Sessions.peekMessages args
  PeekArchivedMessages args -> runSession pool $ Sessions.peekArchivedMessages args
  LookupMessage args -> runSession pool $ Sessions.lookupMessage args
  LookupArchivedMessage args -> runSession pool $ Sessions.lookupArchivedMessage args
  QueueMetricsUnvalidated q -> runSession pool $ Sessions.queueMetricsUnvalidated q
```

### M2: the traced interpreter

Edit `pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs`. Replace `queueOp` with the pair:

```haskell
-- | 'OpInfo' for a non-messaging operation scoped to a single queue.
queueOp :: Text -> OTel.SpanKind -> QueueName -> OpInfo
queueOp fn kind qn = queueOpText fn kind (queueNameToText qn)

-- | 'OpInfo' for a non-messaging operation scoped to a queue named by plain
-- text: the lenient inspection path, which accepts names 'parseQueueName'
-- rejects. The destination attribute carries the name exactly as given.
queueOpText :: Text -> OTel.SpanKind -> Text -> OpInfo
queueOpText fn kind name = (defaultOpInfo fn kind) {opDestination = Just name}
```

and after the `AllQueueMetrics` case add:

```haskell
  -- Non-destructive inspection. No pgmq function backs the four reads (they
  -- are hand-written SELECTs, see design note 019), so, like
  -- pgmq.list_fifo_indexes, they carry this library's own labels. The lenient
  -- metrics read does call pgmq.metrics and says so.
  PeekMessages args@(Types.PeekMessages qn _ _) ->
    withTracedOp config pool (queueOpText "pgmq.peek" OTel.Internal qn) $
      Sessions.peekMessages args
  PeekArchivedMessages args@(Types.PeekMessages qn _ _) ->
    withTracedOp config pool (queueOpText "pgmq.peek_archive" OTel.Internal qn) $
      Sessions.peekArchivedMessages args
  LookupMessage args@(Types.LookupMessage qn msgId) ->
    withTracedOp config pool ((queueOpText "pgmq.lookup_message" OTel.Internal qn) {opMessageId = Just msgId}) $
      Sessions.lookupMessage args
  LookupArchivedMessage args@(Types.LookupMessage qn msgId) ->
    withTracedOp config pool ((queueOpText "pgmq.lookup_archived_message" OTel.Internal qn) {opMessageId = Just msgId}) $
      Sessions.lookupArchivedMessage args
  QueueMetricsUnvalidated q ->
    withTracedOp config pool (queueOpText "pgmq.metrics" OTel.Internal q) $
      Sessions.queueMetricsUnvalidated q
```

Both record constructors are matched positionally through the existing
`Pgmq.Hasql.Statements.Types qualified as Types` import, exactly as every other case does.
Also extend the module's header comment, under "Lifecycle and observability spans", with the
sentence "Inspection spans: `pgmq.peek my-queue`, `pgmq.lookup_message my-queue` (this
library's own labels; no pgmq function backs them)".

### M2: the effectful umbrella and the witnesses

Edit `pgmq-effectful/src/Pgmq/Effectful.hs`. Add after the `-- * Queue Observability` group:

```haskell
    -- * Non-destructive Inspection

    -- | Reads that observe a queue or its archive without leasing anything.
    -- See @docs/design/019-non-destructive-inspection-reads.md@.
    peekMessages,
    peekArchivedMessages,
    lookupMessage,
    lookupArchivedMessage,
    queueMetricsUnvalidated,
    PeekMessages (..),
    LookupMessage (..),
```

add `ArchivedMessage (..),` after `Message (..),` under `-- * Types`, and extend the imports of
`Pgmq.Effectful.Effect`, `Pgmq.Hasql.Statements.Types`, and `Pgmq.Types` accordingly.

Edit `pgmq-hasql/test/UmbrellaExportsSpec.hs`: add `inspection`, `lenientMetrics`, and
`inspectionArguments` to the export list and the body:

```haskell
inspection :: PeekMessages -> LookupMessage -> (Session (Vector Message), Session (Vector ArchivedMessage), Session (Maybe Message), Session (Maybe ArchivedMessage))
inspection page one = (peekMessages page, peekArchivedMessages page, lookupMessage one, lookupArchivedMessage one)

lenientMetrics :: Text -> Session QueueMetrics
lenientMetrics = queueMetricsUnvalidated

inspectionArguments :: Text -> (PeekMessages, LookupMessage)
inspectionArguments q = (PeekMessages q Nothing 50, LookupMessage q (MessageId 1))
```

(add `import Data.Text (Text)`). Edit `pgmq-effectful/test/UmbrellaExportsSpec.hs` the same way
with `(Pgmq :> es) =>` constraints and `Eff es` results, and `import Data.Text (Text)`.

### M2: the foreign-name spec

Create `pgmq-hasql/test/InspectionForeignNameSpec.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

-- | IR-1 acceptance 5: every inspection read works for queues whose names
-- 'parseQueueName' rejects, because an inspection surface sees whatever exists
-- in the database, not only what this library created.
--
-- The names are created and fed through raw SQL (the Haskell API rightly
-- refuses them) and read back through the new sessions. Runs on a dedicated
-- PostgreSQL instance: a mixed-case row in pgmq.meta poisons the typed
-- listQueues decoding for every test sharing a database.
module InspectionForeignNameSpec (tests) where

import Control.Monad (void)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word32)
import Database.PostgreSQL.Migrate (defaultRunOptions, migrationPlan, runMigrationPlan)
import EphemeralDb (ephemeralConfig)
import EphemeralPg qualified as Pg
import Hasql.Decoders qualified as D
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as PoolConfig
import Hasql.Session (Session, statement)
import Hasql.Statement (unpreparable)
import Pgmq.Hasql.Sessions qualified as Sessions
import Pgmq.Hasql.Statements.Types (LookupMessage (..), PeekMessages (..), QueueMetrics (..))
import Pgmq.Migration qualified as Migration
import Pgmq.Types (ArchivedMessage (..), Message (..), MessageId (..), parseQueueName)
import System.Random (randomRIO)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  withResource acquireDb releaseDb $ \getDb ->
    testGroup
      "Inspection of foreign queue names"
      [ testForeignName getDb "mixed-case" (\n -> "MyQueue_" <> n),
        testForeignName getDb "hyphenated" (\n -> "odd-name_" <> n)
      ]

testForeignName :: IO (Pg.Database, Pool.Pool) -> String -> (Text -> Text) -> TestTree
testForeignName getDb label mkName = testCase (label <> " name: peek, lookup, archive, and metrics work") $ do
  (_, pool) <- getDb
  suffix <- T.pack . show <$> randomRIO (10000 :: Word32, 99999)
  let name = mkName suffix
  -- The whole point: this library's validator refuses the name.
  case parseQueueName name of
    Left _ -> pure ()
    Right _ -> assertFailure ("expected parseQueueName to reject " <> T.unpack name)
  assertSession pool (rawUnit ("select pgmq.create('" <> name <> "')"))
  ids <- mapM (\i -> assertSession pool (rawId ("select pgmq.send('" <> name <> "', '{\"i\":" <> T.pack (show i) <> "}'::jsonb)"))) [1 .. 3 :: Int]
  page <- assertSession pool (Sessions.peekMessages (PeekMessages name Nothing 10))
  map messageId (V.toList page) @?= map MessageId ids
  one <- assertSession pool (Sessions.lookupMessage (LookupMessage name (MessageId (head ids))))
  fmap messageId one @?= Just (MessageId (head ids))
  metrics <- assertSession pool (Sessions.queueMetricsUnvalidated name)
  queueLength metrics @?= 3
  assertEqual "metrics report the name as stored" name (queueName metrics)
  void $ assertSession pool (rawBool ("select pgmq.archive('" <> name <> "', " <> T.pack (show (head ids)) <> ")"))
  archived <- assertSession pool (Sessions.peekArchivedMessages (PeekMessages name Nothing 10))
  map (messageId . archivedMessage) (V.toList archived) @?= [MessageId (head ids)]
  found <- assertSession pool (Sessions.lookupArchivedMessage (LookupMessage name (MessageId (head ids))))
  fmap (messageId . archivedMessage) found @?= Just (MessageId (head ids))
  remaining <- assertSession pool (Sessions.peekMessages (PeekMessages name Nothing 10))
  V.length remaining @?= 2
  void $ assertSession pool (rawBool ("select pgmq.drop_queue('" <> name <> "')"))

-- Dedicated database plumbing (as in MixedCaseRemediationSpec) -----------------

acquireDb :: IO (Pg.Database, Pool.Pool)
acquireDb = do
  config <- ephemeralConfig
  started <- Pg.startCached config Pg.defaultCacheConfig
  db <- either (\err -> error ("could not start a dedicated PostgreSQL: " <> show err)) pure started
  component <- either (error . ("Invalid PGMQ migration component: " <>) . show) pure Migration.pgmqMigrations
  plan <- either (error . ("Invalid PGMQ migration plan: " <>) . show) pure (migrationPlan (component :| []))
  installResult <- runMigrationPlan defaultRunOptions (Pg.connectionSettings db) plan
  case installResult of
    Left migrationErr -> error $ "Migration failed: " <> show migrationErr
    Right _ -> pure ()
  pool <-
    Pool.acquire $
      PoolConfig.settings
        [ PoolConfig.size 2,
          PoolConfig.staticConnectionSettings (Pg.connectionSettings db)
        ]
  pure (db, pool)

releaseDb :: (Pg.Database, Pool.Pool) -> IO ()
releaseDb (db, pool) = do
  Pool.release pool
  Pg.stop db

-- Raw statement helpers. Names are spliced into SQL text because these tests
-- must construct names the Haskell API refuses; every spliced value is built
-- above from [A-Za-z0-9_-], so splicing is safe here.

rawUnit :: Text -> Session ()
rawUnit sqlText = statement () (unpreparable sqlText mempty D.noResult)

rawBool :: Text -> Session Bool
rawBool sqlText = statement () (unpreparable sqlText mempty (D.singleRow (D.column (D.nonNullable D.bool))))

rawId :: Text -> Session Int64
rawId sqlText = statement () (unpreparable sqlText mempty (D.singleRow (D.column (D.nonNullable D.int8))))

assertSession :: Pool.Pool -> Session a -> IO a
assertSession pool session = do
  result <- Pool.use pool session
  case result of
    Left err -> assertFailure $ "Session failed: " <> show err
    Right a -> pure a
```

Register it: in `pgmq-hasql/test/Main.hs` add `import InspectionForeignNameSpec qualified`
and `InspectionForeignNameSpec.tests,` next to `AliasingSpec.tests,` (no pool argument; add a
comment that it constructs mixed-case metadata and therefore runs on its own instance); in
the cabal file add `InspectionForeignNameSpec` to `other-modules`. The stock 1.12 fixture is
not used by this spec (it installs the native ledger itself, as the other dedicated specs do),
so it runs identically under every environment.

### M2: the shared effect feature test

Edit `pgmq-effectful/test/TracedInterpreterSpec.hs`. Add a case to the list that
`featureTests traced` builds, after the "explicit premake errors reach runtime error channel"
case:

```haskell
           testCase "non-destructive inspection reads" $
             withSemconvOptIn "messaging/dup,database/dup" $
               isolated $ \pool -> do
                 (tracer, _, spansRef) <- setupTracer
                 let run :: Eff '[Eff.Pgmq, Error PgmqRuntimeError, IOE] a -> IO a
                     run action = assertRight =<< runEff (runError @PgmqRuntimeError ((if traced then runPgmqTraced pool tracer else runPgmq pool) action))
                 queue <- mkUniqueQueue "inspect"
                 let q = queueNameToText queue
                 run (Eff.createQueue queue)
                 ids <- mapM (\i -> run (Eff.sendMessage (SendMessage queue (MessageBody (object ["i" .= (i :: Int)])) Nothing))) [1 .. 3]
                 archivedOk <- run (Eff.archiveMessage (MessageQuery queue (head ids)))
                 assertBool "archive succeeded" archivedOk
                 page <- run (Eff.peekMessages (Types.PeekMessages q Nothing 10))
                 assertEqual "two live messages" (drop 1 ids) (map Pgmq.messageId (V.toList page))
                 archived <- run (Eff.peekArchivedMessages (Types.PeekMessages q Nothing 10))
                 assertEqual "one archived message" (take 1 ids) (map (Pgmq.messageId . Pgmq.archivedMessage) (V.toList archived))
                 found <- run (Eff.lookupMessage (Types.LookupMessage q (ids !! 1)))
                 assertEqual "lookup finds a live message" (Just (ids !! 1)) (fmap Pgmq.messageId found)
                 missing <- run (Eff.lookupArchivedMessage (Types.LookupMessage q (ids !! 1)))
                 assertEqual "a live message is not in the archive" Nothing (fmap (Pgmq.messageId . Pgmq.archivedMessage) missing)
                 metrics <- run (Eff.queueMetricsUnvalidated q)
                 assertEqual "lenient metrics" 2 (Types.queueLength metrics)
                 -- The peeks leased nothing: a real read now returns both with read_ct 0.
                 leased <- run (Eff.readMessage (ReadMessage queue 30 (Just 10) Nothing))
                 assertEqual "both messages still available" (drop 1 ids) (map Pgmq.messageId (V.toList leased))
                 assertEqual "read_ct was zero before this read" [1, 1] (map Pgmq.readCount (V.toList leased))
                 when traced $ do
                   spans <- readIORef spansRef
                   peeks <- spansWithFirstWord "pgmq.peek" spans
                   peekSpan <- singleSpan peeks "peek"
                   spanName peekSpan >>= assertEqual "peek span name" ("pgmq.peek " <> q)
                   case OTel.spanKind peekSpan of
                     OTel.Internal -> pure ()
                     other -> assertFailure ("expected Internal: " <> show other)
                   assertAttrText peekSpan "db.operation" "pgmq.peek"
                   assertAttrText peekSpan "db.operation.name" "pgmq.peek"
                   assertAttrText peekSpan "messaging.destination.name" q
                   assertNoAttr peekSpan "messaging.operation"
                   lookups <- spansWithFirstWord "pgmq.lookup_message" spans
                   lookupSpan <- singleSpan lookups "lookup"
                   assertAttrText lookupSpan "messaging.message.id" (T.pack (show (Pgmq.unMessageId (ids !! 1))))
```

(`readCount` after one real read is `1` because the peeks never incremented it; that is the
assertion.) Extend the module's `Pgmq.Effectful` import with `MessageQuery (..)`, `ReadMessage
(..)`, and `SendMessage (..)` if not already there (they are), and nothing else: `Types`,
`Eff`, `Pgmq`, `V`, `T`, `object`, `(.=)`, `when`, and `readIORef` are already imported.

Run everything:

```bash
nix fmt
cabal test all --test-show-details=direct
```

Expected in the effectful output, twice (plain and traced):

```text
    non-destructive inspection reads:                                       OK (0.9s)
```

and in the hasql output:

```text
  Inspection of foreign queue names
    mixed-case name: peek, lookup, archive, and metrics work:               OK (0.11s)
    hyphenated name: peek, lookup, archive, and metrics work:               OK (0.10s)
```

Commit:

```text
feat(effectful)!: expose the inspection reads through the Pgmq effect

Five constructors (PeekMessages, PeekArchivedMessages, LookupMessage,
LookupArchivedMessage, QueueMetricsUnvalidated) with cases in both
interpreters; the traced one labels the hand-written reads pgmq.peek,
pgmq.peek_archive, pgmq.lookup_message, and pgmq.lookup_archived_message.
Foreign and mixed-case names are proven through every read on a dedicated
instance.

BREAKING CHANGE: interpreters that match every Pgmq constructor must add
the five new cases.

MasterPlan: docs/masterplans/7-expose-non-destructive-queue-inspection-through-typed-reads-json-codecs-and-the-pgmq-inspect-sister-package.md
ExecPlan: docs/plans/27-add-non-destructive-peek-archive-and-lookup-reads-across-the-pgmq-layers.md
Intention: intention_01m3tcw9vmeeftdtqj53d1d6nb
```

### M3: design note 019

Create `docs/design/019-non-destructive-inspection-reads.md` in the style of
`docs/design/017-transient-error-classification.md` (a `Status` section naming the adoption
date and this plan, then headed prose sections). It must state:

- The contract: `peekMessages`, `peekArchivedMessages`, `lookupMessage`,
  `lookupArchivedMessage`, and `queueMetricsUnvalidated` observe and never lease; `vt`,
  `read_ct`, and `last_read_at` are untouched; this is pinned by
  `pgmq-hasql/test/InspectionSpec.hs`.
- Why they exist: every upstream read leases; upstream has no archive read and no fetch by id;
  design note 012 permits hand-written statements for exactly this case; no `pgmq.*` function
  is shadowed and no migration is added.
- The lenient name: the argument is `Text`, any server-accepted name, because an inspection
  surface must show what exists (design note 016 explains why the typed path is strict); a
  validated caller passes `queueNameToText`; the physical table is resolved by
  `pgmq.format_table_name` on the server and spliced as a quoted identifier; `format_table_name`
  rejects `$`, `;`, `--`, and `'`.
- Paging: exclusive `msg_id` cursor, ascending order, `LIMIT`, never `OFFSET`; stable under
  concurrent consumption; a page shorter than `limit` is the last page; callers wanting a
  "has next page" signal request `limit + 1`.
- Limit semantics: positive; `0` returns nothing; negative is a server error; the HTTP layer
  validates.
- Errors: a missing queue is the server's `42P01` inside `PgmqSessionError`; a missing message
  is `Nothing`; both interpreters surface errors identically; `isTransient` is unchanged.
- Why `unpreparable`: the SQL text varies per table and hasql caches prepared statements by
  text.
- Partitioned queues: the parent table is selected, so partitions need no special case.
- Tracing labels: `pgmq.peek`, `pgmq.peek_archive`, `pgmq.lookup_message`,
  `pgmq.lookup_archived_message` are this library's labels (no pgmq function); the lenient
  metrics read keeps `pgmq.metrics`.
- Where enforced: `pgmq-hasql/test/InspectionSpec.hs`,
  `pgmq-hasql/test/InspectionForeignNameSpec.hs`, the inspection case in
  `pgmq-effectful/test/TracedInterpreterSpec.hs`.
- Related: design notes 012 and 016, the ADR
  `docs/adr/queue-inspection-surface-boundary-and-wire-contract.md`, the keiro-ui decisions
  `mori://shinzui/keiro-ui/okf/adrs/concepts/ADR-1` and `ADR-3`.

### M3: Haddocks

Confirm each of these carries the contract in its Haddock (the Concrete Steps above already
include the text; this is the checklist): `ArchivedMessage` in `Pgmq.Types`; `PeekMessages` and
`LookupMessage` in `Pgmq.Hasql.Statements.Types`; the module header and `formatTableName`,
`quoteIdentifier`, and the four builders in `Pgmq.Hasql.Statements.Inspection`;
`queueMetricsUnvalidated` in `QueueObservability` and in `Sessions`; the four sessions; the
`-- * Non-destructive Inspection` section comments in both umbrellas; the five smart
constructors in `Pgmq.Effectful.Effect`; the traced-interpreter header sentence. Run
`cabal haddock pgmq-hasql pgmq-effectful` and confirm no new warnings about missing
documentation on the new names.

### M3: the capability record

Allocate the handle and write the record:

```bash
okf id list docs/capabilities --profile docs/capabilities/profile.dhall
okf id next docs/capabilities --profile docs/capabilities/profile.dhall CAP
```

The second command prints the next free handle (`CAP-10` at the time of planning; `CAP-11` if
plan 28 wrote first). Create `docs/capabilities/non-destructive-queue-inspection.md` with that
handle, modelled on `docs/capabilities/topic-routing.md`:

```yaml
---
title: "Non-destructive queue inspection reads"
type: Capability
description: "Peek at a queue or its archive by keyset page and fetch a message by id without leasing anything, for any server-accepted queue name."
generated:
  by: <your model identity, in the vendor/model form the bundle already uses>
  at: "<RFC 3339 UTC>"
capabilityId: CAP-<N>
provider: mori://shinzui/pgmq-hs
status: shipped
stability: experimental
since: "unreleased"
packages:
  - pgmq-hasql
  - pgmq-effectful
  - pgmq-core
requires:
  - CAP-1
interface:
  - Pgmq
  - Pgmq.Effectful
  - Pgmq.Hasql.Statements.Inspection
evidence:
  - kind: test
    resource: pgmq-hasql/test/InspectionSpec.hs
    proves: Peeks and lookups leave vt, read_ct, and last_read_at byte-identical, a concurrent consumer is undisturbed, keyset paging visits every message exactly once without OFFSET, archive reads carry archived_at, and lookups return a typed Nothing.
  - kind: test
    resource: pgmq-hasql/test/InspectionForeignNameSpec.hs
    proves: Every read works for mixed-case and hyphenated names parseQueueName rejects.
  - kind: test
    resource: pgmq-effectful/test/TracedInterpreterSpec.hs
    proves: The five effect operations run under both interpreters and the traced one labels them pgmq.peek, pgmq.peek_archive, pgmq.lookup_message, pgmq.lookup_archived_message.
  - kind: guide
    resource: docs/design/019-non-destructive-inspection-reads.md
    proves: The contract, the lenient-name rule, and the keyset-only paging policy.
---
```

followed by a body in the bundle's style: what it provides (the five operations and the two
records), a `## Shape` block showing a peek and a lookup through `Pgmq`, and `## Limits`
(lenient names are shown as stored and are never validated or normalised; a missing queue is a
`42P01` error, not a typed result; `limit` must be positive; pre-1.0 and experimental). Add a
row to the table in `docs/capabilities/index.md` and an entry to `docs/capabilities/log.md`
under a new dated heading (`* **Addition**: …`). Validate:

```bash
just docs-check
```

### M3: changelogs

Insert an `## Unreleased` section at the top of each file, directly under the `# Revision
history …` heading. Root `CHANGELOG.md`:

```text
## Unreleased

Add non-destructive inspection reads across the family (IR-1): `peekMessages`,
`peekArchivedMessages`, `lookupMessage`, `lookupArchivedMessage`, and
`queueMetricsUnvalidated` in `pgmq-hasql`, the same five operations on the `Pgmq` effect in
`pgmq-effectful` under both interpreters, and the `ArchivedMessage` record in `pgmq-core`.
The reads observe a queue or its archive without leasing: `vt`, `read_ct`, and
`last_read_at` are untouched. They accept any server-accepted queue name (including names
`parseQueueName` rejects) and page by exclusive `msg_id` cursor, never `OFFSET`. See
`docs/design/019-non-destructive-inspection-reads.md`.

Breaking for `pgmq-effectful` consumers that interpret every `Pgmq` constructor: the effect
gained five constructors.
```

`pgmq-core/CHANGELOG.md`: "Add `ArchivedMessage`, a `Message` plus its `archivedAt`
timestamp, returned by the archive inspection reads in `pgmq-hasql`." (If the integration rule
made this plan add the JSON instances, say so here too.) `pgmq-hasql/CHANGELOG.md`: the five
sessions, the `Pgmq.Hasql.Statements.Inspection` module, the two argument records, the
`archivedMessageDecoder`, the lenient-name and keyset contract, and the `42P01`/`Nothing`
error shapes. `pgmq-effectful/CHANGELOG.md`: the five operations, the traced labels, and the
breaking-change paragraph for exhaustive interpreters.

### M3: close IR-1

Edit `docs/improvement-requests/expose-non-destructive-queue-inspection-reads.md`: change
`status: proposed` to `status: completed`, add `completedAt: "<RFC 3339 UTC when cabal test all
went green>"`, and add a `resolution` walking the request's six acceptance items: (1) byte-identical
`vt`/`read_ct` pinned by `InspectionSpec`; (2) the concurrent consumer case; (3) keyset paging
visits every id once and `toSql` proves no `OFFSET`; (4) archive reads carry `archived_at` and
lookups answer a typed `Nothing` in both tables; (5) foreign and mixed-case names covered on a
dedicated instance; (6) the operations exist on the `Pgmq` effect under both interpreters and,
being dynamic-dispatch constructors, are implementable by any mock. Name design note 019 and
this plan. Then:

```bash
okf log add docs/improvement-requests --kind Update -m "IR-1 completed: non-destructive peek, archive, and lookup reads shipped across pgmq-hasql and pgmq-effectful with lenient names and keyset paging (ExecPlan 27)"
just docs-check
```

Commit:

```text
docs: record the inspection read contract and close IR-1

Design note 019 states the lenient-name, keyset-only, never-lease
contract; the capability catalog gains the inspection reads; every
changed package has an Unreleased changelog entry, the effectful one
naming the breaking constructor additions; IR-1 is completed.

MasterPlan: docs/masterplans/7-expose-non-destructive-queue-inspection-through-typed-reads-json-codecs-and-the-pgmq-inspect-sister-package.md
ExecPlan: docs/plans/27-add-non-destructive-peek-archive-and-lookup-reads-across-the-pgmq-layers.md
Intention: intention_01m3tcw9vmeeftdtqj53d1d6nb
```

Finally update this plan's Progress, Surprises & Discoveries, Decision Log, and Outcomes &
Retrospective sections, record the provenance revision entry once for the session
(`bun agents/skills/exec-plan/record-provenance.ts revision --plan docs/plans/27-… --model <id> --harness <name> --mode implement --note "…"`),
and update the MasterPlan's registry row and Progress checklist for EP-1.


## Validation and Acceptance

Run, from the repository root inside `nix develop`:

```bash
nix fmt
cabal build all
cabal test pgmq-hasql:pgmq-hasql-test --test-show-details=direct
PGMQ_TEST_SCHEMA_VERSION=1.12.0 cabal test pgmq-hasql:pgmq-hasql-test --test-show-details=direct
cabal test pgmq-effectful:pgmq-effectful-test --test-show-details=direct
nix develop .#partman --command cabal test pgmq-hasql:pgmq-hasql-test --test-show-details=direct
just docs-check
```

Acceptance, phrased as behaviour:

1. With a queue holding leased and unleased messages, running five peeks and a lookup and then
   reading the raw `msg_id, read_ct, last_read_at, vt` cells shows exactly the rows read before
   the peeks (`testPeekLeavesRowsUntouched` passes). This is `IR-1` acceptance 1.
2. With fifty messages and a consumer leasing them in batches of five while two hundred peeks
   run, the consumer's collected id set equals the fifty sent ids, every row's `read_ct` is one,
   and a final read returns nothing (`testPeekDoesNotDisturbConsumer` passes). This is
   `IR-1` acceptance 2.
3. With one thousand messages, paging with limit seven from no cursor and then from the last id
   of each page takes 143 pages and visits the thousand ids in order with no duplicates, and
   the statements' SQL contains no `offset` (`testKeysetPagingVisitsEachOnce` and
   `testNoOffset` pass). This is acceptance 3.
4. After archiving two of three messages, the archive page returns those two with
   `archived_at` no earlier than `enqueued_at`, the live page returns the third, and lookups
   answer `Just` for a present id and `Nothing` for an absent one in both tables
   (`testArchiveReads`, `testLookups` pass). This is acceptance 4.
5. A queue created as `MyQueue_<n>` or `odd-name_<n>` through raw SQL, which `parseQueueName`
   rejects, is peeked, looked up, archived-and-peeked, and measured through the new reads
   using the raw name (`InspectionForeignNameSpec` passes). This is acceptance 5.
6. The five operations run through `runPgmq` and `runPgmqTraced`, the traced run emits a span
   named `pgmq.peek <queue>` of kind `Internal` with `db.operation` `pgmq.peek` and a
   `pgmq.lookup_message` span carrying `messaging.message.id`, and both `UmbrellaExportsSpec`
   modules compile using only the umbrellas (the effectful feature case passes twice). This is
   acceptance 6; the mock-interpreter half follows from dynamic dispatch, as every existing
   constructor's does.
7. `just docs-check` passes with the new capability record and `IR-1` at `completed`.
8. On the stock 1.12.0 fixture the hasql suite passes unchanged except the partition case,
   which is skipped or run exactly as `QueueSpec`'s partition cases are.


## Idempotence and Recovery

Every code edit is additive and can be re-applied or reverted file by file; nothing changes a
migration, a schema, or an existing statement's SQL (the `queueMetrics` refactor hoists its SQL
into `queueMetricsSql` byte for byte, which `MetricsSpec` and `AllFunctionsDecoderSpec` continue
to prove). The test suites create randomly named queues and drop them in `finally` blocks, so a
failed run leaves at most an orphaned `test_queue_<n>` in a throwaway database that the next
`ephemeral-pg` start reaps. `InspectionForeignNameSpec` starts and stops its own instance under
the stable root `/tmp/ephpg-pgmq-hs-<uid>`; if a run is killed, the next run's startup sweep
reclaims it.

If the integration grep in M1 is ambiguous (for example `Pgmq.Types` has the policy Haddock but
no `Message` instance because plan 28 is mid-flight in a parallel worktree), do not add the
`ArchivedMessage` instances; record the observation in Surprises & Discoveries and leave the
codec to plan 28, whose own integration rule covers the type once it exists.

`okf id next` must be re-run immediately before writing the capability record; if validation
reports a duplicate `capabilityId`, plan 28 took the handle first, so rename the file's handle
to the next free one and re-validate. Frontmatter edits to `IR-1` are plain text and can be
corrected and re-validated freely; `okf log add` appends, so run it once per closure.

If a commit is rejected by the pre-commit hook, run `nix fmt`, re-stage, and commit again
with the same message.


## Interfaces and Dependencies

No library dependency changes in any package. `pgmq-hasql` already depends on `hasql`,
`vector`, `text`, `time`, and `aeson`; its test suite already has `hasql`, `hasql-pool`,
`vector`, `text`, `time`, `random`, `containers` is not needed (the spec uses `Data.Set`,
which comes from `containers`; add `containers` to the test suite's `build-depends` if the
build reports it missing, which is a test-only dependency and needs no changelog entry).
`pgmq-effectful` already depends on everything the new cases use. If the integration rule
makes this plan add the `ArchivedMessage` codec, `pgmq-core`'s test suite gains `tasty-golden`
and `bytestring` exactly as plan 28 adds them; otherwise `pgmq-core` is unchanged beyond the
type.

At the end of M1 these names exist with these types:

```haskell
-- pgmq-core, Pgmq.Types
data ArchivedMessage = ArchivedMessage { archivedMessage :: !Message, archivedAt :: !UTCTime }

-- pgmq-hasql, Pgmq.Hasql.Statements.Types
data PeekMessages = PeekMessages { unvalidatedQueueName :: !Text, afterMessageId :: !(Maybe MessageId), limit :: !Int32 }
data LookupMessage = LookupMessage { unvalidatedQueueName :: !Text, messageId :: !MessageId }

-- pgmq-hasql, Pgmq.Hasql.Decoders
archivedMessageDecoder :: D.Row ArchivedMessage

-- pgmq-hasql, Pgmq.Hasql.Statements.Inspection
formatTableName :: Statement (Text, Text) Text
quoteIdentifier :: Text -> Text
peekStatement :: Text -> Statement (Maybe MessageId, Int32) (Vector Message)
peekArchivedStatement :: Text -> Statement (Maybe MessageId, Int32) (Vector ArchivedMessage)
lookupStatement :: Text -> Statement MessageId (Maybe Message)
lookupArchivedStatement :: Text -> Statement MessageId (Maybe ArchivedMessage)

-- pgmq-hasql, Pgmq.Hasql.Statements.QueueObservability
queueMetricsUnvalidated :: Statement Text QueueMetrics

-- pgmq-hasql, Pgmq.Hasql.Sessions (and re-exported from Pgmq)
peekMessages :: PeekMessages -> Session (Vector Message)
peekArchivedMessages :: PeekMessages -> Session (Vector ArchivedMessage)
lookupMessage :: LookupMessage -> Session (Maybe Message)
lookupArchivedMessage :: LookupMessage -> Session (Maybe ArchivedMessage)
queueMetricsUnvalidated :: Text -> Session QueueMetrics
```

At the end of M2, `Pgmq.Effectful.Effect` exports the constructors `PeekMessages`,
`PeekArchivedMessages`, `LookupMessage`, `LookupArchivedMessage`, and `QueueMetricsUnvalidated`
on `Pgmq`, and the smart constructors:

```haskell
peekMessages :: (Pgmq :> es) => PeekMessages -> Eff es (Vector Message)
peekArchivedMessages :: (Pgmq :> es) => PeekMessages -> Eff es (Vector ArchivedMessage)
lookupMessage :: (Pgmq :> es) => LookupMessage -> Eff es (Maybe Message)
lookupArchivedMessage :: (Pgmq :> es) => LookupMessage -> Eff es (Maybe ArchivedMessage)
queueMetricsUnvalidated :: (Pgmq :> es) => Text -> Eff es QueueMetrics
```

`Pgmq.Effectful` re-exports all five with `PeekMessages (..)`, `LookupMessage (..)`, and
`ArchivedMessage (..)`. `Pgmq.Effectful.Interpreter.Traced` has the private
`queueOpText :: Text -> OTel.SpanKind -> Text -> OpInfo`. `runPgmq`, `runPgmqTraced`,
`PgmqRuntimeError`, `fromUsageError`, and `isTransient` keep their types and behaviour.

At the end of M3, `docs/design/019-non-destructive-inspection-reads.md` exists, the capability
bundle holds one more record whose handle `okf id next` allocated, every changed package's
changelog has an `Unreleased` section, and `IR-1` is `completed`. The sister-package plans
(`docs/plans/29-…`, `docs/plans/30-…`) consume exactly the names above and nothing else from
this plan.
