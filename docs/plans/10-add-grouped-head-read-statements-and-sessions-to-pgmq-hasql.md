---
id: 10
slug: add-grouped-head-read-statements-and-sessions-to-pgmq-hasql
title: "Add grouped-head read statements and sessions to pgmq-hasql"
kind: exec-plan
created_at: 2026-07-14T14:55:11Z
intention: "intention_01kxgh9geke2dayhx57qp6g9ye"
master_plan: "docs/masterplans/2-support-pgmq-1-12-0-grouped-head-reads.md"
---

# Add grouped-head read statements and sessions to pgmq-hasql

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.


## Purpose / Big Picture

`pgmq` is a message queue that lives inside PostgreSQL. A queue is a table; every queue
operation is a PostgreSQL function like `pgmq.send(...)` or `pgmq.read(...)`. This
repository, `pgmq-hs`, is a Haskell client for those functions, and the package
`pgmq-hasql` is the layer that actually talks to the database (it uses the `hasql` library,
a fast, typed PostgreSQL client).

pgmq 1.12.0 adds two new PostgreSQL functions. This plan makes them callable from Haskell.

To understand what they do, you first need one pgmq concept. Messages can carry a JSON
header called `x-pgmq-group`. All messages sharing the same value in that header form a
**FIFO group** ("first in, first out"): pgmq guarantees they are consumed in the order they
were sent. Different groups are independent of each other.

The existing Haskell function `readGroupedRoundRobin` reads a batch of messages spread fairly
across groups. The new functions do something different and specifically useful for scaling
out workers:

- `pgmq.read_grouped_head(queue_name, vt, qty)` returns the **single oldest message from each
  of up to `qty` distinct groups**, and never two messages from the same group. So if you run
  ten workers and each calls it, each worker ends up holding the head of a different group.
  Every group makes progress simultaneously, while order *within* each group is still
  respected, because a group's second message cannot be handed out until its first is deleted
  or its visibility timeout expires. This is the standard way to scale ordered processing
  horizontally.
- `pgmq.read_grouped_head_with_poll(queue_name, vt, qty, max_poll_seconds, poll_interval_ms)`
  does the same thing, but if the queue has nothing available it waits — re-checking every
  `poll_interval_ms` milliseconds for up to `max_poll_seconds` — instead of returning empty
  immediately. This is called **long polling**. It lets an idle worker block on the database
  rather than spinning in a busy loop.

("Visibility timeout", abbreviated `vt` throughout pgmq, is how many seconds a message stays
hidden from other readers after being read. If the worker that read it does not delete it
within that window, the message becomes visible again and another worker can pick it up. It
is what makes the queue tolerate a worker crashing mid-job.)

After this plan, a Haskell user can write `Sessions.readGroupedHead (ReadGrouped {queueName =
q, visibilityTimeout = 30, qty = 5})` and get back the head message of up to five distinct
groups. You will see it working by running a test that sends messages into three groups and
asserts that a grouped-head read returns exactly one message per group — never two from the
same one.

**This plan depends on
`docs/plans/9-vendor-pgmq-1-12-0-and-add-the-native-schema-migration.md` being complete.**
That plan adds the two SQL functions to this repository's database migrations. This
package's tests install the schema from those migrations, so until plan 9 lands, every test
here would fail with a PostgreSQL error saying the function does not exist. Check that
`pgmq-migration/migrations/0003-upgrade-v1.12.0.sql` exists before you start.


## Progress

- [ ] Milestone 1: `readGroupedHead` and `readGroupedHeadWithPoll` `Statement` values added to `pgmq-hasql/src/Pgmq/Hasql/Statements/Message.hs` and exported; `cabal build pgmq-hasql` succeeds.
- [ ] Milestone 2: matching session wrappers added to `pgmq-hasql/src/Pgmq/Hasql/Sessions.hs` and exported; `cabal build pgmq-hasql` succeeds.
- [ ] Milestone 3: `testReadGroupedHead` added to `pgmq-hasql/test/AdvancedOpsSpec.hs`, proving one-message-per-group behaviour; `cabal test pgmq-hasql` passes.
- [ ] Milestone 4: `testReadGroupedHeadWithPoll` added, proving long polling both returns promptly when a message is waiting and waits when the queue is empty; `cabal test pgmq-hasql` passes.
- [ ] Milestone 5: `nix fmt` clean, `cabal build all && cabal test all` green.


## Surprises & Discoveries

(None yet.)


## Decision Log

- Decision: Reuse the existing `ReadGrouped` and `ReadGroupedWithPoll` parameter types and their existing encoders rather than introducing new types.
  Rationale: `pgmq.read_grouped_head(text, integer, integer)` has exactly the same argument list as the already-supported `pgmq.read_grouped_rr(text, integer, integer)`, and `pgmq.read_grouped_head_with_poll(text, integer, integer, integer, integer)` matches `pgmq.read_grouped_rr_with_poll` likewise. Both return `SETOF pgmq.message_record`, the same row type as every other read. `ReadGrouped` already carries exactly `queueName`, `visibilityTimeout`, `qty`; `ReadGroupedWithPoll` adds exactly `maxPollSeconds` and `pollIntervalMs`. Inventing `ReadGroupedHead` as a structural duplicate would add a type users must learn, a second encoder to keep in sync, and no type safety — the existing types already make an illegal call unrepresentable. The existing code sets the precedent: `readGrouped` and `readGroupedRoundRobin` already share the `ReadGrouped` type. Consequently `pgmq-core` and `pgmq-hasql/src/Pgmq/Hasql/Encoders.hs` need no changes at all in this plan.
  Date: 2026-07-14

- Decision: Do not add the new functions to the umbrella module `pgmq-hasql/src/Pgmq.hs` in this plan.
  Rationale: No grouped-read function is currently exported from `Pgmq` — `readGrouped`, `readGroupedWithPoll`, `readGroupedRoundRobin`, and `readGroupedRoundRobinWithPoll` are all reachable only via `Pgmq.Hasql.Sessions`. That is a real gap in the public API, and it is being fixed deliberately and in one place by `docs/plans/12-expose-grouped-reads-on-the-umbrella-api-and-release-0-5-0-0.md`, which adds a "FIFO / Grouped Reads" section exporting all six functions and the two parameter types together. Doing it piecemeal here would either leave the umbrella API half-migrated or duplicate that plan's work.
  Date: 2026-07-14


## Outcomes & Retrospective

(To be filled during and after implementation.)


## Context and Orientation

### Where you are

The repository root is `/Users/shinzui/Keikaku/bokuno/libraries/pgmq-hs-project/pgmq-hs`.
All paths below are relative to it, and every command runs from there.

Enter the development shell first — it provides GHC 9.12.4, `cabal`, and the PostgreSQL
binaries the tests need:

```bash
nix develop
```

### How a pgmq SQL function becomes a Haskell function here

`pgmq-hasql` is layered. To add one SQL function you touch two of the layers, in this order:

1. **Statement** (`pgmq-hasql/src/Pgmq/Hasql/Statements/Message.hs`) — pairs a literal SQL
   string with an *encoder* (turns a Haskell record into query parameters) and a *decoder*
   (turns result rows into Haskell values). A `Statement a b` is a `hasql` type meaning "give
   me an `a`, I will run SQL and give you back a `b`".
2. **Session** (`pgmq-hasql/src/Pgmq/Hasql/Sessions.hs`) — a one-line wrapper turning a
   `Statement` into a `Session`, which is `hasql`'s type for "a unit of work to run on a
   connection".

The other layers need no work here, and it is worth knowing *why*, so you do not go looking:

- **Parameter types** (`pgmq-hasql/src/Pgmq/Hasql/Statements/Types.hs`) — already has what we
  need; see the Decision Log.
- **Encoders** (`pgmq-hasql/src/Pgmq/Hasql/Encoders.hs`) — already has what we need.
- **Decoders** (`pgmq-hasql/src/Pgmq/Hasql/Decoders.hs`) — `messageDecoder` already decodes a
  `pgmq.message_record` row, which is what both new functions return.
- **`pgmq-hasql/src/Pgmq/Hasql/Statements.hs`** — a blanket re-export
  (`module Pgmq.Hasql.Statements.Message, ...`) with no per-symbol list, so new statements are
  re-exported automatically.
- **`pgmq-core`** — no new types.

### The exact code you are copying

The two functions you add are structurally identical to the round-robin pair that already
exists. Here is that pair, verbatim, from
`pgmq-hasql/src/Pgmq/Hasql/Statements/Message.hs` (lines 276-291):

```haskell
-- | Round-robin FIFO read - fair distribution across message groups (pgmq 1.9.0+)
-- Uses layered round-robin algorithm for fairness.
-- https://pgmq.github.io/pgmq/api/sql/functions/#read_grouped_rr
readGroupedRoundRobin :: Statement ReadGrouped (Vector Message)
readGroupedRoundRobin = preparable sql readGroupedEncoder decoder
  where
    sql = "select * from pgmq.read_grouped_rr($1,$2,$3)"
    decoder = D.rowVector messageDecoder

-- | Round-robin FIFO read with polling (pgmq 1.9.0+)
-- https://pgmq.github.io/pgmq/api/sql/functions/#read_grouped_rr_with_poll
readGroupedRoundRobinWithPoll :: Statement ReadGroupedWithPoll (Vector Message)
readGroupedRoundRobinWithPoll = preparable sql readGroupedWithPollEncoder decoder
  where
    sql = "select * from pgmq.read_grouped_rr_with_poll($1,$2,$3,$4,$5)"
    decoder = D.rowVector messageDecoder
```

`preparable` (from `pgmq-hasql/src/Pgmq/Hasql/Quasi.hs`) builds a *prepared* statement —
PostgreSQL parses and plans it once, then reuses the plan. `D` is the qualified alias for
`Hasql.Decoders`. `D.rowVector` means "decode zero or more rows into a `Vector`".

And the sessions, verbatim, from `pgmq-hasql/src/Pgmq/Hasql/Sessions.hs` (lines 223-229),
where `Msg` is the qualified alias for `Pgmq.Hasql.Statements.Message`:

```haskell
-- | Round-robin FIFO read (pgmq 1.9.0+)
readGroupedRoundRobin :: ReadGrouped -> Session (Vector Message)
readGroupedRoundRobin query = statement query Msg.readGroupedRoundRobin

-- | Round-robin FIFO read with polling (pgmq 1.9.0+)
readGroupedRoundRobinWithPoll :: ReadGroupedWithPoll -> Session (Vector Message)
readGroupedRoundRobinWithPoll query = statement query Msg.readGroupedRoundRobinWithPoll
```

### The parameter types you will use (already defined, do not redefine)

From `pgmq-hasql/src/Pgmq/Hasql/Statements/Types.hs` (lines 210-230):

```haskell
-- | Parameters for FIFO grouped read (pgmq 1.8.0+)
data ReadGrouped = ReadGrouped
  { queueName :: !QueueName,
    visibilityTimeout :: !Int32,
    qty :: !Int32
  }
  deriving stock (Generic)

-- | Parameters for FIFO grouped read with polling (pgmq 1.8.0+)
data ReadGroupedWithPoll = ReadGroupedWithPoll
  { queueName :: !QueueName,
    visibilityTimeout :: !Int32,
    qty :: !Int32,
    maxPollSeconds :: !Int32,
    pollIntervalMs :: !Int32
  }
  deriving stock (Generic)
```

Note the field-name mapping to SQL: `visibilityTimeout` is the SQL `vt` argument, and `qty`
is the SQL `qty` argument, which for the head-reads means *the maximum number of distinct
groups to take a head from* — not, as elsewhere in pgmq, the number of messages taken from one
group. The Haddock comment you write should say so, because the name alone does not.

### The SQL functions being wrapped

Added to this repository's migrations by
`docs/plans/9-vendor-pgmq-1-12-0-and-add-the-native-schema-migration.md`, in
`pgmq-migration/migrations/0003-upgrade-v1.12.0.sql`:

```sql
pgmq.read_grouped_head(queue_name text, vt integer, qty integer)
  RETURNS SETOF pgmq.message_record

pgmq.read_grouped_head_with_poll(queue_name text, vt integer, qty integer,
                                 max_poll_seconds integer DEFAULT 5,
                                 poll_interval_ms integer DEFAULT 100)
  RETURNS SETOF pgmq.message_record
```

`read_grouped_head` works by computing, for every distinct `x-pgmq-group` value in the queue
table, the lowest `msg_id` in that group; it then takes up to `qty` of those head messages
whose visibility timeout has expired, locks them with `FOR UPDATE SKIP LOCKED` (so concurrent
workers never fight over the same row), stamps a new visibility timeout on them, and returns
them. Messages with no `x-pgmq-group` header are all treated as one implicit group named
`_default_fifo_group`.

Two consequences worth internalising, because they shape the tests:

- **A group whose head is currently invisible contributes nothing.** The head is the oldest
  message *regardless of visibility*, but it is only returned if its visibility timeout has
  expired. So a group being actively worked on is simply skipped — it does not fall back to
  the group's second message. That is exactly what preserves in-group ordering.
- **You can never get two messages from the same group in one call.** This is the property
  the test must assert; asserting only on the returned count would pass even against
  `read_grouped_rr` and would therefore prove nothing.

### How the tests work

`pgmq-hasql/test/EphemeralDb.hs` starts a throwaway PostgreSQL server (via the `ephemeral-pg`
library), then installs the pgmq schema by running this repository's own migrations:

```haskell
component <- either (error . ("Invalid PGMQ migration component: " <>) . show) pure Migration.pgmqMigrations
plan <- either (error . ("Invalid PGMQ migration plan: " <>) . show) pure (migrationPlan (component :| []))
installResult <- runMigrationPlan defaultRunOptions connSettings plan
```

**This is why plan 9 is a hard prerequisite**: `Migration.pgmqMigrations` is the
`pgmq-migration` package, and until it ships `0003-upgrade-v1.12.0.sql`, the test database
will not contain `pgmq.read_grouped_head` and every test in this plan will fail with a
PostgreSQL `undefined function` error. No external database is needed and nothing you run
touches a real one.

`pgmq-hasql/test/Main.hs` runs eight specs against one shared connection pool. The one you
will edit is `pgmq-hasql/test/AdvancedOpsSpec.hs`, which already contains `testReadGrouped`
(lines 291-331) and `testReadGroupedRoundRobin` (lines 334-378) — your copy targets. Both use
the `withTestFixture` helper, which allocates a queue with a random name so tests can run
concurrently without colliding, and both clean up with `Sessions.batchDeleteMessages` followed
by `cleanupQueue`.

Worth knowing: **neither `readGroupedWithPoll` nor `readGroupedRoundRobinWithPoll` has any
test today.** The polling test in Milestone 4 will be the first in the repository, so there is
no copy target for it and the plan spells it out in full.


## Plan of Work

### Milestone 1 — Add the two `Statement` values

**Scope.** Add `readGroupedHead` and `readGroupedHeadWithPoll` to
`pgmq-hasql/src/Pgmq/Hasql/Statements/Message.hs`. At the end of this milestone the package
compiles and the statements exist, but nothing calls them yet.

Add to the module's export list, immediately after the existing round-robin block (which
reads `-- Round-robin FIFO functions (pgmq 1.9.0+)` / `readGroupedRoundRobin,` /
`readGroupedRoundRobinWithPoll,`):

```haskell
    -- Grouped-head FIFO functions (pgmq 1.12.0+)
    readGroupedHead,
    readGroupedHeadWithPoll,
```

Add the definitions at the end of the round-robin block in the body of the module:

```haskell
-- | Grouped-head FIFO read (pgmq 1.12.0+)
-- Returns the oldest visible message from each of up to @qty@ distinct message
-- groups, at most one message per group. Groups are identified by the
-- @x-pgmq-group@ message header; messages without that header form a single
-- implicit group. A group whose head message is currently invisible (because
-- another reader holds it) is skipped rather than yielding its second message,
-- which is what preserves ordering within a group.
--
-- Use this to scale ordered processing horizontally: each of N workers calls
-- this and takes the head of a different group, so all groups progress in
-- parallel while each group stays strictly in order.
--
-- Note that @qty@ bounds the number of /groups/ read from, not the number of
-- messages taken from one group.
-- https://pgmq.github.io/pgmq/api/sql/functions/#read_grouped_head
readGroupedHead :: Statement ReadGrouped (Vector Message)
readGroupedHead = preparable sql readGroupedEncoder decoder
  where
    sql = "select * from pgmq.read_grouped_head($1,$2,$3)"
    decoder = D.rowVector messageDecoder

-- | Grouped-head FIFO read with long polling (pgmq 1.12.0+)
-- As 'readGroupedHead', but when no message is available it re-checks every
-- @pollIntervalMs@ milliseconds for up to @maxPollSeconds@ seconds before
-- returning empty, instead of returning empty immediately. This lets an idle
-- worker wait on the database rather than spinning in a busy loop.
--
-- The poll happens inside PostgreSQL, so the database connection is held for
-- the duration. Size @maxPollSeconds@ against your connection pool accordingly.
-- https://pgmq.github.io/pgmq/api/sql/functions/#read_grouped_head_with_poll
readGroupedHeadWithPoll :: Statement ReadGroupedWithPoll (Vector Message)
readGroupedHeadWithPoll = preparable sql readGroupedWithPollEncoder decoder
  where
    sql = "select * from pgmq.read_grouped_head_with_poll($1,$2,$3,$4,$5)"
    decoder = D.rowVector messageDecoder
```

Every name used here — `Statement`, `ReadGrouped`, `ReadGroupedWithPoll`, `Vector`,
`Message`, `preparable`, `readGroupedEncoder`, `readGroupedWithPollEncoder`, `D`,
`messageDecoder` — is **already imported** by this module for the round-robin statements. You
should not need to add a single import. If GHC reports one missing, you have mistyped a name.

**Acceptance.** `cabal build pgmq-hasql` succeeds. The package builds with `-Wall` and
`-Wmissing-export-lists`, so a definition you forgot to export produces an unused-binding
warning.

### Milestone 2 — Add the two session wrappers

**Scope.** Add the one-line `Session` wrappers to `pgmq-hasql/src/Pgmq/Hasql/Sessions.hs`.
At the end of this milestone the functions are callable by a user of the library.

Add to the export list, after the existing round-robin entries:

```haskell
    -- Grouped-head FIFO functions (pgmq 1.12.0+)
    readGroupedHead,
    readGroupedHeadWithPoll,
```

Add the definitions after `readGroupedRoundRobinWithPoll`:

```haskell
-- | Grouped-head FIFO read - one message from each of up to qty groups (pgmq 1.12.0+)
readGroupedHead :: ReadGrouped -> Session (Vector Message)
readGroupedHead query = statement query Msg.readGroupedHead

-- | Grouped-head FIFO read with long polling (pgmq 1.12.0+)
readGroupedHeadWithPoll :: ReadGroupedWithPoll -> Session (Vector Message)
readGroupedHeadWithPoll query = statement query Msg.readGroupedHeadWithPoll
```

As in Milestone 1, every name is already imported.

**Acceptance.** `cabal build pgmq-hasql` succeeds.

### Milestone 3 — Test that a grouped-head read takes one message per group

**Scope.** Add `testReadGroupedHead` to `pgmq-hasql/test/AdvancedOpsSpec.hs` and register it.
This is the first thing that proves the feature actually works rather than merely compiles.

**Design the assertion so it can only pass for the right reason.** Asserting "3 messages were
returned" would pass equally against `read_grouped_rr`, and would tell you nothing. The
property that distinguishes a head-read is *at most one message per group*. So: send two
messages into each of three groups (six messages), read with `qty = 3`, and assert both that
three messages came back **and** that their `x-pgmq-group` header values are three *distinct*
groups. Then assert the stronger ordering property: each returned message is its group's
*first* message, not its second.

To check the group of a returned message you read its headers. `Message` (from `pgmq-core`,
`pgmq-core/src/Pgmq/Types.hs`) carries a `headers` field holding the JSON headers the message
was sent with; the messages are sent with `MessageHeaders (object ["x-pgmq-group" .= (...)])`,
so extract `"x-pgmq-group"` back out of that JSON. Distinguishing first-from-second message
within a group is easiest via the message *body*, which the existing tests already vary
(`MessageBody (object ["msg" .= (1 :: Int)])` and so on): send body `1` and `2` into group A,
`3` and `4` into group B, `5` and `6` into group C, and assert the returned bodies are exactly
`{1, 3, 5}` — the three heads. If any of `2`, `4`, or `6` comes back, ordering within a group
was violated and the test must fail.

Follow the existing structure exactly: `withTestFixture p $ \TestFixture {pool, queueName} ->`,
then `Sessions.createQueue`, then `Sessions.createFifoIndex` (which creates the index that
makes group lookups fast — the existing grouped tests all call it), then
`Sessions.batchSendMessageWithHeaders`, then the read, then
`Sessions.batchDeleteMessages` and `cleanupQueue`.

Register it in the `tests` list alongside `testReadGrouped` and `testReadGroupedRoundRobin`.

**Acceptance.** `cabal test pgmq-hasql` passes. Then confirm the test is not vacuous by
temporarily pointing `readGroupedHead`'s SQL at `pgmq.read_grouped_rr` instead and re-running:
the distinct-groups assertion must fail, because round-robin will happily return two messages
from one group. Revert the change. Record the observed failure in Surprises & Discoveries as
evidence.

### Milestone 4 — Test long polling in both directions

**Scope.** Add `testReadGroupedHeadWithPoll`. There is no existing polling test anywhere in
this repository to copy, so this milestone specifies it fully.

A polling test that only checks "a message came back" would pass even if the poll loop were
broken, because the message was already there. You must test both directions:

**Direction one — a message is waiting, so polling returns promptly and does not wait out the
timeout.** Send messages into two groups. Call `readGroupedHeadWithPoll` with
`maxPollSeconds = 5`, `pollIntervalMs = 100`, `qty = 2`. Assert two messages come back from
two distinct groups. Then assert it returned *quickly*: wrap the call in a wall-clock
measurement (`Data.Time.Clock.getCurrentTime` before and after, `diffUTCTime` between) and
assert the elapsed time is comfortably under `maxPollSeconds` — under two seconds is a safe
bound that will not flake on a slow machine while still failing if the function wrongly
blocked for the full five.

**Direction two — the queue is empty, so polling waits and then returns empty.** On a fresh
queue with nothing in it, call `readGroupedHeadWithPoll` with a *short* `maxPollSeconds` — use
`1`, not `5`, so the suite does not get slower than it needs to be — and `pollIntervalMs =
100`. Assert the result is empty, and assert the elapsed time is at least roughly
`maxPollSeconds`. Give the lower bound a little slack (assert at least ~0.9 seconds rather
than exactly 1.0) because `clock_timestamp()` inside PostgreSQL and `getCurrentTime` in the
test process are not the same clock and the loop checks its deadline at interval boundaries.
This half of the test is what actually proves the poll loop runs: if
`read_grouped_head_with_poll` were wired to plain `read_grouped_head` by mistake, it would
return empty *immediately* and this assertion would catch it.

Keep the total added test time near one second. Do not raise `maxPollSeconds` "to be safe" —
a longer timeout makes the empty-queue case slower without making it more correct.

Note the resource implication, and put it in the Haddock (Milestone 1 already does): the poll
loop runs *inside* PostgreSQL via `pg_sleep`, so the connection is held for the whole
`maxPollSeconds`. A pool of N connections can therefore support at most N concurrently polling
workers.

Register the test in the `tests` list.

**Acceptance.** `cabal test pgmq-hasql` passes, and the suite has not become noticeably
slower (the empty-queue case should add about one second).

### Milestone 5 — Format and verify the whole project

**Scope.** No new behaviour. Run the formatter and the full build and test suite to confirm
nothing else regressed.

The project uses `treefmt` behind a pre-commit hook. If you commit unformatted code the hook
reformats the files and fails the commit, and you must stage the reformatted files and commit
again. Save yourself the round trip by running `nix fmt` first.

**Acceptance.** `nix fmt` makes no further changes; `cabal build all` and `cabal test all`
are green.


## Concrete Steps

All commands run from `/Users/shinzui/Keikaku/bokuno/libraries/pgmq-hs-project/pgmq-hs`
inside `nix develop`.

**Before starting, confirm the prerequisite is in place:**

```bash
ls pgmq-migration/migrations/0003-upgrade-v1.12.0.sql
```

If that file does not exist, stop: implement
`docs/plans/9-vendor-pgmq-1-12-0-and-add-the-native-schema-migration.md` first. Without it,
every test in this plan fails with a PostgreSQL error reading roughly
`function pgmq.read_grouped_head(unknown, integer, integer) does not exist`.

**Milestones 1 and 2** — edit
`pgmq-hasql/src/Pgmq/Hasql/Statements/Message.hs` and `pgmq-hasql/src/Pgmq/Hasql/Sessions.hs`
as described, then:

```bash
cabal build pgmq-hasql
```

**Milestones 3 and 4** — edit `pgmq-hasql/test/AdvancedOpsSpec.hs`, then:

```bash
cabal test pgmq-hasql
```

Expected output ends with a line like:

```text
All 24 tests passed (12.3s)
```

(The exact count depends on how many tests exist when you run it; what matters is that none
fail.) To see only your new tests while iterating, `tasty` supports a pattern filter:

```bash
cabal test pgmq-hasql --test-options='--pattern "GroupedHead"'
```

**Milestone 5:**

```bash
nix fmt
cabal build all
cabal test all
```

Commit:

```bash
git add -A
git commit -m "$(cat <<'EOF'
feat(pgmq-hasql): add grouped-head reads for pgmq 1.12.0

Wrap pgmq.read_grouped_head and pgmq.read_grouped_head_with_poll as
readGroupedHead and readGroupedHeadWithPoll, reusing the existing
ReadGrouped/ReadGroupedWithPoll parameter types and encoders, since the
new SQL functions share their signatures with the round-robin pair.

Tests assert the defining property of a head read -- at most one message
per FIFO group, and always the group's oldest -- rather than only the
returned count, which would pass against read_grouped_rr too. The
polling test covers both a waiting message (returns promptly) and an
empty queue (waits out max_poll_seconds, then returns empty).

MasterPlan: docs/masterplans/2-support-pgmq-1-12-0-grouped-head-reads.md
ExecPlan: docs/plans/10-add-grouped-head-read-statements-and-sessions-to-pgmq-hasql.md
Intention: intention_01kxgh9geke2dayhx57qp6g9ye
EOF
)"
```


## Validation and Acceptance

The plan is complete when all of the following hold.

**The library exposes the two functions with these exact signatures.** From
`Pgmq.Hasql.Sessions`:

```haskell
readGroupedHead :: ReadGrouped -> Session (Vector Message)
readGroupedHeadWithPoll :: ReadGroupedWithPoll -> Session (Vector Message)
```

and from `Pgmq.Hasql.Statements.Message`:

```haskell
readGroupedHead :: Statement ReadGrouped (Vector Message)
readGroupedHeadWithPoll :: Statement ReadGroupedWithPoll (Vector Message)
```

**A grouped-head read returns one message per group, and it is the group's oldest.** This is
the behaviour a user cares about and the thing `testReadGroupedHead` proves. Concretely: with
six messages carrying bodies `1`..`6` sent into groups A, A, B, B, C, C respectively, a call
with `qty = 3` returns exactly three messages, whose bodies are `1`, `3`, and `5` in some
order, and whose `x-pgmq-group` headers are three distinct values. It never returns `2`, `4`,
or `6` while the corresponding head is still outstanding.

**Long polling actually polls.** `testReadGroupedHeadWithPoll` proves it in both directions:
with a message waiting the call returns in well under `maxPollSeconds`; against an empty queue
it blocks for approximately `maxPollSeconds` and then returns an empty vector. The second half
is the one that has teeth — a broken implementation that ignored the poll loop would return
empty instantly and fail it.

**Neither test can pass for the wrong reason.** Verify this rather than assuming it. Point
`readGroupedHead`'s SQL string at `pgmq.read_grouped_rr` and re-run `cabal test pgmq-hasql`:
the distinct-group assertion must fail, because round-robin can return two messages from one
group. Restore the SQL afterwards. Paste the failure output into Surprises & Discoveries.

**Nothing else regressed.** `cabal build all` and `cabal test all` are green, and `nix fmt`
reports no changes.

**No new types or encoders were added.** `git diff --stat` should show changes confined to
`pgmq-hasql/src/Pgmq/Hasql/Statements/Message.hs`,
`pgmq-hasql/src/Pgmq/Hasql/Sessions.hs`, and `pgmq-hasql/test/AdvancedOpsSpec.hs`. If
`pgmq-core/src/Pgmq/Types.hs`, `pgmq-hasql/src/Pgmq/Hasql/Statements/Types.hs`, or
`pgmq-hasql/src/Pgmq/Hasql/Encoders.hs` appear in the diff, re-read the Decision Log — the
existing types are meant to be reused.


## Idempotence and Recovery

**Every change here is additive.** Nothing existing is modified or removed, so there is no
migration step, no data to back up, and no way for a partially-applied change to corrupt
anything. If a milestone goes wrong, `git checkout -- <file>` restores it and you can start
that milestone over.

**The tests are safe to run repeatedly.** Each spins up its own throwaway PostgreSQL server
via `ephemeral-pg` — no external or production database is contacted. Each test allocates a
randomly-named queue through `withTestFixture` and deletes it afterwards, so repeated runs and
concurrent tests do not collide.

**If a test fails with "function pgmq.read_grouped_head does not exist",** the prerequisite
plan is not in place or its migration did not take effect. Confirm
`pgmq-migration/migrations/0003-upgrade-v1.12.0.sql` exists and is listed in
`pgmq-migration/migrations/manifest`, then force a rebuild of the migration package, whose SQL
is embedded at compile time and can go stale:

```bash
cabal build pgmq-migration --ghc-options=-fforce-recomp
cabal test pgmq-hasql
```

**If the polling test is flaky,** do not fix it by widening the timing bounds until it always
passes — that would silently disable the assertion that gives it its value. The bounds
recommended in Milestone 4 (an upper bound of ~2s against `maxPollSeconds = 5`, and a lower
bound of ~0.9s against `maxPollSeconds = 1`) have generous margins in both directions. A
failure outside them is far more likely to be a real defect than a slow machine. Investigate
before relaxing, and record what you find.


## Interfaces and Dependencies

**No new package dependencies.** Everything needed is already a dependency of `pgmq-hasql` or
its test suite. `hasql` supplies `Statement` and `Session`; `vector` supplies `Vector`;
`aeson` supplies `object` / `(.=)` and the `Value` type used for message headers and bodies;
`tasty` and `tasty-hunit` supply the test framework; `ephemeral-pg` supplies the throwaway
database. Milestone 4's timing assertions use `Data.Time.Clock` (`getCurrentTime`,
`diffUTCTime`) from the `time` package, which is already listed in the `pgmq-hasql` test-suite
`build-depends` — no `.cabal` change is needed anywhere in this plan.

**Modules you will change:**

- `pgmq-hasql/src/Pgmq/Hasql/Statements/Message.hs` — add two `Statement` values and export them.
- `pgmq-hasql/src/Pgmq/Hasql/Sessions.hs` — add two `Session` wrappers and export them.
- `pgmq-hasql/test/AdvancedOpsSpec.hs` — add two tests and register them in `tests`.

**Modules you must NOT change, and why** (each is a place a reasonable person might otherwise
go): `pgmq-hasql/src/Pgmq/Hasql/Statements/Types.hs` and `pgmq-hasql/src/Pgmq/Hasql/Encoders.hs`
already provide `ReadGrouped` / `ReadGroupedWithPoll` and their encoders, which the new
functions share with the round-robin pair. `pgmq-hasql/src/Pgmq/Hasql/Decoders.hs` already
provides `messageDecoder` for the `pgmq.message_record` rows both functions return.
`pgmq-hasql/src/Pgmq/Hasql/Statements.hs` re-exports whole modules, so it picks up new
statements with no edit. `pgmq-hasql/src/Pgmq.hs` is intentionally deferred to
`docs/plans/12-expose-grouped-reads-on-the-umbrella-api-and-release-0-5-0-0.md`.

**What this plan hands to the next one.**
`docs/plans/11-add-grouped-head-read-effects-and-traced-spans-to-pgmq-effectful.md`
consumes exactly these two session functions and cannot compile without them:

```haskell
Pgmq.Hasql.Sessions.readGroupedHead :: ReadGrouped -> Session (Vector Message)
Pgmq.Hasql.Sessions.readGroupedHeadWithPoll :: ReadGroupedWithPoll -> Session (Vector Message)
```

The names and types above are a contract. If you rename them, update plan 11 to match before
you commit.
