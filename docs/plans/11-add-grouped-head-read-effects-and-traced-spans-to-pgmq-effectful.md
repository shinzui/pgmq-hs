---
id: 11
slug: add-grouped-head-read-effects-and-traced-spans-to-pgmq-effectful
title: "Add grouped-head read effects and traced spans to pgmq-effectful"
kind: exec-plan
created_at: 2026-07-14T14:55:11Z
intention: "intention_01kxgh9geke2dayhx57qp6g9ye"
master_plan: "docs/masterplans/2-support-pgmq-1-12-0-grouped-head-reads.md"
---

# Add grouped-head read effects and traced spans to pgmq-effectful

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.


## Purpose / Big Picture

`pgmq` is a message queue built into PostgreSQL: a queue is a table, and every queue
operation is a PostgreSQL function such as `pgmq.send(...)` or `pgmq.read(...)`. This
repository, `pgmq-hs`, is a Haskell client. It has two layers a user might program against:

- `pgmq-hasql` — the direct layer. You call a function, it runs SQL, you get a result.
- `pgmq-effectful` — the same operations expressed as an *effect*. "Effectful" is a Haskell
  library for writing code against an abstract capability (here: "this code can use a message
  queue") and deciding later how that capability is actually provided. The benefit is that the
  same application code can run against a real database, or against a traced interpreter that
  emits OpenTelemetry spans for every queue operation, without the application code changing.

pgmq 1.12.0 adds two new operations, and a companion plan
(`docs/plans/10-add-grouped-head-read-statements-and-sessions-to-pgmq-hasql.md`) has already
made them available in `pgmq-hasql`. This plan brings them to `pgmq-effectful`, so that users
of the effect layer get them too — including automatic distributed tracing.

The two operations, briefly. Messages in pgmq can carry a JSON header named `x-pgmq-group`;
all messages sharing a value in that header form a **FIFO group** ("first in, first out"),
and pgmq guarantees they are consumed in order. `readGroupedHead` returns the single oldest
message from each of up to `qty` *distinct* groups — never two from the same group — which
lets a fleet of workers each take the head of a different group so all groups progress in
parallel while order inside each group is preserved. `readGroupedHeadWithPoll` does the same
but waits (**long polling**) for up to `maxPollSeconds` when the queue is empty, rather than
returning empty immediately, so an idle worker can block on the database instead of spinning.

After this plan, a user writing effectful code can call `readGroupedHead someQuery` in any
`Eff` context that has the `Pgmq` effect, and — if they run it under the *traced* interpreter
— automatically get an OpenTelemetry span for the operation, with the same attributes as
every other pgmq read. You will see it working by running a test that asserts a span named
`receive <queue>` is emitted with the attribute `db.operation = "pgmq.read_grouped_head"`.

**This plan depends on
`docs/plans/10-add-grouped-head-read-statements-and-sessions-to-pgmq-hasql.md` being
complete.** The interpreters in this package are thin wrappers that call the session
functions that plan adds. Without them, this package will not compile. Confirm before you
start:

```bash
grep -n "readGroupedHead" pgmq-hasql/src/Pgmq/Hasql/Sessions.hs
```

That must print the exports and definitions of `readGroupedHead` and
`readGroupedHeadWithPoll`. If it prints nothing, implement plan 10 first.


## Progress

- [ ] Milestone 1: `ReadGroupedHead` and `ReadGroupedHeadWithPoll` constructors added to the `Pgmq` effect GADT in `pgmq-effectful/src/Pgmq/Effectful/Effect.hs`, with smart constructors and exports; `cabal build pgmq-effectful` fails only with the expected non-exhaustive-pattern warnings from the two interpreters.
- [ ] Milestone 2: plain interpreter arms added in `pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs`; `cabal build pgmq-effectful` succeeds.
- [ ] Milestone 3: traced interpreter arms added in `pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs` emitting Consumer spans with `db.operation` set to the new SQL function names; `cabal build pgmq-effectful` succeeds.
- [ ] Milestone 4: `TracedInterpreterSpec` test added asserting the span name, span kind, and attributes emitted by `readGroupedHead`; `cabal test pgmq-effectful` passes.
- [ ] Milestone 5: `nix fmt` clean, `cabal build all && cabal test all` green.


## Surprises & Discoveries

(None yet.)


## Decision Log

- Decision: Reuse the existing `ReadGrouped` and `ReadGroupedWithPoll` parameter types for the new effect constructors rather than introducing new ones.
  Rationale: This mirrors the decision already taken in `pgmq-hasql` (see `docs/plans/10-add-grouped-head-read-statements-and-sessions-to-pgmq-hasql.md`), and it mirrors what the existing code already does — `ReadGroupedRoundRobin :: ReadGrouped -> Pgmq m (Vector Message)` reuses the same type as `ReadGrouped :: ReadGrouped -> Pgmq m (Vector Message)`. The new SQL functions take exactly the same arguments and return exactly the same row type. A structurally identical duplicate type would be a burden for users and a second thing to keep in sync, for no added type safety.
  Date: 2026-07-14

- Decision: Emit the new operations as OpenTelemetry Consumer spans via the existing `receiveOp` helper, with `db.operation` set to `"pgmq.read_grouped_head"` and `"pgmq.read_grouped_head_with_poll"`.
  Rationale: These are message-receive operations, exactly like every other pgmq read. The traced interpreter already has a `receiveOp` helper that sets span kind `Consumer` and `messaging.operation = "receive"`, and derives the span *name* from the operation and destination (`"receive my-queue"`), not from the SQL function. The SQL function name lands in the `db.operation` attribute. Following that convention keeps every pgmq read comparable in a trace viewer, which is the point of having a convention. It also means no changes at all are needed to `pgmq-effectful/src/Pgmq/Effectful/Telemetry.hs` — every attribute key involved is already defined there.
  Date: 2026-07-14

- Decision: Do not re-export the new functions from the umbrella module `pgmq-effectful/src/Pgmq/Effectful.hs` in this plan.
  Rationale: No grouped-read function is exported from `Pgmq.Effectful` today; all four existing ones are reachable only via `Pgmq.Effectful.Effect`. That gap is being closed deliberately, for all six grouped reads at once, by `docs/plans/12-expose-grouped-reads-on-the-umbrella-api-and-release-0-5-0-0.md`. Doing part of it here would leave the umbrella API half-migrated.
  Date: 2026-07-14


## Outcomes & Retrospective

(To be filled during and after implementation.)


## Context and Orientation

### Where you are

The repository root is `/Users/shinzui/Keikaku/bokuno/libraries/pgmq-hs-project/pgmq-hs`.
All paths are relative to it and all commands run from there. Enter the development shell
first, which provides GHC 9.12.4, `cabal`, and the PostgreSQL binaries the tests need:

```bash
nix develop
```

### How an operation is added to `pgmq-effectful`

The package has three moving parts, and adding an operation means touching all three. There
is a helpful property here: the two interpreters `case` exhaustively over the effect type, so
**once you add a constructor in step 1, the compiler will tell you exactly what is missing in
steps 2 and 3.** You cannot forget one.

1. **The effect** — `pgmq-effectful/src/Pgmq/Effectful/Effect.hs` defines a GADT (a data type
   whose constructors each declare their own result type) named `Pgmq`. Each constructor is
   one queue operation. Alongside it, each constructor gets a one-line "smart constructor"
   that a user actually calls.

2. **The plain interpreter** — `pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs` maps each
   constructor to the corresponding `pgmq-hasql` session and runs it on a connection pool.

3. **The traced interpreter** — `pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs`
   does the same, but wraps each session in an OpenTelemetry span.

Two more modules exist and **need no changes**, which is worth knowing so you do not go
looking: `pgmq-effectful/src/Pgmq/Effectful/Telemetry.hs` is about propagating trace context
*through message headers* and re-exporting attribute keys — all the keys we need are already
there. `pgmq-effectful/src/Pgmq/Effectful/Traced.hs` is a small helper module about that same
header propagation, unrelated to adding operations.

### The exact code you are copying

**In `pgmq-effectful/src/Pgmq/Effectful/Effect.hs`.** The GADT constructors (lines 168-173):

```haskell
  -- FIFO Read (pgmq 1.8.0+)
  ReadGrouped :: ReadGrouped -> Pgmq m (Vector Message)
  ReadGroupedWithPoll :: ReadGroupedWithPoll -> Pgmq m (Vector Message)
  -- Round-robin FIFO Read (pgmq 1.9.0+)
  ReadGroupedRoundRobin :: ReadGrouped -> Pgmq m (Vector Message)
  ReadGroupedRoundRobinWithPoll :: ReadGroupedWithPoll -> Pgmq m (Vector Message)
```

Note that the constructor `ReadGrouped` and the *type* `ReadGrouped` share a name — that is
fine and intentional in Haskell (constructors and types live in different namespaces), and
the existing code relies on it. The smart constructors (lines 305-311):

```haskell
-- | Round-robin FIFO read (pgmq 1.9.0+)
readGroupedRoundRobin :: (Pgmq :> es) => ReadGrouped -> Eff es (Vector Message)
readGroupedRoundRobin = send . ReadGroupedRoundRobin

-- | Round-robin FIFO read with polling (pgmq 1.9.0+)
readGroupedRoundRobinWithPoll :: (Pgmq :> es) => ReadGroupedWithPoll -> Eff es (Vector Message)
readGroupedRoundRobinWithPoll = send . ReadGroupedRoundRobinWithPoll
```

`(Pgmq :> es)` reads as "the effect list `es` includes the `Pgmq` effect". `send` is
`effectful`'s way of turning a constructor into a usable action.

**In `pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs`.** `runPgmq` is one big
`interpret $ \_ -> \case` with one arm per constructor (lines 126-131):

```haskell
  -- FIFO Read (pgmq 1.8.0+)
  ReadGrouped query -> runSession pool $ Sessions.readGrouped query
  ReadGroupedWithPoll query -> runSession pool $ Sessions.readGroupedWithPoll query
  -- Round-robin FIFO Read (pgmq 1.9.0+)
  ReadGroupedRoundRobin query -> runSession pool $ Sessions.readGroupedRoundRobin query
  ReadGroupedRoundRobinWithPoll query -> runSession pool $ Sessions.readGroupedRoundRobinWithPoll query
```

`runSession` is a local helper that takes a connection from the pool, runs the session, and
converts any database failure into the package's `PgmqRuntimeError` type.

**In `pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs`** (lines 241-254):

```haskell
  -- FIFO Read (Consumer spans)
  ReadGrouped query@(Types.ReadGrouped qn _ _) ->
    withTracedOp config pool (receiveOp "pgmq.read_grouped" qn) $
      Sessions.readGrouped query
  ReadGroupedWithPoll query@(Types.ReadGroupedWithPoll qn _ _ _ _) ->
    withTracedOp config pool (receiveOp "pgmq.read_grouped_with_poll" qn) $
      Sessions.readGroupedWithPoll query
  -- Round-robin FIFO Read (Consumer spans)
  ReadGroupedRoundRobin query@(Types.ReadGrouped qn _ _) ->
    withTracedOp config pool (receiveOp "pgmq.read_grouped_rr" qn) $
      Sessions.readGroupedRoundRobin query
  ReadGroupedRoundRobinWithPoll query@(Types.ReadGroupedWithPoll qn _ _ _ _) ->
    withTracedOp config pool (receiveOp "pgmq.read_grouped_rr_with_poll" qn) $
      Sessions.readGroupedRoundRobinWithPoll query
```

Read that carefully, because the pattern-match idiom is the only subtle thing in this plan.
`query@(Types.ReadGrouped qn _ _)` binds the whole record to `query` (to pass to the session)
*and* positionally destructures it to pull out the queue name as `qn` (to pass to the span
helper). The underscores are the fields the span does not need. `Types` is the qualified
alias for `Pgmq.Hasql.Statements.Types`. **The number of underscores must match the number of
fields in the record** — three fields for `ReadGrouped` (`queueName`, `visibilityTimeout`,
`qty`), five for `ReadGroupedWithPoll` (those three plus `maxPollSeconds` and
`pollIntervalMs`). Get this wrong and GHC will tell you, but the error mentions arity rather
than field names, so it is worth knowing in advance.

### How tracing works here, in plain terms

OpenTelemetry is a standard for recording what a program did as a tree of timed **spans**.
Each span has a *name*, a *kind* (is this program producing a message, consuming one, serving
a request…), and a set of key/value **attributes**. A trace viewer shows them as a timeline.

The traced interpreter builds spans through helpers already defined in
`pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs`. The one that matters:

```haskell
receiveOp fn qn = (queueOp fn OTel.Consumer qn) {opMessagingKind = Just "receive"}
```

So `receiveOp "pgmq.read_grouped_head" queueName` yields an operation description that
produces a span with:

- **kind** `Consumer` — this program is consuming a message.
- **attribute** `messaging.operation` = `"receive"`.
- **attribute** `messaging.system` = `"pgmq"`.
- **attribute** `messaging.destination.name` = the queue name.
- **attribute** `db.system` = `"postgresql"`.
- **attribute** `db.operation` = the string you passed in — this is where
  `"pgmq.read_grouped_head"` lands.
- **name** = `"receive <queue name>"` — note carefully: the span name is derived from the
  messaging operation and the destination, **not** from the SQL function. So a
  `readGroupedHead` on queue `orders` produces a span named `receive orders`, exactly like a
  plain `readMessage` on `orders` would. The two are told apart by the `db.operation`
  attribute. This surprises people; it is the existing convention and this plan follows it.

The only thing you supply is that SQL function name string. All the rest is already built.

### How the tests work

`pgmq-effectful/test/EphemeralDb.hs` starts a throwaway PostgreSQL server (via `ephemeral-pg`)
and installs the pgmq schema by running this repository's own migrations from the
`pgmq-migration` package. No external database is involved and nothing you run touches a real
one. This is the transitive reason plan 9 must land before plan 10 before this plan: the SQL
function has to exist in the test database.

`pgmq-effectful/test/Main.hs` runs three specs. The relevant one is
`pgmq-effectful/test/TracedInterpreterSpec.hs`, which captures emitted spans in memory and
asserts on them. Its existing receive test is your copy target (lines 111-150), and shows every
helper you need:

```haskell
              s <- singleSpan receiveSpans "receive"
              actualSpanName <- spanName s
              assertEqual
                "span name carries destination"
                ("receive " <> queueNameToText queue)
                actualSpanName
              assertSpanKindConsumer s
              assertAttrText s "messaging.system" "pgmq"
              assertAttrText s "messaging.operation" "receive"
              assertAttrText s "messaging.destination.name" (queueNameToText queue)
              assertAttrText s "db.system" "postgresql"
              assertAttrText s "db.operation" "pgmq.read",
```

Available helpers in that file: `setupTracer`, `mkUniqueQueue`, `withSemconvOptIn`,
`spansWithFirstWord`, `singleSpan`, `assertAttrText`, `assertSpanKindConsumer`, `assertRight`.

Note that **no grouped read has any test in `pgmq-effectful` today** — the string "Grouped"
does not appear in either spec. The test you add in Milestone 4 will be the first, so the plan
specifies it in full rather than pointing at a sibling.


## Plan of Work

### Milestone 1 — Add the effect constructors

**Scope.** Add two constructors to the `Pgmq` GADT, their smart constructors, and their
exports, in `pgmq-effectful/src/Pgmq/Effectful/Effect.hs`. At the end of this milestone the
package will **not** compile cleanly — and that is the point. Both interpreters `case`
exhaustively over the GADT, so GHC will now report non-exhaustive patterns (or, since the
package builds with `-Wall`, incomplete-pattern warnings) naming precisely the two arms you
must add in Milestones 2 and 3. Read those messages; they are your checklist.

Add to the export list, after the existing round-robin block (which reads
`-- ** Round-Robin FIFO Read (pgmq 1.9.0+)` / `readGroupedRoundRobin,` /
`readGroupedRoundRobinWithPoll,`):

```haskell
    -- ** Grouped-Head FIFO Read (pgmq 1.12.0+)
    readGroupedHead,
    readGroupedHeadWithPoll,
```

Add to the GADT, after `ReadGroupedRoundRobinWithPoll`:

```haskell
  -- Grouped-head FIFO Read (pgmq 1.12.0+)
  ReadGroupedHead :: ReadGrouped -> Pgmq m (Vector Message)
  ReadGroupedHeadWithPoll :: ReadGroupedWithPoll -> Pgmq m (Vector Message)
```

Add the smart constructors, after `readGroupedRoundRobinWithPoll`:

```haskell
-- | Grouped-head FIFO read (pgmq 1.12.0+)
-- Returns the oldest visible message from each of up to @qty@ distinct message
-- groups, at most one message per group. Groups are identified by the
-- @x-pgmq-group@ message header. A group whose head message is currently held by
-- another reader is skipped rather than yielding its second message, which is
-- what preserves ordering within a group.
--
-- Note that @qty@ bounds the number of /groups/ read from, not the number of
-- messages taken from one group.
readGroupedHead :: (Pgmq :> es) => ReadGrouped -> Eff es (Vector Message)
readGroupedHead = send . ReadGroupedHead

-- | Grouped-head FIFO read with long polling (pgmq 1.12.0+)
-- As 'readGroupedHead', but waits up to @maxPollSeconds@ (re-checking every
-- @pollIntervalMs@) for a message to become available instead of returning empty
-- immediately. The wait happens inside PostgreSQL, so a database connection is
-- held for the duration.
readGroupedHeadWithPoll :: (Pgmq :> es) => ReadGroupedWithPoll -> Eff es (Vector Message)
readGroupedHeadWithPoll = send . ReadGroupedHeadWithPoll
```

The types `ReadGrouped`, `ReadGroupedWithPoll`, `Vector`, and `Message` are already imported
by this module. No import changes are needed.

**Acceptance.** `cabal build pgmq-effectful` reports incomplete-pattern problems in
`Interpreter.hs` and `Interpreter/Traced.hs` naming `ReadGroupedHead` and
`ReadGroupedHeadWithPoll`, and no other errors. Copy those messages into Surprises &
Discoveries if they are not what you expected.

### Milestone 2 — Wire the plain interpreter

**Scope.** Add two arms to `runPgmq` in `pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs`,
after the round-robin arms:

```haskell
  -- Grouped-head FIFO Read (pgmq 1.12.0+)
  ReadGroupedHead query -> runSession pool $ Sessions.readGroupedHead query
  ReadGroupedHeadWithPoll query -> runSession pool $ Sessions.readGroupedHeadWithPoll query
```

`Sessions.readGroupedHead` and `Sessions.readGroupedHeadWithPoll` are the functions added by
`docs/plans/10-add-grouped-head-read-statements-and-sessions-to-pgmq-hasql.md`. If GHC says
they do not exist, that plan is not complete — stop and finish it.

**Acceptance.** `cabal build pgmq-effectful` now reports incomplete patterns only in
`Interpreter/Traced.hs`.

### Milestone 3 — Wire the traced interpreter

**Scope.** Add two arms to the traced interpreter in
`pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs`, after the round-robin arms. This is
the one place where you supply something genuinely new rather than copying: the SQL function
name that becomes the `db.operation` attribute.

```haskell
  -- Grouped-head FIFO Read (Consumer spans)
  ReadGroupedHead query@(Types.ReadGrouped qn _ _) ->
    withTracedOp config pool (receiveOp "pgmq.read_grouped_head" qn) $
      Sessions.readGroupedHead query
  ReadGroupedHeadWithPoll query@(Types.ReadGroupedWithPoll qn _ _ _ _) ->
    withTracedOp config pool (receiveOp "pgmq.read_grouped_head_with_poll" qn) $
      Sessions.readGroupedHeadWithPoll query
```

Mind the underscore counts: three fields for `Types.ReadGrouped`, five for
`Types.ReadGroupedWithPoll`. See Context and Orientation for why.

The module's header documentation (lines 26-51) enumerates the span kinds and names the
interpreter emits. Add the two new operations to that list so the documentation does not
quietly go stale — it is the first thing a user reads to find out what tracing they get.

**A note on tracing a long poll.** `readGroupedHeadWithPoll` can legitimately hold its span
open for the full `maxPollSeconds`. In a trace viewer that appears as a long Consumer span
that did nothing, which looks like a stall but is not. Nothing in the code needs to change for
this — the span duration is honest — but mention it in the Haddock so the first person to see
a five-second `receive` span in production does not go hunting for a bug.

**Acceptance.** `cabal build pgmq-effectful` succeeds with no warnings.

### Milestone 4 — Test that the span is actually emitted

**Scope.** Add a test to `pgmq-effectful/test/TracedInterpreterSpec.hs` asserting that
`readGroupedHead`, run under the traced interpreter, emits the span we expect. This is the
first grouped-read test in the package.

The behaviour under test is not "does the read work" — plan 10 already proved that at the
`pgmq-hasql` layer, and re-proving it here would be redundant. What is unproven, and what
this test must establish, is that **the traced interpreter attaches the right telemetry to
this specific operation**. The failure mode being guarded against is a copy-paste slip: an arm
that runs `Sessions.readGroupedHead` but labels the span `"pgmq.read_grouped_rr"`, which would
silently mislabel every trace in production while every functional test still passed.

So the assertion that carries the weight is on `db.operation`.

Model the test on the existing receive test (lines 111-150). Create a unique queue with
`mkUniqueQueue`, send a message carrying an `x-pgmq-group` header, call `readGroupedHead`
under the traced interpreter with `qty = 1`, then assert against the captured span:

- the span name is `"receive " <> queueNameToText queue` (**not** the function name — see
  Context and Orientation);
- the span kind is `Consumer` (`assertSpanKindConsumer`);
- `messaging.system` is `"pgmq"`;
- `messaging.operation` is `"receive"`;
- `messaging.destination.name` is the queue name;
- `db.system` is `"postgresql"`;
- **`db.operation` is `"pgmq.read_grouped_head"`** — the assertion with teeth.

Also assert the read actually returned the message, so the test cannot pass against an
interpreter arm that emits a correct span while running the wrong session or no session at all.

Testing the polling variant's span as well is optional and, on balance, not worth it: it would
add real wall-clock time to the suite to assert one different string, and the arm is
structurally identical. Skip it, and say so here rather than leaving a reader wondering
whether it was an oversight.

**Acceptance.** `cabal test pgmq-effectful` passes. Then prove the test bites: temporarily
change the traced arm's function name string to `"pgmq.read_grouped_rr"`, re-run, and confirm
the `db.operation` assertion fails. Restore it, and record the observed failure in Surprises &
Discoveries.

### Milestone 5 — Format and verify the whole project

**Scope.** No new behaviour. The project uses `treefmt` behind a pre-commit hook; committing
unformatted code makes the hook rewrite the files and fail the commit, forcing you to stage
and commit again. Run `nix fmt` first and save the round trip.

**Acceptance.** `nix fmt` makes no further changes; `cabal build all` and `cabal test all`
are green.


## Concrete Steps

All commands run from `/Users/shinzui/Keikaku/bokuno/libraries/pgmq-hs-project/pgmq-hs`
inside `nix develop`.

**Before starting, confirm the prerequisite:**

```bash
grep -n "readGroupedHead" pgmq-hasql/src/Pgmq/Hasql/Sessions.hs
```

Expected: several lines, including the export entries and the two definitions. If empty,
implement `docs/plans/10-add-grouped-head-read-statements-and-sessions-to-pgmq-hasql.md` first.

**Milestone 1** — edit `pgmq-effectful/src/Pgmq/Effectful/Effect.hs`, then:

```bash
cabal build pgmq-effectful
```

Expect failure. The useful part of the output looks like:

```text
Pgmq/Effectful/Interpreter.hs:93:16: warning: [GHC-62161] [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In a case alternative:
        Patterns of type ‘Pgmq (Eff localEs) a’ not matched:
            ReadGroupedHead _
            ReadGroupedHeadWithPoll _
```

That listing is your to-do list for Milestones 2 and 3.

**Milestone 2** — edit `pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs`, then rebuild. The
same warning should now name only `Interpreter/Traced.hs`.

**Milestone 3** — edit `pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs`, then:

```bash
cabal build pgmq-effectful
```

Expect a clean build with no warnings.

**Milestone 4** — edit `pgmq-effectful/test/TracedInterpreterSpec.hs`, then:

```bash
cabal test pgmq-effectful
```

To iterate on just the new test, `tasty` accepts a pattern filter:

```bash
cabal test pgmq-effectful --test-options='--pattern "GroupedHead"'
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
feat(pgmq-effectful): add grouped-head read effects for pgmq 1.12.0

Add ReadGroupedHead and ReadGroupedHeadWithPoll to the Pgmq effect,
wired through both the plain and traced interpreters. The traced arms
emit Consumer spans via the existing receiveOp helper, so the operations
carry the same messaging attributes as every other pgmq read, with
db.operation distinguishing them.

Both constructors reuse the existing ReadGrouped/ReadGroupedWithPoll
parameter types, as the round-robin constructors already do.

The traced test asserts db.operation is "pgmq.read_grouped_head", which
is what catches a copy-paste slip that would otherwise mislabel every
trace in production while all functional tests still passed.

MasterPlan: docs/masterplans/2-support-pgmq-1-12-0-grouped-head-reads.md
ExecPlan: docs/plans/11-add-grouped-head-read-effects-and-traced-spans-to-pgmq-effectful.md
Intention: intention_01kxgh9geke2dayhx57qp6g9ye
EOF
)"
```


## Validation and Acceptance

The plan is complete when all of the following hold.

**The effect exposes the two operations with these exact signatures**, from
`Pgmq.Effectful.Effect`:

```haskell
readGroupedHead :: (Pgmq :> es) => ReadGrouped -> Eff es (Vector Message)
readGroupedHeadWithPoll :: (Pgmq :> es) => ReadGroupedWithPoll -> Eff es (Vector Message)
```

**Both interpreters handle them.** The compiler enforces this — the `case` in each
interpreter is exhaustive over the GADT, so a clean `cabal build pgmq-effectful` with no
incomplete-pattern warnings *is* the proof. There is nothing further to check by hand.

**The traced interpreter emits a correctly-labelled span.** This is the substantive claim of
the plan and the thing Milestone 4's test proves. Running `readGroupedHead` on queue `q` under
the traced interpreter emits exactly one span with name `receive q`, kind `Consumer`, and
attributes `messaging.system = "pgmq"`, `messaging.operation = "receive"`,
`messaging.destination.name = q`, `db.system = "postgresql"`, and
`db.operation = "pgmq.read_grouped_head"`.

**The test cannot pass for the wrong reason.** Verify rather than assume: change the traced
arm's function-name string to `"pgmq.read_grouped_rr"`, re-run `cabal test pgmq-effectful`,
and confirm the `db.operation` assertion fails. Restore the string. Paste the failure into
Surprises & Discoveries. Without this check you have not established that the span assertion
is doing anything.

**Nothing else regressed.** `cabal build all` and `cabal test all` are green; `nix fmt`
reports no changes.

**The diff is confined to the expected files.** `git diff --stat` should show only
`pgmq-effectful/src/Pgmq/Effectful/Effect.hs`,
`pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs`,
`pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs`, and
`pgmq-effectful/test/TracedInterpreterSpec.hs`. If
`pgmq-effectful/src/Pgmq/Effectful/Telemetry.hs` appears, re-read the Decision Log — every
attribute key you need is already exported from it.


## Idempotence and Recovery

**Every change is additive.** Two new GADT constructors, two new interpreter arms each, one
new test. Nothing existing is modified or removed, so there is no state to migrate and no way
to leave the repository in a half-broken state that a `git checkout -- <file>` will not undo.

**The intermediate state is a broken build, by design.** After Milestone 1 the package does
not compile cleanly. That is not a mistake; it is the exhaustiveness checker handing you the
list of remaining work. Do not try to make it build by deleting the constructors — just carry
on to Milestone 2.

**The tests are safe to run repeatedly.** Each spins up its own throwaway PostgreSQL server
via `ephemeral-pg`; no external or production database is contacted. Queues are created with
unique names via `mkUniqueQueue`, so repeated and concurrent runs do not collide.

**If the build fails with "Variable not in scope: Sessions.readGroupedHead",** the
prerequisite plan is not in place. Confirm with the `grep` in Concrete Steps and implement
`docs/plans/10-add-grouped-head-read-statements-and-sessions-to-pgmq-hasql.md` first.

**If a test fails with "function pgmq.read_grouped_head does not exist",** the SQL migration
is missing or stale — that is plan 9's territory. The migration SQL is embedded into the
`pgmq-migration` library at compile time and can go stale in an incremental build; force a
rebuild:

```bash
cabal build pgmq-migration --ghc-options=-fforce-recomp
cabal test pgmq-effectful
```

**If the traced test sees no spans at all,** that is a test-harness problem rather than a
problem with your interpreter arm. The spec captures spans in memory via `setupTracer` and
some assertions depend on `withSemconvOptIn`, which sets the `OTEL_SEMCONV_STABILITY_OPT_IN`
environment variable that the traced interpreter reads to decide which generation of attribute
names to emit. Compare your test against the existing receive test (lines 111-150) and make
sure you wrapped it the same way.


## Interfaces and Dependencies

**No new package dependencies.** Everything is already a dependency of `pgmq-effectful` or its
test suite: `effectful-core` supplies `Eff`, `Effect`, `(:>)`, `send`, and `interpret`;
`pgmq-hasql` supplies the sessions; `hs-opentelemetry-api` supplies the span and attribute
machinery; `vector` supplies `Vector`; `tasty` / `tasty-hunit` supply the test framework;
`ephemeral-pg` supplies the throwaway database.

**Modules you will change:**

- `pgmq-effectful/src/Pgmq/Effectful/Effect.hs` — two GADT constructors, two smart
  constructors, two export entries.
- `pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs` — two `case` arms.
- `pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs` — two `case` arms, plus the module
  header's list of emitted spans.
- `pgmq-effectful/test/TracedInterpreterSpec.hs` — one test, registered in `tests`.

**Modules you must NOT change, and why:** `pgmq-effectful/src/Pgmq/Effectful/Telemetry.hs`
already exports every attribute key the new spans use (`messaging_system`,
`messaging_operation`, `messaging_destination_name`, `db_system`, `db_operation`, and their
stable-convention counterparts). `pgmq-effectful/src/Pgmq/Effectful/Traced.hs` is about trace
context propagation through message headers, which is unrelated.
`pgmq-effectful/src/Pgmq/Effectful.hs` (the umbrella) is intentionally deferred to
`docs/plans/12-expose-grouped-reads-on-the-umbrella-api-and-release-0-5-0-0.md`.

**What this plan consumes** — supplied by
`docs/plans/10-add-grouped-head-read-statements-and-sessions-to-pgmq-hasql.md`, and this
package will not compile without it:

```haskell
Pgmq.Hasql.Sessions.readGroupedHead :: ReadGrouped -> Session (Vector Message)
Pgmq.Hasql.Sessions.readGroupedHeadWithPoll :: ReadGroupedWithPoll -> Session (Vector Message)
```

**What this plan hands to the next one.**
`docs/plans/12-expose-grouped-reads-on-the-umbrella-api-and-release-0-5-0-0.md` re-exports
these from `Pgmq.Effectful`:

```haskell
Pgmq.Effectful.Effect.readGroupedHead :: (Pgmq :> es) => ReadGrouped -> Eff es (Vector Message)
Pgmq.Effectful.Effect.readGroupedHeadWithPoll :: (Pgmq :> es) => ReadGroupedWithPoll -> Eff es (Vector Message)
```

The names and types above are a contract with plan 12. If you rename them, update that plan
before you commit.
