---
id: 13
slug: fix-null-parameter-semantics-across-pop-read-and-notify-statements
title: "Fix NULL parameter semantics across pop read and notify statements"
kind: exec-plan
created_at: 2026-07-23T23:12:20Z
intention: intention_01kz9yszpmejztjbet6k4bvcf7
master_plan: "docs/masterplans/3-harden-the-pgmq-hs-family-surfaced-by-the-2026-07-review.md"
---

# Fix NULL parameter semantics across pop read and notify statements

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.
If durable project context changes, update or create a note under `docs/design/` in the same
change.


## Purpose / Big Picture

The pgmq-hs family (a Haskell client stack for PGMQ, the PostgreSQL Message Queue) exposes
several optional parameters as `Maybe` fields whose documentation promises a sane default
when the caller passes `Nothing`. Today every one of those promises is false, and two of
them are dangerous. Live-verified on PostgreSQL 18.4 running the repo's own migration:
calling `pop` with `qty = Nothing` — documented "Nothing = default 1" — deleted and
returned **all five** messages in the test queue in one statement, while an omitted-argument
control call popped exactly one; calling `read` with `batchSize = Nothing` returned all five
and left every one of them invisible (leased). Calling `enableNotifyInsert` with
`throttleIntervalMs = Nothing` — documented "Nothing = default 250ms" — deterministically
fails with a NOT NULL violation (SQLSTATE 23502), which makes a
`withNotifyInsert Nothing` queue configuration fail application reconciliation on **every**
startup. Separately, the `conditional` filter field on `ReadMessage` is silently dead (never
encoded, never sent), and the two single-message visibility-timeout statements throw an
infrastructure-looking decode error when the target row was raced away instead of reporting
"not found".

After this plan, `Nothing` means exactly what the documentation says for every one of these
parameters: `pop` pops one message, `read`/`readWithPoll` read one, `enableNotifyInsert`
installs the documented 250 ms throttle, `ReadMessage.conditional` actually filters, and
`changeVisibilityTimeout`/`setVisibilityTimeoutAt` return `Maybe Message` (`Nothing` for a
missing row) instead of throwing. You can see it working by running the new
`NullSemanticsSpec` test module: every `Nothing` case is pinned by a test that fails against
the current code and passes after the fix.

All fixes are currently latent for registered consumers (keiro-pgmq never pops and always
passes `Just` batch sizes; shibuya-pgmq-adapter pops only in benchmarks with `Just`), but
the parameters are armed traps for the incoming service fleet that will call these APIs
directly.


## Progress

- [x] M1 (2026-08-05): `NullSemanticsSpec` (pgmq-hasql) and the `withNotifyInsert Nothing`
      reconcile test (pgmq-config) written and confirmed **red** against unfixed code,
      reproducing PGH-1 (pop drains queue), PGH-3 (read/readWithPoll lease whole queue),
      PGH-2 (conditional silently ignored), PGH-4 (23502 on enable), and PGH-5 (throw on
      raced-away set_vt row, asserted in its M1 `assertSessionFails` form). Failure
      transcripts captured in Surprises & Discoveries.
- [x] M2 (2026-08-05): Statement-text COALESCE fixes for `pop`, `read`, `read_with_poll`,
      `enable_notify_insert`; `ReadMessage.conditional` wired as a real fourth parameter;
      `changeVisibilityTimeout`/`setVisibilityTimeoutAt` return `Maybe Message` through the
      hasql, session, and effectful layers; the three existing pgmq-hasql tests that consume
      those results updated to assert and unwrap `Just`; all false doc comments corrected;
      superseded design note 010 corrected in the same change. M1 and pre-existing tests
      green (`pgmq-hasql-test` 61/61, `pgmq-config-test` 11/11, `pgmq-effectful-test`
      17/17).
- [x] M3 (2026-08-05): Full family suite green (`cabal test all`: 98 tests across
      pgmq-hasql-test 61, pgmq-effectful-test 17, pgmq-config-test 11, pgmq-migration-test
      9) and `just nix-test` green (hermetic pgmq-hasql-tests and pgmq-migration-tests);
      CHANGELOG "Unreleased (0.5.0.0)" entry written; release-train coordination recorded
      (this plan claims no migration file; family version impact recorded as
      0.5.0.0-breaking); consumer-impact notes for keiro-pgmq (compiles unchanged) and
      shibuya-pgmq-adapter (one-line `leaseExtend` fix needed at bound-bump time) recorded
      for the release-owning plan 12.
- [x] Design distillation pass (2026-08-05): NULL-parameter contract ("no optional
      parameter may widen scope") recorded as
      `docs/design/014-null-parameter-contract.md`, together with the companion
      "absence is not failure" decoder rule; `docs/design/010-read-conditional-null-handling.md`
      marked Superseded with a Correction section.


## Surprises & Discoveries

Seeded from the 2026-07 pgmq-hs review verification (2026-07-23, PostgreSQL 18.4, repo's
own migration):

- `pop('q', NULL)` returned all 5 seeded messages and left 0; the omitted-argument control
  `pop('q')` popped exactly 1. Mechanism: SQL `NULL` fed to `LIMIT` is `LIMIT ALL`.
- `read` with NULL qty returned 5 and left all 5 invisible (`vt` bumped on every row).
- The justification comment in `Message.hs` lines 166-167 ("4-param read fails with NULL
  conditional") is **refuted**: the SQL `CASE` at migration lines 267-270 handles both NULL
  and `'{}'` conditionals (returned all rows) and filters correctly with a real filter.
  `readWithPoll` already binds `conditional` as `$6` and works.
- `enable_notify_insert('q', NULL)`: always fails, exact SQLSTATE 23502 raised at function
  line 19 (the INSERT of an explicit NULL into the NOT NULL DEFAULT 0 column — a column
  DEFAULT does not apply to an explicit NULL). The "destroys the existing trigger" half of
  the original finding is **refuted**: a plpgsql function call is one statement, so the
  error rolls back the function's internal trigger drop atomically; trigger and throttle
  row were verified intact after the failure.

M1 reproduction on this machine (2026-08-05, PostgreSQL 17.10 via `ephemeral-pg`, GHC
9.12.4). Every predicted failure shape reproduced. `cabal test pgmq-hasql-test
--test-show-details=direct`:

```text
  NULL Parameter Semantics
    pop with qty = Nothing pops exactly one message:                 FAIL (0.02s)
      test/NullSemanticsSpec.hs:78:
      Should pop exactly 1 message
      expected: 1
       but got: 5
    read with batchSize = Nothing reads exactly one message:         FAIL (0.01s)
      Should read exactly 1 message
      expected: 1
       but got: 5
    readWithPoll with batchSize = Nothing reads exactly one message: FAIL (0.01s)
      Should read exactly 1 message
      expected: 1
       but got: 5
    conditional filters when Just and is neutral when Nothing:       FAIL (0.01s)
      Filtered read should return only the matching message
      expected: 1
       but got: 2
    enableNotifyInsert with Nothing applies the 250ms default:       FAIL (0.02s)
      Session failed: SessionUsageError (StatementSessionError 1 0
      "select from pgmq.enable_notify_insert($1, $2)" ["\"test_queue_52949\"","null"] True
      (ServerStatementError (ServerError "23502" "null value in column
      \"throttle_interval_ms\" of relation \"notify_insert_throttle\" violates not-null
      constraint" ...)))
    set_vt on a raced-away row does not throw:                       OK (0.02s)

5 out of 61 tests failed (1.27s)
```

`cabal test pgmq-config-test --test-show-details=direct`:

```text
    enables notify insert with the default throttle:   FAIL (0.02s)
      Session failed: SessionUsageError (StatementSessionError 1 0
      "select from pgmq.enable_notify_insert($1, $2)" ["\"cfg_test_43472\"","null"] True
      (ServerStatementError (ServerError "23502" "null value in column
      \"throttle_interval_ms\" of relation \"notify_insert_throttle\" violates not-null
      constraint" ...)))

1 out of 11 tests failed (0.04s)
```

Notes from M1:

- The 23502 error message text confirms the mechanism precisely: `Failing row contains
  (cfg_test_43472, null, 1969-12-31 16:00:00-08)`. The explicit NULL reached the row; the
  column DEFAULT of 0 was never consulted. The bound-parameter list in the error
  (`["\"cfg_test_43472\"","null"]`) shows hasql sending SQL NULL for `$2`.
- The `set_vt` raced-away case passes in its M1 form (`assertSessionFails`), which is the
  documented before-evidence: the session *fails* today where it should report absence.
  M2 rewrites those two assertions to expect `Nothing`.
- The PGH-2 repro is sharper than a bare count: `conditional = Just {"kind":"a"}` returned
  **both** messages, proving the filter is not merely mis-encoded but never sent at all.
- Reproduced on PostgreSQL 17.10 (the nix dev shell's server), not the 18.4 the original
  review used. Both `LIMIT NULL` and the plpgsql-DEFAULT semantics are long-standing
  PostgreSQL behaviour, so the findings are not version-specific.

M2 (2026-08-05):

- The plan did not anticipate `docs/design/010-read-conditional-null-handling.md`, an
  existing design note that records the decision this plan reverses — and records it with
  a root cause that does not hold against the vendored SQL. Its "Option B: COALESCE in SQL
  (not viable)" section argues that `coalesce($4, '{}')` cannot work because `'{}'` would
  match every message. That is exactly backwards: the `CASE` in `pgmq.read` special-cases
  `'{}'` to mean "no filter" (`WHEN %L != '{}'::jsonb THEN (message @> %2$L)::integer ELSE
  1`), which is why the coalesce is correct. The note also claims two overloaded
  `pgmq.read` functions exist; the vendored install SQL defines exactly one, with
  `conditional JSONB DEFAULT '{}'`. Note 010 is now marked Superseded with a Correction
  section, since leaving it would have led the next contributor straight back into the
  dead-field design.
- All three consuming layers compiled with no changes beyond the type signatures: both the
  plain and the traced effectful interpreters pass the session result through
  polymorphically (`withTracedOp ... $ Sessions.changeVisibilityTimeout query`), so no
  span name, kind, or attribute changed — the ADR 0001 telemetry contract is untouched by
  construction rather than by inspection.
- `docs/OPENTELEMETRY_INSTRUMENTATION.md` contains an illustrative interpreter sketch that
  calls `Sessions.changeVisibilityTimeout`; it is result-type-polymorphic prose and needed
  no edit.

M3 (2026-08-05):

- Migration ledger state at landing time: `pgmq-migration/migrations/manifest` contains
  exactly `0001-install-v1.11.0.sql` and `0002-schema-management-comment.sql` — the
  baseline. No sibling plan had claimed a number, so plan 14 takes `0003` unless MasterPlan
  2 plan 9 or keiro MasterPlan 17 plans 116/118 land first. This plan added no migration,
  as designed.
- `cabal test all` runs four suites, not five: there is no `pgmq-core-test` in this
  repository. `pgmq-core`'s types are exercised through the other packages' suites. The
  MasterPlan's Integration Points section names `pgmq-core-test` as the reconciliation
  check for the `Pgmq.Types` file shared by plans 14 and 15; whoever lands second there
  should use `cabal test all` instead.

(Add new discoveries below as work proceeds.)


## Decision Log

- Decision: Fix `pop`, `read`, `read_with_poll`, and `enable_notify_insert` NULL semantics
  in the **client statement text** with `COALESCE($n, default)`, keeping the Haskell
  `Maybe` fields, rather than re-creating the SQL functions server-side or making the
  fields non-`Maybe`.
  Rationale: The SQL functions `pgmq.pop`/`pgmq.read`/`pgmq.read_with_poll` are
  upstream-parity code (the review certified byte-identical SQL parity with the upstream
  pgmq extension); diverging them would complicate the audited upstream upgrade owned by
  `docs/plans/9-vendor-pgmq-1-12-0-and-add-the-native-schema-migration.md`. A non-`Maybe`
  `batchSize`/`qty` field would be a further API break with
  no additional safety once the COALESCE exists, and the `Maybe` API with a now-true
  "Nothing = 1" doc is the smallest honest contract. The one non-upstream function in this
  family, `pgmq.enable_notify_insert`, additionally gets a server-side
  `COALESCE(throttle_interval_ms, 250)` guard — but that ships in plan 14's migration,
  which must re-create that function anyway for its advisory lock (see next entry).
  Date: 2026-07-23

- Decision: This plan ships **no migration file**. The server-side NULL guard for
  `pgmq.enable_notify_insert` is delegated to plan 14
  (`docs/plans/14-make-insert-notifications-survive-crashes-and-document-the-channel-contract.md`),
  whose migration re-creates that exact function for the PGH-8 advisory lock; the new
  definition there includes `COALESCE(throttle_interval_ms, 250)`. This implements the
  master plan's "combine the function's new definition in one migration" option and avoids
  two migrations rewriting the same function in the same release train.
  Rationale: One function, one new definition, one migration. The client-side
  `coalesce($2, 250)` in this plan already fixes every Haskell caller (including the
  pgmq-config reconciler) independently of migration landing order.
  Date: 2026-07-23

- Decision: Wire `ReadMessage.conditional` as a real fourth statement parameter
  (`coalesce($4, '{}'::jsonb)`) instead of deleting the field.
  Rationale: `readWithPoll` already binds `conditional` as `$6` against the same SQL
  `CASE` construct and works, proving the SQL side; the live repro refuted the comment
  that justified the 3-argument form. Deleting the field would be a gratuitous API break
  that removes a working upstream feature.
  Date: 2026-07-23

- Decision: `changeVisibilityTimeout` and `setVisibilityTimeoutAt` change their result
  type from `Message` to `Maybe Message` (`D.rowMaybe` instead of `D.singleRow`),
  propagated through `Sessions` and the `Pgmq` effect. This is a PVP-breaking change, so
  the family release carrying this plan is a **major** bump (0.4.0.1 -> 0.5.0.0), not the
  0.4.x minor bumps that MasterPlan 17's plans 116/118 use for their SQL-only changes.
  The alternative — leaving the throwing variants exported and adding new `...Maybe`
  functions to stay in 0.4.x — was rejected: the throwing variants are precisely the
  hazard (a raced-away row is indistinguishable from infrastructure failure), and keeping
  them exported preserves the trap.
  Rationale: `pgmq.set_vt` is `RETURNS SETOF` and returns 0 rows for an absent `msg_id`;
  `D.singleRow` then produces hasql's `UnexpectedRowCountStatementError` (expected 1, got
  0) wrapped in `StatementSessionError` — the same shape as real infrastructure errors.
  A raced-away message during lease extension is an ordinary, expected event.
  Date: 2026-07-23

- Decision: This plan claims no migration number and never reserves one for a sibling.
  Plan 14 takes the next free manifest number when it lands, after any entries produced by
  MasterPlan 2 plan 9 and keiro MasterPlan 17 plans 116/118. The single version bump,
  changelog consolidation, and consumer rollout are owned by
  `docs/plans/12-expose-grouped-reads-on-the-umbrella-api-and-release-0-5-0-0.md`.
  Rationale: Numeric reservations go stale when independent plans land in a different order,
  and the official repository already has one release owner.
  Date: 2026-07-23

- Decision: Update every existing visibility-timeout test that dereferences the returned
  `Message`, not only the new `NullSemanticsSpec`.
  Rationale: After the public result changes to `Maybe Message`,
  `pgmq-hasql/test/AllFunctionsDecoderSpec.hs`,
  `pgmq-hasql/test/AdvancedOpsSpec.hs`, and `pgmq-hasql/test/MessageSpec.hs` otherwise fail
  to compile before the new behavior can be tested.
  Date: 2026-07-23

- Decision: Mark `docs/design/010-read-conditional-null-handling.md` Superseded and append
  a Correction section to it, rather than deleting it or silently leaving it in place.
  Rationale: The note records a decision this plan reverses, and its stated root cause and
  its "COALESCE is not viable" alternative are both refuted by the vendored SQL and by the
  M1 live reproduction. Deleting it would erase the history of why the field was dead for
  a release; leaving it unmarked would send the next contributor back into the same dead
  end. The Correction states the actual `CASE` predicate so the reasoning can be checked
  against the file it describes.
  Date: 2026-08-05

- Decision: Assert the raced-away `set_vt` result with `assertEqual ... Nothing (fmap
  messageId result)` rather than pattern-matching on the `Maybe Message`.
  Rationale: `Message` has no `Eq`-friendly failure output worth printing, and comparing
  the mapped `MessageId` gives a readable diff if the assertion ever regresses to `Just`.
  Date: 2026-08-05

(Record further decisions as they are made, with dates.)


## Outcomes & Retrospective

Completed 2026-08-05. All five findings (PGH-1 through PGH-5) are fixed and pinned by
tests that were confirmed red before the fix and green after.

What exists now that did not before. `Nothing` means the documented default for every
optional parameter in this family: `pop` pops one message instead of deleting the queue,
`readMessage` and `readWithPoll` read one message instead of leasing the queue, and
`enableNotifyInsert` installs a 250 ms throttle instead of failing with SQLSTATE 23502 on
every startup forever. `ReadMessage.conditional` filters instead of being silently
discarded. `changeVisibilityTimeout` and `setVisibilityTimeoutAt` return `Maybe Message`,
so a message another worker already handled is reported as absence rather than as an error
shaped like infrastructure failure. `pgmq-hasql/test/NullSemanticsSpec.hs` and
`testEnsureQueuesWithNotifyDefault` in `pgmq-config/test/ConfigSpec.hs` hold all of that in
place, and `docs/design/014-null-parameter-contract.md` states the rule for the next
statement someone adds.

What remains, all of it owned elsewhere. No version was bumped and nothing shipped: the
five `.cabal` files still read 0.4.0.1 and the CHANGELOG entry sits under "Unreleased
(0.5.0.0)". `docs/plans/12-expose-grouped-reads-on-the-umbrella-api-and-release-0-5-0-0.md`
owns the single coordinated bump, the changelog consolidation, the consumer-bound rollout,
and the one-line shibuya-pgmq-adapter change the `Maybe Message` result requires
(`Internal.hs` lines 198-206 assign `updated.visibilityTime` to an `IORef` and must now
traverse the `Maybe`). keiro-pgmq needs no source change: its three
`changeVisibilityTimeout` calls are all under `void $`. The server-side
`COALESCE(throttle_interval_ms, 250)` guard for non-Haskell callers belongs to plan 14's
migration, which re-creates `pgmq.enable_notify_insert` anyway.

Lessons. First, the most valuable artifact of milestone 1 was not the red tests but the
error text they printed: `Failing row contains (cfg_test_43472, null, ...)` settled the
mechanism argument instantly, and the bound-parameter list hasql includes in its error
(`["\"cfg_test_43472\"","null"]`) showed the NULL leaving the client. Writing the tests
before reading more SQL would have been faster than the reverse.

Second, a wrong design note is worse than no design note. `docs/design/010` had recorded,
as settled, both a root cause that does not match the vendored SQL and an explicit
argument that the correct fix was "not viable". That note is the reason the field stayed
dead through a release. Correcting it was not bookkeeping; it was the part of this work
most likely to prevent a recurrence.

Third, the plan's instinct to fix this client-side was right for a reason worth repeating:
the client-side coalesce fixes every Haskell caller the moment the code ships, with no
dependency on migration ordering, and leaves the vendored upstream SQL byte-identical for
the audited upgrade. Server-side guards are for callers this repository does not compile.


## Context and Orientation

This file is in the pgmq-hs repository. Ignore its `dist-newstyle/` build directory. It is a
Cabal multi-package project; all commands in this plan run from the repository root unless
stated otherwise. The packages are currently at version 0.4.0.1:

- `pgmq-core` — pure types (`Pgmq.Types`: `QueueName`, `Message`, `MessageBody`, ...).
- `pgmq-hasql` — hasql statements, encoders, decoders, and `Session` wrappers.
  ("hasql" is the PostgreSQL client library used; a `Statement` pairs a SQL string with a
  parameter encoder and a result decoder; a `Session` is a sequence of statements run on
  one connection.)
- `pgmq-effectful` — an `effectful` effect (`Pgmq`) plus a plain interpreter
  (`runPgmq`) and a traced interpreter that adds OpenTelemetry spans.
- `pgmq-config` — declarative queue configuration and a startup reconciler
  (`ensureQueues`) that creates missing queues/bindings/notify throttles.
- `pgmq-migration` — the embedded SQL migration ledger. The install SQL is
  `pgmq-migration/migrations/0001-install-v1.11.0.sql` (2,075 lines); changed SQL ships
  as *new* numbered migration files listed in `pgmq-migration/migrations/manifest`, never
  as edits to an applied file. This plan does not add one (see Decision Log).

Relevant external constraint: keiro's
`docs/adr/0001-keiro-pgmq-job-processing-telemetry-contract.md` pins the traced
pgmq-effectful interpreter's span semantics
(span names, kinds, attributes for publish/receive/delete/visibility operations). Nothing
in this plan may change what the traced interpreter emits; the changes here are statement
text, decoders, and result types, which flow through both interpreters identically. Durable
decisions for this repository belong under `docs/design/`.

Three PostgreSQL facts this plan rests on (all live-verified; embed them in your head
before touching anything):

1. **plpgsql parameter DEFAULTs apply only to omitted arguments, never to SQL NULL.**
   `pgmq.pop(queue_name TEXT, qty INTEGER DEFAULT 1)` called as `pop('q')` uses 1; called
   as `pop('q', NULL)` binds `qty = NULL`. A Haskell encoder that always binds the
   parameter (`E.nullable`) can never trigger the DEFAULT.
2. **`LIMIT NULL` means `LIMIT ALL`.** A NULL flowing into a `LIMIT` clause removes the
   bound entirely.
3. **A plpgsql function call is one statement; an unhandled error rolls back ALL its
   effects atomically.** This is why the 23502 in `enable_notify_insert` does *not*
   destroy the existing trigger (the internal drop rolls back), and why `pop`'s
   read-then-delete is atomic.

The five findings this plan fixes, with exact locations (verify each before editing —
if a line number has drifted, find the construct by name and update this plan):

**PGH-1 (CRITICAL) — `pop` with `Nothing` drains the queue.** SQL: `pgmq.pop` at
migration lines 872-898; the CTE selects `LIMIT $1 FOR UPDATE SKIP LOCKED` (line 887)
feeding a `DELETE`, with no COALESCE. Haskell: the statement always binds `$2`
(`pgmq-hasql/src/Pgmq/Hasql/Statements/Message.hs` lines 253-257,
`sql = "select * from pgmq.pop($1,$2)"`), the encoder is nullable
(`pgmq-hasql/src/Pgmq/Hasql/Encoders.hs` lines 151-154, `E.nullable E.int4`), and no
`fromMaybe` exists anywhere on the path — `Sessions.hs` lines 200-201 and
`pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs` line 125 pass the record straight
through. The doc claim "Nothing = default 1" at
`pgmq-hasql/src/Pgmq/Hasql/Statements/Types.hs` line 176 is false. There is no
visibility-timeout safety net: pop deletes.

**PGH-2 (HIGH) — `ReadMessage.conditional` is a silently-dead API field.** The field
exists (`Statements/Types.hs` lines 154-161) but `readMessageEncoder` binds only three
parameters (`Encoders.hs` lines 144-148) and the statement is the 3-argument read
(`Message.hs` lines 169-173). The comment at `Message.hs` lines 164-168 directs users to
a function `readMessageConditional` that does not exist anywhere (grep: the only
occurrence is that comment), and its justification (lines 166-167) was refuted live: the
SQL `CASE` at migration lines 267-270 (`WHEN %L != '{}'::jsonb THEN (message @> %2$L) ...
ELSE 1`) treats a NULL conditional as "no filter" and filters correctly with a real one.
`readWithPoll` already binds `conditional` as `$6` (`Encoders.hs` line 208, `Message.hs`
lines 244-248). All current callers pass `Nothing` (keiro-pgmq: `Dlq.hs` lines 121, 168,
223 and `Job.hs` line 921 all set `conditional = Nothing`), so wiring it changes no
observable behavior for them.

**PGH-3 (HIGH) — `read`/`readWithPoll` with `batchSize = Nothing` lease the whole
queue.** Same `LIMIT NULL` mechanism on `pgmq.read` (migration lines 250-288; `LIMIT $1`
at line 272; the UPDATE bumps `vt` and `read_ct` at lines 275-282, so every returned row
is leased). Encoder: `Encoders.hs` line 148 (`E.nullable`). `readWithPoll` is identical
(`Encoders.hs` line 205; SQL `LIMIT $1` at migration line 458). Registered consumers all
validate and pass `Just`.

**PGH-4 (partially refuted, remainder confirmed) — `enableNotifyInsert Nothing` always
fails.** `pgmq.enable_notify_insert` (migration lines 1594-1629): the guard
`IF v_throttle_interval_ms < 0` (line 1602) is skipped for NULL (NULL < 0 is NULL, not
true); `PERFORM pgmq.disable_notify_insert` (line 1611); then the INSERT (lines
1613-1617) writes the explicit NULL into `throttle_interval_ms INTEGER NOT NULL DEFAULT
0` (table definition line 30 of the UNLOGGED table at lines 25-32) — 23502, always. The
encoder is nullable (`Encoders.hs` lines 196-199); the statement is
`pgmq-hasql/src/Pgmq/Hasql/Statements/QueueManagement.hs` lines 60-63. pgmq-config
passes `throttleMs` straight through (`pgmq-config/src/Pgmq/Config.hs` lines 109-120;
`pgmq-config/src/Pgmq/Config/Effectful.hs` lines 83-94) and its doc at
`pgmq-config/src/Pgmq/Config/Types.hs` line 59 ("Nothing uses pgmq default (250ms)") is
false today. Because `ensureQueues` is a plain `Session` (each statement autocommits;
it is NOT one transaction), the queue creation commits before the enable fails — so a
`withNotifyInsert Nothing` config fails reconcile on **every** startup, forever. No test
exercises `Nothing` (pgmq-config's `test/ConfigSpec.hs` line 126 uses `Just 500`;
pgmq-hasql's `test/TopicSpec.hs` line 336 uses `Just 100`).

**PGH-5 (MEDIUM) — single-row `set_vt` decode throws on a raced-away row.**
`setVisibilityTimeoutAt` decodes `D.singleRow` (`Message.hs` lines 229-233) over
`pgmq.set_vt` which is `RETURNS SETOF` (migration lines 901-949) and returns 0 rows for
an absent `msg_id`. hasql then raises `UnexpectedRowCountStatementError 1 1 0` (that is
the constructor name in the pinned hasql 1.10.3.5; older hasql called this
"UnexpectedAmountOfRows") inside `StatementSessionError` — indistinguishable in kind
from real infrastructure failure. `changeVisibilityTimeout` (lines 213-217) has the
identical defect; fix BOTH. The batch variants (`D.rowVector`) are fine (0 rows decode
to an empty vector).

Consumer facts relevant here (verified 2026-07-23): keiro-pgmq calls
`Pgmq.changeVisibilityTimeout` at `keiro-pgmq/src/Keiro/PGMQ/Job.hs` lines 994, 1018,
1042 — always under `void $`, so the `Maybe Message` result type change compiles there
unchanged and improves semantics (a benign no-op instead of a throw when the message was
already deleted). shibuya-pgmq-adapter *uses* the `setVisibilityTimeoutAt` result
(`shibuya-pgmq-adapter/src/Shibuya/Adapter/Pgmq/Internal.hs` lines 198-206:
`updated <- ...; writeIORef lastVtRef updated.visibilityTime`), so it needs a one-line
change (`traverse_`/`for_` over the `Maybe`) when its bound is bumped. The release-owning
plan 12 carries that change and the complete consumer-bound rollout.

How the tests provision PostgreSQL: every pgmq-hs test suite self-provisions a temporary
server via the `ephemeral-pg` library (`test/EphemeralDb.hs` in each package wraps
`EphemeralPg.withCached`, then runs the full migration ledger through `pg-migrate`
before handing the suite a `hasql-pool` `Pool`). No external database or environment
variables are needed; the `postgres`/`initdb` binaries must be on PATH, which the nix
dev shell provides (`nix develop`, or the project's direnv). Test suites: `pgmq-hasql-test`
(tasty; entry `pgmq-hasql/test/Main.hs`), `pgmq-config-test`, `pgmq-effectful-test`,
`pgmq-migration-test`.


## Plan of Work

### Milestone 1 — reproduce every finding with red tests

Scope: write the tests first, run them against the unfixed code, and record the failures.
This proves the findings on your machine, pins the exact broken behaviors, and gives M2 an
unambiguous finish line. Nothing in `src/` changes in this milestone.

Create `pgmq-hasql/test/NullSemanticsSpec.hs` exporting `tests :: Pool.Pool -> TestTree`,
modeled on `pgmq-hasql/test/AdvancedOpsSpec.hs` (use `TestUtils.assertSession`,
`TestUtils.runSession`, `EphemeralDb.withTestFixture` for an isolated random queue per
test case; `cleanupQueue` in a finally-position as the existing specs do). The module
carries one scope-widening guard per readable statement plus the notify and set_vt cases.
In prose, the cases are:

1. *pop does not widen scope on Nothing*: create a queue, send 5 messages, run
   `Sessions.pop PopMessage {queueName, qty = Nothing}`, assert exactly 1 message is
   returned, and assert `queueMetrics` (or a follow-up `pop` with `Just 10`) shows 4
   remaining.
2. *read does not widen scope on Nothing*: seed 5, run `Sessions.readMessage ReadMessage
   {queueName, delay = 30, batchSize = Nothing, conditional = Nothing}`, assert exactly 1
   returned; a second immediate read with `batchSize = Just 10` must return 4 (only one
   row was leased).
3. *readWithPoll does not widen scope on Nothing*: same shape via `Sessions.readWithPoll`
   with `maxPollSeconds = 1`, `pollIntervalMs = 100`.
4. *conditional filters when Just and is neutral when Nothing*: seed messages
   `{"kind":"a"}` and `{"kind":"b"}`; read with
   `conditional = Just (object ["kind" .= ("a" :: Text)])` and assert only the matching
   message returns; read with `conditional = Nothing` and assert both return (batchSize
   `Just 10`). (Red today in a specific way: the `Just` case is silently ignored — the
   unfixed code returns *both* messages, which is the dead-field bug.)
5. *enableNotifyInsert Nothing applies the documented 250 ms default*: create a queue,
   run `Sessions.enableNotifyInsert EnableNotifyInsert {queueName, throttleIntervalMs =
   Nothing}`, assert success, then `Sessions.listNotifyInsertThrottles` contains the
   queue with `throttleIntervalMs = 250`. (Red today: the session fails with SQLSTATE
   23502.)
6. *set_vt on a raced-away row is Nothing, not a throw*: create a queue, call
   `Sessions.changeVisibilityTimeout` and `Sessions.setVisibilityTimeoutAt` for
   `MessageId 999999` (never sent), assert the session **succeeds** with `Nothing`; then
   send a message and assert both return `Just` with the expected `visibilityTime`
   ordering. (Red today: the missing-row case fails the session with an
   `UnexpectedRowCountStatementError`-shaped `StatementSessionError`. Note: in M1 this
   test will not even compile against the current `Session Message` type — write it
   against the M2 target signatures and leave it commented-in only after the M2 type
   change, or write the M1 version as `assertSessionFails` and tighten it in M2. Prefer
   the second: M1 asserts the current throwing behavior explicitly, M2 rewrites the
   assertion to `Nothing`.)

Register the module in `pgmq-hasql/pgmq-hasql.cabal` (`other-modules` of test-suite
`pgmq-hasql-test`, alphabetical position) and in `pgmq-hasql/test/Main.hs`'s `testGroup`
list.

Add one test to `pgmq-config/test/ConfigSpec.hs` next to `testEnsureQueuesWithNotify`
(line 123): `testEnsureQueuesWithNotifyDefault` builds
`[withNotifyInsert Nothing (standardQueue qn)]`, runs `ensureQueuesReport` twice, and
asserts the first run yields `EnabledNotify qn Nothing` and the second `SkippedNotify qn`
(reconcile is idempotent and never fails). (Red today: the first `ensureQueuesReport`
session fails with 23502 — and, because queue creation autocommitted, so does every rerun,
which is exactly the "fails reconcile on EVERY startup" production symptom.)

Acceptance: `cabal test pgmq-hasql-test pgmq-config-test` shows the new cases failing
with the predicted shapes (pop/read counts of 5-not-1, the 23502, the row-count decode
error) and every pre-existing case still passing. Paste the failure excerpts into
Surprises & Discoveries.

### Milestone 2 — fix the statements, wire the field, correct the docs

Scope: the actual fixes, smallest-possible diffs, all in `pgmq-hasql`, `pgmq-effectful`,
and doc lines in `pgmq-config`. At the end, all M1 tests are green.

In `pgmq-hasql/src/Pgmq/Hasql/Statements/Message.hs`:

- `pop` (lines 253-257): change the SQL to
  `"select * from pgmq.pop($1,coalesce($2,1))"`. Extend the comment: the COALESCE is what
  makes "Nothing = 1" true, because a bound SQL NULL never triggers a plpgsql DEFAULT and
  `LIMIT NULL` is `LIMIT ALL`.
- `readMessage` (lines 169-173): change the SQL to
  `"select * from pgmq.read($1,$2,coalesce($3,1),coalesce($4,'{}'::jsonb))"`. Delete the
  false comment block (lines 164-168: the "4-param version fails with NULL conditional"
  claim and the pointer to the nonexistent `readMessageConditional`) and replace it with
  a truthful one: conditional is a JSONB containment filter (`message @> conditional`);
  `Nothing` (or `'{}'`) means no filtering; the COALESCEs default the batch size to 1 and
  neutralize a NULL conditional.
- `readWithPoll` (lines 244-248): change the SQL to
  `"select * from pgmq.read_with_poll($1,$2,coalesce($3,1),$4,$5,coalesce($6,'{}'::jsonb))"`.
- `changeVisibilityTimeout` (lines 213-217) and `setVisibilityTimeoutAt` (lines 229-233):
  change the result type to `Maybe Message` and the decoder to
  `D.rowMaybe messageDecoder`; haddock: "Returns Nothing when the message no longer
  exists (already deleted, archived, or popped)." Leave `batchChangeVisibilityTimeout`
  and `batchSetVisibilityTimeoutAt` untouched.

Update the pre-existing tests at the same time; this is required compilation work, not an
optional cleanup:

- `pgmq-hasql/test/AllFunctionsDecoderSpec.hs` obtains the
  `changeVisibilityTimeout` result near line 115 and immediately reads its fields. Assert
  `Just msg` before those field assertions.
- `pgmq-hasql/test/AdvancedOpsSpec.hs` obtains the `setVisibilityTimeoutAt` result near
  line 183. Assert `Just updated` before checking its `messageId`.
- `pgmq-hasql/test/MessageSpec.hs` obtains the `changeVisibilityTimeout` result near line
  246. Assert `Just msg` before checking its `messageId`.

Keep these positive cases: they prove the new result is `Just` for an existing row, while
`NullSemanticsSpec` separately proves `Nothing` for a missing row.

In `pgmq-hasql/src/Pgmq/Hasql/Encoders.hs`:

- `readMessageEncoder` (lines 143-148): append the fourth parameter
  `<> (view #conditional >$< E.param (E.nullable E.jsonb))` and update the "3-param"
  comment on line 143 to "4-param".

In `pgmq-hasql/src/Pgmq/Hasql/Statements/Types.hs`:

- `PopMessage.qty` doc (line 176): keep the meaning, make it true — e.g.
  "Number of messages to pop. Nothing = 1, applied via COALESCE in the statement (a bound
  SQL NULL never triggers the plpgsql DEFAULT)."
- `ReadMessage.conditional` doc (lines 158-159): now a real parameter; document the
  containment semantics.
- `EnableNotifyInsert.throttleIntervalMs` doc (line 184): same COALESCE phrasing with the
  250 ms default.

In `pgmq-hasql/src/Pgmq/Hasql/Statements/QueueManagement.hs`:

- `enableNotifyInsert` (lines 60-63): change the SQL to
  `"select from pgmq.enable_notify_insert($1, coalesce($2, 250))"`. Do NOT touch the
  channel-name haddock on line 59 — plan 14 owns that correction (PGH-9); note this in
  your commit message to avoid a collision.

In `pgmq-hasql/src/Pgmq/Hasql/Sessions.hs`:

- `changeVisibilityTimeout` (lines 163-164) and `setVisibilityTimeoutAt` (lines 171-172):
  result types become `Session (Maybe Message)`.

In `pgmq-effectful/src/Pgmq/Effectful/Effect.hs`:

- The GADT constructors `ChangeVisibilityTimeout` (line 161) and `SetVisibilityTimeoutAt`
  (line 164) change to `-> Pgmq m (Maybe Message)`; the wrapper functions
  `changeVisibilityTimeout` (lines 275-276) and `setVisibilityTimeoutAt` (lines 282-283)
  follow. The plain interpreter (`Interpreter.hs` lines 124 area) and the traced
  interpreter (`Interpreter/Traced.hs`) pass results through polymorphically — recompile
  and fix any type annotations the compiler flags; do not change any span name, kind, or
  attribute in the traced interpreter (ADR 0001 constraint; the operation still runs and
  is still traced identically whether it returns Just or Nothing).

In `pgmq-config/src/Pgmq/Config/Types.hs`:

- Line 59 doc: now true; tighten to "Minimum milliseconds between notifications. Nothing
  uses the documented pgmq default (250 ms), applied via COALESCE in the pgmq-hasql
  statement so SQL NULL never reaches the function."

Then update the M1 set_vt test to its final `Nothing`/`Just` assertions (replacing the
temporary `assertSessionFails` form), and update the M1 conditional test if its `Just`
case was written loosely.

Acceptance: `cabal test pgmq-hasql-test pgmq-config-test pgmq-effectful-test` all green;
in particular every NullSemanticsSpec case passes and the three updated pre-existing
visibility-timeout tests explicitly observe `Just`.

### Milestone 3 — family-wide validation, changelog, and release handoff

Scope: prove nothing else regressed, write this plan's release-note material, and hand it to
the single release owner.

Run the full suite (`cabal test all`) and the nix checks if available
(`just nix-test`). Add a CHANGELOG entry under an "Unreleased (0.5.0.0)" heading in
`CHANGELOG.md` following
the existing format (see the 0.4.0.0 entry for the Breaking Changes section style):
breaking — `changeVisibilityTimeout`/`setVisibilityTimeoutAt` now return `Maybe Message`
across pgmq-hasql and pgmq-effectful; fixed — NULL-parameter semantics for
pop/read/read_with_poll/enable_notify_insert (`Nothing` now means the documented
default, never "unbounded" or "always fails"); `ReadMessage.conditional` now works. Do
NOT bump the version numbers in the five `.cabal` files here. Plan 12 performs the one
coordinated 0.5.0.0 bump after every prerequisite is complete.

Record in this plan's Progress/Decision Log which migration numbers were claimed by
siblings at the time you land (read `pgmq-migration/migrations/manifest` — baseline is
`0001-install-v1.11.0.sql` and `0002-schema-management-comment.sql`).

Acceptance: `cabal test all` green from the pgmq-hs root; CHANGELOG material updated; the
NULL-parameter design note added; and this plan's living sections updated.


## Concrete Steps

All commands run from the repository root inside the nix dev shell (`nix develop` if not
using direnv; the shell provides GHC
9.12.4, cabal, and the PostgreSQL binaries ephemeral-pg needs).

Milestone 1:

```bash
cd .
# create pgmq-hasql/test/NullSemanticsSpec.hs; register in pgmq-hasql.cabal + test/Main.hs
# add testEnsureQueuesWithNotifyDefault to pgmq-config/test/ConfigSpec.hs
cabal build pgmq-hasql-test pgmq-config-test
cabal test pgmq-hasql-test --test-show-details=direct
cabal test pgmq-config-test --test-show-details=direct
```

Expected M1 transcript excerpts (shapes, not exact strings):

```text
pop does not widen scope on Nothing:            FAIL
  Should pop exactly 1 message: expected: 1, but got: 5
enableNotifyInsert Nothing applies 250ms:       FAIL
  Session failed: SessionUsageError (StatementSessionError ... 23502
  ... null value in column "throttle_interval_ms" ...)
set_vt raced-away row:                          OK   (M1 form: assertSessionFails)
```

Milestone 2:

```bash
cd .
# apply the edits listed in Plan of Work M2
cabal build all
cabal test pgmq-hasql-test pgmq-config-test pgmq-effectful-test --test-show-details=direct
```

Expected: all suites report 0 failures; NullSemanticsSpec lists every case OK.

Milestone 3:

```bash
cd .
cabal test all --test-show-details=direct
just nix-test   # optional, hermetic re-run of the hasql + migration suites
cat pgmq-migration/migrations/manifest   # record sibling claims in Decision Log
```


## Validation and Acceptance

The change is effective when a novice can do the following and see the stated results.
From the pgmq-hs root, `cabal test pgmq-hasql-test --test-show-details=direct` shows a
`NullSemanticsSpec` group in which: popping with `qty = Nothing` from a 5-message queue
reports 1 popped / 4 remaining; reading with `batchSize = Nothing` reports 1 read and a
follow-up read of 4; `readWithPoll` the same; a conditional `Just {"kind":"a"}` read
returns only the matching message while `Nothing` returns all; enabling notify with
`throttleIntervalMs = Nothing` succeeds and `listNotifyInsertThrottles` reports 250; and
both single-message visibility functions return `Nothing` for a fabricated `MessageId`
and `Just` for a real one, with no session error either way.
`cabal test pgmq-config-test` shows the `withNotifyInsert Nothing` reconcile passing
twice (enable then skip). `cabal test all` is green. Each of these behaviors demonstrably
fails before M2 (M1's recorded transcripts are the before-evidence).


## Idempotence and Recovery

Every step is re-runnable: tests are additive modules; the src edits are idempotent text
replacements (re-applying them is a no-op); no migration file, no database state, and no
version bump is involved (the release is cut once by plan 12). If M2
must be abandoned midway, revert the working tree — M1's tests remain valid red markers.
The ephemeral-pg databases are created and destroyed per test run; a crashed test run
leaves at most a stale temp directory that the next run ignores.


## Interfaces and Dependencies

At the end of M2 the following signatures exist (full module paths; these are the
load-bearing ones — anything not listed keeps its current signature):

```haskell
-- pgmq-hasql, Pgmq.Hasql.Statements.Message
changeVisibilityTimeout :: Statement VisibilityTimeoutQuery (Maybe Message)
setVisibilityTimeoutAt  :: Statement VisibilityTimeoutAtQuery (Maybe Message)
pop          :: Statement PopMessage (Vector Message)          -- sql uses coalesce($2,1)
readMessage  :: Statement ReadMessage (Vector Message)         -- 4 bound params now
readWithPoll :: Statement ReadWithPollMessage (Vector Message) -- coalesce($3,1)/($6,'{}')

-- pgmq-hasql, Pgmq.Hasql.Sessions
changeVisibilityTimeout :: VisibilityTimeoutQuery -> Session (Maybe Message)
setVisibilityTimeoutAt  :: VisibilityTimeoutAtQuery -> Session (Maybe Message)

-- pgmq-effectful, Pgmq.Effectful.Effect
changeVisibilityTimeout :: (Pgmq :> es) => VisibilityTimeoutQuery -> Eff es (Maybe Message)
setVisibilityTimeoutAt  :: (Pgmq :> es) => VisibilityTimeoutAtQuery -> Eff es (Maybe Message)
```

Dependencies: no new ones. The test module uses the already-present test deps of
`pgmq-hasql-test` (tasty, tasty-hunit, aeson, vector, ephemeral-pg) and of
`pgmq-config-test`. Coordination interfaces: plan 14 owns
`pgmq.enable_notify_insert`'s new SQL definition (including this plan's server-side
COALESCE guard) and the `QueueManagement.hs` line 59 channel Haddock; plan 12 owns every
version bump, consolidated changelog, consumer-bound update, and the shibuya `leaseExtend`
fix this plan's `Maybe` change requires.


## Revision Note

2026-07-23: Relocated from keiro plan 129 into the authoritative pgmq-hs repository. Added
the three required pre-existing test updates for the `Maybe Message` API, moved durable
contract documentation to `docs/design/`, removed stale migration-number reservations, and
assigned release and consumer work to plan 12.
