---
id: 19
slug: give-the-grouped-reads-a-deterministic-return-order
title: "Give the grouped reads a deterministic return order"
kind: exec-plan
created_at: 2026-09-12T14:23:59Z
intention: "intention_01m2b005z8egm8rarkzn70zv4t"
master_plan: "docs/masterplans/5-correct-the-fifo-grouped-read-ordering-index-and-partition-retention-contracts.md"
provenance:
  created_by:
    model: "claude-opus-5[1m]"
    harness: "claude-code"
    at: 2026-09-12T14:23:59Z
---

# Give the grouped reads a deterministic return order

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.
If durable project context changes, update or create ADRs in docs/adr/ in the same change.


## Purpose / Big Picture

A client that calls PGMQ's FIFO grouped reads gets a batch of messages back. Today the *order of
the rows in that batch* is whatever the PostgreSQL planner happened to produce, because the
statement ends in an `UPDATE ... RETURNING` with no `ORDER BY`. The word "FIFO" invites callers to
treat row order as send order, and this repository's own
[`docs/design/008-fifo-read.md`](../design/008-fifo-read.md) says `read_grouped` "ensures strict
ordering within a group", so a caller that iterates the returned batch in order is trusting
something no SQL construct guarantees.

After this plan, `pgmq.read_grouped` and `pgmq.read_grouped_head` return their rows in a defined
order — the same selection order their internal CTEs already compute — and a test fails if that
order is ever lost again. The fix is a local override migration in the style this repository
already uses for other deliberate upstream divergences. `pgmq.read_grouped_rr` already does exactly
this, so the change makes the three grouped reads consistent with each other rather than inventing
a new convention.

You can see it working by seeding one FIFO group with several messages, reading them in a single
batch, and observing that the returned `msg_id`s ascend; before the change, that assertion passes
or fails at the planner's discretion.


## Progress

- [ ] M1: `pgmq-migration/migrations/0007-order-grouped-read-returning.sql` re-creates
  `pgmq.read_grouped` and `pgmq.read_grouped_head` with an ordered final `SELECT`; added to
  `pgmq-migration/migrations/manifest`.
- [ ] M1: `pgmq-migration/test/Main.hs` updated in its three coordinated places (ledger list,
  convergence `take` count, convergence `exceptions`); `cabal test pgmq-migration-test` green.
- [ ] M2: Row-order test added and failing against the pre-override schema, passing after it;
  polling-variant delegation confirmed to inherit the order.
- [ ] M3: Upstream patch prepared against `mori://pgmq/pgmq` and recorded here (not submitted);
  `CHANGELOG.md` entry written; release target recorded.


## Surprises & Discoveries

- Plan authoring (2026-09-12): the two `_with_poll` grouped reads need no override. Both delegate
  to their base function with `FOR r IN SELECT * FROM pgmq.read_grouped(...) LOOP RETURN NEXT r`
  (`vendor/pgmq/pgmq-extension/sql/pgmq.sql` lines 292 and 480), and `RETURN NEXT` preserves the
  inner function's row order, so overriding the two base functions fixes all four entry points.
- Plan authoring (2026-09-12): `pgmq.read_grouped_rr` already carries a `selection_order` column
  through an `updated_messages` CTE and finishes with an ordered `SELECT` (same file, line 126).
  This is not a new pattern to invent — the repository is internally inconsistent, and this plan
  makes two functions match the third.


## Decision Log

- Decision: Override `pgmq.read_grouped` and `pgmq.read_grouped_head` only, not their `_with_poll`
  siblings.
  Rationale: The polling variants delegate and preserve row order (see Surprises & Discoveries), so
  overriding them would duplicate two long bodies for no behavioral gain and would double the
  surface that drifts from upstream at the next vendor bump.
  Date: 2026-09-12

- Decision: Order `read_grouped` by its existing `batch_selection.overall_rank`, not by a global
  `msg_id` sort.
  Rationale: `overall_rank` is the order the function's own author computed — oldest group first,
  then send order within each group — and `read_grouped_rr` already exposes exactly that intent as
  `selection_order`. Within a single group the two orders coincide, so nothing is lost, and keeping
  the author's rank makes the three grouped reads describable by one sentence in
  `docs/design/008-fifo-read.md`.
  Date: 2026-09-12

- Decision: Row order is the only thing this plan changes; batch-filling, visibility predicates,
  advisory locking, and `FOR UPDATE SKIP LOCKED` behavior stay byte-equivalent to upstream.
  Rationale: These CTEs carry the FIFO blocking semantics that consumers already depend on. A local
  override that also "improves" them would be unreviewable against the vendored source and would
  widen the divergence the convergence test has to excuse.
  Date: 2026-09-12


## Outcomes & Retrospective

(To be filled during and after implementation.)

## Context and Orientation

**What this repository is.** `pgmq-hs` packages the PostgreSQL message-queue extension
[PGMQ](https://pgmq.github.io/pgmq/) for Haskell. It has five libraries — `pgmq-core`,
`pgmq-hasql`, `pgmq-effectful`, `pgmq-config`, `pgmq-migration` — released together, currently at
0.6.0.0.

**How PGMQ's SQL gets into a database.** Two paths, and only one is yours to change:

- `vendor/pgmq/` is a byte-exact mirror of an upstream PGMQ tag (currently `v1.13.0` at
  `32c075bb6dbed66a303d1a792393c93e36c09a97`), managed per
  [`docs/design/012-vendor-upstream-pgmq-sql.md`](../design/012-vendor-upstream-pgmq-sql.md).
  **Never edit anything under `vendor/`.**
- `pgmq-migration/migrations/` is an ordered, append-only ledger of SQL files, listed in
  `pgmq-migration/migrations/manifest` and embedded at compile time by
  `pgmq-migration/src/Pgmq/Migration/Internal/Definition.hs` (`embedMigrationManifest`). It
  currently holds six entries:

  ```text
  0001-install-v1.11.0.sql
  0002-schema-management-comment.sql
  0003-notify-crash-safety-and-locking.sql
  0004-upgrade-v1.12.0.sql
  0005-upgrade-v1.13.0.sql
  0006-preserve-partitioned-reentry-v1.13.0.sql
  ```

  Files `0003` and `0006` are *local overrides*: they `CREATE OR REPLACE` upstream functions under
  their original signatures to fix defects this repository found. Read `0003`'s header comment
  before writing yours — it is the house style: a numbered list of each deliberate divergence and
  why it exists. Applied files are immutable; a new fix is a new file. Your file is `0007`.

**The defect.** In `vendor/pgmq/pgmq-extension/sql/pgmq.sql`, `pgmq.read_grouped` (function starts
at line 381) builds its batch through a chain of CTEs — `fifo_groups`, `locked_groups`,
`group_priorities`, `filtered_groups`, `available_messages`, `batch_selection`,
`selected_messages` — where `batch_selection` computes exactly the order the author intended:

```sql
SELECT
    msg_id,
    ROW_NUMBER() OVER (ORDER BY group_priority, msg_rank_in_group) as overall_rank
FROM available_messages
```

and then discards it:

```sql
UPDATE pgmq.%I m
SET vt = clock_timestamp() + %L, read_ct = read_ct + 1, last_read_at = clock_timestamp()
FROM selected_messages sm
WHERE m.msg_id = sm.msg_id
RETURNING m.msg_id, m.read_ct, m.enqueued_at, m.last_read_at, m.vt, m.message, m.headers;
```

An `UPDATE ... RETURNING` emits rows in whatever order the executor updates them; the `ORDER BY`
inside `selected_messages` constrains the CTE, not the output. `pgmq.read_grouped_head` (line 245)
has the same shape, ordering `selected_messages` by `q.msg_id` and then returning unordered.

**The template for the fix.** `pgmq.read_grouped_rr` (line 126) already solved this. It carries a
`selection_order` column through the lock step, wraps the `UPDATE` in a CTE, and finishes with an
ordered plain `SELECT`:

```sql
        updated_messages AS (
            UPDATE pgmq.%5$I m
            SET vt = clock_timestamp() + %6$L, read_ct = read_ct + 1, last_read_at = clock_timestamp()
            FROM selected_messages sm
            WHERE m.msg_id = sm.msg_id
              AND m.vt <= clock_timestamp()
            RETURNING m.msg_id, m.read_ct, m.enqueued_at, m.last_read_at, m.vt, m.message, m.headers, sm.selection_order
        )
        SELECT msg_id, read_ct, enqueued_at, last_read_at, vt, message, headers
        FROM updated_messages
        ORDER BY selection_order;
```

Mirror this. Do **not** change the batch-filling logic, the visibility predicates, the advisory
locking, or the `FOR UPDATE SKIP LOCKED` behavior of either function: this plan changes row order
and nothing else.

**The polling variants need no change.** `pgmq.read_grouped_with_poll` (line 480) and
`pgmq.read_grouped_head_with_poll` (line 292) do not duplicate the query. Each loops
`FOR r IN SELECT * FROM pgmq.read_grouped(...)` (respectively `read_grouped_head`) and
`RETURN NEXT r`, which preserves the inner function's row order. Overriding the two base functions
fixes all four. Verify this by reading those two bodies before assuming it.

**Where the tests live and what breaks.** `pgmq-migration/test/Main.hs` is a tasty suite that
starts a disposable PostgreSQL via the `ephemeral-pg` package (`import EphemeralPg`). Three places
know about the ledger, and a new migration touches all three:

1. `testNativeComponent` (around line 321) asserts the exact ledger list. Its comment — "The one
   place the ledger is spelled out" — is aspirational; see (2).
2. `testConvergence` (line 155) runs `forM_ (take (if latest then 6 else 4) names)`. The `6` is the
   current ledger length. Grow it to `7`.
3. `testConvergence` applies the migration ledger to one database and a fresh vendored
   `pgmq.sql` to another, snapshots both schemas, and asserts they are identical *except* for an
   `exceptions` list of keys (line 168) naming the function bodies that local overrides
   deliberately change:

   ```haskell
   let exceptions =
         [ "body:notify_queue_listeners()",
           "body:enable_notify_insert(text, integer)",
           if latest then "body:create_partitioned(text, text, text, integer)" else "body:create_partitioned(text, text, text)"
         ]
   ```

   Add `"body:read_grouped(text, integer, integer)"` and
   `"body:read_grouped_head(text, integer, integer)"`. The suite also asserts every exception key is
   *present* in both snapshots, so a misspelled signature fails loudly instead of silently excusing
   a real difference. Note the `latest` flag: the suite runs the whole comparison twice, once
   against a 1.12 fixture (four migrations) and once against the vendored 1.13 (six, soon seven).
   `read_grouped_head` exists in both, so both exception lists need it.

**Running the database tests.** `cabal test pgmq-migration-test` from the repository root. The
partition-related cases need pg_partman and skip without it unless `PGMQ_REQUIRE_PARTMAN=1`;
`nix develop .#partman` provides a PostgreSQL with the extension and sets that variable (see
`flake.module.nix` and `pgmq-migration/README.md`). This plan's own cases need no extension, but
run the full suite in that shell before claiming it green.

**ADR and design-note context** (scanned per
[`agents/skills/exec-plan/ADR.md`](../../agents/skills/exec-plan/ADR.md)):

- [`docs/adr/pgmq-1.12-1.13-compatibility.md`](../adr/pgmq-1.12-1.13-compatibility.md) — the
  upgrade and compatibility boundaries. Your migration appends to the ledger it describes; leave the
  1.11 predecessor validator untouched.
- [`docs/design/012-vendor-upstream-pgmq-sql.md`](../design/012-vendor-upstream-pgmq-sql.md) — why
  the override goes in a migration rather than the vendor tree.
- [`docs/design/008-fifo-read.md`](../design/008-fifo-read.md) — the FIFO contract note that
  currently overstates the ordering guarantee. **Do not edit it here.**
  `docs/plans/21-state-the-fifo-ordering-and-partitioned-retention-contracts-truthfully.md` owns it;
  record what it must say in this plan's Decision Log instead.

**Related plans.** The parent MasterPlan is
`docs/masterplans/5-correct-the-fifo-grouped-read-ordering-index-and-partition-retention-contracts.md`.
`docs/plans/20-replace-the-fifo-gin-index-with-one-the-grouped-reads-can-use.md` also adds a local
override migration; coordinate the number with it (see Idempotence and Recovery). The consumer that
motivated this work is `keiro-pgmq`, whose plan
`mori://shinzui/keiro/plans/116-enforce-fifo-group-ordering-under-failure-and-batched-consumption`
consumes the released fix.


## Plan of Work

**M1 — the override migration.** Create
`pgmq-migration/migrations/0007-order-grouped-read-returning.sql`. Head it with a comment in the
style of `0003`: state that it re-creates two upstream functions under their original signatures to
give their batches a defined row order, name the upstream shape it diverges from, and note that the
`_with_poll` variants inherit the fix through delegation. Then `CREATE OR REPLACE FUNCTION
pgmq.read_grouped(queue_name TEXT, vt INTEGER, qty INTEGER)` and
`pgmq.read_grouped_head(queue_name TEXT, vt INTEGER, qty INTEGER)`, copying the vendored bodies
verbatim except for the final step: wrap the `UPDATE` in an `updated_messages` CTE that additionally
returns the selection-order column, and finish with `SELECT <the seven message_record columns> FROM
updated_messages ORDER BY <that column>`. For `read_grouped` the column is
`batch_selection.overall_rank`, which `selected_messages` must now carry through; for
`read_grouped_head` it is the head `msg_id` itself, so `ORDER BY msg_id` on the wrapper suffices and
no extra column is needed. Keep `RETURNS SETOF pgmq.message_record` and `LANGUAGE plpgsql`
unchanged, and keep the `FORMAT` placeholder numbering consistent with the number of `qtable`
arguments you pass. Append the filename to `pgmq-migration/migrations/manifest`.

At the end of M1 the file exists, `cabal build all` succeeds (the manifest is validated at compile
time), and `cabal test pgmq-migration-test` passes with the three test edits below. Acceptance: a
fresh database migrated through `0007` has the new function bodies, and `testConvergence` still
reports no differences outside its exceptions list.

**M2 — pin the order.** Add a case to `pgmq-migration/test/Main.hs` that seeds one queue, sends
several messages into a single FIFO group (`{"x-pgmq-group": "g1"}`) in a known order, reads them in
one call with `qty` greater than one, and asserts the returned `msg_id`s are ascending. Add a second
case for `read_grouped_head` across three groups asserting its rows ascend by `msg_id`. Write both
so they exercise the migrated schema the suite already builds; do not introduce a second database
fixture.

A row-order assertion can pass by luck on a small table, so make the seeding adversarial: insert
enough messages that a sequential scan and an index/bitmap path would plausibly differ, and — before
applying `0007` in a scratch database — confirm you can observe unordered output at least once, so
you know the test discriminates. Record that observation in Surprises & Discoveries; if the
pre-change order turns out to be stable in practice on this PostgreSQL version, say so plainly
rather than claiming a failing-then-passing test you did not see fail.

At the end of M2 the suite pins row order for both functions. Acceptance:
`cabal test pgmq-migration-test` green, and the new cases fail if the `ORDER BY` is deleted from the
migration.

**M3 — upstream patch and release paperwork.** Prepare a patch against the upstream PGMQ source
(`mori://pgmq/pgmq`, tag `v1.13.0`) making the same two changes to
`pgmq-extension/sql/pgmq.sql` and its upgrade script, with a test in
`pgmq-extension/test/sql/base.sql` if that suite can express row order. **Do not submit it.** Record
where the patch lives and what it contains in this plan's Decision Log. Then write the
`CHANGELOG.md` entry for the affected package(s) and state the release target: a migration-only
change is additive server behavior with no Haskell API change, so 0.6.1.0, which keiro's existing
`>=0.6 && <0.7` bound already admits.


## Concrete Steps

Run everything from the repository root
(`/Users/shinzui/Keikaku/bokuno/libraries/pgmq-hs-project/pgmq-hs`, or wherever
`mori path mori://shinzui/pgmq-hs` resolves).

```bash
# 1. Confirm the ledger's next free number and the current tail.
cat pgmq-migration/migrations/manifest
ls pgmq-migration/migrations/
```

```text
0001-install-v1.11.0.sql
0002-schema-management-comment.sql
0003-notify-crash-safety-and-locking.sql
0004-upgrade-v1.12.0.sql
0005-upgrade-v1.13.0.sql
0006-preserve-partitioned-reentry-v1.13.0.sql
```

```bash
# 2. Read the two function bodies you are about to override and the template to follow.
sed -n '245,290p' vendor/pgmq/pgmq-extension/sql/pgmq.sql   # read_grouped_head
sed -n '381,475p' vendor/pgmq/pgmq-extension/sql/pgmq.sql   # read_grouped
sed -n '126,205p' vendor/pgmq/pgmq-extension/sql/pgmq.sql   # read_grouped_rr, the ordered pattern
```

```bash
# 3. Write the migration, then the manifest line.
$EDITOR pgmq-migration/migrations/0007-order-grouped-read-returning.sql
printf '0007-order-grouped-read-returning.sql\n' >> pgmq-migration/migrations/manifest
```

```bash
# 4. Update the three ledger-aware places in the migration suite.
grep -n 'take (if latest then 6 else 4)' pgmq-migration/test/Main.hs
grep -n 'body:notify_queue_listeners' pgmq-migration/test/Main.hs
grep -n '0006-preserve-partitioned-reentry-v1.13.0' pgmq-migration/test/Main.hs
$EDITOR pgmq-migration/test/Main.hs
```

```bash
# 5. Build and test. The manifest is validated at compile time, so a missing or
#    misnamed file fails the build rather than the suite.
cabal build all
cabal test pgmq-migration-test
```

```text
pgmq-migration-test: OK
  native component ledger:            OK
  convergence with fresh install:     OK
  grouped read returns in send order: OK
  grouped head returns in id order:   OK
```

```bash
# 6. Run the whole suite in the partman shell before declaring done.
nix develop .#partman --command cabal test all
```

Update this section with the transcripts you actually saw, including any failures and what you
changed in response.


## Validation and Acceptance

1. **Row order, `read_grouped`.** Seed a queue, send eight messages into group `g1`, call
   `pgmq.read_grouped('q', 30, 8)`. Observe eight rows whose `msg_id`s ascend. Delete the
   `ORDER BY` from the migration, re-apply to a scratch database, and observe the assertion can
   fail — that is what makes the test meaningful.
2. **Row order, `read_grouped_head`.** Seed three groups with three messages each, call
   `pgmq.read_grouped_head('q', 30, 3)`. Observe exactly three rows — one head per group — in
   ascending `msg_id` order.
3. **No behavioral drift.** The batch-filling and visibility semantics must not change: a group
   whose head is invisible must still be skipped by `read_grouped` and still block its group in
   `read_grouped_head`; `read_grouped` must still be able to return several messages of one group.
   Assert at least the blocked-head case, since it is the property most easily broken by editing
   these CTEs.
4. **Convergence.** `testConvergence` passes for both the 1.12 fixture and the vendored 1.13, with
   the two new keys present in both snapshots. A missing-key failure means your signature string is
   wrong, not that the schema diverged.
5. **Full suite.** `nix develop .#partman --command cabal test all` green, with no new pending or
   skipped cases.


## Idempotence and Recovery

The migration is `CREATE OR REPLACE FUNCTION`, so applying it twice is harmless; the ledger's
history table prevents re-application anyway. Re-running the test suite is safe — each case resets
its disposable database.

Two coordination hazards:

- **Migration number collision.** `docs/plans/20-replace-the-fifo-gin-index-with-one-the-grouped-reads-can-use.md`
  also wants the next free number. If plan 20 has already added `0007`, take `0008`, or add your
  statements to its file if it has not been released yet. Check `manifest` immediately before
  writing, not from memory, and record what you found in this plan's Decision Log.
- **A released migration cannot be edited.** If the fix turns out wrong after release, the recovery
  is another `CREATE OR REPLACE` in a new numbered file — never an edit to `0007`.

If the row-order test proves flaky, do not weaken it to an unordered set comparison; that would
re-open the defect. Investigate whether the override actually shipped (compare
`pg_get_functiondef` against the migration) before changing the assertion.


## Interfaces and Dependencies

No Haskell API changes. The existing operations already reach both functions:

```haskell
-- pgmq-hasql/src/Pgmq.hs and pgmq-effectful/src/Pgmq/Effectful.hs (0.6.0.0)
readGrouped          :: ReadGrouped         -> …
readGroupedWithPoll  :: ReadGroupedWithPoll -> …
readGroupedHead      :: ReadGrouped         -> …
readGroupedHeadWithPoll :: ReadGroupedWithPoll -> …
```

Their result type is an ordered `Vector Message`, so a defined SQL row order is observable by every
caller without a signature change.

Files this plan changes:

- `pgmq-migration/migrations/0007-order-grouped-read-returning.sql` (new)
- `pgmq-migration/migrations/manifest`
- `pgmq-migration/test/Main.hs`
- `CHANGELOG.md`

Files this plan must not change: anything under `vendor/`, `pgmq-migration/migrations/0001`–`0006`,
`docs/design/008-fifo-read.md`, and the `pgmq-hasql` FIFO-index statements that
`docs/plans/20-replace-the-fifo-gin-index-with-one-the-grouped-reads-can-use.md` owns.

Test dependencies already declared in `pgmq-migration/pgmq-migration.cabal`: `ephemeral-pg >=0.2.1`,
`hasql`, `tasty`, `tasty-hunit`. pg_partman is needed only by the partition cases, via
`nix develop .#partman`.
