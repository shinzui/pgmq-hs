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
  revisions:
    - model: "gpt-6-astra"
      harness: "codex-cli"
      at: 2026-09-12T15:04:18Z
      mode: "update"
      note: "Validated against repository SQL, tests and ADRs; corrected native/stock scope, ledger coordination, index evidence and retention contracts."
  reviews:
    - model: "gpt-6-astra"
      harness: "codex-cli"
      at: 2026-09-12T15:04:18Z
      verdict: "comments"
      note: "Repository review findings applied in this revision; SQL regressions and performance measurements remain explicit implementation acceptance work."
---

# Give the grouped reads a deterministic return order

This ExecPlan is a living document. Keep its progress, evidence and decisions current.

## Purpose / Big Picture


After applying a new native migration, a direct grouped read returns a predictable sequence:
`read_grouped` returns selected groups in priority order and ascending message IDs within
each group; `read_grouped_head` returns selected heads in ascending message ID order.
Polling delegates to those base functions. A Haskell caller can observe the sequence in its
returned `Vector Message` without sorting it client-side.

This is a result-order correction, not a processing-order or exactly-once guarantee. Selection,
visibility, row locks, batch size and error behavior must remain unchanged. A Haskell library
upgrade alone does not change stock PGMQ extension SQL, which remains supported separately.

## Progress


- [ ] M1: Append migration 0007 with ordered base reads; update manifest and latest convergence, including its comparator sentinel.
- [ ] M2: Add adversarial multi-group, head, polling and Haskell vector-order regressions; verify unchanged selection/visibility behavior.
- [ ] M3: Prepare upstream patch artifact, package/root changelog entries and migration-readiness evidence; no publication.

## Surprises & Discoveries


2026-09-12 repository review: the polling functions delegate, so only two base bodies need
replacement. `testConvergence` excludes local bodies, then deliberately deletes and mutates the normalized head body to test its comparator.
After the body is excepted, the deletion becomes a no-op and its assertion fails (inserting
a changed body still creates a detected difference). Move the sentinel pair to a still-compared
key or raw snapshots after the head body becomes an exception. The four-entry 1.12 checkpoint never
applies the proposed migration and must receive no new exception.

No failing SQL test was run during this plan revision. Small fixtures can happen to arrive
ordered on the old SQL; an unspecified order does not imply a reliable failure on every plan.

## Decision Log


2026-09-12: Carry `batch_selection.overall_rank` to the final SELECT for `read_grouped`;
order heads by `msg_id`. For sends A1, B1, A2, B2 with ascending IDs, a full grouped batch
must return A1, A2, B1, B2. This distinguishes the chosen contract from a global ID sort.
Round-robin remains A1, B1, A2, B2 for this fixture and is not changed.

2026-09-12: Override only the base functions. Keep existing locking clauses and FORMAT
arguments except changes needed to carry rank. Test polling and direct Haskell consumption
rather than assuming an ordered base query proves every caller path.

2026-09-12: Allocate 0007 separately from the index plan's 0008. Never edit an applied
migration, including one applied before publication. Preserve both historical checkpoints
and the predecessor validator.

## Outcomes & Retrospective


Repository validation updated the plan. No implementation or database test result yet.

## Context and Orientation


The five libraries in this checkout declare version 0.6.0.0. The SQL vendor is pristine
PGMQ v1.13.0, commit `32c075bb6dbed66a303d1a792393c93e36c09a97`, from
`mori://pgmq/pgmq/repos/pgmq-upstream`. Its project-relative SQL path is
`pgmq-extension/sql/pgmq.sql` (artifact-level URI pending); the local copy is
`vendor/pgmq/pgmq-extension/sql/pgmq.sql`. Do not change vendor files.

`pgmq-migration/migrations/manifest` lists six SQL files ending at
`0006-preserve-partitioned-reentry-v1.13.0.sql`. The component in
`pgmq-migration/src/Pgmq/Migration/Internal/Definition.hs` embeds that ledger.
`pgmq-migration/pgmq-migration.cabal` already packages `migrations/*.sql`.
Migrations 0003 and 0006 demonstrate local CREATE OR REPLACE overrides with comments
explaining each deliberate divergence.

In vendored `read_grouped`, the common table expressions (named query stages) compute groups,
lock visible heads, reject groups with earlier invisible rows, collect visible messages,
and assign `overall_rank` by group priority and rank within group. `selected_messages`
currently discards that rank. The terminal UPDATE returns the seven message-record fields
without an ordered final SELECT. `read_grouped_head` similarly selects absolute group heads
regardless of visibility, leases visible selected heads, and returns unordered UPDATE rows.
`read_grouped_rr` demonstrates an `updated_messages` CTE followed by a sorted SELECT.

`read_grouped_with_poll` and `read_grouped_head_with_poll` iterate their base functions and
RETURN NEXT each record, then stop after a nonempty batch. These wrappers need no new SQL
body but must be tested with nonzero polling duration. Do not promise row order for arbitrary
outer SQL joins, aggregation or sorting; this contract targets direct function consumption.

`pgmq-migration/test/Main.hs` owns a shared disposable database/connection and must run
serially. `testNativeComponent` asserts the exact manifest names. `testConvergence` compares
the four-migration 1.12 checkpoint and six-migration current state with separate fresh fixtures.
It removes notification and partition-creation body exceptions, checks that those keys exist,
and performs deliberate missing/altered-head and altered-metric sentinel checks. New exceptions
must not erase the evidence those sentinels are intended to provide.

The direct client statements are in `pgmq-hasql/src/Pgmq/Hasql/Statements/Message.hs`;
`pgmq-hasql/test/AdvancedOpsSpec.hs` already tests grouped/head operations. Extend its native
fixture path or add an isolated native-order test module registered in
`pgmq-hasql/test/Main.hs`; do not impose local override behavior on stock-server fixtures.

The [compatibility ADR](../adr/pgmq-1.12-1.13-compatibility.md) requires immutable history,
1.12 client support, preserved 1.11 imports and serial migration tests. The
[vendoring design](../design/012-vendor-upstream-pgmq-sql.md) requires local overrides separate
from upstream bytes. The [FIFO boundary ADR](../adr/fifo-native-overrides-and-index-upgrade-boundary.md)
limits these guarantees to migrated native SQL. Documentation is owned by
`docs/plans/21-state-the-fifo-ordering-and-partitioned-retention-contracts-truthfully.md`;
index changes are owned by `docs/plans/20-replace-the-fifo-gin-index-with-one-the-grouped-reads-can-use.md`.

## Plan of Work


### M1: Ordered SQL and accurate convergence


Create `pgmq-migration/migrations/0007-order-grouped-read-returning.sql`. Copy the two
vendored base definitions under their exact CREATE OR REPLACE signatures. For `read_grouped`,
carry `overall_rank` through `selected_messages`, return it from an `updated_messages` CTE,
and select only the seven message fields ordered by rank. For heads, wrap UPDATE similarly
and select its seven fields ordered by `msg_id`. Preserve argument names, defaults, result
identity, language and existing query stages. Do not copy round-robin's extra visibility guard
or advisory lock into functions that do not currently have them.

Append the manifest entry once and extend `testNativeComponent`. In `testConvergence`, use
all names when `latest` is true and `take 4 names` otherwise. Add the two body exceptions
only when latest is true. Move the comparator's deliberate body-mutation sentinel to an
unexcepted function such as `body:read(text, integer, integer, jsonb)`, verifying its actual
snapshot key, or test mutations on unnormalized snapshots. Keep missing-function and signature
coverage as well. Acceptance is both historical and latest convergence passing, with an injected
change to an unexcepted body or function signature still failing the comparison.

### M2: Observable ordered results without semantic drift


Use the migration suite's disposable fixtures, with reset/migrate at the start of each case.
Send interleaved groups A1, B1, A2, B2 and assert the exact grouped result A1, A2, B1, B2.
Also assert a single group's IDs ascend and head reads return only A1, B1 in ascending ID
order even with quantity greater than the total number of messages. Use returned send IDs;
never assume a sequence starts at one. Cover missing/null group headers' default group.

Make physical row order adversarial using a scratch reverse-ID index and CLUSTER, or update
rows in reverse order, and record the pre-override result. Exercise multiple supported planner
settings diagnostically. Run a mutation experiment with the terminal ORDER BY removed and
record whether the fixture detects it. If the current executor happens to preserve order,
record that limitation; do not claim guaranteed mutation failure or weaken the ordered oracle.
The structural ordered SELECT plus behavioral fixtures together support the contract.

Test both polling functions on nonempty queues and test the returned Haskell vector through
the native fixture. Do not sort results in the test or its outer SQL. Verify read counts and
visibility updates occur once, blocked heads remain blocked, multiple rows from a group remain
possible in `read_grouped`, and empty/zero-quantity behavior matches the baseline. Include a
held-head-lock/two-connection case to show the rewrite preserves existing skip-locked behavior;
this does not assert stronger concurrency semantics than upstream. All tests must finish with
the same selected IDs and lease changes as before, with only their returned sequence changed.

### M3: Reviewable upstream and release-readiness artifacts


Prepare `docs/upstream-patches/grouped-read-ordering.patch` outside the vendor tree. Use Mori
to locate upstream source and verify the tag against upstream before recording its exact base
commit. Include the main SQL changes, relevant FIFO SQL tests and a prospective upgrade script;
do not rewrite an already-released upstream migration and imply it will replay. Validate the
patch with `git apply --check` in an isolated checkout of its recorded base. Record commands
and results in this plan. Do not submit the patch.

Add Unreleased entries to root `CHANGELOG.md` and `pgmq-migration/CHANGELOG.md`. Record that
applying native migration 0007 is required; Haskell package presence alone is insufficient.
Do not select release bounds from downstream memory: verify authoritative Hackage versions
and repository release tags during actual release preparation. Hand exact semantics and
migration evidence to the documentation plan, without editing its owned prose in this child.

## Concrete Steps


Run from the repository root. These are implementation commands, not results of this review.

```bash
cat pgmq-migration/migrations/manifest
rg -n 'read_grouped|overall_rank|updated_messages' vendor/pgmq/pgmq-extension/sql/pgmq.sql
rg -n 'testConvergence|exceptions|snapshot comparison|0006' pgmq-migration/test/Main.hs
# Implement M1 and M2, then:
cabal build all
nix develop .#partman --command cabal test pgmq-migration:pgmq-migration-test --test-options='-j1' --test-show-details=direct
nix develop .#partman --command cabal test pgmq-hasql:pgmq-hasql-test --test-options='-j1' --test-show-details=direct
```

The partman shell supplies PostgreSQL and requires pg_partman. Expected: both suites exit
zero, new ordering fixtures pass, historical imports/convergence remain green and partition
acceptance does not skip. Record actual test names/output rather than invented transcripts.
Use Mori before looking up unfamiliar test-library or migration APIs.

## Validation and Acceptance


M1 passes old 1.12 and new latest convergence independently; only intended latest bodies
differ, while signatures/results and comparator sentinels remain checked. M2 returns the
exact interleaved-group expected sequence through SQL, polling and the Haskell vector;
head cardinality, blocked heads, lease effects and baseline selection stay unchanged.
Retain the pre-override and mutation evidence honestly, including any nondiscriminating case.
M3 produces a patch that applies to its stated base and changelogs describing native-only
behavior. No publication or downstream source changes are required for completion.

## Idempotence and Recovery


Tests use disposable databases and serial execution. Migrations are applied once by the
native ledger; CREATE OR REPLACE bodies can be exercised in scratch databases separately.
Never alter 0001–0006 or a subsequently applied 0007. Repair deployed mistakes with another
numbered migration. If another initiative has occupied 0007, update both this plan and the
parent allocation before implementation; do not merge unrelated payloads. Database tests
must not target an existing user's queue. Revert scratch SQL mutations before final acceptance.

## Interfaces and Dependencies


Keep `pgmq.read_grouped(text, integer, integer)` and
`pgmq.read_grouped_head(text, integer, integer)` returning `SETOF pgmq.message_record`.
No public Haskell types or signatures change. Own the new SQL file, manifest extension,
convergence-test refactor, focused native client test, root/package changelog entry and upstream
patch artifact. Do not change grouped selection algorithms, vendor bytes or FIFO-index code.
The existing migration test dependencies suffice; no new bounds are selected here.

Revision note (2026-09-12): Corrected latest-only exceptions and the hidden comparator sentinel,
added a discriminating multi-group ordering oracle and native-client scope, removed brittle
promised mutation failures and publication/version assumptions, and reserved a separate migration.
