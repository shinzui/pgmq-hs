---
id: 20
slug: replace-the-fifo-gin-index-with-one-the-grouped-reads-can-use
title: "Replace the FIFO GIN index with one the grouped reads can use"
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

# Replace the FIFO GIN index with one the grouped reads can use

This ExecPlan is a living document. Its first milestone is an experiment; a successful
replacement is not assumed in advance.

## Purpose / Big Picture


Measure whether an expression index improves real grouped reads, and, if it does, make it
available through native FIFO-index provisioning with an explicit upgrade path for legacy
indexes. The current GIN index on `headers` does not index the extracted FIFO group key.
That mismatch is real; a guarantee that a btree eliminates full-backlog work is not.

Users should be able to inspect before/after evidence, create the supported index for a new
queue, deliberately upgrade an existing native queue, and reconcile twice without spurious
creation reports. Stock PGMQ 1.12/1.13 extension clients remain supported. The same index name
can identify different definitions on different server installations; existence reporting must
not silently become a claim about performance.

## Progress


- [ ] M1: Create a reproducible full-query experiment and record no-index/GIN/btree results across representative workloads.
- [ ] M2: If justified, append separate migration 0008, test explicit legacy upgrade and idempotence, partitioned/unlogged queues and malformed index collisions.
- [ ] M3: Verify unchanged existence reporting on native and stock servers; document the opt-in upgrade procedure and lock behavior.
- [ ] M4: Prepare upstream patch, root/package changelog entries and evidence handoff; no publication.

## Surprises & Discoveries


2026-09-12 repository validation found only two public FIFO-index creation functions:
`pgmq.create_fifo_index` and `pgmq.create_fifo_indexes_all`. There is no `pgmq.create_fifo`.
Changing the helper affects subsequent calls; it does not retroactively change existing indexes.
The reconciler snapshots existing names and skips indexes that are already present.

Rejecting all GIN indexes in client-side detection would break convergence against a stock
extension: its unchanged create function still creates GIN. A name-only SkippedFifoIndex is
an existence report, consistent with current policy, rather than a claim of useful indexing.
No EXPLAIN or timing experiment was executed during plan validation.

## Decision Log


2026-09-12: Keep the conventional `q_<queue>_fifo_idx` name and published existence query.
Explicit native create calls may replace an exact known legacy GIN definition; startup
reconciliation continues to skip existing indexes. This preserves the existing non-conversion
policy and requires operators to choose when to acquire index-build locks.

2026-09-12: Benchmark the composite group-key/msg_id btree as a candidate, including variants
with vt as a key or included column. Choose using full-query buffers, time, write costs and
lock duration, not a forced planner node. Full group aggregation may remain linear and an
expression index is not automatically index-only, especially while leasing updates vt.

2026-09-12: A negative experiment is useful evidence, not a shipped optimization. If no
candidate justifies replacement, update this plan and the parent to an experiment/documentation
outcome, omit migration 0008 and the SQL patch, and let documentation land with the limitation.
Do not widen scope into grouped-read algorithm changes without revising the plans first.

## Outcomes & Retrospective


Planning corrections complete; performance and implementation acceptance remain pending.

## Context and Orientation


The checkout declares family version 0.6.0.0 and vendors PGMQ v1.13.0 at
`32c075bb6dbed66a303d1a792393c93e36c09a97`. Source ownership is
`mori://pgmq/pgmq/repos/pgmq-upstream`; its project-relative source
`pgmq-extension/sql/pgmq.sql` has an artifact-level URI pending. The local copy is
`vendor/pgmq/pgmq-extension/sql/pgmq.sql` and must remain pristine.

The native manifest currently ends with 0006. This plan's implementation hard-depends on
`docs/plans/19-give-the-grouped-reads-a-deterministic-return-order.md` for its 0007 ledger
prefix and convergence-test refactor. M1's scratch experiment can precede that child.
Reserve `pgmq-migration/migrations/0008-fifo-index-btree-expression.sql` separately; do not
append this work to 0007. The Cabal `migrations/*.sql` source glob already includes new SQL.

`_create_fifo_index_if_not_exists` formats the queue table name then creates
`USING GIN (headers)` with IF NOT EXISTS. Both public wrappers delegate to it; the all-queues
wrapper loops `pgmq.meta`, including standard, unlogged and partitioned queues. An override
therefore must handle partitioned parent/child indexes as well as ordinary tables.

All three grouped reads aggregate the expression
`COALESCE(headers->>'x-pgmq-group', '_default_fifo_group')`. `read_grouped` filters visibility
before MIN(msg_id), rejects earlier invisible rows, and has a selective per-group LATERAL
query with quantity limit. Round-robin aggregates absolute heads and joins back by group;
head reads aggregate absolute heads and join by msg_id, with no equivalent group-key join.
They are different query shapes. An index useful for the LATERAL probe may not improve head
aggregation. Existing msg_id or vt indexes can still participate when the GIN is useless.

`pgmq-hasql/src/Pgmq/Hasql/Statements/QueueObservability.hs` implements
`listFifoIndexQueueNames :: Statement () [Text]` using pg_indexes and the name suffix.
`pgmq-config/src/Pgmq/Config/Reconcile.hs` calls creation only when the name is absent.
`pgmq-config/test/ConfigSpec.hs` already tests CreatedFifoIndex on the first run and
SkippedFifoIndex on the second. Its EphemeralDb and the hasql fixtures use native migrations;
passing them alone does not prove stock-extension compatibility. Add isolated stock tagged
SQL fixtures or reuse the existing compatibility fixtures, without editing shared server
functions during unrelated parallel tests.

The [compatibility ADR](../adr/pgmq-1.12-1.13-compatibility.md) requires client support for
stock 1.12/1.13 and serial migration testing. The
[vendoring policy](../design/012-vendor-upstream-pgmq-sql.md) requires append-only local
overrides. The [reconciliation contract](../design/018-reconciliation-contract.md) reports
presence and avoids resource conversion. The
[FIFO boundary ADR](../adr/fifo-native-overrides-and-index-upgrade-boundary.md) preserves that
policy while allowing explicit operator index repair. Documentation of measured behavior
belongs to `docs/plans/21-state-the-fifo-ordering-and-partitioned-retention-contracts-truthfully.md`.

## Plan of Work


### M1: Measure useful work, including the actual modifying queries


Create a self-contained SQL experiment at `pgmq-migration/test/fixtures/fifo-index-probe.sql`.
It must take a scratch database with the native schema, create uniquely named probe queues,
seed deterministic messages/groups using generate_series, ANALYZE, and contain executable
EXPLAIN statements without placeholders. Include roughly 100,000 rows with hundreds of uneven
groups, a few very large groups, many small groups, default-group headers, and visible/invisible
head variations. Try small and larger batch sizes. Record PostgreSQL version and settings.

Compare no FIFO index, the existing GIN, and these candidates, with one candidate present
at a time and the same data/statistics:

```sql
CREATE INDEX q_fifo_probe_candidate ON pgmq.q_fifo_probe
  ((COALESCE(headers->>'x-pgmq-group', '_default_fifo_group')), msg_id);
-- Alternative definitions, tested separately under the same name:
-- ((COALESCE(headers->>'x-pgmq-group', '_default_fifo_group')), msg_id) INCLUDE (vt)
-- ((COALESCE(headers->>'x-pgmq-group', '_default_fifo_group')), vt, msg_id)
```

Copy the exact inner SQL for each of read_grouped, read_grouped_rr and read_grouped_head,
substituting safe fixture identifiers, quantity and visibility interval. Run EXPLAIN
(ANALYZE, BUFFERS, FORMAT JSON) on the complete modifying query and separately on useful
subqueries to explain its behavior. EXPLAIN around only the PL/pgSQL function call normally
shows a Function Scan, not the nested plan, so that alone is insufficient evidence.
Wrap each modifying measurement in BEGIN/ROLLBACK so it does not consume the next run's
messages. Also measure the actual function call as end-to-end latency evidence. Reset fixtures
or perform equivalent warm-up runs to avoid presenting cache order as an index benefit.

Keep planner defaults for acceptance; disabling sequential scans is diagnostic only. Record
buffers, rows scanned, elapsed time across repeated runs, index size, heap fetches and send/read
write cost. A GroupAggregate over every index entry is still full-backlog work. Select a
candidate only if repeated complete-query measurements show a useful benefit for a stated
workload and disclose regressions/limits on the other reads. Do not assert universal speedup
or constant/logarithmic polling cost. If none qualifies, record the negative result and revise
scope per the Decision Log; M2's replacement is then omitted rather than pretended complete.

### M2: Provisioning and explicit legacy repair


After EP-1 is integrated and M1 justifies a candidate, append 0008 to the manifest. Re-create
only `_create_fifo_index_if_not_exists` under its exact signature. Acquire the existing
per-queue advisory lock before inspecting/building, then recognize three states: absent index,
exact supported new index, and exact upstream legacy GIN. Build when absent; do nothing for
the supported definition; replace the exact legacy definition only when this function is
explicitly called. Do not perform a loop over existing queues during migration. Check catalog
identity, parent relation, access method, keys/expression, validity and absence of unexpected
predicates/options before deciding an object is an eligible legacy replacement. Reject an
unexpected conflicting object with a useful error rather than dropping a DBA's different index.
Use existing identifier quoting/queue-name formatting and never interpolate raw identifiers.

Test no-index creation, exact legacy upgrade, stable index identity on repeated calls,
rollback on an injected build failure and two concurrent create/upgrade calls. A transaction
must restore the old index if replacement fails. Test standard, unlogged and real pg_partman
partitioned queues, checking parent and attached child indexes and a newly created partition.
Exercise both public creation wrappers; there is no third FIFO queue-creation wrapper.
The all-queues operation can acquire locks across a large topology and is an explicit operator
choice, not a migration or startup default.

Extend testNativeComponent's names and latest-only convergence exceptions with
`body:_create_fifo_index_if_not_exists(text)`. The 1.12 checkpoint and original six-entry
payloads remain unchanged. EP-1 already made latest convergence consume all manifest entries.
Acceptance requires function signatures/results still compared, the new definition verified,
and functional grouped-read results identical to EP-1's baseline.

### M3: Existence reporting and operator upgrade


Preserve the name-based listFifoIndexQueueNames API and reconciliation engine. Update its
Haddock if needed to state explicitly that presence does not validate definition or performance.
Test native no-index → CreatedFifoIndex, native new-index → SkippedFifoIndex, and native
legacy-index → SkippedFifoIndex with unchanged definition. Explicitly call create_fifo_index
on that legacy native queue, verify physical replacement, and reconcile again to observe a
skip. Run two reconciliations on stock 1.12 and 1.13: first creates the server's GIN and
second skips, without replacement, repeated creation reports or reliance on a native-only
function. Preserve the public action constructors and statement result meaning.

Write the tested operator procedure in this plan for EP-3 to publish: inspect exact catalog
state and server installation mode, choose a maintenance window, apply the native migration,
explicitly call create_fifo_index for the chosen queue, then verify the definition and read
behavior. Document that applying the migration alone leaves legacy indexes unchanged.
Measure lock duration. CREATE INDEX CONCURRENTLY is not usable inside this transactional
function; an external online procedure, especially for partitioned parents, must be separately
validated before it is offered. Do not improvise a supposedly concurrent parent-index upgrade.

### M4: Evidence handoff and upstream artifact


If SQL shipped, create `docs/upstream-patches/fifo-index.patch` against a verified upstream
base discovered through Mori. Include the helper change, FIFO tests and prospective upgrade
integration; do not modify pristine local vendor files or overwrite a released upstream upgrade
script. Record base commit and `git apply --check` results from an isolated checkout. Do not
submit it. Add root CHANGELOG.md and pgmq-migration/CHANGELOG.md Unreleased entries; add a
pgmq-hasql/CHANGELOG.md entry only if its public documentation changes. Hand measured benefits,
limitations, migration/explicit-upgrade commands and native/stock distinctions to EP-3.
Do not claim a published version or select new bounds without Hackage/tag verification.

## Concrete Steps


Run from the repository root. The probe file is produced in M1 before its command is run.
Use a freshly created disposable database, never a production PGDATABASE default.

```bash
cat pgmq-migration/migrations/manifest
rg -n '_create_fifo_index|create_fifo_indexes_all' vendor/pgmq/pgmq-extension/sql/pgmq.sql
rg -n 'listFifoIndexQueueNames|SkippedFifoIndex' pgmq-hasql/src pgmq-config/src pgmq-config/test
# After writing M1's self-contained fixture and setting a disposable database URL:
psql -X -v ON_ERROR_STOP=1 "$FIFO_PROBE_DATABASE_URL" -f pgmq-migration/test/fixtures/fifo-index-probe.sql
# After implementation:
cabal build all
nix develop .#partman --command cabal test pgmq-migration:pgmq-migration-test --test-options='-j1' --test-show-details=direct
nix develop .#partman --command cabal test pgmq-hasql:pgmq-hasql-test pgmq-config:pgmq-config-test --test-options='-j1' --test-show-details=direct
```

Expected: probe reports actual nested query nodes and measurements; suites exit zero,
partition cases do not skip, repeat creation preserves the supported index, and the stock
compatibility matrix converges. Retain actual output here; no fabricated expected EXPLAIN.
Use Mori to locate dependency APIs before implementing unfamiliar test/catalog helpers.

## Validation and Acceptance


The performance decision must be reproducible from the checked-in SQL fixture and retained
measurements under planner defaults. An index node alone is insufficient. If replacement is
selected, both wrappers provision the measured definition; explicit legacy repair preserves
queue data, supports partitioned/unlogged queues, rolls back safely and is idempotent under
retry. Reconciliation reports existence consistently on native legacy, native new and stock
servers. Historical/latest convergence and EP-1 ordering regressions pass. Operators can tell
whether SQL is native and whether the physical index was upgraded. A negative experiment
requires an explicit parent scope update before this child is marked complete.

## Idempotence and Recovery


Migration 0008 changes the helper, not every existing index. Native ledger history prevents
reapplication and must never be rewritten after application. Physical replacement is atomic
inside the explicit call; failed builds retain the old index on rollback. The queue advisory
lock serializes cooperating callers; unexpected independent DBA DDL may still cause a safe
failure and retry. Reject malformed/conflicting indexes rather than deleting them.

Run experiments only on disposable databases and reset lease/data/statistics conditions
between candidates. If 0008 has been occupied, coordinate a new separate filename with the
parent. Do not merge into 0007. An unacceptable lock cost is a documented operational limit
or a reason to reject the candidate, not permission to rebuild queues automatically.

## Interfaces and Dependencies


Keep the three SQL signatures `_create_fifo_index_if_not_exists(text)`,
`create_fifo_index(text)` and `create_fifo_indexes_all()` returning void. Keep
`withFifoIndex :: QueueConfig -> QueueConfig`, QueueConfig.fifoIndex and
`listFifoIndexQueueNames :: Statement () [Text]` unchanged in type and existence meaning.
Own the probe fixture, optional 0008, manifest/test extensions, focused native/stock index
and reconciliation tests, changelog entries and optional upstream patch. No new public
server capability probe or Haskell dependency is introduced. EP-3 owns publication of the
operator prose and final design/ADR integration; EP-1 owns grouped-read algorithm bodies.

Revision note (2026-09-12): Replaced assumed index-only speedups with a full-query experiment,
removed nonexistent create_fifo, preserved stock-server existence reporting, specified explicit
legacy repair and partition/idempotence tests, and separated ledger ownership from EP-1.
