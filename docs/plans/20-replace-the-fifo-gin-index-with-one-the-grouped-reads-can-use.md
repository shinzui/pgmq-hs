---
id: 20
slug: replace-the-fifo-gin-index-with-one-the-grouped-reads-can-use
title: "Measure and document a supplemental FIFO index"
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
    - model: "gpt-6-astra"
      harness: "codex-cli"
      at: 2026-09-12T15:42:32Z
      mode: "update"
      note: "Added upstream-first maintenance gate and focused drift, successor-upgrade and retirement acceptance for proposed FIFO overrides."
    - model: "gpt-6-astra"
      harness: "codex-cli"
      at: 2026-09-12T15:49:24Z
      mode: "update"
      note: "Applied user prohibition of extension SQL overrides; scoped work to client ordering, additive operator index and documentation; removed override machinery."
  reviews:
    - model: "gpt-6-astra"
      harness: "codex-cli"
      at: 2026-09-12T15:04:18Z
      verdict: "comments"
      note: "Repository review findings applied in this revision; SQL regressions and performance measurements remain explicit implementation acceptance work."
---

# Measure and document a supplemental FIFO index

This ExecPlan is a living document. Its historical filename does not authorize replacing
upstream's index or functions.

## Purpose / Big Picture


Determine whether an optional expression index improves actual grouped reads and, if so,
provide an explicit create/inspect/remove procedure for an independently named index.
Adding an index is allowed; overriding extension SQL is prohibited. Keep upstream's GIN,
create_fifo_index helpers and reconciliation behavior unchanged.

A supplemental index is less coupled than copied function bodies: upstream query changes may
make it less useful without replacing newer upstream behavior. It still consumes storage and
write work and depends on table columns, so measure and document those limits honestly.

## Progress


- [ ] M1: Reproducible no-index/GIN/GIN-plus-candidate measurements for real grouped reads.
- [ ] M2: If useful, tested explicit operator procedure for separately named supplemental indexes, including partitioned queues.
- [ ] M3: Record measured limits, upstream coexistence and documentation handoff, or a negative result with no recommended index.

## Surprises & Discoveries


The upstream helper creates GIN(headers), while grouped reads extract a group expression.
This mismatch does not prove every query stage is a sequential scan or that a btree removes
full-backlog aggregation. There is no pgmq.create_fifo function. Existing index detection
matches q_<queue>_fifo_idx names and reports their presence, not performance.
Earlier planned helper replacement, legacy GIN upgrade and override maintenance are removed.

## Decision Log


2026-09-12: User permits additive indexing and prohibits extension SQL overrides. Use
q_<queue>_group_lookup_idx for the optional index, distinct from upstream's FIFO suffix.
Do not replace existing GIN, alter helpers, add automatic startup provisioning or change the
meaning of withFifoIndex/listFifoIndexQueueNames. A future public provisioning API is separate
work, not required for a measured operator index recommendation.

## Outcomes & Retrospective


Planning updated; no EXPLAIN evidence or index implementation yet. A negative experiment is
acceptable evidence but must not be called a shipped performance improvement.

## Context and Orientation


`vendor/pgmq/pgmq-extension/sql/pgmq.sql` defines the three grouped reads and FIFO helpers.
Source ownership is mori://pgmq/pgmq/repos/pgmq-upstream; its project-relative
pgmq-extension/sql/pgmq.sql has an artifact-level URI pending. The checkout vendors v1.13.0.
Use Mori before dependency lookup and verify authoritative tags before any future version choice.

All three reads aggregate COALESCE(headers->>'x-pgmq-group', '_default_fifo_group'). Grouped
reads have a selective per-group LATERAL lookup, round-robin joins messages by group, and heads
join absolute minima by msg_id. Benefits differ by query shape. Existing msg_id/vt indexes may
participate independently. Leasing updates vt and read_ct, affecting index/write costs.

`pgmq-hasql/src/Pgmq/Hasql/Statements/QueueObservability.hs` detects only the conventional
FIFO suffix. `pgmq-config/src/Pgmq/Config/Reconcile.hs` creates that index only when absent.
The supplemental name must not match the detector. See the
[reconciliation contract](../design/018-reconciliation-contract.md),
[FIFO boundary ADR](../adr/fifo-native-overrides-and-index-upgrade-boundary.md), and
[compatibility ADR](../adr/pgmq-1.12-1.13-compatibility.md). No migration/manifest/body-exception
change is required. EP-1 client ordering and this index experiment are independent.

## Plan of Work


### M1: Measure the full workload


Create `pgmq-migration/test/fixtures/fifo-index-probe.sql`, an executable experiment on disposable
queues with roughly 100,000 deterministic messages, uneven group sizes, visible/invisible
heads, default-group headers and small/large quantities. Record PostgreSQL version, source
commit, statistics and settings. Compare no FIFO index, stock GIN, and stock GIN plus each
candidate, so acceptance measures the intended coexistence rather than a replacement.
A starting candidate for a fixed scratch queue is:

```sql
CREATE INDEX q_fifo_probe_group_lookup_idx ON pgmq.q_fifo_probe
  ((COALESCE(headers->>'x-pgmq-group', '_default_fifo_group')), msg_id);
```

Try vt as an included column or key only if measurements justify the added write cost. Copy
current inner queries into the scratch experiment for EXPLAIN (ANALYZE, BUFFERS, FORMAT JSON),
label their source, and never install those copies as functions. EXPLAIN of only a PL/pgSQL
call normally hides the nested plan; measure full inner queries and actual call latency.
Use BEGIN/ROLLBACK around modifying probes and comparable warm-up/reset runs so one result
does not consume messages or receive a cache advantage. Keep planner defaults for acceptance.

Record repeated timings, buffers, rows scanned, heap fetches, index size and send/lease costs.
An index scan across every group entry is still full-backlog work. Accept a candidate only
for a stated measured workload; disclose regressions and negative results for other reads.
If no candidate helps sufficiently, finish with evidence and no recommended DDL.

### M2: Explicit additive index ownership


If useful, record runnable create/inspect/remove SQL in this plan for EP-3 to publish. Keep the
supplemental name distinct and quote identifiers correctly. Validate absent/present/conflicting
name states by checking definition and relation ownership; IF NOT EXISTS alone does not validate
an index definition. Never silently drop an unexpected existing object. Repeated execution
must leave the supported supplemental index intact. Removal targets only that explicit index.

Test ordinary, unlogged and partitioned queues with real pg_partman, including child indexes
and a newly created partition. Measure build locks. Do not advertise CREATE INDEX CONCURRENTLY
for a partitioned parent without a tested separate procedure; document a maintenance-window
build if that is the supported path. Record failed-build recovery and any invalid-index cleanup
for a concurrently built ordinary-table index. Avoid a general migration/provisioning framework.

Prove coexistence: compare upstream function definitions before/after, verify the original GIN
remains, exercise both upstream FIFO creation wrappers, and run reconciliation twice. With only
a supplemental index present, reconciliation still creates the conventional index once and
then skips it; the supplemental index must not masquerade as upstream's FIFO index. Verify
grouped-read selected IDs/lease behavior are unchanged. Acceptance is useful additive indexing
with no change to upstream SQL or existing report semantics.

### M3: Handoff and future upstream compatibility


Record the recommendation, exact definition, measured workload, limitations and operator
procedure. EP-3 publishes it in docs/user/queue-configuration.md and docs/user/schema-migration.md.
Explain storage/write costs and that withFifoIndex still provisions upstream's GIN. A future
upstream index or query change may make the supplement redundant: inspect and remeasure, then
explicitly remove only the supplemental index when appropriate. Do not automatically replace
it or replay local function bodies when adopting upstream SQL. Normal compatibility checks
remain responsible for table/column changes; an index cannot be promised immune to all DDL.
No upstream patch, SQL override, native migration or Hackage publication is required.

## Concrete Steps


Run from the repository root. M1 creates the fixture before its execution command. Point the
URL at a newly created disposable database with PGMQ installed, never a production default.

```bash
rg -n 'read_grouped|_create_fifo_index' vendor/pgmq/pgmq-extension/sql/pgmq.sql
psql -X -v ON_ERROR_STOP=1 "$FIFO_PROBE_DATABASE_URL" -f pgmq-migration/test/fixtures/fifo-index-probe.sql
```

Run the same probe in the project partman environment for partition acceptance. If automated
coexistence cases are added to the existing suite, run:

```bash
nix develop .#partman --command cabal test pgmq-config:pgmq-config-test --test-options='-j1' --test-show-details=direct
```

Expected: executable nested-query plans and measurements, successful supported index lifecycle,
unchanged upstream function definitions, and stable conventional-index reports. Record actual
commands/output, including partman availability and negative candidates. No invented transcript.

## Validation and Acceptance


Measurements support any recommendation under planner defaults with GIN still present. A
supplemental index has separate ownership and a tested lifecycle; no extension function/index
is replaced and no convergence exception is added. Creation, upstream coexistence, supported
partition behavior and removal are verified. Negative results remain explicit and do not block
the documentation correction or client-ordering work.

## Idempotence and Recovery


Experiments use disposable queues and reset data/leases between candidates. Operator DDL is
explicit and must inspect conflicting/invalid objects rather than claim IF NOT EXISTS proves
correctness. Recovery/removal concerns only the supplemental index. Existing GIN and historical
migrations remain intact. Never run new index DDL automatically on application startup or as
part of a plain vendor refresh.

## Interfaces and Dependencies


Own the experiment fixture, focused coexistence evidence/tests and operator procedure. All
upstream SQL signatures and Haskell index/reconciliation APIs remain unchanged. EP-3 owns user
prose and final documentation entries. No hard dependency on EP-1 and no new package bounds.

Revision note (2026-09-12): Replaced helper override and GIN replacement with a measured,
separately named additive index and explicit operator ownership, as directed by the user.
