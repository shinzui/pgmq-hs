---
id: 11
slug: add-grouped-head-read-effects-and-traced-spans-to-pgmq-effectful
title: "Expose grouped heads and partition controls through effects and configuration"
kind: exec-plan
created_at: 2026-07-14T14:55:11Z
intention: "intention_01kxgh9geke2dayhx57qp6g9ye"
master_plan: "docs/masterplans/2-support-pgmq-1-12-0-grouped-head-reads.md"
provenance:
  revisions:
    - model: "unknown"
      harness: "codex"
      at: 2026-09-10T16:47:51Z
      mode: "update"
      note: "Refresh for released PGMQ 1.12/1.13, partition controls and metrics, safe native upgrades, and the 0.6.0.0 release."
---
# Expose grouped heads and partition controls through effects and configuration


This ExecPlan is a living document. Keep Progress, Surprises & Discoveries, Decision Log,
and Outcomes & Retrospective current.


## Purpose / Big Picture


Give effectful and declarative clients the complete PGMQ 1.12/1.13 capability added by
[EP-10](10-add-grouped-head-read-statements-and-sessions-to-pgmq-hasql.md). A caller can
read one absolute head per FIFO group, use its polling variant, create a partitioned queue
with an explicit premake count, and receive nullable default-partition estimates through
either interpreter. A startup queue configuration can request premake for newly created queues.

An effect is a description of an operation whose interpreter decides how to run it. Here both
interpreters use pgmq-hasql; the traced one also emits OpenTelemetry spans. Declarative queue
configuration is implemented by one shared reconciliation algorithm parameterized by operations,
with direct and effectful adapters. Extend those existing boundaries.

Success means the same configuration creates both pg_partman parents with the requested count
through either adapter, a later reconcile leaves existing partition settings alone, grouped
operations emit correctly labelled Consumer spans, and metrics preserve Nothing/Just results.


## Progress


- [ ] Milestone 1: grouped-head and explicit-premake effects added and wired through both interpreters.
- [ ] Milestone 2: PartitionConfig and the shared reconciler carry optional creation-time premake through both adapters.
- [ ] Milestone 3: plain/traced operations, nullable metrics, and config creation/skip semantics verified on the version matrix.
- [ ] Milestone 4: docs and compatibility notes complete; all high-level package tests pass.


## Surprises & Discoveries


MasterPlan 4 has already extracted `Pgmq.Config.Reconcile` and shipped it in 0.5.0.0.
Adding premake separately to the two adapters would duplicate policy and miss the shared
`ReconcileOps` contract. Its creation-only boundary must survive this extension.

The existing traced partition arm positionally matches the three-field CreatePartitionedQueue.
EP-10 deliberately preserves that record, so its arity does not change. The new operation
carries the explicit Int32 separately. Metrics effects already return the shared QueueMetrics;
they need no new GADT constructor to expose the appended field.

The former plan skipped polling trace tests on timing grounds. A message already available
makes that operation return promptly, so both grouped SQL labels can be tested cheaply.


## Decision Log


Retain July 14's reuse of grouped parameter types, existing receiveOp convention, and EP-12
ownership of umbrella exports. On 2026-09-10, add the explicit premake operation symmetrically
to both interpreters and propagate the expanded metrics result unchanged.

On 2026-09-10, add `premake :: Maybe Int32` to PartitionConfig. Nothing calls the legacy
three-argument operation; Just n calls the explicit 1.13 operation. No optional SQL NULL is
bound, no custom value is silently discarded, and existing queue settings are never changed
or falsely reported as checked. Extend the shared engine and both adapters together.

The durable boundary is documented in [the compatibility ADR](../adr/pgmq-1.12-1.13-compatibility.md)
and [the reconciliation contract](../design/018-reconciliation-contract.md). The record changes
are part of the new 0.6.0.0 release.


## Outcomes & Retrospective


Not implemented. Record executed interpreter/config tests, exact pg_partman environment,
version compatibility results, and source migration examples when complete.


## Context and Orientation


Run from the repository root inside `nix develop` and EP-9's required-partman environment.
This plan starts only after EP-10 has implemented these session names:

```haskell
readGroupedHead :: ReadGrouped -> Session (Vector Message)
readGroupedHeadWithPoll :: ReadGroupedWithPoll -> Session (Vector Message)
createPartitionedQueueWithPremake :: CreatePartitionedQueue -> Int32 -> Session ()
```

ReadGrouped has queueName, visibilityTimeout and qty; ReadGroupedWithPoll adds maxPollSeconds
and pollIntervalMs. CreatePartitionedQueue retains queueName, partitionInterval and
retentionInterval. QueueMetrics now includes `defaultPartitionLength :: Maybe Int64`.

`pgmq-effectful/src/Pgmq/Effectful/Effect.hs` owns the Pgmq GADT and smart constructors.
`Interpreter.hs` runs the sessions. `Interpreter/Traced.hs` uses `withTracedOp` and operation
descriptors; grouped reads use `receiveOp` with Consumer kind. Span names are
`receive <queue>` while the SQL function name is emitted in `db.operation` and
`db.operation.name`. Partition creation follows the existing `pgmq.create_partitioned`
operation descriptor. `Telemetry.hs` and `Traced.hs` handle propagation/attribute utilities
and need no new keys for this work.

`pgmq-effectful/test/PlainInterpreterSpec.hs` and `TracedInterpreterSpec.hs` test the two
interpreters. The traced test helpers include setupTracer, mkUniqueQueue, withSemconvOptIn,
singleSpan, assertAttrText and assertSpanKindConsumer; use the existing error-handling wrapper
and current test structure rather than copying stale line-number snippets.

`pgmq-config/src/Pgmq/Config/Types.hs` defines PartitionConfig and its two existing Text
fields. `Config/Reconcile.hs` defines ReconcileOps and the only reconciliation algorithm.
`Config.hs` supplies the direct operations; `Config/Effectful.hs` supplies the effectful
ones. In `reconcileQueue`'s missing-queue branch the engine constructs CreatePartitionedQueue
from PartitionConfig. Existing queues are handled before that branch and skipped when their
three-way shape agrees. Partition interval and retention settings are not observed or repaired;
premake follows that same boundary.

`pgmq-config/test/ConfigSpec.hs` and the existing fake/backend tests are the natural test sites.
Find all PartitionConfig and ReconcileOps constructions, including tests, examples and benchmarks.
[NULL semantics](../design/014-null-parameter-contract.md) explains why Nothing cannot simply
be passed as SQL NULL; [reconciliation](../design/018-reconciliation-contract.md) explains why
premake must not add an unadvertised update to an existing queue.


## Plan of Work


### Milestone 1 — Effects and both interpreters


Add exports and these constructors/smart constructors in Effect.hs:

```haskell
-- Constructors inside Pgmq:
ReadGroupedHead :: ReadGrouped -> Pgmq m (Vector Message)
ReadGroupedHeadWithPoll :: ReadGroupedWithPoll -> Pgmq m (Vector Message)
CreatePartitionedQueueWithPremake :: CreatePartitionedQueue -> Int32 -> Pgmq m ()

-- Smart constructors:
readGroupedHead :: (Pgmq :> es) => ReadGrouped -> Eff es (Vector Message)
readGroupedHeadWithPoll :: (Pgmq :> es) => ReadGroupedWithPoll -> Eff es (Vector Message)
createPartitionedQueueWithPremake :: (Pgmq :> es) => CreatePartitionedQueue -> Int32 -> Eff es ()
```

Use send with the matching constructor. Plain arms delegate to the exact EP-10 sessions.
Traced grouped arms call receiveOp with `pgmq.read_grouped_head` and
`pgmq.read_grouped_head_with_poll`, using the queue name from the existing argument record.
The explicit-premake arm delegates to the new session and reuses the original partition
creation descriptor and SQL label. No new metric effects or operation labels are needed.

Keep the old createPartitionedQueue effect and its three-field match unchanged. Exhaustiveness
warnings identify missing arms, but are not necessarily compilation errors unless warnings
are promoted; finish both interpreters before recording a clean milestone. Add Haddocks for
the version floor, head leases, polling connection occupation, explicit premake and NULL metric
meaning. A long polling span accurately records its wait.

Acceptance: pgmq-effectful builds without incomplete-pattern warnings, all existing effect
names remain, and both interpreters execute the new operations.


### Milestone 2 — Carry premake through the shared reconciler


Append `premake :: !(Maybe Int32)` to PartitionConfig in Config/Types.hs. Keep the existing
partitionedQueue smart constructor shape, which takes that record. Update all repository
construction examples to set Nothing unless they deliberately request a custom count.

Extend ReconcileOps with
`createPartitionedQueueWithPremake :: StmtTypes.CreatePartitionedQueue -> Int32 -> m ()`.
Wire it in Config.hs to the direct session and in Config/Effectful.hs to the new effect.
In the shared missing-partitioned-queue branch, construct the same three-field query as
before; Nothing uses the existing operation, Just n the new one. Do not bind a nullable
premake parameter or perform a version probe on every call. An explicit option on 1.12
remains an unsupported-operation database error; omitting it stays compatible.

Do not change the existing-queue branch, query pg_partman's settings in production, add
premake drift actions, or reconfigure existing partitions. Document the scope explicitly.
A new config field means source code constructing PartitionConfig must be migrated; a new
ReconcileOps field means custom backends must implement it. Record both for EP-12.

Acceptance: pgmq-config builds through both adapters and the shared engine has one dispatch
decision. Existing queue reconciliation issues no creation call when only premake differs.


### Milestone 3 — Verify public behavior, not just exhaustive matches


In PlainInterpreterSpec, execute both grouped functions and explicit premake and assert their
results or parent settings; propagate invalid premake as the existing PgmqRuntimeError rather
than swallowing it. Test metrics and allQueueMetrics retain Nothing for ordinary queues and
Just values for 1.13 partitioned queues; repeat the available metrics/grouped selections on
the 1.12 fixture.

In TracedInterpreterSpec, seed available grouped messages and test both grouped variants
without waiting out a long timeout. Assert returned IDs, span name receive <queue>, Consumer
kind, queue attribute, and both db.operation keys under the semantic-convention mode that
emits them. Match the correct SQL label for each variant. Test explicit premake through the
traced interpreter with real pg_partman, asserting the existing creation span convention.
Ensure metrics retain the same nullable result under tracing. A deliberate wrong label must
fail its assertion.

For config, exercise both adapters with a new queue and premake Nothing, then another with
Just 2. Real pg_partman must report the effective setting on queue and archive parents.
Reconcile the existing queue again after changing only the requested premake: expect
SkippedQueue, unchanged parent settings and no create call. A shared-engine fake/backend
test can assert exact operation dispatch, but cannot replace the database proof.
Explicit zero/negative counts fail on new queues; they are not silently changed to 4.
On 1.12, Nothing succeeds and Just fails clearly. Preserve other reconciliation behavior,
including foreign queues, FIFO reporting, notify crash recovery and throttle drift.

Acceptance: the high-level suites pass, including a required-partman run with actual execution,
and both adapters have observable tests for creation-only semantics.


### Milestone 4 — Documentation and full verification


Update high-level package README/examples and `docs/user/queue-configuration.md` to show
PartitionConfig with an explicit premake field, explain Nothing versus Just, version requirements,
and the creation-only boundary. Update design note 018's deliberate non-checks to include
premake while preserving its existing architectural rationale. Use the new ADR for the
cross-layer compatibility decision. Leave final changelogs, versions and umbrellas to EP-12.

Acceptance: all family packages compile, pgmq-effectful and pgmq-config suites pass, required
partition tests run, and docs do not imply existing queue settings are reconciled.


## Concrete Steps


Run from the repository root in the development environment:

```bash
rg -n 'readGroupedHead|createPartitionedQueueWithPremake' pgmq-hasql/src/Pgmq/Hasql/Sessions.hs
rg -n 'PartitionConfig|ReconcileOps' pgmq-config pgmq-bench docs/user
cabal build pgmq-effectful pgmq-config
cabal test pgmq-effectful pgmq-config --test-show-details=direct
PGMQ_TEST_SCHEMA_VERSION=1.12.0 cabal test pgmq-effectful --test-options='--pattern "GroupedHead"'
PGMQ_REQUIRE_PARTMAN=1 PGMQ_TEST_SCHEMA_VERSION=1.13.0 cabal test pgmq-effectful pgmq-config --test-show-details=direct
cabal build all
git diff --check
```

Use the exact pg_partman environment entry command recorded by EP-9. Name a 1.12-compatible
metrics/config test group and record its executed command/count. Do not run native-only
crash/re-entry assertions on stock upstream fixtures and mistake their expected differences
for regressions. Do not accept a zero-test pattern match as verification.

Format and commit scoped files with a Conventional Commit and these trailers:

```text
MasterPlan: docs/masterplans/2-support-pgmq-1-12-0-grouped-head-reads.md
ExecPlan: docs/plans/11-add-grouped-head-read-effects-and-traced-spans-to-pgmq-effectful.md
Intention: intention_01kxgh9geke2dayhx57qp6g9ye
```


## Validation and Acceptance


Both interpreters handle all three new operations, and telemetry labels agree with the invoked
SQL functions. Grouped-head polling has a trace assertion with available messages. Existing
metrics effects preserve the expanded nullable result.

Both config adapters pass Nothing through the old creation operation and Just through explicit
premake on new queues. Repeating reconciliation or changing premake on an existing queue
leaves settings intact and reports SkippedQueue. The required-partman run proves the configured
value reached both parent tables. Updated examples compile with the new record fields.


## Idempotence and Recovery


Use isolated queues and EP-9's disposable/versioned fixtures. Partial config execution retains
its established retry behavior: completed operations remain, later runs skip existing queues.
Do not introduce automatic repair of failed creation or changes to existing pg_partman settings.
Restore only intentional test mutations; preserve unrelated edits. Missing extension support
must fail required mode. Diagnose missing session names as an incomplete EP-10 handoff.


## Interfaces and Dependencies


EP-10 owns ReadGrouped, ReadGroupedWithPoll, unchanged CreatePartitionedQueue, expanded
QueueMetrics and the session signatures. EP-11 owns the new effect operations, optional
PartitionConfig field, ReconcileOps extension, adapters and high-level tests. EP-12 owns
umbrella re-exports of all grouped functions, explicit premake and the updated types.

No new production library dependency is expected. Use the existing effectful, Hasql,
OpenTelemetry and test machinery; consult Mori sources before changing dependency APIs.


## Revision Note


2026-09-10: Expanded effects work to explicit premake and nullable metrics, added declarative
premake through the already-extracted shared reconciler, and replaced stale build/tracing
assumptions with real interpreter, version and partition acceptance. Preserved the existing
creation API and creation-only reconciliation boundary.
