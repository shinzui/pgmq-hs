---
id: 12
slug: expose-grouped-reads-on-the-umbrella-api-and-release-0-5-0-0
title: "Expose the complete API and prepare the 0.6.0.0 release"
kind: exec-plan
created_at: 2026-07-14T14:55:11Z
intention: "intention_01kxgh9geke2dayhx57qp6g9ye"
master_plan: "docs/masterplans/2-support-pgmq-1-12-0-grouped-head-reads.md"
provenance:
  revisions:
    - model: "unknown"
      harness: "codex"
      at: 2026-09-10T16:47:52Z
      mode: "update"
      note: "Refresh for released PGMQ 1.12/1.13, partition controls and metrics, safe native upgrades, and the 0.6.0.0 release."
---
# Expose the complete API and prepare the 0.6.0.0 release


This ExecPlan is a living document. Keep Progress, Surprises & Discoveries, Decision Log,
and Outcomes & Retrospective current. Its historical filename remains stable.


## Purpose / Big Picture


Deliver a coherent public release of PGMQ 1.12 grouped heads and 1.13 partition controls and
metrics. A user importing only Pgmq or Pgmq.Effectful can call all six grouped reads and the
explicit-premake creation operation, construct their argument records, and inspect nullable
default-partition metrics. Declarative configuration can set premake on newly created queues.

All five packages already released 0.5.0.0 with MasterPlans 3 and 4's hardening/reconciliation
changes. This initiative prepares **0.6.0.0**, retaining those changes and describing only new
behavior in new changelog entries. QueueMetrics, PartitionConfig and ReconcileOps record
extensions require source migration guidance. The previous plan to release 0.5.0.0 is obsolete.

Completion includes public API compile tests, version/partition validation, operator guidance
and builds of the existing in-scope consumers. A release here is a committed repository
candidate with coherent version/bounds; Hackage upload remains a separate workflow.

Hard prerequisites are [EP-9](9-vendor-pgmq-1-12-0-and-add-the-native-schema-migration.md),
[EP-10](10-add-grouped-head-read-statements-and-sessions-to-pgmq-hasql.md), and
[EP-11](11-add-grouped-head-read-effects-and-traced-spans-to-pgmq-effectful.md).
Completed MasterPlans 3 and 4 are protected baselines, not work to repeat.


## Progress


- [ ] Milestone 1: umbrella modules export the complete grouped/premake API and compile-only tests prove reachability.
- [ ] Milestone 2: all five library versions and family bounds move to 0.6.0.0; new changelog entries describe the real changes.
- [ ] Milestone 3: operator/API documentation explains both server versions, metrics estimates and partition recovery.
- [ ] Milestone 4: required version/partition matrix and source distributions pass; in-scope consumers build against the candidate.
- [ ] Milestone 5: release candidate committed with complete evidence and unchanged published history.


## Surprises & Discoveries


On September 10, all five Hackage preferred-version endpoints list 0.5.0.0 as the latest
normal release. Upstream pgmq-hs tag v0.5.0.0 resolves to
`fc13d7a432dbc0cf0ad4cd3616e5d7d28fdf5abe`. The root changelog explicitly says grouped-head
support was excluded from that release. Current Cabal versions are 0.5.0.0.

The SQL 1.13 release adds premake and default-partition metrics even though its release-note
heading describes a partition bug fix. The native upgrade must preserve the existing local
re-entry override after upstream changes its function identity.

The old consumer instructions assumed 0.4 pins and an unhandled Maybe Message result.
Re-discover current consumers and source before editing: those migrations may already have
shipped with 0.5.0.0. Do not automatically repeat them or rewrite the published change history.


## Decision Log


Retain the July 14 all-six umbrella export decision and the single release-owner decision.
On 2026-09-10, move this plan to 0.6.0.0 and remove completed hardening plans as pending gates.
The new records are source changes, so the coordinated release remains breaking.

On 2026-09-10, make the compatibility matrix explicit: grouped reads and legacy partition
creation work on both 1.12 and 1.13; explicit premake requires 1.13; metrics on 1.12 return
Nothing for the unavailable new field. Native installs always reach the full 1.13 ledger.

On 2026-09-10, require real pg_partman acceptance, not successful skipped tests. Existing rollout
scope stays `mori://shinzui/keiro` and `mori://shinzui/shibuya-pgmq-adapter`; refresh every
direct consumer in Mori and record other projects at their actual retained version.

See [the compatibility ADR](../adr/pgmq-1.12-1.13-compatibility.md) for the durable choices.


## Outcomes & Retrospective


Not implemented. At completion record the candidate SHA, package versions, test commands and
counts, PostgreSQL/pg_partman versions, upstream tag identities, source-distribution results,
and a consumer matrix with exact tested or intentionally retained versions. Distinguish a
repository candidate from a Hackage-published release.


## Context and Orientation


Work from the repository root inside `nix develop`. EP-9 supplies the pg_partman-capable
environment and test-only schema selection. Verify dependency bounds against authoritative
registries/tags after discovering their source with Mori; local corpus versions can lag.

`pgmq-hasql/src/Pgmq.hs` is the direct umbrella. `pgmq-effectful/src/Pgmq/Effectful.hs` is
the effect umbrella. Neither currently exports the four existing grouped reads. EP-10/11 add
two more and explicit premake below those umbrellas; this plan owns the final re-exports.

The library family consists of pgmq-core, pgmq-hasql, pgmq-effectful, pgmq-migration and
pgmq-config. Each has a Cabal file and CHANGELOG.md; the root CHANGELOG.md aggregates them.
`pgmq-bench` is unpublished and keeps its independent version, though its dependency bounds
or source construction sites may need changes.

Current native migrations are 0001 baseline, 0002 canary and 0003 local notification/partition
hardening. EP-9 appends upstream 1.12, upstream 1.13 and local four-argument re-entry
preservation; derive the final names from the manifest. `pgmqV1_11StateValidator` and the
original ledger bytes are deliberately immutable because they describe imported predecessor
state, not the final target.

The relevant local decisions are [vendoring](../design/012-vendor-upstream-pgmq-sql.md),
[NULL semantics](../design/014-null-parameter-contract.md),
[notification re-entry](../design/015-notification-delivery-contract.md),
[reconciliation](../design/018-reconciliation-contract.md), and
[the new compatibility ADR](../adr/pgmq-1.12-1.13-compatibility.md).
The upstream source is `mori://pgmq/pgmq`, with v1.12.0/v1.13.0 SHAs recorded in the MasterPlan.


## Plan of Work


### Milestone 1 — Complete the public imports


Add a FIFO / Grouped Reads section to both umbrellas exporting readGrouped,
readGroupedWithPoll, readGroupedRoundRobin, readGroupedRoundRobinWithPoll, readGroupedHead
and readGroupedHeadWithPoll, plus ReadGrouped(..) and ReadGroupedWithPoll(..).
Add createPartitionedQueueWithPremake beside the existing partition creation operation.
Existing CreatePartitionedQueue(..) and QueueMetrics(..) exports must expose the usable
record constructors and new metric field. Verify config exports include the expanded
PartitionConfig and shared-backend interface where already public.

Write Haddocks explaining the absolute-head rule and visibility leases. Avoid claiming
expiry advances to the next message, or that the algorithm guarantees exactly-once processing.
Explicit premake uses the unchanged three-field creation record plus an Int32; document
the 1.13 floor. Explain metrics Nothing/Just semantics beside QueueMetrics.

Add `pgmq-hasql/test/UmbrellaExportsSpec.hs` importing Pgmq as its only pgmq library import,
and an equivalent effectful test module importing only Pgmq.Effectful. Define typechecked
bindings using all six functions, both grouped records, old/new partition creation and the
new metrics selector. These can be compile-only witnesses; no extra polling sleeps are
needed. Register them in Main and Cabal other-modules so compiling tests actually compiles
the witnesses. External non-pgmq imports for Session/Eff/Int32 are allowed.

Acceptance: both test modules compile, and temporarily removing one export makes the witness
fail to compile. Restore only that change. Existing direct/internal imports continue working.


### Milestone 2 — Version and document the source changes


Move all five libraries from 0.5.0.0 to 0.6.0.0 in one coordinated change, after rechecking
that 0.6.0.0 has not independently been released. Update every internal family bound,
including test, example and benchmark components, to the intended 0.6 range. Keep pgmq-bench's
version unchanged. Scan all Cabal files rather than reusing stale line numbers.

Append a new 0.6.0.0 entry to each package changelog and the aggregate, preserving published
0.5.0.0 entries verbatim. pgmq-hasql covers grouped heads, explicit premake, compatible nullable
metrics and umbrella exports. pgmq-effectful covers the new operations and tracing; pgmq-config
covers optional creation-time premake and the ReconcileOps extension. pgmq-migration covers
both upstream upgrades, identity conversion, metrics type change, separate local re-entry
preservation and unchanged legacy import. pgmq-core records a coordinated bump if unchanged.

Breaking-change guidance must tell users constructing QueueMetrics to supply the new nullable
field, users constructing PartitionConfig to supply premake = Nothing or Just n, and custom
ReconcileOps backends to implement the new explicit creation operation. Existing
CreatePartitionedQueue records need no extra field. Explain 1.12's unavailable metric and the
1.13 requirement for explicit premake. Do not describe the already-published Maybe Message,
queue validation or notification changes as newly introduced here.

Acceptance: `cabal build all --enable-tests --enable-benchmarks` resolves a coherent family,
the new entries accurately match code, and old changelog sections are unchanged.


### Milestone 3 — Publish truthful compatibility and operator guidance in the repository


Update README.md, package READMEs, docs/user/schema-migration.md and
docs/user/queue-configuration.md. EP-9/11 own their technical updates; this plan reconciles
the public story. Native installation reaches 1.13, grouped reads require at least 1.12,
and explicit premake requires 1.13. Preserve meaningful historical mentions of the 1.11 baseline.

Document that premake controls both queue and archive pre-created partitions, defaults to 4,
must be at least 1, and applies only at creation through config. Changing PartitionConfig
for an existing queue does not update its pg_partman settings or check those settings for drift.

Explain that defaultPartitionLength estimates queue-plus-archive spill from planner statistics.
Nothing is unavailable/inapplicable, not zero. Positive values warrant maintenance attention;
zero estimates can lag writes before ANALYZE/autovacuum. Do not promise an exact instantaneous
count or claim upgrading alone drains default partitions.

Include the operator recovery sequence: locate pg_partman's schema, move rows with
partition_data_proc outside a transaction, run maintenance, and ANALYZE default partitions.
Apply the process to both queue and archive as needed. The upgrade's BY DEFAULT identity makes
preserved-ID reinsertion possible; direct producers supplying arbitrary IDs can still desynchronize
the sequence and should use pgmq's send API. This documentation does not authorize running
recovery on production during release preparation.

Reconcile stale vendoring instructions with tagged v1.13.0 and the append-only manifest.
Preserve user changes in CLAUDE.md. Update the existing compatibility ADR with final evidence;
do not create a duplicate historical 1.12-only design note.

Acceptance: examples compile, docs distinguish the two server versions, and no current
instructions promise a 0.5 release or a pre-release upstream pin.


### Milestone 4 — Validate the matrix, packages and consumers


Verify the complete native 1.13 ledger and both tagged schema checkpoints through EP-9's
migration tests. Run the 1.12 compatibility selections for grouped reads, default creation,
metrics and config Nothing. Run explicit-premake success on 1.13 and unsupported-call behavior
on 1.12. The full native 1.13 run with PGMQ_REQUIRE_PARTMAN=1 must execute populated identity
upgrades, queue/archive spill recovery, default/custom premake, local re-entry, metrics and
both config adapters. Record exact executed tests, not only exit status or skip messages.

Build source distributions with `cabal sdist all`. Verify the migration manifest, all referenced
SQL, and pinned test fixtures appear in the relevant tarball. Build/test extracted packages
in a temporary project using their local package set; do not let the development vendor symlink
or an untracked research clone hide missing source files. The default test mode must work from
source distributions; opt-in 1.12 tests need their documented packaged/shared fixture setup.

Refresh consumers:

```bash
mori registry dependents shinzui/pgmq-hs --packages
mori registry show shinzui/keiro --full
mori registry show shinzui/shibuya-pgmq-adapter --full
```

Resolve exact project/package paths with Mori. Within `mori://shinzui/keiro`, inventory all
pgmq bounds and uses in keiro-pgmq and its tests. Within `mori://shinzui/shibuya-pgmq-adapter`,
inventory adapter, example, benchmark and test components. These project-relative paths are
located through the canonical projects rather than hard-coded checkout roots.

Create uncommitted temporary Cabal project overrides incorporating all five candidate packages.
Keep the consumers' current project settings, add this checkout's package paths, and ensure
the build selects these local 0.6 packages rather than Hackage 0.5. Update all in-scope bounds
and source uses affected by the new exported records. Recheck Maybe Message handling, but
change it only if current code still needs migration.

For each consumer, from its resolved root, run:

```bash
cabal --project-file=cabal.project.release-candidate build all --enable-tests --enable-benchmarks
cabal --project-file=cabal.project.release-candidate test all --test-show-details=direct
```

Record all direct consumers found, including those retained at their current published family.
The September 10 inventory reports package-level dependencies in `mori://shinzui/mori`,
`mori://shinzui/shikigami`, `mori://shinzui/rei`, `mori://shinzui/mori-app`,
`mori://shinzui/mori-rei-app`, `mori://tan/mls-service-v2`, and the Shibuya adapter. It also
reports project-level references, which are not necessarily build dependencies. Keiro is
still registered with keiro-pgmq and its Shibuya dependency but absent from that direct
reverse-dependency result; keep its existing rollout scope and inspect the actual Cabal
files. Registry references do not establish current bounds. Do not automatically migrate
additional repositories or force them back to 0.4. Record actual state and any unresolvable
registry entry.

Acceptance: both in-scope consumers build all components and test against the exact candidate,
the required version/partition matrix passes, and source distributions contain their dependencies.


### Milestone 5 — Finish the repository candidate


Format scoped changes, run final verification after version edits, and commit the release
candidate with Conventional Commits and the plan/intention trailers. Record its SHA in the
consumer evidence, ensuring consumer changes refer to a concrete source version.
Do not commit machine-local temporary project overrides. Keep any durable source pin as an
explicit consumer decision rather than silently committing a local path.

Update this plan and the MasterPlan's progress/outcomes, and distill final durable findings
into the compatibility ADR or relevant design note. No Hackage upload or claim of published
0.6.0.0 belongs here.

Acceptance: the candidate commit exists, public API and release notes match it, every required
test result and consumer state is recorded, and no pending prerequisite is hidden.


## Concrete Steps


Run from the repository root in the development environment:

```bash
git status --short
rg '^version:' pgmq-core/pgmq-core.cabal pgmq-hasql/pgmq-hasql.cabal pgmq-effectful/pgmq-effectful.cabal pgmq-migration/pgmq-migration.cabal pgmq-config/pgmq-config.cabal
git ls-remote --tags https://github.com/shinzui/pgmq-hs.git 'v0.5*' 'v0.6*'
cabal build all --enable-tests --enable-benchmarks
cabal test all --test-show-details=direct
PGMQ_REQUIRE_PARTMAN=1 PGMQ_TEST_SCHEMA_VERSION=1.13.0 cabal test all --test-show-details=direct
PGMQ_TEST_SCHEMA_VERSION=1.12.0 cabal test pgmq-hasql --test-options='--pattern "GroupedHead"'
PGMQ_TEST_SCHEMA_VERSION=1.12.0 cabal test pgmq-hasql --test-options='--pattern "Metrics"'
cabal sdist all
git diff --check
```

Use the exact partman environment command and additional compatibility selections recorded by
EP-9/10/11. Review test counts so filtering or missing dependencies cannot turn acceptance into
a no-op. Recheck authoritative Hackage package pages before assigning a release version or
reporting what is published.

The release commit uses these trailers, plus any other plan whose implementation the commit
actually contains:

```text
MasterPlan: docs/masterplans/2-support-pgmq-1-12-0-grouped-head-reads.md
ExecPlan: docs/plans/12-expose-grouped-reads-on-the-umbrella-api-and-release-0-5-0-0.md
Intention: intention_01kxgh9geke2dayhx57qp6g9ye
```

Do not attach completed MasterPlan 3/4 implementation trailers merely because their already-
released behavior is regression-tested.


## Validation and Acceptance


Users can import the two umbrellas and reach all six grouped reads, explicit premake, the
unchanged legacy creation record and nullable default-partition metrics. Config examples
compile and describe the true creation-only behavior. The three public record changes have
concrete source migration examples.

All five libraries have consistent 0.6.0.0 versions/bounds, while pgmq-bench retains its own
version. Historical release entries are preserved. The 1.12/1.13 client matrix, native upgrade
matrix, mandatory real pg_partman tests and source-distribution checks pass. Both existing
in-scope consumers build/test against the candidate, and every other discovered consumer has
an explicit actual retained state. Hackage publishing is accurately left pending.


## Idempotence and Recovery


Before committing, repository version/doc edits can be corrected and revalidated while
preserving unrelated work. Existing migration payloads must never be rewritten, even if a
release fails. Use new forward migrations for deployed corrections. Temporary consumer
project overrides are local validation artifacts; remove only those created for this task
after the evidence is captured. Never reset a whole repository to recover a release attempt.

Tests use disposable databases. Required-partman failure is a real release gate. Stock
extension fixtures do not inherit local native hardening; keep those expectations separate.
Once the candidate is committed, corrections use follow-up commits. Publishing remains outside
this plan.


## Interfaces and Dependencies


EP-12 exports the session/effect names and argument/result types owned by EP-10/11, without
renaming them. It owns both umbrella modules, their compile witnesses and registration, family
version/bound edits, all six changelogs, final documentation integration and in-scope consumer
source/bound changes. EP-9 owns migration provenance and test infrastructure; EP-10/11 own
feature behavior. No new production dependency is introduced by the release plan.


## Revision Note


2026-09-10: Retargeted the obsolete 0.5.0.0 release to 0.6.0.0 after checking Hackage and
upstream tags. Removed already-completed hardening gates, added all 1.13 APIs/record migrations,
required real partition and source-distribution validation, and replaced stale consumer bounds
with current discovery and candidate-based verification. Kept the existing plan path stable.
