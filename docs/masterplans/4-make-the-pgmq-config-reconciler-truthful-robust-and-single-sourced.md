---
id: 4
slug: make-the-pgmq-config-reconciler-truthful-robust-and-single-sourced
title: "Make the pgmq-config reconciler truthful, robust, and single-sourced"
kind: master-plan
created_at: 2026-08-05T23:41:42Z
intention: "intention_01kz9yszpmejztjbet6k4bvcf7"
---

# Make the pgmq-config reconciler truthful, robust, and single-sourced

This MasterPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.


## Vision & Scope

pgmq-config is the declarative queue-configuration package of the pgmq-hs family: an
application declares its queue topology as Haskell values (`QueueConfig`) and calls
`ensureQueues` (or `ensureQueuesEff`) at startup; the reconciler queries the database's
existing state and issues only the mutating calls needed to create what is missing. The
2026-08 idempotency review of the package (this MasterPlan's origin) confirmed that the
core create-only-what-is-missing behavior is correct and idempotent, and surfaced five
follow-up findings: the package documentation promises more than the reconciler does
("ensure all queues exist with the desired settings" — settings drift is in fact never
reconciled or even reported); the reconciliation report claims `CreatedFifoIndex` on
every run whether or not the index already existed, and the `SkippedFifoIndex`
constructor is dead code; the "safe to call on every application startup" claim is
unqualified even though concurrent multi-replica startup against an extension-installed
pgmq 1.11.0 can fail one replica with SQLSTATE 42710 (this repository's migration `0003`
fixed that race for pgmq-migration installs only); a queue created by a non-Haskell
client whose name fails `parseQueueName` makes `listQueues` decoding — and therefore the
whole reconcile — fail at boot; and the entire reconciler exists twice, once in
`pgmq-config/src/Pgmq/Config.hs` against `Hasql.Session` and once, line-for-line, in
`pgmq-config/src/Pgmq/Config/Effectful.hs` against the `Pgmq` effect, so every fix must
land twice.

After this initiative: the reconciler logic exists exactly once, parameterized over a
small operations record, with the Session and Effectful entry points as thin backends;
an application whose database contains foreign queues with names the Haskell validator
rejects still reconciles its own queues successfully at boot; a declared notify throttle
interval that differs from the database row is updated (the one deliberate exception to
"never mutate existing state"), declared queue types that contradict the observed queue
are reported as drift without mutation, and the FIFO index action reports created versus
skipped truthfully from a catalog check; and the package documentation states the real
contract — additive reconciliation with one documented drift exception — including an
explicit concurrent-startup caveat distinguishing extension installs from
pgmq-migration installs.

In scope: the pgmq-config package; the small additions to pgmq-core (an unvalidated
queue listing type), pgmq-hasql (two new read-only statements and sessions), and
pgmq-effectful (two new effect operations with traced-interpreter support) that the
reconciler needs; tests for all of the above; and changelog material handed to the
single release owner. Out of scope: any SQL migration (nothing here changes the
database schema or any `pgmq.*` function — the ledger coupling that constrained
MasterPlan 3 does not arise); the queue-name validation tightening and mixed-case
remediation owned by `docs/plans/15-validate-queue-names-and-classify-transient-errors-across-the-pgmq-layers.md`
(In Progress under MasterPlan 3 as of 2026-08-05); version bumps and consumer rollout,
owned by `docs/plans/12-expose-grouped-reads-on-the-umbrella-api-and-release-0-5-0-0.md`;
and fixing the upstream extension's `enable_notify_insert` race (upstream's problem —
we document the caveat and already ship the fixed function via migration `0003`).


## Decomposition Strategy

Three child plans, in a deliberate sequence. EP-16 is the enabling refactor: extract the
duplicated reconciler into one core parameterized over an operations record, with
behavior pinned by the existing green test suite. Doing this first means every
subsequent behavior change is written once instead of twice. EP-17 makes the
reconciler's state snapshot robust: it introduces an unvalidated queue listing
(`Text` names plus the partitioned/unlogged flags) through all three layers
(pgmq-core type, pgmq-hasql statement and session, pgmq-effectful operation) and
switches the reconciler's existence checks onto it, so foreign nonconforming names can
no longer fail the reconcile. EP-18 makes the report and the documentation truthful: a
catalog-backed FIFO index existence check (the second new read, same three-layer
shape EP-17 established), notify-throttle drift reconciliation via the existing
`pgmq.update_notify_insert`, queue-type drift reporting from the flags EP-17's listing
carries, and the full documentation-contract rewrite including the concurrency caveat.

The decomposition is by functional concern: deduplication (no behavior change), input
robustness (tolerate foreign state), and output truthfulness (report and docs match
reality). Each is independently verifiable — EP-16 by the untouched test suite staying
green, EP-17 by a poisoned-database test that fails before and passes after, EP-18 by
new report-accuracy and drift tests.

Alternatives considered. Folding everything into one plan was rejected: the refactor
wants to land with zero behavior change (its verification is "existing tests
untouched and green"), which is incompatible with bundling behavior changes. Running
EP-17 and EP-18 in parallel after EP-16 was rejected: both edit the same ~150-line
reconciler core and the same `ReconcileAction` type, both add an operation to the same
`Pgmq` effect GADT and traced-interpreter dispatch, and each plan is single-session
sized — the serialization cost is small and the parallel-edit clobbering risk is not.
Making the FIFO report honest by renaming the constructor to `EnsuredFifoIndex`
(documenting "applied unconditionally") instead of adding a catalog check was
considered and rejected: `ReconcileAction` is already changing incompatibly in this
initiative, so the honest-report version costs no extra breakage, and EP-17 has
already established the pattern for a three-layer read.


## Exec-Plan Registry

| # | Title | Path | Hard Deps | Soft Deps | Status |
|---|-------|------|-----------|-----------|--------|
| 16 | Extract the pgmq-config reconciler into a single backend-agnostic core | docs/plans/16-extract-the-pgmq-config-reconciler-into-a-single-backend-agnostic-core.md | None | None | Complete |
| 17 | Reconcile against unvalidated queue listings so foreign names cannot break startup | docs/plans/17-reconcile-against-unvalidated-queue-listings-so-foreign-names-cannot-break-startup.md | EP-16 | MP3 EP-15 | Not Started |
| 18 | Report reconciliation truthfully and document the real contract | docs/plans/18-report-reconciliation-truthfully-and-document-the-real-contract.md | EP-16, EP-17 | None | Not Started |

Status values: Not Started, In Progress, Complete, Cancelled.
"MP3 EP-15" is `docs/plans/15-validate-queue-names-and-classify-transient-errors-across-the-pgmq-layers.md`
under MasterPlan 3 — a soft dependency (see Dependency Graph).


## Dependency Graph

EP-16 has no dependencies and goes first. EP-17 hard-depends on EP-16 because it edits
the reconciler core that EP-16 relocates — implementing it against the pre-refactor
duplicated code would mean writing the change twice and conflicting with the move.
EP-18 hard-depends on EP-16 for the same reason, and on EP-17 because its queue-type
drift reporting reads the partitioned/unlogged flags from the unvalidated listing
EP-17 introduces, and because its second three-layer read (the FIFO index listing)
extends the same effect GADT, traced-interpreter dispatch, and export lists EP-17
touches — landing them in sequence means the second lander extends a known shape
instead of racing a sibling.

The soft dependency on MasterPlan 3's EP-15 (In Progress, and partially present in the
working tree as of 2026-08-05: `parseQueueName` already rejects uppercase and empty
names) runs in both directions but blocks nothing. EP-15's stricter parser widens the
class of database rows that poison `listQueues` decoding from "names outside
`[A-Za-z0-9_]` or overlong" to "anything not lowercase", making EP-17's robustness fix
more urgent; and EP-15's plan documents an operational mixed-case remediation whose
motivation ("a database that still contains mixed-case rows will fail `listQueues`
decoding, and therefore pgmq-config reconciliation") is partially superseded once
EP-17 lands — reconciliation will no longer fail, though typed `listQueues` for
API consumers still will. EP-17 must use foreign names that are invalid under both the
old and the new parser (hyphenated names are the stable choice) so its tests do not
depend on EP-15's landing state, and it must update EP-15's remediation framing if
EP-15 is still open when EP-17 completes (see Integration Points).

No plan in this MasterPlan can run in parallel with another. All three can run in
parallel with any MasterPlan 2 or MasterPlan 3 work except as noted for shared files
below.


## Integration Points

`pgmq-config/src/Pgmq/Config.hs`, `pgmq-config/src/Pgmq/Config/Effectful.hs`, and the
new core module EP-16 creates (`pgmq-config/src/Pgmq/Config/Reconcile.hs`) are touched
by all three plans. EP-16 defines the core module and the operations record
(`ReconcileOps`); EP-17 and EP-18 extend the record with one read each and edit only
the core, never the backends, for logic changes. The order is fixed by the hard
dependencies, so each plan edits the file state its predecessor left.

`pgmq-config/src/Pgmq/Config/Types.hs` (the `ReconcileAction` type) is extended by
EP-18 (drift constructors, FIFO skip semantics) and read by EP-17's tests. EP-18 owns
all constructor changes. Adding or changing constructors of an exported sum type is a
PVP-major change to pgmq-config; see the release integration point.

The `Pgmq` effect GADT (`pgmq-effectful/src/Pgmq/Effectful/Effect.hs`), both its
interpreters (`pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs` and
`Interpreter/Traced.hs`), and the pgmq-hasql export surface
(`pgmq-hasql/src/Pgmq/Hasql/Statements/*.hs`, `Sessions.hs`, `Pgmq.hs`) each gain one
read-only operation from EP-17 (`listQueuesUnvalidated`) and one from EP-18
(`listFifoIndexQueueNames`). EP-17 establishes the pattern (statement, session,
umbrella export, effect constructor, plain-interpreter case, traced-interpreter case
with span name); EP-18 replicates it. Keiro's ADR
`docs/adr/0001-keiro-pgmq-job-processing-telemetry-contract.md` pins the traced
interpreter's span semantics for existing operations; adding new operations with new
span names is additive and does not violate it — neither plan may alter any existing
operation's span.

`pgmq-core/src/Pgmq/Types.hs` is shared with MasterPlan 3's EP-15, which is In
Progress in this working tree (its stricter `parseQueueName` is already present).
EP-17 adds the `UnvalidatedQueue` type next to `Queue` and must not touch
`parseQueueName`, `FromJSON QueueName`, or `notifyChannelName`. If EP-15 is still
uncommitted or open when EP-17 lands, coordinate through git as usual — the edits are
in disjoint regions of the file. EP-17 must also re-read EP-15's M2 remediation prose
and `docs/design/016-queue-name-validation.md` (if present by then) and correct the
claim that mixed-case rows break "pgmq-config reconciliation" — after EP-17 they break
only typed `listQueues` consumers.

Test suites: EP-17 and EP-18 both add specs that create foreign/nonconforming rows in
`pgmq.meta` via raw SQL. Such rows poison every concurrent test that calls the typed
`listQueues` on the shared pool (tasty runs specs in parallel; `ConfigSpec` alone
calls it in four tests). Both plans must therefore run those specs on a dedicated
PostgreSQL instance provisioned per-module via `EphemeralPg.startCached`, mirroring
`pgmq-config/test/NotifyCrashSpec.hs` — the same isolation decision MasterPlan 3
EP-15 recorded for its `AliasingSpec` on 2026-08-05.

Release and changelogs: `docs/plans/12-expose-grouped-reads-on-the-umbrella-api-and-release-0-5-0-0.md`
(MasterPlan 2) is the single owner of the coordinated 0.5.0.0 family release, per
MasterPlan 3's standing decision. Each plan here records its exact package-changelog
material (pgmq-core, pgmq-hasql, pgmq-effectful, pgmq-config as applicable) and bumps
no version. The `ReconcileAction` constructor changes and the new effect operations
ride the same 0.5.0.0 train as MasterPlan 3's breaking changes. If EP-12 has already
cut 0.5.0.0 before this MasterPlan completes, the remaining changes need a subsequent
coordinated release; record that situation in this MasterPlan's Decision Log and hand
ownership to a successor release plan rather than bumping ad hoc.

Migration ledger: explicitly not an integration point. No plan in this MasterPlan
adds, edits, or renumbers anything under `pgmq-migration/migrations/`.


## Progress

- [x] EP-16 (2026-08-05): reconciler core extracted to `Pgmq.Config.Reconcile` over a
      `ReconcileOps` record; Session and Effectful backends are thin adapters; public API
      and behavior unchanged; the pre-existing 14 pgmq-config tests pass without
      modification (`git diff --stat -- pgmq-config/test/` empty), and `cabal test all`
      is green across all five suites.
- [ ] EP-17: `UnvalidatedQueue` listing exists through pgmq-core, pgmq-hasql, and
      pgmq-effectful (plain and traced); the reconciler's existence checks use it; a
      dedicated-instance spec proves a hyphen-named foreign queue no longer fails
      `ensureQueues`/`ensureQueuesReport`; EP-15's remediation framing corrected if
      still open.
- [ ] EP-18: FIFO index existence read through all three layers; report shows
      `CreatedFifoIndex` exactly once then `SkippedFifoIndex`; declared-vs-observed
      notify-throttle drift updates the interval via `pgmq.update_notify_insert` and
      reports it; queue-type drift reported without mutation; haddocks and cabal
      description state the additive contract, the drift exception, and the
      concurrent-startup caveat; changelog material recorded for the release owner.


## Surprises & Discoveries

- Review verification (2026-08-05): all five findings verified by code reading against
  the working tree, the vendored `vendor/pgmq/pgmq-extension/sql/pgmq.sql`, and
  migration `0003-notify-crash-safety-and-locking.sql`; the full 14-test pgmq-config
  suite runs green in 0.81s, so the refactor plan has a solid behavioral pin.
- Plan authoring (2026-08-05): MasterPlan 3's EP-15 was found In Progress with its M2
  parser change already present in the working tree mid-session — `parseQueueName` now
  rejects uppercase and empty names. This changed EP-17's design target (foreign-name
  tests must use names invalid under both parser generations, i.e. hyphens) and
  supplied the test-isolation precedent (dedicated ephemeral instance for specs that
  seed nonconforming meta rows).
- Plan authoring (2026-08-05): `parseTopicPattern` (`pgmq-core/src/Pgmq/Types.hs`)
  checks only non-empty and ≤255 characters — strictly laxer than the server's
  `pgmq.validate_topic_pattern` — and the throttle decoder carries no `D.refine`, so
  the queue-name path in `queueDecoder` is the only poisonable snapshot read. EP-17
  therefore replaces exactly one read, not three.
- EP-16 implementation (2026-08-05): the two reconciler copies really were in sync, so
  the extraction was a line-for-line move with no drift to reconcile, and generic-lens
  labels resolve without annotation on the higher-kinded `ReconcileOps m` record
  (`ops ^. #listQueues` works because `(^.)` pins the type-changing parameters through
  `Const`). The record therefore costs the core nothing beyond a `Monad m` constraint —
  EP-17 and EP-18 can add fields freely. Confirmed the flag-off build
  (`cabal build pgmq-config -f-effectful`) is the only check that catches an effectful
  import leaking into the unconditionally-compiled core; both sibling plans should keep
  it in their acceptance runs.

- Plan authoring (2026-08-05): `pgmq.update_notify_insert` resets `last_notified_at`
  to the epoch as a side effect of changing the interval (vendored SQL; same reset as
  a re-enable). Drift reconciliation therefore causes at most one immediate
  notification after a real config change — acceptable, but EP-18 must document it.


## Decision Log

- Decision: Fix the five review findings under a new MasterPlan rather than adding
  child plans to MasterPlan 3.
  Rationale: The user asked for a master plan; MasterPlan 3 is scoped to the PGH-1..11
  findings of the 2026-07 review and is nearly complete, while this work stems from a
  distinct 2026-08 review with its own decomposition. Coordination with MasterPlan 3's
  EP-15 is modeled explicitly as a soft dependency and integration points.
  Date: 2026-08-05

- Decision: Sequence the three plans as a hard chain (EP-16 → EP-17 → EP-18) instead
  of allowing EP-17/EP-18 parallelism.
  Rationale: Both behavior plans edit the same reconciler core, the same
  `ReconcileAction` type, and the same effect GADT/traced dispatch; each is
  single-session sized, so serialization costs little and eliminates clobbering and
  double-specification (EP-18 is specified against the snapshot record EP-17 leaves).
  Date: 2026-08-05

- Decision: Reconcile declared notify-throttle drift by calling
  `pgmq.update_notify_insert`; report (never mutate) queue-type drift.
  Rationale: The throttle interval is a value pgmq exposes an in-place, non-destructive
  update for, and a declared interval the reconciler silently ignores is the review's
  clearest doc-versus-behavior lie. Queue type has no safe in-place conversion —
  changing it means dropping and recreating a queue, which a bootup reconciler must
  never do — so drift is surfaced in the report and left to the operator.
  Date: 2026-08-05

- Decision: Make the FIFO report honest with a real catalog existence check
  (`pg_indexes`) rather than renaming the action to "ensured".
  Rationale: `ReconcileAction` is already breaking in this train, EP-17 establishes
  the three-layer read pattern the check reuses, and a truthful created/skipped
  distinction is what the report exists for. The check reads `pg_indexes` because
  pgmq's API deliberately has no index-existence function (`pgmq.create_fifo_index`
  is fire-and-forget `IF NOT EXISTS`).
  Date: 2026-08-05

- Decision: The reconciler keeps issuing per-item mutations without wrapping the
  reconcile in a single transaction, and multi-replica concurrency is addressed by
  documentation (and by migration `0003` for pgmq-migration installs), not by
  client-side locking.
  Rationale: Every mutating call is idempotent and convergent on retry (verified in
  the review); a wrapping transaction would hold DDL locks across the whole topology
  and still not serialize replicas against each other; and the remaining
  extension-install race lives in upstream SQL this repository already fixes for its
  own installs. The honest fix is the caveat plus retry guidance EP-18 writes.
  Date: 2026-08-05

- Decision: This MasterPlan bumps no package version; changelog material is handed to
  MasterPlan 2's EP-12 (0.5.0.0).
  Rationale: Standing single-release-owner decision from MasterPlan 3; splitting
  release ownership again would recreate the ambiguity that decision removed.
  Date: 2026-08-05


## Outcomes & Retrospective

(To be filled during and after implementation.)
