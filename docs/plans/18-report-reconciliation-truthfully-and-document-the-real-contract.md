---
id: 18
slug: report-reconciliation-truthfully-and-document-the-real-contract
title: "Report reconciliation truthfully and document the real contract"
kind: exec-plan
created_at: 2026-08-05T23:43:28Z
intention: "intention_01kz9yszpmejztjbet6k4bvcf7"
master_plan: "docs/masterplans/4-make-the-pgmq-config-reconciler-truthful-robust-and-single-sourced.md"
---

# Report reconciliation truthfully and document the real contract

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.


## Purpose / Big Picture

pgmq-config's reconciler (`ensureQueues` and its report-returning sibling
`ensureQueuesReport`) promises, in its package description, "ensure all queues exist
with the desired settings", and its report type promises a truthful record of actions.
The 2026-08 review found three lies. First, settings drift is never reconciled or even
noticed: a declared notify-throttle interval that differs from the database row is
silently skipped (`pgmq.update_notify_insert` exists and is already wrapped by
pgmq-hasql, but the reconciler never calls it), and a declared queue type
(standard/unlogged/partitioned) contradicting the actual queue is silently reported as
`SkippedQueue`. Second, the FIFO-index action is fictional: `CreatedFifoIndex` is
reported on every run whether or not the index existed (the underlying
`pgmq.create_fifo_index` is `CREATE INDEX IF NOT EXISTS`), and the `SkippedFifoIndex`
constructor is dead code that no code path ever produces. Third, the "Safe to call on
every application startup" haddock is unqualified, although concurrent multi-replica
startup can fail one replica with SQLSTATE 42710 when pgmq was installed as the
upstream 1.11.0 extension (this repository's migration
`pgmq-migration/migrations/0003-notify-crash-safety-and-locking.sql` fixed that race
with a per-queue advisory lock — but only for databases installed via pgmq-migration).

After this plan the report and the documentation both tell the truth. A declared
throttle interval that differs from the observed row is updated in place and reported
as `UpdatedNotifyThrottle` with both values; queue-type drift is reported as
`DetectedQueueTypeDrift` without mutating anything; the FIFO action reports
`CreatedFifoIndex` exactly once and `SkippedFifoIndex` thereafter, backed by a real
catalog check; and the haddocks and cabal description state the actual contract —
additive reconciliation whose single mutation-of-existing-state is the declared
throttle update — including the concurrent-startup caveat and its
extension-versus-migration distinction. Observable end to end: run `ensureQueuesReport`
twice with a FIFO-indexed, notify-enabled config, change the declared throttle between
runs, and the two reports read exactly `[CreatedQueue, EnabledNotify,
CreatedFifoIndex, ...]` then `[SkippedQueue, UpdatedNotifyThrottle, SkippedFifoIndex,
...]`.


## Progress

- [x] M1 (2026-08-05): `listFifoIndexQueueNames` reads `pg_indexes` in
      `pgmq-hasql/src/Pgmq/Hasql/Statements/QueueObservability.hs`, wrapped as a
      session and re-exported from the `Pgmq` umbrella; `ListFifoIndexQueueNames`
      added to the `Pgmq` effect and dispatched by both interpreters, traced span
      `pgmq.list_fifo_indexes`. `cabal build pgmq-hasql pgmq-effectful`
      library-warning-clean; `cabal test pgmq-effectful` green (30 tests).
- [x] M2 (2026-08-05): `ReconcileAction` gained `UpdatedNotifyThrottle` and
      `DetectedQueueTypeDrift`, plus the new `ObservedQueueType` sum and the exported
      `defaultThrottleMs` constant; `ReconcileOps` gained `listFifoIndexQueueNames`
      and `updateNotifyInsert`, wired in both backends; the reconciler now snapshots
      throttle intervals (not just names) and FIFO indexes, skips the FIFO call when
      the index exists, updates drifted throttles in place, and reports queue-type
      drift in place of `SkippedQueue`. Three new `ConfigSpec` cases plus additive
      `actionForQueue` arms; `cabal test pgmq-config` green at 20 tests.
- [x] M3 (2026-08-05): contract rewritten on `ensureQueues` (canonical) with
      `ensureQueuesReport` and both `Eff` twins pointing at it; module headers added
      to all three public pgmq-config modules; cabal `description:` replaced;
      `docs/design/018-reconciliation-contract.md` written; changelog material
      recorded under Interfaces and Dependencies; `cabal haddock pgmq-config` reports
      100% coverage on all three modules with no ambiguity warnings; `cabal test all`
      green (144 tests across five suites); committed.


## Surprises & Discoveries

- The plan's test arithmetic was stale before it started (2026-08-05). M2's acceptance
  says "17 tests: 14 existing + 3 new", but sibling plan
  `docs/plans/17-reconcile-against-unvalidated-queue-listings-so-foreign-names-cannot-break-startup.md`
  had already taken pgmq-config from 14 to 17 by adding `ForeignQueueSpec`. The real
  end state is 20. Nothing about the work changed — worth recording only because a
  future reader comparing counts would otherwise think three tests went missing.

- The `Nothing`-versus-250 non-flap case needed no new test (2026-08-05). The plan asks
  for one, but the pre-existing `testEnsureQueuesWithNotifyDefault` already enables with
  `Nothing`, re-reconciles with `Nothing`, asserts `SkippedNotify`, and asserts the
  stored row reads 250 — which is exactly the assertion, and it now exercises the new
  interval comparison rather than the old presence check. It stayed green unmodified,
  which is the stronger signal.

- Haddock flagged two of this initiative's own doc links as ambiguous (2026-08-05):

  ```text
  Warning: 'NotifyConfig' is ambiguous. It is defined
      * at src/Pgmq/Config/Types.hs:62:21
      * at src/Pgmq/Config/Types.hs:62:1
  ```

  A single-constructor record makes the bare name mean both a type and a data
  constructor. The fix is haddock's namespace prefix — `t'NotifyConfig'`,
  `t'QueueConfig'`, `t'ReconcileOps'`. Worth knowing for any future doc work in this
  package: three of its four exported records have this shape. The remaining
  `Rep_QueueConfig` link warnings are inherent to `deriving stock Generic` and predate
  this initiative.

- `docs/design/015-notification-delivery-contract.md` needed no correction
  (2026-08-05). The plan flagged it as the likely home of a stale
  pure-additive-no-updates claim, and MasterPlan 3 had twice found such claims in
  design notes. This one holds up: its statement that unthrottled notification after a
  crash is "bounded and self-correcting: unthrottled notifications until the next
  reconcile restores the configured value" is still exactly right — a truncated
  throttle table means no row, so the reconciler takes the enable path, not the new
  update path.


## Decision Log

- Decision: Throttle drift is reconciled by mutation (`pgmq.update_notify_insert`);
  queue-type drift is reported only.
  Rationale: The throttle interval has a non-destructive in-place update in the pgmq
  API, and a declared value the reconciler silently ignores is the clearest
  doc-versus-behavior lie the review found. Queue type has no safe conversion —
  "fixing" it means dropping and recreating the queue, destroying messages, which a
  bootup reconciler must never do. (MasterPlan 4 Decision Log, 2026-08-05; restated
  here for self-containment.)
  Date: 2026-08-05

- Decision: `DetectedQueueTypeDrift` replaces `SkippedQueue` for a drifted queue
  (the report emits one action per queue-existence check, not both).
  Rationale: One action per check keeps the report a partition of decisions; drift
  is a skip with a reason, and consumers that count skips can match both
  constructors. The constructor carries declared and observed types so a consumer
  can log or alert without re-querying.
  Date: 2026-08-05

- Decision: Compare throttle drift against the declared value with `Nothing`
  meaning 250.
  Rationale: `NotifyConfig.throttleMs = Nothing` is documented as "use the pgmq
  default (250 ms)" and the enable path already applies exactly that via `COALESCE`
  (`pgmq-hasql/src/Pgmq/Hasql/Statements/QueueManagement.hs`), so drift comparison
  must use the same effective value or a `Nothing` config would flap against a
  250-row forever.
  Date: 2026-08-05

- Decision: Partition-interval/retention drift for partitioned queues stays out of
  scope, documented as unobserved.
  Rationale: `pgmq.list_queues()` does not expose partition configuration; observing
  it would mean querying pg_partman's `part_config`, a dependency surface this
  initiative does not open. The haddock states that partition settings are not
  drift-checked.
  Date: 2026-08-05


## Outcomes & Retrospective

Completed 2026-08-05. The report and the documentation now both tell the truth, and each
of the three lies has a test pinning it:

Running `ensureQueuesReport [withFifoIndex (standardQueue qn)]` twice reports
`CreatedFifoIndex` then `SkippedFifoIndex`; before, it claimed `CreatedFifoIndex` forever
and `SkippedFifoIndex` was unreachable. Declaring `withNotifyInsert (Just 250)`, then
`Just 500`, reports `UpdatedNotifyThrottle qn 250 500` and the stored row actually reads
500; before, the second run said `SkippedNotify` and the database kept 250 — editing the
config had no effect at all. Declaring `unloggedQueue` over a live standard queue reports
`DetectedQueueTypeDrift qn UnloggedQueue ObservedStandard` and the queue is untouched;
before, it said `SkippedQueue` and the contradiction vanished.

Convergence held throughout, which was the thing most at risk. The throttle update fires
only while declared and observed differ, so a third run skips; `Nothing` compares equal to
250 so a defaulted config cannot flap; and the pre-existing "ensureQueues is truly
idempotent for notify" test — which pins that a no-drift re-run leaves `last_notified_at`
alone — passed unmodified from the first build. That test is the guard rail that keeps the
new update path from degenerating into the old unconditional re-enable.

What went right: the two prerequisites paid off exactly as designed. Plan 16's single core
meant the behavior change landed in one file; plan 17's `Map Text UnvalidatedQueue`
snapshot handed over the partitioned/unlogged flags with no signature change, and its
three-layer read gave M1 a shape to copy verbatim. M1 through M3 each built and tested
green on the first attempt.

What is worth carrying forward: the asymmetry at the heart of this plan — reconcile
automatically only where the repair is non-destructive and the API supports it in place,
otherwise report and stop — is now written down in
`docs/design/018-reconciliation-contract.md` rather than living in a commit message. That
is the decision a future contributor is most likely to want to relitigate (someone will
eventually propose auto-fixing queue-type drift), and it now has an argument to answer.

Nothing is left open. Versions are unbumped by design; the changelog material below goes
to the release owner.


## Context and Orientation

Multi-package Cabal project; run everything from the repository root inside
`nix develop`; tests self-provision PostgreSQL via `ephemeral-pg`. Prerequisites:
plans 16 and 17 (`docs/plans/16-extract-the-pgmq-config-reconciler-into-a-single-backend-agnostic-core.md`,
`docs/plans/17-reconcile-against-unvalidated-queue-listings-so-foreign-names-cannot-break-startup.md`)
are Complete — the reconciler lives once in
`pgmq-config/src/Pgmq/Config/Reconcile.hs` over a `ReconcileOps m` record, and its
queue snapshot arrives as `UnvalidatedQueue` rows (unvalidated `Text` name plus
`unvalidatedIsPartitioned`/`unvalidatedIsUnlogged` flags) — this plan reads those
flags for drift detection. Verify both statuses in MasterPlan 4's registry before
starting.

The report type is `ReconcileAction` in `pgmq-config/src/Pgmq/Config/Types.hs`:

```haskell
data ReconcileAction
  = CreatedQueue !QueueName !QueueType
  | EnabledNotify !QueueName !(Maybe Int32)
  | CreatedFifoIndex !QueueName
  | BoundTopic !QueueName !TopicPattern
  | SkippedQueue !QueueName
  | SkippedNotify !QueueName
  | SkippedFifoIndex !QueueName
  | SkippedTopicBinding !QueueName !TopicPattern
  deriving stock (Show)
```

`SkippedFifoIndex` is currently unreachable. The reconciler core's current FIFO step
calls the `createFifoIndex` operation unconditionally whenever
`cfg ^. #fifoIndex` is set and always reports `CreatedFifoIndex` — the underlying
`pgmq.create_fifo_index` delegates to a `CREATE INDEX IF NOT EXISTS` on index
`q_<lowercased name>_fifo_idx` over table `pgmq.q_<lowercased name>` (see
`_create_fifo_index_if_not_exists` in the vendored
`vendor/pgmq/pgmq-extension/sql/pgmq.sql`), so the call is harmless but the report is
false after the first run. pgmq's API deliberately has no index-existence query,
which is why the honest check reads PostgreSQL's `pg_indexes` catalog view directly —
a first for pgmq-hasql's statements, all of which currently call `pgmq.*` functions;
that novelty is fine (client-side SQL is not covered by the "do not hand-write SQL"
rule, which applies to pgmq-migration's vendored schema).

The notify snapshot: `Sessions.listNotifyInsertThrottles` returns
`NotifyInsertThrottle` rows (`throttleQueueName :: Text`,
`throttleIntervalMs :: Int32`, plus a timestamp; `pgmq-core/src/Pgmq/Types.hs`). The
reconciler currently keeps only the names in a `Set Text` and skips whenever the name
is present — the interval is dropped, which is exactly why drift is invisible. The
in-place update exists at every layer already: statement `updateNotifyInsert`
(`pgmq-hasql/src/Pgmq/Hasql/Statements/QueueManagement.hs`, SQL
`select from pgmq.update_notify_insert($1, $2)`), session wrapper, and effect
operation `UpdateNotifyInsert` with traced span `"pgmq.update_notify_insert"`
(`pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs`). Its parameter record is
`UpdateNotifyInsert { queueName :: !QueueName, throttleIntervalMs :: !Int32 }`
(`pgmq-hasql/src/Pgmq/Hasql/Statements/Types.hs`). Server-side semantics (vendored
SQL, unchanged by migration 0003): errors if the queue or throttle row does not
exist — the reconciler only calls it when the snapshot has the row, so that is safe —
and, as a side effect, resets `last_notified_at` to the epoch, meaning the next
insert after a real config change notifies immediately once. Document that; it is
the same side effect a manual re-enable has. Contrast: the reconciler must NOT call
`enableNotifyInsert` for drift, because a full re-enable also resets state when
nothing changed — the existing test "ensureQueues is truly idempotent for notify"
(`pgmq-config/test/ConfigSpec.hs`, `testEnsureQueuesIsTrulyIdempotent`) pins that a
second identical run leaves `last_notified_at` alone, and it must stay green.

Queue-type drift raw material: declared `QueueType` (`StandardQueue` /
`UnloggedQueue` / `PartitionedQueue PartitionConfig` in
`pgmq-config/src/Pgmq/Config/Types.hs`) versus observed
`unvalidatedIsPartitioned`/`unvalidatedIsUnlogged` flags. The three observable
states map one-to-one: both flags false = standard, unlogged flag = unlogged,
partitioned flag = partitioned.

The concurrency story the docs must tell (verified in the 2026-08 review): every
mutating call the reconciler issues is idempotent and convergent under sequential
retry; under *concurrent* multi-replica startup, `pgmq.create`/`create_partitioned`
serialize on a per-queue advisory lock, `bind_topic` upserts, and
`enable_notify_insert` is safe only where migration `0003` is applied
(pgmq-migration installs) — against a stock upstream-1.11.0 extension install, two
replicas enabling notify on the same new queue can collide with SQLSTATE 42710
(confirmed at ~28% collision rate in MasterPlan 3's EP-14 work, 400 concurrent
calls). `CREATE INDEX IF NOT EXISTS` has a similar narrow duplicate-name race.
Everything converges on retry; the guidance is "retry startup reconciliation on
failure, or serialize it externally; pgmq-migration installs are already
race-free for notify".

Docs to rewrite: the haddocks on `ensureQueues`/`ensureQueuesReport`
(`pgmq-config/src/Pgmq/Config.hs`) and their `Eff` twins
(`pgmq-config/src/Pgmq/Config/Effectful.hs`), the `ReconcileAction` constructor
haddocks (`Types.hs`), and the `description:` block of
`pgmq-config/pgmq-config.cabal`, which currently says "ensure all queues exist with
the desired settings. … All operations are idempotent."


## Plan of Work

### Milestone 1 — the FIFO-index listing through the layers

Scope: a read-only catalog listing of which queues already have their FIFO index,
following exactly the three-layer pattern plan 17 established.

`pgmq-hasql/src/Pgmq/Hasql/Statements/QueueObservability.hs`, add and export:

```haskell
-- | Queue names (lowercased physical form) that already have the FIFO headers
-- index @q_<name>_fifo_idx@. pgmq exposes no index-existence function —
-- @pgmq.create_fifo_index@ is fire-and-forget @IF NOT EXISTS@ — so this reads
-- the @pg_indexes@ catalog view directly.
listFifoIndexQueueNames :: Statement () [Text]
listFifoIndexQueueNames = preparable sql E.noParams decoder
  where
    sql =
      "select substring(indexname from '^q_(.*)_fifo_idx$') \
      \from pg_indexes \
      \where schemaname = 'pgmq' and indexname ~ '^q_.*_fifo_idx$'"
    decoder = D.rowList (D.column (D.nonNullable D.text))
```

(`pg_indexes.indexname` is of type `name`; if the prepared-statement OID check
rejects the `substring` result as `name` rather than `text`, cast in SQL:
`substring(...)::text`.) Session wrapper `listFifoIndexQueueNames :: Session [Text]`
in `Sessions.hs`, umbrella export in `Pgmq.hs`. Effect constructor
`ListFifoIndexQueueNames :: Pgmq m [Text]` plus smart function in
`pgmq-effectful/src/Pgmq/Effectful/Effect.hs`, plain-interpreter case, and traced
case in the Queue Observability section with an honest span name for a catalog read:

```haskell
  ListFifoIndexQueueNames ->
    withTracedOp config pool (defaultOpInfo "pgmq.list_fifo_indexes" OTel.Internal) $
      Sessions.listFifoIndexQueueNames
```

(The span name is this library's own label — there is no SQL function behind it; do
not modify any existing case, per the keiro ADR constraint
`docs/adr/0001-keiro-pgmq-job-processing-telemetry-contract.md`.)

Acceptance: `cabal build pgmq-hasql pgmq-effectful` warning-clean, pgmq-effectful
tests green.

### Milestone 2 — truthful reconciliation

Scope: the reconciler core and report type change together with their tests.

`pgmq-config/src/Pgmq/Config/Types.hs` — extend and re-haddock `ReconcileAction`:
keep all eight constructors (haddock `SkippedFifoIndex` as "index already existed;
nothing was issued") and add:

```haskell
  | -- | The declared throttle interval differed from the database row; the row
    -- was updated in place via @pgmq.update_notify_insert@ (this also resets
    -- the throttle's @last_notified_at@, so one immediate notification may
    -- follow). Fields: queue, observed interval, declared interval.
    UpdatedNotifyThrottle !QueueName !Int32 !Int32
  | -- | The queue exists but its observed type contradicts the declared one.
    -- Nothing was mutated: converting a queue's type means dropping and
    -- recreating it, which a startup reconciler must never do. Fields: queue,
    -- declared type, observed type.
    DetectedQueueTypeDrift !QueueName !QueueType !ObservedQueueType
```

with a new three-value sum `ObservedQueueType = ObservedStandard |
ObservedUnlogged | ObservedPartitioned` (stock-derived `Eq, Show`; give `QueueType`
an `Eq` instance too if comparison wants it — it currently derives only `Show`).
Export both from `Pgmq.Config.Types` and `Pgmq.Config`.

`pgmq-config/src/Pgmq/Config/Reconcile.hs`:

- `ReconcileOps` gains `listFifoIndexQueueNames :: m [Text]` and
  `updateNotifyInsert :: StmtTypes.UpdateNotifyInsert -> m ()`; `sessionOps` and
  `effectfulOps` supply the session/effect implementations.
- Snapshot phase additionally binds `existingFifo :: Set Text` from the new read.
  Note the physical-form subtlety in a code comment: the catalog names are
  lowercased physical forms; declared names are lowercase-only after MasterPlan 3's
  plan 15, so plain textual matching is exact (`queueNameToText qn`).
- Queue step: keep creation exactly as is; in the exists branch, compare declared
  type against the snapshot row's flags (plan 17 left the snapshot carrying
  `UnvalidatedQueue` values or can be upgraded from `Set Text` to
  `Map Text UnvalidatedQueue` now — see plan 17's M3 note; either way this plan
  needs the flags per name). Matching type → `SkippedQueue`; mismatch →
  `DetectedQueueTypeDrift qn declared observed`. A declared
  `PartitionedQueue pc` against an observed partitioned queue is a match —
  partition settings are not drift-checked (Decision Log).
- Notify step: snapshot becomes `Map Text Int32` (name → interval) instead of
  `Set Text`. Row absent → enable as today (`EnabledNotify`). Row present with
  interval equal to `fromMaybe 250 (nc ^. #throttleMs)` → `SkippedNotify`. Row
  present with a different interval → call the `updateNotifyInsert` op with the
  effective declared value and report
  `UpdatedNotifyThrottle qn observed effectiveDeclared`.
- FIFO step: `fifoIndex` declared and name in `existingFifo` → `SkippedFifoIndex`
  (no call); otherwise call `createFifoIndex` and report `CreatedFifoIndex` (the
  `IF NOT EXISTS` underneath keeps the concurrent-race window harmless).

Tests (`pgmq-config/test/ConfigSpec.hs`, shared pool is fine — no foreign names
involved):

- Update the pattern-total helpers (`isSkipped`, `actionForQueue`) for the new
  constructors — these are the only permitted edits to existing test code, and only
  additively (new match arms).
- `testEnsureQueuesWithFifo` gains a second run asserting `SkippedFifoIndex` and no
  `CreatedFifoIndex`.
- New: throttle drift — ensure with `withNotifyInsert (Just 250)`, re-ensure with
  `withNotifyInsert (Just 500)`; report contains `UpdatedNotifyThrottle qn 250 500`;
  `Sessions.listNotifyInsertThrottles` shows 500; a third identical run reports
  `SkippedNotify`. Also the `Nothing`-vs-250 non-flap case: enable with `Nothing`,
  re-ensure with `Nothing` → `SkippedNotify`, and `testEnsureQueuesIsTrulyIdempotent`
  still proves `last_notified_at` is untouched by a no-drift run.
- New: type drift — `ensureQueues [standardQueue qn]`, then reconcile
  `[unloggedQueue qn]`; report contains
  `DetectedQueueTypeDrift qn UnloggedQueue ObservedStandard`; the queue is still
  standard (`Sessions.listQueues` flags unchanged; valid names, so the typed listing
  is usable here).

Acceptance: `cabal test pgmq-config --test-show-details=direct` green (17 tests:
14 existing + 3 new); `cabal test all` green.

### Milestone 3 — the documentation contract

Scope: make every published sentence match the implementation.

`pgmq-config/src/Pgmq/Config.hs` — rewrite the `ensureQueues`/`ensureQueuesReport`
haddocks to state: the reconciler is additive (creates missing queues, notify
settings, FIFO indexes, bindings; never drops or converts anything; queues absent
from the config are untouched); its single mutation of existing state is the
declared-throttle update, with the epoch-reset side effect named; queue-type drift
is reported, not fixed; partition settings are not drift-checked; and the
concurrency caveat — concurrent replicas converge for queue creation and bindings,
but `enable_notify_insert` on a stock upstream-1.11.0 *extension* install can fail
one replica with SQLSTATE 42710 (retry the reconcile, or serialize startup);
databases installed via this repository's pgmq-migration (migration `0003`) are free
of that race. Mirror the same text on the `Eff` twins in
`pgmq-config/src/Pgmq/Config/Effectful.hs` (or point them at the Session docs with a
one-line summary — pick one and be consistent). Update the `description:` in
`pgmq-config/pgmq-config.cabal`: replace "ensure all queues exist with the desired
settings" and "All operations are idempotent" with the additive-plus-throttle-drift
contract in two or three sentences.

Check `docs/design/` for notes covering reconciler behavior before rewording —
MasterPlan 3 twice found stale claims living there
(`docs/design/015-notification-delivery-contract.md` is the likely candidate; if it
or any note describes `ensureQueues` as pure-additive-no-updates, correct it in the
same change). Consider whether the truthful-report contract deserves its own short
design note under `docs/design/` (next free number; as of writing, `017`); write it
if the haddock alone would leave the "why mutation-for-throttle but report-for-type"
rationale homeless.

Record changelog material for the release owner
(`docs/plans/12-expose-grouped-reads-on-the-umbrella-api-and-release-0-5-0-0.md`):
pgmq-config — breaking `ReconcileAction` extension (`UpdatedNotifyThrottle`,
`DetectedQueueTypeDrift`, `ObservedQueueType`; `SkippedFifoIndex` now actually
emitted), behavior change "declared throttle drift is now reconciled", doc-contract
rewrite; pgmq-hasql — `listFifoIndexQueueNames`; pgmq-effectful —
`ListFifoIndexQueueNames` operation. Bump no version (MasterPlan 4 Decision Log).

Acceptance: `cabal test all --test-show-details=direct` green; `cabal haddock
pgmq-config` builds; the words "desired settings" no longer appear in
`pgmq-config/pgmq-config.cabal`; MasterPlan 4's registry and Progress updated; this
plan's living sections finalized.


## Concrete Steps

From the repository root, inside `nix develop`:

```bash
# M1
cabal build pgmq-hasql pgmq-effectful
cabal test pgmq-effectful --test-show-details=direct

# M2
cabal build all
cabal test pgmq-config --test-show-details=direct

# M3
cabal test all --test-show-details=direct
cabal haddock pgmq-config
nix fmt
```

Expected M2 transcript additions:

```text
pgmq-config
  Pgmq.Config
    creates FIFO index:                                 OK
    second run skips the existing FIFO index:           OK
    updates a drifted notify throttle:                  OK
    reports queue-type drift without mutating:          OK
```

Commit per milestone, conventional style, always with the trailers:

```text
feat(pgmq-config): report reconciliation truthfully and reconcile throttle drift

MasterPlan: docs/masterplans/4-make-the-pgmq-config-reconciler-truthful-robust-and-single-sourced.md
ExecPlan: docs/plans/18-report-reconciliation-truthfully-and-document-the-real-contract.md
Intention: intention_01kz9yszpmejztjbet6k4bvcf7
```


## Validation and Acceptance

A novice can verify each claim from the report alone. FIFO truthfulness: run
`ensureQueuesReport [withFifoIndex (standardQueue qn)]` twice; the first report
contains `CreatedFifoIndex qn`, the second `SkippedFifoIndex qn` — before this plan
the second run also said `CreatedFifoIndex`. Throttle drift: declare
`withNotifyInsert (Just 250)`, reconcile, redeclare `Just 500`, reconcile; the second
report contains `UpdatedNotifyThrottle qn 250 500` and
`pgmq.list_notify_insert_throttles()` shows 500 — before this plan the second report
said `SkippedNotify` and the database kept 250. Type drift: declare `unloggedQueue`
over an existing standard queue; the report says `DetectedQueueTypeDrift` and the
queue's flags are unchanged. No-drift idempotency is unchanged: the pre-existing
"truly idempotent" test still shows `last_notified_at` untouched by an identical
re-run. All demonstrated by `cabal test pgmq-config --test-show-details=direct`, and
`cabal test all` proves the family still builds and passes around the extended effect
GADT.


## Idempotence and Recovery

The reconciler's new behavior remains convergent: the throttle update fires only
while declared and observed intervals differ, so a second identical run reports
`SkippedNotify`; the FIFO catalog check makes the create call itself conditional, and
the underlying SQL stays `IF NOT EXISTS`, so a lost race degrades to a no-op; drift
reporting mutates nothing. All plan steps are working-tree edits plus builds; abandon
with `git checkout -- .` to the last milestone commit. The one user-visible side
effect — `last_notified_at` reset on a genuine throttle change — is inherent to
`pgmq.update_notify_insert` and is documented rather than worked around.


## Interfaces and Dependencies

No new external dependencies. End-state interfaces:

```haskell
-- pgmq-hasql
Pgmq.Hasql.Statements.QueueObservability.listFifoIndexQueueNames :: Statement () [Text]
Pgmq.Hasql.Sessions.listFifoIndexQueueNames :: Session [Text]
-- re-exported from the Pgmq umbrella

-- pgmq-effectful
Pgmq.Effectful.Effect: ListFifoIndexQueueNames :: Pgmq m [Text]
Pgmq.Effectful.Effect.listFifoIndexQueueNames :: (Pgmq :> es) => Eff es [Text]
-- traced span name "pgmq.list_fifo_indexes"

-- pgmq-config, Pgmq.Config.Types
data ObservedQueueType = ObservedStandard | ObservedUnlogged | ObservedPartitioned
data ReconcileAction
  = -- existing eight constructors, plus:
  | UpdatedNotifyThrottle !QueueName !Int32 !Int32   -- queue, observed, declared
  | DetectedQueueTypeDrift !QueueName !QueueType !ObservedQueueType

-- pgmq-config, Pgmq.Config.Reconcile (internal) — ReconcileOps gains:
listFifoIndexQueueNames :: m [Text]
updateNotifyInsert :: StmtTypes.UpdateNotifyInsert -> m ()
```

Coordination: hard-depends on plans 16 (single core) and 17 (`UnvalidatedQueue`
flags in the snapshot; established GADT-extension pattern). Changelog and version
handoff to MasterPlan 2's plan 12 per MasterPlan 4's Decision Log; if plan 12 has
already cut 0.5.0.0, escalate to MasterPlan 4's Decision Log for a successor release
owner instead of bumping here.

Changelog material for the release owner
(`docs/plans/12-expose-grouped-reads-on-the-umbrella-api-and-release-0-5-0-0.md`; this
plan bumps no version):

- **pgmq-hasql** — Added: `listFifoIndexQueueNames` (statement and session, re-exported
  from `Pgmq`), which reports the queues that already carry the FIFO headers index by
  reading the `pg_indexes` catalog view. pgmq exposes no index-existence function.
- **pgmq-effectful** — Added: the `ListFifoIndexQueueNames` operation and its smart
  function, dispatched by both interpreters under the traced span
  `pgmq.list_fifo_indexes` (this library's own label — no `pgmq.*` function backs it).
  Extending the `Pgmq` GADT breaks exhaustive matchers such as custom interpreters.
- **pgmq-config** — Breaking: `ReconcileAction` gains `UpdatedNotifyThrottle` and
  `DetectedQueueTypeDrift`; `SkippedFifoIndex`, previously unreachable, is now actually
  emitted. Consumers that pattern-match the report exhaustively, or that count skips,
  need updating. Added: `ObservedQueueType` and `defaultThrottleMs`.
- **pgmq-config** — Changed: a declared notification throttle interval that differs from
  the stored one is now applied via `pgmq.update_notify_insert` instead of being
  silently ignored. This is the reconciler's only mutation of existing state; it resets
  the throttle's `last_notified_at`, so one immediate notification may follow a genuine
  configuration change. An unchanged interval is still left strictly alone.
- **pgmq-config** — Changed: a queue whose observed type contradicts the declared one is
  reported as drift instead of `SkippedQueue`. Nothing is mutated.
- **pgmq-config** — Fixed: the FIFO index action reports `CreatedFifoIndex` only when it
  actually created the index.
- **pgmq-config** — Docs: the package description and the `ensureQueues` haddock now
  state the real contract, including the concurrent-startup caveat (SQLSTATE 42710 on
  stock upstream-1.11.0 extension installs; pgmq-migration installs are race-free). See
  `docs/design/018-reconciliation-contract.md`.
