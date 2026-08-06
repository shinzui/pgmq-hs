---
id: 16
slug: extract-the-pgmq-config-reconciler-into-a-single-backend-agnostic-core
title: "Extract the pgmq-config reconciler into a single backend-agnostic core"
kind: exec-plan
created_at: 2026-08-05T23:43:28Z
intention: "intention_01kz9yszpmejztjbet6k4bvcf7"
master_plan: "docs/masterplans/4-make-the-pgmq-config-reconciler-truthful-robust-and-single-sourced.md"
---

# Extract the pgmq-config reconciler into a single backend-agnostic core

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.


## Purpose / Big Picture

The pgmq-config package lets an application declare its message-queue topology as
Haskell values and call one function at startup to create whatever is missing in the
database. That reconciliation logic currently exists twice, line for line: once in
`pgmq-config/src/Pgmq/Config.hs` written against `Hasql.Session` (a sequence of
database calls on one connection), and once in `pgmq-config/src/Pgmq/Config/Effectful.hs`
written against the `Pgmq` effect from the pgmq-effectful package. The 2026-08 review
of pgmq-config confirmed the two copies are currently in sync — and that every future
fix must be applied twice or the copies silently drift.

This plan removes the duplication with zero behavior change: the reconciler moves into
one new internal module, parameterized over a small record of database operations, and
the two public modules become thin adapters that supply a Session-backed or
effect-backed record. After this change the public API, the emitted
`ReconcileAction` reports, and all fourteen existing pgmq-config tests are exactly as
before — the proof is that the test suite passes without a single test being edited.
The two sibling plans under the same MasterPlan
(`docs/masterplans/4-make-the-pgmq-config-reconciler-truthful-robust-and-single-sourced.md`)
then change reconciler behavior by editing one file instead of two.


## Progress

- [x] M1 (2026-08-05): `Pgmq.Config.Reconcile` core module exists
      (`pgmq-config/src/Pgmq/Config/Reconcile.hs`, registered under `other-modules:`);
      `Pgmq.Config` delegates to it through `sessionOps`; `cabal build pgmq-config`
      compiles warning-clean and `cabal test pgmq-config --test-show-details=direct`
      reports "All 14 tests passed (0.74s)" with `git diff --stat -- pgmq-config/test/`
      empty.
- [x] M2 (2026-08-05): `Pgmq.Config.Effectful` delegates to the same core through
      `effectfulOps`; `reconcileQueueEff`/`reconcileBindingEff` deleted
      (`grep -c reconcile pgmq-config/src/Pgmq/Config/Effectful.hs` → 0);
      `cabal build pgmq-config -f-effectful`, `cabal build all`, and `cabal test all`
      all green (core 12, effectful 30, migration 9, config 14, hasql 73 — 138 tests
      across five suites); `nix fmt` clean; living sections updated; work committed.


## Surprises & Discoveries

- Generic-lens labels work on the higher-kinded record (2026-08-05): `ReconcileOps m`
  carries the carrier monad as a type parameter, and there was some doubt whether
  `ops ^. #listQueues` would resolve, since generic-lens's `Field` constraint is
  type-changing and its `t`/`b` parameters are only pinned by functional
  dependencies. It resolves cleanly: `(^.)` fixes `t ~ s` through the `Const`
  functor, which discharges the fundep. No type annotations, no `field'` fallback,
  and no new warnings under the package's `-Wall -Wcompat -Wredundant-constraints`
  block.

  ```text
  [2 of 4] Compiling Pgmq.Config.Reconcile
  [4 of 4] Compiling Pgmq.Config [Source file changed]
  ```

- The refactor is genuinely behavior-preserving (2026-08-05): the full
  `cabal test pgmq-config` transcript is identical case-for-case before and after,
  including the three `NotifyCrashSpec` cases that drive `ensureQueues` end-to-end
  against a crashed-and-recovered PostgreSQL instance.

  ```text
  All 14 tests passed (0.74s)
  Test suite pgmq-config-test: PASS
  ```

- `-f-effectful` needs a full reconfigure (2026-08-05): building with the flag off
  and then building again with defaults makes cabal reconfigure and relink
  pgmq-config both times ("configuration changed"). Harmless, but it means the
  flag-off check costs a rebuild rather than being a cheap extra compile — run it
  once per milestone, not per edit.


## Decision Log

- Decision: Parameterize the core over a plain record of monadic operations
  (`ReconcileOps m`) rather than a type class.
  Rationale: There are exactly two backends, both constructed in this package; a
  record needs no instances, no newtype wrappers to select instances, and keeps the
  core's constraint to `Monad m`. A class would add extensibility nobody has asked
  for at the cost of orphan-instance risk if a consumer ever wants a custom backend.
  Date: 2026-08-05

- Decision: Keep `Pgmq.Config.Reconcile` out of the public API (list it under
  `other-modules`, not `exposed-modules`).
  Rationale: The sibling plans will change the record's fields (adding reads);
  keeping the module private makes those changes invisible to the Package Versioning
  Policy. If a consumer later wants custom backends, exposing is a deliberate,
  additive decision.
  Date: 2026-08-05

- Decision: Move the "FIFO index — no way to query if index exists, so always apply
  (idempotent)" comment into the core verbatim rather than correcting it here.
  Rationale: The comment is wrong (a `pg_indexes` catalog query can answer it, which
  is exactly what
  `docs/plans/18-report-reconciliation-truthfully-and-document-the-real-contract.md`
  will add), but this plan's contract is a pure move with zero behavior or wording
  change. Correcting prose here would blur the "nothing changed" acceptance signal.
  The comment travels to the one place EP-18 will edit it.
  Date: 2026-08-05


## Outcomes & Retrospective

Completed 2026-08-05. The reconciliation logic now exists exactly once, in the new
internal module `pgmq-config/src/Pgmq/Config/Reconcile.hs`, parameterized over a
nine-field `ReconcileOps m` record. `Pgmq.Config` shrank from 153 lines to 63 (its
whole reconciler body replaced by `ensureQueuesReport = ensureQueuesReportWith
sessionOps`), and `Pgmq.Config.Effectful` from 126 lines to 43. Neither public API
changed: the export lists, type signatures, and haddocks are byte-identical to
before, and the fourteen pre-existing pgmq-config tests pass without a single edit —
`git diff --stat -- pgmq-config/test/` is empty.

What went right: pinning the refactor on an already-green suite made the whole
milestone a mechanical move, and the two copies really were in sync, so the move was
line-for-line with no reconciliation of drift needed. The `ReconcileOps` record
approach cost nothing in constraint noise — the core needs only `Monad m`, and the
effectful backend's `(Eff.Pgmq :> es)` constraint sits entirely on `effectfulOps`.

What remains for the siblings: `ReconcileOps` is deliberately private, so
`docs/plans/17-reconcile-against-unvalidated-queue-listings-so-foreign-names-cannot-break-startup.md`
can replace the `listQueues` field and
`docs/plans/18-report-reconciliation-truthfully-and-document-the-real-contract.md`
can add fields without any Package Versioning Policy consequence. Both now edit one
file where they would previously have edited two.

Lesson worth carrying: the `-f-effectful` build is the only check that would catch
an accidental effectful import leaking into the shared core, and it is easy to
forget because the default-flag build stays green. It belongs in the acceptance run
of every future plan that touches `Pgmq.Config.Reconcile`, not just this one.


## Context and Orientation

This is a multi-package Cabal project; run all commands from the repository root
inside `nix develop` (it provides GHC, cabal-install, and the PostgreSQL binaries the
tests use — the tests provision their own temporary PostgreSQL via the `ephemeral-pg`
library, so no external database is needed). The pgmq-config package lives in
`pgmq-config/`; its cabal file is `pgmq-config/pgmq-config.cabal`.

The package has three modules today. `Pgmq.Config.Types`
(`pgmq-config/src/Pgmq/Config/Types.hs`) defines the declarative types: `QueueConfig`
(a queue's declared name, type, optional insert-notification setting, FIFO-index
flag, and topic bindings), the `QueueType` sum (`StandardQueue`, `UnloggedQueue`,
`PartitionedQueue`), `NotifyConfig`, `PartitionConfig`, smart constructors
(`standardQueue`, `unloggedQueue`, `partitionedQueue`), modifiers
(`withNotifyInsert`, `withFifoIndex`, `withTopicBinding`), and the `ReconcileAction`
report sum type. This plan does not change that module at all.

`Pgmq.Config` (`pgmq-config/src/Pgmq/Config.hs`) holds the Session-backed
reconciler. Its shape: `ensureQueuesReport :: [QueueConfig] -> Session
[ReconcileAction]` first snapshots existing state with three reads
(`Sessions.listQueues`, `Sessions.listTopicBindings`,
`Sessions.listNotifyInsertThrottles`, all from `Pgmq.Hasql.Sessions` in the
pgmq-hasql package), builds three lookup sets, and then runs a per-config helper
`reconcileQueue` that (a) creates the queue if its name is not in the existing set,
dispatching on `QueueType` to `Sessions.createQueue`, `Sessions.createUnloggedQueue`,
or `Sessions.createPartitionedQueue`; (b) enables insert notification via
`Sessions.enableNotifyInsert` if declared and not present; (c) unconditionally calls
`Sessions.createFifoIndex` when the FIFO flag is set; and (d) binds each declared
topic pattern via `Sessions.bindTopic` if the (queue, pattern) pair is absent,
through a helper `reconcileBinding`. `ensureQueues` is `() <$ ensureQueuesReport`,
and `ensureQueuesWithPool` wraps it in `Hasql.Pool.use`.

`Pgmq.Config.Effectful` (`pgmq-config/src/Pgmq/Config/Effectful.hs`) is the same
logic transcribed symbol for symbol against `Pgmq.Effectful.Effect` (imported
qualified as `Eff`): `ensureQueuesReportEff`, `reconcileQueueEff`,
`reconcileBindingEff` call `Eff.listQueues`, `Eff.createQueue`, and so on — the same
nine operations, same order, same set logic, same report construction. This module is
guarded by the cabal flag `effectful` (see the `if flag(effectful)` block in
`pgmq-config/pgmq-config.cabal`), which is on by default; the flag block adds the
module to `exposed-modules` and the `effectful-core` and `pgmq-effectful`
dependencies.

Two terms used below. A "Session" is hasql's unit of database work — a monadic
sequence of statements executed on one connection (`Hasql.Session.Session`). The
"`Pgmq` effect" is pgmq-effectful's effectful-library effect (a GADT in
`pgmq-effectful/src/Pgmq/Effectful/Effect.hs`) whose interpreters run the
corresponding `Pgmq.Hasql.Sessions` sessions against a connection pool; `Eff es a` is
a computation in the effectful monad with effects `es`.

Code style: the package uses `ImportQualifiedPost`, `OverloadedStrings`,
`OverloadedLabels` with generic-lens (`Data.Generics.Labels`), and lens operators —
field access is written `cfg ^. #queueName`. Follow that style in the new module (the
user's standing preference is generic-lens + lens, not `OverloadedRecordDot`). The
package's `common warnings` block enables `-Wmissing-export-lists`, so the new module
needs an explicit export list.

The current statement-level types the operations take (`CreatePartitionedQueue`,
`EnableNotifyInsert`, `BindTopic`) come from `Pgmq.Hasql.Statements.Types`, imported
qualified as `StmtTypes` in both existing modules. They are plain records; both
backends already construct them, and the core will construct them instead.

The fourteen existing tests live in `pgmq-config/test/` (`ConfigSpec.hs` — eleven
reconciler tests including idempotency, incremental adds, notify-throttle
preservation; `NotifyCrashSpec.hs` — three crash-recovery tests that call
`ensureQueues`). They are the behavioral pin for this refactor: none of them may be
edited, and all must pass before and after.


## Plan of Work

### Milestone 1 — the core module, with the Session backend delegating

Scope: create `pgmq-config/src/Pgmq/Config/Reconcile.hs` and make `Pgmq.Config` a
thin adapter. At the end of this milestone the Session path runs through the core and
`cabal test pgmq-config` is green with untouched tests.

Create `pgmq-config/src/Pgmq/Config/Reconcile.hs` with an explicit export list
(`ReconcileOps (..)`, `ensureQueuesReportWith`). Define the operations record with
one field per database call the reconciler makes — the three snapshot reads and six
mutations:

```haskell
-- | The database operations the reconciler needs, abstracted over the carrier
-- monad so one implementation serves both the 'Hasql.Session.Session' and
-- @Pgmq@-effect entry points.
data ReconcileOps m = ReconcileOps
  { listQueues :: m [Queue],
    listTopicBindings :: m [TopicBinding],
    listNotifyInsertThrottles :: m [NotifyInsertThrottle],
    createQueue :: QueueName -> m (),
    createUnloggedQueue :: QueueName -> m (),
    createPartitionedQueue :: StmtTypes.CreatePartitionedQueue -> m (),
    enableNotifyInsert :: StmtTypes.EnableNotifyInsert -> m (),
    createFifoIndex :: QueueName -> m (),
    bindTopic :: StmtTypes.BindTopic -> m ()
  }
  deriving stock (Generic)
```

`Queue`, `TopicBinding`, `NotifyInsertThrottle`, `QueueName` come from `Pgmq.Types`
(pgmq-core). Then move the bodies of `ensureQueuesReport`, `reconcileQueue`, and
`reconcileBinding` from `Pgmq.Config` into the core verbatim, renaming the top entry
to `ensureQueuesReportWith :: Monad m => ReconcileOps m -> [QueueConfig] -> m
[ReconcileAction]` and replacing every `Sessions.foo args` call with the
corresponding record field applied through the generic-lens label, e.g.
`ops ^. #listQueues` for the reads and `(ops ^. #createQueue) qn` for the mutations.
Keep the set-building, the action ordering (queue action, then notify, then FIFO,
then bindings), and every comment intact — this is a move, not a rewrite. The helpers
`reconcileQueue`/`reconcileBinding` gain the `ReconcileOps m` argument and lose their
module-specific suffixes.

Rewrite `Pgmq.Config` to keep its exact export list and re-exports but delegate:

```haskell
sessionOps :: ReconcileOps Session
sessionOps =
  ReconcileOps
    { listQueues = Sessions.listQueues,
      listTopicBindings = Sessions.listTopicBindings,
      listNotifyInsertThrottles = Sessions.listNotifyInsertThrottles,
      createQueue = Sessions.createQueue,
      createUnloggedQueue = Sessions.createUnloggedQueue,
      createPartitionedQueue = Sessions.createPartitionedQueue,
      enableNotifyInsert = Sessions.enableNotifyInsert,
      createFifoIndex = Sessions.createFifoIndex,
      bindTopic = Sessions.bindTopic
    }

ensureQueuesReport :: [QueueConfig] -> Session [ReconcileAction]
ensureQueuesReport = ensureQueuesReportWith sessionOps
```

`ensureQueues` and `ensureQueuesWithPool` keep their current one-line definitions.
The module's haddocks (the "Safe to call on every application startup" prose on
`ensureQueues` and `ensureQueuesReport`) stay where they are — sibling plan
`docs/plans/18-report-reconciliation-truthfully-and-document-the-real-contract.md`
owns rewording them; this plan must not change any user-facing wording.

Register the new module in `pgmq-config/pgmq-config.cabal` under the library's
`other-modules:` (create that field; the stanza currently has none). Do not add it to
`exposed-modules` (see Decision Log).

Acceptance: `cabal build pgmq-config` compiles warning-clean;
`cabal test pgmq-config` shows all 14 tests passing; `git diff --stat` shows zero
changes under `pgmq-config/test/`.

### Milestone 2 — the Effectful backend delegates

Scope: `Pgmq.Config.Effectful` becomes the second thin adapter; the duplicated logic
is deleted. At the end, both entry points share one core and the full repository
builds and tests green.

Rewrite `Pgmq.Config.Effectful` to keep its export list (`ensureQueuesEff`,
`ensureQueuesReportEff`) and define:

```haskell
effectfulOps :: (Eff.Pgmq :> es) => ReconcileOps (Eff es)
effectfulOps =
  ReconcileOps
    { listQueues = Eff.listQueues,
      listTopicBindings = Eff.listTopicBindings,
      listNotifyInsertThrottles = Eff.listNotifyInsertThrottles,
      createQueue = Eff.createQueue,
      createUnloggedQueue = Eff.createUnloggedQueue,
      createPartitionedQueue = Eff.createPartitionedQueue,
      enableNotifyInsert = Eff.enableNotifyInsert,
      createFifoIndex = Eff.createFifoIndex,
      bindTopic = Eff.bindTopic
    }

ensureQueuesReportEff :: (Eff.Pgmq :> es) => [QueueConfig] -> Eff es [ReconcileAction]
ensureQueuesReportEff = ensureQueuesReportWith effectfulOps
```

Delete `reconcileQueueEff` and `reconcileBindingEff` outright. The core module must
not import anything from pgmq-effectful (it is compiled unconditionally; the
effectful dependency exists only inside the cabal flag block — a stray import would
break `-f-effectful` builds).

Also verify the flag-off configuration still builds, since the core module is now
shared plumbing: `cabal build pgmq-config -f-effectful`.

Acceptance: `cabal build all` and `cabal test all` green;
`grep -c "reconcile" pgmq-config/src/Pgmq/Config/Effectful.hs` shows the logic is
gone (only the delegation remains). Update this plan's living sections and commit.


## Concrete Steps

All commands from the repository root
(`/Users/shinzui/Keikaku/bokuno/libraries/pgmq-hs-project/pgmq-hs`), inside
`nix develop`:

```bash
# M1
cabal build pgmq-config
cabal test pgmq-config --test-show-details=direct

# M2
cabal build pgmq-config -f-effectful
cabal build all
cabal test all --test-show-details=direct
nix fmt
```

Expected test transcript shape (unchanged from before the refactor):

```text
pgmq-config
  Pgmq.Config
    creates a standard queue:                           OK
    ...
    silent ensureQueues adds new queues incrementally:  OK
  NotifyCrashSpec
    post-crash: throttle row truncated, trigger intact: OK
    post-crash: send delivers a notification:           OK
    post-crash: a reconcile restores the throttle row:  OK

All 14 tests passed
```

Run `nix fmt` before committing (treefmt pre-commit hook enforces it). Commit with
conventional-commit style and the required trailers:

```text
refactor(pgmq-config): extract the reconciler into one backend-agnostic core

Move the duplicated Session/Effectful reconciliation logic into
Pgmq.Config.Reconcile, parameterized over a ReconcileOps record; the
public modules become thin adapters. No behavior change: the existing
14 tests pass unmodified.

MasterPlan: docs/masterplans/4-make-the-pgmq-config-reconciler-truthful-robust-and-single-sourced.md
ExecPlan: docs/plans/16-extract-the-pgmq-config-reconciler-into-a-single-backend-agnostic-core.md
Intention: intention_01kz9yszpmejztjbet6k4bvcf7
```


## Validation and Acceptance

The change is a pure refactor, so acceptance is behavioral invariance plus structural
deduplication. Behavioral: `cabal test all --test-show-details=direct` passes with
`git status` showing no modifications under any `test/` directory — in particular the
pgmq-config idempotency tests ("is idempotent (second run skips)", "ensureQueues is
truly idempotent for notify") and the three NotifyCrashSpec cases, which exercise
`ensureQueues` end-to-end against real PostgreSQL instances. Structural: the strings
`Sessions.createQueue`, `Sessions.bindTopic`, etc. appear only inside `sessionOps` in
`Pgmq.Config`, the `Eff.` equivalents only inside `effectfulOps`, and
`reconcileQueue`/`reconcileBinding` exist only in `Pgmq.Config.Reconcile`. The
flag-off build (`cabal build pgmq-config -f-effectful`) proves the core carries no
effectful dependency.


## Idempotence and Recovery

Every step is a working-tree edit plus build/test; re-running any step is safe. If a
milestone goes wrong, `git checkout -- pgmq-config/` restores the previous state
(commit M1 before starting M2 so the fallback is cheap). The refactor makes no
database-visible change, so there is nothing to recover operationally.


## Interfaces and Dependencies

No new package dependencies; the core module uses only what pgmq-config already
depends on (base, containers, generic-lens, lens, hasql via re-exported types,
pgmq-core, pgmq-hasql, text). End-state interfaces:

```haskell
-- pgmq-config, Pgmq.Config.Reconcile (internal, other-modules)
data ReconcileOps m = ReconcileOps
  { listQueues :: m [Queue],
    listTopicBindings :: m [TopicBinding],
    listNotifyInsertThrottles :: m [NotifyInsertThrottle],
    createQueue :: QueueName -> m (),
    createUnloggedQueue :: QueueName -> m (),
    createPartitionedQueue :: StmtTypes.CreatePartitionedQueue -> m (),
    enableNotifyInsert :: StmtTypes.EnableNotifyInsert -> m (),
    createFifoIndex :: QueueName -> m (),
    bindTopic :: StmtTypes.BindTopic -> m ()
  }

ensureQueuesReportWith :: Monad m => ReconcileOps m -> [QueueConfig] -> m [ReconcileAction]

-- pgmq-config, Pgmq.Config — public API unchanged:
ensureQueues :: [QueueConfig] -> Session ()
ensureQueuesWithPool :: Pool.Pool -> [QueueConfig] -> IO (Either Pool.UsageError ())
ensureQueuesReport :: [QueueConfig] -> Session [ReconcileAction]

-- pgmq-config, Pgmq.Config.Effectful — public API unchanged:
ensureQueuesEff :: (Eff.Pgmq :> es) => [QueueConfig] -> Eff es ()
ensureQueuesReportEff :: (Eff.Pgmq :> es) => [QueueConfig] -> Eff es [ReconcileAction]
```

Coordination: the two sibling plans under MasterPlan 4 hard-depend on this plan and
will extend `ReconcileOps` with additional reads
(`docs/plans/17-reconcile-against-unvalidated-queue-listings-so-foreign-names-cannot-break-startup.md`
replaces the `listQueues` field;
`docs/plans/18-report-reconciliation-truthfully-and-document-the-real-contract.md`
adds a FIFO-index read and an update operation). Nothing in this plan anticipates
those changes — it only creates the single place they land.
