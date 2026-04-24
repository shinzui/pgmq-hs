# Make pgmq-config Truly Idempotent

Intention: intention_01kpynq7gfe2w8rbczfsrj5wpm

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

This document is maintained in accordance with `.claude/skills/exec-plan/PLANS.md`.


## Purpose / Big Picture

The `pgmq-config` package advertises that its reconciliation entry points are idempotent — the
cabal synopsis says "All operations are idempotent", the README says "Every operation is
idempotent", and the haddock for `ensureQueues` says "This is idempotent — safe to call on every
application startup." The claim is only partially true today.

Two entry points exist, one per backend: `ensureQueues :: [QueueConfig] -> Session ()` in
`pgmq-config/src/Pgmq/Config.hs` and `ensureQueuesEff :: (Eff.Pgmq :> es) => [QueueConfig] -> Eff
es ()` in `pgmq-config/src/Pgmq/Config/Effectful.hs`. Both delegate to a helper (`applyQueueConfig`
and `applyQueueConfigEff` respectively) that **unconditionally** issues the queue-creation call —
`pgmq.create`, `pgmq.create_unlogged`, or `pgmq.create_partitioned` — for every entry on every
invocation. It then unconditionally issues `pgmq.enable_notify_insert`, `pgmq.create_fifo_index`,
and `pgmq.bind_topic` for any configured extras. No call to `listQueues`,
`listTopicBindings`, or `listNotifyInsertThrottles` is made.

This is wasteful and, in one case, incorrect:

- For a standard or unlogged queue, the underlying SQL uses `CREATE TABLE IF NOT EXISTS` and
  `INSERT ... ON CONFLICT DO NOTHING` (confirmed in `vendor/pgmq/pgmq-extension/sql/pgmq.sql` at
  `pgmq.create_non_partitioned` line 1088 and `pgmq.create_unlogged` line 1156), so a second
  call is a no-op that still acquires the transaction advisory lock and runs several `EXECUTE
  FORMAT` statements.
- For a **partitioned** queue, `pgmq.create_partitioned` (line 1267) calls
  `pg_partman.create_parent` twice (once for the queue table, once for the archive table).
  `pg_partman.create_parent` raises when the parent is already registered in
  `part_config`, so a second run of `ensureQueues` on a partitioned queue fails outright.
- `pgmq.enable_notify_insert` (line 1590) first calls `pgmq.disable_notify_insert`, which issues
  `DROP TRIGGER IF EXISTS trigger_notify_queue_insert_listeners`, then re-creates the trigger.
  Every boot re-creates the trigger unnecessarily.

A sibling entry point, `ensureQueuesReport :: [QueueConfig] -> Session [ReconcileAction]`, is
already written the right way: it queries existing state up front and only issues mutating calls
when a config item is missing. It returns the list of actions taken so callers can log reconciliation.

After this change, the plain `ensureQueues` and `ensureQueuesEff` functions will match the
state-checking behaviour of their reporting siblings. Callers who want a report still use
`ensureQueuesReport` / `ensureQueuesReportEff`; callers who don't care about the report still
call `ensureQueues` / `ensureQueuesEff` but get the same no-redundant-work guarantee. Calling
either entry point twice on the same database, including for partitioned queues, is a true no-op
on the second run — nothing is dropped, nothing is re-registered with pg_partman, and no extra
triggers are rebuilt.

Observable outcome: add a new test `testEnsureQueuesIsTrulyIdempotent` that calls `ensureQueues`
(not the report variant) twice and asserts the second call succeeds. Add a test that does the
same for a partitioned queue stub (skipped if `pg_partman` is not present, see "Surprises &
Discoveries"). Run `cabal test pgmq-config` and see all tests pass. Inspect query-level behaviour
by temporarily enabling PostgreSQL `log_statement = 'all'` and confirming the second
`ensureQueues` pass issues only the three `list*` SELECTs and no `pgmq.create*` calls.


## Progress

- [x] Milestone 1: Reproduce the bug with a failing test (partitioned-queue double call, or a
      statement-log assertion on a standard queue). — 2026-04-23. Added
      `testEnsureQueuesIsTrulyIdempotent` in `pgmq-config/test/ConfigSpec.hs`; it fails on HEAD
      with `last_notified_at must not be reset by second ensureQueues; was 1970-01-01 00:00:00
      UTC`, confirming the bug.
- [x] Milestone 2: Refactor `ensureQueues` in `pgmq-config/src/Pgmq/Config.hs` to share the
      reconciliation path with `ensureQueuesReport`. — 2026-04-23. `ensureQueues` now
      delegates to `ensureQueuesReport` and discards the report. `applyQueueConfig` deleted.
      `Data.Foldable (for_)` import removed (no longer needed — `reconcileQueue` uses
      `traverse`). Previously-failing `testEnsureQueuesIsTrulyIdempotent` now passes; all 8
      `pgmq-config` tests green.
- [x] Milestone 3: Refactor `ensureQueuesEff` in `pgmq-config/src/Pgmq/Config/Effectful.hs` to
      share the reconciliation path with `ensureQueuesReportEff`. — 2026-04-23. Mirror of M2
      applied; `applyQueueConfigEff` deleted; no leftover warnings.
- [x] Milestone 4: Delete now-dead `applyQueueConfig` / `applyQueueConfigEff` helpers, remove the
      stale "no way to query if index exists" comment in `Pgmq/Config.hs`, and tidy imports. —
      2026-04-23. Helpers were deleted as part of M2/M3. The FIFO comment is not stale — it
      explains why `createFifoIndex` is still called unconditionally in the shared
      `reconcileQueue` path — so it was kept in place at `Pgmq/Config.hs:122`. Import tidying
      was folded into M2 (`Data.Foldable` removed). `cabal build --ghc-options="-Werror
      -Wunused-imports"` passes. No separate commit needed.
- [x] Milestone 5: Extend the test suite with positive idempotency coverage for
      `ensureQueues` (not just `ensureQueuesReport`). — 2026-04-23. Added
      `testEnsureQueuesSilentIdempotent` and `testEnsureQueuesSilentIncremental`. The
      Milestone-1 test remains under its original name (`testEnsureQueuesIsTrulyIdempotent`);
      it already covers the "silent notify idempotent" case so no rename was needed. 10/10
      tests pass.
- [x] Milestone 6: Update `pgmq-config/CHANGELOG.md`, bump package version to `0.1.4.0`, and
      tighten the haddock on the two entry points so it accurately describes "queries existing
      state first and skips unchanged items". — 2026-04-23. Haddock already updated in
      M2/M3; cabal version bumped; CHANGELOG entry prepended. No workspace packages depend on
      `pgmq-config`, so no internal bound updates needed.
- [x] Milestone 7: `nix fmt`, `cabal build all`, `cabal test pgmq-config`, `cabal test all`
      clean. — 2026-04-23. `nix fmt` is a no-op. `cabal build all` builds cleanly
      (pgmq-core, pgmq-hasql, pgmq-migration, pgmq-effectful, pgmq-config-0.1.4.0, and
      pgmq-bench benchmark). `cabal test all` passes every suite: pgmq-migration (15),
      pgmq-config (10), pgmq-effectful (6), pgmq-hasql (55). No regressions.


## Surprises & Discoveries

- 2026-04-23 — Baseline on HEAD: all 7 pre-existing `pgmq-config` tests pass, including the
  report-variant `testEnsureQueuesIdempotent`. The bug only surfaces through the silent
  `ensureQueues` entry point and only when `last_notified_at` has been mutated away from the
  epoch default, because `pgmq.disable_notify_insert` DELETEs the throttle row and the
  subsequent INSERT re-picks the default. This confirms the plan's Milestone-1 strategy:
  assert `last_notified_at` is preserved between two `ensureQueues` calls, after a manual bump.
- 2026-04-23 — `time` was not previously a test-suite dep of `pgmq-config`; added `time
  ^>=1.14` to the `pgmq-config-test` `build-depends` to decode `timestamptz` in the new test.


## Decision Log

- Decision: Route `ensureQueues` through the existing `ensureQueuesReport` code path and discard
  the report, rather than writing a third implementation.
  Rationale: The reconciliation logic is non-trivial (three `list*` calls, three `Set`
  comparisons, per-queue per-feature branching). Having two implementations of the same logic —
  one that reports, one that doesn't — is the exact condition that let the bug ship: the report
  variant was correct and the silent variant was not. Sharing the implementation makes
  "idempotent" a single property of a single code path.
  Date: 2026-04-23

- Decision: Keep `ReconcileAction` purely a return-value concern. The silent entry points build
  the report internally and throw it away.
  Rationale: Every caller who cares about the list already uses `ensureQueuesReport` /
  `ensureQueuesReportEff`. Exposing an unused allocation to callers who chose the silent variant
  is harmless — `[ReconcileAction]` for a typical startup is a few dozen constructors at most —
  and it keeps the two entry points thin wrappers.
  Date: 2026-04-23

- Decision: The FIFO index branch stays "always call, rely on SQL `CREATE INDEX IF NOT EXISTS`"
  behaviour that already exists in `ensureQueuesReport`. Do not change it in this plan.
  Rationale: `pgmq.create_fifo_index` is SQL-level idempotent and there is no cheap query to
  detect the index independent of the queue's own tables. Adding one is out of scope; the
  comment "no way to query if index exists, so always apply (idempotent)" in `Pgmq/Config.hs`
  captures the reason. After the refactor the comment should live in the shared helper.
  Date: 2026-04-23

- Decision: Bump the version from `0.1.3.0` to `0.1.4.0` rather than `0.2.0.0`.
  Rationale: Public types and function signatures are unchanged. The behavioural change is a
  bug fix (the library now matches its documented contract). PVP treats this as a "C" bump at
  most.
  Date: 2026-04-23

- Decision: Drop the `!` breaking-change marker from the `fix(pgmq-config):` commit for
  Milestone 2. Rationale: the function signature is unchanged, the documented contract
  ("idempotent") is unchanged, and the behaviour change only removes unwanted side effects.
  Callers who depended on the old broken behaviour (trigger re-creation every boot,
  partitioned-queue failure on second call) are not a contract we want to preserve.
  Date: 2026-04-23


## Outcomes & Retrospective

**Outcome (2026-04-23).** `ensureQueues` and `ensureQueuesEff` now route through the
reporting path and are genuinely idempotent: a second call for a queue with notify-insert
configured preserves `last_notified_at` (previously reset to `to_timestamp(0)`), and the
partitioned-queue path no longer re-registers with `pg_partman`. Public signatures are
unchanged; `pgmq-config-0.1.4.0` is a pure bug-fix release.

**Traceability.** Six commits on `master`, each with the `ExecPlan:` and `Intention:`
trailers:

- M1 `bacbd12` — `test(pgmq-config): demonstrate ensureQueues is not idempotent for notify`
- M2 `e7b5df8` — `fix(pgmq-config): make ensureQueues truly idempotent`
- M3 `1c93a08` — `fix(pgmq-config): mirror idempotent ensureQueues in effectful backend`
- M4 (folded into M2/M3 — no separate commit, as anticipated by the plan)
- M5 `dfb133b` — `test(pgmq-config): positive idempotency coverage for silent ensureQueues`
- M6 `b7554b5` — `docs(pgmq-config): release 0.1.4.0 with idempotency fix`

**Test deltas.** 7 → 10 `pgmq-config` tests. 55 tests in `pgmq-hasql`, 15 in
`pgmq-migration`, and 6 in `pgmq-effectful` continued to pass unchanged.

**Lessons.**

- The report-variant (`ensureQueuesReport`) had the correct reconciliation logic all along.
  The bug was entirely in having two implementations of the same logic and only maintaining
  one of them. Routing the silent variant through the reporting path is cheaper than an
  alternative design (e.g., a shared helper with a `record actions?` flag) and keeps
  "idempotent" as a property of a single code path.
- `pgmq.enable_notify_insert`'s implementation (`DELETE ... ; INSERT ...` under the hood)
  rather than `ON CONFLICT DO NOTHING` made the bug silently destructive: a client that
  relied on `last_notified_at` as a throttle anchor would have had throttling defeated on
  every reboot.
- No need to exercise `pg_partman` to prove the bug — `last_notified_at` provided a cheaper
  reproduction that did not depend on whether the ephemeral test database has
  `pg_partman`. The partitioned-queue failure mode mentioned in the plan remains a
  known-but-not-covered scenario; it is implicitly exercised because both paths share the
  same reconciliation logic post-fix.


## Context and Orientation

The pgmq-hs project is a Haskell client library for pgmq, a PostgreSQL-based message queue. It
is organised as a multi-package Cabal project at the repository root. The package affected by
this plan is `pgmq-config`, whose job is to take a list of declarative `QueueConfig` values and
reconcile them against a live database so every declared queue exists with the configured extras
(notification throttle, FIFO index, topic bindings).

The source files you will edit in this plan:

- `pgmq-config/src/Pgmq/Config.hs` — the Hasql `Session` backend. Exports `ensureQueues`,
  `ensureQueuesWithPool`, and `ensureQueuesReport`. Currently contains a helper
  `applyQueueConfig` (lines 76–112) that does the unconditional create path, and
  `reconcileQueue` (lines 115–169) + `reconcileBinding` (lines 172–188) that do the
  state-checking path.
- `pgmq-config/src/Pgmq/Config/Effectful.hs` — the `effectful` backend behind the `effectful`
  cabal flag (default on). Exports `ensureQueuesEff` and `ensureQueuesReportEff`. Mirrors the
  same unconditional/state-checking split with `applyQueueConfigEff` and `reconcileQueueEff`.
- `pgmq-config/src/Pgmq/Config/Types.hs` — pure DSL types: `QueueConfig`, `QueueType`,
  `PartitionConfig`, `NotifyConfig`, `ReconcileAction`, plus smart constructors
  (`standardQueue`, `unloggedQueue`, `partitionedQueue`) and modifiers (`withNotifyInsert`,
  `withFifoIndex`, `withTopicBinding`). No changes expected here.
- `pgmq-config/test/ConfigSpec.hs` — the Tasty test suite. Current tests call
  `ensureQueuesReport` (the report variant) throughout. There is no test that exercises the
  silent `ensureQueues` end-to-end.
- `pgmq-config/pgmq-config.cabal` — package manifest. Version line sits at line 3 (`version:
  0.1.3.0`).
- `pgmq-config/CHANGELOG.md` — contains only the 0.1.3.0 initial-release entry.
- `docs/user/queue-configuration.md` — user-facing docs; already claims "Every operation is
  idempotent". After this change the claim is actually true; no text changes required unless
  something else becomes inaccurate.

The non-config packages in the same project that this plan reads but does not edit:

- `pgmq-hasql` — low-level Hasql client. Exports `Pgmq.Hasql.Sessions` with functions such as
  `createQueue`, `createUnloggedQueue`, `createPartitionedQueue`, `createFifoIndex`,
  `enableNotifyInsert`, `bindTopic`, `listQueues`, `listTopicBindings`,
  `listNotifyInsertThrottles`. Each wraps a single pgmq SQL call.
- `pgmq-effectful` — `Eff` wrappers over the same calls, exposed as the `Pgmq.Effectful.Effect`
  `Pgmq` effect.
- `pgmq-core` — shared types (`QueueName`, `TopicPattern`, and their smart
  parsers/renderers).

**Term definitions.** Some terms used throughout this plan, defined in plain language:

- **Idempotent** — a function is idempotent if calling it *n* times has the same observable
  effect as calling it once, for any *n ≥ 1*. Here, "observable effect" includes database
  mutations, side-effectful SQL (trigger drop/create), and raised exceptions. A function that
  raises on the second call is not idempotent.
- **Reconciliation** — the `pgmq-config` model: the user supplies a target state
  (`[QueueConfig]`), the library queries the actual database state, and the library applies the
  minimum set of mutations that brings the actual state in line with the target. Pure state
  machines call this "driving to desired state".
- **`ReconcileAction`** — a sum type in `Pgmq.Config.Types` with constructors `CreatedQueue`,
  `EnabledNotify`, `CreatedFifoIndex`, `BoundTopic`, `SkippedQueue`, `SkippedNotify`,
  `SkippedFifoIndex`, `SkippedTopicBinding`. A reconciliation returns `[ReconcileAction]`; each
  element describes one decision the reconciler made.
- **Hasql `Session`** — a monad from the `hasql` library representing a single database
  session (one pooled connection, one transaction scope per `statement` call). `Pool.use pool
  session` runs the session against a connection pool.
- **`Eff es` and `(Pgmq :> es)`** — the `effectful` library's effect monad. `(Pgmq :> es) =>
  Eff es ()` means "a computation in some effect row `es` that at minimum provides the `Pgmq`
  effect defined in `pgmq-effectful`". The `Pgmq` effect exposes functions like `Eff.createQueue`
  that mirror the Hasql `Sessions.createQueue`.
- **`pg_partman`** — a PostgreSQL extension for table partitioning. `pgmq.create_partitioned`
  calls `pg_partman.create_parent` to register the queue and archive tables with pg_partman's
  scheduled-maintenance machinery. `create_parent` raises if the parent is already registered.


## Plan of Work

Work in seven milestones. Each produces a commit that leaves the tree green (`cabal build all`
succeeds, `cabal test pgmq-config` passes; where a milestone only changes docs, "green" means
no compile regressions).

### Milestone 1 — Reproduce the bug

Before any fix, add a failing test that demonstrates the current bug. The simplest
reproduction is the partitioned-queue path: `pgmq.create_partitioned` raises on a second call
because `pg_partman.create_parent` rejects a repeat registration. However, `pg_partman` may
not be installed in the ephemeral test database — `EphemeralDb.hs` sets up pgmq via
`Migration.migrate` but there is no evidence it installs pg_partman.

Check first whether the ephemeral test database has `pg_partman` available. Run an exploratory
one-shot test:

    cd /Users/shinzui/Keikaku/bokuno/libraries/pgmq-hs-project/pgmq-hs
    cabal test pgmq-config --test-options='-p "creates a standard queue"' --test-show-details=streaming

If that works and a quick `psql` check shows `pg_partman` is unavailable, take the alternative
reproduction path: assert the no-work behaviour on a non-partitioned queue by counting PostgreSQL
`pg_stat_activity` or trigger-recreation side-effects. Concretely, the lowest-cost
reproduction is:

1. Add a test in `pgmq-config/test/ConfigSpec.hs` called `testEnsureQueuesIsTrulyIdempotent`
   that runs `ensureQueues` (the silent variant) twice, with a `withNotifyInsert` queue config,
   and asserts that between the two calls the `last_notified_at` column in
   `pgmq.notify_insert_throttle` is not reset. Under the current buggy behaviour, the second
   `ensureQueues` calls `pgmq.enable_notify_insert` → `pgmq.disable_notify_insert` → reinserts
   with `last_notified_at = to_timestamp(0)`, so the timestamp resets to the epoch. After the
   fix, the second call skips notify entirely and the timestamp stays what it was (or at least
   is not reset to epoch).

   To observe the `last_notified_at` value, read it directly via `Hasql.Session.statement` with
   a one-off `Hasql.Statement.Statement` using a `SELECT last_notified_at FROM
   pgmq.notify_insert_throttle WHERE queue_name = $1`. Define this helper inline in the test
   module under a `pg_notify_last_at` name.

2. Run the test and confirm it fails against the current `master` HEAD.

Commit (failing test, expected to be fixed by the next milestones; mark as `test(pgmq-config):`
prefix to satisfy Conventional Commits — it is legitimate new test coverage even though it is
currently red):

    test(pgmq-config): demonstrate ensureQueues is not idempotent for notify

    Add ConfigSpec.testEnsureQueuesIsTrulyIdempotent which runs ensureQueues
    (silent variant) twice and asserts last_notified_at is preserved. Fails on
    HEAD because ensureQueues unconditionally calls pgmq.enable_notify_insert,
    which drops and recreates the trigger and resets last_notified_at to
    to_timestamp(0).

    ExecPlan: docs/plans/7-pgmq-config-idempotent-ensure-queues.md
    Intention: intention_01kpynq7gfe2w8rbczfsrj5wpm

Acceptance: `cabal test pgmq-config` reports exactly one failure — the new test — and every
pre-existing test continues to pass.

If the `pg_partman` extension IS available in the ephemeral test database (surprising — record
in Surprises & Discoveries if so), use the simpler partitioned-queue reproduction instead: a
test that calls `ensureQueues [partitionedQueue qn (PartitionConfig "10000" "100000")]` twice
and expects the second call to succeed. Under the current bug the second call will raise an
error from `pg_partman.create_parent`.


### Milestone 2 — Refactor `ensureQueues` (Hasql backend)

Edit `pgmq-config/src/Pgmq/Config.hs`:

- Replace the body of `ensureQueues` so it calls `ensureQueuesReport` and discards the result:

        ensureQueues :: [QueueConfig] -> Session ()
        ensureQueues configs = () <$ ensureQueuesReport configs

- Delete the `applyQueueConfig` helper (currently lines 76–112). It is no longer used.
- Delete the `Data.Foldable (for_)` import if it becomes unused after the deletion. It IS used
  inside `reconcileQueue` for `notifyAction` (via `for_` on `cfg ^. #notifyInsert`)? Check by
  reading the file after the deletion — at the time of writing `for_` appears in two places:
  inside `applyQueueConfig` and once inside `reconcileQueue`'s `reconcileBinding` dispatch via
  `traverse`. Actually, `for_` is only used in `applyQueueConfig`; `reconcileQueue` uses
  `traverse` from Prelude. Confirm and either remove or keep the import accordingly.
- Update the haddock on `ensureQueues` to reflect the new behaviour. The new haddock should
  read approximately:

        -- | Ensure all declared queues exist with the desired settings.
        -- Queries existing queues, topic bindings, and notification throttles first,
        -- and only issues mutating calls for items that are missing. Safe to call on
        -- every application startup: a second run on an unchanged config is a no-op
        -- modulo the three list queries.
        -- Operations are additive only: queues not in the config are left untouched.

Commit:

    fix(pgmq-config)!: make ensureQueues truly idempotent

    ensureQueues previously issued pgmq.create, pgmq.enable_notify_insert,
    pgmq.create_fifo_index, and pgmq.bind_topic unconditionally on every call.
    This worked by accident for standard/unlogged queues (CREATE TABLE IF NOT
    EXISTS) but was not a no-op (trigger was recreated every boot) and failed
    outright for partitioned queues, because pg_partman.create_parent raises on
    re-registration.

    Route ensureQueues through ensureQueuesReport so it benefits from the same
    state-checking path. Drop the dead applyQueueConfig helper.

    ExecPlan: docs/plans/7-pgmq-config-idempotent-ensure-queues.md
    Intention: intention_01kpynq7gfe2w8rbczfsrj5wpm

Note on the `!` breaking marker: this is arguably not a semantic break — the function signature
is unchanged and the documented contract ("idempotent") now matches reality. Dropping the `!`
is defensible. Decide at commit time and record the call in the Decision Log. Default: omit the
`!` and describe it as a bug fix.

Acceptance: `cabal build pgmq-config` succeeds. The failing test from Milestone 1 now passes.
All other pgmq-config tests continue to pass.


### Milestone 3 — Refactor `ensureQueuesEff` (effectful backend)

Edit `pgmq-config/src/Pgmq/Config/Effectful.hs` with the same shape of change:

- Replace `ensureQueuesEff`:

        ensureQueuesEff :: (Eff.Pgmq :> es) => [QueueConfig] -> Eff es ()
        ensureQueuesEff configs = () <$ ensureQueuesReportEff configs

- Delete `applyQueueConfigEff` (currently lines 48–85).
- Tidy imports.

Commit:

    fix(pgmq-config): mirror idempotent ensureQueues in effectful backend

    Route ensureQueuesEff through ensureQueuesReportEff, matching the fix in
    the Hasql backend. Drop applyQueueConfigEff.

    ExecPlan: docs/plans/7-pgmq-config-idempotent-ensure-queues.md
    Intention: intention_01kpynq7gfe2w8rbczfsrj5wpm

Acceptance: `cabal build pgmq-config -f +effectful` (the default) succeeds.


### Milestone 4 — Cleanup and comment hygiene

Re-read both source files. Remove any remaining dead code, dead imports, or stale comments.
Specifically:

- The comment `-- FIFO index — no way to query if index exists, so always apply (idempotent)`
  currently lives at `Pgmq/Config.hs:158` inside `reconcileQueue`. Ensure it is still
  positioned next to the FIFO index branch after the refactor. It stays relevant because
  `reconcileQueue` still issues `createFifoIndex` unconditionally when requested.
- If `ReconcileAction` constructors are now only reached via the reporting path and the silent
  entry points never let users observe them, nothing changes — the type is still exported for
  the reporting API.
- Run `cabal build --ghc-options=-Wall -Wunused-imports -Werror pgmq-config` (or rely on the
  in-cabal `warnings` block which already sets `-Wall` and friends) and fix any new warnings.

Commit (only if there are changes after the previous two):

    chore(pgmq-config): drop dead imports left over from ensureQueues refactor

    ExecPlan: docs/plans/7-pgmq-config-idempotent-ensure-queues.md
    Intention: intention_01kpynq7gfe2w8rbczfsrj5wpm


### Milestone 5 — Extend test coverage

In `pgmq-config/test/ConfigSpec.hs`, add three positive idempotency tests for the silent
entry point to complement the existing `testEnsureQueuesIdempotent` (which uses the report
variant).

Add to the `tests` group and implement:

- `testEnsureQueuesSilentIdempotent` — calls `ensureQueues` twice with a standard-queue
  config, asserts no error, asserts the queue exists exactly once in `Sessions.listQueues`.
- `testEnsureQueuesSilentNotifyIdempotent` — the test added in Milestone 1 stays; verify it
  now passes. If it lives under a different name in Milestone 1 (`testEnsureQueuesIsTrulyIdempotent`),
  rename it to this canonical name before this milestone's commit.
- `testEnsureQueuesSilentIncremental` — first call `ensureQueues [standardQueue qn1]`, then
  call `ensureQueues [standardQueue qn1, standardQueue qn2]`, and assert both queues exist
  afterward. This mirrors `testEnsureQueuesIncremental` for the silent variant.

If the Milestone-1 test's reproduction path was partitioned-queue-based (pg_partman available),
also keep a `testEnsureQueuesSilentPartitionedIdempotent` test.

Commit:

    test(pgmq-config): positive idempotency coverage for silent ensureQueues

    ExecPlan: docs/plans/7-pgmq-config-idempotent-ensure-queues.md
    Intention: intention_01kpynq7gfe2w8rbczfsrj5wpm


### Milestone 6 — Changelog and version

Edit `pgmq-config/pgmq-config.cabal` line 3:

    version:         0.1.4.0

Prepend a new entry to `pgmq-config/CHANGELOG.md`:

    ## 0.1.4.0 -- 2026-04-23

    * Fix: `ensureQueues` and `ensureQueuesEff` are now truly idempotent. Previously they
      issued `pgmq.create`, `pgmq.enable_notify_insert`, `pgmq.create_fifo_index`, and
      `pgmq.bind_topic` unconditionally on every call, which worked by accident for
      standard/unlogged queues but caused trigger recreation on every boot and failed outright
      for partitioned queues (because `pg_partman.create_parent` raises on re-registration).
      Both entry points now route through the existing state-checking reconciliation path.

Commit:

    docs(pgmq-config): release 0.1.4.0 with idempotency fix

    ExecPlan: docs/plans/7-pgmq-config-idempotent-ensure-queues.md
    Intention: intention_01kpynq7gfe2w8rbczfsrj5wpm


### Milestone 7 — Whole-project validation

Run formatting, full build, and full test suite. If any cross-package breakage falls out,
address it and note in Surprises & Discoveries.

    cd /Users/shinzui/Keikaku/bokuno/libraries/pgmq-hs-project/pgmq-hs
    nix fmt
    cabal build all
    cabal test all

Acceptance: no formatting changes after `nix fmt` (tree is stable), `cabal build all` succeeds,
`cabal test all` reports zero failures across all packages (pgmq-core, pgmq-hasql,
pgmq-effectful, pgmq-migration, pgmq-config).

If `nix fmt` made changes, commit them:

    style: apply treefmt

    ExecPlan: docs/plans/7-pgmq-config-idempotent-ensure-queues.md
    Intention: intention_01kpynq7gfe2w8rbczfsrj5wpm


## Concrete Steps

All commands are run from
`/Users/shinzui/Keikaku/bokuno/libraries/pgmq-hs-project/pgmq-hs` unless noted.

1. Confirm the current reproduction target. Build the package and run the existing tests:

        cabal build pgmq-config
        cabal test pgmq-config

   Expected: build succeeds; all 7 existing tests pass. This is the baseline.

2. Add the failing test from Milestone 1. Edit `pgmq-config/test/ConfigSpec.hs` to add a new
   test case. Re-run:

        cabal test pgmq-config --test-show-details=streaming

   Expected: one failure in `Pgmq.Config` test group, named per Milestone 1.

3. Apply the refactor in `pgmq-config/src/Pgmq/Config.hs` per Milestone 2. Re-run:

        cabal build pgmq-config
        cabal test pgmq-config

   Expected: build succeeds; all 8 tests pass.

4. Apply the mirror refactor in `pgmq-config/src/Pgmq/Config/Effectful.hs` per Milestone 3.
   Re-run the build with the effectful flag (enabled by default):

        cabal build pgmq-config
        cabal test pgmq-config

   Expected: build and test still clean.

5. Add the extra tests from Milestone 5, bump the version and changelog from Milestone 6.

        cabal test pgmq-config

   Expected: all 10 or 11 tests pass (depending on whether partitioned-queue coverage was
   added).

6. Full-tree validation from Milestone 7:

        nix fmt
        cabal build all
        cabal test all

   Expected: no formatter diffs; all packages build; all test suites green. A rough
   historical count is "68 tests" per the add-pgmq-config-package.md plan; today's number
   will be higher and must remain non-regressing.


## Validation and Acceptance

The change is considered accepted when all of the following hold.

**Behavioural acceptance.** A user can write a script (or a test) that calls `ensureQueues`
twice in succession with a `partitionedQueue` config and observes no exception from the second
call. This works only if `pg_partman` is installed in the target database; in the ephemeral
test database `pg_partman` may or may not be available (see Milestone 1). For non-partitioned
configs, a user can call `ensureQueues` twice with a `withNotifyInsert` config and observe that
the row in `pgmq.notify_insert_throttle` for the queue is not rewritten between calls —
specifically, `last_notified_at` is not reset to `to_timestamp(0)`.

**Test acceptance.** All tests in `cabal test pgmq-config` pass, including:

- Pre-existing: `creates a standard queue`, `creates an unlogged queue`, `is idempotent
  (second run skips)`, `creates only new queues incrementally`, `enables notify insert`,
  `creates FIFO index`, `binds topic pattern`.
- New: `testEnsureQueuesSilentIdempotent`, `testEnsureQueuesSilentNotifyIdempotent`,
  `testEnsureQueuesSilentIncremental`, and the optional partitioned variant if it was
  feasible.

**Project acceptance.** `cabal test all` reports zero failures across every package. `nix fmt`
is a no-op.

**Documentation acceptance.** `pgmq-config/CHANGELOG.md` has a `0.1.4.0` entry describing the
fix. `pgmq-config/pgmq-config.cabal` `version:` field reads `0.1.4.0`. The haddock on
`ensureQueues` no longer claims bare idempotency — it states that the function queries existing
state first.


## Idempotence and Recovery

All milestones are additive or purely refactoring. If any milestone's commit breaks the build,
revert it and retry:

    git log --oneline -n 10
    git revert <sha>

The working tree is safe to re-run any milestone from scratch. Because each milestone produces
a separate commit with an `ExecPlan:` trailer, partial progress is fully traceable.

The one destructive-looking operation — deleting `applyQueueConfig` — is safe because the
helper is no longer referenced; `ensureQueues` is the only caller and it is rewritten in the
same commit. `cabal build` catches any missed reference at compile time.

No database migration or persistent state change is required. Existing users on 0.1.3.0 who
upgrade to 0.1.4.0 see no schema change.


## Interfaces and Dependencies

No new library dependencies are introduced. All affected code already depends on:

- `hasql`, `hasql-pool` — for `Session` and `Pool`.
- `effectful-core` — for `Eff es` and the `Pgmq` effect (under the `effectful` cabal flag,
  default on).
- `pgmq-core`, `pgmq-hasql`, `pgmq-effectful` — already used for the existing reconciliation
  path.
- `containers`, `generic-lens`, `lens`, `text` — already used for `Set` construction and field
  access.

Function signatures that must exist at the end of this plan, all unchanged from today:

- In `pgmq-config/src/Pgmq/Config.hs`:

        ensureQueues :: [QueueConfig] -> Session ()
        ensureQueuesWithPool :: Pool.Pool -> [QueueConfig] -> IO (Either Pool.UsageError ())
        ensureQueuesReport :: [QueueConfig] -> Session [ReconcileAction]

- In `pgmq-config/src/Pgmq/Config/Effectful.hs`:

        ensureQueuesEff :: (Eff.Pgmq :> es) => [QueueConfig] -> Eff es ()
        ensureQueuesReportEff :: (Eff.Pgmq :> es) => [QueueConfig] -> Eff es [ReconcileAction]

Behavioural contract, strengthened: both `ensureQueues` and `ensureQueuesEff` query existing
queues, topic bindings, and notification throttles before issuing any mutating call, and skip
mutations for config items that are already present. Both are no-ops on an unchanged config
modulo the three list queries. Both remain additive-only: queues not in the config are never
dropped.
