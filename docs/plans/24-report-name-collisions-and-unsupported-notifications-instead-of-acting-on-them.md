---
id: 24
slug: report-name-collisions-and-unsupported-notifications-instead-of-acting-on-them
title: "Report name collisions and unsupported notifications instead of acting on them"
kind: exec-plan
created_at: 2026-09-16T20:42:28Z
intention: "intention_01m2nz9a82ejh91yg9sk7t6a7a"
master_plan: "docs/masterplans/6-close-the-notification-reconciler-and-evidence-gaps-surfaced-by-the-0-6-1-0-review.md"
provenance:
  created_by:
    model: "claude-fable-5-1"
    harness: "claude-code"
    at: 2026-09-16T20:42:28Z
---

# Report name collisions and unsupported notifications instead of acting on them

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.
If durable project context changes, update or create ADRs in docs/adr/ in the same change.


## Purpose / Big Picture

`pgmq-config` lets an application declare its pgmq queues as Haskell values and call
`ensureQueues` at startup to create whatever is missing. It returns (through
`ensureQueuesReport`) one `ReconcileAction` per decision so the application can log exactly
what happened. The contract, written on `ensureQueues` and in design note 018, is "additive;
report what cannot be repaired; mutate existing state in exactly one documented case".

Two situations violate that contract today by acting where they should report, and one
sentence overclaims.

A queue created by another client under a mixed-case name (`Billing_Events`) shares its
physical table `pgmq.q_billing_events` with the lowercase name this library accepts
(`billing_events`), because pgmq lowercases table names but stores the caller's casing in its
registry `pgmq.meta`. The reconciler matches declared names against `pgmq.meta` textually, so
a config declaring `billing_events` sees no match and calls `pgmq.create('billing_events')`,
which is a no-op on the table but inserts a second registry row. Two registry rows over one
table is the aliasing hazard design note 016 documents: dropping either destroys the other's
messages. After this plan the reconciler reports `DetectedQueueNameCollision billing_events
("Billing_Events" :| [])`, creates nothing, and applies none of that config's notification,
FIFO-index, or topic-binding declarations.

A notification declared on a partitioned queue (`partitionedQueue qn pc & withNotifyInsert ms`)
is enabled today, which installs a trigger that can never deliver on the documented channel:
PostgreSQL clones the trigger onto leaf partitions, where the extracted queue name never
matches a throttle row. After this plan the reconciler reports
`UnsupportedNotifyOnPartitionedQueue qn` and issues no enable or update.

`DetectedQueueTypeDrift`'s Haddock says "Nothing was mutated and nothing will be". The queue
itself is left alone, but the config's notification, FIFO-index, and topic-binding steps still
run against the queue as it exists and each reports its own action. After this plan the
Haddock, the `ensureQueues` contract, design note 018, and the user guide say exactly that; the
guide's `ReconcileAction` listing shows every constructor (today it lacks the two 0.5.0.0
added), and its "every operation is idempotent" sentence points at the real contract.

You can see it working by running the two new tests in `pgmq-config`'s suite: the collision
test seeds a mixed-case queue on a dedicated server and shows one registry row before and
after a reconcile that declares the lowercase name; the partition test shows no trigger and no
throttle row after reconciling a partitioned queue with a notification declared.


## Progress

- [ ] M1: `pgmq-config/test/CollisionSpec.hs` (dedicated server) red on both backends: today a second `pgmq.meta` row appears
- [ ] M1: `pgmq-config/test/PartitionSpec.hs` case "notify on a partitioned queue is reported unsupported" red on both backends: today a trigger and a throttle row appear
- [ ] M2: `DetectedQueueNameCollision` and `UnsupportedNotifyOnPartitionedQueue` added to `ReconcileAction`; `reconcileQueue` reports them; both tests green
- [ ] M2: `cabal build pgmq-config -f-effectful` and `cabal test pgmq-config -f-effectful` green
- [ ] M3: Haddocks on `DetectedQueueTypeDrift`, `ensureQueues`, and the new constructors; `ObservedQueueType` unchanged
- [ ] M3: design note 018 section on collisions and unsupported notifications; drift paragraph corrected
- [ ] M3: `docs/user/queue-configuration.md` Notify Insert, Reconciliation Functions, and Idempotency Guarantees sections corrected
- [ ] M3: `docs/capabilities/declarative-queue-reconciliation.md` limits and evidence updated, bundle log entry added
- [ ] M3: "Unreleased" sections in `CHANGELOG.md` and `pgmq-config/CHANGELOG.md`
- [ ] `nix fmt`, `git diff --check`, `just docs-check` clean


## Surprises & Discoveries

Document unexpected behaviors, bugs, optimizations, or insights discovered during
implementation. Provide concise evidence.

(None yet.)


## Decision Log

- Decision: A collision is one action per declared config carrying every colliding observed
  name as a `NonEmpty Text`, emitted whenever any observed name other than the declared one
  lowercases to it. When the exact row is missing the collision *replaces* creation and
  suppresses the config's sub-resource steps; when the exact row is also present the
  collision is reported in addition to the ordinary existence action and the sub-resources
  proceed, because the exact row is real.
  Rationale: the report keeps "one queue-existence action per config" true (the collision
  stands in for creation only when creation would alias); sub-resources are suppressed only
  when they would target a table the declared name does not own. `NonEmpty` says
  non-emptiness in the type rather than in a comment.
  Date: 2026-09-16
- Decision: The partitioned-notification report keys on the queue's *effective* shape:
  the observed shape when the queue exists, the declared shape when this run created it.
  A pre-existing throttle row on a partitioned queue is also reported as unsupported, with no
  update or re-enable.
  Rationale: `pgmq.list_queues()` reports `is_partitioned`, so the shape is known without a
  new query; a config that declares `standardQueue` over an observed partitioned queue
  already reports drift and must not also install a trigger on the partitioned table.
  Date: 2026-09-16
- Decision: No new `ReconcileOps` field. Both detections use the existing unvalidated
  listing and the existing throttle listing.
  Rationale: `ReconcileOps` is internal but every added field is one more thing each backend
  must wire; nothing here needs a new database call.
  Date: 2026-09-16
- Decision: The drift overclaim is fixed by wording, not by skipping sub-resource steps.
  Rationale: recorded in the MasterPlan; the steps are additive and truthfully reported.
  Date: 2026-09-16


## Outcomes & Retrospective

Summarize outcomes, gaps, and lessons learned at major milestones or at completion.
Compare the result against the original purpose. Before marking the plan complete,
distill durable project context from the Decision Log, Surprises & Discoveries, and
this section into docs/adr/. Keep task-local execution details here.

(To be filled during and after implementation.)


## Context and Orientation

### The repository in one paragraph

`pgmq-hs` is a multi-package Cabal project: `pgmq-core` (types), `pgmq-hasql` (SQL statements
and sessions over the hasql driver), `pgmq-effectful` (an effect wrapper), `pgmq-config` (this
plan's package), and `pgmq-migration` (the extension-free installer). The toolchain comes from
Nix: `nix develop` gives GHC 9.12 and cabal; `nix develop .#partman` is the same shell with
the `pg_partman` extension available and `PGMQ_REQUIRE_PARTMAN=1` exported so partition tests
fail instead of skipping. Tests start disposable PostgreSQL servers through `ephemeral-pg`.
Run `nix fmt` before committing and `git diff --check` for whitespace. `just docs-check`
validates the documentation bundles. Field access in this codebase uses `generic-lens` labels
with `Control.Lens` (`cfg ^. #queueName`), not `OverloadedRecordDot`.

### pgmq facts this plan relies on

pgmq keeps one registry table, `pgmq.meta`, with a unique `queue_name` column plus
`is_partitioned`, `is_unlogged`, and `created_at`. `pgmq.create(name)` runs
`CREATE TABLE IF NOT EXISTS pgmq.q_<lower(name)>` and `INSERT INTO pgmq.meta ... ON CONFLICT DO NOTHING`
with the name *as given*, so `pgmq.create('Billing_Events')` followed by
`pgmq.create('billing_events')` yields one table and two registry rows. The server validates
only length (47 characters) and a few punctuation characters; this library's
`Pgmq.Types.parseQueueName` accepts only lowercase ASCII letters, digits, and underscore, so a
mixed-case row can only come from another client. `pgmq.list_queues()` returns the registry.
`pgmq.enable_notify_insert(name, ms)` inserts a throttle row and creates the trigger
`trigger_notify_queue_insert_listeners` on `pgmq.q_<lower(name)>`; on a partitioned table
PostgreSQL clones the trigger onto every leaf partition, and the cloned trigger's
`TG_TABLE_NAME` is the partition, so the trigger's throttle lookup never matches. The sibling
plan `docs/plans/23-gate-the-notification-fail-open-on-a-real-queue-row-and-state-the-partitioned-queue-contract.md`
records the evidence and makes migration `0007` stop the resulting notification storm on
native installs; on every install the fact this plan reports is the same: partitioned queues
receive no insert notifications on the channel `notifyChannelName` computes.

### The reconciler today

`pgmq-config/src/Pgmq/Config/Reconcile.hs` is the single backend-agnostic core. It is an
internal module (`other-modules` in `pgmq-config/pgmq-config.cabal`). `ReconcileOps m` is a
record of the twelve database operations the reconciler needs; `Pgmq.Config` wires it to
`Hasql.Session` and `Pgmq.Config.Effectful` to the `Pgmq` effect (the latter only when the
`effectful` cabal flag is on, which it is by default, so always also build with
`-f-effectful`). `ensureQueuesReportWith` takes four snapshots — the *unvalidated* queue
listing (names as `Text`, so a foreign name cannot fail decoding), topic bindings, throttle
rows (name and interval), and the names that already have a FIFO index — then folds
`reconcileQueue` over the configs. The existence decision today is:

```haskell
  queueAction <-
    case Map.lookup qnText existingQueues of
      Just observed ->
        let observedType = observedQueueType observed
         in pure
              [ if declaredShape declaredType == observedType
                  then SkippedQueue qn
                  else DetectedQueueTypeDrift qn declaredType observedType
              ]
      Nothing -> do
        case declaredType of
          StandardQueue -> (ops ^. #createQueue) qn
          UnloggedQueue -> (ops ^. #createUnloggedQueue) qn
          PartitionedQueue pc -> ... createPartitionedQueue / createPartitionedQueueWithPremake
        pure [CreatedQueue qn declaredType]
```

followed unconditionally by the notification step (enable when no throttle row, skip when the
interval matches, update when it differs), the FIFO step, and the binding step, and finally
`pure (queueAction ++ notifyAction ++ fifoAction ++ bindingActions)`.

`ReconcileAction` in `pgmq-config/src/Pgmq/Config/Types.hs` has ten constructors:
`CreatedQueue`, `EnabledNotify`, `CreatedFifoIndex`, `BoundTopic`, `SkippedQueue`,
`SkippedNotify`, `SkippedFifoIndex`, `SkippedTopicBinding`, `UpdatedNotifyThrottle`,
`DetectedQueueTypeDrift`. `ObservedQueueType` is `ObservedStandard | ObservedUnlogged |
ObservedPartitioned`. `observedQueueType` derives it from the two booleans. The
`DetectedQueueTypeDrift` Haddock contains the sentence "Nothing was mutated and nothing will
be: converting a queue between standard, unlogged, and partitioned means dropping and
recreating it ...". `ensureQueues` in `pgmq-config/src/Pgmq/Config.hs` carries the canonical
contract Haddock with a "What is /not/ reconciled" list; `ensureQueuesReport`, `ensureQueuesEff`,
and `ensureQueuesReportEff` point at it.

### Tests today

`pgmq-config/test/Main.hs` runs `ConfigSpec.tests pool` on a shared pool, plus
`PartitionSpec.tests`, `NotifyCrashSpec.tests`, and `ForeignQueueSpec.tests`, the last two on
their own servers because they create state that would break concurrent tests on the shared
pool. `ForeignQueueSpec.hs` is the template for a dedicated-server spec: it starts its own
`ephemeral-pg` instance, applies the native ledger with `runMigrationPlan`, seeds a foreign
queue through raw SQL, runs a reconcile through both the Session backend and (under the
`PGMQ_EFFECTFUL` CPP flag) the effect backend, and reports observations as separate test cases.
`PartitionSpec.hs` is the template for a partition case: `isolated` gives a fresh database per
case, `withPartman` skips or fails depending on `PGMQ_REQUIRE_PARTMAN`, `reconcile effectful pool configs`
runs either backend, and `counts` reads pg_partman's `part_config`. Its existing cases match
the report exactly (`[CreatedQueue q _]`, `[SkippedQueue q]`) and do not declare notifications,
so they are unaffected by this plan. `ConfigSpec.hs` has
`testEnsureQueuesReportsQueueTypeDrift`, which asserts a `DetectedQueueTypeDrift` action is
present and the queue is untouched.

### Documents that carry the contract

`docs/design/018-reconciliation-contract.md` explains why the throttle interval is mutated and
the queue type is not, and lists what is not reconciled. `docs/user/queue-configuration.md`
has a "Notify Insert" modifier section, a "Reconciliation Functions" section whose
`ReconcileAction` listing stops at `SkippedTopicBinding` (it predates 0.5.0.0), and an
"Idempotency Guarantees" section that says "Every underlying pgmq operation is idempotent"
and, in its opening paragraph, "Every operation is idempotent — calling it repeatedly is safe
and produces no errors or duplicate side effects". `docs/capabilities/declarative-queue-reconciliation.md`
is the capability record (front matter validated by `just docs-check`, with an `evidence`
list of tests) and already states "drift reported not repaired".

### ADR context

There is no profiled `docs/adr` bundle. No ADR governs the reconciler directly; its durable
contract lives in design note 018, which this plan updates.
[docs/adr/pgmq-1.12-1.13-compatibility.md](../adr/pgmq-1.12-1.13-compatibility.md) is
relevant only for its rule that `ReconcileOps` is internal and premake is creation-only,
neither of which this plan changes. The parent MasterPlan is
[docs/masterplans/6-close-the-notification-reconciler-and-evidence-gaps-surfaced-by-the-0-6-1-0-review.md](../masterplans/6-close-the-notification-reconciler-and-evidence-gaps-surfaced-by-the-0-6-1-0-review.md),
whose Decision Log records why collisions and unsupported notifications are reported rather
than repaired or rejected, and why the drift sentence is reworded rather than the behavior
changed.


## Plan of Work

### Milestone 1: two red tests

Scope: prove both defects with tests that fail today on both backends. At the end, the suite
has a new dedicated-server spec and a new partition case, both red.

Create `pgmq-config/test/CollisionSpec.hs` modeled on `ForeignQueueSpec.hs`: start a dedicated
server, apply the ledger, seed `SELECT pgmq.create('Billing_Events')` through a raw session
script, then for each backend run
`ensureQueuesReport [standardQueue billingEvents & withNotifyInsert (Just 0) & withFifoIndex, standardQueue other]`
where `billingEvents = parseQueueName "billing_events"` and `other` is an unrelated name, and
record: the actions; the number of rows in `pgmq.meta` whose `lower(queue_name) = 'billing_events'`
(via a raw statement); whether a throttle row exists for `billing_events`; whether an index
named `q_billing_events_fifo_idx` exists in `pg_indexes`. Assert, as separate test cases: the
actions for `billing_events` are exactly `[DetectedQueueNameCollision billingEvents ("Billing_Events" :| [])]`
(pattern-match the constructor and compare fields; `ReconcileAction` has no `Eq`); the
registry still has one row for that table; no throttle row and no FIFO index were created;
and `other` reports `CreatedQueue`. Seed and reconcile in one fresh database per backend so
the second backend does not see the first's rows. Register the spec in
`pgmq-config/test/Main.hs` with the same comment style as `ForeignQueueSpec` (it must run on
its own server because a mixed-case row would fail concurrent typed `listQueues` calls on the
shared pool) and add it to the suite's `other-modules` in `pgmq-config/pgmq-config.cabal`.

In `pgmq-config/test/PartitionSpec.hs`, add a case to `backendTests`:
"notify on a partitioned queue is reported unsupported". Under `isolated` and `withPartman`,
reconcile `[partitionedQueue qn (PartitionConfig "10" "100" Nothing) & withNotifyInsert (Just 0)]`
and expect the actions to match `[CreatedQueue q _, UnsupportedNotifyOnPartitionedQueue q']`
with both names equal to `qn`; then read `SELECT count(*) FROM pg_trigger t JOIN pg_class c ON c.oid = t.tgrelid WHERE c.relname = 'q_' || $1 AND t.tgname = 'trigger_notify_queue_insert_listeners'`
and expect `0`, and `SELECT count(*) FROM pgmq.notify_insert_throttle WHERE queue_name = $1` and
expect `0`; reconcile again and expect `[SkippedQueue q, UnsupportedNotifyOnPartitionedQueue q']`.
Add a second case for the pre-existing-row shape: create the partitioned queue, call
`pgmq.enable_notify_insert(qn, 250)` through a raw statement, reconcile with
`withNotifyInsert (Just 500)`, and expect `UnsupportedNotifyOnPartitionedQueue` with no
`UpdatedNotifyThrottle` and the stored interval still `250`.

Both tests must not compile until Milestone 2 adds the constructors; that is acceptable for
the red state only if you first commit the tests referencing the constructors together with
the type change in Milestone 2 and observe the assertion failures. To keep a genuinely red
commit, write the tests first against the *behavior* (registry row count, trigger count,
throttle row) without naming the new constructors, observe them fail, commit, then tighten
the action assertions in Milestone 2.

Run in the `partman` shell:

```bash
nix develop .#partman --command cabal test pgmq-config --test-show-details=direct --test-options='-p "/CollisionSpec/ || /unsupported/"'
```

Expected red output (abbreviated): the collision case reports `2` registry rows where `1` was
expected and a throttle row present; the partition case reports `1` trigger where `0` was
expected and `1` throttle row.

### Milestone 2: report, do not act

Scope: add the two constructors and the two detections; both new tests and every existing
test pass on both backends and with the effectful flag off.

In `pgmq-config/src/Pgmq/Config/Types.hs`, import `Data.List.NonEmpty (NonEmpty)` and add to
`ReconcileAction`:

```haskell
  | -- | A queue with a different casing of this name exists in @pgmq.meta@ and
    -- shares the declared name's physical table (pgmq lowercases table names but
    -- stores the caller's casing). Fields: declared queue, every colliding
    -- registry name. When no row with the exact declared name exists, this action
    -- replaces 'CreatedQueue': creating would insert a second registry row over
    -- the same table, the aliasing hazard @docs/design/016-queue-name-validation.md@
    -- remediates, and none of the config's notification, FIFO-index, or
    -- topic-binding declarations are applied. When the exact row also exists,
    -- the ordinary existence action is reported as well and the config's other
    -- declarations proceed against that row. Nothing is mutated by this action;
    -- resolving the collision is the operator procedure in design note 016.
    DetectedQueueNameCollision !QueueName !(NonEmpty Text)
  | -- | The config declares insert notifications on a queue that is partitioned
    -- (observed, or created as such by this run). pgmq's insert trigger is cloned
    -- onto leaf partitions and never matches a throttle row there, so notifications
    -- are never delivered on 'Pgmq.Types.notifyChannelName' for a partitioned
    -- queue on any install; see @docs/design/015-notification-delivery-contract.md@.
    -- Nothing was enabled, updated, or re-enabled. A throttle row that another
    -- client already installed is left as it is.
    UnsupportedNotifyOnPartitionedQueue !QueueName
```

Rewrite the `DetectedQueueTypeDrift` Haddock paragraph that begins "Nothing was mutated and
nothing will be" to: "The queue itself is not mutated and never will be: converting it between
standard, unlogged, and partitioned means dropping and recreating it, destroying every message
it holds, which a startup reconciler must never do. The config's notification, FIFO-index, and
topic-binding declarations are still reconciled against the queue as it exists, and each
reports its own action. Resolving the drift is an operator decision."

In `pgmq-config/src/Pgmq/Config/Reconcile.hs`: in `ensureQueuesReportWith`, build
`collisionsByLowerName :: Map.Map T.Text [T.Text]` from the unvalidated listing
(`Map.fromListWith (++) [(T.toLower n, [n]) | n <- Map.keys existingQueuesByName]`) and pass
it to `reconcileQueue`. In `reconcileQueue`, compute
`colliding = filter (/= qnText) (Map.findWithDefault [] qnText collisionsByLowerName)`
(declared names are already lowercase, so the key is `qnText`). Restructure the body so that:

- if the exact row is missing and `colliding` is non-empty, return
  `[DetectedQueueNameCollision qn (NE.fromList colliding)]` immediately (use
  `Data.List.NonEmpty.nonEmpty` and pattern-match rather than `fromList`, so the empty case is
  handled by types);
- otherwise compute `queueAction` as today, appending `DetectedQueueNameCollision qn cs` after
  the existence action when `nonEmpty colliding` is `Just cs`;
- compute `effectiveShape` as `observedQueueType observed` for an existing row or
  `declaredShape declaredType` for a created one;
- in the notification step, when `cfg ^. #notifyInsert` is `Just _` and `effectiveShape == ObservedPartitioned`,
  return `[UnsupportedNotifyOnPartitionedQueue qn]` without consulting the throttle map;
  otherwise keep the existing enable/skip/update logic unchanged.

Keep `ReconcileOps` unchanged. Keep every existing action's semantics unchanged: a config
without collision or partitioned notification produces exactly the actions it produces today
(the existing `ConfigSpec` cases pin that).

Then tighten the Milestone 1 tests to assert the constructors, and run:

```bash
nix develop .#partman --command cabal test pgmq-config --test-show-details=direct
nix develop .#partman --command cabal build pgmq-config -f-effectful
nix develop .#partman --command cabal test pgmq-config -f-effectful --test-show-details=direct
```

The flag-off build matters: plans 16 and 17 both found that an accidental `effectful` import
in the shared core or in a test only breaks with `-f-effectful`, and the default build stays
green.

### Milestone 3: say what the reconciler does

Scope: every document that states the contract matches Milestone 2, and the changelog carries
the breaking change.

In `pgmq-config/src/Pgmq/Config.hs`, extend the `ensureQueues` Haddock: in the "What is /not/
reconciled" list, add a bullet for name collisions (report, no creation, no sub-resources when
the exact row is missing) and one for notifications on partitioned queues (report, nothing
installed, link design note 015); reword the drift bullet to match the new
`DetectedQueueTypeDrift` Haddock; in the `ensureQueuesReport` paragraph that says "exactly one
queue-existence action per config", add that `DetectedQueueNameCollision` is that action when
the declared row is missing and an additional action when it is present.

In `docs/design/018-reconciliation-contract.md`, add a section "Two more things that are
reported, not repaired" covering the collision (why creating would alias, with the
`pgmq.create` mechanics above; why the operator procedure in design note 016 is the repair)
and the partitioned notification (why the trigger cannot deliver; that `0007` stops the storm
on native installs but delivery is absent on every install), and correct the drift wording
where it says nothing is mutated.

In `docs/user/queue-configuration.md`: in "Notify Insert", state that notifications on a
partitioned queue are reported as `UnsupportedNotifyOnPartitionedQueue` and never enabled; in
"Reconciliation Functions", replace the `ReconcileAction` listing with all twelve constructors
and their fields, and add `DetectedQueueNameCollision` and `UnsupportedNotifyOnPartitionedQueue`
to the example's commentary; replace the opening sentence "Every operation is idempotent —
calling it repeatedly is safe and produces no errors or duplicate side effects" with a
sentence that a second run against an unchanged config issues no mutations and that the one
in-place mutation (throttle drift) and the reported-not-repaired cases are described on
`ensureQueues`; in "Idempotency Guarantees", keep the per-function list but add that the
reconciler does not call `pgmq.create` for a colliding name or `pgmq.enable_notify_insert` for
a partitioned queue.

In `docs/capabilities/declarative-queue-reconciliation.md`, add two Limits bullets (collision
and partitioned notification, both "reported, not repaired") and two `evidence` entries of
kind `test`: `pgmq-config/test/CollisionSpec.hs` (`proves: A declared name that collides with a foreign mixed-case registry row is reported and not created over the shared table.`)
and `pgmq-config/test/PartitionSpec.hs` (`proves: A notification declared on a partitioned queue is reported as unsupported and no trigger or throttle row is installed.`).
Then `okf log add docs/capabilities --kind Update -m "CAP-<n>: name collisions and partitioned notifications are reported, not repaired"`
(use the capability's actual id from its front matter) and `okf index docs/capabilities --write`
if the preview differs.

Changelogs: add "## Unreleased" at the top of `pgmq-config/CHANGELOG.md` with a "Breaking"
paragraph (two new constructors; exhaustive matchers must handle them; `Data.List.NonEmpty`
appears in the type) and a "Fixes" paragraph for each behavior, and a matching paragraph under
"## Unreleased" in the root `CHANGELOG.md` (create the section if EP-1 has not). Never edit a
published section.


## Concrete Steps

All commands run from the repository root
`/Users/shinzui/Keikaku/bokuno/libraries/pgmq-hs-project/pgmq-hs`.

```bash
nix develop .#partman --command cabal test pgmq-config --test-show-details=direct
nix develop .#partman --command cabal build pgmq-config -f-effectful
nix develop .#partman --command cabal test pgmq-config -f-effectful --test-show-details=direct
PGMQ_TEST_SCHEMA_VERSION=1.12.0 nix develop .#partman --command cabal test pgmq-config --test-show-details=direct
nix fmt
git diff --check
just docs-check
cabal haddock pgmq-config 2>&1 | grep -E '^\s*[0-9]+% '
```

Expected after Milestone 2: every `pgmq-config` case passes on both backends
(`ConfigSpec`, `PartitionSpec` including the two new cases under both `direct` and
`effectful`, `NotifyCrashSpec`, `ForeignQueueSpec`, `CollisionSpec`), the flag-off build and
test pass, and the stock 1.12 selection passes (the partition cases there use `premake = Nothing`,
which 1.12 supports). Expected after Milestone 3: `nix fmt` is a no-op on a second run,
`git diff --check` prints nothing, `just docs-check` exits zero, and `Pgmq.Config` and
`Pgmq.Config.Types` report `100%` Haddock coverage.

Commit after each milestone with the trailers

```text
MasterPlan: docs/masterplans/6-close-the-notification-reconciler-and-evidence-gaps-surfaced-by-the-0-6-1-0-review.md
ExecPlan: docs/plans/24-report-name-collisions-and-unsupported-notifications-instead-of-acting-on-them.md
Intention: intention_01m2nz9a82ejh91yg9sk7t6a7a
```

using Conventional Commits (`test(pgmq-config): ...`, `feat(pgmq-config)!: ...` for the
breaking constructor addition, `docs(pgmq-config): ...`).


## Validation and Acceptance

Collision: on a database whose registry holds `Billing_Events` (created by raw SQL), reconciling
`standardQueue billing_events & withNotifyInsert (Just 0) & withFifoIndex` reports exactly one
action for that config, `DetectedQueueNameCollision billing_events ("Billing_Events" :| [])`;
afterwards `SELECT count(*) FROM pgmq.meta WHERE lower(queue_name) = 'billing_events'` is `1`,
no throttle row and no `q_billing_events_fifo_idx` exist, and an unrelated declared queue in
the same run is created normally. Both backends.

Partitioned notification: reconciling `partitionedQueue qn pc & withNotifyInsert (Just 0)`
reports `CreatedQueue` then `UnsupportedNotifyOnPartitionedQueue`; `pg_trigger` has no
`trigger_notify_queue_insert_listeners` on `pgmq.q_<qn>`; the throttle table has no row for
`qn`; a second run reports `SkippedQueue` then `UnsupportedNotifyOnPartitionedQueue`. A
pre-existing throttle row on a partitioned queue is left at its stored interval and reported
as unsupported. Both backends, native and stock 1.12.

Unchanged behavior: every pre-existing `ConfigSpec`, `PartitionSpec`, `NotifyCrashSpec`, and
`ForeignQueueSpec` case passes without edits to its assertions.

Documentation: `grep -n "DetectedQueueNameCollision\|UnsupportedNotifyOnPartitionedQueue" docs/user/queue-configuration.md docs/design/018-reconciliation-contract.md pgmq-config/src/Pgmq/Config.hs`
finds each in every file; the guide's listing names all twelve constructors; `just docs-check`
passes.


## Idempotence and Recovery

Every step can be repeated: test suites create fresh servers, and the new detections issue no
statements. If `CollisionSpec` fails with a decode error mentioning `InvalidQueueName`, a typed
`listQueues` ran against the seeded database; the spec must use only raw statements and the
reconciler for observation, and must run on its own server. If the partition case skips, run
under `nix develop .#partman`. If the `-f-effectful` test build fails on a missing module, the
new spec imported an effectful module outside the `PGMQ_EFFECTFUL` CPP guard; mirror
`PartitionSpec.hs`'s guards. No production data is touched by this plan; consumers adopt the
change by handling two new constructors where they match exhaustively.


## Interfaces and Dependencies

At the end of Milestone 2, `pgmq-config/src/Pgmq/Config/Types.hs` exports `ReconcileAction`
with twelve constructors, the two new ones being
`DetectedQueueNameCollision :: QueueName -> NonEmpty Text -> ReconcileAction` and
`UnsupportedNotifyOnPartitionedQueue :: QueueName -> ReconcileAction`. `ObservedQueueType`,
`QueueConfig`, `QueueType`, `PartitionConfig`, `NotifyConfig`, the smart constructors, the
modifiers, and `defaultThrottleMs` are unchanged. `Pgmq.Config.Reconcile.ReconcileOps` keeps
its twelve fields and `ensureQueuesReportWith :: Monad m => ReconcileOps m -> [QueueConfig] -> m [ReconcileAction]`
keeps its signature. `Pgmq.Config` and `Pgmq.Config.Effectful` export lists are unchanged.

Libraries: `containers` (`Data.Map.Strict`), `base` (`Data.List.NonEmpty`), `text`,
`generic-lens` and `lens` for field access, all already dependencies of `pgmq-config`. Tests
use `tasty`, `tasty-hunit`, `hasql`, `hasql-pool`, `ephemeral-pg`, `pg-migrate`, and
`pgmq-migration`, all already test dependencies. The new spec needs no new dependency.
