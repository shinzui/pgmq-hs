---
id: 17
slug: reconcile-against-unvalidated-queue-listings-so-foreign-names-cannot-break-startup
title: "Reconcile against unvalidated queue listings so foreign names cannot break startup"
kind: exec-plan
created_at: 2026-08-05T23:43:28Z
intention: "intention_01kz9yszpmejztjbet6k4bvcf7"
master_plan: "docs/masterplans/4-make-the-pgmq-config-reconciler-truthful-robust-and-single-sourced.md"
---

# Reconcile against unvalidated queue listings so foreign names cannot break startup

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.


## Purpose / Big Picture

pgmq queues live in ordinary SQL tables that any client — psql, a Python service, a
trigger — can create through `pgmq.create`. The server-side validator
(`pgmq.validate_queue_name` in the installed schema) only rejects names longer than
47 characters; it happily accepts hyphens, dots, and mixed case. The Haskell
validator `parseQueueName` (`pgmq-core/src/Pgmq/Types.hs`) is far stricter — as of
MasterPlan 3's plan 15 (in progress) it accepts only nonempty lowercase ASCII
letters, digits, and underscores. The pgmq-hasql queue-listing decoder re-validates
every name read back from the database (`queueDecoder` uses `D.refine` with
`parseQueueName`, `pgmq-hasql/src/Pgmq/Hasql/Decoders.hs`), so one foreign queue
named, say, `billing-events` makes `listQueues` fail to decode — and because the
pgmq-config reconciler's first step is `listQueues`, that foreign row makes
`ensureQueues` throw at application startup. Someone else's queue becomes your boot
failure.

After this plan, the reconciler no longer re-validates names it is merely comparing
against: it snapshots existing queues through a new *unvalidated* listing that
decodes names as plain `Text`, and matches them textually against the (validated)
declared names. A database containing `billing-events` — or, once plan 15's stricter
parser is released, a legacy `MyQueue` — no longer prevents an application from
reconciling its own queues. The typed `listQueues` keeps its strict decoding for API
consumers who want validated values. The new listing is exposed at every layer of the
family (pgmq-core type, pgmq-hasql statement/session, `Pgmq` effect operation with
plain and traced interpreters), because the effect-backed reconciler can only reach
the database through the effect.

You can see it working in one test: create a hyphen-named queue via raw SQL, then run
`ensureQueues` for a normal declared queue — before this plan the session fails with
a decode error; after, it succeeds and the report shows the declared queue created.


## Progress

- [x] M1 (2026-08-05): `UnvalidatedQueue` type in pgmq-core
      (`pgmq-core/src/Pgmq/Types.hs`, exported); `unvalidatedQueueDecoder` in
      `pgmq-hasql/src/Pgmq/Hasql/Decoders.hs`; `listQueuesUnvalidated` statement in
      `pgmq-hasql/src/Pgmq/Hasql/Statements/QueueObservability.hs` and session in
      `pgmq-hasql/src/Pgmq/Hasql/Sessions.hs`; both re-exported from the `Pgmq`
      umbrella (`pgmq-hasql/src/Pgmq.hs`). `cabal build pgmq-core pgmq-hasql`
      library-warning-clean (the six `-Wunused-imports` warnings in the pgmq-hasql
      test modules pre-date this plan — confirmed by building the stashed tree).
- [x] M2 (2026-08-05): `ListQueuesUnvalidated` effect constructor and
      `listQueuesUnvalidated` smart function in
      `pgmq-effectful/src/Pgmq/Effectful/Effect.hs`; dispatch added to the plain
      interpreter and to the traced interpreter under the existing
      `"pgmq.list_queues"` span name; no existing case touched.
      `cabal test pgmq-effectful` reports "All 30 tests passed".
- [ ] M3: reconciler snapshot switched to the unvalidated listing; dedicated-instance
      `ForeignQueueSpec` proves the boot-failure fix; full suite green; plan 15
      framing corrected if still open; changelog material recorded; committed.


## Surprises & Discoveries

(None yet.)


## Decision Log

- Decision: Name the type `UnvalidatedQueue` with `unvalidated*`-prefixed fields and
  the reads `listQueuesUnvalidated` / `ListQueuesUnvalidated`.
  Rationale: The name must warn the consumer that `unvalidatedName` may be a value
  `parseQueueName` rejects; prefixed fields follow the existing pgmq-core convention
  (`TopicBinding` uses `binding*`, `NotifyInsertThrottle` uses `throttle*`).
  Date: 2026-08-05

- Decision: The reconciler leaves foreign rows silently untouched rather than
  reporting them.
  Rationale: The reconciler's contract is additive — every existing queue not in the
  config is out of scope, and a foreign-named queue is just such a queue. Reporting
  it would make the report grow with unrelated database content. Sibling plan 18 owns
  all report-shape changes; if drift reporting ever wants foreign rows surfaced, that
  is its decision to make.
  Date: 2026-08-05

- Decision: Test with hyphenated foreign names, not uppercase ones.
  Rationale: A hyphen fails `parseQueueName` in both its pre- and post-plan-15 forms,
  so this plan's tests are stable regardless of whether MasterPlan 3's plan 15 (in
  progress in this working tree) has landed, been reverted, or been amended.
  Date: 2026-08-05


## Outcomes & Retrospective

(To be filled during and after implementation.)


## Context and Orientation

Multi-package Cabal project; run everything from the repository root inside
`nix develop`. Tests self-provision PostgreSQL via the `ephemeral-pg` library — no
external database. This plan touches four packages, bottom of the stack first:

`pgmq-core` (`pgmq-core/src/Pgmq/Types.hs`) holds shared types. `Queue` is the typed
listing row — `name :: QueueName`, `createdAt :: UTCTime`, `isPartitioned :: Bool`,
`isUnlogged :: Bool` — where `QueueName` is an opaque validated newtype whose only
public constructors are `parseQueueName` and a validating `FromJSON`. This file is
concurrently being edited by MasterPlan 3's plan 15
(`docs/plans/15-validate-queue-names-and-classify-transient-errors-across-the-pgmq-layers.md`),
which tightened `parseQueueName` to lowercase-only. Do not touch `parseQueueName`,
`FromJSON QueueName`, or `notifyChannelName`; this plan only adds a type in its own
region of the file.

`pgmq-hasql` holds the SQL statements and session wrappers. The listing statement is
`listQueues` in `pgmq-hasql/src/Pgmq/Hasql/Statements/QueueObservability.hs` (SQL
`select * from pgmq.list_queues()`), its row decoder `queueDecoder` in
`pgmq-hasql/src/Pgmq/Hasql/Decoders.hs` (name column decoded with
`D.nonNullable $ D.refine (first (pack . show) . parseQueueName) D.varchar` — this
`refine` is the poison point), the session wrapper `listQueues` in
`pgmq-hasql/src/Pgmq/Hasql/Sessions.hs`, and the public umbrella module
`pgmq-hasql/src/Pgmq.hs` re-exports sessions and pgmq-core types. The repository's
convention for adding a function is: statement, then session, then umbrella export
(see the "Adding New pgmq Functions" section of `CLAUDE.md`).

`pgmq-effectful` defines the `Pgmq` effect as a GADT in
`pgmq-effectful/src/Pgmq/Effectful/Effect.hs` (one constructor per operation, e.g.
`ListQueues :: Pgmq m [Queue]` around line 193, plus a smart function
`listQueues = send ListQueues`), a plain interpreter
(`pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs`, one case per constructor:
`ListQueues -> runSession pool Sessions.listQueues`), and a traced interpreter
(`pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs`) that wraps each session
in an OpenTelemetry span, e.g.:

```haskell
  ListQueues ->
    withTracedOp config pool (defaultOpInfo "pgmq.list_queues" OTel.Internal) $
      Sessions.listQueues
```

Keiro's ADR (`docs/adr/0001-keiro-pgmq-job-processing-telemetry-contract.md`,
external consumer constraint recorded by MasterPlans 1 and 3) pins the span semantics
of *existing* operations; adding a new operation with its own span is additive
and permitted, but no existing case may change.

`pgmq-config` holds the reconciler, which after prerequisite plan 16
(`docs/plans/16-extract-the-pgmq-config-reconciler-into-a-single-backend-agnostic-core.md`)
lives once in the internal module `pgmq-config/src/Pgmq/Config/Reconcile.hs`,
parameterized over a `ReconcileOps m` record whose `listQueues :: m [Queue]` field is
the poisoned read. The Session backend (`sessionOps` in
`pgmq-config/src/Pgmq/Config.hs`) and effect backend (`effectfulOps` in
`pgmq-config/src/Pgmq/Config/Effectful.hs`) populate the record. The reconciler
compares declared names against the snapshot as `Set QueueName`
(`Set.member qn existingQueues`); notification and binding snapshots already compare
on plain `Text` and are not poisonable — `parseTopicPattern` only checks nonempty and
length ≤ 255, strictly laxer than the server-side pattern validator, and the throttle
decoder has no `refine` at all. The queue listing is the single read this plan
replaces.

Verify plan 16 is Complete (its status in MasterPlan 4's registry) before starting;
this plan edits the post-refactor files and makes no sense against the duplicated
pre-refactor reconcilers.

Test infrastructure: pgmq-config's suite (`pgmq-config/test/Main.hs`) hands a shared
`Hasql.Pool.Pool` to `ConfigSpec`; `NotifyCrashSpec` instead provisions its own
dedicated PostgreSQL instance inside the spec (via `EphemeralPg` and a
pg-migrate-applied ledger — see `pgmq-config/test/NotifyCrashSpec.hs` and
`pgmq-config/test/EphemeralDb.hs`). The dedicated-instance pattern matters here:
tasty runs specs concurrently, and a foreign-named row in the shared database makes
every concurrent typed-`listQueues` call fail (ConfigSpec calls it in four tests).
MasterPlan 3's plan 15 recorded the same isolation decision for its `AliasingSpec` on
2026-08-05. The new spec in this plan must provision its own instance.


## Plan of Work

### Milestone 1 — the unvalidated listing through pgmq-core and pgmq-hasql

Scope: the new row type and the lenient read, usable from a raw `Session`. At the
end, the family's lowest two layers expose the listing.

In `pgmq-core/src/Pgmq/Types.hs`, directly below the `Queue` declaration, add and
export (type and full record) a mirror row that carries the name as unvalidated
text:

```haskell
-- | A row of @pgmq.list_queues()@ with the queue name left unvalidated.
--
-- Queues are created by every client that shares the database, and the
-- server accepts names 'parseQueueName' rejects (its only check is length).
-- This shape exists so state inspection — notably pgmq-config's reconciler —
-- can observe such foreign queues without failing to decode them.
-- 'unvalidatedName' may therefore hold any server-accepted name; do not feed
-- it into APIs expecting a validated 'QueueName' without going through
-- 'parseQueueName'.
data UnvalidatedQueue = UnvalidatedQueue
  { unvalidatedName :: !Text,
    unvalidatedCreatedAt :: !UTCTime,
    unvalidatedIsPartitioned :: !Bool,
    unvalidatedIsUnlogged :: !Bool
  }
  deriving stock (Eq, Generic, Show)
```

Export it from the module's export list next to `Queue`, and re-export it from the
`Pgmq` umbrella (`pgmq-hasql/src/Pgmq.hs` re-exports `Pgmq.Types` names in explicit
lists — add `UnvalidatedQueue (..)` wherever `Queue (..)` appears).

In `pgmq-hasql/src/Pgmq/Hasql/Decoders.hs`, add `unvalidatedQueueDecoder :: D.Row
UnvalidatedQueue` — same column order as `queueDecoder` (queue_name varchar,
is_partitioned bool, is_unlogged bool, created_at timestamptz; note `queueDecoder`
reorders them into the record) but with the name decoded as
`D.column (D.nonNullable D.varchar)` and no `refine`; export it beside
`queueDecoder`. In `pgmq-hasql/src/Pgmq/Hasql/Statements/QueueObservability.hs`, add
and export:

```haskell
-- | Like 'listQueues' but with names left unvalidated, so rows created by
-- other clients with names 'parseQueueName' rejects still decode.
listQueuesUnvalidated :: Statement () [UnvalidatedQueue]
listQueuesUnvalidated = preparable sql E.noParams decoder
  where
    sql = "select * from pgmq.list_queues()"
    decoder = D.rowList unvalidatedQueueDecoder
```

Add the session wrapper `listQueuesUnvalidated :: Session [UnvalidatedQueue]` in
`pgmq-hasql/src/Pgmq/Hasql/Sessions.hs` next to `listQueues`, and export it from
`Pgmq.Hasql.Sessions` and the `Pgmq` umbrella.

Acceptance: `cabal build pgmq-core pgmq-hasql` warning-clean. (Behavioral proof of
the lenient decode lands with M3's spec; no throwaway harness here.)

### Milestone 2 — the effect operation, plain and traced

Scope: make the read reachable from `Eff es`. In
`pgmq-effectful/src/Pgmq/Effectful/Effect.hs`, add a constructor next to
`ListQueues`:

```haskell
  ListQueuesUnvalidated :: Pgmq m [UnvalidatedQueue]
```

plus the smart function `listQueuesUnvalidated = send ListQueuesUnvalidated`,
exported next to `listQueues` (follow the module's section headers). In the plain
interpreter add `ListQueuesUnvalidated -> runSession pool
Sessions.listQueuesUnvalidated` beside the `ListQueues` case. In the traced
interpreter add, in the Queue Observability section:

```haskell
  ListQueuesUnvalidated ->
    withTracedOp config pool (defaultOpInfo "pgmq.list_queues" OTel.Internal) $
      Sessions.listQueuesUnvalidated
```

The span name stays `"pgmq.list_queues"` because that is the SQL function actually
invoked — the span records the database operation, not the decoding strictness. Do
not modify any existing case (keiro ADR constraint, see Context). If pgmq-effectful
has an interpreter-coverage test enumerating constructors, extend it.

Acceptance: `cabal build pgmq-effectful` and `cabal test pgmq-effectful` green.

### Milestone 3 — switch the reconciler and prove the fix

Scope: the behavior change and its test. In
`pgmq-config/src/Pgmq/Config/Reconcile.hs`, change the `ReconcileOps` field
`listQueues :: m [Queue]` to `listQueuesUnvalidated :: m [UnvalidatedQueue]`, build
the existing-queue snapshot as `Set.fromList (map (\q -> q ^. #unvalidatedName)
queues) :: Set Text`, and change `reconcileQueue`'s membership test to
`Set.member qnText existingQueues` (the `qnText = queueNameToText qn` binding already
exists for the notify set). Update `sessionOps` and `effectfulOps` to supply
`Sessions.listQueuesUnvalidated` / `Eff.listQueuesUnvalidated`. Alternatively,
snapshot `Map Text UnvalidatedQueue` and test with `Map.member` — sibling plan 18
will need the partitioned/unlogged flags per name for drift reporting, and the `Map`
shape saves it a second signature change; either shape is acceptable, record the
choice in the Decision Log.

New spec `pgmq-config/test/ForeignQueueSpec.hs`, registered in
`pgmq-config/test/Main.hs` and the test stanza's `other-modules` of
`pgmq-config/pgmq-config.cabal`. It provisions a dedicated PostgreSQL instance the
way `NotifyCrashSpec` does (module-scoped `EphemeralPg` start, pg-migrate ledger
application, its own `Hasql.Pool`; reuse `pgmq-config/test/EphemeralDb.hs` helpers if
they fit, or mirror `NotifyCrashSpec`'s provisioning) — never the suite-shared pool,
because its foreign rows poison concurrent typed-`listQueues` calls in `ConfigSpec`.
Cases, in prose:

1. Evidence (pins the boundary): after `select pgmq.create('billing-events')` via
   `Hasql.Session.sql`, the typed `Sessions.listQueues` session fails, and
   `Sessions.listQueuesUnvalidated` succeeds returning a row with `unvalidatedName ==
   "billing-events"` and both flags `False`. (If plan 15's remediation work later
   changes the typed behavior, this assertion is the one place to revisit.)
2. The fix: with the foreign row present, `ensureQueuesReport [standardQueue qn]`
   (fresh random lowercase `qn`) succeeds; the report contains `CreatedQueue qn`; a
   second run reports `SkippedQueue qn`; the foreign queue still exists and its
   `pgmq.meta` row is untouched (assert via raw
   `select count(*) from pgmq.meta where queue_name = 'billing-events'`).
3. Effect parity: run case 2's first half again through `ensureQueuesReportEff` with
   the plain interpreter over the same pool (pgmq-config's test stanza gains
   `pgmq-effectful` and `effectful-core` as test dependencies if not already present)
   — same outcome. This pins that both backends use the lenient read.

Then the coordination duty: re-read plan 15
(`docs/plans/15-validate-queue-names-and-classify-transient-errors-across-the-pgmq-layers.md`)
and `docs/design/016-queue-name-validation.md` (referenced by the new
`parseQueueName` haddock; it may or may not exist yet). Wherever they state that
mixed-case/nonconforming meta rows break "pgmq-config reconciliation", correct the
claim to "typed `listQueues` consumers" and note that reconciliation now tolerates
such rows (citing this plan's path). Record the edit in plan 15's Revision Note if
plan 15 is still open; if it completed meanwhile, put the correction in the design
note only. The `parseQueueName` upgrade-note haddock in `pgmq-core/src/Pgmq/Types.hs`
gets the same correction if it names reconciliation.

Record changelog material (for the release owner, MasterPlan 2's plan 12 — do not
bump versions): pgmq-core adds `UnvalidatedQueue`; pgmq-hasql adds
`listQueuesUnvalidated`; pgmq-effectful adds the `ListQueuesUnvalidated` operation
(GADT extension — breaking for exhaustive matchers); pgmq-config behavior fix
"foreign queue names no longer fail `ensureQueues`".

Acceptance: `cabal test all --test-show-details=direct` fully green, including the
new spec. The red/green proof: with only the `Reconcile.hs` snapshot change stashed
(`git stash push pgmq-config/src`), ForeignQueueSpec case 2 fails with the decode
error; `git stash pop` and it passes. Run it once and record the transcript in
Surprises & Discoveries.


## Concrete Steps

From the repository root, inside `nix develop`:

```bash
# M1
cabal build pgmq-core pgmq-hasql

# M2
cabal build pgmq-effectful
cabal test pgmq-effectful --test-show-details=direct

# M3
cabal build all
cabal test pgmq-config --test-show-details=direct
cabal test all --test-show-details=direct
nix fmt
```

Expected new-spec transcript shape:

```text
pgmq-config
  ForeignQueueSpec
    typed listQueues rejects a foreign name (evidence): OK
    ensureQueues succeeds despite a foreign queue:      OK
    effectful ensureQueues matches:                     OK
```

Commit per milestone (M1+M2 may share a commit if small), conventional style, with
the required trailers on every commit:

```text
feat(pgmq-config): reconcile against unvalidated names so foreign queues cannot break startup

MasterPlan: docs/masterplans/4-make-the-pgmq-config-reconciler-truthful-robust-and-single-sourced.md
ExecPlan: docs/plans/17-reconcile-against-unvalidated-queue-listings-so-foreign-names-cannot-break-startup.md
Intention: intention_01kz9yszpmejztjbet6k4bvcf7
```


## Validation and Acceptance

Behavior, verifiable by a novice: start from a database containing a queue named
`billing-events` (create it with `select pgmq.create('billing-events')` in psql or a
test session). Before this plan, `ensureQueues [standardQueue myQueue]` fails the
whole session with a decode error naming the queue-name refinement; after this plan
it succeeds, creates the declared queue, and leaves `billing-events` untouched —
demonstrated by `ForeignQueueSpec` on a dedicated database instance, and by
`cabal test pgmq-config --test-show-details=direct` listing its three cases OK. The
rest of the suite (`cabal test all`) proves no regression: the typed `listQueues`
path still validates (pgmq-hasql suite), and the pre-existing fourteen pgmq-config
tests still pass.


## Idempotence and Recovery

All steps are additive working-tree edits; re-running builds and tests is safe. The
new statement is read-only SQL, so nothing operational can be damaged. Databases are
per-run ephemeral instances. If the effect-GADT extension breaks an exhaustive match
inside this repository, the compiler lists the sites — fix them in the same commit;
consumer repositories are the release owner's concern (plan 12). To abandon
mid-milestone, `git checkout -- .` restores the last commit.


## Interfaces and Dependencies

No new external dependencies (pgmq-config's test stanza may add `pgmq-effectful` and
`effectful-core`, both already in this repository). End-state interfaces:

```haskell
-- pgmq-core, Pgmq.Types
data UnvalidatedQueue = UnvalidatedQueue
  { unvalidatedName :: !Text,
    unvalidatedCreatedAt :: !UTCTime,
    unvalidatedIsPartitioned :: !Bool,
    unvalidatedIsUnlogged :: !Bool
  }

-- pgmq-hasql
Pgmq.Hasql.Statements.QueueObservability.listQueuesUnvalidated :: Statement () [UnvalidatedQueue]
Pgmq.Hasql.Sessions.listQueuesUnvalidated :: Session [UnvalidatedQueue]
-- both re-exported from the Pgmq umbrella

-- pgmq-effectful
Pgmq.Effectful.Effect: ListQueuesUnvalidated :: Pgmq m [UnvalidatedQueue]
Pgmq.Effectful.Effect.listQueuesUnvalidated :: (Pgmq :> es) => Eff es [UnvalidatedQueue]
-- dispatched by both interpreters; traced span name "pgmq.list_queues"

-- pgmq-config, Pgmq.Config.Reconcile (internal)
-- ReconcileOps: the listQueues field is replaced by
listQueuesUnvalidated :: m [UnvalidatedQueue]
```

Coordination: hard-depends on plan 16 (the single reconciler core). Soft coordination
with MasterPlan 3's plan 15 as described in M3 — nothing here blocks on it, but the
remediation-framing correction is this plan's duty if plan 15 is still open. Sibling
plan 18 consumes the `UnvalidatedQueue` flags for queue-type drift reporting and
extends the same GADT/interpreters with a second read; it lands strictly after this
plan.
