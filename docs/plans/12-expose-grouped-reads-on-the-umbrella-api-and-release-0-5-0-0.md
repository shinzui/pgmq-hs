---
id: 12
slug: expose-grouped-reads-on-the-umbrella-api-and-release-0-5-0-0
title: "Expose grouped reads on the umbrella API and release 0.5.0.0"
kind: exec-plan
created_at: 2026-07-14T14:55:11Z
intention: "intention_01kxgh9geke2dayhx57qp6g9ye"
master_plan: "docs/masterplans/2-support-pgmq-1-12-0-grouped-head-reads.md"
---

# Expose grouped reads on the umbrella API and release 0.5.0.0

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.


## Purpose / Big Picture

`pgmq` is a message queue that lives inside PostgreSQL, and this repository, `pgmq-hs`, is a
Haskell client for it. The client is split across five packages, two of which offer a
"front door" module that a user is expected to import:

- `Pgmq` (in the `pgmq-hasql` package) — the direct interface: call a function, it runs SQL.
- `Pgmq.Effectful` (in the `pgmq-effectful` package) — the same operations as an *effect*,
  which lets application code be written once and run against a real database or against a
  traced interpreter that emits OpenTelemetry spans.

There is a real gap in both of those front doors today. **None of the grouped reads are
exported from either.** A user who wants `readGrouped`, `readGroupedWithPoll`,
`readGroupedRoundRobin`, or `readGroupedRoundRobinWithPoll` has to know to reach past the
front door into `Pgmq.Hasql.Sessions` or `Pgmq.Effectful.Effect`. Nothing announces that;
they are not in the module a user reads first, so in practice they are close to undiscoverable.
The two new pgmq 1.12.0 functions added by the preceding plans would inherit exactly the same
fate.

("Grouped read" means: pgmq messages can carry a JSON header named `x-pgmq-group`, and all
messages sharing a value in that header form a **FIFO group** — "first in, first out" — whose
messages pgmq guarantees are consumed in order. The grouped reads are the family of functions
that read from a queue while respecting those groups.)

This plan closes the gap for all six grouped reads at once, and then ships the result. After
it, a user can write

```haskell
import Pgmq
```

and reach every grouped read, including the new `readGroupedHead` and
`readGroupedHeadWithPoll`, along with the `ReadGrouped` and `ReadGroupedWithPoll` parameter
types needed to call them — without having to discover an internal module. You will see it
working by compiling a small program that imports only `Pgmq` and calls `readGroupedHead`.

Then the whole coordinated release is cut: all five packages move from 0.4.0.1 to 0.5.0.0 in
lockstep, with changelogs, and the README and user documentation are updated to say the client
targets pgmq 1.12.0. The same release includes the independently verified hardening work in
`docs/masterplans/3-harden-the-pgmq-hs-family-surfaced-by-the-2026-07-review.md`.

**This plan depends on
`docs/plans/10-add-grouped-head-read-statements-and-sessions-to-pgmq-hasql.md` and
`docs/plans/11-add-grouped-head-read-effects-and-traced-spans-to-pgmq-effectful.md`, plus
hardening plans 13, 14, and 15, being complete**. It re-exports functions from plans 10 and
11, while plans 13–15 define the breaking API and behavior that the 0.5.0.0 changelogs and
consumer rollout must describe. Confirm all five prerequisites before starting.

```bash
grep -c readGroupedHead pgmq-hasql/src/Pgmq/Hasql/Sessions.hs pgmq-effectful/src/Pgmq/Effectful/Effect.hs
```

Both counts must be greater than zero.


## Progress

- [ ] Milestone 1: a "FIFO / Grouped Reads" section added to `pgmq-hasql/src/Pgmq.hs` exporting all six grouped reads plus `ReadGrouped(..)` and `ReadGroupedWithPoll(..)`; `cabal build pgmq-hasql` succeeds.
- [ ] Milestone 2: the same section added to `pgmq-effectful/src/Pgmq/Effectful.hs`; `cabal build pgmq-effectful` succeeds.
- [ ] Milestone 3: a compile-time proof added — a test module that imports *only* the umbrella modules and calls the grouped reads, so a future export regression breaks the build; `cabal test all` passes.
- [ ] Milestone 4: all five `.cabal` files bumped 0.4.0.1 → 0.5.0.0; all six `CHANGELOG.md` files given a complete 0.5.0.0 entry covering grouped-head support and plans 13–15.
- [ ] Milestone 5: `README.md`, `docs/user/schema-migration.md`, and `CLAUDE.md` updated to state the client targets pgmq 1.12.0; a design note recording the 1.12.0 upgrade added under `docs/design/`.
- [ ] Milestone 6: Mori consumer inventory refreshed; all keiro and shibuya package/component bounds updated; shibuya adapted to `Maybe Message`; both consumer repositories build and test green; out-of-scope consumers explicitly recorded as remaining on 0.4.
- [ ] Milestone 7: `nix fmt` clean, `cabal build all && cabal test all` green, release commit made.


## Surprises & Discoveries

- Validation (2026-07-23): the hardening review made 0.5.0.0 breaking, not merely additive:
  `changeVisibilityTimeout` and `setVisibilityTimeoutAt` now return `Maybe Message`.
- Validation (2026-07-23): shibuya pins pgmq packages in its adapter library and test
  component, example packages, and benchmark packages. Updating only the adapter library
  leaves the repository split across 0.4 and 0.5.
- Validation (2026-07-23): Mori also reports direct 0.4 pins in rei. Rei is outside this
  initiative's rollout and remains on the compatible 0.4 line unless its owners explicitly
  opt into 0.5; the release record must say so rather than imply every dependent was upgraded.


## Decision Log

- Decision: Export all six grouped reads from the umbrella modules, not only the two new 1.12.0 ones.
  Rationale: The four pre-existing grouped reads (`readGrouped`, `readGroupedWithPoll`, `readGroupedRoundRobin`, `readGroupedRoundRobinWithPoll`) are absent from `Pgmq` and `Pgmq.Effectful` today. Adding only the two new functions would produce a stranger API than the one we started with — a front door where the *newest* grouped reads are visible and the older, more commonly used ones are not. The gap is almost certainly an oversight rather than a design choice: there is no comment or note anywhere justifying the omission, every other operation family (sends, topic routing, observability) *is* exported, and the functions are perfectly good. Fixing the whole family in one place, once, is cheaper than fixing it twice and leaves a coherent API. The change is purely additive, so it cannot break an existing user.
  Date: 2026-07-14

- Decision: Also export the `ReadGrouped(..)` and `ReadGroupedWithPoll(..)` types from the umbrella modules.
  Rationale: Exporting the functions alone would be useless. Every grouped read takes one of these two records as its only argument, so a user who imports just `Pgmq` could name the function but could not construct a value to pass it, and would be forced back into the internal module anyway — leaving the gap only half-closed while appearing to have closed it. The `(..)` exports the constructor and field names, which is what a record needs to be built.
  Date: 2026-07-14

- Decision: Release all five packages as 0.5.0.0 in lockstep.
  Rationale: This is the established convention in this repository — the root `CHANGELOG.md` explicitly notes "All packages share the 0.4.0.1 version", and the per-package changelogs for packages that did not change in a release carry the note "coordinated version bump". The packages are developed and released together, and a user pins them together. Do not break the convention for the sake of a smaller diff.
  Date: 2026-07-14

- Decision: Bump the first component after zero (0.4.0.1 → 0.5.0.0).
  Rationale: Plan 13 changes two public results from `Message` to `Maybe Message`. Under
  pre-1.0 PVP bounds this is the required breaking bump. The grouped-read additions and SQL
  migration also belong in that same coordinated release.
  Date: 2026-07-23

- Decision: This plan is the single owner of version changes, final changelogs, and consumer
  rollout for MasterPlans 2 and 3.
  Rationale: A single release owner prevents competing "last lander" instructions and ensures
  every package and consumer is validated against the same source commit.
  Date: 2026-07-23


## Outcomes & Retrospective

(To be filled during and after implementation.)


## Context and Orientation

### Where you are

The repository root is `/Users/shinzui/Keikaku/bokuno/libraries/pgmq-hs-project/pgmq-hs`.
All paths are relative to it and all commands run from there. Enter the development shell
first — it provides GHC 9.12.4, `cabal`, and the PostgreSQL binaries the tests need:

```bash
nix develop
```

### The five packages

- `pgmq-core` — shared types (`Message`, `QueueName`, and so on). Plan 14 adds the notification
  channel helper and plan 15 tightens queue-name decoding.
- `pgmq-hasql` — the database layer. Its front-door module is `pgmq-hasql/src/Pgmq.hs`.
- `pgmq-effectful` — the effect layer. Its front-door module is
  `pgmq-effectful/src/Pgmq/Effectful.hs`.
- `pgmq-migration` — ships the pgmq schema as plain SQL migrations, so pgmq can be installed
  on a managed PostgreSQL that does not permit the pgmq extension.
- `pgmq-config` — declarative queue configuration. Plan 13 corrects its optional-notification
  contract and plan 14 adds crash-recovery coverage.

There is also `pgmq-bench` (version 0.1.0.0), a benchmark executable that is not published and
is **not** part of the lockstep versioning. Leave its version alone.

### What "umbrella module" means here

An umbrella module is one that defines almost nothing itself and instead re-exports a curated
selection of names from the package's internal modules, so a user can write a single import.
`pgmq-hasql/src/Pgmq.hs` is one: it re-exports from `Pgmq.Hasql.Sessions`,
`Pgmq.Hasql.Statements.Types`, and `Pgmq.Types`, organised under Haddock section headings.
Its current sections are:

```haskell
-- * Queue Management
-- * Message Operations
-- ** With Headers (pgmq 1.5.0+)
-- ** Timestamp-based VT (pgmq 1.10.0+)
-- * Topic Routing (pgmq 1.11.0+)
-- * Types
-- * Queue Name Utilities
```

`pgmq-effectful/src/Pgmq/Effectful.hs` is the same idea for the effect layer, with sections
`Effect`, `Interpreters` (and `Traced Interpreters`), `Errors`, `Traced Operations`,
`Telemetry Utilities`, `Queue Management`, `Message Operations`, `Topic Routing (pgmq 1.11.0+)`,
`Queue Observability`, and `Types`.

**Neither has any grouped-read section.** That is the gap this plan closes.

### The six functions to export

Four already exist. Two were added by the preceding plans. In `pgmq-hasql` they live in
`Pgmq.Hasql.Sessions`:

```haskell
readGrouped                   :: ReadGrouped         -> Session (Vector Message)
readGroupedWithPoll           :: ReadGroupedWithPoll -> Session (Vector Message)
readGroupedRoundRobin         :: ReadGrouped         -> Session (Vector Message)
readGroupedRoundRobinWithPoll :: ReadGroupedWithPoll -> Session (Vector Message)
readGroupedHead               :: ReadGrouped         -> Session (Vector Message)   -- new in 1.12.0
readGroupedHeadWithPoll       :: ReadGroupedWithPoll -> Session (Vector Message)   -- new in 1.12.0
```

In `pgmq-effectful` they live in `Pgmq.Effectful.Effect` with the same names, each having the
shape `(Pgmq :> es) => ReadGrouped -> Eff es (Vector Message)`.

The two parameter types, defined in `pgmq-hasql/src/Pgmq/Hasql/Statements/Types.hs`:

```haskell
data ReadGrouped = ReadGrouped
  { queueName :: !QueueName,
    visibilityTimeout :: !Int32,
    qty :: !Int32
  }

data ReadGroupedWithPoll = ReadGroupedWithPoll
  { queueName :: !QueueName,
    visibilityTimeout :: !Int32,
    qty :: !Int32,
    maxPollSeconds :: !Int32,
    pollIntervalMs :: !Int32
  }
```

`pgmq-effectful` re-exports its types from this same module, so both umbrellas name the same
two types — there is no divergence to reconcile.

Briefly, what distinguishes the six, since the export section you write should say so:
`readGrouped` fills a batch from a single group; `readGroupedRoundRobin` spreads a batch fairly
across groups; `readGroupedHead` takes the single oldest message from each of up to `qty`
*distinct* groups and never two from one group, which is what lets N workers each own a
different group. Each has a `...WithPoll` variant that waits for up to `maxPollSeconds` when
the queue is empty (**long polling**) instead of returning empty immediately.

### Versions and changelogs

All five library packages are at `version: 0.4.0.1` (line 3 of each `.cabal`). There are six
`CHANGELOG.md` files: one per package, plus a root aggregate at `CHANGELOG.md` whose top entry
is `## 0.4.0.1 -- 2026-07-14` and which states that all packages share the version.

The repository follows Conventional Commits (`feat:`, `fix:`, `docs:`, `chore:`, with an
optional scope) — see `CLAUDE.md`.


## Plan of Work

### Milestone 1 — Export the grouped reads from `Pgmq`

**Scope.** Add a new Haddock section to `pgmq-hasql/src/Pgmq.hs` exporting all six grouped
reads and the two parameter types. Nothing else changes; this is purely a re-export.

Place the new section after `-- * Message Operations` and its subsections, and before
`-- * Topic Routing (pgmq 1.11.0+)`. That position keeps the file's rough chronological and
conceptual ordering — grouped reads are message operations, and they predate topics.

Add to the export list:

```haskell
    -- * FIFO / Grouped Reads
    -- $groupedReads
    readGrouped,
    readGroupedWithPoll,
    readGroupedRoundRobin,
    readGroupedRoundRobinWithPoll,

    -- ** Grouped-Head Reads (pgmq 1.12.0+)
    readGroupedHead,
    readGroupedHeadWithPoll,
```

and, in the `-- * Types` section, the two parameter types:

```haskell
    ReadGrouped (..),
    ReadGroupedWithPoll (..),
```

Then add the named documentation chunk referenced by `-- $groupedReads`, somewhere after the
imports. This is a Haddock feature: a `-- $name` marker in the export list pulls in a block
introduced by `-- $name` in the body, letting you write a paragraph of prose that appears in
the rendered docs at that point in the export list. This is the natural place to explain what
the six functions are *for*, which is the thing a user cannot work out from six similar names:

```haskell
-- $groupedReads
--
-- pgmq messages may carry a JSON header named @x-pgmq-group@. All messages sharing
-- a value in that header form a FIFO group, and pgmq guarantees they are consumed in
-- the order they were sent. Messages without the header form a single implicit group.
--
-- The grouped reads differ in how they choose messages across groups:
--
-- * 'readGrouped' fills the batch from a single group.
-- * 'readGroupedRoundRobin' spreads the batch fairly across groups.
-- * 'readGroupedHead' returns the oldest message from each of up to @qty@ distinct
--   groups, never two from the same group. Use this to scale ordered processing
--   horizontally: each of N workers takes the head of a different group, so every
--   group progresses in parallel while each group stays strictly in order.
--
-- Each has a @...WithPoll@ variant that waits up to @maxPollSeconds@ for a message
-- to arrive instead of returning empty immediately (\"long polling\"). The wait
-- happens inside PostgreSQL, so a connection is held for its duration.

```

Update the imports at the bottom of the file: add the six function names to the
`import Pgmq.Hasql.Sessions (...)` list and the two type names to the
`import Pgmq.Hasql.Statements.Types (...)` list. Both import lists are alphabetised —
keep them that way.

**Acceptance.** `cabal build pgmq-hasql` succeeds. `-Wmissing-export-lists` and
`-Wunused-imports` are on, so a name imported but not exported (or vice versa) is a warning.

### Milestone 2 — Export the grouped reads from `Pgmq.Effectful`

**Scope.** The same change to `pgmq-effectful/src/Pgmq/Effectful.hs`. Place the new section
after `-- * Message Operations` and before `-- * Topic Routing (pgmq 1.11.0+)`, matching the
structure of `Pgmq` so the two front doors stay recognisably parallel — a user who learns one
should be able to guess the other.

Export the same six names (from `Pgmq.Effectful.Effect`) and the same two types. Reuse the
`-- $groupedReads` prose from Milestone 1; the concepts are identical and a user reading either
module deserves the same explanation. Duplicating the paragraph is correct here — these are two
separately-published packages and neither should have to depend on the other's Haddocks.

**Acceptance.** `cabal build pgmq-effectful` succeeds.

### Milestone 3 — Prove the umbrella exports, at compile time

**Scope.** Add a test that would fail to *compile* if any of the eight names (six functions,
two types) stopped being exported from an umbrella module. This is the milestone that turns
"we added some exports" into a guarantee that stays true.

A runtime test is the wrong tool here. What we are asserting is a property of the module
interface, and the compiler already checks module interfaces perfectly — a test that merely
*calls* the functions through the umbrella import will fail to build the moment an export is
dropped, which is a louder and earlier failure than any assertion. So the test's job is simply
to *use* the API the way a user would, from a module whose imports are restricted to the front
door.

Add a module `pgmq-hasql/test/UmbrellaExportsSpec.hs` whose **only** pgmq import is:

```haskell
import Pgmq
```

Not `Pgmq.Hasql.Sessions`, not `Pgmq.Hasql.Statements.Types` — those would defeat the purpose
entirely, and a reader who adds one later to fix a build error will have silently destroyed
the test's value. Say so in a comment at the top of the file.

In it, construct a `ReadGrouped` and a `ReadGroupedWithPoll` value using record syntax (this is
what proves the `(..)` type exports work — you cannot build a record whose constructor is not
exported), and call all six grouped reads against a real ephemeral queue. Keep the assertions
minimal: this test exists to prove reachability, not behaviour. Behaviour is already covered by
`pgmq-hasql/test/AdvancedOpsSpec.hs`. Asserting that each call succeeds and returns a `Vector`
is enough. For the two polling variants use `maxPollSeconds = 1` so the test stays fast.

Register the module in `pgmq-hasql/test/Main.hs` and in the `other-modules` stanza of the
`pgmq-hasql` test suite in `pgmq-hasql/pgmq-hasql.cabal`.

Do the same for `pgmq-effectful`: add `pgmq-effectful/test/UmbrellaExportsSpec.hs` importing
only `Pgmq.Effectful`, calling the six effect functions under the plain interpreter, and
register it in that package's `Main.hs` and `.cabal`.

**Acceptance.** `cabal test all` passes. Then prove the test does what it claims: temporarily
remove `readGroupedHead` from the export list of `pgmq-hasql/src/Pgmq.hs` and rebuild — the
test module must fail to compile with a "Variable not in scope" error. Restore the export.
Record the observed error in Surprises & Discoveries.

### Milestone 4 — Version bump and changelogs

**Scope.** Move all five library packages from 0.4.0.1 to 0.5.0.0 and write the changelogs.
No code changes.

Edit `version:` on line 3 of each of `pgmq-core/pgmq-core.cabal`,
`pgmq-hasql/pgmq-hasql.cabal`, `pgmq-effectful/pgmq-effectful.cabal`,
`pgmq-migration/pgmq-migration.cabal`, and `pgmq-config/pgmq-config.cabal`. **Do not touch**
`pgmq-bench/pgmq-bench.cabal` — it is unpublished and not part of the lockstep.

**The packages pin each other with upper bounds, so the version bump is not just the `version:`
lines.** Several `.cabal` files declare internal dependencies as `>=0.4 && <0.5`. Every one of
those must move to `>=0.5 && <0.6`, or `cabal build all` will fail to resolve — a package at
0.5.0.0 cannot satisfy a `<0.5` bound. The exact occurrences, verified against the working tree:

- `pgmq-hasql/pgmq-hasql.cabal:60` — `pgmq-core >=0.4 && <0.5`
- `pgmq-effectful/pgmq-effectful.cabal:60,61` — `pgmq-core`, `pgmq-hasql`
- `pgmq-effectful/pgmq-effectful.cabal:104,106` (test suite) — `pgmq-core`, `pgmq-migration`
- `pgmq-config/pgmq-config.cabal:57,58` — `pgmq-core`, `pgmq-hasql`
- `pgmq-config/pgmq-config.cabal:65` — `pgmq-effectful`

Re-check rather than trusting the line numbers, which drift:

```bash
grep -n "pgmq-\(core\|hasql\|effectful\|migration\|config\).*0\.4" */*.cabal
```

That command must return nothing once the bump is complete.

Write a `## 0.5.0.0 -- <today>` entry at the top of each of the six `CHANGELOG.md` files.
Follow the existing style in each. The substance, per package:

- `pgmq-hasql` — added `readGroupedHead` and `readGroupedHeadWithPoll` (pgmq 1.12.0);
  the `Pgmq` umbrella module now exports all six grouped reads and the `ReadGrouped` /
  `ReadGroupedWithPoll` types, which were previously reachable only via
  `Pgmq.Hasql.Sessions`; breaking: `changeVisibilityTimeout` and
  `setVisibilityTimeoutAt` now return `Maybe Message`; fixed: optional pop/read/poll/notify
  parameters use their documented defaults, `ReadMessage.conditional` is encoded, and SQL
  NULL bodies surface as JSON null.
- `pgmq-effectful` — added the `ReadGroupedHead` and `ReadGroupedHeadWithPoll` effects with
  plain and traced interpreters; the traced interpreter emits Consumer spans with
  `db.operation` set to the new SQL function names; the `Pgmq.Effectful` umbrella module now
  exports all six grouped reads; breaking: both visibility-timeout effects return
  `Maybe Message`; fixed: serialization, deadlock, lock, shutdown/recovery, and resource
  SQLSTATEs classify as transient.
- `pgmq-migration` — the component now installs pgmq 1.12.0 via a new
  `0003-upgrade-v1.12.0` migration; the `0001` baseline is unchanged; the pgmq 1.11 schema
  contract used for legacy history import is deliberately unchanged; a later numbered
  migration makes notification delivery fail open after crash recovery, serializes
  concurrent notification enable, and makes partitioned re-entry idempotent. Use the actual
  manifest filename rather than assuming its number.
- `pgmq-core` — queue names now reject uppercase and empty input through both
  `parseQueueName` and `FromJSON`; `notifyChannelName` is the exported LISTEN contract.
- `pgmq-config` — optional notification throttle documentation is now true and a real
  crash/restart regression test proves notification delivery recovers.
- The root `CHANGELOG.md` — an aggregate entry covering the above, noting that all packages
  share the 0.5.0.0 version and that the client now targets pgmq 1.12.0.

Call out explicitly that the grouped-read exports are additive, but the overall release is
breaking because the two visibility-timeout results now return `Maybe Message`. Include the
mixed-case queue detection and transactional remediation summary from plan 15; do not tell
operators to update or delete `pgmq.meta` rows without preserving `topic_bindings` and
notification configuration.

**Acceptance.** `cabal build all` succeeds with the new versions (which proves any internal
bounds were updated correctly).

### Milestone 5 — Documentation

**Scope.** Update the documents that state which pgmq version this client targets. Prose only.

`README.md` describes the client and, in several places, the `pgmq-migration` component as
being a "1.11 baseline". Update it to say the client targets pgmq 1.12.0, and add the grouped
reads — including `readGroupedHead` — to whatever feature or function listing it carries.
Grep for `1.11` to find every mention rather than trusting a scan by eye.

`docs/user/schema-migration.md` is the user guide for `pgmq-migration`.
`docs/plans/9-vendor-pgmq-1-12-0-and-add-the-native-schema-migration.md` already updates it
for the new migration; verify that landed and that nothing further is stale.

`CLAUDE.md` — confirm plan 9's note about the commit-pinned subtree is present.

Add a `docs/design/` upgrade note, following the pattern of the existing
`docs/design/011-pgmq-1.11.0-upgrade.md`. Take the next free number at the time you land —
do not assume one: plans 13, 14, and 15 have since added `014-null-parameter-contract.md`,
`015-notification-delivery-contract.md`, `016-queue-name-validation.md`, and
`017-transient-error-classification.md`, so at the time of this correction the next free
number is `018`. It should record what upstream actually changed at the SQL level (two new functions;
a dropped `enable_notify_insert(text)` overload that was a no-op here; `pg_dump` and `pg_monitor`
grant housekeeping with no upgrade-path counterpart), the decision to pin the subtree to a commit
because upstream had not tagged v1.12.0, and the decision to leave the pgmq 1.11 schema contract
alone.

**Acceptance.** `grep -rn "1\.11" README.md docs/user/` returns only intentional historical
references (for example, "the 0001 baseline is upstream's 1.11.0 install script"), not stale
claims that the client targets 1.11.

### Milestone 6 — Consumer rollout

**Scope.** Upgrade the in-scope keiro and shibuya consumers against the exact pgmq-hs release
candidate, prove every component resolves one coherent 0.5 family, and record what deliberately
stays on 0.4. The candidate becomes the release commit in Milestone 7; do not assume Hackage
already serves it.

Before editing bounds, follow the repository dependency policy:

```bash
mori registry dependents shinzui/pgmq-hs --packages
mori registry show shinzui/pgmq-hs --full
```

Verify that all five candidate Cabal files say 0.5.0.0. Check authoritative Hackage metadata
and pgmq-hs tags and record that the currently served release is still 0.4.0.1 until the
separate publishing workflow completes. Mori supplies source locations and the consumer
inventory; the authoritative registry and tag distinguish a locally prepared version from a
served one.

For pre-publication validation, create an uncommitted temporary Cabal project file in each
consumer repository. Copy its existing `cabal.project`, add the absolute paths of this
checkout's `pgmq-core`, `pgmq-hasql`, `pgmq-effectful`, `pgmq-migration`, and `pgmq-config`
directories to the existing `packages:` stanza, and invoke Cabal with
`--project-file=cabal.project.release-candidate`. Never commit this machine-local file.

In `mori://shinzui/keiro`, package `keiro-pgmq` (resolve the checkout with
`mori path mori://shinzui/keiro`; at time of writing `/Users/shinzui/Keikaku/bokuno/keiro`),
update every library and test-suite pgmq family bound in `keiro-pgmq/keiro-pgmq.cabal` to
`>=0.5 && <0.6`, including `pgmq-migration`, then run:

```bash
cd "$(mori path mori://shinzui/keiro | tail -1)"
cabal --project-file=cabal.project.release-candidate \
  test keiro-pgmq-test --test-show-details=direct
```

The pre-train baseline was 58 examples, 0 failures, and 2 pending. If the suite has legitimately
grown, record the new baseline rather than forcing the old count; failures must remain zero.
Keiro's three visibility-timeout calls discard the result with `void`, so no source change is
expected.

In `mori://shinzui/shibuya-pgmq-adapter` (resolve with
`mori path mori://shinzui/shibuya-pgmq-adapter`; at time of writing
`/Users/shinzui/Keikaku/bokuno/shibuya-project/shibuya-pgmq-adapter`), update **every**
`^>=0.4` pgmq bound, not only the adapter library:

- the library and test-suite stanzas in
  `shibuya-pgmq-adapter/shibuya-pgmq-adapter.cabal`;
- every component in `shibuya-pgmq-example/shibuya-pgmq-example.cabal`;
- every component in
  `shibuya-pgmq-adapter-bench/shibuya-pgmq-adapter-bench.cabal`.

Adapt `shibuya-pgmq-adapter/src/Shibuya/Adapter/Pgmq/Internal.hs` where
`setVisibilityTimeoutAt` feeds `lastVtRef`: traverse the `Maybe Message`, updating the
reference only for `Just`. A message that disappeared during lease extension leaves the last
visibility-time tracking unchanged.

Validate the whole repository, including components that the old `just test` command skipped:

```bash
cd "$(mori path mori://shinzui/shibuya-pgmq-adapter | tail -1)"
cabal --project-file=cabal.project.release-candidate \
  build all --enable-tests --enable-benchmarks
cabal --project-file=cabal.project.release-candidate \
  test all --test-show-details=direct
```

Finally, preserve a rollout matrix in this plan's Outcomes & Retrospective. Mori currently
finds direct `^>=0.4` pins in `mori://shinzui/rei` (package `rei-core`). Rei is out of scope and
may continue resolving the published 0.4 family; state that explicitly. Mori also reports
package-level pgmq dependencies in `mori://shinzui/mori-app`, `mori://shinzui/mori-rei-app`,
and `mori://tan/mls-service-v2` — the last depends directly on `pgmq-core`, `pgmq-hasql`,
`pgmq-effectful`, and `pgmq-migration`, and is in a different owning namespace. Classify each
of them explicitly; do not let the inventory step silently drop a consumer it did find. Any
newly discovered consumer must be assigned one of two states with evidence: upgraded and tested
on 0.5, or intentionally retained on 0.4. Do not use
"all consumers upgraded" when the matrix contains retained consumers.

**Acceptance.** Every keiro and shibuya bound in scope names 0.5, shibuya source handles the
new `Maybe Message`, both repositories build and test green against the local candidate, and
the recorded Mori inventory accounts for every direct consumer. After Milestone 7 creates the
release commit, record its SHA in both consumer change records. Do not claim the packages are
Hackage-installable until authoritative metadata serves 0.5.0.0; either keep the temporary
source override for validation only or intentionally add a persistent commit pin in a separate,
reviewed consumer decision.

### Milestone 7 — Release

**Scope.** Format, verify everything, and make the release commit.

**Acceptance.** `nix fmt` makes no changes; `cabal build all` and `cabal test all` are green;
the working tree is committed.


## Concrete Steps

All commands run from `/Users/shinzui/Keikaku/bokuno/libraries/pgmq-hs-project/pgmq-hs`
inside `nix develop`.

**Before starting, confirm both prerequisites:**

```bash
grep -c readGroupedHead \
  pgmq-hasql/src/Pgmq/Hasql/Sessions.hs \
  pgmq-effectful/src/Pgmq/Effectful/Effect.hs
```

Both counts must be greater than zero. If either is `0`, implement
`docs/plans/10-add-grouped-head-read-statements-and-sessions-to-pgmq-hasql.md` and
`docs/plans/11-add-grouped-head-read-effects-and-traced-spans-to-pgmq-effectful.md` first.
Also confirm the three hardening plans have no unchecked progress items:

```bash
rg -n '^- \[ \]' \
  docs/plans/13-fix-null-parameter-semantics-across-pop-read-and-notify-statements.md \
  docs/plans/14-make-insert-notifications-survive-crashes-and-document-the-channel-contract.md \
  docs/plans/15-validate-queue-names-and-classify-transient-errors-across-the-pgmq-layers.md
```

The command must return nothing. If it finds an item, complete that plan before changing
versions.

**Milestones 1 and 2** — edit `pgmq-hasql/src/Pgmq.hs` and
`pgmq-effectful/src/Pgmq/Effectful.hs`, then:

```bash
cabal build pgmq-hasql pgmq-effectful
```

**Milestone 3** — add the two `UmbrellaExportsSpec.hs` modules, register them in the
respective `Main.hs` and `.cabal` files, then:

```bash
cabal test all
```

Prove the test bites by removing an export and rebuilding:

```bash
# Temporarily delete the line "    readGroupedHead," from pgmq-hasql/src/Pgmq.hs
cabal build pgmq-hasql --enable-tests
```

Expect a compile error along these lines, from the test module, not the library:

```text
pgmq-hasql/test/UmbrellaExportsSpec.hs:NN:NN: error: [GHC-88464]
    Variable not in scope: readGroupedHead :: ReadGrouped -> Session (Vector Message)
```

Restore the export line and rebuild.

**Milestone 4** — bump the five `.cabal` versions, check internal bounds, and write the six
changelogs:

```bash
grep -n "^version:" */*.cabal
```

Expected after the bump (note `pgmq-bench` stays at 0.1.0.0):

```text
pgmq-bench/pgmq-bench.cabal:3:version:            0.1.0.0
pgmq-config/pgmq-config.cabal:3:version:            0.5.0.0
pgmq-core/pgmq-core.cabal:3:version:            0.5.0.0
pgmq-effectful/pgmq-effectful.cabal:3:version:            0.5.0.0
pgmq-hasql/pgmq-hasql.cabal:3:version:            0.5.0.0
pgmq-migration/pgmq-migration.cabal:3:version:            0.5.0.0
```

Then:

```bash
cabal build all
```

**Milestone 5** — update the documentation. Find every stale version claim:

```bash
grep -rn "1\.11" README.md docs/user/ CLAUDE.md
```

**Milestone 6** — refresh the Mori inventory and perform the keiro/shibuya rollout exactly as
described above. Run the complete consumer builds, not only shibuya's adapter test.

**Milestone 7** — format, verify, commit:

```bash
nix fmt
cabal build all
cabal test all
git add -A
git commit -m "$(cat <<'EOF'
feat!: expose grouped reads and harden pgmq for 0.5.0.0

The Pgmq and Pgmq.Effectful umbrella modules now export all six grouped
reads -- readGrouped, readGroupedWithPoll, readGroupedRoundRobin,
readGroupedRoundRobinWithPoll, and the new 1.12.0 readGroupedHead and
readGroupedHeadWithPoll -- along with the ReadGrouped and
ReadGroupedWithPoll parameter types needed to call them.

The four pre-existing grouped reads were reachable only via
Pgmq.Hasql.Sessions and Pgmq.Effectful.Effect, which made them close to
undiscoverable. The change is additive: code importing the internal
modules directly continues to work.

A new UmbrellaExportsSpec in each package imports only the umbrella
module, so dropping any of these exports becomes a compile error rather
than a silent API regression.

Release all five library packages as 0.5.0.0 in lockstep. The client now
targets pgmq 1.12.0. Optional SQL parameters now honor their documented
defaults, notification delivery survives crash recovery, queue names validate
at every Haskell entry point, transient shutdown errors retry, and SQL NULL
message bodies remain operable.

BREAKING CHANGE: changeVisibilityTimeout and setVisibilityTimeoutAt now return
Maybe Message across pgmq-hasql and pgmq-effectful.

MasterPlan: docs/masterplans/2-support-pgmq-1-12-0-grouped-head-reads.md
ExecPlan: docs/plans/12-expose-grouped-reads-on-the-umbrella-api-and-release-0-5-0-0.md
MasterPlan: docs/masterplans/3-harden-the-pgmq-hs-family-surfaced-by-the-2026-07-review.md
ExecPlan: docs/plans/13-fix-null-parameter-semantics-across-pop-read-and-notify-statements.md
ExecPlan: docs/plans/14-make-insert-notifications-survive-crashes-and-document-the-channel-contract.md
ExecPlan: docs/plans/15-validate-queue-names-and-classify-transient-errors-across-the-pgmq-layers.md
Intention: intention_01kxgh9geke2dayhx57qp6g9ye
EOF
)"
```


## Validation and Acceptance

The plan is complete when all of the following hold.

**A user can reach every grouped read through the front door.** This is the headline claim.
Prove it the way a user would experience it — write a module whose only pgmq import is
`import Pgmq`, construct a `ReadGrouped` with record syntax, and call all six functions. If it
compiles, the claim is true. That module is not a throwaway: it is
`pgmq-hasql/test/UmbrellaExportsSpec.hs`, checked in, so the claim keeps being true. The same
holds for `import Pgmq.Effectful` and `pgmq-effectful/test/UmbrellaExportsSpec.hs`.

**A dropped export is a compile error, not a silent regression.** Verify this rather than
assuming it: delete `readGroupedHead` from the export list of `pgmq-hasql/src/Pgmq.hs`, run
`cabal build pgmq-hasql --enable-tests`, and confirm the *test module* fails with "Variable not
in scope". Restore it. Paste the error into Surprises & Discoveries. If the build still
succeeds, your test module is importing an internal module somewhere and is worthless — find
the import and remove it.

**The breaking change is explicit and migrated.** Grouped-read exports remain additive: a user
who imported `Pgmq.Hasql.Sessions (readGroupedRoundRobin)` continues to compile. Users of
`changeVisibilityTimeout` or `setVisibilityTimeoutAt` must handle `Maybe Message`; the
changelog names this and shibuya is compiled against the new result. `cabal build all` and
`cabal test all` are green after every pre-existing positive visibility-timeout test asserts
`Just` and the new missing-row tests assert `Nothing`.

**All five library packages report 0.5.0.0** (`grep -n "^version:" */*.cabal`), with
`pgmq-bench` still at 0.1.0.0. **No internal dependency bound still mentions 0.4** —
`grep -n "pgmq-\(core\|hasql\|effectful\|migration\|config\).*0\.4" */*.cabal` returns nothing.
And `cabal build all` succeeds, which is the real proof: the packages pin each other with
`<0.5` upper bounds today, so a missed bound is a resolution failure, not a silent problem.

**Every changelog tells the truth.** Six `CHANGELOG.md` files have a 0.5.0.0 entry.
`pgmq-core` describes validation and the channel helper; `pgmq-config` describes corrected
optional-notification behavior and crash coverage. The others describe what actually changed,
distinguishing additive grouped-read exports from the breaking visibility-timeout result.

**Every in-scope consumer resolves coherently.** No keiro or shibuya component retains a 0.4
pgmq bound, both repositories build and test green, and the rollout matrix accounts for
Mori-discovered consumers that intentionally remain on 0.4.

**The documentation no longer claims 1.11.** `grep -rn "1\.11" README.md docs/user/ CLAUDE.md`
returns only deliberate historical references — the `0001` baseline genuinely *is* upstream's
1.11.0 install script and should still say so — and no claim that the client targets 1.11.


## Idempotence and Recovery

**Repository changes remain recoverable before the release commit.** New exports and tests are
additive; version strings, bounds, changelogs, and prose can be corrected and revalidated.
Plans 9 and 14 add append-only SQL migrations and must follow their own recovery rules. Plan 13
contains the deliberate breaking type change. Do not describe the coordinated release as
purely additive.

**The tests are safe to run repeatedly.** They spin up throwaway PostgreSQL servers via
`ephemeral-pg`; no external or production database is contacted. Queues are given unique names
so repeated and concurrent runs do not collide.

**If `cabal build all` fails after the version bump** with a message about being unable to
resolve dependencies, an internal bound was missed — one package still requires `^>=0.4` of a
sibling now at 0.5.0.0. Find it:

```bash
grep -rn "pgmq-\(core\|hasql\|effectful\|migration\|config\)" */*.cabal
```

**If the umbrella test fails to compile with an ambiguity error** — for example, `queueName`
being ambiguous — you have hit a real consequence of exporting record types with `(..)`: the
field names come with them. `ReadGrouped`, `SendMessage`, and several other parameter types all
have a `queueName` field. The repository already handles this with `DuplicateRecordFields` and
`OverloadedRecordDot`-free accessors via `generic-lens` (`view #queueName`), which is why the
existing `Pgmq` type exports do not conflict. Follow the existing style in the module rather
than inventing a new accessor scheme, and construct records with explicit record syntax, which
is unambiguous.

**Releasing is the one step that is hard to undo.** Nothing in this plan publishes to Hackage —
the release here means a commit and a version bump in the repository. If a problem is found
afterwards, a follow-up commit is the remedy. Do not add a `cabal upload` step to this plan; if
publishing is wanted, do it deliberately and separately, after a human has reviewed the tag.


## Interfaces and Dependencies

**No new library dependency is introduced by this release plan.** The grouped-export tests use
existing test dependencies. Plan 14 adds a direct `postgresql-libpq` test dependency for
LISTEN assertions; it remains test-only.

**Modules and files you will change:**

- `pgmq-hasql/src/Pgmq.hs` — new `FIFO / Grouped Reads` export section, two type exports, one
  Haddock prose chunk, extended imports.
- `pgmq-effectful/src/Pgmq/Effectful.hs` — the same.
- `pgmq-hasql/test/UmbrellaExportsSpec.hs` — new file.
- `pgmq-effectful/test/UmbrellaExportsSpec.hs` — new file.
- `pgmq-hasql/test/Main.hs`, `pgmq-effectful/test/Main.hs` — register the new specs.
- `pgmq-hasql/pgmq-hasql.cabal`, `pgmq-effectful/pgmq-effectful.cabal` — `other-modules` for
  the new specs, plus the version bump.
- `pgmq-core/pgmq-core.cabal`, `pgmq-migration/pgmq-migration.cabal`,
  `pgmq-config/pgmq-config.cabal` — coordinated version and internal-bound bump after plans
  9 and 13–15 have changed their package contents.
- Six `CHANGELOG.md` files.
- `README.md`, `docs/user/schema-migration.md`, `CLAUDE.md`, and a new `docs/design/` note.
- External consumer Cabal files in keiro and every shibuya adapter/example/benchmark component,
  plus shibuya's visibility-timeout consumer source.

**Files you must NOT change:** `pgmq-bench/pgmq-bench.cabal` — the benchmark executable is
unpublished and deliberately outside the lockstep versioning.

**The exact interface this plan must produce.** From `Pgmq` (package `pgmq-hasql`):

```haskell
readGrouped                   :: ReadGrouped         -> Session (Vector Message)
readGroupedWithPoll           :: ReadGroupedWithPoll -> Session (Vector Message)
readGroupedRoundRobin         :: ReadGrouped         -> Session (Vector Message)
readGroupedRoundRobinWithPoll :: ReadGroupedWithPoll -> Session (Vector Message)
readGroupedHead               :: ReadGrouped         -> Session (Vector Message)
readGroupedHeadWithPoll       :: ReadGroupedWithPoll -> Session (Vector Message)
```

plus the types `ReadGrouped (..)` and `ReadGroupedWithPoll (..)`.

From `Pgmq.Effectful` (package `pgmq-effectful`), the same six names with the effect shape:

```haskell
readGroupedHead :: (Pgmq :> es) => ReadGrouped -> Eff es (Vector Message)
```

and the same two types.

**What this plan consumes**, supplied by its prerequisites: the names
`Pgmq.Hasql.Sessions.readGroupedHead` and `readGroupedHeadWithPoll` from
`docs/plans/10-add-grouped-head-read-statements-and-sessions-to-pgmq-hasql.md`, and
`Pgmq.Effectful.Effect.readGroupedHead` and `readGroupedHeadWithPoll` from
`docs/plans/11-add-grouped-head-read-effects-and-traced-spans-to-pgmq-effectful.md`; the
breaking result types and corrected optional-parameter semantics from plan 13; the
notification migration and channel helper from plan 14; and queue validation, transient
classification, and nullable-body behavior from plan 15.


## Revision Note

2026-07-23: Expanded this existing release plan to be the single release owner for
MasterPlans 2 and 3. Added hardening prerequisites, breaking-change documentation, complete
package changelogs, Mori-backed consumer inventory, all shibuya component bounds, the
shibuya `Maybe Message` source migration, explicit rei 0.4 retention, and full consumer
build/test acceptance.
