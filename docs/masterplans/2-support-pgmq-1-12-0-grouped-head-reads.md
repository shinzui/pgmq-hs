---
id: 2
slug: support-pgmq-1-12-0-grouped-head-reads
title: "Support pgmq 1.12.0 grouped-head reads"
kind: master-plan
created_at: 2026-07-14T14:54:59Z
intention: "intention_01kxgh9geke2dayhx57qp6g9ye"
---

# Support pgmq 1.12.0 grouped-head reads

This MasterPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.


## Vision & Scope

`pgmq` is a message queue that lives entirely inside PostgreSQL: a queue is a table, and every
queue operation is a PostgreSQL function such as `pgmq.send(...)` or `pgmq.read(...)`. This
repository, `pgmq-hs`, is a Haskell client for those functions, published as five packages
(`pgmq-core`, `pgmq-hasql`, `pgmq-effectful`, `pgmq-migration`, `pgmq-config`). It currently
targets pgmq 1.11.0.

Upstream pgmq 1.12.0 adds two new PostgreSQL functions, and this initiative brings them to
Haskell users. Both concern **FIFO groups** — pgmq messages may carry a JSON header named
`x-pgmq-group`, and all messages sharing a value in that header form a group whose messages
pgmq guarantees are consumed in the order they were sent. The new functions are:

- `pgmq.read_grouped_head(queue_name, vt, qty)` — returns the single oldest message from each
  of up to `qty` *distinct* groups, never two from the same group. This is what lets ordered
  processing scale horizontally: run ten workers, each takes the head of a different group, and
  every group progresses in parallel while order inside each group is still preserved (a
  group's second message cannot be handed out until its first is deleted or its visibility
  timeout expires).
- `pgmq.read_grouped_head_with_poll(queue_name, vt, qty, max_poll_seconds, poll_interval_ms)` —
  the same, but when the queue is empty it waits, re-checking every `poll_interval_ms` for up
  to `max_poll_seconds`, instead of returning empty immediately. This is **long polling**: it
  lets an idle worker block on the database rather than spinning in a busy loop.

After this initiative, a user can install the pgmq 1.12.0 schema through `pgmq-migration`
(without needing the pgmq PostgreSQL extension, which most managed database providers
disallow), call `readGroupedHead` from either the direct `pgmq-hasql` API or the
`pgmq-effectful` effect API, and — under the traced interpreter — get an OpenTelemetry span
for the operation automatically. All five packages ship together as 0.5.0.0.

**In scope.** Vendoring upstream's 1.12.0 SQL and adding it as a native schema migration; the
`pgmq-hasql` statements and sessions; the `pgmq-effectful` effect constructors and both
interpreters, including tracing; tests at every layer; the release.

**Also in scope, deliberately.** Fixing a pre-existing gap discovered during research: **none**
of the four existing grouped reads (`readGrouped`, `readGroupedWithPoll`,
`readGroupedRoundRobin`, `readGroupedRoundRobinWithPoll`) are exported from the umbrella modules
`Pgmq` or `Pgmq.Effectful`. They are reachable only by importing an internal module, which makes
them close to undiscoverable. The two new functions would inherit the same fate. All six are
exposed on the umbrella API, together, as part of this initiative.

**Explicitly out of scope.** Upstream's ~30 other commits between v1.11.0 and v1.12.0 are all
changes to upstream's *Rust* client (`pgmq-rs`) — a `Queue` trait, diesel and rust-postgres
implementations, dependency upgrades. They have no SQL surface and therefore no bearing on a
Haskell client. Also out of scope: publishing to Hackage (the release here is a repository
commit and version bump; publishing should be a separate, human-reviewed step), and any change
to the pgmq 1.11 schema contract used for legacy history import (see the Decision Log — changing
it would break existing users).


## Decomposition Strategy

The initiative decomposes along the **layer boundaries the code already has**, because those
boundaries are exactly where the hard dependencies fall. A pgmq feature enters this repository
as SQL, becomes a `hasql` statement and session, then becomes an effect with an interpreter,
and finally becomes part of the public API. Each of those transitions is a place where the next
layer literally cannot compile — or cannot be tested — without the previous one.

That gives four child plans:

1. **SQL** (`pgmq-migration`) — vendor upstream 1.12.0 and add the native schema migration.
2. **Database layer** (`pgmq-hasql`) — the statements and sessions that call the new functions.
3. **Effect layer** (`pgmq-effectful`) — the effect constructors, both interpreters, and tracing.
4. **Public API and release** — expose all six grouped reads on the umbrella modules, bump
   versions, write changelogs and documentation.

Each produces an independently verifiable behaviour. Plan 1 ends with a database that provably
has the two new functions. Plan 2 ends with a test proving a grouped-head read returns one
message per group and never two. Plan 3 ends with a test proving the traced interpreter emits a
span labelled with the right SQL function. Plan 4 ends with a test module that imports only the
front door and compiles.

**Why not fewer plans.** Merging the SQL work into the client work would produce one plan whose
failure modes span a vendored git subtree, a compile-time-embedded migration manifest, and a
Haskell API — too much to hold at once, and impossible to verify incrementally. The `hasql` and
`effectful` layers are separable because the effect layer's tests assert something genuinely
different (telemetry, not queue behaviour) and because `pgmq-hasql` is usable on its own.

**Why not more plans.** The work is small in absolute terms — two SQL functions with identical
signatures to functions we already support. Splitting further (say, a plan per function, or
separating tests from implementation) would create plans too thin to verify independently, and
would multiply the coordination overhead past the value it buys.

**A note on how small the SQL delta turned out to be, and why that shaped everything.** Research
established that `read_grouped_head` has *exactly* the same argument list and return type as the
already-supported `read_grouped_rr`, and `read_grouped_head_with_poll` likewise matches
`read_grouped_rr_with_poll`. That means no new parameter types, no new encoders, and no
`pgmq-core` changes anywhere in the initiative — the existing `ReadGrouped` and
`ReadGroupedWithPoll` types carry over unchanged. Had that not been true, a fifth plan for the
shared types would have been needed, and it would have been a hard dependency of plans 2, 3, and
4. It is worth stating what *didn't* need doing, because a reader who assumes a new feature
implies new types will otherwise go looking for the plan that adds them.


## Exec-Plan Registry

| # | Title | Path | Hard Deps | Soft Deps | Status |
|---|-------|------|-----------|-----------|--------|
| 9 | Vendor pgmq 1.12.0 and add the native schema migration | docs/plans/9-vendor-pgmq-1-12-0-and-add-the-native-schema-migration.md | None | None | Not Started |
| 10 | Add grouped-head read statements and sessions to pgmq-hasql | docs/plans/10-add-grouped-head-read-statements-and-sessions-to-pgmq-hasql.md | EP-9 | None | Not Started |
| 11 | Add grouped-head read effects and traced spans to pgmq-effectful | docs/plans/11-add-grouped-head-read-effects-and-traced-spans-to-pgmq-effectful.md | EP-10 | None | Not Started |
| 12 | Expose grouped reads on the umbrella API and release 0.5.0.0 | docs/plans/12-expose-grouped-reads-on-the-umbrella-api-and-release-0-5-0-0.md | EP-10, EP-11 | None | Not Started |

Status values: Not Started, In Progress, Complete, Cancelled.
Hard Deps and Soft Deps reference other rows by their # prefix (e.g., EP-9, EP-10).

Note: the plan numbers continue the repository's existing `docs/plans/` sequence, which already
contained plans 1-8 from earlier, unrelated initiatives.


## Dependency Graph

The four plans form a strict chain with one join at the end:

```text
EP-9 (SQL migration)
  └─> EP-10 (pgmq-hasql statements + sessions)
        ├─> EP-11 (pgmq-effectful effects + tracing)
        └─> EP-12 (umbrella API + release)  <── also needs EP-11
```

**EP-9 → EP-10 is a hard dependency, and the reason is the test database.** `pgmq-hasql`'s test
suite does not install pgmq from the PostgreSQL extension; it starts a throwaway PostgreSQL
server and runs *this repository's own migrations* against it, via
`Pgmq.Migration.pgmqMigrations` (see `pgmq-hasql/test/EphemeralDb.hs`). So until EP-9 ships
`0003-upgrade-v1.12.0.sql`, the function `pgmq.read_grouped_head` does not exist in any database
the tests can reach, and every test in EP-10 fails with a PostgreSQL "undefined function" error.
The *library* code of EP-10 would technically compile without EP-9 — a `Statement` is just a SQL
string paired with an encoder and decoder, and nothing type-checks it against a live schema —
but it could not be verified, which under this repository's standards means it is not done.

**EP-10 → EP-11 is a hard dependency for a stronger reason: it will not compile.** The
`pgmq-effectful` interpreters are thin wrappers that call `Sessions.readGroupedHead` and
`Sessions.readGroupedHeadWithPoll` directly. Without EP-10 those names do not exist and the
package fails to build with "Variable not in scope".

**EP-10, EP-11 → EP-12 is a hard dependency for the same reason.** EP-12 re-exports names that
EP-10 and EP-11 define. It also cannot honestly write a changelog claiming pgmq 1.12.0 support
before that support exists.

**Nothing can proceed in parallel.** This is a genuinely serial initiative, which is worth
stating plainly rather than dressing up: the work is a single feature threaded through four
layers, and each layer needs the one below it. There is no fan-out to exploit. The compensating
benefit is that each plan is small, and the chain is short.

**The one place a reader might expect a dependency and find none:** EP-9 does *not* depend on
anything, and in particular it does not need to know what the Haskell API will look like. It
delivers SQL functions; what calls them is not its concern. That is why it can start
immediately.


## Integration Points

**1. The two new PostgreSQL functions.** *(EP-9 defines; EP-10 consumes; EP-11 and EP-12
transitively depend.)*

EP-9 is responsible for making these exist in a migrated database. Their signatures are the
contract:

```sql
pgmq.read_grouped_head(queue_name text, vt integer, qty integer)
  RETURNS SETOF pgmq.message_record

pgmq.read_grouped_head_with_poll(queue_name text, vt integer, qty integer,
                                 max_poll_seconds integer DEFAULT 5,
                                 poll_interval_ms integer DEFAULT 100)
  RETURNS SETOF pgmq.message_record
```

EP-10 consumes them by name and positional argument order in its SQL strings
(`"select * from pgmq.read_grouped_head($1,$2,$3)"`). Neither the names nor the argument order
may drift, and EP-9 must not "improve" the vendored SQL — it is a byte-exact copy of upstream by
design.

**2. The `ReadGrouped` and `ReadGroupedWithPoll` parameter types.** *(Pre-existing; EP-10, EP-11,
and EP-12 all use them; nobody defines them.)*

Defined in `pgmq-hasql/src/Pgmq/Hasql/Statements/Types.hs`. This is the integration point most
likely to be got wrong, because the instinct on adding a feature is to add a type for it. **Do
not.** The new SQL functions have identical signatures to the existing round-robin pair, so the
existing types already fit exactly:

```haskell
data ReadGrouped = ReadGrouped
  { queueName :: !QueueName, visibilityTimeout :: !Int32, qty :: !Int32 }

data ReadGroupedWithPoll = ReadGroupedWithPoll
  { queueName :: !QueueName, visibilityTimeout :: !Int32, qty :: !Int32,
    maxPollSeconds :: !Int32, pollIntervalMs :: !Int32 }
```

The existing code already sets this precedent — `readGrouped` and `readGroupedRoundRobin` share
`ReadGrouped` today. All three child plans that touch Haskell record this in their own Decision
Logs. Their encoders (`readGroupedEncoder`, `readGroupedWithPollEncoder` in
`pgmq-hasql/src/Pgmq/Hasql/Encoders.hs`) are likewise reused unchanged. **Consequence:
`pgmq-core` is not modified anywhere in this initiative.**

**3. The Haskell function names.** *(EP-10 defines for `pgmq-hasql`; EP-11 defines for
`pgmq-effectful`; EP-12 re-exports both.)*

```haskell
-- EP-10, in Pgmq.Hasql.Sessions:
readGroupedHead         :: ReadGrouped         -> Session (Vector Message)
readGroupedHeadWithPoll :: ReadGroupedWithPoll -> Session (Vector Message)

-- EP-11, in Pgmq.Effectful.Effect (and the GADT constructors ReadGroupedHead /
-- ReadGroupedHeadWithPoll):
readGroupedHead         :: (Pgmq :> es) => ReadGrouped         -> Eff es (Vector Message)
readGroupedHeadWithPoll :: (Pgmq :> es) => ReadGroupedWithPoll -> Eff es (Vector Message)
```

EP-11 will not compile if EP-10 renames these; EP-12 will not compile if either does. If a plan
deviates, it must update the dependent plans before committing.

**4. The `db.operation` span attribute values.** *(EP-11 defines; nothing consumes in code, but
they are a user-visible contract.)*

EP-11's traced interpreter labels the new operations `"pgmq.read_grouped_head"` and
`"pgmq.read_grouped_head_with_poll"`. These strings appear in users' trace viewers and in their
alerting queries. Note the convention, which surprises people: the OpenTelemetry span *name* is
derived from the messaging operation and the queue (`"receive orders"`), **not** from the SQL
function — the function name lands only in the `db.operation` attribute. So every pgmq read
produces a span named `receive <queue>`, and they are told apart by `db.operation`. EP-11's test
asserts on that attribute specifically, because a copy-paste slip that ran the right session
under the wrong label would otherwise mislabel every trace in production while all functional
tests still passed.

**5. The lockstep package version.** *(EP-12 owns.)*

All five library packages move 0.4.0.0 → 0.5.0.0 together, per the repository's established
convention (the root `CHANGELOG.md` says "All packages share the 0.4.0.0 version", and unchanged
packages carry a "Version bump only — coordinated release" note). `pgmq-bench` is at 0.1.0.0, is
unpublished, and is deliberately outside the lockstep. No other plan touches a version.

**6. The pgmq 1.11 schema contract — an integration point that must NOT move.** *(Nobody
changes it. Named here because it looks like it should change, and it must not.)*

`pgmq-migration/src/Pgmq/Migration/SchemaContract.hs` defines `pgmqV1_11StateValidator` and a
list of 58 required functions. It is tempting to read this as "the schema pgmq-hs needs" and add
the two new functions to it. That reading is wrong and the change would be a bug. The validator
describes the **predecessor state**: it is run against a user's *existing* database to prove that
database really was migrated to pgmq 1.11.0 by the old `hasql-migration` tooling, before its
migration history is imported into the new ledger. A legitimate pgmq 1.11.0 database does not
have `read_grouped_head`. Adding 1.12.0 functions to the list would cause the import route to
reject exactly the databases it exists to accept. It stays pinned at 1.11. This is recorded in
EP-9's Decision Log too.


## Progress

- [ ] EP-9: Vendor subtree advanced to upstream commit `08ace4087dbf00e51704c5a3d9df2e15fd566127`, new upgrade scripts verified free of extension-only SQL.
- [ ] EP-9: `0003-upgrade-v1.12.0.sql` added and wired into the manifest; `pgmq-migration` builds.
- [ ] EP-9: Existing tests updated for a three-migration component; byte-provenance re-pointed at `0003`, `0001` pinned by MD5.
- [ ] EP-9: Schema-convergence test proves the `0001 + 0003` upgrade path matches a fresh install of the vendored 1.12.0 `pgmq.sql`.
- [ ] EP-9: `docs/user/schema-migration.md`, `CLAUDE.md`, and the vendoring design note updated.
- [ ] EP-10: `readGroupedHead` / `readGroupedHeadWithPoll` `Statement` values added to `pgmq-hasql`.
- [ ] EP-10: Session wrappers added and exported.
- [ ] EP-10: Test proves a grouped-head read returns one message per group, and always the group's oldest.
- [ ] EP-10: Test proves long polling returns promptly when a message waits, and waits out the timeout when the queue is empty.
- [ ] EP-11: `ReadGroupedHead` / `ReadGroupedHeadWithPoll` effect constructors added to the `Pgmq` GADT.
- [ ] EP-11: Plain interpreter arms added.
- [ ] EP-11: Traced interpreter arms added, emitting Consumer spans with the new `db.operation` values.
- [ ] EP-11: Test proves the traced interpreter emits a span with `db.operation = "pgmq.read_grouped_head"`.
- [ ] EP-12: `Pgmq` umbrella exports all six grouped reads plus `ReadGrouped(..)` / `ReadGroupedWithPoll(..)`.
- [ ] EP-12: `Pgmq.Effectful` umbrella exports the same.
- [ ] EP-12: `UmbrellaExportsSpec` in both packages makes a dropped export a compile error.
- [ ] EP-12: All five packages bumped to 0.5.0.0; six changelogs written.
- [ ] EP-12: README, user docs, and a 1.12.0 design note updated; release commit made.


## Surprises & Discoveries

Recorded during research, before implementation began. Each of these changed the shape of the
plan and is repeated in the relevant child plan.

**Upstream has not tagged v1.12.0.** `CLAUDE.md` instructs a tag-based `git subtree pull`, which
cannot run as written. Upstream's newest tag is `v1.11.1`; the tip of `main` is
`08ace4087dbf00e51704c5a3d9df2e15fd566127` ("prepare extension v1.12.0 (#566)"), which sets
`default_version = '1.12.0'` in `pgmq.control`. The subtree is therefore pinned to that commit.
Evidence:

```text
$ git -C <upstream> tag --sort=-v:refname | head -2
v1.11.1
v1.11.0
$ git -C <upstream> log -1 --format='%H %s' origin/main
08ace4087dbf00e51704c5a3d9df2e15fd566127 prepare extension v1.12.0 (#566)
```

**The SQL delta is far smaller than the commit count suggests.** Roughly forty commits landed
upstream between v1.11.0 and v1.12.0, but nearly all are changes to upstream's Rust client. The
entire SQL surface change is two new functions, one dropped function overload, and some
`pg_dump` housekeeping. A reviewer who counts commits will badly overestimate this initiative.

**The new functions have identical signatures to functions we already support.**
`read_grouped_head(text,integer,integer)` matches `read_grouped_rr`, and
`read_grouped_head_with_poll(text,integer,integer,integer,integer)` matches
`read_grouped_rr_with_poll`. Both return `SETOF pgmq.message_record`, the row type every pgmq
read returns. This eliminates an entire work stream: no new types, no new encoders, no
`pgmq-core` changes.

**The dropped `enable_notify_insert(text)` overload is a no-op here, not a breaking change.**
Upstream's 1.11.0→1.11.1 script runs `DROP FUNCTION IF EXISTS pgmq.enable_notify_insert(queue_name text)`.
That reads alarming, but this repository's baseline declares
`pgmq.enable_notify_insert(queue_name TEXT, throttle_interval_ms INTEGER DEFAULT 250)` — and a
defaulted parameter does *not* create a second `pg_proc` entry, so `to_regprocedure('pgmq.enable_notify_insert(text)')`
is already NULL in a pgmq-hs-managed database and the `DROP ... IF EXISTS` drops nothing.
Upstream ships it to clean up databases upgraded from much older versions. `pgmq-hasql` already
calls the two-argument form. No client change is needed, and none should be invented.

**The byte-identity test in `pgmq-migration` breaks by design on the vendor bump.**
`testNativePayload` asserts `0001-install-v1.11.0.sql` is byte-identical to the vendored
`pgmq.sql`. That was true only while the vendored fresh-install script was at 1.11.0. Once the
subtree advances, the two legitimately diverge — `0001` is immutable at the 1.11 baseline. EP-9
replaces the assertion with an MD5 pin on `0001` plus a provenance check that `0003` is exactly
the two vendored upgrade scripts concatenated, and adds a schema-convergence test that is
strictly stronger than what was lost.

**Four existing grouped reads are missing from both umbrella modules.** Found while tracing the
copy target for the new functions: `readGrouped`, `readGroupedWithPoll`, `readGroupedRoundRobin`,
and `readGroupedRoundRobinWithPoll` are exported from `Pgmq.Hasql.Sessions` and
`Pgmq.Effectful.Effect`, but from neither `Pgmq` nor `Pgmq.Effectful`. There is no comment or
design note justifying the omission, and every other operation family is exported, so it appears
to be an oversight. Scope was extended (with the user's agreement) to fix all six at once in
EP-12, rather than adding the new functions to an API where their four siblings are invisible.

**No grouped read has a polling test anywhere in the repository, and no grouped read has any
test in `pgmq-effectful`.** So EP-10's polling test and EP-11's traced grouped test have no
sibling to copy and are specified from scratch in their plans.


## Decision Log

- Decision: Pin the vendored subtree to upstream commit `08ace4087dbf00e51704c5a3d9df2e15fd566127` rather than waiting for a `v1.12.0` tag.
  Rationale: Upstream has not tagged v1.12.0, so `CLAUDE.md`'s tag-based `git subtree pull` cannot run. Pinning to the exact SHA gives identical bytes with reproducible provenance and unblocks the initiative now. A re-pull once the tag lands should be a byte-level no-op; if it is not, that is a genuine upstream change and must be reviewed. The alternatives were rejected: waiting blocks all four plans indefinitely, and hand-copying the two upgrade scripts without moving the subtree pointer would violate `CLAUDE.md`'s rule that all SQL comes from the vendored source.
  Date: 2026-07-14

- Decision: Decompose by layer (SQL → hasql → effectful → API/release) rather than by feature or by package.
  Rationale: The layer boundaries are where the hard dependencies actually fall — each layer literally cannot compile or cannot be tested without the one below it. A decomposition by feature (one plan per new function) would have produced two plans that each touch all four layers, duplicating all the context and making neither independently verifiable. See Decomposition Strategy.
  Date: 2026-07-14

- Decision: Reuse the existing `ReadGrouped` and `ReadGroupedWithPoll` types across all layers; add no new types and do not modify `pgmq-core`.
  Rationale: The new SQL functions have signatures identical to the round-robin pair we already support, so the existing types fit exactly, and the existing code already shares these types across multiple functions. A structurally identical duplicate type would add a concept for users to learn and a second encoder to keep in sync, for zero type safety. Recorded in the Decision Logs of EP-10 and EP-11 as well, because that is where someone would be tempted to add one.
  Date: 2026-07-14

- Decision: Leave `pgmq-migration/src/Pgmq/Migration/SchemaContract.hs` (`pgmqV1_11StateValidator`, evidence key `pgmq_schema_contract_v1.11`) completely unchanged.
  Rationale: `CLAUDE.md` says to update the schema contract "after reviewing the pgmq-hs consumer surface", which invites adding the two new functions. Reviewing it establishes the opposite. The validator does not describe the schema pgmq-hs needs; it describes the *predecessor* state, and is run against a user's existing database to prove it really was migrated to pgmq 1.11.0 by the old tooling before importing its history. A genuine 1.11.0 database will not have `read_grouped_head`. Adding 1.12.0 functions would make the import route reject the very databases it exists to accept. The contract is a snapshot of 1.11 and must remain one.
  Date: 2026-07-14

- Decision: Extend scope to export all six grouped reads — not just the two new ones — from the `Pgmq` and `Pgmq.Effectful` umbrella modules.
  Rationale: Research found that none of the four existing grouped reads are on the umbrella API. Adding only the new pair would leave a stranger API than we started with: a front door where the newest grouped reads are visible and the older, more widely used ones are not. The change is purely additive and cannot break an existing user. Confirmed with the user before planning. Assigned to EP-12 so it happens once, in one place, rather than being smeared across EP-10 and EP-11.
  Date: 2026-07-14

- Decision: Replace the `0001`-versus-vendored-`pgmq.sql` byte-identity test with an MD5 pin on `0001` plus a provenance check on `0003`, and add a schema-convergence test.
  Rationale: The existing test breaks by design once the vendor advances past 1.11.0, since `0001` is immutable. Simply deleting it would lose the guarantee that no migration contains hand-written SQL. The replacement preserves that guarantee and adds a stronger one: that the `0001 + 0003` upgrade path produces the same schema as a fresh install of upstream's 1.12.0 script. Upstream added CI for exactly this class of bug (their commit `885251c`), and without it a `pgmq-migration`-managed database could silently differ from a stock pgmq database — surfacing as a baffling runtime error in a user's application rather than a test failure. Confirmed with the user before planning.
  Date: 2026-07-14

- Decision: Release as 0.5.0.0 across all five library packages in lockstep; do not publish to Hackage as part of this initiative.
  Rationale: Lockstep versioning is the established convention here (the root changelog says all packages share a version, and unchanged packages carry a "version bump only" note). A minor bump is correct under the Package Versioning Policy: the release is additive — new functions, new exports, a new migration — with no removals and no signature changes. Publishing is deliberately excluded because it is irreversible and deserves a separate, human-reviewed step.
  Date: 2026-07-14


## Outcomes & Retrospective

(To be filled during and after implementation.)
