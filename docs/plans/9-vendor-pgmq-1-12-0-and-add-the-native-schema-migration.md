---
id: 9
slug: vendor-pgmq-1-12-0-and-add-the-native-schema-migration
title: "Vendor PGMQ 1.12/1.13 and preserve native upgrade contracts"
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
    - model: "gpt-6-astra"
      harness: "codex-cli"
      at: 2026-09-10T17:02:21Z
      mode: "update"
      note: "Correct prior unknown attribution: the 2026-09-10 PGMQ planning refresh was authored by gpt-6-astra, verified from this session turn_context metadata."
    - model: "gpt-6-astra"
      harness: "codex-cli"
      at: 2026-09-10T17:24:05Z
      mode: "implement"
      note: "Vendor tagged SQL, append migrations, and validate upgrade contracts."
---
# Vendor PGMQ 1.12/1.13 and preserve native upgrade contracts


This ExecPlan is a living document. Keep the living sections current. Its filename and parent
link remain stable despite the expanded target.


## Purpose / Big Picture


Make a native PGMQ database reach 1.13.0 through auditable migrations while retaining its data,
legacy import path, and local partition re-entry behavior. PGMQ exposes message queues as SQL
functions. The native component installs them without requiring the PGMQ extension; partitioned
queues still require the separate pg_partman extension.

The upgrade delivers the 1.12 grouped-head functions and all three 1.13 partition changes:
configurable premake, a nullable default-partition metric, and identity columns that allow
maintenance to relocate rows with their existing message IDs. An operator can prove success by
upgrading a populated partitioned queue, recovering spilled rows, and observing a healthy
default-partition estimate without losing messages.

This plan owns SQL and test infrastructure. The direct APIs belong to
[EP-10](10-add-grouped-head-read-statements-and-sessions-to-pgmq-hasql.md), effects and config
to [EP-11](11-add-grouped-head-read-effects-and-traced-spans-to-pgmq-effectful.md), and the
0.6.0.0 release to [EP-12](12-expose-grouped-reads-on-the-umbrella-api-and-release-0-5-0-0.md).


## Progress


- [x] (2026-09-10) Milestone 1: vendored released v1.13.0, preserved a byte-exact v1.12.0 test snapshot and verified source provenance.
- [x] (2026-09-10) Milestone 2: appended 1.12, 1.13 and local re-entry migrations; original three payload digests unchanged.
- [x] (2026-09-10) Milestone 3: provenance/history tests and both catalog convergence comparisons pass, including comparator mutation checks.
- [x] (2026-09-10) Milestone 4 SQL acceptance: PostgreSQL 17.10 / pg_partman 5.4.3 passed populated prefix upgrades, identities, premake, recovery and concurrent re-entry.
- [x] (2026-09-10) Milestone 4 fixture acceptance: all three clients reject invalid schema versions and required-but-unavailable pg_partman; all four sdists contain regular-file, byte-exact 1.12 fixtures.
- [x] (2026-09-10) Milestone 5: documented upgrade/recovery behavior; required migration, notification crash and effect/config validation passes. EP-10 owns the remaining metrics decoder mismatch.


## Surprises & Discoveries


Implementation: the locked Nix package set already provides pg_partman 5.4.3. A dedicated
`partman` shell can prepend `postgresql.withPackages` without changing generated Nix wiring.
Both catalog comparisons pass without any additional deviations. The initial full-family
run exposed only metrics shape failures: one hasql NULL-semantics test and the shared crash
fixture used the old seven-column decoder. The crash fixture now selects only queue_length
so notification crash acceptance does not depend on EP-10's new decoder.

September 10 research verified v1.12.0 at `08ace4087dbf00e51704c5a3d9df2e15fd566127` and
v1.13.0 at `32c075bb6dbed66a303d1a792393c93e36c09a97`. The former is the old pre-release
pin, now tagged. The latter is the released target, not upstream main.

The manifest already has three entries and the tests derive counts from
`nativeMigrationNames`; the old two-to-three instructions are obsolete. The existing
`testNativePayload` still compares the immutable 1.11 baseline with moving vendor SQL.

The 1.13 script drops `create_partitioned(text,text,text)` and replaces it with a four-argument
function. This loses the local 0003 parent-registration guards. It also appends a metrics
composite attribute with an unguarded `ALTER TYPE`, so replaying the raw file is unsafe.
The earlier claim that this upgrade cannot overwrite hardening no longer holds.

The current partition test can print SKIPPED without failing. Such a result cannot establish
the new partition behavior. Empty schema convergence also cannot establish that the migration's
loop actually updates existing partitioned queue tables.


## Decision Log


On 2026-09-10 during implementation, use a project-specific `partman` shell from the
existing locked PostgreSQL package set. Share pristine 1.12 fixture bytes through repository
symlinks that Cabal materializes as regular source-distribution files; keep fixture loading
local to each client test helper. Use an explicit scalar SQL observation in the notification
crash test to separate server durability acceptance from EP-10's metrics record work.

The July 14 decisions to preserve the baseline checksum, canary, and legacy 1.11 validator
remain. The August 5 choice to allocate from the live manifest remains; prior 0003 examples
and the no-tag rationale are superseded.

On 2026-09-10, vendor v1.13.0 and retain v1.12.0 as a separately pinned test fixture. Append
one 1.12 upgrade composed from the two upstream scripts, one byte-exact 1.13 upgrade, and one
clearly identified local override. This keeps upstream provenance auditable while preserving
the adopted re-entry contract.

On 2026-09-10, require populated-upgrade and real pg_partman tests in addition to versioned
schema convergence. Keep function-body exceptions narrowly keyed by full signature, with
behavioral tests for the exception. Never weaken all body comparisons to make a test green.

The durable decision is [the 1.12/1.13 ADR](../adr/pgmq-1.12-1.13-compatibility.md).


## Outcomes & Retrospective


EP-9 is complete. The SQL implementation passes all 11 migration tests, including real
partition behavior, on PostgreSQL 17.10 / pg_partman 5.4.3. The vendor update is commit
`9137048`. Required native runs also pass all 30 effectful and 20 config tests, including
the real SIGQUIT crash and subsequent notification. Client 1.12 mode passes 73 hasql,
30 effectful and 20 config tests; local partition re-entry is explicitly skipped in stock
mode, and the notification-race group runs only on the native ledger. Dedicated crash and
foreign-queue fixtures still test their own native installations.

The native hasql suite's sole remaining failure is the expected seven-versus-eight-column
metrics decoder in `pop with qty = Nothing`; EP-10 owns that decoder. The remaining native
hasql tests pass, including partition re-entry and 200 concurrent notification-enable pairs.
This is a recorded cross-plan incompatibility, not a full-family green result.

The v1.12 fixture SHA-256 is
`be087bfcb0ec5e65abb76610249750f2ec8dc757956125a1b40430ce95fc7f0f`.
New migration SHA-256 digests, in manifest order:

```text
0004-upgrade-v1.12.0.sql adbd78cfa6f6d093417627ce4ab608a5930d43caf4b7b16d9773a23ff9e3effd
0005-upgrade-v1.13.0.sql e494ff99e02ff6f4e886ee257421145cb510ad4f950835480896437a7fab41d2
0006-preserve-partitioned-reentry-v1.13.0.sql 63f34f86679639dd422346767c48e1965e48055869b56c4dc5cbb56744009033
```

The repeatable required-extension command is:

```bash
nix develop .#partman --command cabal test pgmq-migration:pgmq-migration-test --test-show-details=direct
```

The schema comparator has exactly the planned version-specific body exceptions and compares
all signatures/defaults. Prefix upgrade tests use the actual migration ledger; procedures
recover queue and archive defaults in separate top-level requests and preserve ID/payload sets.

Additional validation commands:

```bash
PGMQ_TEST_SCHEMA_VERSION=1.12.0 nix develop .#partman --command cabal test pgmq-hasql pgmq-effectful pgmq-config --test-show-details=direct
nix develop .#partman --command cabal test pgmq-config pgmq-effectful --test-show-details=direct
nix develop .#partman --command cabal test pgmq-hasql --test-show-details=direct --test-options='-p "! /pop with qty = Nothing/"'
cabal sdist pgmq-migration pgmq-hasql pgmq-effectful pgmq-config --output-directory=/tmp/pgmq-ep9-sdist
nix fmt
git diff --check
```

Direct test-executable negative runs also proved invalid-version failure for each client
and missing-required-partman failure for each client and the migration suite. The ordinary
migration run passes with an explicit optional partition skip; it is not the partition evidence.


## Context and Orientation


Run commands from the repository root inside `nix develop`, which currently provides GHC
9.12.4, Cabal and PostgreSQL. Project-specific development additions belong in
`flake.module.nix`; `nix/haskell.nix` is generated wiring. Discover dependency sources with
Mori before selecting APIs, and verify any new dependency version against its registry/tags.

`pgmq-migration/migrations/manifest` currently contains:

```text
0001-install-v1.11.0.sql
0002-schema-management-comment.sql
0003-notify-crash-safety-and-locking.sql
```

`Pgmq.Migration.Internal.Definition` embeds the manifest through
`embedMigrationManifest` and exposes `pgmqMigrations :: Either DefinitionError MigrationComponent`.
Unlisted SQL files, reordered history and changed payload checksums are not permitted.
`pgmq-migration/pgmq-migration.cabal` packages migrations and explicit vendor source files.
`pgmq-migration/vendor` points to the repository vendor directory.

`0001` has MD5 `faa9b8800005f80fbdf6a33d071183d2`, corresponding to legacy checksum
`+qm4gAAF+A+99qM9BxGD0g==`. `History/HasqlMigration.hs` imports direct and equivalent
1.11 histories. `SchemaContract.hs` validates the predecessor, including the old
three-argument partition signature and seven-field metrics type; do not expand this contract
to describe the new final state. `0002` supplies the exact schema canary comment used by tests.

`0003` locally redefines `notify_queue_listeners()`, `enable_notify_insert(text,integer)`,
and `create_partitioned(text,text,text)`. It keeps crash-missing notification throttle state
from suppressing delivery, locks enable calls, and guards pg_partman registration for both
queue and archive parents. Read the actual SQL and
[design note 015](../design/015-notification-delivery-contract.md). The new upstream partition
function already takes the queue lock but lacks those two registration guards.

The upstream project is `mori://pgmq/pgmq`, discovered with `mori registry show pgmq/pgmq --full`.
Its individual source artifact URIs are pending: paths in this paragraph are relative to that
project's upstream repository. `pgmq-extension/sql/pgmq--1.11.0--1.11.1.sql` contains a guarded
extension dump block, grouped-head read, the helper `_ensure_pg_partman_installed`, and a drop
of the obsolete one-argument notify overload. `pgmq--1.11.1--1.12.0.sql` adds grouped-head
polling. `pgmq--1.12.0--1.13.0.sql` converts existing partitioned queue identities, drops the
three-argument partition function, creates the four-argument one with premake default 4 and
a lower-bound check, appends `default_partition_length bigint` to `metrics_result`, and
redefines `metrics`. `metrics_all` inherits the new row type without a new function signature.

The metric sums nonnegative planner `reltuples` estimates for both default partitions.
It returns NULL for ordinary queues, zero for empty defaults, and can lag real writes until
ANALYZE/autovacuum. Identity conversion affects active partitioned queue tables only; archives
already accept explicit IDs and ordinary queues retain their old identity mode.

Relevant durable context is [vendoring](../design/012-vendor-upstream-pgmq-sql.md),
[notification hardening](../design/015-notification-delivery-contract.md), and
[the new compatibility ADR](../adr/pgmq-1.12-1.13-compatibility.md). The ADR directory was
introduced by this refresh; it has no OKF profile.


## Plan of Work


### Milestone 1 — Pin released sources


Advance `vendor/pgmq` to v1.13.0 using the existing subtree workflow, preserving pristine
upstream bytes. Confirm the tag SHA before pulling. A subtree pull makes a commit itself;
include the MasterPlan, ExecPlan and Intention trailers in its merge message rather than
assuming a later empty commit can add them. Preserve unrelated working-tree changes.

Save byte-exact v1.12.0 fresh-install SQL at
`pgmq-migration/test/fixtures/pgmq-1.12.0.sql` through a reproducible extraction of
`pgmq-extension/sql/pgmq.sql` from the pinned v1.12.0 tag. Add a short provenance comment in
the test code, not inside the copied SQL. Record a SHA-256 digest for the fixture and verify
it against the tag before accepting it. Use apply_patch for repository edits; a small script
may derive and apply an exact patch from source bytes rather than manually transcribing SQL.
Package the fixture through `extra-source-files`.

Inspect all three upgrade scripts for `ALTER EXTENSION`, `@extschema@` and extension-dependent
operations. The guarded `pg_extension_config_dump` block is inert for native installs and
must remain. Verify `pgmq.control` says 1.13.0. The old baseline/vendor equality test will
fail until milestone 3; that is expected, not permission to edit 0001.

Acceptance: both tag identities, the fixture digest, and the vendored version agree; the
upgrades have been reviewed for native execution, including 1.13's data and type alterations.


### Milestone 2 — Append upgrades and restore re-entry


Re-read the manifest. With the current three entries, append these files in this order:

```text
0004-upgrade-v1.12.0.sql
0005-upgrade-v1.13.0.sql
0006-preserve-partitioned-reentry-v1.13.0.sql
```

The first is exactly bytes(1.11.0→1.11.1) + one newline + bytes(1.11.1→1.12.0). The second
is exactly bytes(1.12.0→1.13.0). Do not merge them: the separate checkpoint makes version
testing and upgrade evidence unambiguous. Add all source scripts to the Cabal source list.

The third migration is a local `CREATE OR REPLACE` of
`create_partitioned(text,text,text,integer)` derived from the new upstream function. Retain
its identity mode, explicit premake validation and forwarding to both create_parent calls,
retention setup, archive layout, and advisory locking. Port only the queue and archive
`part_config` existence guards from 0003. Do not reintroduce the three-argument overload or
reapply 0003, which would undo the new signature and recreate old behavior. Label the local
migration and reference the ADR; it is an explicit exception to pristine upstream payloads.

Acceptance: `cabal build pgmq-migration` passes. A complete fresh migration has both grouped
functions, the four-argument partition function, no three-argument catalog entry, and the
eighth metrics attribute. Three-argument SQL calls still resolve via defaults. Migration
tests establish this without depending on the not-yet-updated Haskell metrics decoder.


### Milestone 3 — Preserve history and prove both schema checkpoints


Update `pgmq-migration/test/Main.hs`. Replace `testNativePayload` with a fixed baseline MD5
check (PostgreSQL can calculate it over the existing connection), exact concatenation equality
for 0004, exact upstream equality for 0005, and explicit source/behavior checks for 0006.
Pin unchanged 0002/0003 bytes as well, recording their current digests before editing.
Do not claim the local override is byte-equal to upstream.

Add the new names to `testNativeComponent`; other counts already derive from
`nativeMigrationNames` and `pendingAfterBaseline`. Preserve all shared-ledger, checksum-rejection
and equivalent-history tests, including their original 1.11 fixture inputs. After import,
verify only pending suffix entries run, the final schema is 1.13, and another component run
reports AlreadyApplied for every entry. The deliberate removed-metrics_all test still proves
the baseline was not replayed; neither upgrade recreates that function.

Compare a database at the 0004 checkpoint with the pinned 1.12 fresh fixture, and the complete
ledger with fresh vendored 1.13 SQL. Use isolated ephemeral databases or serial clean resets;
never compare a 1.12 checkpoint with 1.13 SQL. Build sorted snapshots of full function identity,
return type, argument defaults and body, ordered composite attributes, tables/columns and
constraints. Exclude only the deliberate schema comment. Do not fabricate identical object
IDs or compare database-specific OIDs.

Allow body differences for `notify_queue_listeners()` and `enable_notify_insert(text,integer)`
at both versions; for `create_partitioned(text,text,text)` at 1.12; and for
`create_partitioned(text,text,text,integer)` at 1.13. All signatures/defaults remain compared.
Require every allowlist entry to exist and cite its local override. Do not keep the old
three-argument entry at 1.13 or ignore unexpected extra overloads. Other bodies must match.
If an upstream divergence appears, record the exact object and source diff and resolve it
explicitly rather than weakening all comparisons.

Add a test of the snapshot comparator using a modified in-memory snapshot (missing grouped
function, wrong eighth attribute, or altered unallowlisted body). It must report the changed
object. Do not corrupt migration SQL with line deletion to prove the test works.

Acceptance: migration tests pass at both checkpoints, provenance failures are detectable,
the baseline import still accepts genuine 1.11 state, and the final metrics type has eight
ordered attributes.


### Milestone 4 — Verify real partition behavior and provision the test matrix


Own the reproducible pg_partman test setup. The current Nix shell uses plain PostgreSQL;
inspect Mori sources and the selected environment's official package definitions before
adding a project-specific `flake.module.nix` development shell or a dedicated test command.
Record exact PostgreSQL/pg_partman versions actually exercised and the command here.
pg_partman must be installed in the ephemeral database, not merely discoverable through
`pg_available_extensions`. No new production extension is installed by the Haskell library.

Add `PGMQ_REQUIRE_PARTMAN=1` handling to partition fixtures so required runs fail on absent
or unusable pg_partman. Ordinary local suites may still report an explicit skip, but these
are not release evidence. Reuse/update `pgmq-hasql/test/QueueSpec.hs` and fixture setup in
`pgmq-hasql/test/EphemeralDb.hs`, `pgmq-effectful/test/EphemeralDb.hs`, and
`pgmq-config/test/EphemeralDb.hs`. EP-10/11 own feature assertions, not a second
environment implementation.

Provide test-only `PGMQ_TEST_SCHEMA_VERSION` selection. Default/1.13.0 runs the full native
component. The 1.12.0 mode runs the pristine pinned 1.12 fixture on a clean test database;
keep local-hardening-only tests scoped to native mode rather than claiming stock upstream
has local fixes. Locate fixture bytes explicitly from the repository test setup and package
them where needed; do not rely on a machine-local research clone. Ordinary source-distribution
tests must continue working in the default mode. Invalid version values fail immediately.

For SQL acceptance, first install the old three-entry native ledger and create both numeric-
and time-partitioned queues with real pg_partman. Insert messages, retain their IDs and
payloads, create an ordinary queue too, then apply the new suffix. Assert active partitioned
identities become BY DEFAULT, ordinary identities remain ALWAYS, archive IDs are unchanged,
and all message data remains. Repeat from the 1.12 checkpoint to prove a partially upgraded
native ledger reaches the same final state.

Create 1.13 queues with omitted premake and explicit 2; inspect both queue and archive
`part_config` rows for 4 and 2 respectively. Zero/negative explicit values must fail before
creating queue tables/meta entries. Repeat sequential and concurrent creation under the local
override; both callers finish, without duplicate parent registration. Exercise existing
notification regression tests against the full native ledger to prove they still work.

Use an integer interval 10, retention 100 and premake 2, as upstream's test does. Send a burst
beyond the created range, prove a default partition contains rows, and ANALYZE both defaults.
Verify metrics observes the sum including a message moved to the archive default, and NULL
for ordinary/unlogged queues. Recover queue and archive through the installed pg_partman's
partition-data procedure outside a transaction, then run maintenance and ANALYZE again.
Compare complete message ID/payload sets before and after; the recovered defaults should be
empty and report zero. Disable background maintenance in this fixture so the timing is
deterministic, and keep all work in disposable databases. Resolve pg_partman's actual schema
instead of hard-coding `public` or `partman`.

Acceptance: the required-partman run executes these cases, preserves IDs/data and local
re-entry, and reports server versions with zero missing-extension skips. This plan establishes
SQL behavior; the direct metrics decoder changes arrive in EP-10.


### Milestone 5 — Document the final upgrade and validate


Update `docs/user/schema-migration.md` and `pgmq-migration/README.md` with the real manifest,
1.13 final state, the separate 1.12 checkpoint, and immutable legacy-import semantics.
Update the stale module-per-version instructions in `docs/design/012-vendor-upstream-pgmq-sql.md`
to the actual manifest architecture and distinguish pristine upstream scripts from approved
local hardening. Reflect the released tag in `CLAUDE.md` while preserving any user edits.

Document default-partition estimates and operator recovery for both queue and archive, with
the need to ANALYZE and run partition_data_proc outside a transaction. The schema upgrade
enables recovery; it does not evacuate spills, repair sequences from arbitrary direct inserts,
or schedule maintenance. Do not claim native installation removes the pg_partman requirement.
Update the ADR and [notification contract](../design/015-notification-delivery-contract.md)
with implemented four-argument preservation once verified.

Acceptance: `cabal test pgmq-migration:pgmq-migration-test` and required partition SQL tests
pass; formatting and `git diff --check` are clean. Record any temporary cross-package failures
caused by 1.13's changed metrics row until EP-10; the full-family green gate is EP-12.


## Concrete Steps


From the repository root, start with:

```bash
git status --short
mori registry show pgmq/pgmq --full
mori registry docs pgmq/pgmq
git ls-remote --tags https://github.com/pgmq/pgmq.git v1.12.0 v1.13.0
cat pgmq-migration/migrations/manifest
nix develop
```

Expected remote SHAs are recorded in Surprises & Discoveries. Pull the verified v1.13.0
subtree using an explicit commit message containing these trailers:

```text
MasterPlan: docs/masterplans/2-support-pgmq-1-12-0-grouped-head-reads.md
ExecPlan: docs/plans/9-vendor-pgmq-1-12-0-and-add-the-native-schema-migration.md
Intention: intention_01kxgh9geke2dayhx57qp6g9ye
```

After deriving the new payloads and tests, run:

```bash
cabal build pgmq-migration
cabal test pgmq-migration:pgmq-migration-test --test-show-details=direct
PGMQ_REQUIRE_PARTMAN=1 cabal test pgmq-migration:pgmq-migration-test --test-show-details=direct
git diff --check
```

The last test command is run in the pg_partman-capable environment added in milestone 4,
not assumed to work in today's plain shell. Capture its exact environment entry command.
The downstream compatibility commands after EP-10 are:

```bash
PGMQ_TEST_SCHEMA_VERSION=1.12.0 cabal test pgmq-hasql --test-options='--pattern "GroupedHead"'
PGMQ_REQUIRE_PARTMAN=1 PGMQ_TEST_SCHEMA_VERSION=1.13.0 cabal test all --test-show-details=direct
```

Expected success means the named tests ran and passed, not a filter selecting zero tests.
Stage only this plan's files and use Conventional Commits with the trailers above.


## Validation and Acceptance


Accept only when the two upstream payloads have exact provenance, existing ledger bytes are
unchanged, both schema comparisons pass with their version-specific allowlists, and all
legacy-history rejection/import tests retain their meaning. The native final state has one
four-argument partition function, both grouped-head functions, and an eight-field metrics type.

The required pg_partman run must show default/custom premake on both parents, valid re-entry,
identity conversion on existing active queues, unchanged ordinary queues, nullable metrics,
archive-inclusive estimates, and data-preserving recovery. A schema snapshot or skipped
partition test alone is insufficient. Repeat the component through the ledger to prove safe
AlreadyApplied behavior; do not rerun the raw type-altering script.


## Idempotence and Recovery


Migration application is idempotent through the ledger. Upstream 1.13's ADD ATTRIBUTE is not
raw-script idempotent. Follow the migration runner's actual transaction policy; on failure
inspect the ledger and schema before retrying. An applied migration is immutable and needs a
new forward repair if defective. Never delete ledger rows to force replay.

Complete the full suffix before starting application queue-creation traffic. If execution
stops after the upstream 1.13 step but before the local override, the database temporarily
lacks local partition re-entry guards; resume through the ledger and apply the pending local
step before declaring the upgrade healthy. Do not assume the runner makes all three files
one transaction without checking its actual policy.

In disposable tests, reset databases and reinstall pg_partman as required; dropping queue
schemas while leaving stale pg_partman configuration can poison later fixtures. Test cleanup
must remove fixture-owned parent registrations or discard the whole database.

Preserve pre-change files and user edits when correcting repository work. Do not use blanket
checkout/reset commands. Subtree reversal requires reviewing dependent commits; it is not
a database rollback. No production database is part of this plan.


## Interfaces and Dependencies


The production `pgmqMigrations` type and legacy validator interfaces remain unchanged.
Current migration-engine bounds are retained; this plan does not select a new Haskell
dependency version. Use Mori before changing any dependency API. Test support for pg_partman
is an environment requirement and must not silently become a production dependency installer.

EP-9 owns migrations, fixture provenance, schema snapshots, and the shared test settings.
EP-10 owns metrics decoding and direct feature tests. EP-11 owns effect/config behavior tests.
The versioned allowlists, new SQL signatures and native hardening exception are the exact
contracts described above and in the ADR.


## Revision Note


2026-09-10 implementation: completed all five milestones; recorded tagged payload hashes,
required-partman acceptance, client fixture validation, source-distribution contents and the
expected metrics decoder handoff to EP-10. Updated the ADR and replaced obsolete per-version
module instructions with the actual immutable manifest workflow.

2026-09-10: Replaced obsolete pre-release/three-migration instructions with the released
1.12/1.13 chain, populated upgrade coverage, versioned convergence, and a separate local
four-argument re-entry override. Added required pg_partman testing and test-only schema
selection; preserved existing checksum and predecessor contracts.

2026-09-10 (provenance correction): The session's recorded model for the planning refresh was
`gpt-6-astra`. Added a corrective revision entry with the verified model and `codex-cli`
harness; retained the earlier `unknown` entry to preserve append-only provenance history.
