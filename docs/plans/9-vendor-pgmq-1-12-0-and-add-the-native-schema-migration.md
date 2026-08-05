---
id: 9
slug: vendor-pgmq-1-12-0-and-add-the-native-schema-migration
title: "Vendor pgmq 1.12.0 and add the native schema migration"
kind: exec-plan
created_at: 2026-07-14T14:55:11Z
intention: "intention_01kxgh9geke2dayhx57qp6g9ye"
master_plan: "docs/masterplans/2-support-pgmq-1-12-0-grouped-head-reads.md"
---

# Vendor pgmq 1.12.0 and add the native schema migration

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.


## Purpose / Big Picture

`pgmq` is a message queue that lives entirely inside PostgreSQL: queues are tables in a
schema named `pgmq`, and every queue operation is a PostgreSQL function such as
`pgmq.send(...)` or `pgmq.read(...)`. This repository, `pgmq-hs`, is a Haskell client for
those functions.

Most people install pgmq as a PostgreSQL *extension* (a `CREATE EXTENSION pgmq;` statement
that requires a compiled `.so` file to be present on the database server). Many managed
PostgreSQL providers do not allow installing arbitrary extensions. The `pgmq-migration`
package in this repository exists to solve that: it ships the pgmq schema as plain SQL
migrations that create the same schema, tables, types, and functions *without* the
extension. We call this the "native" install.

Today `pgmq-migration` installs pgmq version 1.11.0. Upstream pgmq 1.12.0 adds two new
PostgreSQL functions that this repository's Haskell client will want to call:

- `pgmq.read_grouped_head(queue_name text, vt integer, qty integer)` — reads the single
  oldest ("head") message from each of up to `qty` distinct FIFO groups, in one statement.
  A "FIFO group" is simply a set of messages that carry the same value in the JSON message
  header `x-pgmq-group`; pgmq guarantees messages within one group are consumed in order.
  This function lets a fleet of workers each grab the head of a *different* group, so many
  groups progress in parallel while order is still preserved inside each group.
- `pgmq.read_grouped_head_with_poll(queue_name text, vt integer, qty integer,
  max_poll_seconds integer, poll_interval_ms integer)` — the same thing, but if no message
  is available it keeps re-checking every `poll_interval_ms` milliseconds until
  `max_poll_seconds` elapses, instead of returning empty immediately. This is "long
  polling": it lets a worker wait for work without hammering the database in a tight loop.

After this plan is complete, a database migrated by `pgmq-migration` will have both of
those functions, and you can prove it by connecting with `psql` and calling them. This
plan delivers **only the SQL layer**. The Haskell functions that call these SQL functions
are added by `docs/plans/10-add-grouped-head-read-statements-and-sessions-to-pgmq-hasql.md`,
which cannot be implemented until this plan is done, because its tests run against a
database migrated by this package.

You will see it working by running the `pgmq-migration` test suite, which spins up a
throwaway PostgreSQL server, applies the migrations, and asserts the new functions exist
and that the upgraded schema is indistinguishable from a fresh 1.12.0 install.


## Progress

- [ ] Milestone 1: vendor tree advanced to upstream pgmq at commit `08ace4087dbf00e51704c5a3d9df2e15fd566127` ("prepare extension v1.12.0"), with the two new upgrade scripts present under `vendor/pgmq/pgmq-extension/sql/` and confirmed free of extension-only SQL patterns.
- [ ] Milestone 2: `<NNNN>-upgrade-v1.12.0.sql` added at the next free manifest number (`0003` if this plan lands first, `0004` if plan 14's migration is already there), listed in the manifest, and the two new vendored upgrade scripts added to `extra-source-files`; `cabal build pgmq-migration` succeeds.
- [ ] Milestone 3: `pgmq-migration/test/Main.hs` updated — byte-provenance test re-pointed at the new migration, `0001` baseline pinned by hash, migration counts and history-import outcome lists grown by one from whatever the live manifest holds; `cabal test pgmq-migration:pgmq-migration-test` passes.
- [ ] Milestone 4: schema-convergence test added proving the full migration ledger produces the same `pgmq` schema as a fresh install of the vendored 1.12.0 `pgmq.sql`, carrying an explicit deliberate-deviation allowlist — seeded with plan 14's three functions if that migration has landed, empty otherwise; suite passes.
- [ ] Milestone 5: `docs/user/schema-migration.md` and `CLAUDE.md` updated to describe the three-migration component; `nix fmt` clean.


## Surprises & Discoveries

(None yet.)


## Decision Log

- Decision: Pin the vendor bump to upstream commit `08ace4087dbf00e51704c5a3d9df2e15fd566127` rather than a `v1.12.0` git tag.
  Rationale: Upstream has not tagged `v1.12.0` yet. Its newest tag is `v1.11.1`; the tip of `main` is the commit `prepare extension v1.12.0 (#566)`, which sets `default_version = '1.12.0'` in `pgmq.control`. `CLAUDE.md` instructs a tag-based `git subtree pull`, which cannot run as written. Pinning to the exact SHA gives identical bytes with reproducible provenance and unblocks the rest of the initiative. A follow-up re-pull once the tag lands should be a byte-level no-op; if it is not, that is a real upstream change and must be reviewed.
  Date: 2026-07-14

- Decision: Do not reserve a migration number. Read `pgmq-migration/migrations/manifest` at implementation time and claim the next free one; this plan's `0003-upgrade-v1.12.0.sql` is an illustration of the filename, and the slug is the part that is fixed.
  Rationale: The decision on 2026-08-05 to implement `docs/masterplans/3-harden-the-pgmq-hs-family-surfaced-by-the-2026-07-review.md` first means its plan 14 migration will very likely occupy `0003` before this plan runs, making this one `0004`. External keiro MasterPlan 17 plans 116 and 118 may also append. A plan that hard-codes a number silently collides with whatever actually landed, and the manifest is the executable authority in a way no plan can be. Ordering is safe in both directions: the vendored upgrade scripts redefine only `read_grouped_head`, `_ensure_pg_partman_installed`, and `read_grouped_head_with_poll`, none of which plan 14 touches, so neither migration can clobber the other (verified 2026-08-05).
  Date: 2026-08-05

- Decision: Keep `pgmq-migration/migrations/0001-install-v1.11.0.sql` byte-for-byte immutable and add the 1.12.0 changes as a new appended migration (`0003-upgrade-v1.12.0.sql` if this plan lands first).
  Rationale: `CLAUDE.md` mandates it, but there is a concrete mechanical reason too. `pgmq-migration` embeds migration bytes at compile time and the `hasql-migration` history-import route in `pgmq-migration/src/Pgmq/Migration/History/HasqlMigration.hs` re-hashes the bytes of the *first* manifest entry (`directPayload = case embeddedMigrationEntries of (_, payload) :| _ -> payload`) and compares the MD5 against a checksum recorded in a user's pre-existing legacy ledger (`+qm4gAAF+A+99qM9BxGD0g==`). Editing `0001` would change that hash and would make every existing user's history import fail with `HasqlMigrationChecksumMismatch`. Appending is safe; editing or reordering is not.
  Date: 2026-07-14

- Decision: Do **not** add the two new 1.12.0 functions to `requiredFunctions` in `pgmq-migration/src/Pgmq/Migration/SchemaContract.hs`, and leave `pgmqV1_11StateValidator` / the evidence key `pgmq_schema_contract_v1.11` untouched.
  Rationale: That validator does not describe "the schema pgmq-hs needs". It describes the *predecessor* state — it is run against a user's existing database to prove that database really was migrated to pgmq 1.11.0 by the old `hasql-migration` tooling, before its history rows are imported into the new ledger. A legitimate 1.11.0 database will not have `read_grouped_head`. Adding 1.12.0 functions to this list would cause the equivalent-history import route to reject exactly the databases it exists to accept. The contract is a snapshot of 1.11, and must stay one.
  Date: 2026-07-14

- Decision: Compose `0003-upgrade-v1.12.0.sql` as the byte-exact concatenation of the two vendored upstream upgrade scripts (`pgmq--1.11.0--1.11.1.sql` followed by `pgmq--1.11.1--1.12.0.sql`), joined by a single newline, with no hand-written SQL.
  Rationale: `CLAUDE.md` forbids hand-writing SQL — all SQL must come from the vendored source. Upstream ships the 1.11.0 → 1.12.0 path as two scripts because it released a 1.11.1 patch in between. This repository does not track upstream patch releases as separate migrations, so both are applied as one step. Concatenation preserves upstream's exact statements and lets a test re-derive the file from the vendored bytes, which is what makes the provenance check meaningful.
  Date: 2026-07-14

- Decision: Include upstream's `DROP FUNCTION IF EXISTS pgmq.enable_notify_insert(queue_name text);` verbatim even though it is a no-op here.
  Rationale: `0001-install-v1.11.0.sql` declares `pgmq.enable_notify_insert(queue_name TEXT, throttle_interval_ms INTEGER DEFAULT 250)`. A defaulted parameter does *not* create a second `pg_proc` entry, so `to_regprocedure('pgmq.enable_notify_insert(text)')` is already `NULL` in a pgmq-hs-managed database and the `DROP ... IF EXISTS` will drop nothing. Upstream ships it to clean up databases that upgraded from much older pgmq versions where a genuine one-argument overload did exist. Keeping it verbatim preserves byte-provenance against the vendored source at zero risk. It is called out here so a future reader does not mistake it for a breaking change to this repository's API — `pgmq-hasql` already calls the two-argument form (`select from pgmq.enable_notify_insert($1, $2)`).
  Date: 2026-07-14


- Decision: Write the schema-convergence test with a deliberate-deviation allowlist (signature-only comparison for listed functions, signature-and-body for all others), empty when this plan lands.
  Rationale: `docs/plans/14-make-insert-notifications-survive-crashes-and-document-the-channel-contract.md` adds a later migration that redefines `pgmq.notify_queue_listeners`, `pgmq.enable_notify_insert`, and `pgmq.create_partitioned` on purpose. An unconditional body-equality assertion turns red the moment that lands, on a correct change, and the tempting repair — deleting the body comparison — would throw away the guarantee for the other ~55 functions, which is the only reason this test exists. Making the two plans hard dependencies of each other was rejected: they need a shared mechanism, not an ordering. This also means a body mismatch has two distinct diagnoses, and the plan now tells you how to tell them apart. Recorded in `docs/masterplans/2-support-pgmq-1-12-0-grouped-head-reads.md` Integration Point 7 as well.
  Date: 2026-08-05


## Outcomes & Retrospective

(To be filled during and after implementation.)


## Context and Orientation

### Where you are

The repository root is `/Users/shinzui/Keikaku/bokuno/libraries/pgmq-hs-project/pgmq-hs`.
All paths below are relative to it. Every command in this plan is run from the repository
root unless stated otherwise.

Enter the development shell first — it provides GHC 9.12.4, `cabal`, and PostgreSQL
binaries:

```bash
nix develop
```

### The packages, briefly

`pgmq-hs` is a multi-package Cabal project. The package this plan changes is
`pgmq-migration`. The other packages (`pgmq-core`, `pgmq-hasql`, `pgmq-effectful`,
`pgmq-config`) are Haskell client code and are **not** touched by this plan, but they
depend on `pgmq-migration` in their test suites: both `pgmq-hasql/test/EphemeralDb.hs` and
`pgmq-effectful/test/EphemeralDb.hs` call `Pgmq.Migration.pgmqMigrations` to install the
pgmq schema into a throwaway database before running their tests. That is why this plan is
a hard prerequisite for the rest of the initiative.

### How `pgmq-migration` is built

The package ships raw `.sql` files and embeds their bytes into the compiled library at
build time. The moving parts:

- `pgmq-migration/migrations/` holds the SQL files and a plain-text file named `manifest`.
- `pgmq-migration/migrations/manifest` lists the SQL files, one filename per line, **in
  the order they must be applied**. Today it contains exactly two lines:

  ```text
  0001-install-v1.11.0.sql
  0002-schema-management-comment.sql
  ```

  The manifest is strictly validated at compile time. Blank lines, comment lines, absolute
  paths, nested paths, and duplicate entries are all build errors. Critically, a `.sql`
  file that exists in `migrations/` but is *not* listed in the manifest is also a build
  error (`UnlistedSqlFiles`). So dropping a new SQL file into the directory without adding
  a manifest line will fail the build, loudly. That is intentional.

- `pgmq-migration/src/Pgmq/Migration/Internal/Definition.hs` embeds the manifest and the
  files it names, using Template Haskell, and builds a `MigrationComponent` named `"pgmq"`:

  ```haskell
  embeddedMigrationEntries :: NonEmpty (FilePath, ByteString)
  embeddedMigrationEntries =
    $(embedMigrationManifest "migrations/manifest")

  pgmqMigrations :: Either DefinitionError MigrationComponent
  pgmqMigrations =
    migrationComponentFromEmbeddedSql "pgmq" mempty embeddedMigrationEntries
  ```

  The `{-# OPTIONS_GHC -fplugin=Database.PostgreSQL.Migrate.Embed.RecompilePlugin #-}`
  pragma at the top of that file exists because GHC 9.12 cannot otherwise tell that the
  module depends on a whole *directory*; without it, adding a SQL file would leave stale
  embedded bytes in the build.

- Each migration gets an identifier derived by stripping `.sql` from its filename. So the
  new file `0003-upgrade-v1.12.0.sql` becomes the migration id `pgmq / 0003-upgrade-v1.12.0`.

- `pgmq-migration/src/Pgmq/Migration.hs` is a thin re-export exposing `pgmqMigrations`,
  `MigrationComponent`, and `DefinitionError`.

The underlying migration engine is the third-party library `pg-migrate` (version 1.1.x). It
records which migrations have been applied in a ledger stored in a PostgreSQL schema named
`pgmigrate`. You do not need to understand its internals; you only need to know that
applying the plan twice is safe and the second run reports every migration as
`AlreadyApplied` rather than re-running it.

### The vendored upstream SQL

`vendor/pgmq/` is a copy of the upstream pgmq repository (https://github.com/tembo-io/pgmq),
maintained as a **git subtree**. A git subtree is just upstream's files committed directly
into this repository under a prefix directory, with a merge commit recording where they came
from — no submodule, no separate clone needed by anyone who checks this repo out.

The two files that matter:

- `vendor/pgmq/pgmq-extension/sql/pgmq.sql` — the *fresh install* script. Running it on an
  empty database produces the complete, current pgmq schema.
- `vendor/pgmq/pgmq-extension/sql/pgmq--<from>--<to>.sql` — *upgrade* scripts, one per
  upstream version step, that transform a database at version `<from>` into version `<to>`.

`pgmq-migration/vendor` is a symbolic link pointing at `../vendor`, which is how the
Template Haskell `embedFile` calls inside the package resolve those paths.

**A rule this repository enforces (see `CLAUDE.md`): all SQL must come from the vendored
source. Do not hand-write SQL.**

### The existing migrations

- `0001-install-v1.11.0.sql` (63 KB, 2075 lines) is a **byte-for-byte identical copy** of
  the vendored `vendor/pgmq/pgmq-extension/sql/pgmq.sql` as it stood at upstream tag
  `v1.11.0`. Its MD5 is `faa9b8800005f80fbdf6a33d071183d2`. It is immutable, for the
  reasons in the Decision Log.

- `0002-schema-management-comment.sql` is a two-line marker, sometimes called the "canary",
  whose entire body is:

  ```sql
  COMMENT ON SCHEMA pgmq IS
    'Managed by pg-migrate component pgmq through 0002-schema-management-comment';
  ```

  Its purpose is to leave a visible fingerprint on the database proving the schema is
  managed by this component. Several tests assert on that exact comment string.

  **You must not change the text of that comment, and `0003` must not issue its own
  `COMMENT ON SCHEMA pgmq`.** The test helper `hasCanaryComment` in
  `pgmq-migration/test/Main.hs` compares the schema comment for exact string equality
  against the `0002` text. If `0003` overwrites the comment, four existing tests break.

### What actually changed upstream between 1.11.0 and 1.12.0

Upstream made roughly forty commits in this window, but nearly all of them are changes to
upstream's *Rust* client library (`pgmq-rs`), which has no bearing on a Haskell client. At
the SQL level — the only level `pgmq-hs` consumes — the entire delta is contained in two
upgrade scripts.

`vendor/pgmq/pgmq-extension/sql/pgmq--1.11.0--1.11.1.sql` contains, in order:

1. A `DO` block calling `pg_catalog.pg_extension_config_dump('pgmq.topic_bindings', '')`,
   guarded by `IF EXISTS(SELECT 1 FROM pg_extension WHERE extname = 'pgmq')`. This makes
   `pg_dump` include the contents of the `topic_bindings` table when pgmq is installed as
   an extension. In a native (non-extension) install the guard is false, so the block does
   nothing. It is safe and is kept verbatim.
2. `CREATE OR REPLACE FUNCTION pgmq.read_grouped_head(queue_name TEXT, vt INTEGER, qty INTEGER)
   RETURNS SETOF pgmq.message_record` — **the first new function.**
3. `CREATE OR REPLACE FUNCTION pgmq._ensure_pg_partman_installed()` — a re-assertion of an
   internal helper whose body is *identical* to the one already in `0001`. Verified by
   diffing the function body at upstream `v1.11.0` against upstream `main`: no change. It
   is a harmless no-op re-definition (upstream ships it to normalise databases upgraded
   from much older versions).
4. `DROP FUNCTION IF EXISTS pgmq.enable_notify_insert(queue_name text);` — a no-op here,
   see the Decision Log entry that explains why.

`vendor/pgmq/pgmq-extension/sql/pgmq--1.11.1--1.12.0.sql` contains exactly one statement:

5. `CREATE OR REPLACE FUNCTION pgmq.read_grouped_head_with_poll(queue_name TEXT, vt INTEGER,
   qty INTEGER, max_poll_seconds INTEGER DEFAULT 5, poll_interval_ms INTEGER DEFAULT 100)
   RETURNS SETOF pgmq.message_record` — **the second new function.** Its body loops,
   calling `pgmq.read_grouped_head` and sleeping `poll_interval_ms` between attempts, until
   it finds messages or `max_poll_seconds` elapses.

The fresh-install script `pgmq.sql` also gained both functions, plus one cosmetic change:
the `GRANT ... TO pg_monitor` statements were moved earlier in the file (to just after the
`pgmq.meta` table is created) so that a fresh install's `pg_dump` output matches an
upgraded database's. There is deliberately **no** corresponding statement in the upgrade
scripts, because an already-installed database already has those grants. No action is
needed for it, and none should be invented.

Note that both new functions return `SETOF pgmq.message_record`, the same row type every
other pgmq read function returns. Nothing about the message row shape changes in 1.12.0.

### Terms used in the tests you will edit

- **Evidence key** — a label under which `pg-migrate` records that some external condition
  was verified (for example, "this database really is at pgmq 1.11").
- **History import** — the process of taking a database that was previously migrated by the
  *old* `hasql-migration` tooling (whose ledger lives in `public.schema_migrations`) and
  telling the *new* `pg-migrate` ledger "treat migration X as already applied, do not run
  it". This is what lets an existing production database adopt `pgmq-migration` without
  re-running a 63 KB install script over a schema that already exists.
- **Canary** — the `0002` schema comment described above.


## Plan of Work

### Milestone 1 — Advance the vendored upstream SQL to 1.12.0

**Scope.** Move the `vendor/pgmq` subtree from its current position (upstream `v1.11.0`) to
upstream commit `08ace4087dbf00e51704c5a3d9df2e15fd566127`. After this milestone the two
new upgrade scripts exist on disk under `vendor/pgmq/pgmq-extension/sql/`, and the vendored
`pgmq.control` reads `default_version = '1.12.0'`. No Haskell or migration file changes yet
— the build and the test suite are expected to still pass at this point except for the one
byte-identity test, which Milestone 3 re-points. Do not be alarmed when it fails; that
failure is the designed signal that the baseline and the vendored fresh-install script have
legitimately diverged.

**Why a commit and not a tag.** See the Decision Log. Upstream's newest tag is `v1.11.1`;
`v1.12.0` is not tagged yet, and `CLAUDE.md`'s tag-based command cannot run as written.

**Verification.** `CLAUDE.md` requires checking that new upgrade SQL contains no
extension-only constructs, because a native install has no extension to alter. The patterns
to search for are `ALTER EXTENSION` (adds objects to an extension's membership) and
`@extschema@` (a placeholder the extension build system substitutes; meaningless in plain
SQL). Neither should appear. `pg_extension_config_dump` *does* appear, but only inside a
`DO` block guarded on the extension existing, so it is inert natively — expect to see it and
accept it.

**Acceptance.** `ls vendor/pgmq/pgmq-extension/sql/ | grep 1.12` prints
`pgmq--1.11.1--1.12.0.sql`; `grep default_version vendor/pgmq/pgmq-extension/pgmq.control`
prints `1.12.0`; the grep for `ALTER EXTENSION` and `@extschema@` across the two new scripts
returns nothing.

### Milestone 2 — Add the upgrade migration and wire it into the build

> **Read this before you create any file: your migration number may not be `0003`.**
>
> This plan writes the new migration as `0003-upgrade-v1.12.0.sql` throughout, because `0003`
> was the next free number when the plan was authored. **That is an illustration, not a
> reservation.** The manifest is the only authority. Run:
>
> ```bash
> cat pgmq-migration/migrations/manifest
> ```
>
> and claim the next free sequential number after every entry actually present, leaving no
> gap. As of 2026-08-05 the decision is to implement
> `docs/masterplans/3-harden-the-pgmq-hs-family-surfaced-by-the-2026-07-review.md` **first**,
> and its plan
> `docs/plans/14-make-insert-notifications-survive-crashes-and-document-the-channel-contract.md`
> also appends a migration — so if that has landed, the manifest already holds three entries
> and yours is `0004-upgrade-v1.12.0.sql`. Wherever this plan says `0003`, substitute the
> number you actually claimed; the slug `upgrade-v1.12.0` does not change. Two consequences
> follow, both handled where they arise below: Milestone 3's counts go from *N* to *N+1*
> rather than specifically from two to three, and Milestone 4's convergence allowlist starts
> populated rather than empty.
>
> Ordering is safe in either direction. The two vendored upgrade scripts contain only four
> top-level statements between them — `CREATE OR REPLACE` of `pgmq.read_grouped_head`,
> `pgmq._ensure_pg_partman_installed`, and `pgmq.read_grouped_head_with_poll`, plus
> `DROP FUNCTION IF EXISTS pgmq.enable_notify_insert(queue_name text)`. None of them touches
> the three functions plan 14 hardens (`pgmq.notify_queue_listeners`,
> `pgmq.enable_notify_insert(text,integer)`, `pgmq.create_partitioned`), and that `DROP`
> targets a one-argument overload this repository never had. So this migration applied after
> plan 14's cannot clobber its work. Verified 2026-08-05.

**Scope.** Create the new migration file as the byte-exact
concatenation of the two new vendored upgrade scripts, add it to the manifest, and add the
two vendored scripts to the package's `extra-source-files` so they ship in the source
tarball (the test suite reads them from disk to verify provenance). At the end of this
milestone the library compiles and a migrated database has the two new functions, though no
test asserts it yet.

**The files to change.**

`pgmq-migration/migrations/0003-upgrade-v1.12.0.sql` — new file, generated, never
hand-edited. Generate it with the `cat` command in Concrete Steps rather than typing SQL.

`pgmq-migration/migrations/manifest` — append one line. If nothing else has landed since this
plan was authored, the file ends up as exactly:

```text
0001-install-v1.11.0.sql
0002-schema-management-comment.sql
0003-upgrade-v1.12.0.sql
```

If plan 14's migration landed first, its filename sits at line 3 and yours becomes line 4.
Either way you **append**; you never insert.

Order matters: it is application order. Your new entry goes last. Never reorder existing lines —
`HasqlMigration.hs` takes `head` of the embedded entries to find the `0001` payload whose
MD5 it must match, so moving `0001` off the front silently breaks history import for every
existing user.

`pgmq-migration/pgmq-migration.cabal` — the `extra-source-files` stanza currently reads:

```text
extra-source-files:
  migrations/*.sql
  migrations/manifest
  vendor/pgmq/pgmq-extension/sql/pgmq--1.10.0--1.10.1.sql
  vendor/pgmq/pgmq-extension/sql/pgmq--1.10.1--1.11.0.sql
  vendor/pgmq/pgmq-extension/sql/pgmq.sql
```

The `migrations/*.sql` glob already picks up `0003`, so no change is needed for the
migration itself. But add the two new vendored upgrade scripts, because Milestone 3's
provenance test reads them:

```text
  vendor/pgmq/pgmq-extension/sql/pgmq--1.11.0--1.11.1.sql
  vendor/pgmq/pgmq-extension/sql/pgmq--1.11.1--1.12.0.sql
```

**Acceptance.** `cabal build pgmq-migration` succeeds. If you forgot the manifest line you
will get a compile error naming `UnlistedSqlFiles` — that is the strict-membership check
doing its job.

### Milestone 3 — Update the existing tests for a three-migration component

**Scope.** `pgmq-migration/test/Main.hs` hard-codes the assumption that the component has
exactly two migrations, in several places. This milestone updates every one of them and
re-points the byte-provenance test. At the end, `cabal test
pgmq-migration:pgmq-migration-test` passes again.

The test suite runs against a throwaway PostgreSQL server provided by the `ephemeral-pg`
library — no database setup is required of you, and no external database is touched. All six
existing tests share one server and isolate themselves by dropping the `pgmq` schema, the
legacy `public.schema_migrations` table, and the `pgmigrate` ledger schema between tests
(see the `resetDb` and `withCleanDb` helpers).

> **Correction (2026-08-05, after plan 14 landed).** Items 2, 3 and 4 below describe
> per-migration edits that no longer exist. Plan 14 appended
> `0003-notify-crash-safety-and-locking.sql` and, rather than bump every hard-coded pair to
> a triple, rewrote the expectations to derive from the plan: `pgmq-migration/test/Main.hs`
> now has `nativeMigrationNames`, `migrationNames`, and `pendingAfterBaseline`, and
> `testNativeRunner`, `testDirectHistoryImport`, `testEquivalentHistoryImport`, and
> `assertNativeCanaryLifecycle` all use them. **Your migration therefore needs exactly one
> expectation edit**: add its name to the list in `testNativeComponent` (item 2), in
> manifest order — the counts and pending lists in items 3 and 4 update themselves. The
> *other* content of item 3 still applies: add the two `functionExists` post-conditions for
> the new 1.12.0 functions, since nothing else proves 1.12.0 landed. Item 1 is unaffected.
> Note also that the ledger is now `0001`, `0002`, `0003`, so your file is
> `0004-upgrade-v1.12.0.sql` unless something else landed in between — read the manifest.

**The five things to change.**

1. **The byte-provenance test, `testNativePayload`.** It currently reads:

   ```haskell
   testNativePayload = do
     nativePath <- findFile ["pgmq-migration/migrations/0001-install-v1.11.0.sql", "migrations/0001-install-v1.11.0.sql"]
     vendorPath <- findFile ["vendor/pgmq/pgmq-extension/sql/pgmq.sql", "../vendor/pgmq/pgmq-extension/sql/pgmq.sql"]
     native <- ByteString.readFile nativePath
     vendored <- ByteString.readFile vendorPath
     native @?= vendored
   ```

   That assertion was true only while the vendored fresh-install script was still at 1.11.0.
   It is now false by design and must be replaced by two assertions that together preserve
   the same guarantee — "no migration in this package contains hand-written SQL":

   - `0001` still has the exact bytes it has always had. Since the file it used to be
     compared against has moved on, pin it by content hash instead: assert the MD5 of
     `0001-install-v1.11.0.sql` equals `faa9b8800005f80fbdf6a33d071183d2`. This is what
     makes the immutability rule mechanically enforced rather than merely documented, and
     it is the same hash the `hasql-migration` history-import route depends on.
   - `0003` is exactly the two vendored upgrade scripts concatenated. Read
     `vendor/pgmq/pgmq-extension/sql/pgmq--1.11.0--1.11.1.sql` and
     `vendor/pgmq/pgmq-extension/sql/pgmq--1.11.1--1.12.0.sql`, join them the same way the
     generation command in Concrete Steps does, and compare to the bytes of
     `0003-upgrade-v1.12.0.sql`.

   Reuse the existing `findFile` helper, which tries a repository-root-relative path and
   then a package-relative one so the suite works whether `cabal` runs it from the repo root
   or the package directory. Pass both candidate paths for every new file you read.

   **Compute the MD5 in PostgreSQL, not in Haskell.** No hashing library is currently a
   dependency of this test suite, and none needs to be: the suite already holds a live
   PostgreSQL connection (the `conn` threaded through every test from `main`), and PostgreSQL
   has a built-in `md5()`. So read the file's bytes and run a one-parameter statement
   `SELECT md5($1)` against the connection, comparing the returned text to
   `faa9b8800005f80fbdf6a33d071183d2`. This adds no dependency and no new concept — it reuses
   the same `preparable` / `Session.statement` machinery the file's other helpers
   (`functionExists`, `hasCanaryComment`) already use, and it computes the digest the same way
   the legacy `hasql-migration` ledger did.

2. **`testNativeComponent`.** It asserts `length migrations @?= 2`. Change to `3`. The test
   name string `"component pgmq has two migrations and no dependencies"` must change too —
   leaving a stale name is exactly the kind of thing that misleads the next reader.

3. **`testNativeRunner`.** It runs the plan twice and asserts the per-migration outcome
   lists `[AppliedNow, AppliedNow]` then `[AlreadyApplied, AlreadyApplied]`. Both grow by one
   element for the migration you add — to three elements against the two-entry manifest this
   plan was authored on, or four if plan 14's migration landed first. Read the manifest and
   count rather than hard-coding the number from this plan. While you are here, extend its
   post-conditions: it already asserts
   `functionExists c "pgmq.metrics_all()" >>= (@?= True)`. Add the two new functions, which
   is the first place anything actually proves 1.12.0 landed:

   ```haskell
   functionExists c "pgmq.read_grouped_head(text,integer,integer)" >>= (@?= True)
   functionExists c "pgmq.read_grouped_head_with_poll(text,integer,integer,integer,integer)" >>= (@?= True)
   ```

   The existing `functionExists` helper runs `SELECT pg_catalog.to_regprocedure($1) IS NOT NULL`.

4. **`testDirectHistoryImport` and `testEquivalentHistoryImport`.** Both perform the same
   "canary flow" after importing history: they verify the plan, expect the un-applied
   migrations to be reported as pending, run the plan, and expect those to apply. Every
   migration after `0001` is pending in this flow, so the pending list grows by one for each
   migration added since this plan was authored. The `issues` assertion currently expects
   `[PendingMigration canaryId]` where `canaryId` is `migrationId "pgmq"
   "0002-schema-management-comment"`; it must now expect `0002`, your new
   `upgrade-v1.12.0` migration id, and plan 14's if that landed first — in manifest order.
   The outcome assertion `[AlreadyApplied, AppliedNow]` gains one `AppliedNow` per added
   migration, and the repeated-run assertion `[AlreadyApplied, AlreadyApplied]` grows to
   match. Derive the expected lists from the manifest you actually have.

   Do not change the ledger filenames or checksums these tests insert (`'pgmq_v1.11.0'` with
   checksum `'+qm4gAAF+A+99qM9BxGD0g=='`, and the two-step pair). Those describe a *legacy*
   database at pgmq 1.11.0, which is precisely the state the import route is designed to
   accept. They are inputs, not expectations.

   Note the deliberate tripwire in `testDirectHistoryImport`: after setting up the historical
   schema it runs `DROP FUNCTION pgmq.metrics_all()`, then asserts after import that
   `functionExists "pgmq.metrics_all()"` is still `False`. That proves the 63 KB baseline was
   *not* replayed — the whole point of history import. Leave it alone; it still holds,
   because `0003` does not define `metrics_all`.

5. **`testEquivalentContractRejections`** and `SchemaContract.hs` need **no change**. See the
   Decision Log: the 1.11 contract describes the predecessor state and must not absorb 1.12
   objects.

**Acceptance.** `cabal test pgmq-migration:pgmq-migration-test` passes, with the runner test
now proving both new functions exist.

### Milestone 4 — Prove the upgrade path converges with a fresh 1.12.0 install

**Scope.** Add one new test. This is the highest-value test in the plan and it is the reason
this milestone is separate: everything up to here proves "we ran some SQL"; this proves "the
SQL we ran produces the *right* schema".

**The risk it addresses.** There are now two ways a database can arrive at pgmq 1.12.0:
the *upgrade path* (`0001` at the 1.11.0 baseline, then `0003`) and a *fresh install* (run
upstream's `pgmq.sql`, which is now at 1.12.0). Nothing so far guarantees these agree. If
upstream ever forgets to mirror a fresh-install change into an upgrade script — or if our
concatenation misses a statement — a `pgmq-migration`-managed database would silently differ
from a stock pgmq database, and the difference would surface as a baffling runtime error in a
user's application, not as a test failure. Upstream added a CI check for exactly this class
of bug (their commit `885251c`, "Check that upgrade scripts do not diverge from a fresh
install"). This test is the pgmq-hs equivalent.

**How to build it.** The two schemas cannot coexist — upstream's `pgmq.sql` hard-codes the
schema name `pgmq` — so compare them sequentially in one database:

1. On a clean database (use the existing `withCleanDb` helper), run the full migration plan
   (`0001`, `0002`, `0003`).
2. Introspect the resulting `pgmq` schema into a sorted, deterministic snapshot value.
3. `DROP SCHEMA pgmq CASCADE` and also drop the `pgmigrate` ledger schema, returning to a
   clean slate.
4. Execute the vendored `vendor/pgmq/pgmq-extension/sql/pgmq.sql` verbatim as a fresh
   install, using the existing `runSql` helper (which uses `Session.script`, so it handles a
   multi-statement file).
5. Introspect again, and assert the two snapshots are equal.

**What the snapshot must contain.** Compare the things that would actually break a client:

- Every function in the `pgmq` schema, identified by its `regprocedure` signature (name plus
  argument types) **and** its body (`pg_proc.prosrc`). Including the body is what catches a
  function that exists in both paths but was updated in only one of them.
- Every table in the `pgmq` schema, with each column's name, formatted type, and not-null
  flag.
- Every constraint on those tables, with its name and type.
- Every composite type in the schema, with its attribute names and types in order.

Sort every list in SQL (`ORDER BY`) so the comparison is stable and a failure message points
at the offending object rather than at an arbitrary reordering.

**Expect the canary to differ, and exclude it.** The upgrade path runs `0002`, which sets a
`COMMENT ON SCHEMA pgmq`. A fresh install of upstream's `pgmq.sql` sets no such comment. Do
not include the schema comment in the snapshot, or the test will fail for a reason that is
not a defect. (Do not "fix" this by removing `0002` — the canary is a deliberate feature.)

**Give the test a deliberate-deviation allowlist.** Define, next to the test, a named list of
`pgmq` function signatures that this repository knowingly redefines away from upstream. For any
function on that list, compare the signature only; for every function not on it, compare
signature *and* body. Document the list's contract in a comment above it: *an entry means
"pgmq-hs deliberately owns this function's body; a plan recorded why".*

**What the list contains depends on what has landed, so check.**
`docs/plans/14-make-insert-notifications-survive-crashes-and-document-the-channel-contract.md`,
under `docs/masterplans/3-harden-the-pgmq-hs-family-surfaced-by-the-2026-07-review.md`, ships a
migration whose whole purpose is to redefine three functions — `pgmq.notify_queue_listeners`,
`pgmq.enable_notify_insert`, and `pgmq.create_partitioned` — with crash-fallback,
advisory-locking, and idempotence behaviour upstream does not have. **As of 2026-08-05 that
MasterPlan is being implemented first**, so expect the migration to be present already:

```bash
grep -l 'notify_queue_listeners' pgmq-migration/migrations/*.sql
```

- **If plan 14's migration is present**, seed the list with those three signatures when you
  write the test, each with a comment naming the decision in plan 14 that authorised it.
  Without the entries your new test fails the moment you add it, on behaviour that is correct.
- **If it is not present**, the list starts empty and every body is compared; plan 14 adds its
  three entries when it lands.

Either way the mechanism is the same and it is not optional. The tempting shortcut when the
test goes red — deleting the body comparison — would silently discard the guarantee for the
other ~55 functions, which is the only reason this test exists.

**If the test fails on function bodies.** First check whether the function is one this
repository deliberately owns (the allowlist above) — if a hardening plan has landed a
redefinition and not allowlisted it, that is the bug, and the fix is the allowlist entry plus
its justifying comment, not a weaker assertion. Otherwise, check whether the divergence is
upstream's. Two facts are already established and should reassure you: the body of
`pgmq._ensure_pg_partman_installed` is byte-identical between upstream `v1.11.0` and upstream
`main` (verified by diffing them), so its `CREATE OR REPLACE` in the upgrade script is a
genuine no-op; and the bodies of `read_grouped_head` and `read_grouped_head_with_poll` are
character-for-character the same in the upgrade scripts as in `pgmq.sql`, including an
idiosyncratic stray tab before `WHERE q.vt <= clock_timestamp()`. So the bodies are expected to
match. If they nonetheless do not, that is a real upstream divergence: record it in Surprises &
Discoveries with the exact diff, narrow the snapshot to signatures only so the suite still
guards object identity, and open an upstream issue. Do not paper over it by weakening the test
silently.

**Acceptance.** The new test passes. Then prove it is not vacuous: temporarily remove the
`read_grouped_head` definition from `0003-upgrade-v1.12.0.sql`, re-run the suite, and confirm
the convergence test fails naming the missing function. Restore the file afterwards (and
regenerate it with the `cat` command rather than editing by hand).

### Milestone 5 — Update the documentation

**Scope.** Three documents describe `pgmq-migration` as a two-migration component and must
be corrected. No code changes.

`docs/user/schema-migration.md` is the user-facing guide. Update: the "What the component
contains" table (add a `0003-upgrade-v1.12.0` row describing it as the upstream 1.11.0 →
1.12.0 upgrade, applied as one step, and stating that `0001` remains byte-identical to
upstream's `pgmq.sql` *as of v1.11.0*); the sentence stating the component has two
migrations; and the expected post-import output block, which currently shows `0001
AlreadyApplied` / `0002 AppliedNow` and must gain a `0003 AppliedNow` line. Also state
plainly that a database managed by this component is now at pgmq 1.12.0 and has
`read_grouped_head` and `read_grouped_head_with_poll`.

`CLAUDE.md`'s `pgmq-migration` section instructs `git subtree pull ... <new-tag>`. Add a
sentence recording that upstream had not tagged v1.12.0 when this work was done and the
subtree is therefore pinned to commit `08ace4087dbf00e51704c5a3d9df2e15fd566127`, and that a
re-pull at the eventual tag should be a byte-level no-op.

`docs/design/012-vendor-upstream-pgmq-sql.md` is the architecture decision record for
vendoring. Its worked example (lines ~245-260) literally uses `v1.12.0` as its hypothetical
next version, and its step 4 still describes a module-per-version design
(`Pgmq.Migration.Migrations`, `V1_11_0.hs`) that no longer exists. Correct the stale design
description and update the worked example to reflect what was actually done, including the
commit pin.

**Acceptance.** `nix fmt` runs clean (the project uses treefmt with a pre-commit hook; if you
commit without formatting, the hook rewrites files and the commit fails, and you must stage
and re-commit).


## Concrete Steps

All commands run from the repository root,
`/Users/shinzui/Keikaku/bokuno/libraries/pgmq-hs-project/pgmq-hs`, inside `nix develop`.

### Milestone 1

Advance the subtree. The `--squash` flag collapses upstream's history into a single commit,
which is what the existing subtree used:

```bash
git subtree pull --prefix vendor/pgmq \
  https://github.com/tembo-io/pgmq.git \
  08ace4087dbf00e51704c5a3d9df2e15fd566127 --squash
```

Confirm the new files arrived and the version moved:

```bash
ls vendor/pgmq/pgmq-extension/sql/ | grep -E '1\.11\.0--1\.11\.1|1\.11\.1--1\.12\.0'
grep default_version vendor/pgmq/pgmq-extension/pgmq.control
```

Expected output:

```text
pgmq--1.11.0--1.11.1.sql
pgmq--1.11.1--1.12.0.sql
default_version = '1.12.0'
```

Check for extension-only SQL patterns, as `CLAUDE.md` requires:

```bash
grep -nE 'ALTER EXTENSION|@extschema@' \
  vendor/pgmq/pgmq-extension/sql/pgmq--1.11.0--1.11.1.sql \
  vendor/pgmq/pgmq-extension/sql/pgmq--1.11.1--1.12.0.sql
```

Expected output: nothing, and a non-zero exit status from `grep`. If either pattern appears,
stop — a native install cannot execute it, and the migration would need rework. Record the
finding in Surprises & Discoveries before proceeding.

Confirm the only new SQL objects are the two expected functions:

```bash
grep -nE '^(CREATE|DROP)' \
  vendor/pgmq/pgmq-extension/sql/pgmq--1.11.0--1.11.1.sql \
  vendor/pgmq/pgmq-extension/sql/pgmq--1.11.1--1.12.0.sql
```

Expected output:

```text
vendor/pgmq/pgmq-extension/sql/pgmq--1.11.0--1.11.1.sql:14:CREATE OR REPLACE FUNCTION pgmq.read_grouped_head(
vendor/pgmq/pgmq-extension/sql/pgmq--1.11.0--1.11.1.sql:55:CREATE OR REPLACE FUNCTION pgmq._ensure_pg_partman_installed()
vendor/pgmq/pgmq-extension/sql/pgmq--1.11.0--1.11.1.sql:65:DROP FUNCTION IF EXISTS pgmq.enable_notify_insert(queue_name text);
vendor/pgmq/pgmq-extension/sql/pgmq--1.11.1--1.12.0.sql:3:CREATE OR REPLACE FUNCTION pgmq.read_grouped_head_with_poll(
```

(Line numbers may shift; the set of objects is what matters.)

Commit this on its own so the vendored bytes are isolated from our changes:

```bash
git add vendor/pgmq
git commit -m "$(cat <<'EOF'
chore(vendor): advance pgmq subtree to upstream 1.12.0

Pin to upstream commit 08ace4087dbf00e51704c5a3d9df2e15fd566127
("prepare extension v1.12.0"). Upstream has not tagged v1.12.0 yet;
its newest tag is v1.11.1.

Brings in pgmq--1.11.0--1.11.1.sql and pgmq--1.11.1--1.12.0.sql, which
add pgmq.read_grouped_head and pgmq.read_grouped_head_with_poll.

MasterPlan: docs/masterplans/2-support-pgmq-1-12-0-grouped-head-reads.md
ExecPlan: docs/plans/9-vendor-pgmq-1-12-0-and-add-the-native-schema-migration.md
Intention: intention_01kxgh9geke2dayhx57qp6g9ye
EOF
)"
```

Note: `git subtree pull` creates its own merge commit, so the working tree may already be
clean. If `git status` shows nothing to commit, that is fine — the subtree commit is the
record.

### Milestone 2

First fix your migration number from the live manifest, and use it for every command below —
`0003` here assumes nothing landed since this plan was authored:

```bash
cat pgmq-migration/migrations/manifest      # what is actually there?
MIG=0003-upgrade-v1.12.0.sql                # or 0004-... if plan 14's migration is present
```

Generate the migration by concatenation. **Do not type SQL by hand.** The `printf '\n'`
between the two files guarantees the second script starts on its own line even if the first
does not end with a newline:

```bash
{
  cat vendor/pgmq/pgmq-extension/sql/pgmq--1.11.0--1.11.1.sql
  printf '\n'
  cat vendor/pgmq/pgmq-extension/sql/pgmq--1.11.1--1.12.0.sql
} > "pgmq-migration/migrations/$MIG"
```

Add the manifest line:

```bash
printf '%s\n' "$MIG" >> pgmq-migration/migrations/manifest
cat pgmq-migration/migrations/manifest
```

Expected output, if this plan landed first:

```text
0001-install-v1.11.0.sql
0002-schema-management-comment.sql
0003-upgrade-v1.12.0.sql
```

If plan 14's migration is already present, your entry is the fourth line and follows it.

Edit `pgmq-migration/pgmq-migration.cabal` to add the two vendored upgrade scripts to
`extra-source-files`, as described in the Plan of Work.

Build:

```bash
cabal build pgmq-migration
```

Expected: a successful build. A failure mentioning `UnlistedSqlFiles` means the manifest line
is missing or misspelled.

### Milestone 3

Edit `pgmq-migration/test/Main.hs` per the Plan of Work. No `.cabal` dependency changes are
needed — the MD5 pin is computed by PostgreSQL over the existing connection.

Run:

```bash
cabal test pgmq-migration:pgmq-migration-test
```

Expected: all tests pass. The suite starts its own temporary PostgreSQL server; if you see a
connection error, confirm you are inside `nix develop` so the PostgreSQL binaries are on
`PATH`.

To see the new functions with your own eyes rather than trusting the test, the quickest route
is to add a temporary `traceShowM`-style assertion — but better, just trust `functionExists`,
which is a direct `to_regprocedure` lookup, and confirm the test fails if you misspell the
signature.

### Milestone 4

Add the convergence test to `pgmq-migration/test/Main.hs` and register it in the test tree.
Run the suite again:

```bash
cabal test pgmq-migration:pgmq-migration-test
```

Then prove the test is not vacuous:

```bash
# Temporarily break the migration ($MIG is the filename you claimed in Milestone 2).
grep -v 'read_grouped_head' "pgmq-migration/migrations/$MIG" > /tmp/broken.sql
cp /tmp/broken.sql "pgmq-migration/migrations/$MIG"
cabal test pgmq-migration:pgmq-migration-test   # expect FAILURE naming the missing function
```

Restore by regenerating from the vendored source (never by hand-editing):

```bash
{
  cat vendor/pgmq/pgmq-extension/sql/pgmq--1.11.0--1.11.1.sql
  printf '\n'
  cat vendor/pgmq/pgmq-extension/sql/pgmq--1.11.1--1.12.0.sql
} > "pgmq-migration/migrations/$MIG"
cabal test pgmq-migration:pgmq-migration-test   # expect PASS
```

Record the observed failure message in Surprises & Discoveries as evidence the test bites.

### Milestone 5

Edit the three documents. Then format and commit:

```bash
nix fmt
cabal build all && cabal test pgmq-migration:pgmq-migration-test
git add -A
git commit -m "$(cat <<'EOF'
feat(pgmq-migration): add native 1.12.0 upgrade migration

Append 0003-upgrade-v1.12.0.sql, generated as the byte-exact
concatenation of the vendored pgmq--1.11.0--1.11.1.sql and
pgmq--1.11.1--1.12.0.sql upgrade scripts. Adds pgmq.read_grouped_head
and pgmq.read_grouped_head_with_poll to a natively-migrated database.

The 0001 baseline stays immutable and is now pinned by MD5, since the
vendored pgmq.sql it was compared against has advanced to 1.12.0. A new
test proves the 0001+0003 upgrade path converges to the same schema as a
fresh install of the vendored pgmq.sql.

The 1.11 schema contract is deliberately unchanged: it validates the
predecessor state of a legacy database before history import, and a
genuine 1.11 database does not have the 1.12 functions.

MasterPlan: docs/masterplans/2-support-pgmq-1-12-0-grouped-head-reads.md
ExecPlan: docs/plans/9-vendor-pgmq-1-12-0-and-add-the-native-schema-migration.md
Intention: intention_01kxgh9geke2dayhx57qp6g9ye
EOF
)"
```


## Validation and Acceptance

The plan is complete when all of the following hold.

**The test suite passes.** From the repository root inside `nix develop`:

```bash
cabal test pgmq-migration:pgmq-migration-test
```

Every test passes, including the new convergence test, and including the two new
`functionExists` assertions inside `testNativeRunner`.

**The rest of the project still builds.** `pgmq-hasql` and `pgmq-effectful` bootstrap their
test databases through this package, so a mistake here breaks them:

```bash
cabal build all
cabal test all
```

All suites pass. They do not yet exercise the new functions — that is the next plan's job —
but they must not regress.

**The functions are really there.** The most direct proof is behavioural rather than
structural. After `cabal test pgmq-migration:pgmq-migration-test` passes, the runner test has
asserted, via `SELECT pg_catalog.to_regprocedure(...) IS NOT NULL`, that both
`pgmq.read_grouped_head(text,integer,integer)` and
`pgmq.read_grouped_head_with_poll(text,integer,integer,integer,integer)` exist in a database
built solely from this package's migrations. If you want to watch it work end to end against
a real queue, the fastest route is to wait for
`docs/plans/10-add-grouped-head-read-statements-and-sessions-to-pgmq-hasql.md`, whose tests
send messages tagged with `x-pgmq-group` headers and read their heads back. Do not add a
Haskell caller in this plan; keeping the SQL and client layers in separate plans is what
makes each independently verifiable.

**Provenance is enforced, not assumed.** The suite now fails if `0001`'s bytes change (MD5
pin) or if `0003` stops being the exact concatenation of the vendored upgrade scripts. Both
are guarantees `CLAUDE.md` states in prose; after this plan they are mechanical.

**Nothing about the 1.11 history-import contract moved.** `git diff` shows no changes to
`pgmq-migration/src/Pgmq/Migration/SchemaContract.hs` or
`pgmq-migration/src/Pgmq/Migration/History/HasqlMigration.hs`. If you find yourself editing
either, re-read the Decision Log — you are probably about to break existing users' imports.


## Idempotence and Recovery

**Re-running migrations is safe.** `pg-migrate` records applied migrations in the `pgmigrate`
ledger schema; a second run of the plan reports every migration as `AlreadyApplied` and
executes nothing. `testNativeRunner` asserts exactly this. Beyond that, the SQL itself is
written defensively: `0003`'s statements are `CREATE OR REPLACE FUNCTION` and `DROP FUNCTION
IF EXISTS`, both of which are safe to execute more than once.

**Regenerating `0003` is safe and is the only correct way to edit it.** It is a derived file.
If it is ever wrong, do not patch it — re-run the `cat` command from Concrete Steps. The
provenance test will tell you immediately if the result is not what the vendored source says
it should be.

**Backing out the subtree pull.** `git subtree pull` produces ordinary commits. If the vendor
bump must be undone before anything depends on it, `git revert` the subtree merge commit. If
you have not committed yet, `git checkout -- vendor/pgmq` restores it.

**If `cabal build` reports stale embedded SQL.** The Template Haskell embedding depends on a
directory, which GHC 9.12 cannot track; the `RecompilePlugin` pragma in
`pgmq-migration/src/Pgmq/Migration/Internal/Definition.hs` exists to force re-checking. If
you nonetheless see the old two-migration behaviour after adding `0003`, force a rebuild of
that package:

```bash
cabal build pgmq-migration --ghc-options=-fforce-recomp
```

**No production database is at risk from this plan.** Every test spins up its own throwaway
PostgreSQL server via `ephemeral-pg`. Nothing here connects to an external database.

**The one genuinely destructive operation** is inside the new convergence test: it runs `DROP
SCHEMA pgmq CASCADE` between the two halves of the comparison. That is confined to the
ephemeral test database and must never be lifted out of the test suite into library code.


## Interfaces and Dependencies

**Libraries already in use, unchanged by this plan:** `pg-migrate` (^>=1.1.0.0) supplies
`MigrationComponent`, `migrationPlan`, `runMigrationPlan`, `MigrationId`, and the
`AppliedNow` / `AlreadyApplied` outcome type. `pg-migrate-embed` (^>=1.1.0.0) supplies
`embedMigrationManifest` and the strict manifest validation. `pg-migrate-import-hasql-migration`
(^>=1.1.0.0) supplies the legacy-ledger history import. `ephemeral-pg` (>=0.2.1) supplies the
throwaway PostgreSQL server used by the test suite. `hasql` and `hasql-transaction` supply the
database session and statement types. `tasty` / `tasty-hunit` supply the test framework.

**No new package dependencies.** The `0001` baseline is pinned by computing its MD5 with
PostgreSQL's built-in `md5()` over the connection the test suite already holds, rather than by
adding a Haskell hashing library. If you find yourself editing `build-depends` in this plan,
stop and re-read Milestone 3 — you have probably reached for `cryptohash-md5`, which is not
needed.

**Artifacts that must exist when this plan is complete:**

- `pgmq-migration/migrations/0003-upgrade-v1.12.0.sql`, byte-equal to the vendored
  `pgmq--1.11.0--1.11.1.sql` and `pgmq--1.11.1--1.12.0.sql` joined by a newline.
- `pgmq-migration/migrations/manifest` listing exactly three files, `0001` first.
- `Pgmq.Migration.pgmqMigrations :: Either DefinitionError MigrationComponent` — its type is
  **unchanged**; the component it yields simply now contains three migrations rather than
  two. This is the only interface the downstream packages consume, and it is deliberately
  stable, which is why plans 10 and 11 need no coordination with this one beyond ordering.

**Two SQL functions that must exist in a migrated database when this plan is complete** —
these are the contract that `docs/plans/10-add-grouped-head-read-statements-and-sessions-to-pgmq-hasql.md`
builds on, and it will fail without them:

```sql
pgmq.read_grouped_head(queue_name text, vt integer, qty integer)
  RETURNS SETOF pgmq.message_record

pgmq.read_grouped_head_with_poll(queue_name text, vt integer, qty integer,
                                 max_poll_seconds integer DEFAULT 5,
                                 poll_interval_ms integer DEFAULT 100)
  RETURNS SETOF pgmq.message_record
```

**Interfaces that must NOT change:** `Pgmq.Migration.SchemaContract.pgmqV1_11StateValidator`
and `pgmqV1_11StateEvidenceKey`; the migration id `pgmq / 0001-install-v1.11.0`; the bytes
and MD5 of `0001-install-v1.11.0.sql`; the canary comment text in
`0002-schema-management-comment.sql`; the position of `0001` as the first manifest entry.
Each of these is load-bearing for existing users' history import, and each is explained in
the Decision Log.


## Revision Note

2026-08-05: Cascaded from
`docs/plans/14-make-insert-notifications-survive-crashes-and-document-the-channel-contract.md`,
which landed first. Added a correction above Milestone 3's "five things to change":
`pgmq-migration/test/Main.hs` no longer enumerates the ledger positionally, so this plan's
migration needs one expectation edit (the name list in `testNativeComponent`) rather than
four. The ledger is now `0001`, `0002`, `0003`, and the convergence allowlist this plan
builds must be seeded with the three functions plan 14's migration deliberately diverges
from upstream — `pgmq.notify_queue_listeners()`,
`pgmq.enable_notify_insert(TEXT, INTEGER)`, and
`pgmq.create_partitioned(TEXT, TEXT, TEXT)` — all three of which are upstream-vendored
code, verified by diff against `vendor/pgmq/pgmq-extension/sql/pgmq.sql`.
