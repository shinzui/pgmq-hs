# Design: Vendor Upstream pgmq SQL for pgmq-migration

## Problem Statement

The pgmq-migration package maintains hand-written SQL files that are intended to replicate the upstream pgmq extension's schema. In practice, these files have diverged from upstream in ways that introduce bugs and missing functionality. The most critical example: the `metrics()` function uses `pg_stat_get_xact_tuples_inserted()` (transaction-scoped, always returns 0 in a standalone query) instead of the upstream's sequence-based approach, causing `total_messages` to always report 0.

A comprehensive diff reveals **14 significant divergences** beyond the metrics bug. Maintaining parity with hand-written SQL is unsustainable as pgmq evolves.

## Current Architecture

```
pgmq-migration/
  database/
    v1.9.0/          # Full schema snapshot (10 hand-written SQL files)
    v1.10.0/         # Full schema snapshot (10 hand-written SQL files)
    v1.11.0/         # Full schema snapshot (11 hand-written SQL files)
    migrations/
      v1.9.0_to_v1.10.0.sql   # Hand-written incremental upgrade
      v1.10.0_to_v1.11.0.sql  # Hand-written incremental upgrade (skips 1.10.1!)
  src/
    Pgmq/Migration/Migrations/
      V1_9_0.hs       # embedFile for each SQL file
      V1_10_0.hs
      V1_11_0.hs
      V1_9_0_to_V1_10_0.hs
      V1_10_0_to_V1_11_0.hs
```

Each version directory contains hand-written SQL files numbered `01_schema.sql` through `10_fifo.sql` (or `11_topics.sql`). These were originally derived from upstream `pgmq.sql` but manually split into logical units and adapted for non-extension deployment. The Haskell modules use `file-embed` to compile these SQL files into the binary at build time.

The upgrade path skips version 1.10.1 entirely — upstream requires 1.10.0→1.10.1→1.11.0 but pgmq-hs jumps directly from 1.10.0 to 1.11.0.

## Audit of Current Divergences from Upstream

### Critical Bugs

| Divergence | pgmq-hs | Upstream | Impact |
|------------|---------|----------|--------|
| `metrics()` total_messages | `pg_stat_get_xact_tuples_inserted()` — returns tuples inserted in the **current transaction** only | `last_value` / `is_called` from the identity sequence | **total_messages always 0** when queried outside the inserting transaction |

### Behavioral Differences

| Divergence | pgmq-hs | Upstream | Impact |
|------------|---------|----------|--------|
| `read()` signature | 2 separate overloads (3-arg and 4-arg) | Single function with `DEFAULT '{}'` and CASE guard | Different call semantics |
| `read_with_poll()` signature | 3 overloads with conflicting parameter orders | Single function with defaults | Parameter order confusion |
| `read_grouped()` query plan | `INNER JOIN` with subquery | `CROSS JOIN LATERAL` with per-group `LIMIT` | Performance: upstream avoids full table scan per group |
| Notification triggers | `AFTER INSERT` row trigger, queue name via `TG_ARGV` | `CONSTRAINT TRIGGER ... DEFERRABLE`, queue name from `TG_TABLE_NAME` | Upstream defers notification to transaction commit; pgmq-hs fires mid-transaction |
| Notification channel name | Bare queue name (e.g. `my_queue`) | Fully qualified (e.g. `pgmq.q_my_queue.INSERT`) | Client must know which convention to LISTEN on |
| `create_partitioned()` partition column | Hardcoded `msg_id` | Dynamic via `_get_partition_col()` — `msg_id` for integer intervals, `enqueued_at` for time intervals | Time-based partitioning broken in pgmq-hs |
| `_get_partition_col()` | Queries `pg_partman.part_config` at runtime | Stateless type check on the interval string | Different semantics, unnecessary DB round-trip |
| `convert_archive_partitioned()` | No idempotency checks | Checks if already partitioned, checks table existence | pgmq-hs fails on re-run |
| pg_partman version handling | Hardcoded `p_type := 'range'` | Version-aware: v5 uses `'range'`, older uses `'native'` | Breaks on pg_partman < 5 |

### Missing Functions

| Function | Purpose |
|----------|---------|
| `read_grouped_head()` | Read head message from N different FIFO groups in one call |
| `_get_pg_partman_major_version()` | Detect pg_partman version for compatible API calls |
| `_ensure_pg_partman_installed()` | Reusable partman prerequisite check |

### Extra Functions (pgmq-hs only)

| Function | Notes |
|----------|-------|
| `_get_pg_stat_get_xact_tuples_inserted()` | Incorrect metrics helper — should be removed |
| `_get_partman_config()` | Helper for pgmq-hs's divergent `_get_partition_col()` — unnecessary with upstream approach |

## Proposal: Vendor Upstream pgmq via Git Subtree

### Strategy

Add the upstream [tembo-io/pgmq](https://github.com/tembo-io/pgmq) repository as a git subtree at `vendor/pgmq/`. The pgmq-migration package then uses the vendored `pgmq.sql` as its single source of truth, eliminating hand-maintained SQL.

### Repository Layout

```
pgmq-hs/
  vendor/
    pgmq/                          # git subtree from tembo-io/pgmq
      pgmq-extension/
        sql/
          pgmq.sql                 # <-- single source of truth (fresh install)
          pgmq--1.10.0--1.10.1.sql # upgrade migration (vendored)
          pgmq--1.10.1--1.11.0.sql # upgrade migration (vendored)
          ...
        pgmq.control
      ...
  pgmq-migration/
    database/
      vendor -> ../../vendor/pgmq/pgmq-extension/sql  # symlink (for discoverability)
    src/
      Pgmq/Migration/
        Migrations.hs
        Migrations/
          V1_11_0.hs              # embedFile pgmq.sql
          V1_10_0_to_V1_10_1.hs   # embedFile pgmq--1.10.0--1.10.1.sql
          V1_10_1_to_V1_11_0.hs   # embedFile pgmq--1.10.1--1.11.0.sql
```

### Why Git Subtree Over Alternatives

| Approach | Pros | Cons |
|----------|------|------|
| **Git subtree** (proposed) | Full source in-tree; works with `file-embed`; diffable; no extra tooling for consumers; `git subtree pull` for updates | Adds upstream history to repo; merge conflicts on pull |
| Git submodule | Lightweight pointer; clean history | Requires `git submodule init` in CI and for consumers; `file-embed` needs the files at build time; Nix flake builds need special handling |
| Nix fetch + patch | Hermetic; no vendored files in repo | Complex build; opaque to developers; `file-embed` requires pre-build step; hard to review SQL diffs |
| Manual copy (status quo) | Simple | Divergence, bugs, maintenance burden — the problem we're solving |

### Splitting `pgmq.sql` for Migration Ordering

Upstream ships a single monolithic `pgmq.sql` (2,122 lines). The current pgmq-migration design splits this into numbered files for ordered execution. Two approaches:

#### Option A: Use `pgmq.sql` Directly (Recommended)

Embed the entire `pgmq.sql` as a single migration script. The file is already ordered correctly (types before functions, tables before references). PostgreSQL executes it as one transaction.

```haskell
-- V1_11_0.hs
module Pgmq.Migration.Migrations.V1_11_0 where

import Data.FileEmbed (embedFile)

version :: String
version = "v1.11.0"

migrations :: [MigrationCommand]
migrations =
  [ MigrationInitialization
  , MigrationScript "pgmq_v1.11.0" $(embedFile "vendor/pgmq/pgmq-extension/sql/pgmq.sql")
  ]
```

Benefits:
- Zero transformation of upstream SQL
- No risk of split-induced ordering bugs
- Simpler version module code
- Trivial to verify: vendored file == upstream file

Tradeoff:
- Loses granular migration tracking (one checksum for the whole schema)
- Harder to identify which part of the schema a migration failure occurred in

#### Option B: Automated Split with a Build Script

Write a script that splits `pgmq.sql` into numbered files by parsing section comments (upstream uses `-- section_name` markers). The split files go into a generated directory that `file-embed` reads from.

Benefits:
- Preserves granular migration names
- Easier to debug migration failures

Tradeoffs:
- Requires maintaining a splitter script
- Section markers in upstream could change without notice
- Adds a build step before `cabal build`

**Recommendation:** Option A. The granular tracking provided by numbered files has not prevented the divergence bugs we're fixing. A single file with a checksum is more reliable and far simpler to maintain.

### Non-Extension Adaptations

The upstream `pgmq.sql` is designed for `CREATE EXTENSION pgmq` deployment. For non-extension use (which is what pgmq-migration provides), a small set of adaptations is needed:

1. **Schema creation** — upstream conditionally creates the `pgmq` schema only if the extension isn't loaded. The non-extension path already works as-is in `pgmq.sql` (the `DO` block handles both cases).

2. **`pg_extension_config_dump` calls** — these are no-ops when not running as an extension. They can be left in place (they'll silently succeed or be skipped) or stripped via a thin preprocessing pass.

3. **`module_pathname` references** — upstream `pgmq.sql` is pure PL/pgSQL with no C functions, so there are no `$libdir` references to worry about.

After review, **upstream `pgmq.sql` works without modification for non-extension deployment**. The conditional schema creation block already handles this:

```sql
DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'pgmq') THEN
    CREATE SCHEMA IF NOT EXISTS pgmq;
  END IF;
END;
$$;
```

### Upgrade Migrations: Use Upstream Migration Files Directly

Upstream pgmq ships version-to-version migration files following the naming convention `pgmq--X.Y.Z--A.B.C.sql`. A key finding from auditing these files:

**The upstream migration files from 1.9.0 onward contain zero extension-specific patterns.** No `ALTER EXTENSION`, no `pg_extension_config_dump`, no `@extschema@`. They are pure SQL/PL/pgSQL scripts that work identically in both extension and non-extension deployments. (Older migrations pre-1.5.2 did contain `ALTER EXTENSION` statements, but those are outside our supported version range.)

This means we can **embed the upstream migration files directly** instead of hand-writing upgrade scripts — the same approach as the base `pgmq.sql`.

#### Supported Version Chain

We only need to support recent migrations. The upgrade path from the currently supported versions:

```
1.10.0 → 1.10.1 → 1.11.0
```

| File | Lines | Changes |
|------|-------|---------|
| `pgmq--1.10.0--1.10.1.sql` | 291 | Function signature refinements; `read_grouped` LATERAL join optimization |
| `pgmq--1.10.1--1.11.0.sql` | 791 | Topic routing, batch validation, notification throttle management |

Older migration paths (pre-1.10.0) are dropped. Users on versions older than 1.10.0 should do a fresh install rather than an incremental upgrade.

**The current pgmq-hs skips 1.10.0→1.10.1**, going directly from 1.10.0 to 1.11.0 with a hand-written script. This means existing pgmq-hs installations miss the `read_grouped()` LATERAL join performance fix and any function signature changes from 1.10.1.

#### New Upgrade Module Design

Each upgrade step embeds the corresponding upstream migration file:

```haskell
-- V1_10_0_to_V1_10_1.hs (NEW — currently missing)
migrations :: [MigrationCommand]
migrations =
  [ MigrationScript "pgmq_v1.10.0_to_v1.10.1"
      $(embedFile "vendor/pgmq/pgmq-extension/sql/pgmq--1.10.0--1.10.1.sql")
  ]

-- V1_10_1_to_V1_11_0.hs
migrations :: [MigrationCommand]
migrations =
  [ MigrationScript "pgmq_v1.10.1_to_v1.11.0"
      $(embedFile "vendor/pgmq/pgmq-extension/sql/pgmq--1.10.1--1.11.0.sql")
  ]
```

The aggregation module chains them:

```haskell
-- Migrations.hs
upgradeMigrations :: [MigrationCommand]
upgradeMigrations =
  [ MigrationInitialization ]
  ++ V1_10_0_to_V1_10_1.migrations  -- NEW
  ++ V1_10_1_to_V1_11_0.migrations
```

#### Handling the Existing Hand-Written Upgrade Scripts

Existing deployments that already ran the hand-written `v1.10.0_to_v1.11.0` migration have that name recorded in `schema_migrations`. The new vendored migrations use different names (`pgmq_v1.10.0_to_v1.10.1`, `pgmq_v1.10.1_to_v1.11.0`), so `hasql-migration` will treat them as new, unapplied migrations.

This is actually correct behavior — the vendored migrations will re-apply the upstream function definitions, overwriting the divergent pgmq-hs versions. All the operations are `CREATE OR REPLACE FUNCTION` or `DROP FUNCTION IF EXISTS` + `CREATE FUNCTION`, which are safe to re-run. The structural changes (e.g., `ALTER TABLE ADD COLUMN`) use `IF NOT EXISTS` guards.

For clarity, a compatibility note should be added to the migration module documentation.

### Workflow for Upgrading to a New pgmq Version

```bash
# 1. Pull upstream changes into the subtree
git subtree pull --prefix vendor/pgmq \
  https://github.com/tembo-io/pgmq.git v1.12.0 --squash

# 2. Review the new migration file upstream provides
cat vendor/pgmq/pgmq-extension/sql/pgmq--1.11.0--1.12.0.sql

# 3. Verify no extension-specific patterns were introduced
grep -E 'ALTER EXTENSION|pg_extension_config_dump|@extschema@' \
  vendor/pgmq/pgmq-extension/sql/pgmq--1.11.0--1.12.0.sql

# 4. Create the version module (embedFile from vendored pgmq.sql)
#    Create the upgrade module (embedFile from vendored migration file)
#    Update Pgmq.Migration.Migrations to chain the new upgrade step

# 5. Run tests against ephemeral-pg
cabal test pgmq-migration
```

### Validation Strategy

To prevent future divergence, add a CI check:

1. **Checksum verification** — the embedded SQL's SHA-256 must match `vendor/pgmq/pgmq-extension/sql/pgmq.sql`. This ensures no one edits the vendored file directly without going through `git subtree pull`.

2. **Upstream freshness check** (optional) — a scheduled CI job compares the vendored commit against the latest upstream tag and opens an issue if they differ by more than one minor version.

## Migration Path from Current State

### Phase 1: Add Subtree and Switch to Vendored SQL

1. `git subtree add --prefix vendor/pgmq https://github.com/tembo-io/pgmq.git v1.11.0 --squash`
2. Replace `V1_11_0.hs` to embed from `vendor/pgmq/pgmq-extension/sql/pgmq.sql`
3. Remove the hand-written `database/v1.11.0/` directory
4. Replace upgrade modules:
   - Remove `V1_9_0_to_V1_10_0.hs` and its hand-written SQL
   - Add `V1_10_0_to_V1_10_1.hs` embedding `pgmq--1.10.0--1.10.1.sql`
   - Replace `V1_10_0_to_V1_11_0.hs` with `V1_10_1_to_V1_11_0.hs` embedding `pgmq--1.10.1--1.11.0.sql`
5. Update `Migrations.hs` to chain: `V1_10_0_to_V1_10_1` → `V1_10_1_to_V1_11_0`
6. Remove `V1_9_0.hs`, `V1_10_0.hs`, and the `database/v1.9.0/`, `database/v1.10.0/` directories
7. Remove the hand-written `database/migrations/` directory
8. Run full test suite

### Phase 2: Update CLAUDE.md and Workflows

Update the "pgmq-migration" section in CLAUDE.md to reflect the new workflow:

```markdown
## pgmq-migration

SQL is vendored from upstream pgmq via git subtree at `vendor/pgmq/`.
Do not hand-write SQL — all SQL comes from the vendored source.

When updating the pgmq schema version:

1. `git subtree pull` the new upstream version into `vendor/pgmq/`
2. Verify the new migration file has no extension-specific patterns
3. Create a new version module that embeds `vendor/pgmq/.../pgmq.sql`
4. Create an upgrade module that embeds the upstream migration file
5. Update `Pgmq.Migration.Migrations` to chain the new upgrade step
6. Run `cabal test pgmq-migration`
```

## Risks and Mitigations

| Risk | Mitigation |
|------|------------|
| Upstream `pgmq.sql` changes internal structure, breaking single-file embedding | The file is pure SQL executed top-to-bottom; structural changes don't affect execution order |
| `hasql-migration` checksum mismatch on existing deployments when switching from split files to single file | New version (v1.12.0+) uses a new migration name; old split migrations remain tracked but aren't re-run |
| Upstream adds C-language functions requiring `$libdir` | Monitor upstream; pgmq has been pure PL/pgSQL since inception and the maintainers have stated this is intentional |
| `git subtree pull` conflicts | Use `--squash` to flatten upstream history; conflicts are limited to `vendor/pgmq/` |
| `file-embed` path changes if vendor directory moves | Pin the path in cabal's `extra-source-files` and the Haskell module; CI checksum verification catches drift |

## Decision

Vendor upstream pgmq as a git subtree at `vendor/pgmq/`. Use the vendored SQL for both fresh installs (`pgmq.sql`) and version-to-version upgrades (`pgmq--X.Y.Z--A.B.C.sql`). The upstream migration files from 1.9.0 onward contain no extension-specific patterns and work as-is for non-extension deployment.

This eliminates all hand-maintained SQL — both the split schema snapshots that caused the metrics bug and 13 other divergences, and the hand-written upgrade scripts that skipped the 1.10.0→1.10.1 step. Future pgmq version upgrades become a mechanical process: `git subtree pull`, embed the new files, update the module chain.
