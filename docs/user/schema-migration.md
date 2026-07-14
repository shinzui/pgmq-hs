# Schema Migration

The `pgmq-migration` package installs the PGMQ schema into PostgreSQL **without** the pgmq
extension. Use it when you don't have superuser access, or when your platform won't let you
install extensions.

It ships the schema as a [`pg-migrate`](https://hackage.haskell.org/package/pg-migrate)
component. `pgmq-migration` itself contains no runner: you build a plan with `pg-migrate`
and run it. This means PGMQ composes into a plan alongside your own application's
migrations, in dependency order, under a single ledger.

## Dependencies

```cabal
build-depends:
  , pgmq-migration
  , pg-migrate
  -- only when importing a pre-0.4 ledger, see "Importing an existing installation":
  , pg-migrate-import-hasql-migration
```

Only `pgmqMigrations`, `AlternativeHistoryPolicy`, `pgmqHasqlMigrationMappings`,
`pgmqHasqlMigrationSourceConfig`, `pgmqV1_11StateValidator`, and
`pgmqV1_11StateEvidenceKey` come from this package. Every other name below comes from
`pg-migrate` (`Database.PostgreSQL.Migrate`) or `pg-migrate-import-hasql-migration`
(`Database.PostgreSQL.Migrate.History.HasqlMigration`).

## What the component contains

`pgmqMigrations :: Either DefinitionError MigrationComponent` is a component named `pgmq`
with no dependencies and two migrations:

| Migration | Purpose |
|-----------|---------|
| `0001-install-v1.11.0` | The full PGMQ 1.11.0 schema, byte-identical to upstream's vendored `pgmq.sql` |
| `0002-schema-management-comment` | Sets `COMMENT ON SCHEMA pgmq`. Non-destructive marker proving the native runner owns the schema |

The baseline is vendored from upstream via git subtree and is checked byte-for-byte against
`vendor/pgmq/pgmq-extension/sql/pgmq.sql` by the test suite. No SQL in this package is
hand-written.

## Fresh installation

For a database with no PGMQ schema:

```haskell
import Data.List.NonEmpty (NonEmpty (..))
import Database.PostgreSQL.Migrate
  (defaultRunOptions, migrationPlan, runMigrationPlan)
import Pgmq.Migration (pgmqMigrations)

installPgmq :: Settings -> IO ()
installPgmq connectionSettings = do
  component <- either (fail . show) pure pgmqMigrations
  plan      <- either (fail . show) pure (migrationPlan (component :| []))
  result    <- runMigrationPlan defaultRunOptions connectionSettings plan
  either (fail . show) print result
```

Both migrations report `AppliedNow` on the first run and `AlreadyApplied` on every run
after, so this is safe to call on every boot.

To check for pending work without applying it, use `verifyMigrationPlan` instead of
`runMigrationPlan`; it returns a `VerificationReport` whose issues list any
`PendingMigration`.

## Importing an existing installation

**Who needs this:** anyone whose PGMQ schema was installed by **pgmq-migration 0.3.0.0 or
earlier**. Those releases used `hasql-migration`, which records applied migrations in a
`public.schema_migrations` table. The native `pg-migrate` runner keeps its own ledger and
does not read that table.

If you skip the import, the runner sees no record of the baseline and attempts to install
the PGMQ schema on a database that already has it.

The import runs **no schema SQL**. It verifies the old ledger and writes the corresponding
rows into the native ledger, so the baseline is recorded as already applied.

### Choosing a policy

Pick the one matching how the database was originally installed:

| Policy | Original install | How it is verified |
|--------|------------------|--------------------|
| `DirectFullInstallHistory` | One step, as `pgmq_v1.11.0` | Reproduces the stored base64 MD5 and requires **exact payload equality** with the native baseline |
| `EquivalentTwoStepUpgradeHistory` | Installed at v1.10.0, upgraded v1.10.1 → v1.11.0 | Checksums of both upgrade rows, **plus** a read-only PGMQ 1.11 catalog contract |

The two-step route exists because those databases were built from a different byte
sequence than the native baseline — payload equality is impossible by construction. Rather
than trusting that, `pgmq-migration` verifies the *result*: `Pgmq.Migration.SchemaContract`
inspects the live catalog for every schema, table, column, constraint, type, and function
that pgmq-hs depends on, and the import fails if any is missing. The check is read-only and
changes no state.

Because that route asserts equivalence rather than identity, it is never selected
implicitly. You must opt in:

```haskell
withEquivalentHistory AllowEquivalentHistory defaultImportOptions
```

Passing `defaultImportOptions` with `EquivalentTwoStepUpgradeHistory` fails with
`HistoryEquivalentStateDisallowed`. That is deliberate — it forces the choice to be
explicit in your code.

### Performing the import

```haskell
import Data.List.NonEmpty (NonEmpty (..))
import Database.PostgreSQL.Migrate
  ( EquivalentHistoryPolicy (AllowEquivalentHistory)
  , connectionProviderFromSettings
  , defaultImportOptions
  , defaultRunOptions
  , migrationPlan
  , runMigrationPlan
  , withEquivalentHistory
  )
import Database.PostgreSQL.Migrate.History.HasqlMigration (importHasqlMigrationHistory)
import Pgmq.Migration (pgmqMigrations)
import Pgmq.Migration.History.HasqlMigration
  ( AlternativeHistoryPolicy (..)
  , pgmqHasqlMigrationMappings
  , pgmqHasqlMigrationSourceConfig
  )

cutover :: Settings -> IO ()
cutover connectionSettings = do
  let policy   = DirectFullInstallHistory
      provider = connectionProviderFromSettings connectionSettings
      options  = defaultImportOptions
      -- EquivalentTwoStepUpgradeHistory requires instead:
      -- options = withEquivalentHistory AllowEquivalentHistory defaultImportOptions

  component <- either (fail . show) pure pgmqMigrations
  plan      <- either (fail . show) pure (migrationPlan (component :| []))
  config    <- either (fail . show) pure (pgmqHasqlMigrationSourceConfig provider policy)
  mappings  <- either (fail . show) pure (pgmqHasqlMigrationMappings policy)

  -- 1. Import. Verifies the old ledger; runs no schema SQL.
  importReport <- importHasqlMigrationHistory options config provider plan mappings
  either (fail . show) print importReport

  -- 2. Run the native plan.
  runReport <- runMigrationPlan defaultRunOptions connectionSettings plan
  either (fail . show) print runReport
```

### What you should observe

After a successful import, the native plan reports:

```
0001-install-v1.11.0            AlreadyApplied   -- imported; SQL not replayed
0002-schema-management-comment  AppliedNow       -- the canary
```

`0002` is the point of the exercise. It is the first migration the native runner applies on
its own, and it only sets a schema comment — so a successful, non-destructive `AppliedNow`
proves the runner is correctly wired to your database before any migration that *does*
change the schema ever runs. Run the plan once more and both report `AlreadyApplied`.

The import is idempotent: calling it again reports `AlreadyImported`.

## Failure modes

The import verifies rather than trusts, and aborts without touching your schema if
anything doesn't line up:

| Failure | Meaning |
|---------|---------|
| `HasqlMigrationChecksumMismatch` | The ledger's stored MD5 doesn't match the payload — the recorded SQL isn't what this package shipped |
| `HasqlMigrationDuplicateLedgerFilename` | The same filename appears twice in `public.schema_migrations` |
| `HistoryEquivalentStateDisallowed` | You selected `EquivalentTwoStepUpgradeHistory` without `withEquivalentHistory AllowEquivalentHistory` |
| `HistoryStateValidationFailed` | The PGMQ 1.11 catalog contract failed — a required table, type, or function is missing. The error names what's absent |

A contract failure means the live schema isn't the PGMQ 1.11 surface pgmq-hs expects.
Reconcile the database before importing rather than forcing the import through.

## Upgrading PGMQ itself

Contributors: see the `pgmq-migration` section of [`CLAUDE.md`](../../CLAUDE.md). The
baseline is immutable — new schema versions are appended as new SQL migrations plus a
manifest entry, never by editing `0001`.
