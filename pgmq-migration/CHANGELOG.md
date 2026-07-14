# Changelog for pgmq-migration

## 0.4.0.0 -- 2026-07-10

### Breaking Changes

* Replace the `hasql-migration` command/session API with the native
  `pgmqMigrations :: Either DefinitionError MigrationComponent` API.
* Remove `migrate`, `upgrade`, `validate`, predecessor command lists, and predecessor
  result types. Consumers now compose and run a `pg-migrate` plan.
* Require the `pg-migrate` 1.1 family (`pg-migrate`, `pg-migrate-embed`, and
  `pg-migrate-import-hasql-migration`), up from 1.0. That release reshapes types this
  package's callers handle directly: `HistoryImportReport` becomes a multi-field record,
  `CleanupFailed` carries a primary error plus a `NonEmpty CleanupIssue`, and `SqlError`
  and `HistoryValidationError` gain constructors that exhaustive matches must cover.

### New Features

* Add exact-MD5 direct history import for `pgmq_v1.11.0`.
* Add an explicitly opted-in two-step equivalent-history route guarded by a read-only
  PGMQ 1.11 schema contract.
* Append `0002-schema-management-comment` as an observable native-runner canary after the
  imported historical baseline.

### Other Changes

* Load `Database.PostgreSQL.Migrate.Embed.RecompilePlugin` in the manifest-embedding
  module. GHC 9.12 offers Template Haskell no way to depend on the migrations directory
  itself, so adding or removing a SQL file could otherwise reuse a stale object file and
  skip strict manifest membership validation. This forces the module to recompile whenever
  GHC runs over the package. Note that a build where *only* SQL files changed can still be
  short-circuited by `cabal`'s own up-to-date check before GHC is invoked.

## 0.3.0.0 -- 2026-05-31

* Version bump only — coordinated release with pgmq-effectful 0.3.0.0.
  No source-level changes since 0.2.0.0.

## 0.2.0.0 -- 2026-04-23

* Version bump only — coordinated release with pgmq-effectful 0.2.0.0.
  No source-level changes since 0.1.3.0.

## 0.1.3.0 -- 2026-03-12

### Other Changes

* Update repository homepage URL to shinzui/pgmq-hs

## 0.1.2.0 -- 2026-03-03

### Other Changes

* Vendor upstream pgmq SQL via git subtree, replacing hand-written SQL files
* SQL is now embedded from `vendor/pgmq/pgmq-extension/sql/` instead of local `database/` directory

## 0.1.1.0 -- 2026-02-23

### New Features

* Support for PGMQ v1.11.0 schema installation
* Incremental migration from v1.10.0 to v1.11.0 (topic routing, batch topic sends, throttle management)

## 0.1.0.0 -- 2026-02-21

* Initial release
* Support for PGMQ v1.9.0 schema installation
* Support for PGMQ v1.10.0 schema installation
* Incremental migration support (v1.9.0 to v1.10.0 upgrade path)
