# Revision history for pgmq-hs

## Unreleased

The family now supports `effectful-core` 2.7 and no longer claims support for 2.5. The
declared range in `pgmq-effectful`, `pgmq-config` and `pgmq-bench` is `^>=2.6 || ^>=2.7`,
so a consumer already pinned to `effectful-core` 2.7 can depend on these packages. No
Haskell API changed: the effect layer builds unmodified against both ends of the range.

Prefer `effectful-core` 2.7.1.1 or newer over 2.7.0.0. 2.7.0.0 is inside the supported
range and builds correctly, but upstream increased the per-operation overhead of
dynamically dispatched effects in that release and fixed it in 2.7.1.1. Every
`pgmq-effectful` queue operation is dynamically dispatched, so the regression lands on the
hot path. This is a recommendation, deliberately not a version constraint.

The test suites now require `ephemeral-pg >=0.3.1.0` and pin every temporary PostgreSQL
cluster to a stable root at `/tmp/ephpg-pgmq-hs`. ephemeral-pg reaps abandoned clusters at
startup, but only inside its own temporary root; with `temporaryRoot` unset that root is
`$TMPDIR`, which `nix develop` allocates fresh per shell. A killed test run therefore
leaked its postmaster indefinitely, because no later run ever looked in the directory that
held it. Pinning one root lets a later run reclaim what an earlier one abandoned. No
Haskell API changed and no library package gained a dependency; this is test
infrastructure only.

`pg-migrate-test-support` caps `ephemeral-pg <0.3`, so `cabal.project` relaxes that single
bound through `allow-newer`.

## 0.6.0.0 -- 2026-09-10

The five-library family now supports PGMQ 1.12 grouped heads and 1.13 partition controls
and metrics. Both umbrellas expose all six grouped reads. Explicit premake creation is
available through sessions, effects, and declarative configuration. Native migrations reach
1.13 while preserving the 1.11 import contract and local notification/partition hardening.

Breaking source changes: constructors of `QueueMetrics` must supply
`defaultPartitionLength :: Maybe Int64`; constructors of `PartitionConfig` must supply
`premake = Nothing` or `Just n`. `CreatePartitionedQueue` keeps its three fields.
Grouped heads and legacy partition creation work on 1.12/1.13; explicit premake requires
1.13. The nullable metric is unavailable on 1.12 and inapplicable to ordinary queues.
Its value estimates spill across queue and archive defaults; it is not an instantaneous count.
See [the upgrade guide](docs/user/pgmq-0.6-upgrade.md) for examples and operator guidance.

## 0.5.0.0 -- 2026-08-06

All packages share the 0.5.0.0 version. This release hardens the whole family against the
defects surfaced by the 2026-07 and 2026-08 reviews: queue-name aliasing, NULL-parameter
semantics, notification delivery after a crash, transient-error classification, and a
reconciler that reported actions it had not taken. Grouped-head read support (pgmq 1.12.0)
is not in this release.

### Breaking Changes

* **pgmq-core**: queue names are now validated consistently at every entry path.
  `parseQueueName` rejects the empty string and any character outside lowercase ASCII
  letters, digits, and underscore (previously uppercase was accepted and empty passed
  every check), and `FromJSON QueueName` is a hand-written instance that validates via
  `parseQueueName` (previously newtype-derived, accepting any string of any length, so
  configuration-loaded names bypassed validation entirely). Lowercase-only is a
  correctness requirement: pgmq's SQL lowercases physical table names while `pgmq.meta`
  stores the caller's original casing and the notification trigger looks up the
  lowercased name, so `MyQueue` and `myqueue` were two logical queues silently
  interleaving in one physical table — dropping either destroyed the other's messages,
  and a mixed-case notification throttle was never matched by the trigger. Upgrade note:
  `listQueues` re-validates names read back from the database, so a deployment whose
  `pgmq.meta` still contains mixed-case rows must run the transactional remediation in
  `docs/design/016-queue-name-validation.md` — which preserves topic bindings and
  notification configuration — before upgrading. Do not update or delete `pgmq.meta`
  rows by hand: both child foreign keys cascade on delete.
* **pgmq-hasql, pgmq-effectful**: `changeVisibilityTimeout` and `setVisibilityTimeoutAt`
  now return `Maybe Message` instead of `Message`, at the statement, session, and effect
  layers. `pgmq.set_vt` is `RETURNS SETOF` and yields zero rows when the target message no
  longer exists (already deleted, archived, or popped). Decoding that with a single-row
  decoder raised an `UnexpectedRowCountStatementError` — the same error shape a genuine
  infrastructure failure has — so a caller extending a lease could not distinguish a lost
  race from a broken database. Callers that used the result must now handle `Nothing`;
  callers that discarded it compile unchanged. The batch variants are unaffected.
* **pgmq-config**: `ReconcileAction` gains two constructors, `UpdatedNotifyThrottle` and
  `DetectedQueueTypeDrift`, and `SkippedFifoIndex` is now actually emitted (it was
  unreachable dead code). Exhaustive matchers and consumers that count skips need
  updating. See the reconciliation fixes below for what each one now means.
* **pgmq-effectful**: the `Pgmq` effect GADT gains the `ListQueuesUnvalidated` and
  `ListFifoIndexQueueNames` constructors. Custom interpreters that match exhaustively must handle
  them; both stock interpreters already do.

### New Features

* **pgmq-core, pgmq-hasql**: `Pgmq.Types.notifyChannelName :: QueueName -> Text` returns
  the LISTEN/NOTIFY channel a queue's insert notifications arrive on, re-exported from the
  `Pgmq` umbrella module. It is now the contract; do not assemble the name by hand. See
  the corresponding documentation fix below.
* **pgmq-core, pgmq-hasql, pgmq-effectful**: an unvalidated queue listing, for reading a
  `pgmq.meta` that another client may have written names into. pgmq's server-side
  validator checks only length, so any co-tenant can create a name `parseQueueName`
  rejects, and the typed `listQueues` decoder fails the whole listing on one such row.
  `UnvalidatedQueue` (pgmq-core) decodes the name as `Text`; `listQueuesUnvalidated`
  (pgmq-hasql, re-exported from `Pgmq`) and the `ListQueuesUnvalidated` effect
  (pgmq-effectful) expose it. The typed `listQueues` keeps its strict decoding.
* **pgmq-hasql, pgmq-effectful**: `listFifoIndexQueueNames` reports which queues already carry a
  `q_<name>_fifo_idx`. pgmq exposes no index-existence query — `create_fifo_index`
  delegates to `CREATE INDEX IF NOT EXISTS` and reports nothing back — so this reads the
  `pg_indexes` catalog view directly. It is the first statement in this library that
  queries a catalog rather than calling a `pgmq.*` function; its traced span is named
  `pgmq.list_fifo_indexes` after the library operation, since no SQL function backs it.
* **pgmq-config**: `ObservedQueueType` (the three-way queue shape `pgmq.list_queues`
  actually reports) and `defaultThrottleMs` (250) are now exported.

### Bug Fixes

* **pgmq-config**: a queue created by another client under a name `parseQueueName` rejects
  no longer fails your application's startup. `ensureQueues` snapshotted existing queues
  through the typed `listQueues`, whose decoder re-validates every name read back, so one
  foreign row — say `billing-events` — made reconciliation throw at boot. It now
  snapshots through `listQueuesUnvalidated` and matches declared names textually; foreign
  rows are simply queues it does not manage, consistent with its additive contract.
* **pgmq-config**: `CreatedFifoIndex` is no longer reported when no index was created. The
  reconciler called `create_fifo_index` unconditionally and always reported creation,
  while `SkippedFifoIndex` was unreachable. It now consults a `pg_indexes` snapshot, skips
  the call when the index is present, and reports which of the two happened.
* **pgmq-config**: a declared notify throttle that differs from the stored row is now
  applied instead of silently ignored. The snapshot kept only queue names, so drift was
  invisible. It now carries intervals, and a difference is written via
  `pgmq.update_notify_insert` and reported as `UpdatedNotifyThrottle` with both values.
  `Nothing` and a stored 250 compare equal, so a defaulted config does not flap, and an
  unchanged interval is still left strictly alone rather than re-enabled — re-enabling
  resets `last_notified_at`. This is the reconciler's only mutation of existing state.
* **pgmq-config**: a declared queue type contradicting the live queue is now reported as
  `DetectedQueueTypeDrift` (carrying the declared and observed shapes) rather than
  `SkippedQueue`. Nothing is mutated: converting a queue's type means dropping and
  recreating it, destroying its messages, which a startup reconciler must never do.
  Partition interval and retention are not drift-checked, because `pgmq.list_queues` does
  not report them.
* **pgmq-migration**: insert notifications no longer stop permanently after a PostgreSQL
  crash. `pgmq.notify_insert_throttle` is `UNLOGGED`, so crash recovery truncates it, and
  the trigger notified only when its throttle `UPDATE` matched a row — after a crash it
  fired, matched nothing, and silently never notified again until an application restart
  re-enabled notify. Sends succeeded and messages accumulated while listeners starved.
  Migration `0003-notify-crash-safety-and-locking.sql` makes the trigger fail open: when
  the throttle row is absent it notifies unthrottled until the next reconcile restores the
  configured interval. Losing the throttle in a crash is acceptable; losing deliveries
  silently is not.
* **pgmq-migration**: concurrent `enable_notify_insert` calls for the same queue no longer
  race. Two replicas reconciling the same config at startup could both pass the function's
  internal `DROP TRIGGER IF EXISTS`, and the loser then failed with SQLSTATE 42710
  (duplicate_object), taking down that replica's entire startup reconcile — measured at
  roughly a 28% collision rate. The function now takes the per-queue advisory lock, as
  `pgmq.create` and `pgmq.create_partitioned` already do, which makes concurrent callers
  convergent.
* **pgmq-migration**: `pgmq.create_partitioned` is now re-entrant. The advisory lock
  serialized concurrent creators, but the second one still called `partman.create_parent`
  on a parent the first had just registered, and pg_partman rejects an already-managed
  parent. Both `create_parent` calls are now guarded by a `part_config` probe.
* **pgmq-migration**: `pgmq.enable_notify_insert` coalesces a NULL `throttle_interval_ms`
  to the documented 250 ms, so non-Haskell callers get the same guarantee the pgmq-hasql
  statement already provides.
* **pgmq-hasql**: `pop` with `qty = Nothing` now pops one message, as documented. It
  previously deleted and returned every visible message in the queue. The `Maybe`
  parameter was encoded as a nullable bind, so `Nothing` reached PostgreSQL as SQL NULL; a
  plpgsql parameter `DEFAULT` applies only to omitted arguments, and NULL in a `LIMIT`
  clause means `LIMIT ALL`. Because `pop` deletes, there was no visibility timeout to
  recover the messages.
* **pgmq-hasql**: `readMessage` and `readWithPoll` with `batchSize = Nothing` now read one
  message, as documented. They previously leased the entire queue through the same
  `LIMIT NULL` path, hiding every message from other consumers for the visibility timeout.
* **pgmq-hasql, pgmq-config**: `enableNotifyInsert` with `throttleIntervalMs = Nothing`
  (and the `pgmq-config` `withNotifyInsert Nothing` that wraps it) now installs the
  documented 250 ms throttle. It previously failed with SQLSTATE 23502 on every call,
  because a column `DEFAULT` does not apply to an explicitly supplied NULL. Since queue
  reconciliation is not one transaction, this failed application startup repeatedly and
  permanently for any config using the default throttle.
* **pgmq-hasql**: `ReadMessage.conditional` now filters. The field existed and was
  documented, but was never encoded, so a `Just` filter was silently ignored and every
  visible message was returned. `readWithPoll`'s conditional already worked.
* **pgmq-effectful**: `isTransient` now classifies retry-worthy server errors as
  transient. Serialization failures (40001), deadlocks (40P01), lock timeouts (55P03),
  server shutdown and recovery (57P01/57P02/57P03), and resource exhaustion (class 53)
  all arrive as server errors inside `StatementSessionError`, which previously mapped to
  permanent unconditionally — so retry loops gated on `isTransient` failed fast on
  exactly the errors retries exist for. Every other statement error, including decode
  and row-count mismatches, remains permanent. The whitelist is pinned in both
  directions by tests and recorded in `docs/design/017-transient-error-classification.md`.
* **pgmq-hasql**: a message whose body is SQL NULL no longer poisons every read batch.
  The `message` column is nullable and `pgmq.send('q', NULL::jsonb)` is legal SQL for
  any non-Haskell producer; one such row made every batch containing it fail at decode —
  after the read statement had already bumped `vt` and `read_ct` for the whole batch —
  and the row could not be seen or archived through this client. A SQL NULL body now
  decodes as JSON `null` (`MessageBody Aeson.Null`, deliberately indistinguishable from
  an explicitly-sent JSON `null` body), so the row is readable, identifiable, and
  archivable through the normal API.

### Documentation

* **pgmq-hasql**: the documented LISTEN/NOTIFY channel name was wrong. `enableNotifyInsert`
  claimed notifications arrive on `pgmq_<queue_name>`; the real channel is
  `pgmq.q_<lowercased queue name>.INSERT`, so anyone following the documentation listened
  on a channel that never receives anything. Corrected on the Haddock and in
  `docs/design/006-queue-notifications.md`, and replaced by the `notifyChannelName` helper
  above. The full contract — including the poll-fallback requirement and the crash
  fail-open semantics — is in `docs/design/015-notification-delivery-contract.md`.
* **pgmq-config**: the package promised to "ensure all queues exist with the desired
  settings" and that "all operations are idempotent", neither of which described what the
  reconciler does. `ensureQueues`' Haddock is now the canonical statement of the contract:
  additive reconciliation, one documented mutation of existing state (throttle drift, with
  its `last_notified_at` side effect named), queue-type drift reported rather than
  repaired, partition settings deliberately unchecked, and the concurrent multi-replica
  caveat — SQLSTATE 42710 on stock upstream 1.11.0 extension installs, race-free on
  pgmq-migration installs via migration `0003`. `ensureQueuesReport` and both `Eff` twins
  point at that one description so the four copies cannot drift. Haddock coverage on the
  public modules is now 100%. The rationale is in
  `docs/design/018-reconciliation-contract.md`.

### Other Changes

* **pgmq-config**: the duplicated `Session` and `Effectful` reconciliation logic is now a
  single backend-agnostic core in `Pgmq.Config.Reconcile`, parameterized over a
  `ReconcileOps` record; the public modules are thin adapters over it. `Pgmq.Config.Reconcile`
  is an internal module and is not part of the public API.

## 0.4.0.1 -- 2026-07-14

All packages share the 0.4.0.1 version. Only pgmq-migration changed; pgmq-core,
pgmq-hasql, pgmq-effectful, and pgmq-config are coordinated version bumps with no
library source changes.

### Bug Fixes

* **pgmq-migration**: Add the explicit `SourceLedgerPolicy` and
  `pgmqHasqlMigrationSourceConfigWithPolicy` API so a verified PGMQ predecessor row can be
  imported from a deliberately shared `public.schema_migrations` table. The existing helper
  remains strict by default, selected payloads retain exact base64-MD5 verification, and
  unrelated rows are reported without being claimed or modified.

## 0.4.0.0 -- 2026-07-14

All packages share the 0.4.0.0 version. Only pgmq-migration changed; pgmq-core,
pgmq-hasql, pgmq-effectful, and pgmq-config are coordinated version bumps with no
library source changes.

### Breaking Changes

* **pgmq-migration**: Replace the public `hasql-migration` runner surface with a native
  `pg-migrate` component. `Pgmq.Migration` now exports only `pgmqMigrations`,
  `MigrationComponent`, and `DefinitionError`. Existing ledgers must be imported through
  the direct or explicitly opted-in equivalent-history adapter before the native runner is
  enabled; the native runner does not read `public.schema_migrations` on its own.
* **pgmq-migration**: Remove the `migrate`, `upgrade`, and `validate` operations, the
  migration metadata accessors, the `hasql-migration` re-exports, and the seven
  `Pgmq.Migration.Migrations.*`, `.Sessions`, `.Statements`, and `.Transactions` modules.
* **pgmq-migration**: Require the `pg-migrate` 1.1 family, up from 1.0. Downstream
  projects that compose a `pg-migrate` plan must handle the reshaped
  `HistoryImportReport` and `CleanupFailed`, plus new `SqlError` and
  `HistoryValidationError` constructors.

### New Features

* **pgmq-migration**: Add `Pgmq.Migration.History.HasqlMigration`, offering an exact-MD5
  direct import of a `pgmq_v1.11.0` ledger and an explicitly opted-in two-step
  equivalent-history route for a `v1.10.0 -> v1.10.1 -> v1.11.0` ledger.
* **pgmq-migration**: Add `Pgmq.Migration.SchemaContract`, a read-only PGMQ 1.11 schema
  contract that guards the equivalent-history route.
* **pgmq-migration**: Append a non-destructive schema-management comment as migration
  `0002`, proving the first native-only upgrade after either predecessor-history route.

### Other Changes

* **pgmq-migration**: Force recompilation of the manifest-embedding module via
  `pg-migrate-embed`'s `RecompilePlugin`, so an added or removed SQL file cannot reuse
  stale embedded bytes and skip manifest validation.
* **build**: Migrate the Nix flake to flake-parts on the haskell-nix-dev base flake.
* **build**: Remove a stray `result-1` Nix build symlink from version control and ignore
  `result-*`.

## 0.3.0.0 -- 2026-05-31

### Breaking Changes

* **pgmq-effectful**: Now requires the `hs-opentelemetry` 1.0 package
  family and `hs-opentelemetry-semantic-conventions` 1.40 (previously
  the 0.x series). Downstream projects must upgrade their OpenTelemetry
  dependencies to build against this release.

### New Features

* **pgmq-effectful**: `Pgmq.Effectful.Telemetry` re-exports the stable
  semantic-convention attribute keys `messaging_operation_name`,
  `messaging_operation_type`, `db_system_name`, and `db_operation_name`.
* **pgmq-effectful**: The traced interpreter honours
  `OTEL_SEMCONV_STABILITY_OPT_IN` to choose old, stable, or duplicate
  messaging and database attributes. By default, traced spans keep the
  previous v1.24 attribute names for compatibility. Set
  `OTEL_SEMCONV_STABILITY_OPT_IN=messaging,database` to emit stable
  `messaging.operation.name`, `messaging.operation.type`,
  `db.system.name`, and `db.operation.name` attributes, or use
  `messaging/dup,database/dup` to emit both old and stable names during
  migration.

### Other Changes

* **pgmq-core**, **pgmq-hasql**, **pgmq-migration**, **pgmq-config**:
  Version bumps only to keep the shared-version / single-tag release
  model. No source-level changes since 0.2.0.0.

## 0.2.0.0 -- 2026-04-23

### Breaking Changes

* **pgmq-effectful**: OpenTelemetry semantic conventions updated to
  spec v1.24. Attribute names, span names, and values all change
  (`messaging.operation.type` → `messaging.operation`, `"send"` →
  `"publish"`, `"pgmq <op>"` → `"<operation> <destination>"`, etc.).
  Queue management operations move from `Producer` to `Internal` span
  kind. Dashboards and alerts keyed on the old names need updating.
* **pgmq-effectful**: Trace context propagation now routes through the
  tracer provider's configured propagator (W3C, B3, Datadog, …)
  instead of being hard-wired to W3C. `injectTraceContext` /
  `extractTraceContext` take a `TracerProvider`. `TraceHeaders` is
  now `Network.HTTP.Types.RequestHeaders`. `readMessageWithContext`
  returns `Vector (Message, OpenTelemetry.Context.Context)`.
* **pgmq-effectful**: Renamed interpreter error type from `PgmqError`
  to `PgmqRuntimeError`. Replaced the opaque `PgmqPoolError UsageError`
  constructor with three structured constructors
  (`PgmqAcquisitionTimeout`, `PgmqConnectionError`,
  `PgmqSessionError`). Legacy names are deprecated and will be removed
  in 0.3.0.0.
* **pgmq-effectful**: `runPgmqTraced` / `runPgmqTracedWith` now require
  an `Error PgmqRuntimeError` effect. Previously they silently swallowed
  errors as `IOError`s outside the Error channel; code that relied on
  that behaviour must now wrap with `runError @PgmqRuntimeError`.

### New Features

* **pgmq-effectful**: `fromUsageError` converter and `isTransient`
  classification helper for retry logic.

### Bug Fixes

* **pgmq-config**: `ensureQueues` and `ensureQueuesEff` are now truly
  idempotent. They previously re-issued queue creation, notify-insert,
  FIFO-index, and topic-bind SQL on every call, which broke partitioned
  queues outright (because `pg_partman.create_parent` raises on
  re-registration) and caused trigger recreation on every boot for
  standard queues.

### Other Changes

* **pgmq-core**, **pgmq-hasql**, **pgmq-migration**: Version bumps only
  to keep the shared-version / single-tag release model. No
  source-level changes since 0.1.3.0.

## 0.1.3.0 -- 2026-03-12

### New Features

* **pgmq-config**: New package for declarative queue configuration DSL — define queue topology as Haskell values and call a single function at startup to ensure all queues exist with desired settings

### Other Changes

* Update documentation URLs from tembo.io to pgmq.github.io
* Update repository homepage URLs to shinzui/pgmq-hs

## 0.1.2.0 -- 2026-03-03

### Other Changes

* **pgmq-migration**: Vendor upstream pgmq SQL via git subtree, replacing hand-written SQL files
* Add mori.dhall project identity manifest

## 0.1.1.0 -- 2026-02-23

### New Features

#### pgmq 1.11.0+ Support

* **pgmq-core**: Topic routing types (`RoutingKey`, `TopicPattern`, `TopicBinding`, `RoutingMatch`, `TopicSendResult`, `NotifyInsertThrottle`); extended `PgmqError` with new constructors
* **pgmq-hasql**: Topic management, topic sending, and notification management functions
* **pgmq-effectful**: Effectful effects and interpreters for all pgmq 1.11.0 operations
* **pgmq-migration**: v1.11.0 schema installation and v1.10.0 → v1.11.0 migration path

### Other Changes

* Improved README with usage example and cleaner structure

### Upstream Compatibility

| pgmq-hs Feature | Minimum pgmq Version |
|-----------------|---------------------|
| Topic routing | 1.11.0 |
| Topic sending | 1.11.0 |
| Notification throttle management | 1.11.0 |

## 0.1.0.0 -- 2026-02-21

Initial release of all packages:
- **pgmq-core** 0.1.0.0
- **pgmq-hasql** 0.1.0.0
- **pgmq-effectful** 0.1.0.0
- **pgmq-migration** 0.1.0.0

### Breaking Changes

- **Message type**: Added `headers :: Maybe Value` field (pgmq 1.5.0+)
- **QueueMetrics type**: Added `queueVisibleLength :: Int64` field (pgmq 1.5.0+)
- **ReadMessage type**: Added `conditional :: Maybe Value` field (pgmq 1.5.0+)
- **pop function**: Now takes `PopMessage` and returns `Vector Message` (pgmq 1.7.0+)

### New Features

#### pgmq 1.5.0+ Support
- Message headers: `sendMessageWithHeaders`, `sendMessageWithHeadersForLater`,
  `batchSendMessageWithHeaders`, `batchSendMessageWithHeadersForLater`
- Conditional read filtering via `conditional` field in `ReadMessage`
- Added `queueVisibleLength` to `QueueMetrics`

#### pgmq 1.7.0+ Support
- Pop with quantity via `PopMessage` type
- Queue notifications: `enableNotifyInsert`, `disableNotifyInsert`

#### pgmq 1.8.0+ Support
- Batch visibility timeout: `batchChangeVisibilityTimeout`
  - pgmq commit: [b19033d](https://github.com/tembo-io/pgmq/commit/b19033d) "Add support for batch visibility timeout updates (#452)"
- Notification throttling via `throttleIntervalMs` in `EnableNotifyInsert`
  - pgmq commit: [f0b4acb](https://github.com/tembo-io/pgmq/commit/f0b4acb) "Add notification throttling for insert listeners (#445)"
- FIFO read functions for SQS-style message grouping:
  - `readGrouped`: Fills batch from same message group
  - `readGroupedWithPoll`: Same with polling support
  - pgmq commit: [730f679](https://github.com/tembo-io/pgmq/commit/730f679) "Consider supporting FIFO + message keys (#442)"
- FIFO index management:
  - `createFifoIndex`: Create FIFO index for a specific queue
  - `createFifoIndexesAll`: Create FIFO indexes for all queues

#### pgmq 1.9.0+ Support
- Round-robin FIFO read functions for fair distribution across message groups:
  - `readGroupedRoundRobin`: Layered round-robin across groups
  - `readGroupedRoundRobinWithPoll`: Same with polling support
  - pgmq commit: [cb5dd01](https://github.com/tembo-io/pgmq/commit/cb5dd01) "update fifo test and migration sql (#475)"
  - pgmq commit: [2129a38](https://github.com/tembo-io/pgmq/commit/2129a38) "fix message ordering in `read_grouped_rr` (#477)"
- Note: The `conditional` parameter was removed from FIFO functions in pgmq 1.9.0
  - pgmq commit: [9e9c3dc](https://github.com/tembo-io/pgmq/commit/9e9c3dc) "Remove `conditional` param from FIFO (#480)"

#### pgmq 1.10.0+ Support
- Timestamp-based `set_vt` API
- `lastReadAt` field on `Message` type

### Deprecations

- `detachArchive` is now deprecated (no-op in pgmq, will be removed in pgmq 2.0)

### Upstream Compatibility

| pgmq-hs Feature | Minimum pgmq Version | Reference Commit |
|-----------------|---------------------|------------------|
| Message headers | 1.5.0 | - |
| Conditional read | 1.5.0 | - |
| Pop with quantity | 1.7.0 | [e7c5c93](https://github.com/tembo-io/pgmq/commit/e7c5c93) |
| Queue notifications | 1.7.0 | [9531535](https://github.com/tembo-io/pgmq/commit/9531535) |
| Batch set_vt | 1.8.0 | [b19033d](https://github.com/tembo-io/pgmq/commit/b19033d) |
| Notification throttling | 1.8.0 | [f0b4acb](https://github.com/tembo-io/pgmq/commit/f0b4acb) |
| FIFO read_grouped | 1.8.0 | [730f679](https://github.com/tembo-io/pgmq/commit/730f679) |
| FIFO indexes | 1.8.0 | [730f679](https://github.com/tembo-io/pgmq/commit/730f679) |
| FIFO round-robin | 1.9.0 | [cb5dd01](https://github.com/tembo-io/pgmq/commit/cb5dd01) |
| Timestamp set_vt | 1.10.0 | - |
| lastReadAt | 1.10.0 | - |
