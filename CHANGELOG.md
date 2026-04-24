# Revision history for pgmq-hs

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
