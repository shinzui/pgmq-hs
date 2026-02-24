# Revision history for pgmq-hs

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
