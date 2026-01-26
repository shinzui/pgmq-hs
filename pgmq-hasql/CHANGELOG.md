# Revision history for pgmq-hasql

## Unreleased

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
  - pgmq commit: [b19033d](https://github.com/tembo-io/pgmq/commit/b19033d)
- Notification throttling via `throttleIntervalMs` in `EnableNotifyInsert`
  - pgmq commit: [f0b4acb](https://github.com/tembo-io/pgmq/commit/f0b4acb)
- FIFO read functions:
  - `readGrouped`: SQS-style batch filling from same message group
  - `readGroupedWithPoll`: Same with polling support
  - pgmq commit: [730f679](https://github.com/tembo-io/pgmq/commit/730f679)
- FIFO index management:
  - `createFifoIndex`: Create FIFO index for a specific queue
  - `createFifoIndexesAll`: Create FIFO indexes for all queues
- New types: `ReadGrouped`, `ReadGroupedWithPoll`

#### pgmq 1.9.0+ Support
- Round-robin FIFO read functions:
  - `readGroupedRoundRobin`: Fair distribution across message groups
  - `readGroupedRoundRobinWithPoll`: Same with polling support
  - pgmq commits: [cb5dd01](https://github.com/tembo-io/pgmq/commit/cb5dd01), [2129a38](https://github.com/tembo-io/pgmq/commit/2129a38)
- Note: FIFO functions do not support `conditional` parameter (removed in pgmq 1.9.0)
  - pgmq commit: [9e9c3dc](https://github.com/tembo-io/pgmq/commit/9e9c3dc)

### Deprecations

- `detachArchive` is now deprecated (no-op in pgmq, will be removed in pgmq 2.0)

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
