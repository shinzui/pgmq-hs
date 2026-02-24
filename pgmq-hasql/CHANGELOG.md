# Revision history for pgmq-hasql

## 0.1.1.0 -- 2026-02-23

### New Features

#### pgmq 1.11.0+ Support

* Topic management: `bindTopic`, `unbindTopic`, `validateRoutingKey`, `validateTopicPattern`, `testRouting`, `listTopicBindings`, `listTopicBindingsForQueue`
* Topic sending: `sendTopic`, `sendTopicWithHeaders`, `batchSendTopic`, `batchSendTopicForLater`, `batchSendTopicWithHeaders`, `batchSendTopicWithHeadersForLater`
* Notification management: `listNotifyInsertThrottles`, `updateNotifyInsert`
* New parameter types: `BindTopic`, `UnbindTopic`, `SendTopic`, `SendTopicWithHeaders`, `BatchSendTopic`, `BatchSendTopicForLater`, `BatchSendTopicWithHeaders`, `BatchSendTopicWithHeadersForLater`, `UpdateNotifyInsert`

## 0.1.0.0 -- 2026-02-21

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
- Notification throttling via `throttleIntervalMs` in `EnableNotifyInsert`
- FIFO read functions:
  - `readGrouped`: SQS-style batch filling from same message group
  - `readGroupedWithPoll`: Same with polling support
- FIFO index management:
  - `createFifoIndex`: Create FIFO index for a specific queue
  - `createFifoIndexesAll`: Create FIFO indexes for all queues
- New types: `ReadGrouped`, `ReadGroupedWithPoll`

#### pgmq 1.9.0+ Support
- Round-robin FIFO read functions:
  - `readGroupedRoundRobin`: Fair distribution across message groups
  - `readGroupedRoundRobinWithPoll`: Same with polling support
- Note: FIFO functions do not support `conditional` parameter (removed in pgmq 1.9.0)

#### pgmq 1.10.0+ Support
- Timestamp-based `set_vt` API
- `lastReadAt` field on `Message` type

### Deprecations

- `detachArchive` is now deprecated (no-op in pgmq, will be removed in pgmq 2.0)

* Initial release with full PGMQ API coverage
