# Revision history for pgmq-hs

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
- Notification throttling via `throttleIntervalMs` in `EnableNotifyInsert`

### Deprecations

- `detachArchive` is now deprecated (no-op in pgmq, will be removed in pgmq 2.0)

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
