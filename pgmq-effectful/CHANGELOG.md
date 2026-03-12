# Revision history for pgmq-effectful

## 0.1.3.0 -- 2026-03-12

### Other Changes

* Update repository homepage URL to shinzui/pgmq-hs

## 0.1.2.0 -- 2026-03-03

* Version bump only (no changes)

## 0.1.1.0 -- 2026-02-23

### New Features

* Effectful effects and interpreters for pgmq 1.11.0 topic routing operations
* Topic management: `bindTopic`, `unbindTopic`, `validateRoutingKey`, `validateTopicPattern`, `testRouting`, `listTopicBindings`, `listTopicBindingsForQueue`
* Topic sending: `sendTopic`, `sendTopicWithHeaders`, `batchSendTopic`, `batchSendTopicForLater`, `batchSendTopicWithHeaders`, `batchSendTopicWithHeadersForLater`
* Notification management: `listNotifyInsertThrottles`, `updateNotifyInsert`

## 0.1.0.0 -- 2026-02-21

* Initial release
* Effectful effects and interpreters for all pgmq operations
* OpenTelemetry instrumentation via traced interpreter
* Support for pgmq 1.5.0 through 1.10.0 features
