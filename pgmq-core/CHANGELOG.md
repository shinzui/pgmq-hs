# Revision history for pgmq-core

## 0.2.0.0 -- 2026-04-23

* Version bump only — coordinated release with pgmq-effectful 0.2.0.0.
  No source-level changes since 0.1.3.0.

## 0.1.3.0 -- 2026-03-12

### Other Changes

* Update documentation URLs from tembo.io to pgmq.github.io
* Update repository homepage URL to shinzui/pgmq-hs

## 0.1.2.0 -- 2026-03-03

* Version bump only (no changes)

## 0.1.1.0 -- 2026-02-23

### New Features

* Topic routing types (pgmq 1.11.0+): `RoutingKey`, `TopicPattern`, `TopicBinding`, `RoutingMatch`, `TopicSendResult`
* Notification throttle type: `NotifyInsertThrottle`
* Validation functions: `parseRoutingKey`, `routingKeyToText`, `parseTopicPattern`, `topicPatternToText`
* Extended `PgmqError` with `InvalidRoutingKey` and `InvalidTopicPattern` constructors

## 0.1.0.0 -- 2026-02-21

* Initial release
* Core types: `Message`, `MessageBody`, `MessageHeaders`, `MessageId`, `Queue`, `QueueName`, `PgmqError`
* Queue name validation following pgmq-rs conventions
* Template Haskell `Lift` instance for `QueueName`
