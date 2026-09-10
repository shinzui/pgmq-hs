# Revision history for pgmq-core

## 0.6.0.0 -- 2026-09-10

Coordinated family version bump; core types are unchanged from 0.5.0.0.

## 0.5.0.0 -- 2026-08-06

### Breaking Changes

* Queue names are now validated consistently at every entry path. `parseQueueName` rejects
  the empty string and any character outside lowercase ASCII letters, digits, and
  underscore — previously uppercase was accepted and the empty string passed every check.
  `FromJSON QueueName` is now a hand-written instance that validates via `parseQueueName`;
  it was newtype-derived, accepting any string of any length, so configuration-loaded
  names bypassed validation entirely.

  Lowercase-only is a correctness requirement, not a style choice. pgmq's SQL lowercases
  physical table names while `pgmq.meta` stores the caller's original casing, and the
  notification trigger looks up the lowercased name. `MyQueue` and `myqueue` were
  therefore two logical queues silently interleaving in one physical table: dropping
  either destroyed the other's messages, and a mixed-case notification throttle was never
  matched by the trigger. Rejection was chosen over normalization because normalizing
  would silently join pre-existing mixed-case metadata and make the parsed name disagree
  with what the caller wrote.

  **Upgrade note**: `listQueues` re-validates names read back from the database, so a
  deployment whose `pgmq.meta` still contains mixed-case rows must run the transactional
  remediation in `docs/design/016-queue-name-validation.md` — which preserves topic
  bindings and notification configuration — before upgrading. Do not update or delete
  `pgmq.meta` rows by hand: both child foreign keys cascade on delete.

### New Features

* `notifyChannelName :: QueueName -> Text` returns the LISTEN/NOTIFY channel a queue's
  insert notifications arrive on (`pgmq.q_<lowercased name>.INSERT`). It is now the
  contract; do not assemble the name by hand. It lives here rather than in pgmq-hasql
  because a LISTEN consumer needs a raw connection anyway and may not depend on
  pgmq-hasql at all.
* `UnvalidatedQueue`, a queue listing row whose name is plain `Text`. pgmq's server-side
  validator checks only name length, so any client sharing the database can create a name
  `parseQueueName` rejects; this type is what the lenient pgmq-hasql and pgmq-effectful
  listings decode into.

### Other Changes

* New `pgmq-core-test` suite pinning both queue-name entry paths, `parseQueueName` and
  `FromJSON`.

## 0.4.0.1 -- 2026-07-14

* Version bump only — coordinated release with pgmq-migration 0.4.0.1.
  No source-level changes since 0.4.0.0.

## 0.4.0.0 -- 2026-07-14

* Version bump only — coordinated release with pgmq-migration 0.4.0.0.
  No source-level changes since 0.3.0.0.

## 0.3.0.0 -- 2026-05-31

* Version bump only — coordinated release with pgmq-effectful 0.3.0.0.
  No source-level changes since 0.2.0.0.

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
