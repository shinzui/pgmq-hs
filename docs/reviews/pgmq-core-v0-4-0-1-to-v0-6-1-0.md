---
type: Review
title: pgmq-core from v0.4.0.1 to v0.6.1.0 — validation and the channel helper are correct, with one documentation gap
description: Lowercase-only queue-name validation and the hand-written FromJSON match the server's behavior, and notifyChannelName matches the trigger for ordinary queues; its Haddock does not say that a partitioned queue's channel is never published to.
generated:
  by: anthropic/claude-fable-5-1
  at: "2026-09-16T20:57:04Z"
reviewId: REV-4
subject: mori://shinzui/pgmq-hs/packages/pgmq-core
subjectKind: component
reviewedSha: 163413b3ccc85ce6a517646da6ff8969c2dd7482
coverage: incremental
baseSha: 7535d4915d4ccaa717d860cb0aa54b142d3e2bf8
reviewedAt: "2026-09-16T20:57:04Z"
reviewerKind: model
reviewer: process:claude-code
provider: anthropic
model: claude-fable-5-1
effort: xhigh
outcome: commented
dimensions:
  - correctness
  - documentation
produced:
  - mori://shinzui/pgmq-hs/masterplans/6-close-the-notification-reconciler-and-evidence-gaps-surfaced-by-the-0-6-1-0-review
  - mori://shinzui/pgmq-hs/plans/23-gate-the-notification-fail-open-on-a-real-queue-row-and-state-the-partitioned-queue-contract
context: >-
  Read the diff of pgmq-core/src/Pgmq/Types.hs between the two tags and the vendored
  validate_queue_name, format_table_name, and notify_queue_listeners bodies it must agree with;
  did not run the suite.
---

# pgmq-core from v0.4.0.1 to v0.6.1.0

## What was examined

`parseQueueName` (now non-empty, lowercase ASCII letters, digits, underscore, at most 47
characters), the hand-written `FromJSON QueueName`, the new `UnvalidatedQueue` record, and
`notifyChannelName`. Concerns examined: agreement with the server's own checks and with the
trigger's channel derivation, and whether the Haddocks state the contract.

## Remark

`notifyChannelName` computes `pgmq.q_<lowercased name>.INSERT`, which is exactly what the
trigger publishes for an ordinary queue. For a partitioned queue the trigger fires on a leaf
partition and, on stock installs, never publishes at all (see REV-1 for the native-install
behavior). The Haddock and design note 015 do not say so, so a reader following them listens
on a channel that never receives anything for a partitioned queue.

## Verified correct

The 47-character limit matches `pgmq.validate_queue_name`. Lowercase-only is justified by
`format_table_name` lowercasing physical names while `pgmq.meta` keeps the caller's casing;
the aliasing rationale in the Haddock is accurate. The `FromJSON` instance routes through
`parseQueueName`, closing the configuration-loaded bypass. `UnvalidatedQueue` carries the
four `queue_record` columns as plain values. No behavior in this package depends on the
effectful-core or PostgreSQL version.
