---
type: Review
title: pgmq-config from v0.4.0.1 to v0.6.1.0 — the reconciler can create over a mixed-case alias and overclaims on drift
description: The single-core reconciler, throttle drift, FIFO presence, and foreign-name handling are correct, but a declared lowercase name colliding with a foreign mixed-case row is created over the shared table, and the drift Haddock and user guide overclaim.
generated:
  by: anthropic/claude-fable-5-1
  at: "2026-09-16T20:57:04Z"
reviewId: REV-2
subject: mori://shinzui/pgmq-hs/packages/pgmq-config
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
outcome: changes-requested
dimensions:
  - correctness
  - design
  - documentation
  - test-coverage
produced:
  - mori://shinzui/pgmq-hs/masterplans/6-close-the-notification-reconciler-and-evidence-gaps-surfaced-by-the-0-6-1-0-review
  - mori://shinzui/pgmq-hs/plans/24-report-name-collisions-and-unsupported-notifications-instead-of-acting-on-them
context: >-
  Read the full current Reconcile.hs, Config.hs, Config/Effectful.hs, and Config/Types.hs
  (the range rewrote them), the config test suites, design note 018, the capability record,
  and the user guide; checked every upstream pgmq function the reconciler calls for
  convergence under concurrent callers; did not run the suites.
---

# pgmq-config from v0.4.0.1 to v0.6.1.0

## What was examined

The extraction of the reconciler into `Pgmq.Config.Reconcile` with a `ReconcileOps` record,
the unvalidated snapshot, throttle-drift updates, FIFO-index presence via `pg_indexes`,
queue-type drift reporting, premake forwarding, and the contract documentation. Concerns
examined: correctness of each decision path, convergence under concurrent replicas, whether
the documentation matches behavior, and test coverage of the new paths.

## Findings

A declared lowercase name whose only registry match is a foreign mixed-case row (`Foo` when
`foo` is declared) is invisible to the textual match, so the reconciler calls
`pgmq.create('foo')`. That is a no-op on the shared physical table but inserts a second
`pgmq.meta` row, recreating the aliasing hazard design note 016 remediates. The contract says
foreign rows are unmanaged; it does not say the reconciler may alias one. A lowercased
comparison would let it report the collision instead.

The `DetectedQueueTypeDrift` Haddock says "Nothing was mutated and nothing will be", but the
reconciler still enables notifications, creates the FIFO index, and binds topics on the drifted
queue. Each of those reports its own action, so the report is truthful; the sentence is not.

The user guide's `ReconcileAction` listing stops at `SkippedTopicBinding` and lacks the two
constructors 0.5.0.0 added, and its opening sentence "every operation is idempotent" predates
the one documented in-place mutation.

A notification declared on a partitioned queue is enabled although it can never be delivered
on the documented channel (see REV-1); the reconciler could report it as unsupported.

## Verified correct

The core needs only `Monad m`; both adapters wire the same twelve operations. Throttle drift
compares `Nothing` as 250 so a defaulted config does not flap, updates only when the stored
interval differs, and never re-enables (which would reset `last_notified_at`). FIFO presence
reads `pg_indexes` with the same name `CREATE INDEX IF NOT EXISTS` checks, so report and
statement agree. Queue-type drift is derived from the two booleans `pgmq.list_queues()`
reports. Concurrent reconciles converge: `pgmq.create` takes the queue lock and upserts,
`bind_topic` is `ON CONFLICT DO NOTHING`, `create_fifo_index` is `IF NOT EXISTS`, and
on the native ledger `enable_notify_insert` takes the queue lock. The foreign-name fix
(snapshot through the unvalidated listing) is correct and covered by a dedicated-instance
test.
