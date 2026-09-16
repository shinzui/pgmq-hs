---
type: Review
title: pgmq-migration from v0.4.0.1 to v0.6.1.0 — the notify fail-open storms on partitioned queues
description: Migrations 0004 and 0005 match upstream byte-for-byte and 0006 preserves the partition guards, but 0003's fail-open notifies unthrottled on every insert into a partitioned queue, on a per-partition channel.
generated:
  by: anthropic/claude-fable-5-1
  at: "2026-09-16T20:57:04Z"
reviewId: REV-1
subject: mori://shinzui/pgmq-hs/packages/pgmq-migration
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
  - performance
  - operability
  - test-coverage
  - documentation
produced:
  - mori://shinzui/pgmq-hs/masterplans/6-close-the-notification-reconciler-and-evidence-gaps-surfaced-by-the-0-6-1-0-review
  - mori://shinzui/pgmq-hs/plans/23-gate-the-notification-fail-open-on-a-real-queue-row-and-state-the-partitioned-queue-contract
context: >-
  Read every change to pgmq-migration/migrations, the manifest, the package's test suite, and
  the vendored pgmq SQL between the two tags; diffed 0004 and 0005 against the upstream
  upgrade scripts; compared 0003's and 0006's bodies against upstream 1.13.0; read the plan
  retrospectives and ADRs for already-captured performance findings; and ran a scratch
  PostgreSQL 18.6 with pg_partman in the partman shell, applying the ledger 0001 through
  0006, to observe the trigger on a partitioned queue.
---

# pgmq-migration from v0.4.0.1 to v0.6.1.0

## What was examined

The four migrations added in this range — `0003-notify-crash-safety-and-locking.sql`,
`0004-upgrade-v1.12.0.sql`, `0005-upgrade-v1.13.0.sql`, and
`0006-preserve-partitioned-reentry-v1.13.0.sql` — plus the manifest, the vendored pgmq
1.13.0 source, and `pgmq-migration/test/Main.hs`. The package's Haskell sources did not
change in the range. Concerns examined: correctness of the hand-written SQL against the
upstream bodies it replaces, whether the local hardening survives the upstream upgrades,
performance cost on the insert path, operability after a crash, and whether the tests would
catch a regression.

## Finding: the fail-open fires on every insert into a partitioned queue

Migration 0003 makes `pgmq.notify_queue_listeners()` notify when its throttle row is absent,
so a crash that truncates the `UNLOGGED` throttle table cannot silence delivery. PostgreSQL
clones a row trigger onto every leaf partition, and inside the clone `TG_TABLE_NAME` is the
partition. The extracted name (`pq_p0` for queue `pq`) can never have a throttle row, so on
a partitioned queue the branch fires on every insert. Reproduced on the native ledger:

```text
-- partitioned queue 'pq', enable_notify_insert('pq', 250), three sends
Asynchronous notification "pgmq.q_pq_p0.INSERT" received   -- send 1
Asynchronous notification "pgmq.q_pq_p0.INSERT" received   -- send 2, inside 250 ms
Asynchronous notification "pgmq.q_pq_p0.INSERT" received   -- send 3
notify_insert_throttle.last_notified_at for pq: 1969-12-31 (never updated)
-- ordinary queue 'oq', same setup, two sends
Asynchronous notification "pgmq.q_oq.INSERT" received       -- send 1 only, throttled
```

Upstream's trigger matched zero rows on a partition and did nothing, so before 0003 a
partitioned queue with notifications enabled was silent. After 0003 every insert publishes a
notification, unthrottled, on a channel `notifyChannelName` never names, and every notifying
transaction takes PostgreSQL's database-wide notification lock at commit. This is both a
correctness gap (the fail-open cannot distinguish a truncated row from a name that can never
have one) and a performance regression on the insert path of any partitioned queue with
notifications enabled. No test covers notifications on a partitioned queue, and no
retrospective captures it; MasterPlan 5 explicitly defers auditing the notification overrides
to separate work. Design note 015 already describes the same failure class for a mixed-case
throttle row and calls it "missing permanently, not crash-missing".

A related documentation gap: the fail-open on ordinary queues lasts "until the next
reconcile", and the reconciler runs only at application startup, so after a database crash
the unthrottled state persists until the application is restarted.

## Verified correct

Migration 0004 is byte-identical to the concatenation of upstream's 1.11.0→1.11.1 and
1.11.1→1.12.0 upgrade scripts apart from one blank line; 0005 is byte-identical to the
1.12.0→1.13.0 script. Upstream did not re-create `notify_queue_listeners` or
`enable_notify_insert` between 1.11.0 and 1.13.0, so 0003's versions survive 0004 and 0005.
0006 correctly re-applies both `part_config` re-entry guards to the four-argument
`create_partitioned` while keeping the advisory lock, premake forwarding, and
`GENERATED BY DEFAULT`. The catalog-convergence test excepts exactly the three deliberately
diverged bodies. `enable_notify_insert` in 0003 takes the per-queue advisory lock before
its drop-and-create sequence and coalesces a NULL throttle to 250. The schema comparator's
positional `take 6` is the one ledger expectation that does not derive from the manifest.

## Performance already captured elsewhere

The GIN index that grouped reads cannot use and the unordered grouped output are recorded in
MasterPlan 5 and plans 19–21. The extra `EXISTS` probe 0003 adds on the throttled path is one
unique-index lookup per suppressed insert and is documented in design note 015.
