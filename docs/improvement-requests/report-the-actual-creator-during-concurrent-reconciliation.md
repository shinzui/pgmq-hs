---
type: Improvement Request
title: Report the actual creator during concurrent reconciliation
description: >-
  Prevent duplicate-index errors during concurrent reconciliation and make each caller's action report distinguish real creation from a no-op after another caller wins.
generated:
  by: openai/gpt-6-sol
  at: "2026-09-22T21:46:00Z"
reviews: []
requestId: IR-5
status: proposed
origin: mori://shinzui/keiro-runtime-kenshou
---

# Improvement Request: Report the Actual Creator During Concurrent Reconciliation

## Problem

`pgmq-config` promises a report of actions taken by `ensureQueuesReport`. In `Pgmq.Config.Reconcile`, each caller snapshots the catalog before entering the per-resource create operation, then unconditionally reports `CreatedQueue`, `EnabledNotify`, or `CreatedFifoIndex` if the snapshot lacked that resource. The underlying PGMQ operations converge safely under concurrent startup, but a losing caller can report a creation that another caller performed.

The scenario in `mori://shinzui/keiro-runtime-kenshou/plans/8-cover-pgmq-hs-in-isolation` reconciled ten standard queues with notification throttles and FIFO indexes from eight concurrent callers, dropping the resources between fifty rounds. In the first run, all callers succeeded and the catalog converged to ten queues, but the combined reports contained eighty `CreatedQueue`, eighty `EnabledNotify`, and eighty `CreatedFifoIndex` actions per round, while each resource could be created only once. That run is `01a0cb0f-51a6-75cb-a822-e07dc52ecc71` on durable PostgreSQL 18.

A repeat run, `01a0cb13-f5ae-77cd-9b7f-e6e4e7c0f1b9`, found an additional failure: several callers received SQLSTATE `23505` for duplicate key `pg_class_relname_nsp_index` during `select from pgmq.create_fifo_index($1)`. The final catalog still converged. `CREATE INDEX IF NOT EXISTS` is not enough to make concurrent index creation safe against this catalog race, contrary to the current comment in `Pgmq.Config.Reconcile`.

## Requested Change

Serialize or safely retry the FIFO index creation race so concurrent reconcilers complete without SQLSTATE `23505`. Return action reports that describe the operation's actual effect under concurrent callers. The successful creator should report creation once per resource; callers that observe or lose a race to the created resource should report the matching `Skipped...` action. Preserve the report vocabulary where possible. Include topic bindings and partitioned queues in the concurrency audit, because they use the same snapshot-before-action pattern.

## Acceptance

1. Start eight independent clients at one barrier, each reconciling the same ten declarations on an empty migrated database. Repeat after dropping the resources for fifty rounds. No caller errors, the final catalog matches the declaration, and the combined reports contain exactly one creating action per resource per round.
2. A second reconciliation with no concurrent changes reports only `Skipped...` actions; changing a throttle reports one `UpdatedNotifyThrottle` and the remaining callers report the settled value.
3. Tests cover standard and partitioned queues, notification throttles, FIFO indexes, and topic bindings; the report remains truthful when a create operation returns successfully after another client won the race.

## Non-goals

This request does not change queue semantics or require callers to serialize their startup. It concerns the report's truthfulness when the database has already converged.
