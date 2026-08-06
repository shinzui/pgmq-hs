# Design Document 018: The pgmq-config reconciliation contract

## Status

**Adopted (2026-08-05)**, as part of
`docs/plans/18-report-reconciliation-truthfully-and-document-the-real-contract.md`
under `docs/masterplans/4-make-the-pgmq-config-reconciler-truthful-robust-and-single-sourced.md`.


## The contract

`ensureQueues` and `ensureQueuesReport` (`pgmq-config/src/Pgmq/Config.hs`, with
effect-backed twins in `pgmq-config/src/Pgmq/Config/Effectful.hs`) reconcile a declared
queue topology against a live database. The contract in one sentence: **create what is
missing, report what is not, and mutate existing state in exactly one documented case.**

Concretely, a run snapshots existing state with four read-only queries — queues, topic
bindings, notification throttles, and FIFO indexes — and then, per declared config:

- **Queue absent** → create it with the declared type; report `CreatedQueue`.
- **Queue present, shape matches** → report `SkippedQueue`; issue nothing.
- **Queue present, shape differs** → report `DetectedQueueTypeDrift`; issue nothing.
- **Throttle row absent** → enable notify with the declared interval; report
  `EnabledNotify`.
- **Throttle row present, interval matches** → report `SkippedNotify`; issue nothing.
- **Throttle row present, interval differs** → update it in place; report
  `UpdatedNotifyThrottle` with observed and declared values.
- **FIFO index declared and absent** → create it; report `CreatedFifoIndex`.
- **FIFO index declared and present** → report `SkippedFifoIndex`; issue nothing.
- **Binding absent / present** → bind and report `BoundTopic`, or report
  `SkippedTopicBinding`.

Queues, bindings, and throttles that exist in the database but are absent from the config
are never touched. Nothing is ever dropped, disabled, or converted.


## Why the throttle interval is mutated but the queue type is not

This is the asymmetry that needs justifying, because both are "declared value disagrees
with stored value".

The throttle interval has a non-destructive in-place update in pgmq's own API:
`pgmq.update_notify_insert(queue_name, interval_ms)` rewrites one column of one row in
`pgmq.notify_insert_throttle`. Nothing is lost. Meanwhile, a declared interval the
reconciler silently ignores is a straightforward lie: the whole point of putting the value
in the config is that changing it changes behavior. Before this change, editing
`withNotifyInsert (Just 500)` in application code and redeploying had no effect whatsoever
on a queue that already had a throttle row, and the report cheerfully said `SkippedNotify`.
That was the clearest documentation-versus-behavior gap the 2026-08 review found.

A queue's type has no such update. Converting between standard, unlogged, and partitioned
means dropping the queue and recreating it, which destroys every message it holds. A
process that runs automatically at application startup, potentially on several replicas at
once, must never do that — the failure mode is silent, unrecoverable data loss triggered by
a one-word config edit. So the drift is surfaced in the report, with both the declared and
observed shapes, and a human decides.

The general rule: **reconcile automatically only where the repair is non-destructive and
the API supports it in place; otherwise report and stop.**


## Deliberate non-checks

Partition interval and retention interval are not compared for partitioned queues.
`pgmq.list_queues()` reports only two booleans per queue (partitioned, unlogged), so
observing partition settings would mean querying pg_partman's `part_config` table — a
dependency surface this library does not open. `DetectedQueueTypeDrift` therefore compares
only the three-way shape, and a declared `PartitionedQueue` matches any observed
partitioned queue.

`ObservedQueueType` exists precisely to make this boundary visible in the type system: it
is what the database can tell us, as distinct from `QueueType`, which is what the config
asks for.


## Why `Nothing` compares equal to 250

`NotifyConfig`'s `throttleMs = Nothing` is documented as "use the pgmq default". The
enable path already realizes that as `coalesce($2, 250)` in the pgmq-hasql statement
(`pgmq-hasql/src/Pgmq/Hasql/Statements/QueueManagement.hs`), matching
`pgmq.enable_notify_insert`'s own `DEFAULT 250`. Drift comparison has to use the same
effective value, or a `Nothing` config would see a stored 250, call it drift, update it to
250, and do so on every single startup forever. `Pgmq.Config.Types.defaultThrottleMs` names
that constant so the three places cannot silently diverge.


## Why the FIFO check reads a catalog view

pgmq exposes no index-existence query. `pgmq.create_fifo_index` delegates to
`CREATE INDEX IF NOT EXISTS` and returns nothing, so a caller cannot distinguish "I created
it" from "it was already there". The reconciler used to report `CreatedFifoIndex`
unconditionally, which meant the report was false from the second run onward and
`SkippedFifoIndex` was unreachable dead code.

`Pgmq.Hasql.Sessions.listFifoIndexQueueNames` therefore reads `pg_indexes` directly for
indexes matching `q_<name>_fifo_idx` in the `pgmq` schema. This is the first pgmq-hasql
statement that touches a PostgreSQL catalog rather than a `pgmq.*` function. That is
acceptable: the repository's "do not hand-write SQL" rule governs pgmq-migration's vendored
schema, not client-side queries.

Renaming the action to `EnsuredFifoIndex` and documenting it as "applied unconditionally"
was considered and rejected. `ReconcileAction` was already changing incompatibly in this
release, so the honest version cost no extra breakage, and a truthful created-versus-skipped
distinction is the entire reason a report type exists.


## Concurrency: convergent, with one caveat

Every mutating call the reconciler issues is idempotent and convergent under sequential
retry. Under concurrent multi-replica startup:

- `pgmq.create` and `pgmq.create_partitioned` serialize on a per-queue advisory lock.
- `pgmq.bind_topic` upserts.
- `CREATE INDEX IF NOT EXISTS` has a narrow duplicate-name race that degrades to a no-op.
- `pgmq.enable_notify_insert` is the problem. On a stock upstream-1.11.0 **extension**
  install, two replicas enabling notify on the same brand-new queue can collide with
  SQLSTATE 42710 (`duplicate_object`), failing one replica's entire startup reconcile —
  measured at roughly a 28% collision rate over 400 concurrent calls. See
  `docs/design/015-notification-delivery-contract.md` for the mechanism.

Databases installed through this repository's `pgmq-migration` package are free of that
race: migration `0003-notify-crash-safety-and-locking.sql` adds
`PERFORM pgmq.acquire_queue_lock(queue_name)` as the first statement of
`enable_notify_insert`. For extension installs, the guidance is to retry the startup
reconcile — every operation converges — or to serialize reconciliation across replicas.

Wrapping the whole reconcile in one transaction was rejected. It would hold DDL locks
across the entire topology for the duration of startup, and it still would not serialize
one replica against another, so it buys nothing the retry guidance does not already give.
Swallowing 42710 inside the reconciler was likewise rejected: it would classify a genuine
duplicate-object error as success everywhere else on the same code path.

The reconciler is not transactional for the same reason. Each statement autocommits, so a
failure part-way leaves completed work in place and the next run resumes from there. That
is what makes retry the right recovery story.


## Foreign queues are out of scope, not fatal

The queue snapshot is read through `listQueuesUnvalidated`, which decodes names as plain
`Text`. A queue created by another client under a name `parseQueueName` rejects — pgmq's
server-side validator only checks length — is therefore simply a queue this reconciler does
not manage, rather than a decode failure that takes down application startup. See
`docs/design/016-queue-name-validation.md`.
