# Design Document 016: Queue-name validation and the mixed-case remediation

## Status

**Adopted (2026-08-05)**, as part of
`docs/plans/15-validate-queue-names-and-classify-transient-errors-across-the-pgmq-layers.md`.


## The contract

A queue name is non-empty, at most 47 characters, and drawn from lowercase ASCII letters,
digits, and underscore only — `[a-z0-9_]{1,47}`. `Pgmq.Types.parseQueueName` enforces
this, and because the `QueueName` constructor is unexported, every runtime entry path goes
through it: `parseQueueName` itself, and the hand-written `FromJSON` instance (which calls
`parseQueueName` via `Aeson.withText`). The `Lift` instance exists only for compile-time
splices of already-validated values. The 47-character bound is PostgreSQL's 63-character
identifier limit minus the longest name prefix the SQL layer constructs
(`archived_at_idx_`).

The `FromJSON` instance was previously newtype-derived and accepted any string of any
length, so configuration-loaded names bypassed validation entirely. Deriving `FromJSON` on
a smart-constructor newtype is the bug pattern; the same derived-instance bypass still
exists on `RoutingKey` and `TopicPattern` and is deliberately out of scope here (recorded
in plan 15's Decision Log as an adjacent hazard for a follow-up).


## Why lowercase-only is a correctness requirement

pgmq's SQL holds three views of a queue name that agree only for lowercase input:

1. **Physical table names are lowercased.** `pgmq.format_table_name` returns
   `lower(prefix || '_' || queue_name)`
   (`pgmq-migration/migrations/0001-install-v1.11.0.sql` line 244).
2. **Metadata stores the caller's original casing.** `pgmq.create` inserts the raw name
   into `pgmq.meta` (lines 1145-1151).
3. **The notification trigger extracts the lowercased name.** It fires on the physical
   table and derives the queue name from `TG_TABLE_NAME` (migration `0003`, previously
   install SQL line 1572).

Consequences, all demonstrated live by `pgmq-hasql/test/AliasingSpec.hs` against this
repository's own migration:

- `create('MyQueue')` then `create('myqueue')` yields ONE physical table (`q_myqueue`)
  with TWO `pgmq.meta` rows. Messages sent through either name interleave in the one
  table, and `drop_queue` on either name destroys the other's messages while the other's
  meta row lives on, pointing at nothing.
- `enable_notify_insert('MyQueue')` inserts a throttle row keyed `'MyQueue'`, but the
  trigger looks up `'myqueue'` and never matches it. Since migration `0003` the trigger
  fails open on a missing row, so notifications fire unthrottled and the configured
  interval is silently ignored (`last_notified_at` stays frozen at the epoch); before
  `0003`, the same mismatch silently suppressed every notification.
- `pgmq.acquire_queue_lock` hashes the RAW name, so `MyQueue` and `myqueue` do not even
  serialize against each other while mutating the same physical table.

Rejection was chosen over silent normalization. Normalizing would re-introduce aliasing
against pre-existing mixed-case metadata (a normalized `MyQueue` would silently join a
previously-created `myqueue`'s physical table while a `MyQueue` meta row still exists) and
would make `queueNameToText` disagree with what the caller wrote. Rejection is loud, at
the boundary, and matches the smart-constructor design the type already had.


## The upgrade consequence

`queueDecoder` (`pgmq-hasql/src/Pgmq/Hasql/Decoders.hs`) re-validates names read back
from the database through `parseQueueName` via `D.refine`. Under the stricter parser, a
database that still contains mixed-case rows in `pgmq.meta` fails `listQueues` decoding —
and therefore pgmq-config reconciliation — until those rows are remediated. Run the
remediation below against every deployed database **before** upgrading the packages. As of
2026-08-05 no registered consumer creates mixed-case names, but deployed databases must be
verified independently.


## Detection

Rows needing remediation, with the context an operator should record first:

```sql
SELECT m.queue_name,
       lower(m.queue_name) AS canonical_name,
       EXISTS (
         SELECT 1 FROM pgmq.meta lower_meta
         WHERE lower_meta.queue_name = lower(m.queue_name)
       ) AS has_lowercase_twin,
       (SELECT count(*) FROM pgmq.topic_bindings b
        WHERE b.queue_name = m.queue_name) AS binding_count,
       (SELECT throttle_interval_ms FROM pgmq.notify_insert_throttle n
        WHERE n.queue_name = m.queue_name) AS throttle_interval_ms
FROM pgmq.meta m
WHERE m.queue_name <> lower(m.queue_name);
```

Back up the returned metadata and the complete child rows (`pgmq.topic_bindings`,
`pgmq.notify_insert_throttle`) before changing anything.


## Remediation

Both foreign keys onto `pgmq.meta (queue_name)` — from `pgmq.topic_bindings` and from
`pgmq.notify_insert_throttle` — lack `ON UPDATE` and carry `ON DELETE CASCADE`. So a
naive `UPDATE pgmq.meta SET queue_name = lower(queue_name)` fails while children
reference the row, and a naive `DELETE` silently cascades away routing and notification
configuration. The remediation therefore creates or reuses the canonical (lowercase)
parent first, repoints the children, and only then deletes the mixed-case row — one
transaction per DO block, safe to rerun (after success the detection query returns no
rows, and the loop body never executes again).

```sql
DO $remediate$
DECLARE
  bad RECORD;
  twin_exists BOOLEAN;
BEGIN
  FOR bad IN
    SELECT m.queue_name AS mixed_name, lower(m.queue_name) AS canonical_name
    FROM pgmq.meta m
    WHERE m.queue_name <> lower(m.queue_name)
  LOOP
    -- Serialize against concurrent create/drop of either casing (the advisory
    -- lock hashes the raw name, so both are needed) and against a concurrent
    -- remediation.
    PERFORM pgmq.acquire_queue_lock(bad.mixed_name);
    PERFORM pgmq.acquire_queue_lock(bad.canonical_name);
    PERFORM 1 FROM pgmq.meta
      WHERE queue_name IN (bad.mixed_name, bad.canonical_name)
      FOR UPDATE;

    twin_exists := EXISTS (
      SELECT 1 FROM pgmq.meta WHERE queue_name = bad.canonical_name
    );

    IF NOT twin_exists THEN
      -- Rename in place: create the canonical parent (preserving metadata),
      -- so the children can be repointed under it.
      INSERT INTO pgmq.meta (queue_name, is_partitioned, is_unlogged, created_at)
      SELECT bad.canonical_name, m.is_partitioned, m.is_unlogged, m.created_at
      FROM pgmq.meta m WHERE m.queue_name = bad.mixed_name;
    ELSE
      -- The two rows already alias one physical table; the canonical parent
      -- exists. A mixed-case binding whose pattern the canonical queue already
      -- has is the same binding — drop it rather than collide with the
      -- (pattern, queue_name) unique constraint. Keep an existing canonical
      -- throttle configuration in preference to the mixed-case one.
      DELETE FROM pgmq.topic_bindings b
      WHERE b.queue_name = bad.mixed_name
        AND EXISTS (
          SELECT 1 FROM pgmq.topic_bindings t
          WHERE t.queue_name = bad.canonical_name AND t.pattern = b.pattern
        );
      DELETE FROM pgmq.notify_insert_throttle
      WHERE queue_name = bad.mixed_name
        AND EXISTS (
          SELECT 1 FROM pgmq.notify_insert_throttle
          WHERE queue_name = bad.canonical_name
        );
    END IF;

    -- Repoint the surviving children. UPDATE preserves bound_at on bindings
    -- and last_notified_at on the throttle row; the trigger on the physical
    -- table (already installed under the lowercased table name) starts
    -- matching the throttle row the moment it carries the canonical name.
    UPDATE pgmq.topic_bindings SET queue_name = bad.canonical_name
    WHERE queue_name = bad.mixed_name;
    UPDATE pgmq.notify_insert_throttle SET queue_name = bad.canonical_name
    WHERE queue_name = bad.mixed_name;

    -- The mixed-case row has no children left; the CASCADE has nothing to eat.
    DELETE FROM pgmq.meta WHERE queue_name = bad.mixed_name;
  END LOOP;
END
$remediate$;
```

No physical objects move: the queue table, archive table, and notification trigger were
created under the lowercased physical name all along. The remediation only makes the
metadata agree with them.

If the stricter parser must be rolled back after release (an unanticipated mixed-case
deployment surfaces), the safe path is this remediation, not a parser revert — record any
such event in plan 15's Decision Log.


## Where this is enforced

`pgmq-core/test/QueueNameSpec.hs` pins acceptance and rejection through both entry paths.
`pgmq-hasql/test/AliasingSpec.hs` documents, live against PostgreSQL, exactly what the
rejections prevent. `pgmq-hasql/test/MixedCaseRemediationSpec.hs` seeds mixed-case
metadata with topic bindings and a notification throttle, runs the DO block above, proves
that bindings (including `bound_at`) and throttle configuration survive under the
canonical name in both the twin and no-twin cases, and proves a second run changes
nothing. Both database-backed specs run on dedicated PostgreSQL instances because a
mixed-case meta row poisons `listQueues` decoding for every concurrent test.


## Related documents

- Design note 006 / 015: the notification channel and delivery contract the throttle
  mismatch corrupts.
- Design note 014: the NULL-parameter contract; the sibling rule that boundary values
  must mean what the documentation says they mean.
