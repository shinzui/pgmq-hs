# Design Document 015: The notification delivery contract

## Status

**Adopted (2026-08-05)**. Corrects the channel-name claim in design note 006. Applies to
`pgmq.notify_queue_listeners`, `pgmq.enable_notify_insert`, and every consumer that
LISTENs for pgmq insert notifications.


## The channel is a function, not a format string

```haskell
Pgmq.Types.notifyChannelName :: QueueName -> Text
notifyChannelName q = "pgmq.q_" <> T.toLower (queueNameToText q) <> ".INSERT"
```

The trigger raises `PG_NOTIFY('pgmq.' || TG_TABLE_NAME || '.' || TG_OP, NULL)`, where
`TG_TABLE_NAME` is the physical table — the `q_` prefix plus the queue name lowercased by
`pgmq.format_table_name`. Three details of that make hand-assembly a trap, and all three
were got wrong in this repository's own documentation until 2026-08-05, which claimed
`pgmq_<queue_name>` on every component:

- the separator is a dot, not an underscore;
- the physical `q_` prefix is part of the name;
- the trigger operation is appended, so the channel ends in `.INSERT`.

Because the name contains dots, it is not a bare identifier. LISTEN requires it
double-quoted:

```sql
LISTEN "pgmq.q_myqueue.INSERT";
```

The helper lives in `pgmq-core` rather than `pgmq-hasql` because a LISTEN consumer needs a
raw connection anyway and may not depend on hasql at all; `pgmq-core` is the family's
dependency floor and already owns `QueueName`. The `toLower` mirrors the SQL lowercasing
and stays as defence even once queue-name validation rejects uppercase names.

`pgmq-hasql/test/NotifyChannelSpec.hs` pins the contract from both sides: a real
notification arrives byte-equal to the helper's output, and a listener subscribed to the
old documented name receives nothing.


## Delivery is best-effort; the throttle is not delivery

Two independent facts about pgmq notifications, and one rule that follows from them.

**PostgreSQL NOTIFY is fire-and-forget.** Notifications are not queued for disconnected
listeners. A consumer that reconnects has no way to learn what it missed.

**The throttle deliberately drops notifications.** `enable_notify_insert` with a non-zero
`throttle_interval_ms` suppresses notifications inside the interval by design. A listener
is told *that* messages exist, never *how many*.

**Therefore every LISTEN consumer must keep a poll fallback.** LISTEN is a latency
optimisation over polling; it is not a delivery mechanism, and no change to this library
can make it one.


## Losing throttle state must not stop delivery

`pgmq.notify_insert_throttle` is `UNLOGGED`. That is deliberate: the trigger updates
`last_notified_at` on every notification it lets through, and for a `throttle_interval_ms = 0`
queue that is every single insert. Making the table logged would WAL-log a row update per
message — a permanent write-amplification cost to defend a rare path.

The consequence is that PostgreSQL truncates the table during crash recovery (documented,
intended behaviour — it is what makes unlogged tables fast). Before migration 0003 the
trigger notified only when its throttle `UPDATE` matched a row, so after a crash it fired,
matched nothing, and **silently never notified again**. Sends succeeded, messages
accumulated, listeners starved. Only an application restart healed it, because
`ensureQueues` reads the same truncated table and re-enables what is missing.

The rule adopted here:

> A trigger whose bookkeeping state is missing must fail **open**. Losing the throttle
> interval in a crash is acceptable data loss; losing deliveries silently is not.

Migration 0003 implements it by disambiguating the two causes of a zero-row update:

```sql
  IF updated_count > 0 THEN
    PERFORM PG_NOTIFY(...);
  ELSIF NOT EXISTS (
    SELECT 1 FROM pgmq.notify_insert_throttle nit
    WHERE nit.queue_name = queue_name_extracted
  ) THEN
    PERFORM PG_NOTIFY(...);
  END IF;
```

Row present but throttled: suppress, which is correct. Row absent: notify. The `NOT EXISTS`
probe runs only when the `UPDATE` matched no row, so a delivered notification costs nothing
extra; a throttle-suppressed insert pays one additional index probe.

The trigger deliberately does **not** re-insert the row. The configured interval is gone —
that is the crash's data loss — and inventing one in the hot path would silently change
throttling for the queue. The degradation is bounded and self-correcting: unthrottled
notifications until the next reconcile restores the configured value.

One sharp edge inherited from the queue-name aliasing family (design note 016):
`enable_notify_insert` stores the throttle row under the caller's original casing, while
the trigger looks the name up lowercased from `TG_TABLE_NAME`. A row keyed `'MyQueue'` is
therefore invisible to the trigger — missing *permanently*, not crash-missing — and
fail-open converts that mismatch from "never notify" (the pre-0003 behaviour) into
"notify unthrottled on every insert", which no re-enable under the same mixed-case name
can heal. This is unreachable through pgmq-hs, whose `parseQueueName` rejects the casing
at every entry path, but remains open to any non-Haskell caller; the detection and
remediation are design note 016's.

`pgmq-config/test/NotifyCrashSpec.hs` drives the real cycle: start PostgreSQL, enable
notify, SIGQUIT it, restart on the same data directory, and prove that a post-recovery send
still reaches a LISTENing client.

One trap for anyone writing a similar test: `ephemeral-pg` runs PostgreSQL with `fsync`,
`synchronous_commit`, and `full_page_writes` off, so an immediate shutdown discards every
commit still in the WAL buffers — including the schema install itself. Issue a `CHECKPOINT`
before the crash. It flushes WAL and dirty buffers to the OS, which survives a process
kill, and it does not make unlogged tables crash-safe, so the behaviour under test is
preserved exactly.


## Reconciling the same queue from two replicas is convergent

`enable_notify_insert` internally disables and re-creates: `DROP TRIGGER IF EXISTS`, upsert
the throttle row, `CREATE CONSTRAINT TRIGGER`. On a fresh queue the drop finds nothing and
takes no lock, so two replicas can both pass it; the loser then blocks on the throttle
row's unique constraint until the winner commits, resumes, and creates a trigger that now
exists — SQLSTATE 42710, failing that replica's entire startup reconcile. Measured at
roughly a 28% collision rate over 400 concurrent calls.

The fix is `PERFORM pgmq.acquire_queue_lock(queue_name)` as the first statement, matching
what `pgmq.create` and `pgmq.create_partitioned` already do. Serializing the whole function
makes concurrent callers convergent — the second drops and re-creates the first's identical
trigger, an idempotent no-op in effect — with no error-code matching anywhere.

Swallowing 42710 in the reconciler was rejected: it would treat a real duplicate-object
error as success everywhere else in the same code path.

Accepted and documented side effect: a concurrent second enable resets `last_notified_at`
to the epoch, exactly as a sequential re-enable does.

The same reasoning applies to `pgmq.create_partitioned`, but locking is not sufficient
there. The second replica's `CREATE TABLE IF NOT EXISTS` is a no-op while
`partman.create_parent` rejects an already-managed parent, so the guard is an idempotence
probe against `part_config`, not a lock. Idempotence and mutual exclusion are different
properties; a re-entrant function needs both.

PGMQ 1.13 replaces the three-argument partition function with a four-argument function
whose optional `premake` defaults to 4. Migration 0006 restores both registration guards
after the pristine upstream upgrade in 0005; it retains advisory locking, premake forwarding
and `GENERATED BY DEFAULT` identities. It does not recreate a three-argument overload.
Required pg_partman tests verify concurrent callers finish successfully and recovery keeps
message IDs and payloads. Finish the full migration suffix before starting queue-creation
traffic. See [the compatibility ADR](../adr/pgmq-1.12-1.13-compatibility.md).


## Related documents

- Design note 006: the original notification design. Its channel-name claim is corrected
  in place.
- Design note 014: the NULL-parameter contract. Migration 0003 also adds the server-side
  `COALESCE(throttle_interval_ms, 250)` that note calls for, for non-Haskell callers.
- Design note 016: queue-name validation and the mixed-case remediation, including the
  aliased throttle key that fail-open turns into permanent unthrottled notification.
