---
id: 23
slug: gate-the-notification-fail-open-on-a-real-queue-row-and-state-the-partitioned-queue-contract
title: "Gate the notification fail-open on a real queue row and state the partitioned-queue contract"
kind: exec-plan
created_at: 2026-09-16T20:42:28Z
intention: "intention_01m2nz9a82ejh91yg9sk7t6a7a"
master_plan: "docs/masterplans/6-close-the-notification-reconciler-and-evidence-gaps-surfaced-by-the-0-6-1-0-review.md"
provenance:
  created_by:
    model: "claude-fable-5-1"
    harness: "claude-code"
    at: 2026-09-16T20:42:28Z
  revisions:
    - model: "gpt-6-astra"
      harness: "codex-cli"
      at: 2026-10-25T20:33:49Z
      mode: "update"
      note: "Corrected partition identity guard, alias and ordinary-queue regression coverage, recovery wording, and serial test invocation."
    - model: "gpt-6-astra"
      harness: "codex-cli"
      at: 2026-10-26T03:51:08Z
      mode: "update"
      note: "Add populated six-entry upgrade proof without trigger recreation and explicit native test selection."
  reviews:
    - model: "gpt-6-astra"
      harness: "codex-cli"
      at: 2026-10-25T20:33:49Z
      verdict: "approved"
      note: "Reviewed revised planning document; implementation evidence remains pending. Corrected partition identity guard, alias and ordinary-queue regression coverage, recovery wording, and serial test invocation."
---

# Gate the notification fail-open on a real queue row and state the partitioned-queue contract

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.
If durable project context changes, update or create ADRs in docs/adr/ in the same change.


## Purpose / Big Picture

pgmq is a message queue implemented as PostgreSQL tables and functions. This repository ships
a Haskell client for it and, in the `pgmq-migration` package, a way to install pgmq's SQL
without the PostgreSQL extension, as an ordered ledger of migration files under
`pgmq-migration/migrations/`. One of those files, `0003-notify-crash-safety-and-locking.sql`,
replaces pgmq's insert-notification trigger function with a version that "fails open": when
the trigger cannot find the queue's throttle row, it notifies anyway, so that a server crash
(which truncates the `UNLOGGED` throttle table) cannot silently stop notifications.

That change has a defect on partitioned queues. PostgreSQL clones a row-level trigger onto
every leaf partition, and inside the clone `TG_TABLE_NAME` is the partition's name
(`q_pq_p0`), not the queue table's (`q_pq`). The trigger derives the queue name by dropping
the first two characters, so it looks for a throttle row named `pq_p0`, which normally does not exist.
Upstream's trigger then does nothing; ours fails open. The result today, on any database
installed through `pgmq-migration`, is that a partitioned queue with notifications enabled
publishes one `NOTIFY` per insert, unthrottled, on a channel named after whichever partition
the row landed in. No listener on the documented channel ever hears it, and every inserting
transaction pays PostgreSQL's notification cost at commit (notifying transactions serialize
on a database-wide lock).

After this plan, a new migration `0007` gates the fail-open branch on the extracted name
having a row in `pgmq.meta`, the table that lists every queue. A partitioned queue's leaf
names normally do not, but another client can register one as a queue alias. Therefore
`0007` first returns for `pg_class.relispartition` at `TG_RELID`, before the throttle UPDATE.
The native contract is silence even when an alias has metadata or a throttle row; normal
stock schemas are silent too, but stock SQL is not hardened against manually aliased leaves.
Ordinary queues keep crash recovery. A test proves that no notification arrives on any
channel when a partitioned queue receives inserts, on both the native ledger and the stock
1.12 schema, and the existing crash-recovery and channel-name tests still pass. Every
document that describes notification delivery says that partitioned queues receive no insert
notifications, and that the post-crash unthrottled state on ordinary queues lasts until an
explicit reconciliation run or enable call restores the row. Reconciliation commonly runs
at application startup, but neither API requires a process restart.

You can see it working by running the new test in the `partman` shell, which fails on the
current ledger and passes after `0007` is appended, and by repeating the psql transcript in
Context and Orientation, which shows three notifications before and none after.


## Progress

- [ ] M1: compiling tests reproduce partition notifications, including native leaf-alias paths, with a working listener control.
- [ ] M2: immutable-ledger migration 0007 fixes fresh and populated six-entry installs without trigger recreation; ordinary throttling, post-crash delivery, and reconciliation recovery remain correct.
- [ ] M3: delivery docs, Haddocks, capability evidence, ADR verification, and all affected Unreleased entries agree; documentation checks pass.


## Surprises & Discoveries

Document unexpected behaviors, bugs, optimizations, or insights discovered during
implementation. Provide concise evidence.

The 2026-10-25 review found that queue metadata does not establish physical relation shape:
`pgmq.create` uses `CREATE TABLE IF NOT EXISTS` before registering the supplied name, so a
leaf name can acquire alias metadata. A throttle row for that alias would also bypass an
ELSIF-only fix. The revised migration tests physical partition membership before UPDATE;
the new isolated cases must demonstrate the counterexamples and correction.

The pre-implementation refresh found that `testPartitions` applies only the historical
six-entry prefix. Fresh-install behavior and catalog convergence alone would leave already
installed notification triggers untested; M2 now covers that transition explicitly.


## Decision Log

- Decision (2026-10-25, supersedes the 2026-09-16 membership-only design): return for
  actual partition relations before UPDATE, then require queue metadata for ordinary
  missing-row fail-open. Do not resolve a parent to deliver notifications.
  Rationale: metadata aliases invalidate the old assumption that a leaf name cannot be
  registered; a successful throttle UPDATE also bypasses a fail-open-only guard. The
  `pg_class` lookup adds work on every invocation, not just the zero-update path. This is
  accepted for correctness within the existing override lineage; delivery on the parent
  channel remains separate upstream work in `mori://pgmq/pgmq`.
- Decision: The new test lives in `pgmq-hasql`'s suite, not `pgmq-migration`'s.
  Rationale: it needs `LISTEN` through libpq (`postgresql-libpq` is already a `pgmq-hasql`
  test dependency and the listener helper exists in `NotifyChannelSpec.hs`), a partitioned
  queue (the suite already creates the `pg_partman` extension when available), and it must
  run against both the native ledger and the stock 1.12 fixture, which only the client suites
  select via `PGMQ_TEST_SCHEMA_VERSION`. The migration suite pins the ledger's shape and
  digests; behavior of the trigger is a client-visible contract.
  Date: 2026-09-16
- Decision: Migration `0007` re-creates only `pgmq.notify_queue_listeners()`.
  Rationale: `enable_notify_insert` and `create_partitioned` in `0003` and `0006` are not part
  of the defect; the master plan excludes auditing them. Keeping `0007` to one function keeps
  the catalog-convergence exception list unchanged (that body is already excepted).
  Date: 2026-09-16


## Outcomes & Retrospective

Summarize outcomes, gaps, and lessons learned at major milestones or at completion.
Compare the result against the original purpose. Before marking the plan complete,
distill durable project context from the Decision Log, Surprises & Discoveries, and
this section into docs/adr/. Keep task-local execution details here.

(To be filled during and after implementation.)


## Context and Orientation

### The repository in one paragraph

`pgmq-hs` is a multi-package Cabal project: `pgmq-core` (types, including `QueueName` and
`notifyChannelName` in `pgmq-core/src/Pgmq/Types.hs`), `pgmq-hasql` (SQL statements and
sessions over the hasql driver; the umbrella module is `pgmq-hasql/src/Pgmq.hs`),
`pgmq-effectful` (an effect wrapper over the sessions), `pgmq-config` (a startup reconciler),
and `pgmq-migration` (the extension-free installer). The toolchain comes from Nix:
`nix develop` gives GHC 9.12 and cabal; `nix develop .#partman` is the same shell with the
`pg_partman` PostgreSQL extension available to test servers and `PGMQ_REQUIRE_PARTMAN=1`
exported, so partition tests fail instead of skipping. Tests start disposable PostgreSQL
servers through the `ephemeral-pg` library; no external database is needed. Run `nix fmt`
before committing (a pre-commit hook enforces formatting) and `git diff --check` for
whitespace. `just docs-check` validates the documentation bundles.

### How the native ledger works

`pgmq-migration/migrations/manifest` lists migration files in order:

```text
0001-install-v1.11.0.sql
0002-schema-management-comment.sql
0003-notify-crash-safety-and-locking.sql
0004-upgrade-v1.12.0.sql
0005-upgrade-v1.13.0.sql
0006-preserve-partitioned-reentry-v1.13.0.sql
```

`pgmq-migration/src/Pgmq/Migration/Internal/Definition.hs` embeds every file the manifest
names at compile time (`embedMigrationManifest "migrations/manifest"`) and exposes them as one
pg-migrate component named `pgmq`. The SQL runner applies each file in its own transaction and
records it, so a file is applied exactly once per database. Files are never edited after they
ship: `pgmq-migration/test/Main.hs` pins the MD5 of the three historical local files in
`testNativePayload`, compares `0004` and `0005` byte-for-byte to the vendored upstream upgrade
scripts, and `testNativeComponent` spells out the ledger as a list. Most ledger expectations in
that suite derive from `nativeMigrationNames`. `testPartitions` deliberately installs through
`0006` twice to test the historical 1.13 partition upgrade and re-entry; retain those
checkpoints. They do not exercise notification triggers across a `0006` to `0007` upgrade.
There is also a positional count in `testConvergence`, which applies
`take (if latest then 6 else 4)` migrations before comparing
the catalog to fresh upstream SQL; that `6` must become the ledger length when `0007` is
appended. The convergence comparison excepts exactly three function bodies from the
byte-for-byte catalog comparison:

```haskell
  let exceptions =
        [ "body:notify_queue_listeners()",
          "body:enable_notify_insert(text, integer)",
          if latest then "body:create_partitioned(text, text, text, integer)" else "body:create_partitioned(text, text, text)"
        ]
```

Because `notify_queue_listeners()` is already excepted, replacing its body again in `0007`
needs no change to that list. The vendored upstream SQL is under `vendor/pgmq/` (a git subtree
pinned to pgmq v1.13.0) and must never be edited.

### The trigger today

Migration `0003` defines the function that runs after every insert into a queue table:

```sql
CREATE OR REPLACE FUNCTION pgmq.notify_queue_listeners()
RETURNS TRIGGER AS $$
DECLARE
  queue_name_extracted TEXT; -- Queue name extracted from trigger table name
  updated_count        INTEGER; -- Number of rows updated (0 or 1)
BEGIN
  queue_name_extracted := substring(TG_TABLE_NAME from 3);

  UPDATE pgmq.notify_insert_throttle
  SET last_notified_at = clock_timestamp()
  WHERE queue_name = queue_name_extracted
    AND (
      throttle_interval_ms = 0 -- No throttling configured
          OR clock_timestamp() - last_notified_at >=
             (throttle_interval_ms * INTERVAL '1 millisecond') -- Throttle interval has elapsed
    );

  -- Check how many rows were updated (will be 0 or 1)
  GET DIAGNOSTICS updated_count = ROW_COUNT;

  IF updated_count > 0 THEN
    PERFORM PG_NOTIFY('pgmq.' || TG_TABLE_NAME || '.' || TG_OP, NULL);
  ELSIF NOT EXISTS (
    SELECT 1 FROM pgmq.notify_insert_throttle nit
    WHERE nit.queue_name = queue_name_extracted
  ) THEN
    -- Fail open: the trigger exists but its throttle row does not. ...
    PERFORM PG_NOTIFY('pgmq.' || TG_TABLE_NAME || '.' || TG_OP, NULL);
  END IF;

RETURN NEW;
END;
$$ LANGUAGE plpgsql;
```

`pgmq.enable_notify_insert(queue_name, throttle_interval_ms)` inserts a row into
`pgmq.notify_insert_throttle` keyed by the queue name as the caller wrote it and creates the
trigger `trigger_notify_queue_insert_listeners` on `pgmq.q_<name>`. Upstream's version of the
function (in `vendor/pgmq/pgmq-extension/sql/pgmq.sql`, unchanged from 1.11.0 through 1.13.0)
is identical except that it has no `ELSIF` branch. `pgmq.meta` is the queue registry: one row
per queue with `queue_name`, `is_partitioned`, `is_unlogged`, `created_at`; `queue_name` is
unique. The channel a listener must subscribe to is computed by
`Pgmq.Types.notifyChannelName`, which returns `pgmq.q_<lowercased name>.INSERT`.

### The defect, reproduced

The reviewer ran this against a scratch server in the `partman` shell after applying
`0001` through `0006` and creating the extension `pg_partman`. The transcript is the acceptance
baseline for this plan; you can repeat it with any PostgreSQL 17 or 18 that has pg_partman.

```sql
SELECT pgmq.create_partitioned('pq', '10', '100');
SELECT pgmq.enable_notify_insert('pq', 250);
SELECT c.relname, t.tgname, t.tgparentid
FROM pg_trigger t JOIN pg_class c ON c.oid = t.tgrelid
WHERE t.tgname = 'trigger_notify_queue_insert_listeners' ORDER BY 1;
```

```text
   relname    |                tgname                 | tgparentid
--------------+---------------------------------------+------------
 q_pq         | trigger_notify_queue_insert_listeners |          0
 q_pq_default | trigger_notify_queue_insert_listeners |      16908
 q_pq_p0      | trigger_notify_queue_insert_listeners |      16908
 q_pq_p10     | trigger_notify_queue_insert_listeners |      16908
 ...
```

The trigger was cloned onto every partition (`tgparentid` names the parent trigger). Then, in
one psql session, `LISTEN` on the documented channel and on every partition's channel, send
three messages, and flush:

```sql
LISTEN "pgmq.q_pq.INSERT";
LISTEN "pgmq.q_pq_p0.INSERT"; LISTEN "pgmq.q_pq_p10.INSERT"; -- one per partition
SELECT pgmq.send('pq', '{"a":1}'::jsonb);
SELECT pgmq.send('pq', '{"a":2}'::jsonb);
SELECT pg_sleep(0.3);
SELECT pgmq.send('pq', '{"a":3}'::jsonb);
SELECT 1 AS flush;
SELECT * FROM pgmq.notify_insert_throttle;
```

```text
Asynchronous notification "pgmq.q_pq_p0.INSERT" received from server process with PID 98486.
Asynchronous notification "pgmq.q_pq_p0.INSERT" received from server process with PID 98486.
Asynchronous notification "pgmq.q_pq_p0.INSERT" received from server process with PID 98486.
 queue_name | throttle_interval_ms |    last_notified_at
------------+----------------------+------------------------
 pq         |                  250 | 1969-12-31 16:00:00-08
```

Three inserts, three notifications, no throttling (the two sends inside 250 ms both
notified), all on a partition channel, and the throttle row untouched. The review's two-send ordinary-queue control produced one notification on `pgmq.q_oq.INSERT` and advanced
`last_notified_at`. In a plain-PostgreSQL check with no pgmq at all, a row trigger on a
partitioned parent reported `TG_TABLE_NAME=parent_p1` for a row inserted through the parent,
which is the mechanism.

Before `0003`, upstream's trigger on a partitioned queue matched zero rows and did nothing; the
regression is that `0003`'s fail-open cannot tell "row truncated by a crash" from "row that can
never exist". Design note 015 already describes the second case for a mixed-case throttle row
(keyed `MyQueue`, invisible to a trigger that lowercases) and calls it "missing permanently,
not crash-missing"; leaf partitions are the same class, and unlike the mixed-case case they are
reachable through this library's own `partitionedQueue ... & withNotifyInsert`.

### The documents that state the contract

`docs/design/015-notification-delivery-contract.md` is the canonical statement of delivery:
channel name, throttle, poll-fallback requirement, crash fail-open, and the note that the
degradation lasts "until the next reconcile restores the configured value". It does not say
that recovery needs an explicit enable/reconcile invocation: startup-only callers can remain
unthrottled until restart, while other callers can restore the row without restarting. `docs/design/006-queue-notifications.md`
is the original design note with the corrected channel derivation. `docs/capabilities/insert-notifications.md`
is the `CAP-4` capability record in the `docs/capabilities` OKF bundle; its `evidence` list
names the tests that prove the capability and its Limits section lists known boundaries.
`docs/user/schema-migration.md` describes the ledger for users, with a table of the six
migrations and a sample of applied names. The Haddocks on `notifyChannelName`
(`pgmq-core/src/Pgmq/Types.hs`), on the `enableNotifyInsert` statement
(`pgmq-hasql/src/Pgmq/Hasql/Statements/QueueManagement.hs`), session
(`pgmq-hasql/src/Pgmq/Hasql/Sessions.hs`), and effect (`pgmq-effectful/src/Pgmq/Effectful/Effect.hs`)
describe the channel and the poll fallback and say nothing about partitioned queues.

### ADR context

There is no profiled `docs/adr` OKF bundle; ADRs are plain Markdown files under `docs/adr/`
with Status, Context, Decision, Consequences, and Alternatives headings (see
`docs/adr/pgmq-1.12-1.13-compatibility.md` for the shape). The following records are relevant.

[docs/adr/fifo-native-overrides-and-index-upgrade-boundary.md](../adr/fifo-native-overrides-and-index-upgrade-boundary.md)
prohibits new copied upstream function bodies *for FIFO work* and states that "historical
notification/partition overrides are not removed or extended by this initiative". This plan
is that separate work: it does not open a new override, it corrects the one `0003` opened, in
an append-only file. The
[notification boundary ADR](../adr/notification-override-lineage-and-partitioned-queue-boundary.md)
records the strengthened guard and its native/stock scope; implementation adds acceptance
evidence to it.

[docs/adr/pgmq-1.12-1.13-compatibility.md](../adr/pgmq-1.12-1.13-compatibility.md) sets the
rules the ledger follows: existing bytes and order never change; local deviations are appended
after pristine upstream upgrades; their bodies are excepted from catalog convergence but their
behavior is tested; one transaction per migration. Its verification paragraph records
acceptance on PostgreSQL 17.10 with pg_partman 5.4.3 (the original review observed a later server;
EP-3 records the actual runtime version for the new candidate).

The parent MasterPlan is
[docs/masterplans/6-close-the-notification-reconciler-and-evidence-gaps-surfaced-by-the-0-6-1-0-review.md](../masterplans/6-close-the-notification-reconciler-and-evidence-gaps-surfaced-by-the-0-6-1-0-review.md).
The sibling plan `docs/plans/24-report-name-collisions-and-unsupported-notifications-instead-of-acting-on-them.md`
makes the reconciler report a notification declared on a partitioned queue; it links to the
sentence this plan adds to design note 015 and does not touch SQL.


## Plan of Work

### Milestone 1: a red test that shows the storm

Scope: add `pgmq-hasql/test/PartitionedNotifySpec.hs`, a test that creates a partitioned
queue, enables notifications, listens on the documented channel and on every partition's
channel, sends three messages, and asserts that nothing arrives within one second. At the end
of this milestone the test exists, is registered in `pgmq-hasql/test/Main.hs`, and fails on the
current native ledger with a message naming the partition channel that received a
notification. It must be skipped (with a printed `SKIPPED` line) when `pg_partman` is not
installed and fail when `PGMQ_REQUIRE_PARTMAN=1` is set and `pg_partman` is missing, exactly
as `pgmq-config/test/PartitionSpec.hs`'s `withPartman` does.

Model the module on `pgmq-hasql/test/NotifyChannelSpec.hs`, which already has `withListener`
(opens a raw libpq connection to the test server's connection string, issues `LISTEN`, runs an
action, closes), `awaitNotify` (polls `LibPQ.notifies` for a bounded number of 100 ms ticks),
`sendProbe`, and `enableNotify`. Generalize the listener to take a list of channels (issue one
`LISTEN` per channel, each name double-quoted because it contains dots). Get the partition
channel names from the catalog after creating the queue:

```sql
SELECT 'pgmq.' || c.relname || '.INSERT'
FROM pg_inherits i JOIN pg_class c ON c.oid = i.inhrelid
WHERE i.inhparent = ('pgmq.q_' || $1)::regclass
```

Create the queue with `Sessions.createPartitionedQueue (StmtTypes.CreatePartitionedQueue qn "10" "100")`
(numeric interval so the first rows land in `q_<qn>_p0`), enable with
`StmtTypes.EnableNotifyInsert qn (Just 0)` (zero means never throttle, so any notification that
the trigger *would* send is sent), send three messages with `Sessions.sendMessage`, then call
`awaitNotify conn 10` (ten ticks, one second). The assertion is that the result is `Nothing`;
on failure, report the channel that received a notification. Also assert, as a second test case
in the same module, that after the sends the throttle row's `last_notified_at` is still the
epoch (`to_timestamp(0)`), which documents that the throttle path never matched. Clean up the
queue with `Sessions.dropQueue` at the end of each case.

Use a fresh database/unique queue per case and bracket cleanup so assertion failures do
not leak fixtures. Do not reuse `NotifyChannelSpec.enableNotify` unchanged: it calls
`createQueue` and would register a partitioned fixture as an ordinary queue. Enable the
already-created partitioned queue directly. Prove the listener works with a separate ordinary
control queue on the same connection, drain its notification, then begin the silence window;
check libpq input/status failures rather than treating a broken listener as silence. Send in
separate committed sessions and assert that the messages were actually stored.

Add native-only alias cases in an isolated database: after creating `pq`, register `pq_p0`
using raw `pgmq.create('pq_p0')` and confirm that the physical table remains a partition.
Test first with only this metadata alias, then with a zero-interval throttle row for `pq_p0`
seeded directly (do not replace the cloned trigger). Parent inserts must notify neither
channel; in the latter case neither parent nor alias timestamp may advance. These cases
catch both a membership-only fail-open guard and a guard placed after UPDATE. Discard the
isolated database as a whole; do not drop the alias and thereby destroy the parent's leaf.
Stock 1.12 runs the normal partition cases, not this native hardening expectation.

Register the module in `pgmq-hasql/test/Main.hs` next to `NotifyChannelSpec.tests pool db`, and
add it to the `other-modules` of the `pgmq-hasql-test` suite in `pgmq-hasql/pgmq-hasql.cabal`.

Run it in the `partman` shell. Expected: the first case fails with a message such as
`expected no notification on any channel, but "pgmq.q_test_queue_..._p0.INSERT" received one`.
That is the red state. Commit it with a message that says the test reproduces the defect.

### Milestone 2: migration 0007 and the ledger

Scope: append the migration, update the ledger-derived expectations, and turn the test green
on both schema selections without changing any other test's outcome.

Create `pgmq-migration/migrations/0007-notify-only-registered-queues.sql` with this content
(the header comment is part of the file; the function body adds an early partition-relation return and changes the
`ELSIF` condition):

```sql
-- Notification fail-open only for a registered queue.
--
-- Migration 0003 made pgmq.notify_queue_listeners() fail open when its throttle
-- row is missing, so crash recovery truncating the UNLOGGED throttle table
-- cannot silently stop delivery. That branch could not tell a row lost to a
-- crash from a missing leaf-name row. PostgreSQL clones a row trigger onto
-- every leaf partition, and inside the clone TG_TABLE_NAME is the partition
-- (q_pq_p0), so the extracted name (pq_p0) normally has no throttle row. On a
-- partitioned queue the fail-open therefore fired on every insert, unthrottled,
-- on a per-partition channel no listener on the documented channel receives.
--
-- The fail-open now requires the extracted name to be a queue registered in
-- pgmq.meta. A relation guard before UPDATE rejects leaf partitions even
-- when their extracted names have alias metadata or throttle rows. Ordinary
-- queues keep crash recovery; a leaf partition, or a
-- mixed-case throttle row the trigger cannot match, behaves as upstream's
-- trigger does: no notification. Partitioned queues receive no insert
-- notifications on the hardened native install. Stock SQL is silent for
-- normal partitioned queues but does not guard manually aliased leaves.
--
-- See docs/design/015-notification-delivery-contract.md and
-- docs/adr/notification-override-lineage-and-partitioned-queue-boundary.md.

CREATE OR REPLACE FUNCTION pgmq.notify_queue_listeners()
RETURNS TRIGGER AS $$
DECLARE
  queue_name_extracted TEXT; -- Queue name extracted from trigger table name
  updated_count        INTEGER; -- Number of rows updated (0 or 1)
BEGIN
  -- Metadata names do not prove that this is an ordinary queue relation.
  IF EXISTS (
    SELECT 1 FROM pg_catalog.pg_class c
    WHERE c.oid = TG_RELID AND c.relispartition
  ) THEN
    RETURN NEW;
  END IF;

  queue_name_extracted := substring(TG_TABLE_NAME from 3);

  UPDATE pgmq.notify_insert_throttle
  SET last_notified_at = clock_timestamp()
  WHERE queue_name = queue_name_extracted
    AND (
      throttle_interval_ms = 0 -- No throttling configured
          OR clock_timestamp() - last_notified_at >=
             (throttle_interval_ms * INTERVAL '1 millisecond') -- Throttle interval has elapsed
    );

  -- Check how many rows were updated (will be 0 or 1)
  GET DIAGNOSTICS updated_count = ROW_COUNT;

  IF updated_count > 0 THEN
    PERFORM PG_NOTIFY('pgmq.' || TG_TABLE_NAME || '.' || TG_OP, NULL);
  ELSIF EXISTS (
      SELECT 1 FROM pgmq.meta m
      WHERE m.queue_name = queue_name_extracted
    )
    AND NOT EXISTS (
      SELECT 1 FROM pgmq.notify_insert_throttle nit
      WHERE nit.queue_name = queue_name_extracted
    ) THEN
    -- Fail open only for a queue that could have had a throttle row: the
    -- extracted name is registered in pgmq.meta, so a missing throttle row
    -- can be crash truncation. Both
    -- probes are unique-index lookups and run only when the UPDATE matched
    -- nothing. The row is deliberately NOT re-inserted here (see 0003): the
    -- configured interval is the crash's data loss, and the reconciler
    -- restores it when explicitly invoked, typically at application startup.
    PERFORM PG_NOTIFY('pgmq.' || TG_TABLE_NAME || '.' || TG_OP, NULL);
  END IF;

RETURN NEW;
END;
$$ LANGUAGE plpgsql;
```

Append `0007-notify-only-registered-queues.sql` as the last line of
`pgmq-migration/migrations/manifest`. The `extra-source-files` glob `migrations/*.sql` in
`pgmq-migration/pgmq-migration.cabal` already covers the new file; the recompile plugin on
`Definition.hs` picks up the manifest change on the next build.

In `pgmq-migration/test/Main.hs`: add `"0007-notify-only-registered-queues"` to the list in
`testNativeComponent`; add the new file's MD5 to the `testNativePayload` list (compute it with
`md5 -q pgmq-migration/migrations/0007-notify-only-registered-queues.sql` on macOS or `md5sum`
on Linux, after `nix fmt` has run so the bytes are final); and in `testConvergence` replace
`take (if latest then 6 else 4) names` with `take (if latest then length names else 4) names`
so the "latest" checkpoint applies the whole ledger and no future append has to edit this
line again (the `4` is the 1.12 checkpoint, which is a genuine position in history). Leave the
`exceptions` list alone.

Update `docs/user/schema-migration.md`: "six migrations" becomes "seven migrations"; add a
table row `| \`0007-notify-only-registered-queues\` | Local notification fail-open restricted to queues registered in \`pgmq.meta\`, restoring upstream's silence on partitioned queues |`;
add `0007-notify-only-registered-queues AppliedNow` to the sample applied-names output after the
`0006` line.

Prove the deployed upgrade path in a new serial case in
`pgmq-migration/test/Main.hs`, using its existing `installPrefix settings 6` helper.
The client fixture installs the latest ledger before tests run, and the public
`Pgmq.Migration` API does not expose historical payloads for a client test to select.
Keep prefix construction beside the packaged SQL files rather than introducing a public
API or reading sibling-package sources. Start from the suite's clean database, install
pg_partman using the existing required/skip policy, and bracket cleanup of the fixture.
Add `postgresql-libpq` to this suite's test dependencies, reusing the dependency already
used by hasql/config tests without changing the project's resolution or third-party pins.
Adapt `NotifyChannelSpec`'s listener to connect to this suite's disposable database (pass its
connection string from setup), subscribe to the ordinary, parent, and leaf channels, and
check connection/input errors. Keep this transport helper local to the test suite.

Create an ordinary queue and a numeric partitioned queue, enable both with interval zero,
and store messages before applying the suffix. Capture the message IDs/payloads, trigger identities,
and throttle settings. Show the pre-upgrade partition notification with a live listener,
then drain pending events before running the full `pgmqMigrations` plan. Require six
`AlreadyApplied` entries and only `0007` as `AppliedNow`, preserved data/trigger identities
and throttle settings, silence on parent/leaf channels for new partition inserts, and
continued ordinary delivery. Do not call enable or reconcile between the upgrade and these
assertions: the existing cloned triggers must pick up the replaced function. A second
migration run reports all seven entries `AlreadyApplied`. Keep the existing historical
`testPartitions` checkpoints at six; this separate case supplies the missing upgrade proof.

Run the migration suite and the new test in the `partman` shell, then the new test again with
`PGMQ_TEST_SCHEMA_VERSION=1.12.0` (the stock fixture has upstream's trigger, which is silent
for the normal partition fixture, so the test must pass there too and proves the contract is the same
on both installs). Run `pgmq-config`'s suite (its `NotifyCrashSpec` starts its own server,
applies the full ledger, crashes it, and asserts a post-crash notification on an ordinary
queue; it must stay green because `pgmq.meta` is a logged table that survives the crash) and
`pgmq-hasql`'s `NotifyChannelSpec` (ordinary-queue channel contract).

Also pin ordinary nonzero throttling: enable a long nonzero interval on an ordinary control
queue, send twice in separate commits inside that interval, and require exactly one
notification and no timestamp advance on the suppressed insert. Extend the dedicated native
`pgmq-config/test/NotifyCrashSpec.hs` fixture to use a nonzero declared interval and verify
that explicit reconciliation restores that interval after a crash, without an application
process restart. Its existing post-crash notification assertion still has to pass. EP-1
owns these regression assertions; EP-2 consumes them unchanged. Also add an isolated native mixed-case fixture
with no lowercase metadata row: raw create/enable under the mixed-case name must not
publish on the lowercase channel. This tests the remaining metadata gate independently
of the partition-relation guard.

### Milestone 3: say what the trigger does

Scope: every document and Haddock that describes notification delivery states the two facts
this plan establishes, the ADR records implementation evidence for the boundary, and the changelogs carry the change.

In `docs/design/015-notification-delivery-contract.md`, after the section "Losing throttle
state must not stop delivery", add a section titled "Fail-open applies only to a registered
queue" that: reproduces the psql transcript above in fenced `sql` and `text` blocks; explains
`TG_TABLE_NAME` on cloned partition triggers; states the rule (the fail-open requires the
extracted name in `pgmq.meta`, after actual partitions return before UPDATE); explains the
per-invocation catalog lookup; states that normal partitioned queues receive no insert
notifications on native or stock installs, with alias hardening guaranteed only by 0007 and that `notifyChannelName` for a partitioned queue
names a channel nothing publishes to; and names migration `0007`. In the existing paragraph
that says the degradation lasts "until the next reconcile restores the configured value", add
that the reconciler is commonly invoked at application startup but can also be called explicitly, so after a database crash every ordinary
queue with notifications enabled stays unthrottled until an explicit enable/reconcile call
restores its throttle row, often on application restart, and that
during that window every inserting transaction on such a queue pays PostgreSQL's notification
serialization at commit. Update the "mixed-case" paragraph to say `0007` suppresses missing-row fail-open when
there is no exact lowercase metadata row; it does not remediate pre-existing aliases. In `docs/design/006-queue-notifications.md`, add one sentence at the
channel-derivation paragraph: partitioned queues do not receive insert notifications, with a
link to design note 015.

In `docs/capabilities/insert-notifications.md` (front matter is validated by
`just docs-check`; keep the existing keys), add a Limits bullet: "Partitioned queues receive no
insert notifications. The trigger fires on the leaf partition, whose name normally has no
throttle row; on the extension-free install migration `0007` keeps it that way instead of
notifying unthrottled." Add an `evidence` entry of kind `test` for
`pgmq-hasql/test/PartitionedNotifySpec.hs` with `proves: No notification arrives on the documented channel or any partition channel when a partitioned queue receives inserts, on the native ledger and the stock 1.12 schema.`
Extend the crash bullet to say the unthrottled state lasts until an enable/reconcile call restores the row, typically at
application startup. Then run `okf log add docs/capabilities --kind Update -m "CAP-4: partitioned queues receive no insert notifications; post-crash duration stated"`
so the bundle's log matches the edited concept, and `okf index docs/capabilities --write` if the
index preview differs.

Haddocks: on `notifyChannelName` in `pgmq-core/src/Pgmq/Types.hs`, add a paragraph that a
partitioned queue's channel is never published to (the trigger fires on leaf partitions) and
link design note 015. On `enableNotifyInsert` in
`pgmq-hasql/src/Pgmq/Hasql/Statements/QueueManagement.hs`, `pgmq-hasql/src/Pgmq/Hasql/Sessions.hs`,
and `pgmq-effectful/src/Pgmq/Effectful/Effect.hs`, add one sentence: enabling on a partitioned
queue succeeds server-side but does not deliver on its documented parent channel; see the `pgmq-config` reconciler for the
report. Keep Haddock coverage at 100% on public modules (`cabal haddock pgmq-hasql` reports it).

Update `docs/adr/notification-override-lineage-and-partitioned-queue-boundary.md` in the same
plain-Markdown shape as the other ADRs: Status (Accepted, dated); Context (migration `0003`
opened a local override lineage for `notify_queue_listeners`, `enable_notify_insert`, and
`create_partitioned`; the FIFO override ADR scopes its prohibition to FIFO and defers this
lineage; the partitioned defect and its evidence); Decision (corrections to that lineage are
appended, never edited; actual partitions return before UPDATE and the fail-open is gated on `pgmq.meta`; partitioned queues are outside
notification delivery on every install; making them deliver is upstream work, not a local
override; the convergence exception list is not widened for this); Consequences and
verification (the test names and shells); Alternatives (parent resolution, client guard,
editing `0003`). Link the FIFO and compatibility ADRs and design note 015.

Changelogs: append to the existing "## Unreleased" section (create it only if absent) in `CHANGELOG.md`,
`pgmq-migration/CHANGELOG.md`, `pgmq-core/CHANGELOG.md`, `pgmq-hasql/CHANGELOG.md`,
and `pgmq-effectful/CHANGELOG.md` (EP-3
turns these into 0.7.0.0 entries; never edit a published section). The migration entry
describes `0007` and the behavior change on native installs (partitioned queues stop
notifying per insert); the core and hasql entries are documentation-only notes about
`notifyChannelName` and `enableNotifyInsert`; the root entry summarizes both and states the
post-crash duration.


## Concrete Steps

All commands run from the repository root
`/Users/shinzui/Keikaku/bokuno/libraries/pgmq-hs-project/pgmq-hs`.

Milestone 1, after writing and registering the test:

```bash
PGMQ_TEST_SCHEMA_VERSION=1.13.0 nix develop .#partman --command cabal test pgmq-hasql --test-show-details=direct --test-options='-j2 -p "/PartitionedNotifySpec/"'
```

Expected before the fix (abbreviated):

```text
  PartitionedNotifySpec
    no notification arrives for a partitioned queue: FAIL
      expected no notification within 1s, but "pgmq.q_test_queue_..._p0.INSERT" received one
The normal partition case fails; native alias cases fail too.
```

Milestone 2, after appending the migration and updating the suite:

```bash
md5 -q pgmq-migration/migrations/0007-notify-only-registered-queues.sql
nix develop .#partman --command cabal test pgmq-migration:pgmq-migration-test --test-show-details=direct -j1 --test-options='-j1'
PGMQ_TEST_SCHEMA_VERSION=1.13.0 nix develop .#partman --command cabal test pgmq-hasql --test-show-details=direct --test-options='-j2 -p "/PartitionedNotifySpec/"'
PGMQ_TEST_SCHEMA_VERSION=1.12.0 nix develop .#partman --command cabal test pgmq-hasql --test-show-details=direct --test-options='-j2 -p "/PartitionedNotifySpec/"'
PGMQ_TEST_SCHEMA_VERSION=1.13.0 nix develop .#partman --command cabal test pgmq-config --test-show-details=direct --test-options='-j2'
PGMQ_TEST_SCHEMA_VERSION=1.13.0 nix develop .#partman --command cabal test pgmq-hasql --test-show-details=direct --test-options='-j2'
```

Expected: every migration test passes (record the observed count; component and
payload tests cover seven files), both `PartitionedNotifySpec` runs execute the normal cases (native also executes alias cases),
`pgmq-config` reports every `NotifyCrashSpec` case passing, and the full `pgmq-hasql` suite is green.
The migration suite prints the server as
`Partition test versions: <server_version> / pg_partman <extversion>`; keep that line for EP-3.

To repeat the reviewer's manual evidence after the fix, run the psql transcript from Context and
Orientation against a scratch server with the seven-file ledger applied; expected: no
`Asynchronous notification` line after the three sends, and `last_notified_at` still the epoch.

Milestone 3, after the documentation edits:

```bash
nix fmt
git diff --check
just docs-check
nix develop --command cabal haddock pgmq-core pgmq-hasql pgmq-effectful
# Inspect the successful command output for public-module coverage; do not mask failure with a pipe.
```

Expected: `nix fmt` changes nothing on a second run, `git diff --check` prints nothing,
`just docs-check` exits zero, and every public module reports `100%` Haddock coverage.

Commit after each milestone with the trailers

```text
MasterPlan: docs/masterplans/6-close-the-notification-reconciler-and-evidence-gaps-surfaced-by-the-0-6-1-0-review.md
ExecPlan: docs/plans/23-gate-the-notification-fail-open-on-a-real-queue-row-and-state-the-partitioned-queue-contract.md
Intention: intention_01m2nz9a82ejh91yg9sk7t6a7a
```

using Conventional Commits types (`test(pgmq-hasql): ...`, `fix(pgmq-migration): ...`,
`docs: ...`).


## Validation and Acceptance

Acceptance is behavior, observed three ways.

On the native ledger with `0007` applied, a partitioned queue with notifications enabled and
three inserts produces no notification on `notifyChannelName` or on any partition channel
within one second, and its throttle row's `last_notified_at` remains `to_timestamp(0)`. The
same test passes with `PGMQ_TEST_SCHEMA_VERSION=1.12.0`. The M1 run records the failure
before the fix; the six-entry upgrade case retains a reproducible failing-behavior control
without editing the final manifest.

An ordinary queue is unchanged: `NotifyChannelSpec` still receives exactly one notification on
`notifyChannelName` for a probe send, and `NotifyCrashSpec` still receives a notification on an
ordinary queue after a crash-and-restart cycle, and still sees the throttle row restored by a
reconcile.

Native alias acceptance also proves metadata alone cannot enable fail-open and a matching
leaf throttle row cannot be updated or publish. Ordinary nonzero throttling still suppresses
a second in-window send; explicit post-crash reconciliation restores its declared interval.

The populated six-entry upgrade fixes existing triggers without reinstalling them, preserves
messages and throttle settings, applies only `0007`, and is a no-op on the next ledger run.

The ledger is consistent: `testNativeComponent` lists seven names, `testNativePayload` pins the
new digest, and the latest catalog checkpoint converges with fresh upstream 1.13 SQL with only
the same three body exceptions.

Documentation acceptance: `grep -n "partitioned" docs/design/015-notification-delivery-contract.md docs/capabilities/insert-notifications.md pgmq-core/src/Pgmq/Types.hs pgmq-hasql/src/Pgmq/Hasql/Statements/QueueManagement.hs`
finds the new statements; `just docs-check` passes; the new ADR exists and links resolve.


## Idempotence and Recovery

The migration is `CREATE OR REPLACE`, and the ledger applies it once per database; re-running
any test suite creates fresh disposable servers, so every step can be repeated. If the
`PartitionedNotifySpec` skips instead of running, you are not in the `partman` shell; re-run
under `nix develop .#partman`. If `testNativePayload` fails after `nix fmt`, the formatter
changed the SQL bytes: recompute the MD5 and update the pin (this is expected once). If a
disposable server from an interrupted run lingers, the suites reap it on the next start from
`/tmp/ephpg-pgmq-hs-<uid>`. Nothing in this plan touches a production database; operators adopt
`0007` by running their normal migration step, which applies exactly one new file.


## Interfaces and Dependencies

No Haskell type or function signature changes. New artifacts:

- `pgmq-migration/migrations/0007-notify-only-registered-queues.sql`, listed last in
  `pgmq-migration/migrations/manifest`; the embedded component `pgmq` gains a seventh
  migration named `0007-notify-only-registered-queues`.
- `pgmq-hasql/test/PartitionedNotifySpec.hs` exporting `tests :: Pool.Pool -> Database -> TestTree`,
  registered in `pgmq-hasql/test/Main.hs` and listed in the test suite's `other-modules`. It
  uses `Database.PostgreSQL.LibPQ` (already a test dependency), `Pgmq.Hasql.Sessions`, and
  `Pgmq.Hasql.Statements.Types`.
- `docs/adr/notification-override-lineage-and-partitioned-queue-boundary.md`.

The populated-upgrade fixture uses the migration suite's existing `pg-migrate` dependency
and packaged historical payloads. Its only added test dependency is `postgresql-libpq`,
already used elsewhere in this project; no public interface or library dependency changes.

Libraries: `postgresql-libpq` for `LISTEN` (there is no hasql API for notifications), `tasty`
and `tasty-hunit` as in the rest of the suite, `ephemeral-pg` through the existing
`EphemeralDb` helper. The `partman` shell is required for the partitioned test and the
migration suite's partition case.


Revision note (2026-10-25): strengthened the proposed migration with a relation guard before
UPDATE, added native alias and listener-liveness acceptance, clarified explicit post-crash
recovery, preserved existing Unreleased entries, and passed serial execution to Tasty itself.
The membership-only design is superseded by the notification boundary ADR; implementation
and live SQL acceptance remain pending.

Pre-implementation refresh (2026-10-25): Added populated 0006-to-0007 upgrade acceptance in the
serial migration suite using packaged payloads and existing triggers; retained historical
partition checkpoints.
