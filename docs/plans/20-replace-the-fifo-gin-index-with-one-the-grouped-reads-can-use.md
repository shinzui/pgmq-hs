---
id: 20
slug: replace-the-fifo-gin-index-with-one-the-grouped-reads-can-use
title: "Replace the FIFO GIN index with one the grouped reads can use"
kind: exec-plan
created_at: 2026-09-12T14:23:59Z
intention: "intention_01m2b005z8egm8rarkzn70zv4t"
master_plan: "docs/masterplans/5-correct-the-fifo-grouped-read-ordering-index-and-partition-retention-contracts.md"
provenance:
  created_by:
    model: "claude-opus-5[1m]"
    harness: "claude-code"
    at: 2026-09-12T14:23:59Z
---

# Replace the FIFO GIN index with one the grouped reads can use

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.
If durable project context changes, update or create ADRs in docs/adr/ in the same change.


## Purpose / Big Picture

PGMQ offers `pgmq.create_fifo_index(queue_name)`, documented in its own SQL comment as "a GIN index
on the headers column to improve FIFO read performance" and in
[`docs/design/008-fifo-read.md`](../design/008-fifo-read.md) as the "FIFO index for better
performance". This repository exposes it through `pgmq-config`'s `withFifoIndex`, and `keiro-pgmq`
provisions it for every ordered job queue on the strength of that claim.

The index cannot help. It is `USING GIN (headers)`, which serves the jsonb containment and existence
operator classes — `@>`, `?`, `?|`, `?&`. Every grouped read instead evaluates
`COALESCE(headers->>'x-pgmq-group', '_default_fifo_group')`: extraction equality, a `GROUP BY` on
that expression, and `MIN(msg_id)` per group. A GIN index on `headers` is unusable for all of it, so
each grouped read is a full scan of the queue table whether or not the index exists. At a fleet's
polling cadence against a large backlog, that is an O(N) cost per poll that the documentation says
has already been paid.

After this plan, `pgmq.create_fifo_index` builds a btree expression index on the actual grouped-read
key, and the improvement is demonstrated with `EXPLAIN` output captured in this plan — a plan that
sequential-scans before the change and index-scans after it, on a queue seeded large enough for the
planner to care. The FIFO index reconciliation surface still reports truthfully which queues have
their index, including for queues provisioned before this change.

You can see it working by seeding 100,000 messages across many groups, running the grouped-read
probe under `EXPLAIN (ANALYZE, BUFFERS)`, and comparing the node type and rows read before and
after.


## Progress

- [ ] M1: Candidate index definition chosen and proven against the real grouped-read predicates
  with `EXPLAIN` on a seeded queue; before/after plans pasted into Validation and Acceptance.
- [ ] M2: Local override migration re-creates `pgmq._create_fifo_index_if_not_exists` to build the
  btree expression index, with the old GIN index's disposition implemented; manifest and the three
  ledger-aware places in `pgmq-migration/test/Main.hs` updated.
- [ ] M3: FIFO index detection still truthful — `listFifoIndexQueueNames` and `pgmq-config`
  reconcile reporting handle pre-existing GIN indexes and the new definition; `pgmq-hasql` and
  `pgmq-config` suites green.
- [ ] M4: Upstream patch prepared against `mori://pgmq/pgmq` and recorded here (not submitted);
  `CHANGELOG.md` entries written; release target recorded.


## Surprises & Discoveries

- Plan authoring (2026-09-12): the index is created by `CREATE INDEX IF NOT EXISTS
  <qtable>_fifo_idx` (`vendor/pgmq/pgmq-extension/sql/pgmq.sql:1556`). Keeping that name therefore
  cannot work by itself: against a queue that already has the GIN index, `IF NOT EXISTS` makes the
  new definition a silent no-op. Either the override drops the old index first, or the new index
  takes a different name — and the second choice collides with detection (see the Decision Log).
- Plan authoring (2026-09-12): detection is name-based, not definition-based.
  `listFifoIndexQueueNames` matches `indexname ~ '^q_.*_fifo_idx$'` in `pg_indexes`
  (`pgmq-hasql/src/Pgmq/Hasql/Statements/QueueObservability.hs:44`), and `pgmq-config`'s reconciler
  turns that into `CreatedFifoIndex`/`SkippedFifoIndex`
  (`pgmq-config/src/Pgmq/Config/Reconcile.hs:74`, `:172`). A rename without a matching change there
  would make the reconciler create an index on every startup and report it as new each time.


## Decision Log

- Decision: The index must be a btree expression index whose leading key is
  `COALESCE(headers->>'x-pgmq-group', '_default_fifo_group')`, with `msg_id` as the second key.
  Rationale: That expression, a `GROUP BY` on it, and `MIN(msg_id)` per group are exactly what all
  three grouped reads execute. A composite `(fifo_key, msg_id)` btree can serve the per-group
  minimum as an index-only ordered scan and the equality join back to the group key. Whether `vt`
  belongs in the index — as a third key or an `INCLUDE` column, given the `vt <= clock_timestamp()`
  predicates — is left to M1's `EXPLAIN` evidence rather than decided here.
  Date: 2026-09-12

- Decision: The index-name question (keep `q_<queue>_fifo_idx` and drop the old index, or take a new
  name and teach detection about both) is M1's to settle with evidence, and whichever way it goes
  this plan owns the `pgmq-hasql`/`pgmq-config` consequences.
  Rationale: Both options have a real cost — an in-place `DROP INDEX` plus `CREATE INDEX` is a write
  lock on a live queue table, while a rename spreads knowledge of two index names through the
  reconciler and its reporting. The parent MasterPlan assigns ownership of
  `listFifoIndexQueueNames` to this plan precisely so the choice is made in one place.
  Date: 2026-09-12

- Decision: Do not change when the index is created, who creates it, or the `fifoIndex` field in
  `pgmq-config`'s `QueueConfig`.
  Rationale: This plan changes what the index *is*, not the provisioning contract. Consumers
  including `keiro-pgmq` already call `withFifoIndex` and must keep compiling unchanged.
  Date: 2026-09-12


## Outcomes & Retrospective

(To be filled during and after implementation.)


## Context and Orientation

**What this repository is.** `pgmq-hs` packages the PostgreSQL message-queue extension
[PGMQ](https://pgmq.github.io/pgmq/) for Haskell, as five libraries released together — `pgmq-core`,
`pgmq-hasql`, `pgmq-effectful`, `pgmq-config`, `pgmq-migration` — currently at 0.6.0.0.

**The two SQL paths, and which one is yours.** `vendor/pgmq/` is a byte-exact mirror of an upstream
PGMQ tag (`v1.13.0` at `32c075bb6dbed66a303d1a792393c93e36c09a97`), governed by
[`docs/design/012-vendor-upstream-pgmq-sql.md`](../design/012-vendor-upstream-pgmq-sql.md) — never
edit it. `pgmq-migration/migrations/` is the append-only ledger listed in
`pgmq-migration/migrations/manifest` and embedded at compile time by
`pgmq-migration/src/Pgmq/Migration/Internal/Definition.hs`. It holds six entries today, ending in
`0006-preserve-partitioned-reentry-v1.13.0.sql`; `0003` and `0006` are *local overrides* that
`CREATE OR REPLACE` upstream functions under their original signatures. Read `0003`'s header comment
for the house style. The next free number is `0007`, which
`docs/plans/19-give-the-grouped-reads-a-deterministic-return-order.md` may claim first — check
`manifest` before writing.

**The defective function.** `vendor/pgmq/pgmq-extension/sql/pgmq.sql`, line 1547:

```sql
CREATE OR REPLACE FUNCTION pgmq._create_fifo_index_if_not_exists(queue_name TEXT)
RETURNS void AS $$
DECLARE
    qtable TEXT := pgmq.format_table_name(queue_name, 'q');
    index_name TEXT := qtable || '_fifo_idx';
BEGIN
    -- Create GIN index on headers for efficient FIFO key lookups
    EXECUTE FORMAT(
        $QUERY$
        CREATE INDEX IF NOT EXISTS %I ON pgmq.%I USING GIN (headers);
        $QUERY$,
        index_name, qtable
    );
END;
$$ LANGUAGE plpgsql;
```

Two public wrappers delegate to it: `pgmq.create_fifo_index(queue_name)` (line 1565) and
`pgmq.create_fifo_indexes_all()` (line 1574, looping over `pgmq.meta`). `pgmq.create_fifo` (line
1568's caller, around line 1566) also calls it during FIFO queue creation. Overriding the one
internal function therefore fixes every path — confirm that by grepping
`_create_fifo_index_if_not_exists` in the vendored SQL before assuming it.

**What the grouped reads actually ask of an index.** From the same file: `read_grouped` (line 381),
`read_grouped_rr` (line 126), and `read_grouped_head` (line 245) all open with

```sql
SELECT COALESCE(headers->>'x-pgmq-group', '_default_fifo_group') AS fifo_key,
       MIN(msg_id) AS head_msg_id
FROM pgmq.<qtable>
GROUP BY COALESCE(headers->>'x-pgmq-group', '_default_fifo_group')
```

(`read_grouped` adds `WHERE vt <= clock_timestamp()` to that aggregate; the other two compute the
head regardless of visibility and filter afterwards), then join back on
`COALESCE(m.headers->>'x-pgmq-group', '_default_fifo_group') = fg.fifo_key` with
`m.vt <= clock_timestamp()` and ordering by `msg_id`. These are the predicates your index must
serve. There is no `@>` or `?` anywhere in them, which is why the GIN index is inert.

**How index state is detected and reported.** `pgmq-hasql/src/Pgmq/Hasql/Statements/QueueObservability.hs:44`:

```haskell
listFifoIndexQueueNames :: Statement () [Text]
listFifoIndexQueueNames = preparable sql E.noParams decoder
  where
    sql =
      "select substring(indexname from '^q_(.*)_fifo_idx$')::text \
      \from pg_indexes \
      \where schemaname = 'pgmq' and indexname ~ '^q_.*_fifo_idx$'"
```

Its haddock explains why it exists: `pgmq.create_fifo_index` delegates to
`CREATE INDEX IF NOT EXISTS` and reports nothing back, so a caller that wants to say truthfully
whether it created an index has to look in `pg_indexes`. `pgmq-config`'s reconciler consumes it —
`existingFifoIndexes <- ops ^. #listFifoIndexQueueNames` (`Reconcile.hs:74`), then at `:172` it
either emits `SkippedFifoIndex qn` or calls `createFifoIndex` and emits `CreatedFifoIndex qn`. The
truthful-reporting requirement is a durable contract recorded in
[`docs/design/018-reconciliation-contract.md`](../design/018-reconciliation-contract.md); read it
before changing detection.

**Where the tests live and what a new migration breaks.** `pgmq-migration/test/Main.hs` starts a
disposable PostgreSQL through the `ephemeral-pg` package. Three places know the ledger:
`testNativeComponent`'s explicit list (around line 321), `testConvergence`'s
`take (if latest then 6 else 4) names` (line 158 — the `6` is the ledger length), and
`testConvergence`'s `exceptions` list (line 168), which names each function body a local override
deliberately changes. Add `"body:_create_fifo_index_if_not_exists(text)"` there, for both the
`latest` and non-`latest` branches, since the function exists in 1.12 and 1.13 alike. The suite also
asserts each exception key is present in both snapshots, so a wrong signature string fails loudly.
`pgmq-hasql` and `pgmq-config` have their own suites (`pgmq-hasql-test`, `pgmq-config-test`) that
cover the statements and the reconciler.

**Running the tests.** `cabal test pgmq-migration-test`, `cabal test pgmq-hasql-test`,
`cabal test pgmq-config-test`, or `cabal test all`, from the repository root. Partition cases need
pg_partman and skip without it unless `PGMQ_REQUIRE_PARTMAN=1`; `nix develop .#partman` supplies
both (see `flake.module.nix` and `pgmq-migration/README.md`). This plan's cases need no extension,
but run the full suite in that shell before claiming green.

**ADR and design-note context** (scanned per
[`agents/skills/exec-plan/ADR.md`](../../agents/skills/exec-plan/ADR.md)):

- [`docs/design/018-reconciliation-contract.md`](../design/018-reconciliation-contract.md) — the
  truthful-reporting contract the reconciler must keep after the index changes shape.
- [`docs/design/012-vendor-upstream-pgmq-sql.md`](../design/012-vendor-upstream-pgmq-sql.md) — why
  the fix is a migration and not a vendor edit.
- [`docs/adr/pgmq-1.12-1.13-compatibility.md`](../adr/pgmq-1.12-1.13-compatibility.md) — the
  compatibility boundaries your migration appends to; leave the 1.11 predecessor validator alone.
- [`docs/design/008-fifo-read.md`](../design/008-fifo-read.md) — carries the false performance claim.
  **Do not edit it here**;
  `docs/plans/21-state-the-fifo-ordering-and-partitioned-retention-contracts-truthfully.md` owns it.
  Record what it must say in this plan's Decision Log.

**Related plans.** Parent MasterPlan:
`docs/masterplans/5-correct-the-fifo-grouped-read-ordering-index-and-partition-retention-contracts.md`.
Sibling sharing the migration ledger:
`docs/plans/19-give-the-grouped-reads-a-deterministic-return-order.md`. The consumer that motivated
this work is `keiro-pgmq`, whose plan
`mori://shinzui/keiro/plans/118-correct-partitioned-retention-semantics-and-the-fifo-index`
consumes the released index and keeps its own `EXPLAIN` example.


## Plan of Work

**M1 — prove the index before shipping it.** Start in a scratch database, not in a migration. Create
a queue, seed it with roughly 100,000 messages spread over enough groups to be realistic (a few
hundred groups, uneven sizes, some groups with an invisible head), `ANALYZE` the table, and capture
`EXPLAIN (ANALYZE, BUFFERS)` for the grouped-read probe — the `fifo_groups` aggregate and the
join-back shown in Context and Orientation — with no FIFO index, with the current GIN index, and
with each candidate btree definition. Candidates to try, at minimum:

```sql
CREATE INDEX … ON pgmq.<qtable>
  ((COALESCE(headers->>'x-pgmq-group', '_default_fifo_group')), msg_id);

CREATE INDEX … ON pgmq.<qtable>
  ((COALESCE(headers->>'x-pgmq-group', '_default_fifo_group')), msg_id) INCLUDE (vt);

CREATE INDEX … ON pgmq.<qtable>
  ((COALESCE(headers->>'x-pgmq-group', '_default_fifo_group')), vt, msg_id);
```

The expression in the index must match the query's expression *textually equivalent* to PostgreSQL's
matching rules — a `COALESCE` written differently will not be recognised — so copy it from the
vendored SQL rather than retyping it. Decide the winner on the captured plans: node type, rows
read, buffers. If no candidate beats the sequential scan for a given read, say so explicitly rather
than shipping an index that only looks better; a negative result for one of the three grouped reads
is a finding for `docs/design/008-fifo-read.md`, not a reason to abandon the plan.

Settle the naming decision here too, with the same evidence discipline: measure how long
`DROP INDEX` + `CREATE INDEX` holds locks on the seeded table, and check whether
`CREATE INDEX CONCURRENTLY` is usable from inside a plpgsql function (it is not, inside a
transaction block — record the consequence for operators). At the end of M1 this plan's Validation
section contains the before/after transcripts and the Decision Log names the definition and the
index name.

**M2 — the override migration.** Create the migration file (number from `manifest`, likely `0007`
or `0008`), headed in `0003`'s style: state that it re-creates
`pgmq._create_fifo_index_if_not_exists` under its original signature, why the GIN index was
useless, what the new definition is, and how pre-existing GIN indexes are handled. Implement the
naming decision from M1 — if the name is kept, drop the old index inside the function before
creating the new one, guarded so it only drops an index whose definition is the old GIN one; if the
name changes, create the new index and leave the old one's removal to a documented operator step
rather than dropping something a DBA may have built deliberately. Append the filename to `manifest`
and make the three `pgmq-migration/test/Main.hs` edits.

Add a migration test asserting the index *definition*, not just its presence: query
`pg_indexes.indexdef` for the created index and assert it is a btree on the expected expression.
This is the test that would have caught the original defect.

**M3 — keep reporting truthful.** Whatever M1 decided, make `listFifoIndexQueueNames` and the
`pgmq-config` reconcile path tell the truth for three populations: a queue with no FIFO index, a
queue with the new btree index, and a queue that still has an old GIN index from before this change.
The third is the interesting one — `SkippedFifoIndex` on a queue whose index cannot serve a grouped
read is exactly the false report this initiative exists to remove. Extend `pgmq-hasql-test` and
`pgmq-config-test` with cases for all three. If the honest answer needs a definition check rather
than a name match, change the statement's SQL and its haddock together, and state the new meaning of
the result in the haddock.

**M4 — upstream patch and release paperwork.** Prepare a patch against upstream PGMQ
(`mori://pgmq/pgmq`, tag `v1.13.0`) replacing the GIN index with the proven definition in
`pgmq-extension/sql/pgmq.sql`, its upgrade script, and a test in `pgmq-extension/test/sql/base.sql`
if that suite can assert an index definition. **Do not submit it.** Record its location and content
in the Decision Log. Write the `CHANGELOG.md` entries and state the release target: 0.6.1.0 if the
Haskell API and the documented meaning of `listFifoIndexQueueNames`'s result are unchanged, and the
next breaking version otherwise — in which case note that `keiro-pgmq`'s `>=0.6 && <0.7` bound must
move, tracked on the keiro side by
`mori://shinzui/keiro/masterplans/17-harden-keiro-pgmq-fifo-ordering-dlq-operator-paths-and-provisioning-surfaced-by-the-2026-07-pgmq-review`.


## Concrete Steps

Run everything from the repository root (`mori path mori://shinzui/pgmq-hs`).

```bash
# 1. Read the function you are overriding, its wrappers, and the predicates it must serve.
sed -n '1545,1585p' vendor/pgmq/pgmq-extension/sql/pgmq.sql
grep -n '_create_fifo_index_if_not_exists' vendor/pgmq/pgmq-extension/sql/pgmq.sql
sed -n '381,420p'   vendor/pgmq/pgmq-extension/sql/pgmq.sql   # read_grouped's fifo_groups CTE
```

```bash
# 2. Confirm the next free migration number (plan 19 may have taken 0007).
cat pgmq-migration/migrations/manifest
```

```bash
# 3. M1 evidence, in a scratch database. Seed, ANALYZE, then EXPLAIN the probe with
#    no index / GIN / each btree candidate, and save every plan.
psql "$PGDATABASE" -f /tmp/seed-fifo-queue.sql
psql "$PGDATABASE" -c 'ANALYZE pgmq.q_fifo_probe'
psql "$PGDATABASE" -c 'EXPLAIN (ANALYZE, BUFFERS) <the fifo_groups aggregate>'
```

```text
-- before (expected shape)
HashAggregate  (cost=… rows=…)
  ->  Seq Scan on q_fifo_probe  (actual rows=100000 …)

-- after (expected shape)
GroupAggregate  (cost=… rows=…)
  ->  Index Only Scan using q_fifo_probe_fifo_idx on q_fifo_probe  (actual rows=… …)
```

```bash
# 4. Write the migration and the manifest line, then the three test edits.
$EDITOR pgmq-migration/migrations/0007-fifo-index-btree-expression.sql   # or 0008
printf '0007-fifo-index-btree-expression.sql\n' >> pgmq-migration/migrations/manifest
grep -n 'take (if latest then 6 else 4)' pgmq-migration/test/Main.hs
grep -n 'body:notify_queue_listeners' pgmq-migration/test/Main.hs
$EDITOR pgmq-migration/test/Main.hs
```

```bash
# 5. Build and test, including the statement and reconciler suites.
cabal build all
cabal test pgmq-migration-test
cabal test pgmq-hasql-test
cabal test pgmq-config-test
nix develop .#partman --command cabal test all
```

Replace the placeholder transcripts above with the plans and output you actually captured, including
the rejected candidates — a discarded index definition with its plan is the evidence that the chosen
one is right.


## Validation and Acceptance

1. **The index is used.** On a queue seeded with ~100,000 messages across many groups and freshly
   `ANALYZE`d, the `fifo_groups` aggregate that opens every grouped read plans as an index scan
   (ideally index-only) after the change, where it plans as a sequential scan before. Paste both
   plans here. State the measured rows/buffers, not just the node name.
2. **The index definition is pinned.** A migration test reads `pg_indexes.indexdef` for a created
   FIFO index and asserts a btree on
   `COALESCE((headers ->> 'x-pgmq-group'::text), '_default_fifo_group'::text)` (plus the second
   key), so a future revert to GIN fails the suite.
3. **Every creation path builds the new index.** `pgmq.create_fifo_index`,
   `pgmq.create_fifo_indexes_all`, and FIFO queue creation all produce the btree definition —
   assert on at least the first two.
4. **Reporting is truthful for all three populations.** No index → reconcile creates and reports
   `CreatedFifoIndex`; new btree index present → reports `SkippedFifoIndex`; legacy GIN index
   present → reports what M3 decided, and the decision is documented. No population may be reported
   as having a working FIFO index when it does not.
5. **Convergence and the rest of the suite.** `testConvergence` passes for both the 1.12 fixture and
   the vendored 1.13 with the new exception key present in both snapshots;
   `nix develop .#partman --command cabal test all` green with no new pending cases.
6. **Consumer sanity.** State in Outcomes which released version carries the change, so
   `mori://shinzui/keiro/plans/118-correct-partitioned-retention-semantics-and-the-fifo-index` can
   bound against it.


## Idempotence and Recovery

`CREATE OR REPLACE FUNCTION` is idempotent, and the ledger's history table prevents
re-application. The index DDL inside the function keeps `IF NOT EXISTS` semantics, so calling
`create_fifo_index` repeatedly stays a no-op — but note the hazard that motivated this plan: `IF NOT
EXISTS` is satisfied by an index of the same *name*, regardless of definition, so if the name is
kept the drop step is what makes re-running converge on the right index rather than silently keeping
the wrong one.

Index creation takes a lock on the queue table. `CREATE INDEX CONCURRENTLY` cannot run inside a
transaction block and therefore not from inside this plpgsql function; if M1 finds the lock duration
unacceptable for large live queues, the recovery path is to document an out-of-band operator
procedure (build concurrently under the expected name, then let the function's `IF NOT EXISTS` see
it) rather than to make the function unsafe. Record whichever way it goes.

Two coordination hazards:

- **Migration number collision** with
  `docs/plans/19-give-the-grouped-reads-a-deterministic-return-order.md`. Read `manifest`
  immediately before writing and record what you found.
- **A released migration is immutable.** A wrong index definition is corrected by another
  `CREATE OR REPLACE` in a new numbered file, plus an explicit `DROP INDEX` for the bad index —
  never by editing the released file.

If M1 cannot demonstrate a plan improvement for any candidate, stop and report rather than shipping
a differently-shaped but equally useless index; that outcome changes what
`docs/plans/21-state-the-fifo-ordering-and-partitioned-retention-contracts-truthfully.md` must say
and is a legitimate result of this plan.


## Interfaces and Dependencies

SQL surface after this plan (signatures unchanged, definitions changed):

```sql
pgmq._create_fifo_index_if_not_exists(queue_name TEXT) RETURNS void  -- now builds a btree expression index
pgmq.create_fifo_index(queue_name TEXT)                RETURNS void  -- unchanged wrapper
pgmq.create_fifo_indexes_all()                         RETURNS void  -- unchanged wrapper
```

Haskell surface that must keep compiling for consumers:

```haskell
-- pgmq-config/src/Pgmq/Config/Types.hs
withFifoIndex :: QueueConfig -> QueueConfig      -- unchanged
data QueueConfig = QueueConfig { …, fifoIndex :: !Bool, … }

-- pgmq-hasql/src/Pgmq/Hasql/Statements/QueueObservability.hs
listFifoIndexQueueNames :: Statement () [Text]   -- type unchanged; SQL and haddock may change
```

Files this plan changes:

- `pgmq-migration/migrations/000{7,8}-fifo-index-btree-expression.sql` (new)
- `pgmq-migration/migrations/manifest`
- `pgmq-migration/test/Main.hs`
- `pgmq-hasql/src/Pgmq/Hasql/Statements/QueueObservability.hs` and `pgmq-hasql/test/` (if M3 needs
  a definition-aware check)
- `pgmq-config/src/Pgmq/Config/Reconcile.hs` and `pgmq-config/test/` (only if reporting must change)
- `CHANGELOG.md`

Files this plan must not change: anything under `vendor/`, migrations `0001`–`0006`,
`docs/design/008-fifo-read.md`, and the grouped-read function bodies owned by
`docs/plans/19-give-the-grouped-reads-a-deterministic-return-order.md`.

Test dependencies already declared: `ephemeral-pg >=0.2.1`, `hasql`, `tasty`, `tasty-hunit` in
`pgmq-migration/pgmq-migration.cabal`, with equivalents in the `pgmq-hasql` and `pgmq-config`
suites. pg_partman is needed only by unrelated partition cases, via `nix develop .#partman`.
