---
id: 5
slug: correct-the-fifo-grouped-read-ordering-index-and-partition-retention-contracts
title: "Correct the FIFO grouped-read ordering, index, and partition-retention contracts"
kind: master-plan
created_at: 2026-09-12T14:23:07Z
intention: "intention_01m2b005z8egm8rarkzn70zv4t"
provenance:
  created_by:
    model: "claude-opus-5[1m]"
    harness: "claude-code"
    at: 2026-09-12T14:23:07Z
---

# Correct the FIFO grouped-read ordering, index, and partition-retention contracts

This MasterPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.
If durable project context changes, update or create ADRs in docs/adr/ in the same change.


## Vision & Scope

PGMQ's grouped reads and its FIFO index are the ordering primitives underneath every FIFO
consumer of this repository — `mori://shinzui/keiro` (package `keiro-pgmq`) and
`mori://shinzui/shibuya-pgmq-adapter`. The July 2026 keiro-pgmq review catalogued seven findings
against that stack as
`mori://shinzui/keiro/masterplans/17-harden-keiro-pgmq-fifo-ordering-dlq-operator-paths-and-provisioning-surfaced-by-the-2026-07-pgmq-review`.
Three of them are defects in *this* repository's SQL and documentation rather than in keiro's
Haskell, and keiro relocated them here on 2026-09-12 (see that MasterPlan's Decision Log and the
Surprises entry it recorded the same day). This MasterPlan owns them. The four findings that live
in `keiro-pgmq/src` stay with keiro.

The three relocated defects, all re-verified against the vendored PGMQ 1.13.0 in
`vendor/pgmq/pgmq-extension/sql/pgmq.sql` on 2026-09-12:

- **PGQ-2 — grouped reads have no defined return order.** `pgmq.read_grouped` (vendored SQL from
  line 381) ends in `UPDATE pgmq.%I m SET ... FROM selected_messages sm WHERE m.msg_id = sm.msg_id
  RETURNING ...`. The `ORDER BY msg_id` inside the `selected_messages` CTE does not constrain the
  `UPDATE`'s output, so the batch a client receives is in plan-dependent order. `read_grouped_head`
  (line 245) has the same shape. A consumer that trusts row order to be send order — which
  "FIFO" invites — is trusting the planner. `read_grouped_rr` already carries a
  `selection_order` column for exactly this reason, so the repository is internally inconsistent.
- **PGQ-5 — the FIFO index cannot serve the grouped-read predicates.**
  `pgmq._create_fifo_index_if_not_exists` (line 1547) executes
  `CREATE INDEX IF NOT EXISTS %I ON pgmq.%I USING GIN (headers)`. A jsonb GIN index serves the
  containment and existence operator classes (`@>`, `?`, `?|`, `?&`). Every grouped read instead
  executes `COALESCE(headers->>'x-pgmq-group', '_default_fifo_group')` extraction equality, a
  `GROUP BY` on that expression, and `MIN(msg_id)` per group. None of that can use a GIN index on
  `headers`, so the grouped reads stay full table scans no matter how many times
  `pgmq.create_fifo_index` is called — while that function's own comment advertises "a GIN index on
  the headers column to improve FIFO read performance" and
  [`docs/design/008-fifo-read.md`](../design/008-fifo-read.md) repeats the claim as "FIFO index for
  better performance". At fleet polling cadence against a large backlog this is an O(N)-per-poll
  tax the documentation says is already paid.
- **PGQ-4 (upstream half) — partitioned retention silently drops unprocessed work.**
  `pgmq.create_partitioned` writes `retention_keep_table = false` into pg_partman's `part_config`
  for the queue table (vendored line 1449) and for the archive table (line 1519). pg_partman
  therefore *drops whole partitions* once they age past `retention_interval`, with no regard for
  whether their messages were read, and the same rule expires archived rows. A consumer outage or
  backlog longer than the retention interval is bulk message loss. Nothing in this repository's
  documentation says so.

After this initiative: both grouped reads return each batch in a documented, deterministic order;
`pgmq.create_fifo_index` builds an index the grouped-read predicates actually use, proven by
`EXPLAIN` against a seeded queue rather than asserted; the FIFO reconciliation surface still
reports index state truthfully after that change; and
[`docs/design/008-fifo-read.md`](../design/008-fifo-read.md) states the real ordering contract —
including that strict per-group FIFO under a batch larger than one message wants PGMQ 1.12's
`read_grouped_head`, not `read_grouped` — alongside the destructive retention semantics.

In scope: local override migrations on top of the vendored 1.13.0 install, their tests in
`pgmq-migration/test/Main.hs`, the `listFifoIndexQueueNames`/`pgmq-config` consequences of any
index rename, corrections to `docs/design/008-fifo-read.md` and the user-facing docs under
`docs/user/`, and an upstream patch prepared for the PGMQ project but not submitted.

Out of scope: the four findings that stay with keiro MasterPlan 17 — DLQ redrive header
preservation and DLQ archive/purge visibility safety (`mori://shinzui/keiro/plans/117-preserve-headers-on-dlq-redrive-and-make-archive-and-purge-visibility-safe`),
and the `Job`/`JobTuning` ordering enforcement plus keiro's own haddock corrections and
`mkPartitionSpec` guardrail (`mori://shinzui/keiro/plans/116-enforce-fifo-group-ordering-under-failure-and-batched-consumption`
and `mori://shinzui/keiro/plans/118-correct-partitioned-retention-semantics-and-the-fifo-index`).
Also out of scope: changing `read_grouped`'s batch-filling semantics or its visibility rules;
adding a grouped-head read strategy to `mori://shinzui/shibuya-pgmq-adapter` (keiro's decision,
recorded in its MasterPlan 17); refusing or rewriting a caller's `retention_interval`; and
publishing to Hackage.


## Decomposition Strategy

Three child plans, split by the artifact each one changes and by the evidence each one must
produce.

EP-1 (`docs/plans/19-give-the-grouped-reads-a-deterministic-return-order.md`) owns PGQ-2: a local
override of `pgmq.read_grouped` and `pgmq.read_grouped_head` (and their `_with_poll` siblings,
which share the bodies) that carries the selection order through to the returned rows, plus the
test that pins it. Its evidence is a deterministic multi-row batch.

EP-2 (`docs/plans/20-replace-the-fifo-gin-index-with-one-the-grouped-reads-can-use.md`) owns
PGQ-5: a local override of `pgmq._create_fifo_index_if_not_exists` that builds a btree expression
index on the grouped-read key, the decision about whether that index keeps the existing
`q_<queue>_fifo_idx` name, and whatever `pgmq-hasql`/`pgmq-config` must change so FIFO index state
is still reported truthfully. Its evidence is an `EXPLAIN` plan that changes from a sequential scan
to an index scan on a seeded queue.

EP-3 (`docs/plans/21-state-the-fifo-ordering-and-partitioned-retention-contracts-truthfully.md`)
owns the documentation truth: PGQ-4's upstream half and the corrections to
`docs/design/008-fifo-read.md` that EP-1 and EP-2 make necessary. Its evidence is prose, so it runs
last and describes what actually shipped rather than what was planned.

Alternatives considered. Folding EP-1 and EP-2 into one plan because they share migration `0007`
was rejected: one changes the body of a hot read function and the other changes DDL plus a
catalog-matching statement in `pgmq-hasql`, so they carry different risk and different acceptance
evidence, and coupling them would serialize a one-line ordering fix behind an index-naming
decision. Splitting EP-3's two halves (ordering contract, retention contract) into separate plans
was rejected: both are edits to the same two documents, and a reader needs them in one pass.
Deferring PGQ-2 entirely on the grounds that `read_grouped_head` returns at most one row per group
was rejected: `read_grouped` remains supported and its multi-row batches are still returned in
undefined order, and keiro's drain path uses it today.

ADR and design-note context, per the exec-plan skill's
[ADR workflow](../../agents/skills/exec-plan/ADR.md):

- [`docs/adr/pgmq-1.12-1.13-compatibility.md`](../adr/pgmq-1.12-1.13-compatibility.md) — fixes the
  upgrade and compatibility boundaries for the 1.12/1.13 work; every migration here appends to the
  ledger it describes and must keep the 1.11 predecessor validator unchanged.
- [`docs/design/012-vendor-upstream-pgmq-sql.md`](../design/012-vendor-upstream-pgmq-sql.md) — the
  vendoring policy that makes a local override migration (not an edit to vendored SQL) the only
  legitimate way to change upstream function bodies here.
- [`docs/design/008-fifo-read.md`](../design/008-fifo-read.md) — the FIFO contract note that
  currently carries both false claims; EP-3 owns its correction.
- [`docs/design/018-reconciliation-contract.md`](../design/018-reconciliation-contract.md) — the
  truthful-reporting contract EP-2 must not break when the FIFO index changes shape or name.
- [`docs/design/015-notification-delivery-contract.md`](../design/015-notification-delivery-contract.md)
  and migration `0003-notify-crash-safety-and-locking.sql` — the precedent for how a local override
  is written, headed, and registered in the convergence test's exceptions.

Candidate ADR at completion: the FIFO delivery contract for this repository — which grouped read
guarantees what, in which order rows arrive, what the FIFO index does and does not accelerate, and
that partitioned retention is destructive to unprocessed work.


## Exec-Plan Registry

| # | Title | Path | Hard Deps | Soft Deps | Status |
|---|-------|------|-----------|-----------|--------|
| 1 | Give the grouped reads a deterministic return order | docs/plans/19-give-the-grouped-reads-a-deterministic-return-order.md | None | None | Not Started |
| 2 | Replace the FIFO GIN index with one the grouped reads can use | docs/plans/20-replace-the-fifo-gin-index-with-one-the-grouped-reads-can-use.md | None | EP-1 | Not Started |
| 3 | State the FIFO ordering and partitioned-retention contracts truthfully | docs/plans/21-state-the-fifo-ordering-and-partitioned-retention-contracts-truthfully.md | EP-1, EP-2 | None | Not Started |

Status values: Not Started, In Progress, Complete, Cancelled.


## Dependency Graph

EP-1 and EP-2 can proceed in parallel. Neither needs anything the other produces; the soft
dependency is file coordination only. Both append to the native migration ledger, whose next free
number is `0007` (the ledger currently ends at
`pgmq-migration/migrations/0006-preserve-partitioned-reentry-v1.13.0.sql`). Whichever plan lands
first creates `0007`; the second either appends its `CREATE OR REPLACE` statements to that file if
it is not yet released, or takes `0008`. Both plans must state which they did and update
`pgmq-migration/migrations/manifest` accordingly.

EP-3 hard-depends on both because it documents shipped behavior: it cannot state the ordering
guarantee before EP-1 defines it, and it cannot describe the FIFO index before EP-2 decides its
shape and name. EP-3's PGQ-4 retention half has no dependency and may be drafted earlier, but it
lands as one documentation change.


## Integration Points

**`pgmq-migration/migrations/0007-*.sql` and `.../manifest` (EP-1, EP-2).** The shared local
override migration. Follow the header convention of
`pgmq-migration/migrations/0003-notify-crash-safety-and-locking.sql`: a numbered list of the
deliberate divergences from upstream and why each exists, with every function re-created under its
original signature so the file is a drop-in replacement. Never edit
`0001-install-v1.11.0.sql` or the vendored SQL under `vendor/pgmq/`. Adding a file means adding one
line to `manifest`.

**`pgmq-migration/test/Main.hs` (EP-1, EP-2).** Three coordinated edits, and the reason a naive
migration turns the suite red:

1. `testNativeComponent` (around line 321) is the one place the ledger is spelled out; append the
   new migration name to its list.
2. `testConvergence` runs `take (if latest then 6 else 4) names` (line 158) — the `6` is the
   current ledger length and must grow with it.
3. `testConvergence` compares a migrated database against a fresh vendored install and subtracts an
   `exceptions` list of deliberately divergent keys (line 168). Every function body a local
   override changes needs an entry — `body:read_grouped(text, integer, integer)` and
   `body:read_grouped_head(text, integer, integer)` for EP-1,
   `body:_create_fifo_index_if_not_exists(text)` for EP-2 — or the test fails on a correct change.
   The suite also asserts each exception key is *present* in both snapshots, so a typo fails loudly
   rather than silently excusing a real difference.

**`pgmq-hasql/src/Pgmq/Hasql/Statements/QueueObservability.hs` (EP-2 owns, EP-3 documents).**
`listFifoIndexQueueNames` (line 44) identifies FIFO indexes by matching `indexname ~ '^q_.*_fifo_idx$'`
in `pg_indexes`, and `pgmq-config`'s reconciler consumes it to decide `CreatedFifoIndex` vs
`SkippedFifoIndex` (`pgmq-config/src/Pgmq/Config/Reconcile.hs:74` and `:172`). Two viable designs,
and EP-2 owns the choice: keep the `q_<queue>_fifo_idx` name — in which case the override must
`DROP INDEX` the old GIN index before creating the btree one, because `CREATE INDEX IF NOT EXISTS`
is a no-op against an existing index of the same name — or take a new name, in which case this
statement and the reconciler's reporting must recognise both, and an existing queue's stale GIN
index needs an explicit disposition. EP-1 must not touch this file.

**`docs/design/008-fifo-read.md` (EP-3 owns).** EP-1 and EP-2 record what the note must say in
their own Decision Logs but do not edit it; a single owner keeps the contract from being described
twice in two voices.

**Release.** The family is released at 0.6.0.0 and `keiro-pgmq` is bounded `>=0.6 && <0.7`, which
already admits an additive 0.6.1.0. A migration-only change is additive server behavior with no
Haskell API change, so EP-1 targets 0.6.1.0. EP-2 targets 0.6.1.0 as well unless its index-naming
decision changes a published type or the documented meaning of `listFifoIndexQueueNames`'s result,
in which case it states the higher bump and records that keiro must move its bound — tracked on the
keiro side by MasterPlan 17, not here.

**Upstream.** These are defects in the PGMQ project's SQL (`mori://pgmq/pgmq`), not only in this
vendor copy. Each of EP-1 and EP-2 prepares an upstream patch against the tagged source and
records it in the plan; neither submits one.


## Progress

- [ ] EP-1: `read_grouped` and `read_grouped_head` (with their `_with_poll` siblings) return rows in a defined order via a local override migration; the migration ledger, convergence exceptions, and `testNativeComponent` list are updated together.
- [ ] EP-1: A multi-row grouped-read batch is asserted to arrive in send order; the 1.11 predecessor validator and fresh-install convergence still pass.
- [ ] EP-2: `_create_fifo_index_if_not_exists` builds a btree expression index on the grouped-read key; the old GIN index's disposition and the index name are decided and recorded.
- [ ] EP-2: `EXPLAIN` evidence captured before and after against a seeded queue, showing the grouped-read probe moving off a sequential scan; FIFO index reporting still truthful through `pgmq-config`.
- [ ] EP-3: `docs/design/008-fifo-read.md` states the real ordering contract, the head-read guidance for strict per-group FIFO, and what the FIFO index does and does not accelerate.
- [ ] EP-3: Partitioned retention documented as dropping whole partitions of unprocessed messages, for both the queue and the archive table, wherever this repository describes `create_partitioned`.
- [ ] Family released with the migration(s); ADR distillation pass done (the FIFO delivery contract candidate).


## Surprises & Discoveries

- Relocation (2026-09-12): the keiro-side plans that originally owned this work assumed a
  pgmq-hs 0.4.1.0/0.4.2.0 release train and migration number `0003`. Both are dead: the family
  shipped 0.6.0.0 on 2026-09-10 and the ledger already holds six entries, so the next free number
  is `0007`. Recorded here so neither child plan inherits the stale numbers.
- Relocation (2026-09-12): PGMQ 1.12.0's `read_grouped_head`, already exposed by this repository at
  0.6.0.0 as `readGroupedHead`/`readGroupedHeadWithPoll`, changes how PGQ-2 should be read. It
  returns at most one message per group and computes each group's head as `MIN(msg_id)` regardless
  of visibility, so cross-group row order cannot affect per-group FIFO for that function — but its
  `RETURNING` is equally unordered, and `read_grouped` remains supported and still hands out
  multi-row batches in planner order. EP-1 therefore covers both functions, and EP-3 documents head
  reads as the primitive to reach for when strict per-group order must survive a batch larger than
  one.


## Decision Log

- Decision: Take ownership of PGQ-2, PGQ-5, and PGQ-4's upstream half from keiro MasterPlan 17;
  leave PGQ-1, PGQ-3, PGQ-6, PGQ-7, and PGQ-4's keiro half with keiro.
  Rationale: The three relocated findings are defects in this repository's SQL and documentation,
  reachable by every FIFO consumer, not just keiro. The four that stay are in
  `keiro-pgmq/src/Keiro/PGMQ/Job.hs` and `.../Dlq.hs` — code that does not exist here, so
  relocating them would have left them unimplementable. This mirrors the relocation of keiro
  MasterPlan 21 into
  `docs/masterplans/3-harden-the-pgmq-hs-family-surfaced-by-the-2026-07-review.md`, which that
  MasterPlan's out-of-scope list deliberately stopped short of by naming keiro plans 116 and 118 as
  the owners of exactly these findings.
  Date: 2026-09-12

- Decision: Fix the defective function bodies with local override migrations appended to the native
  ledger, never by editing the vendored upstream SQL.
  Rationale: [`docs/design/012-vendor-upstream-pgmq-sql.md`](../design/012-vendor-upstream-pgmq-sql.md)
  makes the vendor tree a byte-exact mirror of an upstream tag, and migrations `0003` and `0006`
  already establish the override pattern with `CREATE OR REPLACE` under the original signature.
  Editing an applied migration would desynchronize existing databases.
  Date: 2026-09-12

- Decision: Prepare an upstream patch for each SQL defect against `mori://pgmq/pgmq` and record it
  in the owning plan, but do not submit it.
  Rationale: The defects are upstream's, so a fix that only lives in this vendor copy leaves every
  other PGMQ user exposed and leaves this repository carrying overrides indefinitely. Submission is
  a separate, explicitly authorized step.
  Date: 2026-09-12

- Decision: Three child plans split by artifact and evidence type, with documentation last.
  Rationale: Deterministic ordering is proven by a row-order assertion, the index by an `EXPLAIN`
  plan, and the contracts by prose that must describe what shipped rather than what was intended.
  Date: 2026-09-12


## Outcomes & Retrospective

(To be filled during and after implementation.)
