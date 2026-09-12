---
id: 21
slug: state-the-fifo-ordering-and-partitioned-retention-contracts-truthfully
title: "State the FIFO ordering and partitioned-retention contracts truthfully"
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

# State the FIFO ordering and partitioned-retention contracts truthfully

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.
If durable project context changes, update or create ADRs in docs/adr/ in the same change.


## Purpose / Big Picture

Two of this repository's documented promises are false, and both were believed by a downstream team
until a review caught them.

The first is about ordering. [`docs/design/008-fifo-read.md`](../design/008-fifo-read.md) says
`pgmq.read_grouped` "ensures strict ordering within a group" and lists the FIFO index under "FIFO
index for better performance". Neither holds as written: strict per-group order survives a batch
larger than one message only if the consumer stops processing a group at its first failure, which
PGMQ cannot enforce and this repository never said was required; and the GIN index the FIFO index
functions build cannot serve any predicate the grouped reads evaluate. PGMQ 1.12 added
`read_grouped_head`, which *does* give per-group ordering safety for larger batches by handing out
at most one head per group, and nothing in this repository tells a reader that is the function to
reach for.

The second is about data. `pgmq.create_partitioned` configures pg_partman with
`retention_keep_table = false` for both the queue table and the archive table, so once a partition
ages past `retention_interval` pg_partman **drops it** — messages that were never read included, and
archived rows too. [`docs/user/queue-configuration.md`](../user/queue-configuration.md) presents the
setting as "partition by day, retain for 7 days" with no warning, and
`pgmq-config/src/Pgmq/Config/Types.hs`'s `PartitionConfig` haddock says nothing at all. A consumer
outage longer than the retention interval is silent bulk message loss.

After this plan, a reader of this repository learns: which grouped read to use for which ordering
guarantee and what each one actually promises about row order; what the FIFO index does and does not
accelerate, with the measured evidence behind the claim; and that partitioned retention destroys
unprocessed messages, stated where someone choosing a retention interval will see it.

You can see it working by reading the two documents and the `PartitionConfig` haddock and finding no
claim that the code does not keep — and by checking that every statement about ordering or the index
matches what the migrations from the sibling plans actually shipped.


## Progress

- [ ] M1: `docs/design/008-fifo-read.md` corrected — per-function ordering contract, the row-order
  guarantee the sibling migration shipped, head-read guidance, and the index's real effect with its
  `EXPLAIN` evidence cited.
- [ ] M2: Partitioned retention documented as destructive to unprocessed messages and to archived
  rows, in `docs/user/queue-configuration.md` and in the `PartitionConfig` haddock in
  `pgmq-config/src/Pgmq/Config/Types.hs`.
- [ ] M3: Durable contract recorded — `docs/design/008-fifo-read.md` cross-referenced from the
  compatibility ADR (or a new ADR written if the FIFO contract warrants its own record); consumer
  notes handed to keiro; `CHANGELOG.md` documentation entry written.


## Surprises & Discoveries

(None yet.)


## Decision Log

- Decision: This plan documents what shipped, so it runs after
  `docs/plans/19-give-the-grouped-reads-a-deterministic-return-order.md` and
  `docs/plans/20-replace-the-fifo-gin-index-with-one-the-grouped-reads-can-use.md`.
  Rationale: The ordering sentence depends on which columns EP-1's override ordered by, and the
  index sentence depends on the definition EP-2 proved — including the possibility that EP-2 finds
  no candidate helps one of the three grouped reads, which this document must then say.
  Date: 2026-09-12

- Decision: Document the destructive retention behavior; do not add a validating constructor or
  refuse a caller's `retention_interval` here.
  Rationale: Upstream's behavior is what it is, and this repository's job is to state it. The
  construction-time guardrail belongs to the consumer that owns a policy about it —
  `mori://shinzui/keiro/plans/118-correct-partitioned-retention-semantics-and-the-fifo-index` adds
  `mkPartitionSpec` for that purpose. Duplicating the policy in two layers would leave two places to
  disagree.
  Date: 2026-09-12

- Decision: State the archive-table half of the retention behavior as prominently as the queue-table
  half.
  Rationale: `pgmq.create_partitioned` applies the same `retention_keep_table = false` to the
  archive (`vendor/pgmq/pgmq-extension/sql/pgmq.sql:1519`), so "archive it for audit" is not durable
  on a partitioned queue. An operator reading only about the queue table would draw exactly the
  wrong conclusion about their audit trail.
  Date: 2026-09-12


## Outcomes & Retrospective

(To be filled during and after implementation.)


## Context and Orientation

**What this repository is.** `pgmq-hs` packages the PostgreSQL message-queue extension
[PGMQ](https://pgmq.github.io/pgmq/) for Haskell as five libraries released together — `pgmq-core`,
`pgmq-hasql`, `pgmq-effectful`, `pgmq-config`, `pgmq-migration` — currently at 0.6.0.0. Its
documentation is split by purpose: `docs/design/NNN-*.md` are durable design/contract notes,
`docs/adr/` holds architecture decision records, `docs/user/` holds task-oriented guides, and
haddock on the types carries the contract a caller sees in their editor.

**Document 1: the FIFO design note.** [`docs/design/008-fifo-read.md`](../design/008-fifo-read.md)
was written for PGMQ 1.8/1.9 and never revised for 1.12. Its "Behavioral Differences" section says:

```text
### `read_grouped` (SQS-style)
- Fills the entire batch from a single message group
- Ensures strict ordering within a group
- Best for processing related messages together
```

and its "FIFO Index Functions" section presents `pgmq.create_fifo_index(queue_name)` as "Create
FIFO index for better performance". Both claims are the ones this plan corrects. The note predates
`read_grouped_head` entirely and does not mention it, even though this repository has exposed it
since 0.6.0.0 (`pgmq-hasql/src/Pgmq.hs:54`, `pgmq-effectful/src/Pgmq/Effectful.hs:76`).

**What is actually true about ordering**, from `vendor/pgmq/pgmq-extension/sql/pgmq.sql`:

- `read_grouped` (line 381) fills a batch group-by-group, oldest group first, and *can* return
  several messages of the same group in one batch. Its `filtered_groups` CTE excludes a group that
  has any earlier invisible message, so a group whose head is in flight is skipped — but once a
  consumer holds three messages of one group simultaneously, nothing in the server stops that
  consumer from processing message 2 after message 1 failed. Strict per-group ordering therefore
  depends on consumer behavior, and the safe configuration is one message per group per batch.
- `read_grouped_head` (line 245) returns at most one message per group, choosing each group's head
  as `MIN(msg_id)` **regardless of visibility** and then taking it only if visible. An in-flight or
  failed head therefore blocks its group in the server. This is the function to use when strict
  per-group order must hold for a batch larger than one message.
- `read_grouped_rr` (line 126) interleaves fairly across groups, one rank at a time, and already
  returns rows in a defined `selection_order`.
- Row order: `docs/plans/19-give-the-grouped-reads-a-deterministic-return-order.md` gives
  `read_grouped` and `read_grouped_head` a defined return order through a local override migration.
  Read that plan's Decision Log for the exact ordering columns it chose before writing the sentence
  that describes them.

**What is actually true about the index.**
`docs/plans/20-replace-the-fifo-gin-index-with-one-the-grouped-reads-can-use.md` replaces the
`USING GIN (headers)` index with a btree expression index on the grouped-read key and captures
`EXPLAIN` evidence in its Validation section. Cite that evidence rather than re-deriving it, and
carry over any negative result it recorded — if some grouped read still cannot use the index, the
design note must say which and why.

**Document 2: the retention claims.** Three places describe partitioned queues and none of them
mention destruction:

- [`docs/user/queue-configuration.md`](../user/queue-configuration.md) around lines 69-88 shows
  `retentionInterval = "7 days"` under the comment "partition by day, retain for 7 days", then
  explains only that these settings apply at creation time and that existing queues are not
  reconciled.
- `pgmq-config/src/Pgmq/Config/Types.hs:59-67` — `PartitionConfig`'s haddock documents `premake`
  carefully and `retentionInterval` not at all.
- `docs/capabilities/declarative-queue-reconciliation.md` also mentions retention; check it and any
  other hit from `grep -rn retention docs/ pgmq-*/src` before declaring M2 done.

**What is actually true about retention.** `pgmq.create_partitioned` (vendored SQL from line 1379)
calls pg_partman's `create_parent` for the queue and archive tables, then writes:

```sql
    UPDATE %I.part_config
    SET
        retention = %L,
        retention_keep_table = false,
        retention_keep_index = true,
    …
```

for the queue table (line 1446-1450) and the same with `retention_keep_table = false` for the
archive table (line 1515-1519). `retention_keep_table = false` instructs pg_partman's maintenance
run to **drop** partitions older than `retention`, not to detach or archive them. Nothing consults
`vt`, `read_ct`, or any processing state. The local override in
`pgmq-migration/migrations/0006-preserve-partitioned-reentry-v1.13.0.sql` re-creates this function
for an unrelated re-entry fix and preserves the retention behavior unchanged — read its header to
see how a local override describes its divergences, and confirm the retention lines are still as
described before documenting them.

**ADR context** (scanned per
[`agents/skills/exec-plan/ADR.md`](../../agents/skills/exec-plan/ADR.md)):

- [`docs/adr/pgmq-1.12-1.13-compatibility.md`](../adr/pgmq-1.12-1.13-compatibility.md) — the only
  ADR in this repository. It records the 1.12/1.13 upgrade and compatibility boundaries, including
  how grouped heads and partition controls were introduced. M3 decides whether the FIFO contract
  belongs as a cross-reference from here or as a new ADR of its own.
- [`docs/design/012-vendor-upstream-pgmq-sql.md`](../design/012-vendor-upstream-pgmq-sql.md) — the
  vendoring policy; explains why documented behavior must be read from the vendor tree plus the
  override migrations, never from upstream's website alone.
- [`docs/design/018-reconciliation-contract.md`](../design/018-reconciliation-contract.md) — the
  truthful-reporting contract; the retention documentation must not contradict what the reconciler
  claims to check (it does not check partition settings at all).

**Related plans.** Parent MasterPlan:
`docs/masterplans/5-correct-the-fifo-grouped-read-ordering-index-and-partition-retention-contracts.md`.
Hard dependencies: `docs/plans/19-give-the-grouped-reads-a-deterministic-return-order.md` and
`docs/plans/20-replace-the-fifo-gin-index-with-one-the-grouped-reads-can-use.md`. The consumer that
raised these findings keeps its own halves in
`mori://shinzui/keiro/plans/116-enforce-fifo-group-ordering-under-failure-and-batched-consumption`
and `mori://shinzui/keiro/plans/118-correct-partitioned-retention-semantics-and-the-fifo-index`;
those plans correct keiro's own haddocks, so the wording chosen here should be one a consumer can
quote.


## Plan of Work

**M1 — rewrite the FIFO contract note.** Revise
[`docs/design/008-fifo-read.md`](../design/008-fifo-read.md). Keep its historical sections (upstream
commits, the 1.9.0 removal of `conditional`) — they are provenance. Replace the "Behavioral
Differences" section with a per-function contract that states, for each of `read_grouped`,
`read_grouped_rr`, and `read_grouped_head` (plus their `_with_poll` variants): how many messages per
group a batch can contain, what happens to a group whose head is invisible, what row order the
caller receives after the sibling plan's migration, and what the caller must do to preserve per-group
order. Say plainly that `read_grouped` with a batch larger than one message gives strict per-group
order only if the consumer abandons a group at its first failure, and that `read_grouped_head` is
the function that makes that property the server's responsibility. Then replace the "FIFO Index
Functions" performance claim with what EP-2 measured, citing its plan by path for the transcripts.
Add the 1.12 functions to the API listing so the note stops describing a 1.9-era surface.

At the end of M1 the note contains no claim the code does not keep. Acceptance: every ordering or
index sentence can be traced either to a line in `vendor/pgmq/pgmq-extension/sql/pgmq.sql`, to an
override migration, or to an `EXPLAIN` transcript in EP-2 — and a reader asking "which grouped read
should I call?" can answer it from this document alone.

**M2 — tell the truth about retention.** In
[`docs/user/queue-configuration.md`](../user/queue-configuration.md), add the destructive semantics
next to the `PartitionConfig` example rather than in a footnote: pg_partman drops whole partitions
past `retentionInterval`, processed or not, and the same applies to the archive table, so a backlog
or consumer outage longer than the interval loses messages permanently. Give the example a retention
interval that does not read as a recommendation, or annotate the existing one. In
`pgmq-config/src/Pgmq/Config/Types.hs`, document `retentionInterval` in `PartitionConfig`'s haddock
with one sentence of the same substance, so the warning reaches a caller's editor. Sweep the rest of
`docs/` and the package haddocks for any other retention or "archive for audit" claim that implies
durability on a partitioned queue, and correct each.

At the end of M2 no document in this repository describes partitioned retention as ordinary cleanup.
Acceptance: `grep -rn retention docs/ pgmq-*/src` shows every user-facing mention either warning
about or explicitly out of scope for destruction.

**M3 — record the durable contract and hand off.** Decide whether the FIFO delivery contract
warrants its own ADR in `docs/adr/` or a cross-reference from
[`docs/adr/pgmq-1.12-1.13-compatibility.md`](../adr/pgmq-1.12-1.13-compatibility.md) to the revised
design note; the parent MasterPlan names this as its ADR candidate, so record the decision either
way. Write the `CHANGELOG.md` documentation entry. Finally, summarize in Outcomes the two sentences
a consumer needs — the ordering guarantee per function and the retention warning — so keiro's plans
116 and 118 can quote them instead of paraphrasing.


## Concrete Steps

Run everything from the repository root (`mori path mori://shinzui/pgmq-hs`).

```bash
# 1. Confirm what the sibling plans actually shipped before writing about it.
cat pgmq-migration/migrations/manifest
sed -n '1,40p' pgmq-migration/migrations/0007-*.sql
grep -n 'Decision:' docs/plans/19-give-the-grouped-reads-a-deterministic-return-order.md
grep -n -A 20 '## Validation and Acceptance' docs/plans/20-replace-the-fifo-gin-index-with-one-the-grouped-reads-can-use.md
```

```bash
# 2. Re-read the source of truth for each claim you are about to write.
sed -n '126,205p'   vendor/pgmq/pgmq-extension/sql/pgmq.sql   # read_grouped_rr
sed -n '245,290p'   vendor/pgmq/pgmq-extension/sql/pgmq.sql   # read_grouped_head
sed -n '381,475p'   vendor/pgmq/pgmq-extension/sql/pgmq.sql   # read_grouped
sed -n '1440,1525p' vendor/pgmq/pgmq-extension/sql/pgmq.sql   # create_partitioned's part_config writes
```

```bash
# 3. Edit the documents.
$EDITOR docs/design/008-fifo-read.md
$EDITOR docs/user/queue-configuration.md
$EDITOR pgmq-config/src/Pgmq/Config/Types.hs
```

```bash
# 4. Sweep for stragglers and verify nothing else still claims durability or GIN usefulness.
grep -rn 'retention' docs/ pgmq-config/src pgmq-hasql/src pgmq-core/src pgmq-effectful/src
grep -rni 'gin' docs/ pgmq-*/src
grep -rn 'strict ordering' docs/
```

```bash
# 5. Build (haddock edits are code edits) and run the suites.
cabal build all
nix develop .#partman --command cabal test all
```

```text
cabal build all: OK
pgmq-config-test:    OK
pgmq-hasql-test:     OK
pgmq-migration-test: OK
pgmq-effectful-test: OK
```

Update this section with what you actually ran, and paste the final `grep` results as the evidence
that the sweep is complete.


## Validation and Acceptance

1. **Every ordering claim is traceable.** For each sentence in `docs/design/008-fifo-read.md` that
   asserts an ordering or blocking behavior, name its evidence: a line in the vendored SQL, an
   override migration, or a transcript in
   `docs/plans/20-replace-the-fifo-gin-index-with-one-the-grouped-reads-can-use.md`. List the
   mapping in Outcomes. Any sentence without evidence must be deleted or rewritten.
2. **The head-read guidance exists and is findable.** A reader looking for "strict per-group order
   with a batch larger than one" finds `read_grouped_head` named as the answer in the design note,
   and the note explains the head-blocks-its-group mechanism rather than only asserting it.
3. **The index claim matches measurement.** The note states what the FIFO index accelerates in terms
   of the predicates it serves, cites EP-2's evidence, and — if EP-2 found a grouped read the index
   cannot help — says which one.
4. **Retention warning reaches both audiences.** `docs/user/queue-configuration.md` warns adjacent
   to the code example, and `PartitionConfig`'s haddock warns in the type. Both mention the archive
   table as well as the queue table.
5. **No straggler claims.** `grep -rn 'retention' docs/ pgmq-*/src`, `grep -rni 'gin' docs/ pgmq-*/src`,
   and `grep -rn 'strict ordering' docs/` produce no remaining statement that contradicts M1 or M2.
   Paste the output.
6. **Nothing else regressed.** `cabal build all` succeeds and
   `nix develop .#partman --command cabal test all` is green — haddock edits can break the build, and
   a doc-only plan is still a code change.


## Idempotence and Recovery

Every change here is a documentation or haddock edit; re-running any step is safe, and there is no
database or schema state involved. If a correction turns out to be wrong, fix it in place — these
files are not append-only ledgers.

The one real hazard is writing ahead of the evidence. If this plan is executed before EP-1 and EP-2
have landed, its ordering and index sentences will describe intentions rather than behavior, which
is the exact failure mode it exists to remove. If you find yourself needing to write "will" rather
than "does", stop and record in Progress that M1 is blocked on the sibling plan, rather than
documenting a promise.

If M3 concludes that the FIFO contract needs its own ADR and that ADR would contradict
[`docs/adr/pgmq-1.12-1.13-compatibility.md`](../adr/pgmq-1.12-1.13-compatibility.md), do not
silently supersede the existing record: state the relationship explicitly in the new one.


## Interfaces and Dependencies

No API changes. The only code edit is haddock on an existing type:

```haskell
-- pgmq-config/src/Pgmq/Config/Types.hs
data PartitionConfig = PartitionConfig
  { partitionInterval :: !Text,
    retentionInterval :: !Text,   -- documentation added; type and semantics unchanged
    premake :: !(Maybe Int32)
  }
```

Files this plan changes:

- `docs/design/008-fifo-read.md`
- `docs/user/queue-configuration.md`
- `pgmq-config/src/Pgmq/Config/Types.hs` (haddock only)
- `docs/adr/pgmq-1.12-1.13-compatibility.md` or a new `docs/adr/` record, per M3
- `docs/capabilities/declarative-queue-reconciliation.md` and any other file the M2 sweep implicates
- `CHANGELOG.md`

Files this plan must not change: anything under `vendor/`, any migration under
`pgmq-migration/migrations/`, and the SQL or statement code owned by
`docs/plans/19-give-the-grouped-reads-a-deterministic-return-order.md` and
`docs/plans/20-replace-the-fifo-gin-index-with-one-the-grouped-reads-can-use.md`.

Dependencies: the two sibling plans must be complete, since this plan documents what they shipped.
No new package dependencies.
