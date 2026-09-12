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
  revisions:
    - model: "gpt-6-astra"
      harness: "codex-cli"
      at: 2026-09-12T15:04:18Z
      mode: "update"
      note: "Validated against repository SQL, tests and ADRs; corrected native/stock scope, ledger coordination, index evidence and retention contracts."
  reviews:
    - model: "gpt-6-astra"
      harness: "codex-cli"
      at: 2026-09-12T15:04:18Z
      verdict: "comments"
      note: "Repository review findings applied in this revision; SQL regressions and performance measurements remain explicit implementation acceptance work."
---

# Correct the FIFO grouped-read ordering, index, and partition-retention contracts

This MasterPlan is a living document. Keep Progress, Surprises & Discoveries, Decision Log,
and Outcomes & Retrospective current. This revision validates and updates the plan; it does
not implement the SQL changes.

## Vision & Scope


Give native PGMQ installations a defined grouped-read result order, measure and improve the
optional FIFO index where evidence supports it, and document partition retention accurately.
The motivating findings are PGQ-2, PGQ-5, and the server-documentation half of PGQ-4 from
`mori://shinzui/keiro/masterplans/17-harden-keiro-pgmq-fifo-ordering-dlq-operator-paths-and-provisioning-surfaced-by-the-2026-07-pgmq-review`.
Consumer processing, DLQ handling and retention policy remain owned by that project, including
`mori://shinzui/keiro/plans/116-enforce-fifo-group-ordering-under-failure-and-batched-consumption`,
`mori://shinzui/keiro/plans/117-preserve-headers-on-dlq-redrive-and-make-archive-and-purge-visibility-safe`,
and `mori://shinzui/keiro/plans/118-correct-partitioned-retention-semantics-and-the-fifo-index`.

Repository validation on 2026-09-12 confirms the underlying concerns. In
`vendor/pgmq/pgmq-extension/sql/pgmq.sql`, `read_grouped` and `read_grouped_head` end in
unordered `UPDATE ... RETURNING`; `read_grouped_rr` already ends in an ordered SELECT.
The FIFO helper creates a GIN index on headers, which does not index the extracted group-key
expression used by grouped reads. This does not prove every node is a sequential scan:
existing message-ID and visibility indexes can still participate. A replacement index's
benefit and remaining full-backlog work must be measured rather than assumed.

Both the vendored `create_partitioned` and local migration 0006 set
`retention_keep_table = false` for queue and archive parents. Maintenance can therefore
destroy unread and in-flight rows in eligible partitions. Time partitioning uses
`enqueued_at` for queues and `archived_at` for archives; numeric partitioning uses `msg_id`
for both. Retention is partition-level maintenance, not a per-message expiry timer or
acknowledgement policy. An outage can cause loss, but does not guarantee deletion at an
exact elapsed interval. Operator overrides and failed or unscheduled maintenance affect
what happens.

The scope is the append-only native migration ledger, focused SQL/client regression tests,
index upgrade instructions, and current user documentation and Haddocks. The native fixes
are not automatically installed by upgrading a Haskell client or a stock PGMQ extension.
Clients must continue to work with stock PGMQ 1.12 and 1.13. Ordering means ascending
message IDs within a selected group, not concurrent producer commit order or exactly-once
processing. Head reads lease at most one head per group; successful processing, lease renewal,
and acknowledgement remain consumer responsibilities.

Out of scope: changing grouped-read selection, locking or visibility rules; automatic index
replacement across all existing queues during migration/startup; queue retention policy
validation; downstream implementation; Hackage publication; submitting upstream patches.
Prepare reviewable upstream patch artifacts only if SQL changes are implemented. The working
tree declares family version 0.6.0.0; this review does not certify registry publication or
choose new dependency bounds. Release planning must verify Hackage and upstream tags first.

## Decomposition Strategy


Keep three functional work streams: ordered result delivery, measured index provisioning,
and accurate operator/consumer contracts. EP-1 is a small semantic correction with row-order
regressions. EP-2 starts with a performance experiment, then implements only a justified index
and explicit upgrade path. EP-3 documents the outcomes, including a negative index experiment.

EP-1 owns migration 0007; EP-2 owns a separate 0008 if its experiment justifies a migration.
Do not combine their payloads or edit a previously applied file, even before publication.
EP-2's complete implementation follows EP-1 because its ledger acceptance applies that prefix;
its isolated performance experiment may be done earlier. EP-3 may draft retention and existing
server semantics independently, with final integration against both SQL outcomes. This avoids
making an inconclusive performance result block a known documentation correction.

Read the accepted [compatibility ADR](../adr/pgmq-1.12-1.13-compatibility.md), which preserves
1.12 client support, immutable historical migrations, the original 1.11 predecessor validator,
and serial migration testing. [Vendoring policy](../design/012-vendor-upstream-pgmq-sql.md)
requires pristine vendor bytes and separate local overrides.
[Reconciliation policy](../design/018-reconciliation-contract.md) promises index existence
reporting and no automatic conversion of existing resources. The new
[FIFO scope ADR](../adr/fifo-native-overrides-and-index-upgrade-boundary.md) records the
native/extension distinction and explicit index-repair boundary. The existing
[FIFO design note](../design/008-fifo-read.md) is stale implementation-era guidance;
EP-3 corrects it and current capability pages rather than treating it as authoritative SQL.

## Exec-Plan Registry


| # | Title | Path | Hard Deps | Soft Deps | Status |
|---|-------|------|-----------|-----------|--------|
| 1 | Give the grouped reads a deterministic return order | docs/plans/19-give-the-grouped-reads-a-deterministic-return-order.md | None | None | Not Started |
| 2 | Replace the FIFO GIN index with one the grouped reads can use | docs/plans/20-replace-the-fifo-gin-index-with-one-the-grouped-reads-can-use.md | EP-1 (ledger integration; experiment independent) | None | Not Started |
| 3 | State the FIFO ordering and partitioned-retention contracts truthfully | docs/plans/21-state-the-fifo-ordering-and-partitioned-retention-contracts-truthfully.md | None | EP-1, EP-2; final integration required | Not Started |

## Dependency Graph


EP-1 → EP-2 is the native-ledger implementation order. EP-2's experiment has no code dependency
on EP-1. EP-1 and EP-2 both integrate with EP-3: documentation may land with explicit current
behavior and must be reconciled before initiative completion. If EP-2 finds no worthwhile
candidate, record a completed experiment with no DDL change, update its title/registry scope,
and document the limitation. Do not mark the index performance objective achieved without evidence.

## Integration Points


**Native ledger and packaging.** EP-1 owns 0007 and the initial convergence-test refactor;
EP-2 appends 0008 and extends only latest-state exceptions. Both update
`pgmq-migration/migrations/manifest`, the explicit names in `testNativeComponent`, and test
acceptance in `pgmq-migration/test/Main.hs`. Migration SQL is already included by the Cabal
`migrations/*.sql` glob. Preserve 0001–0006 and all upstream fixtures.

**Convergence assertions.** EP-1 keeps the four-entry 1.12 checkpoint and uses all manifest
entries for latest state instead of extending the hardcoded six. New local-body exceptions
apply only to latest state. The test currently also deletes the normalized `read_grouped_head` body to prove missing
functions are detected; after excluding that body, deletion is a no-op and the sentinel fails.
Move the missing/altered-body sentinel pair to an unexcepted function or run it against raw
snapshots. Otherwise a correct ordering override breaks the test. EP-2 adds only its helper body exception if it ships a helper override.
Signatures and results must remain compared even when bodies are excepted.

**Index creation and reporting.** EP-2 owns `_create_fifo_index_if_not_exists`, its tests,
and explicit legacy-upgrade procedure. Keep `q_<queue>_fifo_idx` and the published existence
meaning of `listFifoIndexQueueNames :: Statement () [Text]`. An existing legacy GIN index
remains present, so `SkippedFifoIndex` is truthful about existence, not performance. Explicit
native `create_fifo_index` may upgrade an exact known legacy definition after the operator
chooses to do so; ordinary reconciliation must not silently trigger replacement. Stock
extension installs must converge after two reconciliations without repeated creation reports.
There is no `pgmq.create_fifo` queue-creation function. The only public creation entry points
are `create_fifo_index` and `create_fifo_indexes_all`.

**Documentation and shared changelogs.** EP-3 owns `docs/design/008-fifo-read.md`, affected
user guides, capability records and public Haddocks. EP-1 and EP-2 supply measured results and
migration names in their plans. EP-1 starts the root Unreleased entry; EP-2 appends its distinct
item; EP-3 consolidates without overwriting either. Each SQL child updates
`pgmq-migration/CHANGELOG.md`; changed client packages receive their own entries. EP-3 owns the
final ADR and design-policy cross-reference updates, including convergence exception counts.

**Upstream artifacts.** EP-1 owns `docs/upstream-patches/grouped-read-ordering.patch`; EP-2
owns `docs/upstream-patches/fifo-index.patch` if justified. Base patches on verified source from
`mori://pgmq/pgmq/repos/pgmq-upstream`, initially the vendored v1.13.0 tag, and record the exact
base commit and applicability. Upstream project-relative source path
`pgmq-extension/sql/pgmq.sql` has an artifact-level URI pending. Never rewrite a released
upstream upgrade script as if existing installations would replay it; prepare a prospective
upgrade artifact in the patch and explain its intended release integration.

## Progress


- [x] 2026-09-12: Validated all four plans against SQL, ledger tests, client/reconciler code, and relevant ADR/design context; corrected the decomposition and children.
- [ ] EP-1: Ordered native base reads and polling/client regressions, with unchanged selection semantics.
- [ ] EP-1: Latest convergence and historical checkpoints pass, including comparator sentinels and predecessor imports.
- [ ] EP-2: Reproducible full-query/index experiment completed; measured benefit or negative result recorded.
- [ ] EP-2: If justified, separate index migration, explicit legacy upgrade, retry/partition tests and stock-client compatibility verified.
- [ ] EP-3: Current ordering, index, native/extension and time/numeric retention contracts documented and validated.
- [ ] Final integration: changelogs, upstream patch artifacts, ADR distillation and release-readiness notes complete; publication excluded.

## Surprises & Discoveries


The original four files record `claude-opus-5[1m]` creation and no review entries. Repository
review found valid SQL concerns but missing local context: the comparator sentinel would fail
once the head body was excluded; exceptions were incorrectly requested at the historical
1.12 checkpoint; `create_fifo` does not exist; current user guides already expose grouped
heads; capability pages still describe obsolete exports and constructor shapes.

Changing name-based detection to reject GIN on every server would make a stock extension
recreate or attempt to create its own GIN repeatedly and report creation on every startup.
Overriding the helper alone also does not replace any already-created index. Therefore
provisioning, physical upgrade and existence reporting must be specified separately.

A group-expression btree might improve selective joins without avoiding full group aggregation.
Neither index-only execution nor removal of O(N) work has been demonstrated in this review.
No performance transcript, SQL regression result, release status or publication is inferred.

## Decision Log


2026-09-12: Preserve the three concerns relocated from the canonical downstream MasterPlan
cited in Vision, but ground contracts in this repository's supported server modes and existing
ADR. This prevents a consumer-specific guarantee becoming an unsupported library promise.

2026-09-12: Use separate ordered migration files and latest-only convergence exceptions,
including repair of the comparator sentinel. Applied migration immutability is stronger than
release immutability; publication is not permission to rewrite a database's history.

2026-09-12: Preserve FIFO existence reporting and make legacy index replacement explicit.
This respects the adopted reconciliation policy and stock-server compatibility. Record the
boundary in the FIFO scope ADR now; do not claim the proposed index is implemented.

2026-09-12: Treat EP-2 as an evidence-gated optimization and EP-3 as independently draftable
with final integration. A changed scan node is insufficient proof of a useful optimization.
No improvement means an explicit scope/outcome update, not fabricated successful acceptance.

2026-09-12: Remove publication from completion criteria and defer release-number selection
until authoritative registry/tag verification and an actual compatibility diff are available.

## Outcomes & Retrospective


Plan validation and revision are complete. Implementation remains unstarted. The concrete SQL
ordering and index experiments are future acceptance work, not results of this review.

Revision note (2026-09-12): Reworked the coordination and all three children after repository
validation exposed native/extension, convergence-test, provisioning, performance-evidence and
retention gaps in the externally authored plan.
