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
    - model: "gpt-6-astra"
      harness: "codex-cli"
      at: 2026-09-12T15:42:32Z
      mode: "update"
      note: "Added upstream-first maintenance gate and focused drift, successor-upgrade and retirement acceptance for proposed FIFO overrides."
    - model: "gpt-6-astra"
      harness: "codex-cli"
      at: 2026-09-12T15:49:24Z
      mode: "update"
      note: "Applied user prohibition of extension SQL overrides; scoped work to client ordering, additive operator index and documentation; removed override machinery."
  reviews:
    - model: "gpt-6-astra"
      harness: "codex-cli"
      at: 2026-09-12T15:04:18Z
      verdict: "comments"
      note: "Repository review findings applied in this revision; SQL regressions and performance measurements remain explicit implementation acceptance work."
---

# Correct the FIFO grouped-read ordering, index, and partition-retention contracts

This is a living coordination document. Implementation is unstarted.

## Vision & Scope


Improve the Haskell client's returned message order, measure and provide an optional
supplemental index, and correct FIFO/retention documentation. **Do not override extension-owned
SQL functions**, including in the native migration path. Adding an index is permitted.
This supersedes the earlier proposed local overrides and their maintenance framework.

The motivating findings come from
`mori://shinzui/keiro/masterplans/17-harden-keiro-pgmq-fifo-ordering-dlq-operator-paths-and-provisioning-surfaced-by-the-2026-07-pgmq-review`.
In `vendor/pgmq/pgmq-extension/sql/pgmq.sql`, grouped and head reads have unordered
UPDATE output; round-robin already orders its output. The existing FIFO helper creates GIN
on headers rather than an index on the extracted group expression. These are upstream
facts to accommodate, not permission to copy and replace their function bodies.

EP-1 narrows the original SQL guarantee explicitly: direct Haskell grouped/head reads return
ascending msg_id through an outer ORDER BY around the unchanged function call. It does not
promise selected-group contiguity, change selected rows, or fix direct SQL callers' order.
Round-robin retains its upstream layering. EP-2 measures a separately named btree index and,
if useful, provides explicit operator DDL. Existing upstream GIN and its helper remain intact.
EP-3 explains these boundaries and partition maintenance that can drop unread/in-flight and
archived rows. Time queue/archive parents use enqueued_at/archived_at; numeric parents use
msg_id. Neither row sorting nor an index guarantees successful processing order.

No extension-body override, new FIFO migration, convergence exception, override inventory,
fingerprint maintenance, index replacement, automatic startup DDL, or retention behavior change
belongs to this initiative. Existing historical migrations remain immutable; auditing their
older notification/partition overrides is separate work. No upstream release upgrade, Hackage
publication or upstream submission is required. Future upstream adoption follows the normal
pristine-source and append-only migration workflow.

## Decomposition Strategy


Three independent concerns: client result ordering, measured index guidance, and accurate
contracts. EP-1 owns client query changes and regressions. EP-2 owns the experiment and a
bounded operator index procedure. EP-3 integrates their outcomes into current documentation.
There are no reserved 0007/0008 migrations and no shared override mechanism.

The [compatibility ADR](../adr/pgmq-1.12-1.13-compatibility.md) establishes stock 1.12/1.13
support and immutable history. The [vendoring policy](../design/012-vendor-upstream-pgmq-sql.md)
keeps upstream bytes pristine. The [reconciliation contract](../design/018-reconciliation-contract.md)
reports the conventional index's presence. The
[FIFO boundary ADR](../adr/fifo-native-overrides-and-index-upgrade-boundary.md) now records
no function overrides and independently owned supplemental indexes.

## Exec-Plan Registry


| # | Title | Path | Hard Deps | Soft Deps | Status |
|---|-------|------|-----------|-----------|--------|
| 1 | Order grouped/head results in the Haskell client | docs/plans/19-give-the-grouped-reads-a-deterministic-return-order.md | None | None | Not Started |
| 2 | Measure and document a supplemental FIFO index | docs/plans/20-replace-the-fifo-gin-index-with-one-the-grouped-reads-can-use.md | None | None | Not Started |
| 3 | State FIFO and retention contracts accurately | docs/plans/21-state-the-fifo-ordering-and-partitioned-retention-contracts-truthfully.md | None | EP-1, EP-2 final integration | Not Started |

Filenames are retained as stable references; their earlier server-override wording is historical.

## Dependency Graph


EP-1 and EP-2 have no implementation dependency. EP-3 may correct existing documentation now,
but final integration consumes the two outcomes. A negative index experiment is an acceptable
recorded result; it must not be presented as an optimization that shipped.

## Integration Points


**Client order.** EP-1 owns `pgmq-hasql/src/Pgmq/Hasql/Statements/Message.hs` and relevant
client tests. Change only the four grouped/head direct and polling query wrappers to order
returned rows by msg_id. Both session and effect APIs consume these statements. Preserve
round-robin order and server selection/lease behavior. EP-3 owns final public contract prose.

**Index ownership.** EP-2 owns a reproducible experiment in
`pgmq-migration/test/fixtures/fifo-index-probe.sql` and tested operator DDL recorded in its plan.
Use `q_<queue>_group_lookup_idx`, distinct from upstream's `q_<queue>_fifo_idx` and its catalog
matcher. Do not replace GIN or change create_fifo_index, create_fifo_indexes_all,
listFifoIndexQueueNames, withFifoIndex or reconciliation actions. The supplemental index is
opt-in and separately inspected/removed. EP-3 publishes the supported procedure and limits.

**Documentation and changelogs.** EP-3 owns current FIFO/retention user guides, relevant
Haddocks, capability pages and ADR/design integration. EP-1 supplies ordering test evidence
and starts root/pgmq-hasql Unreleased entries. EP-2 records measurements; EP-3 adds index/docs
entries without claiming a migration or unverified release. No pgmq-migration release change
is implied by an experiment fixture.

**Upstream compatibility.** Existing migration/convergence checks remain intact; no new body
exceptions or copied production SQL. Query-plan experiments may extract current SQL into a
scratch fixture for measurement, labelled with its source and never installed as production
functions. Retain client behavior tests when upstream advances. Index recommendations should
be remeasured when relevant query shapes change, not automatically recreated during upgrade.

## Progress


- [x] 2026-09-12: Repository validation completed and user constraints incorporated.
- [ ] EP-1: Four client wrappers order grouped/head results; stock/native regressions pass.
- [ ] EP-2: Reproducible index experiment and opt-in create/inspect/remove procedure, or explicit negative result.
- [ ] EP-3: Accurate client/raw-SQL, FIFO, index ownership and retention documentation.
- [ ] Final integration: tests, documentation checks, changelogs and durable decisions complete.

## Surprises & Discoveries

- Cross-repository audit (2026-09-12): `mori://shinzui/keiro` was clean at
  `14dd9036` before this audit; its committed MasterPlan 17 and children 116/118
  still prescribed SQL migration adoption and automatic GIN replacement. Updated
  `mori://shinzui/keiro/masterplans/17-harden-keiro-pgmq-fifo-ordering-dlq-operator-paths-and-provisioning-surfaced-by-the-2026-07-pgmq-review`
  and children 116–118 to match this boundary. No conflicting runtime implementation
  was found in that checkout or the clean `mori://shinzui/shibuya-pgmq-adapter`
  checkout at `2b67a65`. These observations cover local checkout state at audit time.


Original plans assumed native function replacements. Those would add recurring upstream
maintenance and are now prohibited. No replacement migrations were implemented. The additional
override-drift/retirement machinery proposed in the preceding revision is also removed.
The existing presence report describes upstream's named FIFO index; it cannot be repurposed
as a performance certificate or supplemental-index detector without changing its contract.

## Decision Log


2026-09-12: User explicitly prohibited extension SQL overrides and permitted adding an index.
Apply that restriction to both native and extension paths. Remove all proposed override files,
ledger dependencies, body exceptions and associated maintenance machinery from this initiative.

2026-09-12: Define client grouped/head order as ascending msg_id using outer query ORDER BY.
This deliberately replaces the earlier selected-group-contiguous SQL guarantee with a smaller
client guarantee, preserving per-group ID order without reproducing upstream selection logic.
Raw function output and round-robin semantics remain upstream-owned.

2026-09-12: Use a separately named, measured supplemental index with explicit operator ownership.
No mutation of upstream's existing index or helper and no automatic provisioning subsystem.

## Outcomes & Retrospective


The plan is updated; no client or index implementation has run. Existing historical migration
bytes remain untouched. Direct SQL return order remains an upstream limitation.

Revision note (2026-09-12): Superseded conditional function overrides with a strict prohibition;
scoped ordering to the client and indexing to optional additive DDL; removed override maintenance
infrastructure and migration dependencies throughout the children.

Revision (2026-09-12): Audited downstream local changes and synchronized Keiro handoffs; aligned child display titles with client ordering and supplemental index scope.
