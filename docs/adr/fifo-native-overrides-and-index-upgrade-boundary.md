# FIFO client ordering and supplemental index boundary

## Status

Accepted, revised 2026-09-12 following the user's explicit prohibition of extension SQL
overrides and permission to add an index. Supersedes this record's earlier conditional
native-override and retirement design. The filename is retained as a stable reference.
Implementation remains planned in
[MasterPlan 5](../masterplans/5-correct-the-fifo-grouped-read-ordering-index-and-partition-retention-contracts.md).

## Context

Copied upstream function bodies add recurring upgrade maintenance and can hide or replace new
upstream behavior. A supplemental index has a narrower relationship with upstream: query changes
may affect its usefulness, but it does not replace function logic. It still has storage/write
costs and depends on table columns; it is not immune to schema changes.

## Decision

Do not override extension-owned SQL functions for FIFO work, on native or extension installs.
No new copied function bodies, migrations installing them, convergence exceptions, override
inventories or reapplication mechanisms. Keep pristine upstream source and normal upstream
upgrade adoption. The [compatibility ADR](pgmq-1.12-1.13-compatibility.md) still governs
stock 1.12/1.13 support and immutable existing history; historical notification/partition
overrides are not removed or extended by this initiative.

Order the four Haskell grouped/head query results with outer ORDER BY msg_id. This guarantees
ascending IDs in returned vectors, not group contiguity, raw-SQL function order, producer commit
order or successful processing order. Round-robin preserves upstream layering. Selection,
locking and leasing remain entirely upstream-owned.

A separately named q_<queue>_group_lookup_idx may be recommended after complete-query measurement.
Keep upstream q_<queue>_fifo_idx, create_fifo_index helpers and presence-report semantics
unchanged. The supplement is an explicit operator create/inspect/remove action, not automatically
managed by withFifoIndex, reconciliation, migrations or vendor refreshes. Validate ordinary,
unlogged and partitioned behavior and document locks, redundancy and write/storage costs.

Document destructive time/numeric partition retention without changing it. Queue/archive time
parents use enqueued_at/archived_at; numeric parents use msg_id. Processing/acknowledgement state
does not protect eligible partitions from retention maintenance.

## Consequences and verification

Test client result order against unchanged supported stock/native SQL. Test supplemental-index
coexistence, lifecycle and unchanged upstream function definitions/reporting. A future relevant
upstream query/index change calls for measurement and possibly explicit removal of the supplement,
not a function-body rebase. Negative performance evidence is a valid documentation outcome.
No new override-maintenance subsystem is necessary.

## Alternatives

Conditional local overrides and a drift/retirement registry were rejected by the user constraint.
Replacing upstream GIN/helper behavior would also change ownership and complicate upgrades.
Adding the ORDER BY in the client query is bounded and preserves upstream selection. Claiming
raw-SQL order from that client change was rejected; a server-level correction belongs upstream.

See [vendoring policy](../design/012-vendor-upstream-pgmq-sql.md) and
[reconciliation policy](../design/018-reconciliation-contract.md).
