# FIFO native overrides and explicit index upgrade boundary

## Status

Accepted scope and compatibility boundary, 2026-09-12. SQL implementation remains planned in
[MasterPlan 5](../masterplans/5-correct-the-fifo-grouped-read-ordering-index-and-partition-retention-contracts.md).
This acceptance records the constraints on that work, not a claim that its migrations or
performance experiments have completed. The repository uses plain-Markdown ADRs.

## Context

The native migration package and direct clients have different responsibilities. Local SQL
migrations do not update a stock PGMQ extension, while clients support stock 1.12 and 1.13
per the [compatibility ADR](pgmq-1.12-1.13-compatibility.md). Upstream source is owned by
mori://pgmq/pgmq/repos/pgmq-upstream; its project-relative pgmq-extension/sql/pgmq.sql has
an artifact-level URI pending.

The vendored grouped/head reads have unordered UPDATE output. The conventional FIFO index
is a GIN on headers rather than the extracted group expression. The
[reconciliation contract](../design/018-reconciliation-contract.md) intentionally reports
index existence and skips existing resources. Treating every legacy GIN as absent in the
client would cause repeated create calls against stock SQL, which still creates GIN.

## Decision

Scope local result-order guarantees to databases that applied the corresponding native
migration. Preserve stock extension support and never infer server patch state merely from
a Haskell package version. Result order is distinct from consumer processing order and lease
safety. Grouped heads lease at most one message per group; consumers still own renewal,
side effects, acknowledgement and redelivery handling.

Keep the conventional FIFO index name and the existing existence meaning of
listFifoIndexQueueNames and CreatedFifoIndex/SkippedFifoIndex. An index-presence report is
not a definition or performance certificate. If an expression index proves useful, install
its helper through a separate append-only migration; do not automatically rebuild indexes
on existing queues in that migration or in ordinary startup reconciliation. Explicit native
create_fifo_index calls may upgrade only the exact known legacy definition, atomically and
with tested serialization, while rejecting unexpected conflicting objects. Operators choose
when to acquire build locks. Stock servers retain their own implementation.

Retain partition retention semantics and creation-only reconciliation. Document destruction
of eligible whole queue/archive partitions without checking processing state. Time-based
queue/archive parents use enqueued_at/archived_at; numeric parents use msg_id. Do not promise
per-message expiry timing or archive permanence. Performance claims require complete-query
measurements and must disclose remaining full-backlog work; a changed scan node is insufficient.

## Consequences and verification

The ordering plan tests SQL, polling and native client results without changing selection.
The index plan must cover native legacy/new/absent states, stock 1.12/1.13 reconciliation,
explicit upgrade retry/rollback, and partitioned indexes. A negative performance experiment
may lead to documentation without a replacement index. Documentation must state that the
helper migration alone leaves existing indexes unchanged.

Historical migration payloads and 1.12 checkpoint exceptions remain immutable in meaning.
New exceptions belong only to the latest migrated state and need focused behavioral tests.
Changing an excepted body also requires preserving the convergence comparator's sentinel
coverage. See the [vendoring policy](../design/012-vendor-upstream-pgmq-sql.md).

## Alternatives

A global definition-aware presence filter was rejected because it cannot make stock server
DDL produce a native replacement and changes the public report contract. Automatically
rebuilding all queues was rejected because it expands startup/migration lock scope and the
adopted no-conversion boundary. A new public capability/repair-report API is unnecessary for
this initiative; it would require a separately justified compatibility design.
