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
  revisions:
    - model: "gpt-6-astra"
      harness: "codex-cli"
      at: 2026-09-12T15:04:19Z
      mode: "update"
      note: "Validated against repository SQL, tests and ADRs; corrected native/stock scope, ledger coordination, index evidence and retention contracts."
  reviews:
    - model: "gpt-6-astra"
      harness: "codex-cli"
      at: 2026-09-12T15:04:19Z
      verdict: "comments"
      note: "Repository review findings applied in this revision; SQL regressions and performance measurements remain explicit implementation acceptance work."
---

# State the FIFO ordering and partitioned-retention contracts truthfully

This ExecPlan is a living document. Documentation may be drafted independently; its final
statements must agree with the implemented SQL and measured index outcome.

## Purpose / Big Picture


Help a caller choose a grouped-read strategy without confusing leased-message order with
successful processing order, and help an operator choose partition retention knowing that
unprocessed queue rows and archived rows can be destroyed. Explain which behavior belongs
to native local overrides and which remains stock PGMQ behavior.

The original concerns are valid, but the documentation task is broader than two paragraphs.
Current guides already expose grouped heads while older design/capability pages contradict
them. Correct those inconsistencies and state the index's measured limits. Do not claim that
head reads alone guarantee application processing order, or that numeric retention is a duration.

## Progress


- [ ] M1: Correct FIFO behavior, native/stock scope, API examples and stale capability/export claims.
- [ ] M2: Document time and numeric partition retention, archive control columns, maintenance timing and loss risk beside user-facing configuration.
- [ ] M3: Integrate SQL/index outcomes, explicit legacy upgrade guidance, ADR/design references and changelogs; validate links, Haddocks and affected capability profile.

## Surprises & Discoveries


2026-09-12 repository validation: `docs/user/effectful-grouped-reads.md` and
`docs/user/pgmq-0.6-upgrade.md` already document grouped heads. In contrast,
`docs/capabilities/fifo-message-group-reads.md` has an obsolete ReadGrouped example and
claims the umbrellas do not export grouped reads. `docs/capabilities/effectful-integration.md`
also carries obsolete export claims. These are concrete sweep targets, not evidence that
head-read documentation is absent everywhere.

The native create_partitioned override preserves queue/archive retention configuration.
Time partitions use enqueued_at versus archived_at; numeric partitions use msg_id in both.
The default numeric examples cannot be explained as messages expiring after a number of days.

## Decision Log


2026-09-12: Make the documentation independently draftable with explicit current behavior.
Final integration depends on the actual outcomes of plans 19 and 20, including a documented
negative index experiment. Never describe a planned override as installed or published.

2026-09-12: Distinguish result sequence, selection eligibility, lease ownership and successful
processing. Head reads reduce a batch to one message per group but cannot enforce a worker's
side effects, lease renewal or acknowledgement timing. Retention policy and consumer failure
handling remain consumer responsibilities.

2026-09-12: Preserve retention behavior and the reconciler's creation-only partition settings.
Document destructive defaults without adding a new validating constructor or reconfiguring
existing queues. Preserve index-existence reporting and explain the explicit native upgrade.

## Outcomes & Retrospective


Plan validation complete; user-facing documentation has not yet been rewritten by this plan.

## Context and Orientation


The checkout's libraries declare 0.6.0.0; native SQL comes from pristine vendored PGMQ 1.13.0
plus six migrations in `pgmq-migration/migrations/manifest`. The direct Haskell client supports
stock PGMQ 1.12/1.13 too. Its public session umbrella is `pgmq-hasql/src/Pgmq.hs`; the effect
umbrella is `pgmq-effectful/src/Pgmq/Effectful.hs`. Both expose all six grouped operations.
Argument records live in `pgmq-hasql/src/Pgmq/Hasql/Statements/Types.hs`; copy real constructor
shapes from there when correcting examples, rather than relying on the old design proposal.

`vendor/pgmq/pgmq-extension/sql/pgmq.sql` is the baseline source. `read_grouped` prioritizes
eligible groups by oldest visible message, excludes a group with an earlier invisible message,
and may return multiple visible messages per group. It can skip invisible messages later in
an otherwise eligible group. `read_grouped_rr` also may return multiple messages per group,
layered across eligible groups, with an existing selection_order output sort. An invisible
absolute head blocks its group, but invisible later messages can still be skipped. Neither
batching strategy is made strict simply by processing the returned list sequentially.

`read_grouped_head` selects the absolute minimum msg_id per group regardless of visibility,
returns at most one eligible head per group, and leaves a still-present head blocking later
messages until it is removed. An expired lease can allow the head itself to be redelivered
while the original worker still runs. Workers must preserve a live lease, finish work before
acknowledging, tolerate redelivery and coordinate use of other read/delete APIs. IDs define
queue order; concurrent transaction commit order is not guaranteed. Failure handling that
abandons later same-group messages is necessary for batched application processing, but is
not by itself a complete server FIFO guarantee.

Stock read_grouped/head result order is unspecified. Plan 19's proposed native 0007 adds
selected-group priority then per-group msg_id order for read_grouped and ascending msg_id
for heads. Polling delegates to base functions. A1, B1, A2, B2 sends produce grouped
A1, A2, B1, B2 after the override, not a global ID sort. Round-robin's existing layered
order is different. State the exact required native ledger entry when documenting this.

Plan 20 measures an expression-index candidate and may append native 0008. It keeps the
conventional index name and name-based existence reporting. Legacy GIN remains until an
operator explicitly upgrades it; stock servers keep their own GIN helper. An index-presence
report is not an assurance of performance. Carry over any negative measurement.

Retention comes from `create_partitioned` and
`pgmq-migration/migrations/0006-preserve-partitioned-reentry-v1.13.0.sql`. Both parent settings
use retention_keep_table=false without testing read_ct, vt or application completion. Time
queue partitions are controlled by enqueued_at, time archive partitions by archived_at;
numeric parents use msg_id. During successful retention maintenance, eligible whole partitions
may be dropped regardless of processing. No precise per-row expiry time is promised.
User-modified retention_schema/retention_keep_table settings can change the disposition;
default partitions, maintenance failures and partition boundaries affect actual cleanup.
Do not describe an outage longer than the interval as guaranteed immediate deletion.

Use the [compatibility ADR](../adr/pgmq-1.12-1.13-compatibility.md) for supported server modes,
the [FIFO boundary ADR](../adr/fifo-native-overrides-and-index-upgrade-boundary.md) for local
ordering/index scope, [vendoring policy](../design/012-vendor-upstream-pgmq-sql.md) for immutable
SQL, and [reconciliation policy](../design/018-reconciliation-contract.md) for existence and
creation-only checks. Upstream source ownership is `mori://pgmq/pgmq/repos/pgmq-upstream`;
project-relative `docs/partitioned-queues.md` has an artifact URI pending and contains an
archive-forever claim contradicted by the SQL. Use SQL as the local behavior evidence.

## Plan of Work


### M1: Accurate FIFO contract and current API surface


Rewrite the behavioral sections in `docs/design/008-fifo-read.md`. Label historical proposed
code as historical, or replace it with current compiled API examples. Explain all six read
operations, how many messages a group can contribute, visibility gaps, ordering for each
installation mode and consumer obligations. Head reads are the preferred primitive when a
batch should contain at most one message per group, not a promise of exactly-once processing.

Update `docs/user/effectful-grouped-reads.md`, relevant public Haddocks in
`pgmq-hasql/src/Pgmq/Hasql/Statements/Message.hs` and
`pgmq-effectful/src/Pgmq/Effectful/Effect.hs`, and
`docs/capabilities/fifo-message-group-reads.md` plus
`docs/capabilities/effectful-integration.md`. Verify their real file paths and declarations
before editing. Correct the obsolete constructor example and umbrella-export claims. Read
`docs/user/pgmq-0.6-upgrade.md` for existing accurate examples and preserve its historical
release context. Add new migration-specific guidance instead of implying the original 0.6
release contained later fixes. Acceptance: all current grouped-read descriptions agree with
source and installation mode, and examples use the actual public API.

### M2: Retention where the operator chooses it


In `docs/user/queue-configuration.md`, put the data-loss explanation adjacent to the
PartitionConfig example. Document both supported interval modes: time is based on queue
send time versus archive time, numeric retention uses message-ID distance and partition
boundaries rather than a wall-clock duration or exact retained row count. Explain that
unread/in-flight work and archived rows can be dropped during maintenance. Choose examples
as illustrations, not a universally safe retention recommendation.

Add equivalent concise Haddocks to PartitionConfig.retentionInterval in
`pgmq-config/src/Pgmq/Config/Types.hs` and to the partition-creation record/operations in
`pgmq-hasql/src/Pgmq/Hasql/Statements/Types.hs` and relevant QueueManagement Haddocks.
Update `docs/user/schema-migration.md`, numeric examples in `docs/user/pgmq-0.6-upgrade.md`,
and `docs/capabilities/declarative-queue-reconciliation.md` where they imply weaker risks.
Distinguish creation-time config from later operator pg_partman settings: ensureQueues does
not validate or repair retention. Sweep current public docs/Haddocks for archive-forever or
acknowledgement-based retention claims. Historical plans and pristine vendor docs are evidence,
not text to rewrite wholesale. Acceptance: callers encounter the loss semantics at both the
configuration and direct API entry points, with time/numeric and queue/archive distinctions.

### M3: Final integration, operator handoff and durable records


Read the completed outcomes of plans 19 and 20 before finalizing migration-specific claims.
If implementation is still pending, label current stock/native baseline behavior and keep the
final integration checkbox open. Publish EP-2's measured scope and explicit native legacy-index
upgrade procedure in `docs/user/queue-configuration.md` and `docs/user/schema-migration.md`.
Explain that an existing GIN is skipped by ordinary reconciliation and that applying the helper
migration alone does not replace it. If the experiment rejected replacement, document that
outcome instead of inventing migration 0008 or an upgrade command.

Update `docs/design/018-reconciliation-contract.md` to clarify existence is not definition or
performance validation. Update `docs/design/012-vendor-upstream-pgmq-sql.md` and the existing
compatibility ADR's current-state convergence references to distinguish the unchanged historical
checkpoints from the new latest exceptions. Retain historical acceptance evidence as historical.
Distill actual outcomes into the FIFO boundary ADR; do not mark proposed SQL implemented
before its tests pass. Add root and affected package changelog entries without overwriting
published history or selecting an unverified release number.

Capability pages belong to a profiled OKF bundle in mori.dhall. Preserve their stable handles,
read `docs/capabilities/profile.dhall` and existing index/log conventions, update meaningful
revision timestamps/log entries, and run strict validation. Do not invent ADR OKF metadata:
this repository's ADRs are plain Markdown with no profiled bundle.

## Concrete Steps


Run from the repository root. Read exact APIs before editing prose or examples.

```bash
cat pgmq-migration/migrations/manifest
rg -n 'readGrouped|ReadGrouped' pgmq-hasql/src/Pgmq.hs pgmq-effectful/src/Pgmq/Effectful.hs pgmq-hasql/src/Pgmq/Hasql/Statements/Types.hs
rg -n -i 'retention|strict order|archive.*forever|GIN|not.*export' docs/user docs/design docs/capabilities pgmq-config/src pgmq-hasql/src pgmq-effectful/src
# After editing affected pages and their bundle metadata:
okf validate docs/capabilities --strict --profile docs/capabilities/profile.dhall --profile-enforce --log-enforce
cabal haddock pgmq-config pgmq-hasql pgmq-effectful
cabal build all
```

Expected: bundle validation succeeds, relevant Haddocks render without new unresolved links,
and public examples refer to real constructors/exports. Record any existing unrelated warnings
separately. Use the existing umbrella compile witnesses or compile edited API examples when
necessary. Documentation-only work does not need to repeat expensive SQL suites already passed
by the SQL children; final integration retains their test evidence. If a claimed behavior cannot
be traced to source, use a small disposable SQL experiment rather than asserting it.

## Validation and Acceptance


Every current ordering/index claim maps to the stock SQL, an applied native migration, or a
retained measured result. Head-read guidance explains leases and consumer obligations rather
than promising processing success. Retention guidance covers unread work, archives, numeric
and time partitioning and maintenance timing at both high-level configuration and direct API
surfaces. Existence reporting matches stock/native behavior, and an operator can distinguish
migration application from physical legacy-index upgrade. Capability profile validation,
Haddocks and API-example checks pass. Record the claim-to-evidence mapping and precise
installation requirements in Outcomes.

## Idempotence and Recovery


Prose/Haddock edits are reversible. Never rewrite pristine vendor SQL/docs or historical
migration bytes to make a claim appear true. Preserve capability handles and existing plan
provenance. If sibling SQL changes during drafting, revise all current docs together before
final integration. No database migration, Hackage publication or downstream message sending
is part of this child. Consumers can read its final Outcomes through the parent handoff.

## Interfaces and Dependencies


No public Haskell signatures or retention behavior change. Own current FIFO/retention user
docs, relevant public Haddocks, capability records and metadata, final ADR/design references
and documentation changelog entries. SQL and performance implementation belong to
`docs/plans/19-give-the-grouped-reads-a-deterministic-return-order.md` and
`docs/plans/20-replace-the-fifo-gin-index-with-one-the-grouped-reads-can-use.md`.
These are soft drafting dependencies and required final integration inputs. No new package
bounds are chosen. Downstream consumer policy remains with
`mori://shinzui/keiro/plans/116-enforce-fifo-group-ordering-under-failure-and-batched-consumption`
and `mori://shinzui/keiro/plans/118-correct-partitioned-retention-semantics-and-the-fifo-index`.

Revision note (2026-09-12): Corrected unsupported strict-FIFO claims, added lease/visibility-gap
limits and native/stock distinctions, covered numeric/time and archive retention, identified
stale capability/API pages, and made documentation independently draftable with final integration.
