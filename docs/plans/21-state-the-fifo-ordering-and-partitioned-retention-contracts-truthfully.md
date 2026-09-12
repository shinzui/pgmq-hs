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
      at: 2026-09-12T15:04:19Z
      verdict: "comments"
      note: "Repository review findings applied in this revision; SQL regressions and performance measurements remain explicit implementation acceptance work."
---

# State the FIFO ordering and partitioned-retention contracts truthfully

This ExecPlan is a living document; implementation is unstarted.

## Purpose / Big Picture


Explain actual FIFO selection/processing limits, the Haskell client's result order, optional
additive indexing, and destructive partition retention. No extension-owned SQL functions may
be overridden. Adding a separately owned index is permitted. Do not claim a native-only FIFO
patch or describe new override-maintenance machinery.

## Progress


- [ ] M1: Correct FIFO semantics, client/raw-SQL distinction and obsolete capability/API claims.
- [ ] M2: Document time/numeric queue/archive retention beside configuration and direct APIs.
- [ ] M3: Integrate client-ordering and optional supplemental-index evidence; validate docs/Haddocks and capability metadata.

## Surprises & Discoveries


Current user guides already expose grouped heads, while older capability pages contain obsolete
constructor/export claims. Retention can be numeric, not just time-based. Earlier versions of
this initiative proposed local function overrides; the user prohibited them and permitted an
additive index. Documentation must reflect that explicit scope change throughout.

## Decision Log


2026-09-12: Document Haskell grouped/head ascending-msg_id results after EP-1, while raw SQL
function ordering remains upstream-owned. Round-robin keeps its layering. Explain the client
contract without promising group-contiguous output or successful processing order.

2026-09-12: Publish EP-2's index only if measurement supports it; name it separately from
upstream's GIN and state that withFifoIndex still controls the conventional index. No helper
replacement, automatic index installation, new native migration or override workflow.

## Outcomes & Retrospective


Plan revised; user-facing documentation and behavior changes remain to be implemented.

## Context and Orientation


`pgmq-hasql/src/Pgmq/Hasql/Statements/Message.hs` wraps upstream reads. EP-1 changes four
outer SELECTs to sort grouped/head results by msg_id; round-robin remains unchanged.
`pgmq-hasql/src/Pgmq.hs` and `pgmq-effectful/src/Pgmq/Effectful.hs` expose all six operations.
Read actual argument records in `pgmq-hasql/src/Pgmq/Hasql/Statements/Types.hs` when correcting
examples. `docs/user/effectful-grouped-reads.md` and `docs/user/pgmq-0.6-upgrade.md` already
contain current head-read examples. Older docs/capabilities/fifo-message-group-reads.md and
effectful-integration.md contain stale export claims.

`vendor/pgmq/pgmq-extension/sql/pgmq.sql` is the upstream baseline. Grouped reads may return
several visible messages per eligible group. Round-robin also may return several, layered
across groups. Both may skip invisible later messages. Heads return at most one absolute head
per group; an invisible head blocks that group, and lease expiry can redeliver the same head
while an old worker still runs. Consumer renewal, acknowledgement timing and side effects
remain the consumer's responsibility. Stopping later same-group processing after failure is
necessary for a batched consumer but does not independently establish a server FIFO guarantee.
IDs define queue order, not concurrent producer transaction commit order.

EP-2 measures a separately named supplemental group-expression index. Upstream's GIN and
helper remain unchanged; conventional FIFO-index reports mean presence, not performance.
The supplement is explicit operator DDL with separate inspection/removal, not a second
reconciliation subsystem. Incorporate negative measurements and write/storage costs.

Both upstream create_partitioned and historical native migration 0006 configure queue/archive
parents with retention_keep_table=false. Maintenance can drop eligible whole partitions without
checking read_ct, vt or processing completion. Time queue partitions use enqueued_at; archives
use archived_at. Numeric parents use msg_id and numeric retention distance, not a duration or
exact retained row count. Boundaries, successful maintenance and operator pg_partman settings
affect actual deletion; do not promise a precise per-message expiry time or archive permanence.

The [compatibility ADR](../adr/pgmq-1.12-1.13-compatibility.md) records stock 1.12/1.13 support
and immutable history. Follow the [FIFO boundary ADR](../adr/fifo-native-overrides-and-index-upgrade-boundary.md),
[vendoring policy](../design/012-vendor-upstream-pgmq-sql.md) and
[reconciliation policy](../design/018-reconciliation-contract.md). The existing historical
notification/partition overrides are not rewritten or extended by this initiative.

## Plan of Work


### M1: Accurate FIFO and client API guidance


Correct docs/design/008-fifo-read.md and affected public Haddocks in Message.hs and
pgmq-effectful/src/Pgmq/Effectful/Effect.hs. Distinguish selection, lease, result sequence and
processing success. All six operations must have accurate batch/visibility behavior; heads are
the primitive for one message per group per batch, not an exactly-once guarantee.

After EP-1, state that four Haskell grouped/head wrappers order by msg_id independently of
raw function output. Interleaved A1, B1, A2, B2 IDs remain interleaved, not group-contiguous.
Do not describe a server patch or change round-robin layering. Update the two stale capability
pages and docs/user/effectful-grouped-reads.md, preserving historical release context in the
0.6 upgrade guide. Label old proposed API examples as historical or replace them with actual
public declarations. Acceptance: consistent source-backed contracts and real constructor examples.

### M2: Retention where callers choose it


In docs/user/queue-configuration.md, explain unread/in-flight and archive loss adjacent to
PartitionConfig. Cover both time and numeric modes and distinct queue/archive control columns.
Examples illustrate settings, not a universally safe retention policy. Add concise matching
Haddocks to pgmq-config/src/Pgmq/Config/Types.hs and direct partition records/operations in
pgmq-hasql/src/Pgmq/Hasql/Statements/Types.hs and QueueManagement.hs.

Sweep docs/user/schema-migration.md, docs/user/pgmq-0.6-upgrade.md and
docs/capabilities/declarative-queue-reconciliation.md for misleading retention/archive claims.
State that partition settings are creation-only for ensureQueues and are not later checked or
repaired. Preserve pristine vendor material and historical plans as evidence rather than
rewriting their history. Acceptance: both high-level config and direct API callers encounter
the same accurate retention semantics.

### M3: Index guidance and final integration


Read EP-1 and EP-2 outcomes before documenting completed behavior. Publish EP-2's supported
supplemental-index create/inspect/remove procedure in queue-configuration.md and
schema-migration.md if it demonstrated useful benefit. Use its distinct name, ownership,
locking/partition limits, measurements and storage/write costs. Explain that upstream's GIN
remains and withFifoIndex does not manage this extra index. If results are negative, document
that limitation without claiming an index improvement shipped.

Clarify presence reporting in docs/design/018-reconciliation-contract.md without changing the
policy. Future upstream imports use the established pristine-source/append-only process;
there are no new FIFO overrides to rebase, replay or retire. A relevant upstream query/index
change may justify remeasurement and explicit removal of a redundant supplement. It does not
justify rewriting upstream functions. Keep the FIFO ADR aligned with actual outcomes and add
root/affected package documentation changelog entries without inventing release numbers.

Capability files belong to the profiled bundle in mori.dhall. Preserve stable handles, read
docs/capabilities/profile.dhall, update revision timestamps/log entries per existing conventions
and run strict validation. ADRs are plain Markdown without a profiled bundle.

## Concrete Steps


Run from the repository root after source review and documentation edits:

```bash
rg -n 'readGrouped|ReadGrouped' pgmq-hasql/src/Pgmq.hs pgmq-effectful/src/Pgmq/Effectful.hs pgmq-hasql/src/Pgmq/Hasql/Statements/Types.hs
rg -n -i 'retention|strict order|archive.*forever|GIN|not.*export' docs/user docs/design docs/capabilities pgmq-config/src pgmq-hasql/src pgmq-effectful/src
okf validate docs/capabilities --strict --profile docs/capabilities/profile.dhall --profile-enforce --log-enforce
cabal haddock pgmq-config pgmq-hasql pgmq-effectful
cabal build all
```

Expected: valid capability metadata, correct examples, rendered Haddocks without new broken
links and a successful build. Record actual outputs and unrelated pre-existing warnings.
Reuse the children's relevant behavioral evidence instead of rerunning expensive SQL suites
solely for prose edits. Do not claim tests or measurements that were not run.

## Validation and Acceptance


Every current claim maps to upstream SQL, the actual client wrapper or a retained measurement.
Client order is not misrepresented as a raw-SQL/server guarantee. The supplement has separate
ownership and no helper/GIN replacement. Retention covers time/numeric and queue/archive cases.
There are no instructions to introduce FIFO override migrations, body exceptions or override
maintenance tooling. Links, examples, Haddocks and capability metadata validate.

## Idempotence and Recovery


Documentation edits are reversible; preserve stable capability handles, provenance and historical
migration bytes. If child outcomes change, update current claims together. No server migration,
upstream submission, Hackage publication or downstream message sending is part of this child.

## Interfaces and Dependencies


Own current FIFO/retention docs, relevant public Haddocks, capability records, final ADR/design
integration and documentation changelog entries. No API signatures or retention behavior change.
Plans 19 and 20 are independent implementation inputs needed for final integration, not hard
drafting dependencies. Consumer policy remains with
mori://shinzui/keiro/plans/116-enforce-fifo-group-ordering-under-failure-and-batched-consumption
and mori://shinzui/keiro/plans/118-correct-partitioned-retention-semantics-and-the-fifo-index.

Revision note (2026-09-12): Removed every proposed server-override/retirement workflow; aligned
contracts with client-only ordering, optional additive indexing and the user's strict SQL boundary.
