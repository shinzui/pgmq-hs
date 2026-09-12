---
id: 19
slug: give-the-grouped-reads-a-deterministic-return-order
title: "Order grouped/head results in the Haskell client"
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

# Order grouped/head results in the Haskell client

This ExecPlan is a living document. Its filename preserves the original plan reference.

## Purpose / Big Picture


Return grouped and grouped-head Haskell results in ascending msg_id order by adding ORDER BY
to the client SELECT around PGMQ's unchanged function call. This preserves ascending IDs within
each group but does not keep groups contiguous or alter which messages the server selects.
Direct SQL callers receive only upstream's guarantees. Round-robin keeps its layered order.
No extension SQL function may be overridden, including through native migrations.

## Progress


- [ ] M1: Add outer ordering to four client statements and precise Haddocks.
- [ ] M2: Verify ordered vectors, unchanged selection/leases, polling and round-robin behavior on supported stock/native fixtures.
- [ ] M3: Record compatibility evidence and root/package changelog entries; hand semantics to documentation.

## Surprises & Discoveries


The four current statements are simple SELECT calls returning Vector Message. An outer ORDER BY
can sort their result without duplicating PGMQ's selection, locks or update logic. Earlier plan
versions instead copied server bodies and promised group-contiguous order; both are superseded.
No migration or ordering implementation has been performed by this planning revision.

## Decision Log


2026-09-12: User prohibits extension-owned SQL overrides. Use client query ordering only and
explicitly narrow the guarantee to ascending msg_id in Haskell results. Do not infer transaction
commit order from IDs or successful processing order from a lease. No native-only guarantee,
new migration, body exception, drift registry or retirement framework is needed.

## Outcomes & Retrospective


Plan updated; implementation and test results pending. Raw SQL return ordering remains upstream-owned.

## Context and Orientation


`pgmq-hasql/src/Pgmq/Hasql/Statements/Message.hs` defines readGrouped,
readGroupedWithPoll, readGroupedHead and readGroupedHeadWithPoll as preparable SELECT calls
with a rowVector decoder. readGroupedRoundRobin and its polling sibling are separate statements.
Sessions and plain/traced effects delegate to these statements; preserve their public signatures.
`pgmq-hasql/test/AdvancedOpsSpec.hs` already covers grouped/head operations. Inspect its fixtures
and the existing effect tests before extending them; do not mutate shared server functions to
force output order during concurrent tests.

`vendor/pgmq/pgmq-extension/sql/pgmq.sql` supplies the baseline server behavior. Grouped reads
may lease several visible messages of a group; heads lease at most one absolute head per group.
Round-robin already returns layered selection order. The native manifest has six historical
entries and remains unchanged by this child. Stock 1.12/1.13 support is established by the
[compatibility ADR](../adr/pgmq-1.12-1.13-compatibility.md). Follow the
[FIFO boundary ADR](../adr/fifo-native-overrides-and-index-upgrade-boundary.md) and
[vendoring policy](../design/012-vendor-upstream-pgmq-sql.md): no new function overrides.
Use Mori before looking up dependency APIs; no new dependency bounds are needed.

## Plan of Work


### M1: Sort the result at the client query boundary


Add an outer `ORDER BY msg_id` to each of the four grouped/head SELECT statements. For example:

```sql
select * from pgmq.read_grouped($1,$2,$3) order by msg_id
```

Do not change encoders, record projections, argument defaults, qty, server definitions or
round-robin queries. Update the four statements' Haddocks to specify ascending msg_id, no group
contiguity guarantee, and unchanged server eligibility. Acceptance: build succeeds and only
the intended client query text/contract changes. A1, B1, A2, B2 with ascending send IDs returns
that ID sequence, not the earlier plan's A1, A2, B1, B2 grouping promise.

### M2: Prove the client guarantee without relying on server output order


Add focused cases to AdvancedOpsSpec or an isolated registered test module. Seed interleaved
groups and assert exact returned vectors for direct and polling reads, using returned send IDs
rather than assumed sequence starts. Cover multiple messages per group, quantity spanning
groups, missing group headers, invisible heads, head quantity exceeding the group count,
empty results and polling timeout. Assert read_ct/visibility change once and selected rows
match the unchanged server eligibility. Do not sort the test result before asserting it.

Use adversarial physical row order in an isolated queue to make the outer ordering useful;
record whether removing the outer ORDER BY causes a failure on the tested PostgreSQL version.
Unspecified baseline output can happen to be sorted, so do not claim a guaranteed mutation
failure. Preserve a round-robin regression whose layered order differs from ascending IDs.
Exercise sessions and existing plain/traced effect paths to confirm the same result contract.
Run supported stock 1.12/1.13 fixtures and native fixtures; do not add a local server patch
just to make a compatibility case pass. Acceptance: ordered client vectors with unchanged
selection and round-robin results, on both installation modes.

### M3: Evidence and consumer handoff


Add Unreleased entries in root CHANGELOG.md and pgmq-hasql/CHANGELOG.md. State precisely that
client queries now sort grouped/head results; no server migration is required or supplied.
Hand the test evidence to docs/plans/21-state-the-fifo-ordering-and-partitioned-retention-contracts-truthfully.md.
Do not claim a release number without authoritative registry/tag checks during release work.
No upstream patch artifact or submission is required to implement this client-only contract.

## Concrete Steps


Run from the repository root after the edits:

```bash
rg -n 'readGrouped|read_grouped' pgmq-hasql/src/Pgmq/Hasql/Statements/Message.hs
cabal build all
nix develop .#partman --command cabal test pgmq-hasql:pgmq-hasql-test --test-options='-j1' --test-show-details=direct
nix develop .#partman --command cabal test pgmq-effectful:pgmq-effectful-test --test-options='-j1' --test-show-details=direct
```

Expected: build and affected suites exit zero, with the new client ordering cases and existing
round-robin/head/effect cases passing. Record actual fixture/server coverage and output; if stock
compatibility fixtures are separate commands, record and run those commands before completion.

## Validation and Acceptance


Four client statements sort returned rows by msg_id; round-robin preserves layering. Test vectors
are exact, selection/leases remain unchanged, and stock/native callers get the same client
contract. No vendor SQL, migration payload, manifest or convergence exception changes. Review
the diff for that boundary in addition to running behavioral tests.

## Idempotence and Recovery


Client edits are reversible and add no persistent database state. Tests use disposable fixtures.
If upstream adds ordering later, keep the explicit client contract until compatibility testing
justifies simplifying it; no server body needs rebasing or replaying. Historical migrations
remain untouched. A future raw-SQL guarantee requires an upstream fix and separate adoption.

## Interfaces and Dependencies


Public parameter/result types remain unchanged. Own Message.hs, focused client/effect test cases
and root/pgmq-hasql changelog entries. EP-3 owns broad documentation integration. There is no
hard dependency on the index child and no new SQL migration or override-maintenance mechanism.

Revision note (2026-09-12): Replaced the server-override plan with four outer client ORDER BY
clauses and an explicit client-only, ascending-ID contract under the user's no-override rule.
