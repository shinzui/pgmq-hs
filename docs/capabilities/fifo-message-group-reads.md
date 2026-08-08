---
title: "FIFO message-group reads"
type: Capability
description: "SQS-style grouped and round-robin reads that lease a whole message group in order, plus the FIFO index management they require."
generated:
  by: adopt-capabilities/0.9.2
  at: "2026-08-08T00:00:00Z"
capabilityId: CAP-2
provider: mori://shinzui/pgmq-hs
status: shipped
stability: experimental
since: "0.1.0.0"
packages:
  - pgmq-hasql
requires:
  - CAP-1
interface:
  - Pgmq.Hasql.Sessions
  - Pgmq.Hasql.Statements.Types
evidence:
  - kind: test
    resource: pgmq-hasql/test/AdvancedOpsSpec.hs
    proves: readGrouped, readGroupedWithPoll, readGroupedRoundRobin(WithPoll), and createFifoIndex(esAll) behave against a real pgmq schema.
  - kind: guide
    resource: docs/design/008-fifo-read.md
    proves: The FIFO read design — message keys, group filling, and round-robin fairness across groups.
---

# FIFO message-group reads

On top of the [core message-queue client](message-queue-client.md), pgmq-hasql provides
SQS-style FIFO reads: a batch is filled from a single message group so ordering within a
group is preserved, and a round-robin variant distributes fairly across groups. A consumer
chooses these explicitly — they require a FIFO index and message keys, and are proven by
their own test — so they are their own capability rather than part of the plain read path.

What it provides:

- `readGrouped`, `readGroupedWithPoll` — fill a batch from the same message group (pgmq
  1.8.0+).
- `readGroupedRoundRobin`, `readGroupedRoundRobinWithPoll` — layered round-robin across
  groups for fair distribution (pgmq 1.9.0+).
- `createFifoIndex`, `createFifoIndexesAll` — create the `q_<name>_fifo_idx` these reads
  rely on.

## Shape

```haskell
import Pgmq.Hasql.Sessions qualified as Sessions
import Pgmq.Hasql.Statements.Types (ReadGrouped (..))

Pool.use pool $ do
  Sessions.createFifoIndex q
  grouped <- Sessions.readGrouped (ReadGrouped q 30 (Just 10) groupKey)
  pure ()
```

## Limits

- **Not surfaced through either convenience umbrella.** This is the weakest-discoverability
  surface in the catalog and the honest bound is worth stating plainly: the grouped and
  round-robin reads and `createFifoIndex(esAll)` are **not** re-exported by the `Pgmq`
  session umbrella (which has no grouped-read function at all) nor by the `Pgmq.Effectful`
  umbrella. A consumer must import `Pgmq.Hasql.Sessions` / `Pgmq.Hasql.Statements.Types`
  directly, or the `Pgmq.Effectful.Effect` GADT constructors. The functionality is tested;
  only its packaging is inconvenient.
- **No conditional filter on FIFO reads.** pgmq removed the `conditional` parameter from
  the FIFO functions in 1.9.0, so grouped reads cannot be filtered the way `readMessage`
  and `readWithPoll` can.
- Pre-1.0 and uniformly `experimental`, as with every record here.
