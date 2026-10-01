---
title: "Non-destructive queue inspection reads"
type: Capability
description: "Peek at a queue or its archive by keyset page and fetch a message by id without leasing anything, for any server-accepted queue name."
generated:
  by: anthropic/claude-opus-5-5
  at: "2026-10-01T22:10:00Z"
capabilityId: CAP-10
provider: mori://shinzui/pgmq-hs
status: shipped
stability: experimental
since: "unreleased"
packages:
  - pgmq-hasql
  - pgmq-effectful
  - pgmq-core
requires:
  - CAP-1
interface:
  - Pgmq
  - Pgmq.Effectful
  - Pgmq.Hasql.Statements.Inspection
evidence:
  - kind: test
    resource: pgmq-hasql/test/InspectionSpec.hs
    proves: Peeks and lookups leave vt, read_ct, and last_read_at byte-identical, a concurrent consumer is undisturbed, keyset paging visits every message exactly once without OFFSET, archive reads carry archived_at, and lookups return a typed Nothing.
  - kind: test
    resource: pgmq-hasql/test/InspectionForeignNameSpec.hs
    proves: Every read works for mixed-case and hyphenated names parseQueueName rejects.
  - kind: test
    resource: pgmq-effectful/test/TracedInterpreterSpec.hs
    proves: The five effect operations run under both interpreters and the traced one labels them pgmq.peek, pgmq.peek_archive, pgmq.lookup_message, pgmq.lookup_archived_message.
  - kind: guide
    resource: docs/design/019-non-destructive-inspection-reads.md
    proves: The contract, the lenient-name rule, and the keyset-only paging policy.
---

# Non-destructive queue inspection reads

Every read the [core client](message-queue-client.md) offers leases: it pushes `vt` into the
future and bumps `read_ct`. These reads observe instead. An operator or an inspection surface
can browse a live queue, browse its archive, and fetch one message by id, and the consumers
polling that queue never notice. A consumer adopts this separately from queueing because it
is a different decision: it trades the typed, validated queue name for a lenient one so it can
see queues other clients created.

What it provides:

- **Peeks** — `peekMessages` over `pgmq.q_<name>` and `peekArchivedMessages` over
  `pgmq.a_<name>`, each taking a `PeekMessages` (name, exclusive `msg_id` cursor, limit) and
  returning rows in ascending `msg_id` order; archive rows are `ArchivedMessage` (a `Message`
  plus `archivedAt`).
- **Lookups** — `lookupMessage` and `lookupArchivedMessage`, each taking a `LookupMessage`
  (name, id) and answering `Nothing` when the message is not there.
- **Lenient metrics** — `queueMetricsUnvalidated`, the `queueMetrics` projection for a queue
  named by plain text.
- The same five operations on the `Pgmq` effect under `runPgmq` and `runPgmqTraced`.

## Shape

```haskell
import Pgmq
import Hasql.Pool qualified as Pool

Right page <- Pool.use pool (peekMessages (PeekMessages "orders" Nothing 50))
Right next <- Pool.use pool (peekMessages (PeekMessages "orders" (Just lastSeenId) 50))
Right found <- Pool.use pool (lookupMessage (LookupMessage "orders" (MessageId 42)))
```

## Limits

- **Lenient names are shown as given.** They are never validated or normalised by this
  library; the physical table comes from `pgmq.format_table_name` on the server, which
  lowercases and rejects `$`, `;`, `--`, and `'`.
- **A missing queue is an error, not a typed result**: the server's `42P01` inside the
  session error (`PgmqSessionError` through the effect). A missing message is `Nothing`.
- **`limit` must be positive.** `0` returns nothing; a negative value is a server error.
- **Breaking for exhaustive `Pgmq` interpreters**: the effect gained five constructors.
- Not yet released (`since: unreleased`); pre-1.0 and uniformly `experimental`.
