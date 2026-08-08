---
title: "PostgreSQL message-queue client (hasql)"
type: Capability
description: "Create queues and send, read, lease, delete, and archive messages on pgmq over a hasql connection pool, with validated queue names and metrics."
generated:
  by: adopt-capabilities/0.9.2
  at: "2026-08-08T00:00:00Z"
capabilityId: CAP-1
provider: mori://shinzui/pgmq-hs
status: shipped
stability: experimental
since: "0.1.0.0"
packages:
  - pgmq-hasql
  - pgmq-core
interface:
  - Pgmq
  - Pgmq.Types
evidence:
  - kind: test
    resource: pgmq-hasql/test/MessageSpec.hs
    proves: Send, read, delete, archive, and batch variants round-trip against a real pgmq schema.
  - kind: test
    resource: pgmq-hasql/test/QueueSpec.hs
    proves: Queue creation (standard, unlogged, partitioned) and listing behave as documented.
  - kind: test
    resource: pgmq-hasql/test/RoundTripSpec.hs
    proves: Message bodies and headers survive the encode → database → decode cycle (property-based).
  - kind: test
    resource: pgmq-hasql/test/NullSemanticsSpec.hs
    proves: NULL-parameter contracts for pop, read, and poll match the documented single-message defaults.
  - kind: test
    resource: pgmq-hasql/test/MetricsSpec.hs
    proves: queueMetrics/allQueueMetrics decode the pgmq metrics rows, including queueVisibleLength.
  - kind: test
    resource: pgmq-hasql/test/MixedCaseRemediationSpec.hs
    proves: The transactional mixed-case pgmq.meta remediation preserves bindings and notification config.
  - kind: guide
    resource: docs/design/014-null-parameter-contract.md
    proves: The rationale for the nullable-bind fixes behind pop/read/notify default handling.
---

# PostgreSQL message-queue client (hasql)

The `Pgmq` umbrella module is the flat, `Hasql.Session`-returning client a consumer
depends on to use PostgreSQL as a message queue through the pgmq schema. One package
(`pgmq-hasql`, over `pgmq-core` types) gives you queue administration, the full message
lifecycle, visibility-timeout control, and queue observability. Adopting it is a single
decision — you get all of these together, proven by the same suite of round-trip tests —
so they are one capability rather than one record per function.

What it provides:

- **Queue management** — `createQueue`, `createUnloggedQueue`, `createPartitionedQueue`,
  `dropQueue`.
- **Send** — `sendMessage`, `sendMessageForLater`, the `batchSend*` family, and the
  header-carrying `*WithHeaders` variants (pgmq 1.5.0+).
- **Read / lease / ack** — `readMessage`, `readWithPoll`, `pop`, `deleteMessage`,
  `batchDeleteMessages`, `archiveMessage`, `batchArchiveMessages`,
  `deleteAllMessagesFromQueue`.
- **Visibility timeout** — `changeVisibilityTimeout`, `batchChangeVisibilityTimeout`, and
  the timestamp-based `setVisibilityTimeoutAt` / `batchSetVisibilityTimeoutAt` (pgmq
  1.10.0+).
- **Observability** — `listQueues`, `queueMetrics`, `allQueueMetrics`, and the lenient
  `listQueuesUnvalidated` for reading a `pgmq.meta` another client may have written into.
- **Validated queue names** — `Pgmq.Types.parseQueueName` and the `QueueName` newtype,
  the single entry path a consumer uses to name a queue.

## Shape

```haskell
import Pgmq
import Pgmq.Types (parseQueueName, MessageBody (..))
import Hasql.Pool qualified as Pool

Right q <- pure (parseQueueName "orders")
Pool.use pool $ do
  createQueue q
  _mid <- sendMessage (SendMessage q (MessageBody (Aeson.object ["id" .= (1 :: Int)])))
  msgs <- readMessage (ReadMessage q 30 (Just 10) Nothing)   -- vt=30s, batch=10
  pure ()
```

## Limits

- **Pre-1.0, uniformly unstable.** Every capability in this catalog is `stability:
  experimental` because pgmq-hs makes no cross-version compatibility promise before 1.0.
  The client works; its signatures may still change between minor releases (see the
  0.2.0.0 and 0.5.0.0 breaking changes).
- **Queue names are lowercase-only.** `parseQueueName` rejects the empty string and any
  character outside `[a-z0-9_]`. This is a correctness requirement, not style: pgmq
  lowercases physical table names while `pgmq.meta` keeps the original casing, so
  `MyQueue` and `myqueue` silently aliased one physical table. A deployment whose
  `pgmq.meta` already holds mixed-case rows will fail `listQueues` decoding until it runs
  the transactional remediation in `docs/design/016-queue-name-validation.md`.
- **`changeVisibilityTimeout` / `setVisibilityTimeoutAt` return `Maybe Message`** (since
  0.5.0.0). Zero rows from `pgmq.set_vt` mean the message was already deleted, archived,
  or popped — a lost race, distinct from infrastructure failure. The batch variants do
  not carry this signal.
- **A SQL NULL message body decodes as JSON `null`** (`MessageBody Aeson.Null`),
  deliberately indistinguishable from an explicitly-sent JSON `null`. This keeps a row a
  non-Haskell producer wrote with `NULL::jsonb` readable and archivable instead of
  poisoning the batch.
- **`detachArchive` is a deprecated no-op** in pgmq and will be removed in pgmq 2.0; it is
  retained only for source compatibility and does nothing.
