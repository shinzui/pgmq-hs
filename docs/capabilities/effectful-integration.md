---
title: "Effectful integration with a typed runtime error model"
type: Capability
description: "A dynamic Effectful `Pgmq` effect over the whole pgmq surface, run against a hasql pool, with structured PgmqRuntimeError values, an isTransient retry classifier, and an isAmbiguousReply lost-reply classifier."
generated:
  by: anthropic/claude-opus-5-5
  at: "2026-10-01T14:40:00Z"
capabilityId: CAP-5
provider: mori://shinzui/pgmq-hs
status: shipped
stability: experimental
since: "0.1.0.0"
packages:
  - pgmq-effectful
requires:
  - CAP-1
interface:
  - Pgmq.Effectful
  - Pgmq.Effectful.Effect
  - Pgmq.Effectful.Interpreter
evidence:
  - kind: test
    resource: pgmq-effectful/test/PlainInterpreterSpec.hs
    proves: The plain interpreter runs pgmq operations against a pool and surfaces typed PgmqRuntimeError through the Error channel.
  - kind: test
    resource: pgmq-effectful/test/ClassificationSpec.hs
    proves: isTransient classifies the documented retry-worthy SQLSTATEs as transient and everything else as permanent, pinned in both directions.
  - kind: test
    resource: pgmq-effectful/test/DisconnectSpec.hs
    proves: Connections lost mid-statement (pg_terminate_backend, backend SIGKILL, immediate server restart) surface as transient errors, a SIGKILL reply is reported as ambiguous, and the same pool completes a later send.
  - kind: guide
    resource: docs/design/013-pgmq-effectful-error-model.md
    proves: The design of PgmqRuntimeError and why the error channel is typed.
  - kind: guide
    resource: docs/design/017-transient-error-classification.md
    proves: The exact transient/permanent rules behind isTransient and isAmbiguousReply, including the disconnect shapes and the pool caveat.
---

# Effectful integration with a typed runtime error model

For consumers built on the [`effectful`](https://hackage.haskell.org/package/effectful)
ecosystem, `pgmq-effectful` exposes the whole [core client](message-queue-client.md) as a
dynamic `Pgmq` effect and runs it against a hasql pool. Errors arrive as a structured
`PgmqRuntimeError` through the `Error` effect channel rather than as untyped exceptions,
and `isTransient` tells retry logic which failures are worth retrying. The effect, its
interpreter, and its error model always ship and are proven together, so they are one
capability.

What it provides:

- **The `Pgmq` effect** — GADT constructors covering send, read, poll, pop, ack, archive,
  delete, visibility timeout, topics, notifications, observability, and (uniquely at this
  layer) the FIFO grouped-read constructors.
- **Interpreters** — `runPgmq` runs the effect against a `Hasql.Pool.Pool`.
- **Typed errors** — `PgmqRuntimeError` (`PgmqAcquisitionTimeout`, `PgmqConnectionError`,
  `PgmqSessionError`), `fromUsageError` to lift raw `hasql-pool` errors, `isTransient`
  for retry gating, and `isAmbiguousReply` to recognize a lost reply (the statement was
  delivered but no verdict came back) before retrying a non-idempotent operation.

## Shape

```haskell
import Pgmq.Effectful (Pgmq, runPgmq, PgmqRuntimeError (..), isTransient)
import Effectful (runEff)
import Effectful.Error.Static (runError)

runEff . runError @PgmqRuntimeError . runPgmq pool $ do
  createQueue q
  sendMessage (SendMessage q body)
```

## Limits

- **`isTransient` errs toward retrying the unknown.** `OtherConnectionError` is classified
  transient despite hasql documenting it as "not transient by default"; the whitelist
  prefers letting a retry happen over failing fast on an unrecognized connection error.
  The recognized transient SQLSTATEs are `40001`, `40P01`, `55P03`, `57P01/57P02/57P03`,
  and class `53`. Statement and script errors with no SQLSTATE (libpq's own
  connection-loss result) and the driver's stray-result shape
  `UnexpectedRowCountStatementError 1 1 1` are transient too; genuine decode and row-count
  mismatches, unknown types, and driver errors remain permanent. After such a disconnect the
  pool keeps the dead connection, so a retry loop sees one more transient
  `ConnectionSessionError` before a fresh connection succeeds.
- **The library retries nothing itself.** An ambiguous reply (`isAmbiguousReply`) to a send
  may have committed; reconciling before re-sending is the caller's job, and every retry
  loop must be bounded.
- **FIFO grouped reads exist on the effect GADT but are not re-exported by the
  `Pgmq.Effectful` umbrella** — see [FIFO message-group reads](fifo-message-group-reads.md)
  for the direct-import requirement.
- The legacy `PgmqError` / `PgmqPoolError` names were removed after their deprecation
  cycle; use `PgmqRuntimeError`.
- Pre-1.0 and uniformly `experimental`.
