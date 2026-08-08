---
title: "Effectful integration with a typed runtime error model"
type: Capability
description: "A dynamic Effectful `Pgmq` effect over the whole pgmq surface, run against a hasql pool, with structured PgmqRuntimeError values and an isTransient retry classifier."
generated:
  by: adopt-capabilities/0.9.2
  at: "2026-08-08T00:00:00Z"
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
  - kind: guide
    resource: docs/design/013-pgmq-effectful-error-model.md
    proves: The design of PgmqRuntimeError and why the error channel is typed.
  - kind: guide
    resource: docs/design/017-transient-error-classification.md
    proves: The exact transient/permanent SQLSTATE whitelist behind isTransient.
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
  `PgmqSessionError`), `fromUsageError` to lift raw `hasql-pool` errors, and `isTransient`
  for retry gating.

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
  and class `53`; every other statement error, including decode and row-count mismatches,
  is permanent.
- **FIFO grouped reads exist on the effect GADT but are not re-exported by the
  `Pgmq.Effectful` umbrella** — see [FIFO message-group reads](fifo-message-group-reads.md)
  for the direct-import requirement.
- The legacy `PgmqError` / `PgmqPoolError` names were removed after their deprecation
  cycle; use `PgmqRuntimeError`.
- Pre-1.0 and uniformly `experimental`.
