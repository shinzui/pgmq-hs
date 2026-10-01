# Design Document 017: Transient error classification

## Status

**Adopted (2026-08-05)**, as part of
`docs/plans/15-validate-queue-names-and-classify-transient-errors-across-the-pgmq-layers.md`.

**Revised (2026-10-01)** by
`docs/plans/26-classify-postgresql-disconnects-surfaced-as-statement-errors-as-transient.md`
(BUG-1, IR-4): connections lost while waiting for a reply now classify as transient, script
errors follow the SQLSTATE rule, and `isAmbiguousReply` marks lost replies.


## The contract

`Pgmq.Effectful.Interpreter.isTransient` (re-exported from `Pgmq.Effectful`) answers one
question: is this error worth retrying? It is a pure predicate consumers call on an
already-surfaced `PgmqRuntimeError`; it does not retry anything itself.

Transient: pool acquisition timeouts; networking connection errors; hasql's catch-all
`OtherConnectionError` (unrecognized libpq errors — classing the unknown as transient errs
toward letting retries happen); session-level connection drops; the two shapes hasql uses
for a connection lost while waiting for a reply (see "Client-synthesized errors" and "Stray
results" below); and — the part this note was first written for — server-reported statement
or script errors whose SQLSTATE names a transient condition:

| SQLSTATE | Condition |
|----------|-----------|
| `40001` | serialization_failure |
| `40P01` | deadlock_detected |
| `55P03` | lock_not_available |
| `57P01` | admin_shutdown |
| `57P02` | crash_shutdown |
| `57P03` | cannot_connect_now |
| `53xxx` | insufficient resources (53000/53100/53200/53300/53400) |

Everything else is permanent: authentication and compatibility failures, missing types,
driver errors, every other server-reported SQLSTATE (constraint violations, undefined
objects, bad input syntax), and every genuine decode-side statement error (row-count,
column count/type, cell decode) — retrying cannot fix a decoder mismatch or a unique
violation.

The library itself still retries nothing. A second predicate, `isAmbiguousReply`, tells a
caller when an error means "delivered, but no verdict came back", which is the case where a
blind retry of a non-idempotent operation can duplicate it (see "Lost replies and
idempotence").


## Why

These SQLSTATEs arrive as `ServerError` inside `ServerStatementError` inside
`StatementSessionError` — and `isTransient` previously mapped the whole
`StatementSessionError` constructor to permanent, unconditionally. But these are
precisely the errors retries exist for. `40P01` is genuinely reachable in this library:
overlapping batch delete/archive statements lock message rows in statement-internal
order, so two sessions finalizing overlapping id sets can deadlock. `57P02`/`57P03` are
the crash-recovery and temporarily-unavailable siblings of `57P01`; a worker that treats
"the database is restarting" as permanent fails work it would have completed two seconds
later. Class `53` (out of memory, disk full, too many connections) is load, and load
passes.

The consumer this bites is real: shibuya-pgmq-adapter gates every ack/poll retry on
`isTransient` (`retryingTransient` in `Shibuya/Adapter/Pgmq/Internal.hs`), so before this
change its retry loops failed fast on deadlocks and serialization failures. keiro-pgmq
does not call `isTransient` (verified 2026-07-23).

Plain/traced interpreter parity is structural, not policy: both interpreters surface the
same `PgmqRuntimeError` via `fromUsageError`, and `isTransient` is one shared function.
The traced interpreter's span semantics (keiro ADR 0001) are untouched — classification
happens in the consumer after the error has surfaced, never in the interpreter.


## Client-synthesized errors: an empty SQLSTATE

hasql (1.10.2.3, `mori://hasql/hasql/packages/hasql`) turns a failure into
`ConnectionSessionError` only when *sending* a query fails
(`Hasql.Comms.Roundtrip.toPipelineIO`). When the connection dies while hasql is *waiting
for the reply*, libpq's `PQgetResult` returns a result libpq manufactured itself ("server
closed the connection unexpectedly"). That result has status `FatalError` but no error
fields, because libpq keeps the text in the connection's error buffer. hasql's
`checkExecStatus` builds a `ServerError` from `PQresultErrorField` and folds each missing
field to empty, so the disconnect surfaces as:

```haskell
PgmqSessionError
  (StatementSessionError 1 0 "<sql>" [<params>] True
    (ServerStatementError (ServerError "" "" Nothing Nothing Nothing)))
```

The PostgreSQL wire protocol requires every `ErrorResponse` the server sends to carry the
SQLSTATE field, so an empty code can only be libpq-local: connection loss, lost protocol
synchronization, or client-side out-of-memory, all of which a fresh connection can outlive.
`isTransient` therefore treats a `ServerError` with an empty SQLSTATE as transient, in
statements and scripts alike. Every server-reported SQLSTATE keeps exactly its previous
policy. Observed with SIGKILL of the backend and with an immediate shutdown of the server
(`DisconnectSpec`); the kenshou TCP-reset scenario produced the same value.


## Stray results: `UnexpectedRowCountStatementError 1 1 1`

hasql's `Hasql.Comms.Recv.singleResult` calls `PQgetResult` once, expects a result, calls it
again, and expects nothing. If the second call returns a result it fails with
`TooManyResultsError context 1` without looking at the first, and
`Hasql.Engine.Errors.fromRecvError` renders that as `UnexpectedRowCountStatementError 1 1 1`.
Every construction of this constructor in hasql 1.10 uses bounds `1 1`: "no result" becomes
actual `0`, and the decoders' own row-count failures (`single`, `maybe`) can only report an
actual count outside the bounds. So `1 1 1`, an actual count inside its own bounds, is never
a row-count mismatch; it is the driver saying a second result arrived for a single-command
roundtrip.

For this library's extended-protocol statements that happens when the server sends a FATAL
`ErrorResponse` (for example `57P01` from `pg_terminate_backend`) and then closes, and libpq
notices the closed socket while still handling that response: the server's error becomes
the first result and libpq appends its own connection-loss result as the second. The kenshou
backend-termination run observed exactly this. It is timing-dependent: locally (macOS, Unix
socket and TCP) `pg_terminate_backend` surfaced the server's `57P01` as the only result, which
was already transient. `isTransient` treats exactly `1 1 1` as transient; `1 1 0`, `1 1 2`,
and every other count stay permanent.


## What the pool does afterwards

hasql-pool 1.4.1 (`mori://hasql/hasql/packages/hasql-pool`) evicts a connection only when
the session failed with `ConnectionSessionError`; after either shape above it returns the
dead connection to the pool. The next use of that connection fails at the send step, which
*is* `ConnectionSessionError` ("no connection to the server"): transient, and the pool
evicts the connection. The use after that gets a fresh connection. A retry loop gated on
`isTransient` therefore pays one extra transient failure per dead pooled connection, plus
however long the server takes to accept connections again. `DisconnectSpec` measured
exactly two attempts for each fault. The library does not probe connection health to avoid
the extra attempt (see `docs/adr/transient-classification-is-value-based.md`).


## Residual gap: `DriverSessionError`

If the connection dies after a statement's reply arrives but before the pipeline's sync
reply, hasql reports a `DriverSessionError` whose details live only in its message text.
`isTransient` keeps `DriverSessionError` permanent: matching message text would be fragile,
the window is a few bytes of the same server write, and no run has observed it. Only an
upstream hasql change that maps every receive-side connection loss to
`ConnectionSessionError` would close this gap.


## Lost replies and idempotence

Making the lost-reply shapes transient means a retry loop now re-runs an operation whose
reply was lost. For a read, ack, or archive that is harmless. For `sendMessage` it is not:
the statement may have committed before the connection dropped, and re-sending it produces a
duplicate. `isTransient` cannot express "safe to retry blindly" versus "retry only after
checking", so the library exports a second predicate:

```haskell
isAmbiguousReply :: PgmqRuntimeError -> Bool
```

It answers `True` exactly when a statement was delivered but no server verdict came back:
an empty-SQLSTATE `ServerError` inside a `StatementSessionError` or a `ScriptSessionError`,
and `UnexpectedRowCountStatementError 1 1 1`. It answers `False` for everything else,
including `ConnectionSessionError` (the send failed, so nothing reached the server), real
server errors, and decode failures (the verdict arrived). Every ambiguous reply is also
transient, so `isAmbiguousReply err` implies `isTransient err`, and plain retry loops keep
working unchanged.

The predicate is conservative on purpose. The stray-result shape's usual producer is a
server FATAL that aborted the transaction, so the statement most likely did *not* commit.
But hasql discards the first result, and the value alone cannot prove which result it was.
Reconciling unnecessarily costs one read; duplicating a send costs correctness.

Two rules apply to every retry loop built on these predicates:

1. **Bound every loop** by an attempt count or a deadline. During a permanent outage every
   attempt fails with an error that is individually transient; `isTransient` is a verdict on
   one error, not an oracle for whether the outage will heal.
2. **The ambiguous state is sticky.** Once `isAmbiguousReply` fires for a non-idempotent
   operation, further transient failures (typically the reconciliation read failing while
   the server is still down) retry the reconciliation, never the original send, until it
   succeeds or the bound is hit.

The recommended loop, which the README also carries (`lookupByKey` stands for whatever
reconciliation the application can do, such as finding the message by a dedupe key it put
in the headers; `pause` is the caller's backoff):

```haskell
-- Bounded: stop after maxAttempts. Sticky: once a send's reply is lost, look
-- the message up instead of sending again, and keep looking it up through
-- further transient failures until the lookup succeeds.
sendOnce pool key body = go 1 False
  where
    go attempt reconciling = do
      result <-
        runEff . runError @PgmqRuntimeError . runPgmq pool $
          if reconciling then lookupByKey key else sendMessage (message key body)
      case result of
        Right outcome -> pure (Right outcome)
        Left (_cs, err)
          | attempt >= maxAttempts -> pure (Left err)
          | isAmbiguousReply err -> pause attempt >> go (attempt + 1) True
          | isTransient err -> pause attempt >> go (attempt + 1) reconciling
          | otherwise -> pure (Left err)
```

The library still does not retry anything itself, and whether to re-send after a lost reply
remains the caller's decision; `isAmbiguousReply` tells the caller when that decision is
needed.


## Where this is enforced

`pgmq-effectful/test/ClassificationSpec.hs` pins every constructor case and, for
`StatementSessionError`, both directions of the SQLSTATE whitelist: the nine transient
codes above, three representative permanent codes (`23505`, `42P01`, `22P02`), and the
non-server row-count errors `1 1 0` and `1 1 2`. It also pins the lost-reply rules (empty
SQLSTATE in a statement and in a script, `1 1 1`), the script SQLSTATE rule (`57P01`
transient, `23505` permanent), `isAmbiguousReply` in both directions, and the implication
`isAmbiguousReply e ⇒ isTransient e` over every sample it constructs. When adding a code to
the whitelist, add its case there.

`pgmq-effectful/test/DisconnectSpec.hs` drives real faults against a dedicated PostgreSQL
while a `readWithPoll` is blocked server-side (`pg_terminate_backend`, SIGKILL of the
backend, and an immediate-shutdown restart), and asserts for each that the surfaced error is
a known disconnect shape, is transient, that any ambiguous reply is transient, and that the
same pool completes a later send. It also asserts that the SIGKILL reply is ambiguous.
