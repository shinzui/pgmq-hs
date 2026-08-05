# Design Document 017: Transient error classification

## Status

**Adopted (2026-08-05)**, as part of
`docs/plans/15-validate-queue-names-and-classify-transient-errors-across-the-pgmq-layers.md`.


## The contract

`Pgmq.Effectful.Interpreter.isTransient` (re-exported from `Pgmq.Effectful`) answers one
question: is this error worth retrying? It is a pure predicate consumers call on an
already-surfaced `PgmqRuntimeError`; it does not retry anything itself.

Transient: pool acquisition timeouts; networking connection errors; hasql's catch-all
`OtherConnectionError` (unrecognized libpq errors — classing the unknown as transient errs
toward letting retries happen); session-level connection drops; and — the part this note
exists for — server-reported statement errors whose SQLSTATE names a transient condition:

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
objects, bad input syntax), and every decode-side statement error (row-count, column
count/type, cell decode) — retrying cannot fix a decoder mismatch or a unique violation.


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


## Where this is enforced

`pgmq-effectful/test/ClassificationSpec.hs` pins every constructor case and, for
`StatementSessionError`, both directions of the SQLSTATE whitelist: the nine transient
codes above, three representative permanent codes (`23505`, `42P01`, `22P02`), and the
non-server row-count error. When adding a code to the whitelist, add its case there.
