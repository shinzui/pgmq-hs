# Transient-error classification is a pure function of the error value

## Status

Accepted, 2026-10-01. Introduced by
[ExecPlan 26](../plans/26-classify-postgresql-disconnects-surfaced-as-statement-errors-as-transient.md),
which fixed BUG-1 and completed IR-4. This repository has no profiled ADR bundle; this record
follows its existing plain-Markdown decision convention and introduces no OKF metadata. The
rule table lives in
[design note 017](../design/017-transient-error-classification.md); this record holds the
decisions behind it.

## Context

`Pgmq.Effectful.isTransient` is the retry gate consumers call on an already-surfaced
`PgmqRuntimeError`; `shibuya-pgmq-adapter` gates every ack and poll retry on it. It used to
answer `False` for real connection loss, because hasql (1.10) reports only a *send*-side
failure as `ConnectionSessionError`. A connection that dies while hasql is *waiting for the
reply* surfaces through statement constructors instead. One shape is a `ServerError` with an
empty SQLSTATE, which is libpq's own connection-loss result with no fields. The other is
`UnexpectedRowCountStatementError 1 1 1`, a stray second result after a server FATAL.

hasql-pool (1.4) evicts a connection only on `ConnectionSessionError`, so after either shape
the dead connection goes back into the pool. The next use fails at the send step with
`ConnectionSessionError`, which evicts it.

Classifying those shapes transient means a retry loop re-runs an operation whose reply was
lost. For a send that may already have committed, a blind retry duplicates the message.

## Decision

**Classify by value; do not probe the connection.** `isTransient` stays a pure predicate. An
empty SQLSTATE is transient, because every server `ErrorResponse` carries one, so an empty
code is client-side. Exactly `UnexpectedRowCountStatementError 1 1 1` is transient, because
hasql 1.10 never reports a genuine row-count mismatch with an actual count inside its own
bounds. We rejected a health probe that would check `PQstatus` after a failure and rewrite the
error to `ConnectionSessionError`. It would add a `postgresql-libpq` dependency to the
interpreters and change the error value consumers see. It would also miss callers of
`fromUsageError`. Its only real benefit, immediate eviction, arrives one call later anyway.

**Accept one extra transient failure per dead pooled connection.** That
`ConnectionSessionError` is documented, not engineered away.

**Script errors follow the SQLSTATE rule.** `ScriptSessionError` used to be permanent
unconditionally, which left one server-error constructor outside the policy.

**`DriverSessionError` stays permanent.** The disconnect window that lands there (after a
statement's reply, before the pipeline sync) carries its details only in message text.
Matching that text would be fragile, and no run has observed the window.

**Lost replies get their own predicate.** `isAmbiguousReply` answers `True` for exactly the
lost-reply shapes, and it implies `isTransient`. It is deliberately conservative: it reports
the stray-result shape as ambiguous even though its usual producer aborted the transaction,
because hasql discards the first result. Reconciling unnecessarily costs a read; a duplicate
send costs correctness. The library still retries nothing itself.

## Consequences

A consumer's retry loop gated on `isTransient` now survives backend termination, backend
SIGKILL, TCP reset, and an immediate server restart. It converges once the server accepts
connections again, after at most one extra failure per dead pooled connection.
`pgmq-effectful/test/DisconnectSpec.hs` proves this against real faults.

Non-idempotent operations, sends above all, must gate their retry on `isAmbiguousReply` and
reconcile before re-sending. Every documented retry loop is bounded, because each error in a
permanent outage is individually transient, and the ambiguous state is sticky: once set, only
the reconciliation is retried.

The `1 1 1` rule depends on hasql 1.10's encoding. A hasql upgrade must recheck
`Hasql.Comms.Recv.singleResult` and `Hasql.Engine.Errors.fromRecvError`. An upstream change
that maps every receive-side connection loss to `ConnectionSessionError` would make both rules
redundant, close the `DriverSessionError` gap, and is the natural follow-up to file with
`mori://hasql/hasql`.
