---
type: Improvement Request
title: Classify disconnects during PostgreSQL faults as transient
description: >-
  Preserve retryability when a PostgreSQL crash, backend termination, or TCP reset surfaces as a statement error without a SQLSTATE instead of a typed connection error.
generated:
  by: openai/gpt-6-sol
  at: "2026-09-22T21:21:00Z"
reviews: []
requestId: IR-4
status: completed
completedAt: "2026-10-01T14:25:00Z"
resolution: >-
  Implemented by mori://shinzui/pgmq-hs/plans/26-classify-postgresql-disconnects-surfaced-as-statement-errors-as-transient.
  (1) pgmq-effectful/test/DisconnectSpec.hs terminates a backend, SIGKILLs it, and crashes and restarts PostgreSQL while a readWithPoll is in flight; every outcome classifies transient and the same pool completes a later send without being recreated. The TCP-reset scenario was not ported: SIGKILL produces the same empty-SQLSTATE value deterministically without a proxy.
  (2) ClassificationSpec keeps genuine row-count mismatches (1 1 0, 1 1 2), invalid SQL and constraint SQLSTATEs, missing types, driver errors, and authentication and compatibility errors permanent. The row-count shape is matched by its exact value 1 1 1, which hasql 1.10 reserves for a stray second result, rather than by a connection-health probe, so no other row-count mismatch became transient.
  (3) docs/design/017-transient-error-classification.md describes the no-SQLSTATE and stray-result cases and why each is distinguishable from a permanent error.
  For the non-goal, the library still retries nothing; the new isAmbiguousReply tells a caller when a reply was lost so it can reconcile a non-idempotent send before replaying it.
origin: mori://shinzui/keiro-runtime-kenshou
---

# Improvement Request: Classify Disconnects During PostgreSQL Faults as Transient

## Problem

`Pgmq.Effectful.isTransient` is the retry predicate used by consumers of pgmq-hs. Its current policy in `docs/design/017-transient-error-classification.md` correctly treats connection errors and server SQLSTATE `57P01` through `57P03` as transient, while treating ordinary row-count and decode mismatches as permanent. Live faults can reach neither recognized branch. An immediate PostgreSQL shutdown and a TCP reset have surfaced as `PgmqSessionError (StatementSessionError ... (ServerStatementError (ServerError "" "" Nothing Nothing Nothing)))`, with an empty SQLSTATE. A terminated backend has also surfaced as `UnexpectedRowCountStatementError 1 1 1`. The present predicate returns `False` for both shapes even though the same pool succeeds immediately after the fault heals.

The reproducer is `mori://shinzui/keiro-runtime-kenshou/plans/8-cover-pgmq-hs-in-isolation`, implemented by the `pgmq/effectful/concurrency/postgres-restart-recovery`, `pgmq/effectful/concurrency/backend-termination-recovery`, and `pgmq/effectful/concurrency/network-partition` scenarios. In the crash scenario, PostgreSQL 17 and 18 both preserved all twenty committed sends, kept `fsync=on`, and recovered through the same pool in 362 and 358 milliseconds, respectively. Only the transient-classification check failed. The corresponding run identifiers are `01a0cafc-cef2-7716-aa94-78d41afa7951` and `01a0cafc-7cda-7428-b7c5-efe34a8ceb48`.

## Requested Change

Make disconnect and shutdown failures retryable even when libpq or hasql supplies no SQLSTATE. Preserve the existing permanent classification for real statement, schema, authentication, and decode errors. For the row-count shape, use connection health or a more specific driver error at the point of failure; do not classify every row-count mismatch as transient. Keep the plain and traced interpreters on one classification policy.

## Acceptance

1. Fault tests terminate a backend, crash and restart PostgreSQL, and reset a TCP connection while a PGMQ operation is in flight. Every disconnect outcome is classified transient, and the same pool completes a later operation without being recreated.
2. Classification tests retain permanent outcomes for a genuine row-count mismatch, a decode mismatch, an invalid SQL statement, and authentication or compatibility errors.
3. `docs/design/017-transient-error-classification.md` describes the no-SQLSTATE and lost-reply cases, including which observations are enough to distinguish them from permanent errors.

## Non-goals

This request does not make pgmq-hs retry operations itself. Callers still decide whether replaying a non-idempotent operation is safe after an ambiguous lost reply.
