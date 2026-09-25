---
type: Bug Report
title: Recoverable PostgreSQL disconnects are classified as permanent
description: PostgreSQL faults can produce statement errors that isTransient calls permanent even though the same pool recovers.
generated:
  by: process:codex
  at: "2026-09-25T04:34:56Z"
bugId: BUG-1
status: reported
severity: degraded
origin: mori://shinzui/keiro-runtime-kenshou/masterplans/1-build-an-extensive-verification-suite-for-the-keiro-runtime
affects: mori://shinzui/pgmq-hs/packages/pgmq-effectful
capability: mori://shinzui/pgmq-hs/okf/capabilities/concepts/CAP-5
affectedVersion: "0.6.1.0"
environment: PostgreSQL 17.11 and 18.6 with fsync enabled; released Kenshou cohort; macOS aarch64.
observed: Immediate server shutdown and TCP reset can yield a statement ServerError with an empty SQLSTATE, while backend termination can yield UnexpectedRowCountStatementError; isTransient returns False for all three although the same pool succeeds after recovery.
expected: A retry classifier advertised as identifying retry-worthy failures should identify a confirmed connection interruption as transient, even when the driver reports it through a statement-error constructor; the published SQLSTATE and row-count policy needs a precise disconnect exception.
reproduction:
  - Build the released cohort of mori://shinzui/keiro-runtime-kenshou, containing pgmq-effectful 0.6.1.0.
  - Run `cabal run kenshou -- run pgmq/effectful/concurrency/postgres-restart-recovery --out runs --dim pg.version=18` with the default durable PostgreSQL fixture.
  - Inspect PostgreSQL 18 run `01a0d6d6-cb25-7609-b1d1-0eeebbba50ec` and PostgreSQL 17 run `01a0d6d7-cae8-72ed-ba4a-17a8cc224e53`; committed keys survive, the same pool sends again after restart, and only `outage-error-transient` fails.
  - Run `pgmq/effectful/concurrency/backend-termination-recovery` and `pgmq/effectful/concurrency/network-partition`; PostgreSQL 18 runs `01a0d6d7-028e-73ac-b7a5-e582b4f85c36` and `01a0d6d7-1f5e-768c-9b95-822031453720` show the other two error shapes and same-pool recovery.
workaround: Treat connection interruption as an operation-specific recovery case at the caller boundary, and check durable message keys before retrying a send whose reply was lost.
reviews:
  - kind: model
    reviewer: process:codex
    reviewed_at: "2026-09-25T04:34:56Z"
    document_timestamp: "2026-09-25T04:34:56Z"
    scope: content-and-metadata
    outcome: commented
    provider: OpenAI
    model: gpt-6-sol
    effort: medium
    context: Reproduced immediate shutdown on PostgreSQL 17 and 18 and backend termination and TCP reset on PostgreSQL 18; compared sealed verdicts with the released classifier and capability.
---

# Recoverable PostgreSQL disconnects are classified as permanent

The shipped capability promises an `isTransient` retry gate. The design note at `mori://shinzui/pgmq-hs` with project-relative path `docs/design/017-transient-error-classification.md` makes row-count mismatches and unrecognized statement errors permanent because it assumes retries cannot repair them. These runs isolate a different cause: each error occurred during a real connection fault, and the same pool completed a later operation. The document-level Mori URI is pending.

The source of truth for the observed outcomes is `mori://shinzui/keiro-runtime-kenshou` at `runs/01a0d6d6-cb25-7609-b1d1-0eeebbba50ec/`, `runs/01a0d6d7-028e-73ac-b7a5-e582b4f85c36/`, and `runs/01a0d6d7-1f5e-768c-9b95-822031453720/`; artifact-level run URIs are pending. The existing remediation request is `mori://shinzui/pgmq-hs/okf/improvement-requests/concepts/IR-4`. This report records broken retry classification, not an instruction to replay an ambiguous send automatically.
