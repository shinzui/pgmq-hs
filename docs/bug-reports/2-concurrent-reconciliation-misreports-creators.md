---
type: Bug Report
title: Concurrent reconciliation misreports resource creators
description: Multiple reconcilers can each claim they created the same queue, notification rule, or FIFO index.
generated:
  by: process:codex
  at: "2026-09-25T04:34:56Z"
bugId: BUG-2
status: reported
severity: degraded
origin: mori://shinzui/keiro-runtime-kenshou/masterplans/1-build-an-extensive-verification-suite-for-the-keiro-runtime
affects: mori://shinzui/pgmq-hs/packages/pgmq-config
capability: mori://shinzui/pgmq-hs/okf/capabilities/concepts/CAP-9
affectedVersion: "0.6.1.0"
environment: Eight concurrent reconciler processes, ten new queue declarations per round, fifty rounds, PostgreSQL 17.11 and 18.6.
observed: In one round eighty creation actions were reported for ten physical queues, and the same overcount affected notification and FIFO index actions; the final catalog still converged.
expected: The shipped declarative-reconciliation capability says its action report truthfully accounts for every action taken, so only the process that actually creates each resource should report its creation; the others should report a skipped action.
reproduction:
  - Build the released cohort of mori://shinzui/keiro-runtime-kenshou, containing pgmq-config 0.6.1.0.
  - Run `cabal run kenshou -- run pgmq/config/concurrency/concurrent-reconcile --out runs --dim pg.version=18` with the default durable fixture.
  - Inspect run `01a0d6d6-85fe-7214-b7f3-cea91e397461`, especially `summaries.verdicts.concurrent-reconcile-observations`; round 1 reports eighty creations for ten catalog resources and later rounds repeat the overcount.
  - Repeat on PostgreSQL 17; run `01a0d6d7-9fe9-7653-bad0-83c881850ae8` also shows the false reports while its catalog converges.
workaround: Treat `Created...` entries from simultaneous startup processes as decisions based on an earlier snapshot, then inspect the final catalog; serialize reconciliation when an accurate per-process action audit is required.
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
    context: Reproduced the concurrent reconciliation case on PostgreSQL 17 and 18 and checked creator counts, catalog convergence, the published capability and the documented FIFO race.
---

# Concurrent reconciliation misreports resource creators

The shipped `mori://shinzui/pgmq-hs/okf/capabilities/concepts/CAP-9` provision claim includes truthful action accounting. `ensureQueuesReport` takes a catalog snapshot before a mutation and reports creation from that snapshot; eight callers can therefore all report that they created one resource. The source of truth is `mori://shinzui/keiro-runtime-kenshou` at `runs/01a0d6d6-85fe-7214-b7f3-cea91e397461/` and `runs/01a0d6d7-9fe9-7653-bad0-83c881850ae8/`; artifact-level run URIs are pending.

The same scenario also reproduces a FIFO index `23505` catalog race. `Pgmq.Config.ensureQueues` already documents that narrower concurrent-startup limitation and recommends retry, so this report's broken provision claim is the false action report. The broader remediation request is `mori://shinzui/pgmq-hs/okf/improvement-requests/concepts/IR-5`.
