---
type: Review
title: pgmq-effectful from v0.4.0.1 to v0.6.1.0 — new operations, transient classification, and spans are correct
description: Every new effect constructor is dispatched by both interpreters with the right session and span label, and the transient SQLSTATE whitelist matches design note 017; nothing to act on.
generated:
  by: anthropic/claude-fable-5-1
  at: "2026-09-16T20:57:04Z"
reviewId: REV-5
subject: mori://shinzui/pgmq-hs/packages/pgmq-effectful
subjectKind: component
reviewedSha: 163413b3ccc85ce6a517646da6ff8969c2dd7482
coverage: incremental
baseSha: 7535d4915d4ccaa717d860cb0aa54b142d3e2bf8
reviewedAt: "2026-09-16T20:57:04Z"
reviewerKind: model
reviewer: process:claude-code
provider: anthropic
model: claude-fable-5-1
effort: xhigh
outcome: approved
dimensions:
  - correctness
  - performance
  - documentation
context: >-
  Read the diff of pgmq-effectful/src between the two tags — the effect GADT, the plain
  interpreter, the traced interpreter, and the umbrella module — against the pgmq-hasql
  sessions they call; read the dependency-bounds ADR for the effectful-core 2.7 note; did not
  run the suite.
---

# pgmq-effectful from v0.4.0.1 to v0.6.1.0

## What was examined

The five new effect constructors (`CreatePartitionedQueueWithPremake`, `ReadGroupedHead`,
`ReadGroupedHeadWithPoll`, `ListQueuesUnvalidated`, `ListFifoIndexQueueNames`), the
`Maybe Message` return of the two single-message visibility operations, the `isTransient`
whitelist, and the traced interpreter's span labels. Concerns examined: correctness of
dispatch in both interpreters, the retry classification, per-operation cost, and
documentation.

## Verified correct

Both interpreters dispatch every new constructor to the matching `Pgmq.Hasql.Sessions`
function. The traced interpreter labels grouped-head reads as consumer receive spans named
after the SQL function, labels the unvalidated listing with the same `pgmq.list_queues` as
the typed one, and gives the catalog-backed FIFO listing a library-owned label because no
`pgmq.*` function backs it. `isTransient` treats serialization failures, deadlocks, lock
timeouts, the three shutdown states, and class 53 as transient and everything else reported by
the server as permanent, which is the list design note 017 records and tests pin in both
directions. The umbrella module only added exports.

## Performance already captured elsewhere

The effectful-core 2.7.0.0 dynamic-dispatch overhead and the recommendation to prefer 2.7.1.1
are recorded in the dependency-bounds ADR and the 0.6.1.0 changelog; no new per-operation cost
was introduced in this package.
