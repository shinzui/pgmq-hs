---
type: Review
title: pgmq-hasql from v0.4.0.1 to v0.6.1.0 — NULL coalescing, grouped heads, and metrics projection are correct
description: Every client SQL change matches the vendored function it calls and the encoders bind in the right order; the only remark is that the test suites create the shared ephemeral root with the umask rather than the 0700 the changelog claims.
generated:
  by: anthropic/claude-fable-5-1
  at: "2026-09-16T20:57:04Z"
reviewId: REV-3
subject: mori://shinzui/pgmq-hs/packages/pgmq-hasql
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
outcome: commented
dimensions:
  - correctness
  - performance
  - documentation
  - test-coverage
produced:
  - mori://shinzui/pgmq-hs/masterplans/6-close-the-notification-reconciler-and-evidence-gaps-surfaced-by-the-0-6-1-0-review
  - mori://shinzui/pgmq-hs/plans/25-create-the-ephemeral-root-with-owner-only-permissions-and-record-acceptance-on-the-current-postgresql
context: >-
  Read the diff of pgmq-hasql/src between the two tags — statements, encoders, decoders,
  sessions, and the umbrella module — and the vendored pgmq 1.13.0 bodies of read,
  read_with_poll, pop, set_vt, metrics, metrics_all, create_fifo_index, list_queues, and
  format_table_name; read the test support module that pins the ephemeral root and the
  ephemeral-pg source that creates its registry; did not run the suites.
---

# pgmq-hasql from v0.4.0.1 to v0.6.1.0

## What was examined

The NULL-parameter coalescing on `read`, `read_with_poll`, `pop`, and
`enable_notify_insert`; the `conditional` filter now bound on `read`; `set_vt` decoding
as `Maybe Message`; the SQL-NULL body decode; the unvalidated queue listing; the
`pg_indexes` FIFO listing; the grouped-head statements; the explicit-premake creation; and
the metrics projection with the JSON lookup for the nullable eighth attribute. Concerns
examined: correctness against the vendored function bodies, parameter order between encoders
and SQL, per-call performance, and documentation.

## Remarks

The 0.6.1.0 changelog says the shared ephemeral root `/tmp/ephpg-pgmq-hs-<uid>` is "a fixed
path created `0700`". The three `EphemeralDb.hs` copies and the migration suite create it
with `createDirectoryIfMissing`, which applies the umask; only ephemeral-pg's registry inside
it is `0700`. The uid key already prevents collisions, so nothing breaks; the sentence
describes code that does not exist.

Performance changes are all negligible or expected: `to_jsonb(m)` per metrics row sits next
to a `count(*)`; `coalesce($4,'{}'::jsonb)` reaches the `ELSE 1` arm of the read functions'
`CASE`, so no per-row containment runs when no filter is given; the `pg_indexes` scan runs
once per reconcile.

## Verified correct

A bound NULL never triggers a plpgsql parameter default and `LIMIT NULL` means `LIMIT ALL`,
so the `coalesce($n,1)` on `read`, `read_with_poll`, and `pop` is the right fix, and
`'{}'` is the value the upstream functions test for "no filter". Every encoder binds
parameters in the order the SQL names them, including the six-parameter `read_with_poll`.
`set_vt` is `RETURNS SETOF`, so `rowMaybe` is the correct decoder for the single-message
variants. The FIFO listing's regex captures the same name `create_fifo_index` derives and
matches what `CREATE INDEX IF NOT EXISTS` would skip. The metrics projection lists the seven
stable columns and looks the eighth up by name, so it decodes on both 1.12 and 1.13. The
umbrella module only added exports. The transient SQLSTATE whitelist that pgmq-effectful
consults is reached through hasql's `ServerError` and is pinned in both directions by tests.
