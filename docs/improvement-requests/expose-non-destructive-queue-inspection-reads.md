---
type: Improvement Request
title: Expose non-destructive queue inspection reads
description: >-
  Add bounded, keyset-paginated reads that observe queue and archive contents without mutating
  visibility timeouts or read counts — peek, archive browsing, and fetch-by-message-id — so an
  inspection surface can show messages without disturbing consumers.
generated:
  by: anthropic/claude-fable-5
  at: "2026-08-19T00:00:00Z"
requestId: IR-1
status: proposed
origin: mori://shinzui/keiro-ui
---

# Improvement Request: Expose Non-Destructive Queue Inspection Reads

## Status

Proposed by the keiro runtime UI initiative
(`mori://shinzui/keiro-ui/masterplans/1-keiro-runtime-ui-foundations`, filed under
`mori://shinzui/keiro-ui/plans/3-audit-pgmq-hs-and-file-ui-endpoint-improvement-requests`). The
initiative is preparing a browser UI for operating keiro-runtime applications; queues are owned
by pgmq-hs, so the read primitives a queue browser needs belong here
(`mori://shinzui/keiro-ui/okf/adrs/concepts/ADR-1` records the ownership principle).
Implementation is pgmq-hs's own downstream work.

## Problem

Every message read pgmq-hs offers today leases: `pgmq.read`, `read_with_poll`, and `pop` all
mutate `vt` (the visibility timeout) and `read_ct` (the read counter) as a side effect of
returning rows. An operator who "just looks at" a queue through these APIs steals messages from
consumers for the duration of the visibility timeout and inflates read counts that retry and
dead-letter policies may depend on. An inspection surface must observe without disturbing.

Three specific reads are missing (audited 2026-08-19 at HEAD `9ee9a2f`, re-confirmed at filing
time):

1. **No non-destructive peek** over a queue table `pgmq.q_<name>`.
2. **No archive reads**: `archiveMessage` writes into `pgmq.a_<queue>`, but no statement,
   session, effect, or type reads an archive table back.
3. **No fetch-by-id**: a `MessageId` can be acted on (delete, archive, visibility change) but
   never fetched.

## Requested Change

Following the repository's layering recipe (statement → session → `Pgmq` umbrella re-export →
effect constructor → both interpreters):

1. A bounded, keyset-paginated, non-destructive peek over `pgmq.q_<name>`: ordered by `msg_id`,
   taking an exclusive `msg_id` cursor and a limit (never `OFFSET`), returning full message
   rows (id, body, headers, enqueue time, `vt`, `read_ct`) without modifying any row.
2. The equivalent bounded read over the archive table `pgmq.a_<name>`, including the archival
   timestamp.
3. Fetch a single message by `MessageId`, in both the queue table and the archive table,
   returning a typed not-found result rather than an error when absent.

On the vendored-SQL discipline of `docs/design/012-vendor-upstream-pgmq-sql.md`: upstream pgmq
provides no functions for any of these reads, so there is nothing to vendor; the new statements
are plain `SELECT`s over tables the upstream schema defines and duplicate no upstream function.
This request asks for exactly the kind of hand-written statement that note permits — reads that
upstream has no equivalent for — and none that it forbids.

Queue-name handling should follow `docs/design/016-queue-name-validation.md`: inspection reads
must work for foreign and mixed-case queue names (the lenient path), since an inspection
surface sees whatever exists in the database, not only queues this library created.

## Acceptance

1. Peeking N messages from a queue leaves every row's `vt` and `read_ct` byte-identical —
   asserted by a test that reads the raw rows before and after the peek.
2. A concurrently polling consumer observes no change in message availability while peeks run.
3. Keyset pagination is stable: paging through a queue by repeatedly passing the last `msg_id`
   as the cursor visits every message exactly once while the queue is quiescent, regardless of
   queue size, and never uses `OFFSET`.
4. Archive reads return archived messages with their archival timestamps; fetch-by-id returns
   the message when present and a typed not-found otherwise, in both tables.
5. All new reads decode queues with foreign or mixed-case names.
6. The operations are available through the `Pgmq` effect and both interpreters (plain and
   traced), and are implementable by a mock interpreter without a database.

## Non-goals

No new SQL that shadows an existing `pgmq.*` function (leasing reads stay on the vendored
functions). No retention, purge, or archival-policy semantics. No HTTP surface — that is the
separate sister-package request
(`mori://shinzui/pgmq-hs/okf/improvement-requests/concepts/IR-3`). No change to the semantics
of the existing leasing reads.
