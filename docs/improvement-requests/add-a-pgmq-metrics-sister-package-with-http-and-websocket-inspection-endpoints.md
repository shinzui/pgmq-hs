---
type: Improvement Request
title: Add a pgmq-metrics sister package with HTTP and WebSocket inspection endpoints
description: >-
  Create a sister package (working name pgmq-metrics) exposing an embeddable WAI application
  with queue listing, metrics, non-destructive browsing, topic bindings, health, CORS, and a
  NOTIFY-accelerated but poll-authoritative WebSocket live feed, so a browser can inspect pgmq
  queues without any process reaching into pgmq-owned tables.
generated:
  by: anthropic/claude-fable-5
  at: "2026-08-19T00:00:00Z"
requestId: IR-3
status: proposed
origin: mori://shinzui/keiro-ui
---

# Improvement Request: Add a pgmq-metrics Sister Package with HTTP and WebSocket Inspection Endpoints

## Status

Proposed by the keiro runtime UI initiative
(`mori://shinzui/keiro-ui/masterplans/1-keiro-runtime-ui-foundations`, filed under
`mori://shinzui/keiro-ui/plans/3-audit-pgmq-hs-and-file-ui-endpoint-improvement-requests`).
Queue-level views belong to pgmq-hs per the initiative's layer-ownership matrix
(`mori://shinzui/keiro-ui/okf/adrs/concepts/ADR-1`); higher layers (shibuya adapters, keiro)
consume these endpoints and add only their own semantics. This request builds on the
non-destructive reads of `mori://shinzui/pgmq-hs/okf/improvement-requests/concepts/IR-1` and
the JSON codecs of `mori://shinzui/pgmq-hs/okf/improvement-requests/concepts/IR-2`. It follows
the precedent chain of downstream needs driven upstream one repository at a time:
`mori://shinzui/keiro/okf/improvement-requests/concepts/IR-9` →
`mori://shinzui/shibuya/okf/improvement-requests/concepts/IR-2` →
`mori://shinzui/shibuya-pgmq-adapter/okf/improvement-requests/concepts/IR-1`.
Implementation is pgmq-hs's own downstream work.

## Problem

pgmq-hs has no process boundary a browser can reach: zero executables, and no `wai`, `warp`,
`websockets`, or any web dependency anywhere in the project (audited 2026-08-19 at HEAD
`9ee9a2f`, re-confirmed at filing time). The only alternative to a pgmq-hs-owned surface is for
some other process to query `pgmq.*` tables directly — exactly the schema-boundary violation
the whole runtime stack forbids elsewhere. Without this package, the composed UI has queue
screens with nothing to call.

## Requested Change

A new top-level package — working name `pgmq-metrics`, final name pgmq-hs's choice — following
the established sister-package pattern (`mori://shinzui/keiro-ui/okf/adrs/concepts/ADR-4`;
live precedents kiroku's `kiroku-metrics` and shibuya's `shibuya-metrics`): the package depends
on the pgmq-hs libraries, no core package gains a web dependency, and it exports both a
convenience runner (host chooses bind address and port) and the bare WAI `Application` so a
host can mount several inspection surfaces in one process.

Endpoints (read-only; wire format per the cross-project conventions — project
`mori://shinzui/keiro-ui`, path `docs/architecture/inspection-api-conventions.md`,
artifact-level URI pending — snake_case new fields, cursor pagination, structured error
envelope):

1. Queue listing via `listQueuesUnvalidated` (per `docs/design/016-queue-name-validation.md`,
   strict decoding can fail on foreign or mixed-case names, and an inspection surface must show
   what exists).
2. Per-queue and all-queue metrics via the existing `queueMetrics`/`allQueueMetrics`
   operations, labeling total versus visible depth per
   `docs/design/002-queue-visible-length.md`, with the `pgmq.metrics_all()` polling cost
   documented: upstream loops over `pgmq.meta` running per-queue `count(*)` scans — O(number of
   queues) — so the endpoint documentation must state the cost and recommend a sane poll
   interval.
3. Non-destructive queue browsing, archive browsing, and message-by-id, wrapping IR-1's reads.
4. Topic bindings and routing configuration.
5. Health probes.
6. A WebSocket live feed built on the `enable_notify_insert` channels (channel names from
   `notifyChannelName`), speaking the cross-project WebSocket convention
   (`mori://shinzui/keiro-ui/okf/adrs/concepts/ADR-2`: typed `type`-tagged frames, explicit
   subscribe/unsubscribe, snapshot-then-delta, ping/pong, bounded queues with in-band overflow
   signaling, `goodbye`). The delivery contract of
   `docs/design/015-notification-delivery-contract.md` binds this feed and must be stated in
   its documentation: NOTIFY is fire-and-forget, unqueued for disconnected listeners, and
   throttle-suppressed — a wake-up hint, never data. The feed's contract is therefore
   poll-driven freshness with push acceleration
   (`mori://shinzui/keiro-ui/okf/adrs/concepts/ADR-3`): every live view has an authoritative
   poll path, and a client that misses notifications still converges.
7. CORS support from day one, configured by the host with an explicit allowed-origins list,
   disabled by default — a browser page on another origin is the primary client.

Error mapping follows the typed error model of
`docs/design/013-pgmq-effectful-error-model.md` and
`docs/design/017-transient-error-classification.md`: transient classifications map to 503-class
responses, permanent ones to 4xx/500-class, with machine-readable snake_case error codes.

## Acceptance

1. With the example server running against a database with one queue,
   `curl http://localhost:<port>/queues` returns HTTP 200 with a JSON array containing that
   queue.
2. Queue metrics responses label total and visible depth distinctly, and the all-queue metrics
   endpoint's documentation states the O(queues) polling cost.
3. Browsing a queue over HTTP leaves consumers undisturbed (IR-1's non-destructive guarantee
   holds end to end through the endpoint).
4. A WebSocket client subscribing to a queue receives a snapshot, then insert notifications as
   they occur; killing the LISTEN connection does not wedge the feed — the authoritative poll
   path converges, and reconnection resumes pushes.
5. With CORS unconfigured, responses carry no CORS headers; with an allowed origin configured,
   a browser page on that origin can call every endpoint.
6. No existing pgmq-hs library package gains a web dependency; the new package exports the bare
   WAI `Application`.

## Non-goals

No mutation endpoints in the first iteration (no send, delete, purge, or visibility changes —
those need the safety discipline of a separate request). No authentication or authorization
design beyond honestly documenting its absence (the initiative's recorded posture: trusted
network or authenticating reverse proxy). No Prometheus exposition beyond what the conventions
require. No LISTEN-based library streaming API in pgmq-hs core — the listener loop lives in the
sister package unless pgmq-hs decides otherwise.
