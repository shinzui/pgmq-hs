# Revision history for pgmq-inspect

## Unreleased

A dedicated runtime-wide LISTEN backend refcounts notification channels, bounds
buffered batches, reconnects with backoff and restores LISTENs. Polling remains
authoritative. `InspectRuntime` exposes separate listener health and per-channel
acknowledgement. Graceful frame delivery shares one one-second window across
runner/runtime cleanup. The full contract and workload evidence are in design note 021.

WebSocket metrics subscriptions with shared per-queue refresh scheduling, bounded
outboxes and admission, generation-safe snapshots, poll convergence, frame and
connection caps, overflow signaling and scoped graceful shutdown via
`withInspectRuntime`.

Initial read-only HTTP inspection surface with configurable CORS, bounded shared admission, safe errors and a mountable application.
