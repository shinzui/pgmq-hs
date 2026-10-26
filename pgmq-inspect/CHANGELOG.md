# Revision history for pgmq-inspect

## 0.7.0.0 -- 2026-10-25

Test infrastructure: create and validate owner-only (`0700`) ephemeral roots, tighten
existing owned directories, reject unsafe paths and preserve lifecycle registry contents.

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

Standalone `pgmq-inspect` executable with checked options, environment URL fallback,
CORS flags, actual bound-port reporting and isolated connections with finite
statement/lock/connect/acquisition budgets. Serves an OpenAPI 3 document at
`openapi.json`, advertised by the service descriptor. Relative server URLs support
prefix mounts, including the canonical trailing-slash root. Tests validate actual
responses and exact int64 client operations. The inspection guide documents role
grants, deployment, all routes/frames and measured workload limits.

Initial release at 0.7.0.0 with the six-package family. Internal family bounds move
together; this candidate preparation does not publish packages.

The initial CLI supports optparse-applicative 0.19, the released family exercised
by both Cabal and Nix; the library does not depend on the CLI parser.
