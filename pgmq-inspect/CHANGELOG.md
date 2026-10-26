# Revision history for pgmq-inspect

## 0.7.0.0 -- 2026-10-26

Initial release with the six-package 0.7.0.0 family.

### New Features

- Mountable read-only HTTP inspection for non-destructive live/archive paging,
  lookup, metrics and bindings, with explicit-origin CORS, shared nonblocking
  admission and safe errors.
- Standalone `pgmq-inspect` executable with checked options, database URL environment
  fallback, CORS flags, actual bound-port reporting and isolated connections with
  finite statement/lock/connect/acquisition budgets. The CLI supports
  optparse-applicative 0.19; the library does not depend on the CLI parser.
- OpenAPI 3 at `/openapi.json`, advertised by the service descriptor. Relative links
  and server URLs support prefix mounts and the canonical trailing-slash root.
- WebSocket metrics subscriptions with shared per-queue refresh scheduling, bounded
  outboxes and admission, generation-safe snapshots, poll convergence, frame and
  connection caps, overflow signaling and scoped shutdown through
  `withInspectRuntime`.
- A runtime-wide LISTEN backend refcounts channels, bounds buffered batches,
  reconnects with backoff and restores LISTENs. Polling remains authoritative.
  `InspectRuntime` exposes separate listener health and per-channel acknowledgement.
  Graceful frame delivery shares one one-second window across runner/runtime cleanup.

### Other Changes

- Tests validate actual response schemas, exact int64 client operations and restricted
  role access. The inspection guide documents deployment, grants, routes, frames and
  measured workload limits; design note 021 retains the runtime contract and evidence.
- Ephemeral PostgreSQL roots are created/validated as owner-only (`0700`), existing
  owned roots are tightened, unsafe paths are rejected and registry contents survive.
