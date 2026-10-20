---
title: "HTTP and WebSocket queue inspection"
type: Capability
description: "Embeddable read-only HTTP and WebSocket queue inspection with domain JSON, non-destructive browsing, bounded admission, a shared live metrics feed and explicit-origin CORS."
generated:
  by: openai/gpt-6.1-sol
  at: "2026-10-20T05:23:41Z"
capabilityId: CAP-12
provider: mori://shinzui/pgmq-hs
status: shipped
stability: experimental
since: "unreleased"
packages:
  - pgmq-inspect
requires:
  - CAP-4
  - CAP-10
  - CAP-11
interface:
  - Pgmq.Inspect
evidence:
  - kind: test
    resource: pgmq-inspect/test/HttpSpec.hs
    proves: Domain response shapes, unchanged lease cells after browsing, separate inspection query deadlines and worker-pool progress.
  - kind: test
    resource: pgmq-inspect/test/CorsSpec.hs
    proves: Disabled, listed, unlisted and preflight origin behavior without credentials.
  - kind: test
    resource: pgmq-inspect/test/ErrorSpec.hs
    proves: Safe error classification, contextual queue absence and prompt saturated admission.
  - kind: test
    resource: pgmq-inspect/test/RouterSpec.hs
    proves: Database-free routing and integer overflow rejection.
  - kind: test
    resource: pgmq-inspect/test/ServerSpec.hs
    proves: Prefix mounts and upgrades, startup failure propagation, scoped thread/socket cleanup.
  - kind: test
    resource: pgmq-inspect/test/WebSocketSpec.hs
    proves: Frozen frames, snapshots before updates, caps, safe errors, idle ages, goodbye and host-owned server cleanup.
  - kind: test
    resource: pgmq-inspect/test/RefreshSpec.hs
    proves: Shared reads for 100 subscriptions, rate limits, fairness, generations, poll-hint coalescing and deadline recovery.
  - kind: test
    resource: pgmq-inspect/test/ListenerSpec.hs
    proves: Refcounts, one backend, UNLISTEN, bounded buffered batches, fd-wait cancellation and backend cleanup.
  - kind: test
    resource: pgmq-inspect/test/LiveFeedSpec.hs
    proves: Notify acceleration, poll convergence without triggers, foreign names and recovery after killing the listener backend.
  - kind: test
    resource: pgmq-inspect/test/OutboxSpec.hs
    proves: Deterministic capacity-one/two overflow ordering and stale queued-frame rejection.
  - kind: test
    resource: pgmq-inspect/test/RawWebSocketSpec.hs
    proves: Header-only rejection of a huge declared frame and an assembled fragmented-message limit.
  - kind: test
    resource: pgmq-inspect/test/IsolationSpec.hs
    proves: A real inspection connection can stall uncancellably without consuming worker capacity or creating unbounded retries.
  - kind: guide
    resource: docs/design/evidence/021-inspection-feed/README.md
    proves: Recorded consumer throughput, p95 and shared query counts with 100 subscribers, plus the measurement limits.
  - kind: guide
    resource: docs/design/021-inspection-surface-wire-contract.md
    proves: Routes, pagination, errors, resource costs and the HTTP wire contract.
---

# HTTP and WebSocket queue inspection

List lenient queue names, inspect total and visible depth, browse live/archive
messages without leasing them, look up IDs, inspect bindings, test routing, list
notification throttles and check health through a WAI application or Warp runner.
Subscribe over WebSocket for a snapshot followed by shared, rate-limited metric
updates. One refcounted listener accelerates eligible queues with insert hints;
polling stays authoritative throughout listener loss/reconnection.

## Shape

```haskell
import Pgmq.Inspect

serveInspection inspectionPool =
  runInspectServer defaultInspectServerConfig (poolEnv inspectionPool Nothing)
```

Supply a dedicated inspection pool with finite statement/lock/acquisition/connect
budgets. `withInspectApplication` mounts in an existing server; route on stripped
pathInfo when mounting under a prefix. JSON records come from the owning packages.

## Limits

Read-only; no authentication. Use a trusted network or authenticating proxy.
CORS is disabled by default and supports only explicit origins, without credentials.
Shared nonblocking admission bounds live reads. Row limits do not bound payload
bytes or scanned rows. `/metrics` performs a full count per queue; use on demand.
The blackholed-reply issue IR-6 remains; the graceful frame-drain window does not
bound a blackholed database call. The feed carries metrics only. Foreign and
partitioned queues are poll-only. Push reports registered notification interest,
not trigger presence, listener health or channel acknowledgement. Overflow requires
an HTTP re-read or re-subscription. Shared query counts still have real worker
latency/throughput cost; measure against SLOs before choosing caps and intervals. Pagination observes a moving view; refresh
from the first page to discover late commits/archivals behind a cursor. Standalone executable and OpenAPI delivery are tracked by ExecPlan 31.
