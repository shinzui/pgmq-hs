---
title: "HTTP queue inspection"
type: Capability
description: "Embeddable read-only HTTP queue inspection with domain JSON, non-destructive browsing, bounded admission and explicit-origin CORS."
generated:
  by: openai/gpt-6.1-sol
  at: "2026-10-20T04:24:33Z"
capabilityId: CAP-12
provider: mori://shinzui/pgmq-hs
status: shipped
stability: experimental
since: "unreleased"
packages:
  - pgmq-inspect
requires:
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
  - kind: guide
    resource: docs/design/021-inspection-surface-wire-contract.md
    proves: Routes, pagination, errors, resource costs and the HTTP wire contract.
---

# HTTP queue inspection

List lenient queue names, inspect total and visible depth, browse live/archive
messages without leasing them, look up IDs, inspect bindings, test routing, list
notification throttles and check health through a WAI application or Warp runner.

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
The blackholed-reply issue IR-6 remains. Pagination observes a moving view; refresh
from the first page to discover late commits/archivals behind a cursor. WebSocket
and executable delivery are tracked by ExecPlans 30 and 31.
