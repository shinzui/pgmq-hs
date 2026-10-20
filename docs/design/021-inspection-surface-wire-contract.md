# Inspection surface wire contract

Status: adopted, 2026-10-20 UTC. Introduced by ExecPlan 29; the feed and standalone
server extend this contract in ExecPlans 30 and 31.

## The package

`pgmq-inspect` exports a mountable WAI `Application` through
`withInspectApplication`, a host-supplied effect runner (`InspectEnv`), checked
configuration, and the scoped Warp runners `withInspectServer` and
`runInspectServer`. Default binding is `127.0.0.1:9092`, with CORS disabled.
It depends on pgmq-hs, never the reverse, and needs no keiro, kiroku or shibuya.
Domain records use their owning packages' JSON instances.

## Routes

All paths are GET and relative to the mount root. Bodies are JSON.

| Path | Success body | Route-specific failures |
|---|---|---|
| `/` | `{service, version, websocket_path:"ws"}` | — |
| `/queues` | array of lenient queue records | — |
| `/queues/{queue}` | exact metadata queue record | `queue_not_found` |
| `/queues/{queue}/metrics` | QueueMetrics | `queue_not_found` |
| `/metrics` | array of QueueMetrics | — |
| `/queues/{queue}/messages` | page of Message | `queue_not_found`, `invalid_cursor`, `invalid_limit` |
| `/queues/{queue}/messages/{msg_id}` | Message | `queue_not_found`, `invalid_message_id`, `message_not_found` |
| `/queues/{queue}/archive` | page of ArchivedMessage | `queue_not_found`, `invalid_cursor`, `invalid_limit` |
| `/queues/{queue}/archive/{msg_id}` | ArchivedMessage | `queue_not_found`, `invalid_message_id`, `message_not_found` |
| `/queues/{queue}/bindings` | array of TopicBinding filtered by exact queue name | `queue_not_found` |
| `/bindings` | array of TopicBinding | — |
| `/routing/test?routing_key=…` | array of RoutingMatch | `invalid_routing_key` |
| `/notify/throttles` | array of NotifyInsertThrottle | — |
| `/health/live` | `{"alive":true}` without database work | — |
| `/health/ready` | `{ready,checks:[{name:"postgres",healthy,latency_ms,error}]}` | readiness body with status 503 when unhealthy |
| `/ws` | WebSocket upgrade | plain GET: `websocket_upgrade_required` |

A queue is resolved by its exact name in lenient metadata, including foreign and
mixed-case names. A missing queue is 404 even for bindings; a valid missing ID in
an existing table is `message_not_found`. An undefined table alone never implies
queue absence: scoped reads recheck metadata after 42P01. Missing global schema,
metadata or other tables remain database errors.

`queue_length` counts every row. `queue_visible_length` counts rows whose `vt`
has passed; the difference includes leased and delayed messages. Nullable
`default_partition_length` is unknown/unsupported, never zero by substitution.
`pgmq.metrics_all()` loops over `pgmq.meta`, running a `count(*)` scan per queue.
Its cost depends on total rows scanned. Use `/metrics` on demand, measure before
choosing intervals, and prefer the shared feed for repeated observation.

## Pagination

`from` is an optional exclusive msg_id cursor. `limit` defaults to 50 and is
silently capped at 500 (both configurable). The server reads limit+1, returns
limit items, and includes `next_cursor` only when the lookahead row exists.
The last page omits the key. The cursor is the last returned ID, not an offset.

IDs/from accept decimal digits in 0..9223372036854775807. Limits must be positive
decimal integers. Tokens longer than 128 characters, signed, empty or malformed
values are rejected before database work. Parsing uses Integer before narrowing;
a large positive limit within the token bound is capped before Int32 conversion.
Checked configuration reserves room for lookahead.

Clients treat cursors as opaque and echo the exact decimal value. Numeric IDs
above 2^53 require lossless JSON parsing before JavaScript Number conversion.
Browsing is a moving view: late producer commits and late archival can insert
older IDs behind a cursor. Restart at the first page to refresh; this is not a
replay or snapshot guarantee. Inspection leaves vt/read_ct/last_read_at unchanged.

## The error envelope

```json
{"error":{"code":"database_error","message":"The database could not complete the inspection request.","details":{"sqlstate":"23505"}}}
```

`details` is omitted when absent. Codes are stable; public messages may change.
Raw driver SQL, parameters, server message/detail/hint and call stacks never reach
responses, readiness checks or feed errors.

| Code | HTTP status | Meaning |
|---|---|---|
| `not_found` | 404 | no matching route |
| `method_not_allowed` | 405 | only GET; CORS may answer preflight OPTIONS |
| `queue_not_found` | 404 | metadata confirms queue absence |
| `message_not_found` | 404 | valid ID absent from the selected live/archive table |
| `invalid_cursor` | 400 | invalid from |
| `invalid_limit` | 400 | invalid limit |
| `invalid_message_id` | 400 | invalid ID path segment |
| `invalid_routing_key` | 400 | missing or invalid routing_key |
| `inspection_busy` | 503 | shared nonblocking admission is full; retry later |
| `inspection_timeout` | 503 | configured PostgreSQL statement deadline (57014) |
| `database_unavailable` | 503 | typed error classified transient by isTransient |
| `database_error` | 500 | remaining database errors |
| `websocket_upgrade_required` | 426 | plain GET /ws |
| `websocket_unavailable` | 503 | host selected rejecting upgrade seam |

## Resource and cost limits

One runtime owns `maxConcurrentReads` (default 2), shared by HTTP and feed work.
Full admission rejects promptly; it does not queue waiting database requests or
spawn detached retries. Hosts must supply a dedicated inspection pool/role with
finite statement_timeout and lock_timeout plus finite acquisition/connect budgets,
separate from worker connections. Standalone defaults are defined by ExecPlan 31.
A statement deadline is not a hard network deadline: the blackholed-reply issue
IR-6 remains. Best-effort readiness timeout cannot repair uninterruptible I/O.

A row cap does not cap bytes or query work. A one-row page may contain a 1 MiB
payload; partitioned reads may scan all leaves. See the measured cost evidence in
[019](019-non-destructive-inspection-reads.md). No automatic index DDL is applied.

## CORS

Disabled by default. `CorsAllowOrigins` accepts explicit HTTP(S) origins, with no
wildcards, credentials, path, query or fragment. Listed origins get echoed
Access-Control-Allow-Origin and Vary: Origin, never credentials. Preflight OPTIONS
is answered by middleware. Unlisted origins are served normally without CORS
headers, leaving the browser to enforce access. CORS is not authentication.

## Mounting under a prefix

Strip the prefix from WAI pathInfo before delegation; rawPathInfo may retain it.
Both HTTP and WebSocket upgrade routing use pathInfo. Descriptor links are
relative (`ws`), so use a mount-root base URL ending in `/` when resolving them.
Port 0 honors bindHost; startup is acknowledged before the continuation runs.
Socket and thread ownership is scoped across success, failure and cancellation.

## Authentication

None. Deploy only on a trusted network or behind an authenticating reverse proxy.
The surface is read-only but exposes message contents.

## Wire stability

Published shapes are frozen: never remove or re-type fields; additions are
allowed. Incompatible changes use new paths. Domain fields/null/timestamps/IDs
follow [020](020-json-wire-encodings.md). Wire envelopes have reviewed golden
fixtures whose absence fails tests rather than generating a passing fixture.

## WebSocket

Filled by [ExecPlan 30](../plans/30-add-the-notify-accelerated-poll-authoritative-websocket-live-feed-to-pgmq-inspect.md).

## Related documents

See [019](019-non-destructive-inspection-reads.md),
[020](020-json-wire-encodings.md),
[015](015-notification-delivery-contract.md),
[016](016-queue-name-validation.md),
[017](017-transient-error-classification.md), and the
[boundary ADR](../adr/queue-inspection-surface-boundary-and-wire-contract.md).
