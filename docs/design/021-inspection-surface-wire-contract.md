# Inspection surface wire contract

Status: adopted, 2026-10-20 UTC. Introduced by ExecPlan 29; the feed extends this contract in ExecPlan 30. The standalone server and OpenAPI
document are delivered by ExecPlan 31.

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

Open `/ws` and explicitly subscribe; a new connection watches no queues. Every
successful subscription atomically publishes a snapshot and activates updates,
so an update cannot overtake its initial snapshot. Re-subscribe obtains a current
shared snapshot through the same rate-limited coordinator; it cannot bypass the
minimum refresh interval. Unsubscribe invalidates queued frames for that generation.
Bytes already handed to the socket writer cannot be recalled.

The exact client and server frames are:

```json
{"type":"subscribe","queue":"orders"}
```

```json
{"type":"unsubscribe","queue":"orders"}
```

```json
{"type":"ping"}
```

```json
{"type":"snapshot","queue":"orders","metrics":{"queue_name":"orders","queue_length":3,"newest_msg_age_sec":1,"oldest_msg_age_sec":9,"total_messages":12,"scrape_time":"2026-09-30T12:00:00Z","queue_visible_length":2,"default_partition_length":null},"push":true}
```

```json
{"type":"update","queue":"orders","metrics":{"queue_name":"orders","queue_length":3,"newest_msg_age_sec":1,"oldest_msg_age_sec":9,"total_messages":12,"scrape_time":"2026-09-30T12:00:00Z","queue_visible_length":2,"default_partition_length":null},"source":"notify"}
```

```json
{"type":"update","queue":"orders","metrics":{"queue_name":"orders","queue_length":3,"newest_msg_age_sec":1,"oldest_msg_age_sec":9,"total_messages":12,"scrape_time":"2026-09-30T12:00:00Z","queue_visible_length":2,"default_partition_length":null},"source":"poll"}
```

```json
{"type":"pong"}
```

```json
{"type":"error","code":"overflow","message":"The server dropped frames because this connection was not reading them; re-subscribe or re-read the queue over HTTP.","queue":null}
```

```json
{"type":"goodbye"}
```

Frames carry metrics only; re-read messages through HTTP. Error frames have code,
a safe human message and an always-present queue key (null when not queue-specific).
Codes are invalid_frame, too_many_subscriptions, too_many_queues, frame_too_large,
overflow, queue_not_found, inspection_busy, inspection_timeout,
database_unavailable and database_error. Invalid frames never echo raw payloads.
A full connection cap rejects the upgrade with HTTP 429 and the inspection_busy
envelope. A host can still select rejectingWebSocketApp for HTTP-only use.

Push means registered notification interest for a validated, non-partitioned queue
with a configured listener. It does not promise trigger installation, connection
health or LISTEN acknowledgement. Foreign and partitioned queues, and runtimes
without listener settings, are poll-only (push false). A valid queue without an
insert trigger can have push true and still receive only poll updates.

One runtime-wide listener backend (application_name pgmq-inspect-listener) owns
refcounted channel interest, using notifyChannelName rather than re-deriving names.
It reconciles LISTEN/UNLISTEN, drains at most 64 buffered notifications per slice,
and brackets its socket-read registration on every exit. After failure it reconnects
with exponential backoff from 100ms to 5s and reissues desired LISTENs. Connection
health and per-channel acknowledgement are separate read-only runtime observations;
neither is HTTP readiness. Polls keep running during listener failure.

A shared coordinator reads metrics once per distinct eligible queue, never per
browser tab. Notifications set one dirty flag per queue and coalesce for the
100ms default debounce. Polling defaults to 5s; starts for a queue are separated by
at least metricsMinRefreshUs (default 1s). HTTP and feed reads share maxConcurrentReads
(default 2). Least-recently-started eligible work keeps a hot queue from starving
another. Missed ticks coalesce; no catch-up jobs accumulate. A ready hint and due
poll can share one notify-tagged read while advancing the poll deadline.

Poll updates emit when any metrics field except scrape_time changes. Ages advance
on nonempty idle queues, so unchanged row counts do not imply a quiet feed. Notify
updates emit even with an unchanged fingerprint. The source tag identifies the
refresh driver, not a reliable event, a message or a replay position.

Per connection, wsMaxSubscriptions defaults to 100, and wsQueueCapacity to 256.
There are at most 100 connections and wsMaxQueues (default 100) active distinct
queues per runtime. After the last subscriber leaves, listener interest and cached
metrics are removed. A minimal cooldown lease survives through the minimum-refresh
window; active plus cooldown entries are capped at twice wsMaxQueues, and exhausted
cooldown capacity answers inspection_busy. This prevents subscription churn from
resetting the refresh floor without retaining an unbounded historical map.

Both individual frames and assembled messages are capped at wsMaxFrameBytes
(default 16,384) by the WebSocket library before unbounded receive accumulation.
One bounded writer handles every JSON frame. Full outboxes drop oldest frames and
send an overflow error before the next retained frame. Treat subsequent updates as
incomplete until re-reading HTTP or re-subscribing. The writer stops on send failure.
WebSocket control pings run every 30s; JSON ping/pong remains available.

Shutdown stops new subscriptions and refreshes, queues goodbye and attempts a
one-second graceful delivery window before cancelling/joining transports. The runner
keeps Warp alive for that attempt; host-owned Warp need not stop for runtime cleanup.
The one-second window is shared across runner and runtime cleanup, not restarted
in each phase. A stalled socket may prevent delivery. Database cleanup still has
the explicit IR-6 cancellation limitation; this delivery window is not a network
or total process-exit deadline.

Polling is truth, notifications are wake-up hints. A client that misses every
notification still converges through authoritative metrics reads; there is no
push-only inspection information. Those reads scan rows. The retained
[workload evidence](evidence/021-inspection-feed/README.md) shows shared query counts
and material observed throughput/p95 costs with 100 subscribers, even on a short
ordinary-queue fixture. Measure against worker SLOs before tuning caps/intervals;
isolated pools do not isolate database CPU/IO. Use on-demand HTTP reads when
continuous observation cannot meet that budget.

These frame fields are a compatibility surface once released: additive changes
only; incompatible protocol changes need a new path. The owning JSON policies
and explicit-null rules apply unchanged.

## Related documents

See [019](019-non-destructive-inspection-reads.md),
[020](020-json-wire-encodings.md),
[015](015-notification-delivery-contract.md),
[016](016-queue-name-validation.md),
[017](017-transient-error-classification.md), and the
[boundary ADR](../adr/queue-inspection-surface-boundary-and-wire-contract.md).
