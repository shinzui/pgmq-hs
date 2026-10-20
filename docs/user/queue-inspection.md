# Inspect queues with pgmq-inspect

`pgmq-inspect` serves queues, metrics, messages, archives, routing and health over
HTTP, plus a shared WebSocket metrics feed. Browsing never leases or removes a
message: `vt`, `read_ct` and `last_read_at` stay unchanged. Run the executable or
mount its WAI application in a Haskell host. It requires no keiro process or UI.
PGMQ 1.12 and 1.13 are supported.

## Running the standalone server

From this checkout:

```sh
nix develop
cabal build pgmq-inspect:exe:pgmq-inspect
cabal run pgmq-inspect:exe:pgmq-inspect -- --version
```

The version is `0.1.0.0`. Alternatively, `nix build .#pgmq-inspect` produces
`result/bin/pgmq-inspect`. The schema must already be installed in the database;
the inspector performs no migrations or notification-trigger installation.
Use an existing PGMQ extension installation or install the native schema through
`Pgmq.Migration.pgmqMigrations` and the pg-migrate ledger, as described in the
[migration guide](schema-migration.md). Use an owner connection for
schema installation and queue creation, then a separate inspection login below.

Set a libpq keyword connection string or PostgreSQL URL, preferably through the
environment so credentials stay out of shell history and process arguments:

```sh
export PGMQ_INSPECT_DATABASE_URL='host=/path/to/socket dbname=app user=pgmq_inspector'
cabal run pgmq-inspect:exe:pgmq-inspect -- --port 9092
```

Startup prints `pgmq-inspect 0.1.0.0 listening on http://127.0.0.1:9092`.
Then run:

```sh
base=http://127.0.0.1:9092
curl -sS "$base/health/ready"
curl -sS "$base/queues"
curl -sS "$base/openapi.json" | jq '.info.title, (.paths | length)'
```

Readiness returns `ready:true` and a healthy `postgres` check. The OpenAPI title
is `pgmq-inspect` and it describes 16 HTTP paths. The separate `/ws` endpoint is
a WebSocket protocol, documented below. A successful bind means HTTP is serving;
check readiness to confirm database access.

| Option | Default and meaning |
|---|---|
| `--database-url CONNINFO` | Overrides `PGMQ_INSPECT_DATABASE_URL` |
| `--host HOST` | `127.0.0.1`; IPv6 addresses are supported |
| `--port PORT` | `9092`; `0` chooses a free port printed at startup |
| `--cors-origin ORIGIN` | Repeat for each browser origin; absent means disabled |
| `--poll-interval-ms MS` | `5000`; authoritative metrics poll interval |
| `--pool-size N` | `4`; dedicated inspection connections, separate from workers |
| `--no-listener` | Disable LISTEN acceleration; keep authoritative polling |
| `--version`, `--help` | Print and exit without opening a database connection |

Missing, empty or unparsable connection strings exit with status 2 and one
sanitized diagnostic line. Invalid option values also exit 2. Ports must be in
0..65535; pool sizes and intervals must be positive and fit the runtime's checked
integer range. Numeric tokens are bounded to 128 decimal digits; milliseconds
are checked before multiplication. Configuration is validated before allocation.
Ctrl-C unwinds the scoped server, feed and pool. The feed attempts `goodbye`
within one shared one-second delivery window; stalled sockets can prevent delivery.

The executable applies `statement_timeout=2000ms` and `lock_timeout=250ms` to
every inspection connection, including the listener's startup settings. Other
supplied connection options are preserved, with those deadlines taking precedence.
Connection timeout is 2 seconds and pool acquisition timeout is 1 second. Its
shared active-read cap is 2, regardless of pool size. A server-side statement
limit is **not a hard end-to-end network deadline**: a blackholed response can
still stall driver cancellation and cleanup (IR-6). These limits do not promise
a total process-exit deadline.

## Least-privilege database access

The tested role recipe is
[`inspection-role.sql`](../../pgmq-inspect/test/fixtures/inspection-role.sql).
Run it as the actual queue-creating owner in a database containing pgmq, after
reviewing existing grants. It creates a separate, non-superuser login without
role/database creation, replication, inherited membership or RLS bypass. Configure
its password/authentication using your normal secret management.

The essential grants are schema USAGE, SELECT on pgmq tables **and sequences**,
and EXECUTE on this whitelist:

```sql
GRANT EXECUTE ON FUNCTION
  pgmq.list_queues(), pgmq.format_table_name(text,text),
  pgmq.metrics(text), pgmq.metrics_all(),
  pgmq.list_topic_bindings(), pgmq.list_topic_bindings(text),
  pgmq.test_routing(text), pgmq.validate_routing_key(text),
  pgmq.list_notify_insert_throttles()
TO pgmq_inspector;
```

Metrics read sequence `last_value`/`is_called`; SELECT on tables or membership in
`pg_monitor` alone is insufficient. Grant SELECT on sequences, not USAGE or UPDATE,
so the inspector cannot advance them with `nextval`. Direct queue/archive SELECT
backs non-destructive browsing. No mutation or consumption functions are granted.

PostgreSQL normally grants function EXECUTE to PUBLIC. The recipe revokes that
in the pgmq schema before granting the whitelist, and removes the creating owner's
future default PUBLIC EXECUTE grant. That default revocation is owner-wide,
across schemas: a schema-local default REVOKE does not undo the global grant.
Explicitly preserve the functions existing worker/admin roles need before applying
this change. Re-audit grants after a PGMQ upgrade, especially SECURITY DEFINER
functions, other role memberships and functions outside pgmq that can write on
another role's behalf. A default read-only transaction setting is defense in depth;
the tests turn it off and verify the ACL still denies writes.

The recipe grants default SELECT on future tables and sequences **for the owner
running it**. Repeat those defaults for every actual queue/partition creator,
including maintenance owners, and grant existing objects separately. Future queues
created after the grants are exercised by the integration test. Default privileges
are not retroactive and do not follow an unrelated creator automatically. The CLI
never creates this role or changes database grants itself.

## HTTP routes

All paths below are GET, relative to the mount root, and return JSON. CORS middleware
may answer preflight OPTIONS; other methods, including HEAD, receive 405.

| Path | 200 body |
|---|---|
| `/` | `{service,version,websocket_path:"ws",openapi_path:"openapi.json"}` |
| `/openapi.json` | OpenAPI 3.0 document |
| `/queues` | Array of Queue |
| `/queues/{queue}` | Queue |
| `/queues/{queue}/metrics` | QueueMetrics |
| `/metrics` | Array of QueueMetrics |
| `/queues/{queue}/messages?from=…&limit=…` | Message page |
| `/queues/{queue}/messages/{msg_id}` | Message |
| `/queues/{queue}/archive?from=…&limit=…` | ArchivedMessage page |
| `/queues/{queue}/archive/{msg_id}` | ArchivedMessage |
| `/queues/{queue}/bindings` | Array of TopicBinding |
| `/bindings` | Array of TopicBinding |
| `/routing/test?routing_key=demo.created` | Array of RoutingMatch |
| `/notify/throttles` | Array of NotifyInsertThrottle |
| `/health/live` | `{"alive":true}`; no database read |
| `/health/ready` | Readiness; unhealthy returns the same shape with status 503 |

Queue names resolve exactly against lenient metadata, including foreign names and
mixed case. Percent-encode the whole path segment with `encodeURIComponent` in a
browser; do not apply the Haskell strict queue-name validator to names being listed.
A missing queue is 404 on every scoped route, including bindings. A valid missing ID
in an existing table is `message_not_found`. A missing global schema/table is a
500 database error. Scoped undefined-table errors recheck metadata to distinguish
a concurrently dropped queue from another missing table.

The domain shapes are:

| Record | Fields (all present) |
|---|---|
| Queue | `queue_name`, `created_at`, `is_partitioned`, `is_unlogged` |
| Message | `msg_id`, `read_ct`, `enqueued_at`, `last_read_at`, `vt`, `message`, `headers` |
| ArchivedMessage | Message fields plus `archived_at` |
| QueueMetrics | `queue_name`, `queue_length`, `queue_visible_length`, `newest_msg_age_sec`, `oldest_msg_age_sec`, `total_messages`, `scrape_time`, `default_partition_length` |
| TopicBinding | `pattern`, `queue_name`, `bound_at`, `compiled_regex` |
| RoutingMatch | `pattern`, `queue_name`, `compiled_regex` |
| NotifyInsertThrottle | `queue_name`, `throttle_interval_ms`, `last_notified_at` |
| Readiness | `ready`, `checks` array of `{name,healthy,latency_ms,error}` |

Timestamps are ISO-8601 UTC strings. Optional record fields are explicit null:
`last_read_at`, headers, ages, partition estimate, readiness latency and error.
Bodies and headers can be **any JSON value**, including scalar, array and null;
absent/JSON-null optional headers normalize to null. On PGMQ 1.12 and ordinary
queues, `default_partition_length` is null, not an assumed zero. On 1.13 partitioned
queues it is a planner estimate for default-partition rows.

`queue_length` includes visible, leased and delayed rows. `queue_visible_length`
counts rows whose `vt` has passed. `total_messages` comes from the queue sequence;
it is not the current table size. Poll comparisons ignore only `scrape_time`, so
ages advancing on a nonempty idle queue can cause updates even when depth is constant.

## Browsing and pagination

The retained transcript uses a disposable `demo` queue with three sent messages
and message 1 archived. To prepare the same data in a **throwaway** database,
use its queue owner connection (not the inspection login):

```sh
psql "$PGMQ_OWNER_DATABASE_URL" -v ON_ERROR_STOP=1 <<'SQL'
SELECT pgmq.create('demo');
SELECT pgmq.send('demo','{"hello":"world"}');
SELECT pgmq.send('demo','{"hello":"world"}');
SELECT pgmq.send('demo','{"hello":"world"}');
SELECT pgmq.archive('demo',1);
SELECT pgmq.bind_topic('demo.*','demo');
SELECT pgmq.enable_notify_insert('demo',250);
SQL
```

These are fixture setup operations; the inspector offers no mutation endpoints.


```sh
curl -sS "$base/queues/demo/messages?limit=1"
curl -sS "$base/queues/demo/messages?from=2&limit=1"
curl -sS "$base/queues/demo/messages/2"
curl -sS "$base/queues/demo/archive"
curl -sS "$base/queues/demo/archive/1"
```

A page is `{"items":[…],"next_cursor":2}` when more rows exist. The final page
omits `next_cursor` entirely. Echo its exact decimal value as `from`; it is an
exclusive msg_id cursor, never an offset. Page size defaults to 50 and is capped
at 500, both configurable by an embedding host. An oversized positive limit is
accepted and capped before conversion, not rejected for exceeding 500.

IDs and cursors are decimal digits in 0..9223372036854775807. Limits are positive
decimal integers. Signed, empty, malformed or longer-than-128-character tokens
are rejected before database work. Numeric IDs remain JSON integers; a browser
must preserve tokens above 2^53 before converting them to JavaScript Number.
`BigInt(alreadyRoundedNumber)` cannot restore the original ID.

Pages are a moving view, not a shared transaction snapshot or replay log. Late
producer commits and late archival can insert lower IDs behind a cursor. Restart
at the first page to refresh. Inspection changes no lease fields and leaves
messages available to consumers.

## Errors and health

```sh
curl -sS "$base/queues/demo/messages?from=bad"
curl -sS "$base/health/live"
curl -sS "$base/health/ready"
```

The first request returns HTTP 400:

```json
{"error":{"code":"invalid_cursor","message":"from must be a non-negative integer msg_id.","details":{"from":"bad"}}}
```

Errors use `{error:{code,message,details?}}`; details is omitted when absent.
Codes are stable; human messages may change. Raw SQL, parameters, server diagnostics
and call stacks never appear in HTTP, readiness or WebSocket errors.

| Code | Status |
|---|---|
| `invalid_cursor`, `invalid_limit`, `invalid_message_id`, `invalid_routing_key` | 400 |
| `not_found`, `queue_not_found`, `message_not_found` | 404 |
| `method_not_allowed` | 405 |
| `websocket_upgrade_required` (plain GET `/ws`) | 426 |
| `database_error` | 500 |
| `inspection_busy`, `inspection_timeout`, `database_unavailable`, `websocket_unavailable` | 503 |

`inspection_busy` means shared capacity is full; retry later with backoff.
`inspection_timeout` identifies PostgreSQL statement cancellation (57014).
Transient driver/database errors map to `database_unavailable`. Other database
errors map to `database_error`, with at most SQLSTATE in details. Failed readiness
always uses Readiness with `ready:false` and a safe dependency error, including
when admission is full; latency may be null. Liveness and the document/descriptor
remain available without admission to database work.

## Live metrics over WebSocket

Connect to `ws://127.0.0.1:9092/ws` (or use `wss` behind your TLS proxy).
A new connection watches nothing. Send one queue per subscription:

```json
{"type":"subscribe","queue":"demo"}
{"type":"unsubscribe","queue":"demo"}
{"type":"ping"}
```

The server sends a snapshot before any update for that subscription:

```json
{"type":"snapshot","queue":"demo","metrics":{"queue_name":"demo","queue_length":2,"newest_msg_age_sec":1,"oldest_msg_age_sec":1,"total_messages":3,"scrape_time":"2026-10-20T05:52:12.962481Z","queue_visible_length":2,"default_partition_length":null},"push":false}
```

That frame was captured from the standalone smoke run, with the complete curl
and WebSocket conversation retained in the [transcript](../design/evidence/022-inspection-adoption/standalone-transcript.txt).
Later frames use `type:"update"`, the same `queue` and `metrics`, and
`source:"poll"` or `source:"notify"` in place of `push`. `ping` receives
`{"type":"pong"}`. Deliberate shutdown attempts `{"type":"goodbye"}` before
closing. WebSocket control pings also run every 30 seconds.

Errors are `{"type":"error","code":"overflow","message":"…","queue":null}`;
queue is always present, null for a connection-level error. Codes are
`invalid_frame`, `too_many_subscriptions`, `too_many_queues`, `frame_too_large`,
`overflow`, `queue_not_found`, `inspection_busy`, `inspection_timeout`,
`database_unavailable` and `database_error`. Bad frames never echo raw payloads.
An exhausted connection cap rejects the upgrade with HTTP 429 and the
`inspection_busy` envelope. An HTTP-only host seam can reject with 503
`websocket_unavailable`.

The default limits are 100 connections, 100 subscriptions per connection,
100 distinct active queues per runtime, 256 buffered frames per connection,
and 16,384 bytes for both individual frames and assembled messages. Full outboxes
drop the oldest frame and send an overflow error before the next retained frame.
Re-read HTTP or re-subscribe before treating subsequent updates as complete.
Unsubscribe invalidates queued frames from that generation; bytes already handed
to the writer cannot be recalled.

One runtime shares refreshes across all clients. Polls default to 5 seconds,
notifications debounce for 100ms, and starts for a queue are separated by at least
1 second. Least-recently-started eligible work preserves fairness; missed ticks
coalesce. HTTP and feed reads share the same cap of 2. A minimal bounded cooldown
lease survives the last unsubscribe to prevent churn from bypassing the floor;
active plus cooldown entries are capped at twice the active-queue cap.

`push:true` means registered notification interest for a validated non-partitioned
queue with listener settings. It does not promise a trigger, healthy connection
or LISTEN acknowledgement. Foreign names, partitioned queues and `--no-listener`
are poll-only (`push:false`). Even `push:true` queues without insert triggers
converge by polling. The single refcounted listener reconnects from 100ms up to
5 seconds backoff and reissues desired LISTENs. Its health/acknowledgement are
separate runtime observations, not HTTP readiness. Notifications are wake-up hints;
`source` is not an event ID or replay position. Re-read messages over HTTP.

## CORS and deployment

CORS is disabled by default. Allow your dashboard origin explicitly:

```sh
cabal run pgmq-inspect:exe:pgmq-inspect -- --cors-origin http://localhost:5173
curl -sS -i -H 'Origin: http://localhost:5173' "$base/health/live"
```

Listed origins receive an echoed `Access-Control-Allow-Origin` and `Vary: Origin`.
Unlisted origins are served without CORS headers. Wildcards, credentials, paths,
queries and fragments are rejected in the allow list. Browser enforcement of CORS
is not authentication; WebSocket access also needs your proxy's origin/auth policy.
There is no built-in authentication or TLS. Use a trusted network or an
authenticating reverse proxy: this read-only service exposes message contents.

## Embedding in your own server

Supply a dedicated inspection pool with finite statement, lock, connection and
acquisition limits, separate from worker pools. `poolEnv inspectionPool Nothing`
uses the plain interpreter with polling only. Pass dedicated connection settings
as `Just settings` to enable the runtime-wide listener. `tracedPoolEnv` adds the
host's OpenTelemetry tracer. Do not give the inspector unrestricted worker capacity.

```haskell
import Data.List (stripPrefix)
import Data.Text (Text)
import Network.HTTP.Types (status404)
import Network.Wai (Application, pathInfo, responseLBS)
import Pgmq.Inspect

mountUnder :: [Text] -> Application -> Application
mountUnder prefix app req respond =
  case stripPrefix prefix (pathInfo req) of
    Just rest -> app req {pathInfo = rest} respond
    Nothing -> respond (responseLBS status404 [] "Not found")

withEmbeddedInspection :: InspectConfig -> InspectEnv -> (Application -> IO a) -> IO a
withEmbeddedInspection config env k =
  withInspectApplication config env (k . mountUnder ["pgmq"])
```

Use `OverloadedStrings` for this example; it is compiled as
[`EmbeddingExample.hs`](../../pgmq-inspect/test/EmbeddingExample.hs) and exercised by
the client tests. Keep the continuation alive for the host server's lifetime.
`withInspectServer` scopes its own Warp server and reports the actual port;
`withInspectRuntime` exposes explicit application, shutdown and drain actions for
host lifecycle integration. Shutdown must run while the host's transport is still
available for a best-effort goodbye. `withInspectApplicationWith` is the explicit
WebSocket seam for HTTP-only or host-supplied upgrade handling.

Route using stripped WAI `pathInfo`; retain `rawPathInfo` if the host needs it for
logging. Both HTTP and upgrades then work under `/pgmq`. Canonicalize the mount root
to `/pgmq/` at the proxy/host. Resolve the descriptor's `ws` and `openapi.json`
relative to that trailing-slash URL. The served OpenAPI document explicitly sets
`servers:[{url:"."}]`, so clients resolve `/pgmq/openapi.json` to `/pgmq/queues`,
including encoded queue names. Do not substitute the absolute server URL `/`.

## Building an independent browser client

Fetch the descriptor, fetch its `openapi_path`, then generate your HTTP client
from that document. HTTP schemas describe every method/path/status; WebSocket
frames above are the separate live protocol. Client generators differ in numeric
handling: select an exact int64 representation and a lossless JSON decoder.

[`inspection-client.mjs`](../../pgmq-inspect/test/client/inspection-client.mjs)
is a dependency-free acceptance fixture that generates GET functions from the
served operations and checks root/prefix mounts and IDs `9007199254740993` and
`9223372036854775807`. It runs with Node 22 and uses the original token from a
JSON.parse reviver's `context.source`, before precision is lost. Feature-detect
that support in your target browser or use a lossless JSON library; ordinary
JSON.parse followed by BigInt conversion is insufficient. This tests the supplied
client fixture, not a particular third-party generator or the unimplemented
keiro-ui browser application.

Published JSON fields are snake_case and additive: fields are never removed or
retyped after release. Incompatible changes require new paths. See
[JSON policy](../design/020-json-wire-encodings.md),
[wire contract](../design/021-inspection-surface-wire-contract.md) and the
[OpenAPI golden](../../pgmq-inspect/test/golden/openapi.json).

## Measuring inspection cost

A page cap bounds rows returned, not payload bytes or query work. A single row
can contain a megabyte payload. Partitioned reads may scan/sort many leaves;
[100,000-row live/archive plans](../design/evidence/019-inspection-reads/README.md)
cover ordinary, unlogged, numeric and time partitions on both supported schemas.
Inspection applies no index DDL. Metrics scan queue rows; `/metrics` calls
`metrics_all`, which loops over queues and counts their rows.

Prefer the shared feed to per-tab polling, and use all-queue metrics on demand.
The [100-subscriber workload](../design/evidence/021-inspection-feed/README.md)
records shared read counts and material observed worker costs. At 50ms configuration,
inspection-on trials ran roughly 27.5–28k messages/s with batch p95 3.26–3.58ms,
versus 34–36k and about 1.8ms without inspection. At 500ms there were only 2–3
metrics reads per on trial, yet the shared-host throughput/p95 remained worse than
off trials. These are diagnostic measurements, not a causal estimate or timing gate;
external work affected the host. They do not establish negligible overhead or a
universally safe poll interval. Separate pools protect worker connection capacity,
not PostgreSQL CPU/IO. Measure against your worker SLOs, tune intervals/caps, and
use on-demand reads if continuous inspection exceeds the budget.

## Reproducing acceptance

Inside `nix develop`, `cabal test pgmq-inspect-test` runs disposable PostgreSQL,
curl, the actual executable, schema/codec checks, root/prefix Node clients,
read-only-role tests and live-feed faults. Node 22+, curl and PostgreSQL are
provided in the shell. The [adoption evidence](../design/evidence/022-inspection-adoption/README.md)
explains how to retain the actual startup/curl/WebSocket transcript. No persistent
development database is stopped or modified by those tests.
