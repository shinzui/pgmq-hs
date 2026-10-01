---
id: 30
slug: add-the-notify-accelerated-poll-authoritative-websocket-live-feed-to-pgmq-inspect
title: "Add the NOTIFY-accelerated, poll-authoritative WebSocket live feed to pgmq-inspect"
kind: exec-plan
created_at: 2026-10-01T00:15:41Z
intention: "intention_01m3tcw9vmeeftdtqj53d1d6nb"
master_plan: "docs/masterplans/7-expose-non-destructive-queue-inspection-through-typed-reads-json-codecs-and-the-pgmq-inspect-sister-package.md"
provenance:
  created_by:
    model: "claude-fable-5-1"
    harness: "claude-code"
    at: 2026-10-01T00:15:41Z
---

# Add the NOTIFY-accelerated, poll-authoritative WebSocket live feed to pgmq-inspect

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.
If durable project context changes, update or create ADRs in docs/adr/ in the same change.


## Purpose / Big Picture

After `docs/plans/29-create-the-pgmq-inspect-sister-package-with-the-http-inspection-surface.md`
lands, a browser can ask the `pgmq-inspect` package what a queue looks like right now: its row,
its metrics, its messages. What it cannot do is *watch* a queue. It has to poll an HTTP route on
a timer, and the timer is a trade-off it cannot win: a short interval hammers the database with
`count(*)` scans, a long interval shows stale numbers.

This plan adds the WebSocket live feed that improvement request `IR-3`
(`docs/improvement-requests/add-a-pgmq-metrics-sister-package-with-http-and-websocket-inspection-endpoints.md`)
asks for in its sixth requested change and fourth acceptance item. A client opens a WebSocket to
`/ws`, sends `{"type":"subscribe","queue":"orders"}`, and immediately receives a `snapshot` frame
carrying the queue's current metrics. From then on it receives `update` frames whenever the
queue changes. Two things drive those updates. One is PostgreSQL's `LISTEN`/`NOTIFY`: when the
queue has insert notifications enabled, every insert wakes the server through the channel
`notifyChannelName` computes, and the server pushes fresh metrics within a debounce window of
about a hundred milliseconds. The other is an authoritative poll: on a configurable interval the
server re-reads every subscribed queue's metrics and pushes an `update` whenever they changed.
The second driver is the one the contract rests on. PostgreSQL notifications are fire-and-forget,
are not queued for a listener that is momentarily disconnected, and are deliberately suppressed by
the queue's throttle interval, so a feed that trusted them would silently show stale data. Here a
client that misses every notification still converges, and a server whose `LISTEN` connection is
killed keeps serving updates from the poll while it reconnects in the background. Each `update`
says which driver produced it (`"source":"notify"` or `"source":"poll"`), so a client can see the
mechanism working.

You can see it working end to end from the test suite: `WebSocketSpec` subscribes with a real
WebSocket client, sends messages through the library, watches notify-sourced updates arrive
within two seconds, then kills the server's `LISTEN` backend with `pg_terminate_backend`, proves
updates keep arriving from the poll, and proves notify-sourced updates resume after the
reconnect. That last sequence is exactly the acceptance `IR-3` states: "killing the LISTEN
connection does not wedge the feed — the authoritative poll path converges, and reconnection
resumes pushes."

The feed carries queue metrics, never message bodies. A notification tells the server only that
*something* was inserted, not what; the honest live signal is therefore "this queue changed; here
are its metrics now", and a client that wants the messages re-reads them through the paged HTTP
routes, which keeps the HTTP path authoritative exactly as the cross-project conventions require.


## Progress

- [ ] M1: `Pgmq.Inspect.WebSocket` exists with the frame types, hand-written JSON instances, and golden files for every frame under `pgmq-inspect/test/golden/`
- [ ] M1: per-connection machinery (bounded outbox with drop-oldest and overflow signaling, reader, writer, poller, hint consumer, shutdown watcher) and the shared `WebSocketState` with the connection cap
- [ ] M1: `Pgmq.Inspect.Server` gains `withInspectRuntime`; `withInspectApplication` keeps its signature and delegates; `withInspectServer` signals shutdown and drains before cancelling Warp
- [ ] M1: `WebSocketSpec` green with no listener configured: `snapshot` with `"push":false`, poll-sourced `update`, `pong`, `queue_not_found`, `invalid_frame`, `too_many_subscriptions`, `overflow`, `goodbye`
- [ ] M2: `Pgmq.Inspect.Listener` exists: one server-wide `LISTEN` connection, refcounted channel registry, `threadWaitReadSTM` wait loop over `onLibpqConnection`, reconnect with exponential backoff and re-`LISTEN`, `listenerHealthy`
- [ ] M2: subscribe registers validated, non-partitioned queues for push; `snapshot` says `"push":true`; notify-sourced updates arrive within two seconds; debounce coalesces bursts
- [ ] M2: the backend-kill test proves the feed does not wedge and that pushes resume after reconnection
- [ ] M3: Haddock on `Pgmq.Inspect.WebSocket` is the protocol reference; the WebSocket section of `docs/design/021-inspection-surface-wire-contract.md` is written
- [ ] M3: the capability record plan 29 created lists the feed and `WebSocketSpec` as evidence; `pgmq-inspect/CHANGELOG.md` and root `CHANGELOG.md` carry `Unreleased` entries; `just docs-check` passes


## Surprises & Discoveries

Document unexpected behaviors, bugs, optimizations, or insights discovered during
implementation. Provide concise evidence.

(None yet.)


## Decision Log

- Decision: The feed carries `QueueMetrics` and a `source` tag, never message bodies.
  Rationale: a `NOTIFY` carries no payload, is throttled, and is lossy (design note 015), so the
  only honest live signal is "this queue changed; here are its metrics"; message contents are
  re-read through the paged HTTP routes, which stay the authoritative read path
  (`mori://shinzui/keiro-ui/okf/adrs/concepts/ADR-3`). Date: 2026-09-30
- Decision: One server-wide `LISTEN` connection shared by every WebSocket client, with a
  refcounted channel registry, rather than one `LISTEN` connection per client. Rationale: a
  hundred browser tabs must not hold a hundred PostgreSQL backends; the registry makes
  reconnection a pure function of "which channels have subscribers" so re-`LISTEN` after a drop
  is total and idempotent. This is the design the MasterPlan names as a candidate for promotion
  to an ADR if the retrospective finds it durable. Date: 2026-09-30
- Decision: Wait for socket readability with `Control.Concurrent.threadWaitReadSTM` inside
  `Hasql.Session.onLibpqConnection`, composed with the registry-changed flag in one `orElse`,
  instead of depending on `hasql-notifications`. Rationale: that package's `waitForNotifications`
  blocks a hasql `Connection` forever, which would stop us issuing `LISTEN` for new
  subscriptions on the same connection; and its release line pins a different hasql range than
  the git revision `cabal.project` pins. The loop below is ten lines on top of the same libpq
  calls it uses. Date: 2026-09-30
- Decision: Subscribing to a queue whose name fails `parseQueueName`, or that is partitioned, or
  when no listener is configured, succeeds with `"push":false`. Rationale: `notifyChannelName`
  is typed over `QueueName` and design note 015 forbids re-deriving the channel formula; a
  partitioned queue never publishes on the parent channel (design note 015 after
  `docs/plans/23-gate-the-notification-fail-open-on-a-real-queue-row-and-state-the-partitioned-queue-contract.md`);
  in all three cases the poll path still converges, which is the authoritative one anyway.
  Date: 2026-09-30
- Decision: A notify-sourced refresh always emits an `update`, even if the metrics fingerprint
  is unchanged; a poll-sourced refresh emits only on change. Rationale: a notification means an
  insert happened, and a client that sees `"source":"notify"` learns the wake-up path works even
  when a consumer drained the message in between; the poll is a steady-state reconciler and
  should stay quiet when nothing moved. Date: 2026-09-30
- Decision: Every outbound frame, including `pong`, `snapshot`, and `error`, goes through the
  per-connection bounded outbox and one writer thread. Rationale: a single writer keeps frame
  order deterministic (the `overflow` error always precedes the first frame after a drop) and
  avoids concurrent writes on one `WS.Connection`. Date: 2026-09-30
- Decision: Readiness (`GET /health/ready`) is left unchanged by this plan; `listenerHealthy` is
  exposed through `InspectRuntime` for tests and hosts. Rationale: the readiness body is a
  published shape owned by plan 29; adding a listener check is additive and can follow once
  plan 31 documents it, but a server whose listener is reconnecting is still ready to serve
  reads. Date: 2026-09-30
- Decision: Server shutdown sends `goodbye` and waits up to one second for connections to drain
  before Warp is cancelled. Rationale: the conventions require `goodbye` before any
  server-initiated close so a client can tell a deliberate shutdown from a dropped connection;
  an unbounded drain would let one stuck client block shutdown. Date: 2026-09-30


## Outcomes & Retrospective

Summarize outcomes, gaps, and lessons learned at major milestones or at completion.
Compare the result against the original purpose. Before marking the plan complete,
distill durable project context from the Decision Log, Surprises & Discoveries, and
this section into docs/adr/. Keep task-local execution details here.

(To be filled during and after implementation.)


## Context and Orientation

### The repository and the package this plan extends

pgmq-hs is a multi-package Cabal project in this repository: `pgmq-core` (types),
`pgmq-hasql` (statements and sessions over the hasql driver), `pgmq-effectful` (a `Pgmq` effect
for the effectful library with a plain and an OpenTelemetry-traced interpreter), `pgmq-migration`,
`pgmq-config`, and `pgmq-bench`. The toolchain comes from Nix: run every build and test through
`nix develop --command …` from the repository root, and run `nix fmt` before every commit or the
pre-commit hook rejects it. Two environment quirks cost time: GNU sed is first on `PATH`, so
in-place edits are `sed -i -e '…'` (never `sed -i ''`), and the session scratchpad path is too
long for a PostgreSQL Unix socket, so any scratch server must listen on TCP.

This plan builds on the package `pgmq-inspect`, created by
`docs/plans/29-create-the-pgmq-inspect-sister-package-with-the-http-inspection-surface.md`. That
package is the HTTP inspection surface of MasterPlan 7: a WAI `Application` (WAI is the standard
Haskell interface between a web server and an application; an `Application` is a plain value a
host mounts into a server such as Warp) that serves queue listings, metrics, non-destructive
message browsing, bindings, and health probes as JSON. Before starting, verify the following
exist exactly as this plan assumes; the names come from the MasterPlan's Integration Points,
and plan 29 may have refined them, in which case use plan 29's names and treat the shapes below
as the binding contract:

```bash
cd /Users/shinzui/Keikaku/bokuno/libraries/pgmq-hs-project/pgmq-hs
ls pgmq-inspect/src/Pgmq/Inspect/ pgmq-inspect/test/
grep -n "pollIntervalUs\|notifyDebounceUs\|wsMaxConnections\|wsMaxSubscriptions\|wsQueueCapacity" pgmq-inspect/src/Pgmq/Inspect/Config.hs
grep -n "runInspection\|listenerSettings\|type Inspection" pgmq-inspect/src/Pgmq/Inspect/Env.hs
grep -n "withInspectApplication\|withInspectApplicationWith\|withInspectServer\|rejecting" pgmq-inspect/src/Pgmq/Inspect/Server.hs
grep -n "websocketsApp\|\[\"ws\"\]" pgmq-inspect/src/Pgmq/Inspect/Http.hs
grep -n "queue_not_found\|database_unavailable\|database_error\|ErrorBody" pgmq-inspect/src/Pgmq/Inspect/Error.hs
grep -n "withPgmqDb\|withTestServer" pgmq-inspect/test/EphemeralDb.hs pgmq-inspect/test/TestServer.hs
grep -n "queueMetricsUnvalidated" pgmq-effectful/src/Pgmq/Effectful/Effect.hs pgmq-hasql/src/Pgmq/Hasql/Sessions.hs
grep -n "instance ToJSON QueueMetrics" pgmq-hasql/src/Pgmq/Hasql/Statements/Types.hs
grep -n "tasty-golden\|websockets\|wai-websockets" pgmq-inspect/pgmq-inspect.cabal
```

What this plan assumes those files provide:

- `Pgmq.Inspect.Config` exports `InspectConfig` with (among other fields) `pollIntervalUs :: Int`
  (default `5_000_000`), `notifyDebounceUs :: Int` (default `100_000`), `wsMaxConnections :: Int`
  (default `100`), `wsMaxSubscriptions :: Int` (default `100`), and
  `wsQueueCapacity :: Numeric.Natural.Natural` (default `256`), plus `defaultInspectConfig`.
- `Pgmq.Inspect.Env` exports `type Inspection a = Eff '[Pgmq, Error PgmqRuntimeError, IOE] a`
  and `InspectEnv { runInspection :: forall a. Inspection a -> IO (Either PgmqRuntimeError a), listenerSettings :: Maybe Hasql.Connection.Settings.Settings }`,
  with constructors `poolEnv :: Pool -> Maybe Settings -> InspectEnv` and `tracedPoolEnv`.
- `Pgmq.Inspect.Server` exports `withInspectApplication :: InspectConfig -> InspectEnv -> (Application -> IO a) -> IO a`,
  `withInspectServer`, `runInspectServer`, and `RunningInspectServer { serverPort, serverThread }`,
  and has an internal `withInspectApplicationWith :: InspectConfig -> InspectEnv -> WS.ServerApp -> (Application -> IO a) -> IO a`
  whose `WS.ServerApp` argument is the seam this plan fills; plan 29 passes a rejecting app
  there.
- `Pgmq.Inspect.Http` routes on WAI `pathInfo` and, on `["ws"]`, calls
  `Network.Wai.Handler.WebSockets.websocketsApp WS.defaultConnectionOptions seam req`, answering
  426 with code `websocket_upgrade_required` when the request is not an upgrade.
- `Pgmq.Inspect.Error` exports the error-body type (fields: code, message, optional details) and
  a classifier from `PgmqRuntimeError` to an HTTP status and body that yields
  `queue_not_found` for SQLSTATE `42P01`, `database_unavailable` when
  `Pgmq.Effectful.isTransient` holds, and `database_error` otherwise. This plan reuses that
  classifier's *code* and *message* for `error` frames; if plan 29 exposed it as a function
  returning `(Status, ErrorBody)`, take the body's code and message and ignore the status.
- `pgmq-inspect/test/EphemeralDb.hs` is the family's per-package copy of the ephemeral
  PostgreSQL harness, exporting `withPgmqDb :: (Pool -> Database -> IO a) -> IO (Either StartError a)`
  (the `Database` handle is needed here because `EphemeralPg.connectionSettings :: Database -> Settings`
  is how the test builds listener settings), and `pgmq-inspect/test/TestServer.hs` starts the
  application on an OS-assigned port and hands the port to a test.
- `pgmq-effectful` exposes `queueMetricsUnvalidated :: (Pgmq :> es) => Text -> Eff es QueueMetrics`
  (the `pgmq.metrics($1)` call with the name passed as text so a foreign name works), added by
  `docs/plans/27-add-non-destructive-peek-archive-and-lookup-reads-across-the-pgmq-layers.md`.
  If the grep above finds nothing, this plan adds it in Milestone 1 (see the fallback paragraph
  there).
- `QueueMetrics` (in `pgmq-hasql/src/Pgmq/Hasql/Statements/Types.hs`) has the hand-written
  `ToJSON` instance from `docs/plans/28-provide-stable-json-codecs-for-the-inspection-facing-records.md`
  with fields `queue_name`, `queue_length`, `newest_msg_age_sec`, `oldest_msg_age_sec`,
  `total_messages`, `scrape_time`, `queue_visible_length`, `default_partition_length`. It has
  no `Eq` instance (it derives only `Generic` and `Show`), which is why change detection below
  compares a projected tuple.
- `pgmq-inspect.cabal` already depends on `websockets ^>=0.13` and `wai-websockets ^>=3.0` for
  the seam, and its test suite on `tasty-golden`. If any is missing, add it where this plan says.

### The notification contract you are building on

`pgmq.enable_notify_insert(queue, throttle_ms)` installs a trigger on the queue's physical table
that raises `PG_NOTIFY` after an insert. The channel is `pgmq.q_<lowercased queue name>.INSERT`,
computed for you by `Pgmq.Types.notifyChannelName :: QueueName -> Text`; never assemble it by
hand. The name contains dots, so `LISTEN` needs it double-quoted:

```sql
LISTEN "pgmq.q_orders.INSERT";
```

Three properties of the mechanism, documented in
[docs/design/015-notification-delivery-contract.md](../design/015-notification-delivery-contract.md),
shape every decision in this plan. First, `NOTIFY` is fire-and-forget: a listener that is
disconnected, even for the few hundred milliseconds of a reconnect, misses notifications
permanently; nothing is queued. Second, the throttle suppresses notifications by design: with
`throttle_interval_ms = 250` (the default when `Nothing` is passed to `EnableNotifyInsert`), a
burst of inserts produces one notification per 250 ms; the listener is told *that* messages
exist, never how many. Third, the payload is empty; a notification is a wake-up hint, not data.
Therefore every consumer of notifications, this feed included, must keep a poll fallback, and
the poll, not the push, is the source of truth. Design note 015 also records that on a
partitioned queue the trigger clones onto leaf partitions and never publishes on the parent's
channel, so partitioned queues effectively receive no insert notifications; after
`docs/plans/23-gate-the-notification-fail-open-on-a-real-queue-row-and-state-the-partitioned-queue-contract.md`
that is the documented contract. The feed treats partitioned queues as poll-only.

A queue's metrics come from `pgmq.metrics(queue_name)`, which runs `count(*)` over the queue
table (so it costs a scan per call, proportional to the queue's size) and returns
`queue_length` (every row, visible or not), `queue_visible_length` (rows whose `vt` has
expired, so available to a consumer), `newest_msg_age_sec`, `oldest_msg_age_sec`,
`total_messages` (the identity sequence's last value, a lifetime counter), `scrape_time`, and on
PGMQ 1.13 a planner estimate `default_partition_length` for partitioned queues (`Nothing`
otherwise; see [docs/adr/pgmq-1.12-1.13-compatibility.md](../adr/pgmq-1.12-1.13-compatibility.md)).
Total versus visible depth is the distinction [docs/design/002-queue-visible-length.md](../design/002-queue-visible-length.md)
explains. The poll interval therefore trades freshness against scan cost; the default of five
seconds is conservative, and the user guide (plan 31) must say so.

### Queue names

`Pgmq.Types.parseQueueName` accepts `[a-z0-9_]{1,47}` only. The server accepts more (its only
check is length), and an inspection surface must show whatever exists, so the HTTP routes and
this feed take the queue name as plain `Text` and use the lenient `listQueuesUnvalidated`
listing and `queueMetricsUnvalidated` read. The one place a validated name matters here is the
`LISTEN` channel: `notifyChannelName` requires a `QueueName`, so push acceleration is offered
only for names `parseQueueName` accepts. Design note
[docs/design/016-queue-name-validation.md](../design/016-queue-name-validation.md) explains why
mixed-case names are rejected and why the formula must not be re-derived elsewhere.

### The typed error model

Every database operation runs through `InspectEnv.runInspection`, which returns
`Either PgmqRuntimeError a`. `PgmqRuntimeError` (from `Pgmq.Effectful`) has three constructors
mirroring hasql-pool's errors; `isTransient` says whether a retry is worth it. The error model is
[docs/design/013-pgmq-effectful-error-model.md](../design/013-pgmq-effectful-error-model.md) and
the classification policy is
[docs/design/017-transient-error-classification.md](../design/017-transient-error-classification.md).
`error` frames reuse plan 29's mapping so the HTTP envelope and the frame speak one code
vocabulary.

### The two sister-package precedents

Two shipped packages speak the same structural protocol and this plan borrows from both.
`shibuya-metrics` (`mori://shinzui/shibuya`, file `shibuya-metrics/src/Shibuya/Metrics/WebSocket.hs`)
contributes the shared `WebSocketState` with a connection count, a cap, and a
`shutdownRequested` `TVar`; the `acquireConnection`/`releaseConnection` STM pair with a
`mask` around the slot; the `race_` of a receive loop and a push loop; the
`waitForPushOrShutdown` pattern (`registerDelay` composed with the shutdown flag through
`orElse`) that lets the push loop send `goodbye` on shutdown; and change detection by comparing
the last sent metrics. `kiroku-metrics` (`mori://shinzui/kiroku`, file
`kiroku-metrics/src/Kiroku/Metrics/WebSocket.hs`) contributes the positional (field-less)
frame constructors to avoid record-selector clashes under re-export, `WS.withPingThread conn 30`,
treating `WS.ConnectionException` as a normal end of connection, a bounded per-connection queue
with drop-oldest overflow signaled by an in-band `error` frame, and `sendMsg` swallowing a
closed-connection exception so cleanup never rethrows. Neither precedent has a
PostgreSQL `LISTEN` loop; that part is new here and is modelled on
`hasql-notifications` (`mori://hasql/hasql`, file `hasql-notifications/src/Hasql/Notifications.hs`),
whose `waitForNotifications` does `PQ.notifies` → `PQ.socket` → `threadWaitRead` →
`PQ.consumeInput` in a loop. We do not depend on that package: its loop holds the hasql
`Connection` forever, which would prevent issuing `LISTEN` for a new subscription on the same
connection, and its published bounds track a different hasql line from the git revision
`cabal.project` pins.

### The hasql escape hatch

The pinned hasql exports `Hasql.Session.onLibpqConnection` with this signature (quoted from
`hasql/src/library/Hasql/Engine/Contexts/Session.hs` in the pinned revision):

```haskell
onLibpqConnection ::
  (Pq.Connection -> IO (Either SessionError a, Pq.Connection)) ->
  Session a
```

It hands the raw `Database.PostgreSQL.LibPQ.Connection` to a callback inside a session and takes
the (possibly replaced) connection back. Its documentation says throwing is acceptable and
leads to the connection being reset. `Hasql.Connection.use :: Connection -> Session a -> IO (Either SessionError a)`
takes the connection's `MVar` for the duration of the session, so only one session runs on a
connection at a time; the loop below is the only user of the listener connection. Connection
settings (`Hasql.Connection.Settings.Settings`) form a monoid where the rightmost setting
wins, and `Hasql.Connection.Settings.applicationName :: Text -> Settings` sets libpq's
`application_name`, which is how the test finds the listener's backend in `pg_stat_activity`.

### Cross-project conventions and ADRs

Locally, [docs/adr/queue-inspection-surface-boundary-and-wire-contract.md](../adr/queue-inspection-surface-boundary-and-wire-contract.md)
fixes the package boundary (the listener loop lives in the sister package, never in a core
library), the keiro-independence posture, and the rule that published wire shapes are frozen:
once a frame or field ships in a release it is never removed or re-typed. The WebSocket frame
convention is `mori://shinzui/keiro-ui/okf/adrs/concepts/ADR-2` (type-tagged frames, explicit
subscribe and unsubscribe, snapshot-then-delta, ping/pong, in-band `error` with overflow
signaling, `goodbye`, bounded queues) and the push-is-a-hint rule is
`mori://shinzui/keiro-ui/okf/adrs/concepts/ADR-3`. The conventions document itself is
`mori://shinzui/keiro-ui`, path `docs/architecture/inspection-api-conventions.md`
(artifact-level URI pending); its area 5 lists the structural shape every new surface keeps, and
area 6 states the rule that no endpoint may exist in push-only form. The JSON field policy is
`docs/design/020-json-wire-encodings.md` (plan 28): snake_case, `Maybe` as explicit `null`,
hand-written instances, `toEncoding` via `pairs` so key order is deterministic.


## Plan of Work

### Milestone 1: the protocol and the poll-driven feed

Scope: the frame types and their JSON, the per-connection machinery, the shared state with the
connection cap and shutdown flag, and the Server wiring, with no `LISTEN` connection yet. At the
end a client can subscribe, receive a `snapshot` with `"push":false`, receive poll-sourced
updates, ping, hit every error path, overflow a tiny outbox, and read `goodbye` on shutdown.
Acceptance: `WebSocketSpec`'s Milestone 1 group passes against ephemeral PostgreSQL, and the
golden files pin every frame.

Create `pgmq-inspect/src/Pgmq/Inspect/WebSocket.hs`. Its module Haddock is the protocol
reference for the package (plan 31 copies it into the user guide), so write it as such: list
every client and server frame with a JSON example, state the snapshot-then-update discipline,
the `push` flag's meaning, the `source` tag, the overflow rule, the server-side 30-second ping,
`goodbye`, and the poll-is-truth contract in plain words. The types:

```haskell
-- | A frame sent by the client. Tagged on @"type"@.
data ClientFrame
  = -- | @{"type":"subscribe","queue":"orders"}@ — start watching a queue.
    Subscribe !Text
  | -- | @{"type":"unsubscribe","queue":"orders"}@ — stop watching it.
    Unsubscribe !Text
  | -- | @{"type":"ping"}@ — answered with 'Pong'.
    Ping
  deriving stock (Eq, Show)

-- | Which driver produced an 'Update'.
data UpdateSource = NotifySource | PollSource
  deriving stock (Eq, Show)

-- | A frame sent by the server. Tagged on @"type"@. Constructors are positional so the
-- re-exporting umbrella never collides on record selectors.
data ServerFrame
  = -- | Sent once after a successful subscribe: the queue name, its metrics now, and whether
    -- insert notifications are being listened for (@"push"@).
    Snapshot !Text !QueueMetrics !Bool
  | -- | Fresh metrics for a subscribed queue and the driver that produced them.
    Update !Text !QueueMetrics !UpdateSource
  | -- | Answer to 'Ping'.
    Pong
  | -- | A per-connection fault: a machine-readable snake_case code, a human sentence, and the
    -- queue it concerns when there is one (@null@ otherwise).
    ErrorFrame !Text !Text !(Maybe Text)
  | -- | Sent before any server-initiated close.
    Goodbye
  deriving stock (Show)
```

Write the instances by hand following `docs/design/020-json-wire-encodings.md`. `FromJSON ClientFrame`
uses `withObject`, reads `"type"`, and fails on any other tag; also give `ClientFrame` a `ToJSON`
so tests and client authors can encode it. `ToJSON ServerFrame` defines both `toJSON` (with
`object`) and `toEncoding` (with `pairs`, in the order shown below), because aeson's `object`
builds a hash-ordered map and only `toEncoding` keeps key order deterministic for the golden
files. `UpdateSource` encodes as the strings `"notify"` and `"poll"`. The exact wire shapes:

```json
{"type":"subscribe","queue":"orders"}
{"type":"unsubscribe","queue":"orders"}
{"type":"ping"}
{"type":"snapshot","queue":"orders","metrics":{"queue_name":"orders","queue_length":3,"newest_msg_age_sec":1,"oldest_msg_age_sec":9,"total_messages":12,"scrape_time":"2026-09-30T12:00:00Z","queue_visible_length":2,"default_partition_length":null},"push":true}
{"type":"update","queue":"orders","metrics":{"queue_name":"orders","queue_length":4,"newest_msg_age_sec":0,"oldest_msg_age_sec":10,"total_messages":13,"scrape_time":"2026-09-30T12:00:01Z","queue_visible_length":3,"default_partition_length":null},"source":"notify"}
{"type":"pong"}
{"type":"error","code":"overflow","message":"The server dropped frames because this connection was not reading them; re-subscribe or re-read the queue over HTTP.","queue":null}
{"type":"goodbye"}
```

`"queue"` on an `error` frame is always present and is `null` when the fault is not about one
queue. The frame-level codes this module emits are `invalid_frame` (an undecodable client
frame), `too_many_subscriptions`, `overflow`, and, reusing plan 29's classifier, `queue_not_found`,
`database_unavailable`, and `database_error`. No `FromJSON ServerFrame` is written, because
`QueueMetrics` is encode-only by plan 28; tests decode server frames to `Data.Aeson.Value`.

Define the shared state, mirroring shibuya-metrics:

```haskell
data WebSocketState = WebSocketState
  { connectionCount :: !(TVar Int),
    maxConnections :: !Int,
    shutdownRequested :: !(TVar Bool)
  }

newWebSocketState :: Int -> IO WebSocketState
requestShutdown :: WebSocketState -> STM ()        -- sets shutdownRequested
activeConnections :: WebSocketState -> STM Int
```

Define the per-connection state. The outbox is the only path to the socket:

```haskell
data Outbox = Outbox
  { frames :: !(TBQueue ServerFrame),
    overflowed :: !(TVar Bool)
  }

-- | Drop-oldest enqueue. When the queue is full, the oldest frame is discarded and the
-- overflow flag is raised; the writer emits an @overflow@ error before the next frame it sends.
enqueueFrame :: Outbox -> ServerFrame -> STM ()
enqueueFrame Outbox {frames, overflowed} frame = do
  full <- isFullTBQueue frames
  when full $ do
    _ <- readTBQueue frames
    writeTVar overflowed True
  writeTBQueue frames frame

data Subscription = Subscription
  { subscriptionChannel :: !(Maybe (Text, SubscriberKey)),  -- Milestone 2: the LISTEN registration
    subscriptionFingerprint :: !MetricsFingerprint
  }

type MetricsFingerprint = (Text, Int64, Maybe Int32, Maybe Int32, Int64, Int64, Maybe Int64)

-- | Every field of 'QueueMetrics' except @scrape_time@, which changes on every read.
metricsFingerprint :: QueueMetrics -> MetricsFingerprint

data ConnectionState = ConnectionState
  { subscriptions :: !(TVar (Map Text Subscription)),
    hints :: !(TVar (Set Text)),   -- queues a notification said changed, awaiting debounce
    outbox :: !Outbox
  }
```

In Milestone 1 `SubscriberKey` can be a placeholder newtype over `()` in this module;
Milestone 2 moves it to `Pgmq.Inspect.Listener`. Field access follows the family convention:
`OverloadedLabels` with `generic-lens` (`st ^. #outbox`) or plain selectors, never
`OverloadedRecordDot`.

The server application and the five threads per connection:

```haskell
websocketApp :: InspectConfig -> InspectEnv -> Maybe Listener -> WebSocketState -> WS.ServerApp
websocketApp cfg env mListener wsState pending =
  mask $ \restore -> do
    outcome <- atomically (acquireConnection wsState)
    case outcome of
      AtCapacity -> restore (WS.rejectRequest pending "Too many connections")
      ShuttingDown -> restore (WS.rejectRequest pending "Server shutting down")
      Acquired ->
        restore (serveConnection cfg env mListener wsState pending `catch` normalPeerClosure)
          `finally` atomically (releaseConnection wsState)
```

`normalPeerClosure` swallows `WS.ConnectionClosed` and `WS.CloseRequest` and rethrows anything
else, exactly as shibuya-metrics does. `serveConnection` accepts the request, wraps everything in
`WS.withPingThread conn 30 (pure ())`, allocates a `ConnectionState` with an outbox of capacity
`wsQueueCapacity cfg`, and runs the reader, writer, poller, hint consumer, and shutdown watcher
under `race_` so that whichever ends first tears the others down; `finally` releases every
subscription's listener registration (a no-op in Milestone 1).

The **reader** loops on `WS.receiveData conn :: IO LBS.ByteString`, decodes with
`eitherDecode'`, enqueues `ErrorFrame "invalid_frame" "…" Nothing` on failure, and otherwise
dispatches. `Ping` enqueues `Pong`. `Unsubscribe q` removes `q` from the map and, when the
subscription held a channel registration, unregisters it (Milestone 2). `Subscribe q` first
checks the cap: if the map already holds `wsMaxSubscriptions cfg` entries and `q` is not among
them, enqueue `ErrorFrame "too_many_subscriptions" "…" (Just q)` and stop. Otherwise run one
inspection that resolves the queue row through the lenient listing and reads the metrics only
when the row exists:

```haskell
lookupQueue :: Text -> Inspection (Maybe (UnvalidatedQueue, QueueMetrics))
lookupQueue q = do
  rows <- Eff.listQueuesUnvalidated
  case find ((== q) . unvalidatedName) rows of
    Nothing -> pure Nothing
    Just row -> Just . (row,) <$> Eff.queueMetricsUnvalidated q
```

`Right Nothing` enqueues `ErrorFrame "queue_not_found" ("No queue named " <> q <> " exists.") (Just q)`
and subscribes nothing. `Left err` enqueues an error frame whose code and message come from plan
29's classifier (so a transient outage is `database_unavailable`). `Right (Just (row, metrics))`
inserts the subscription (re-subscribing an already-watched queue keeps its existing
registration and simply re-sends a snapshot) and enqueues `Snapshot q metrics push`, where
`push` is computed by the Milestone 2 registration step and is `False` throughout Milestone 1.

The **writer** is the only thread that touches the socket after the accept:

```haskell
writerLoop :: Outbox -> WS.Connection -> IO ()
writerLoop ob conn = loop
  where
    loop = do
      (frame, dropped) <- atomically $ (,) <$> readTBQueue (frames ob) <*> swapTVar (overflowed ob) False
      when dropped $ sendFrame conn (ErrorFrame "overflow" overflowMessage Nothing)
      sendFrame conn frame
      case frame of
        Goodbye -> WS.sendClose conn ("server shutting down" :: Text)
        _ -> loop
```

`sendFrame conn = WS.sendTextData conn . encode`, catching `WS.ConnectionException` so a dead
socket never turns cleanup into a second failure. Sending `Goodbye` ends the writer and therefore
the `race_`, which closes the connection.

The **poller** sleeps `pollIntervalUs cfg`, then refreshes every subscribed queue with
`PollSource`. `refreshQueue` is shared with the hint consumer:

```haskell
refreshQueue :: InspectConfig -> InspectEnv -> Maybe Listener -> ConnectionState -> UpdateSource -> Text -> IO ()
```

It runs `queueMetricsUnvalidated q`; on `Right m` it compares `metricsFingerprint m` with the
stored fingerprint and enqueues `Update q m source` when they differ or when `source` is
`NotifySource`, storing the new fingerprint either way; on a `42P01` (the queue was dropped) it
enqueues `queue_not_found` for that queue and removes the subscription (unregistering the
channel); on any other `Left` it enqueues the classified error frame and keeps the
subscription. The poller enqueues at most one database error frame per cycle, not one per
queue.

The **hint consumer** is Milestone 2's notification path but is written now so the thread
structure is final:

```haskell
hintLoop cfg env mListener st = forever $ do
  q <- atomically $ readTVar (hints st) >>= maybe retry pure . Set.lookupMin
  threadDelay (notifyDebounceUs cfg)
  atomically $ modifyTVar' (hints st) (Set.delete q)
  stillWatched <- Map.member q <$> readTVarIO (subscriptions st)
  when stillWatched $ refreshQueue cfg env mListener st NotifySource q
```

The queue name stays in the set during the debounce sleep, so a burst of notifications for one
queue collapses into one metrics read; a notification arriving after the delete schedules the
next cycle. In Milestone 1 nothing writes to `hints`.

The **shutdown watcher** blocks on `shutdownRequested` becoming `True`, enqueues `Goodbye`, and
then blocks forever (`atomically retry` behind a never-set flag, or `forever (threadDelay maxBound)`);
the writer's `Goodbye` handling ends the race.

Now wire the Server. Add to `Pgmq.Inspect.Server`:

```haskell
data InspectRuntime = InspectRuntime
  { runtimeApplication :: !Application,
    -- | Ask every WebSocket connection to send @goodbye@ and close; HTTP keeps serving.
    runtimeRequestShutdown :: !(IO ()),
    -- | Wait until no WebSocket connection remains, for at most the given microseconds.
    runtimeAwaitDrained :: !(Int -> IO ()),
    -- | 'False' when no listener is configured or the LISTEN connection is down (Milestone 2).
    runtimeListenerHealthy :: !(STM Bool)
  }

withInspectRuntime :: InspectConfig -> InspectEnv -> (InspectRuntime -> IO a) -> IO a
withInspectRuntime cfg env k = do
  wsState <- newWebSocketState (wsMaxConnections cfg)
  withOptionalListener (listenerSettings env) $ \mListener ->        -- Milestone 2; Milestone 1 passes Nothing
    withInspectApplicationWith cfg env (websocketApp cfg env mListener wsState) $ \app ->
      k (runtime app wsState mListener)
        `finally` (atomically (requestShutdown wsState) >> awaitDrained wsState 1_000_000)

withInspectApplication :: InspectConfig -> InspectEnv -> (Application -> IO a) -> IO a
withInspectApplication cfg env k = withInspectRuntime cfg env (k . runtimeApplication)
```

`awaitDrained` is `registerDelay` composed through `orElse` with `activeConnections == 0`.
Rewrite plan 29's `withInspectServer` on top of `withInspectRuntime` so its bracket release
runs `runtimeRequestShutdown`, then `runtimeAwaitDrained 1_000_000`, then cancels the Warp
thread; `runInspectServer` follows. The plan 29 signatures of `withInspectApplication`,
`withInspectServer`, and `runInspectServer` do not change. Delete the rejecting seam app if
nothing else uses it, or keep it exported for hosts that want the HTTP surface alone; either way
record the choice in the Decision Log.

Fallback if `queueMetricsUnvalidated` does not exist: add
`queueMetricsUnvalidated :: Statement Text QueueMetrics` to
`pgmq-hasql/src/Pgmq/Hasql/Statements/QueueObservability.hs` with the same SQL as
`queueMetrics` and `E.param (E.nonNullable E.text)` as encoder, a session of the same name in
`Pgmq.Hasql.Sessions`, an export from `Pgmq`, a constructor
`QueueMetricsUnvalidated :: Text -> Pgmq m QueueMetrics` in `Pgmq.Effectful.Effect` with the
smart constructor, a case in `runPgmq`, and a case in `runPgmqTracedWith` labelled
`pgmq.metrics` with `OTel.Internal` kind and the text as destination; extend the umbrella
witnesses and changelogs of both packages, and record it in the Decision Log as scope this plan
absorbed.

Tests for Milestone 1 live in `pgmq-inspect/test/WebSocketSpec.hs` (registered in
`pgmq-inspect/test/Main.hs` and the cabal `other-modules`). The golden group encodes one value of
each frame with `Data.Aeson.encode` and compares with `Test.Tasty.Golden.goldenVsString` against
`pgmq-inspect/test/golden/ws-<frame>.json` (`ws-subscribe`, `ws-unsubscribe`, `ws-ping`,
`ws-snapshot`, `ws-update-notify`, `ws-update-poll`, `ws-pong`, `ws-error`, `ws-goodbye`), using
the sample metrics shown above with a fixed `scrape_time`. The behavioural group uses the
`websockets` client. Extend `pgmq-inspect/test/TestServer.hs` with
`withTestRuntime :: InspectConfig -> InspectEnv -> (Int -> InspectRuntime -> IO a) -> IO a`,
which calls `withInspectRuntime`, binds the application with `Warp.openFreePort` and
`Warp.runSettingsSocket` in an `async`, and on exit requests shutdown, drains for one second, and
cancels Warp. Test helpers:

```haskell
withWs :: Int -> (WS.Connection -> IO a) -> IO a
withWs port = WS.runClient "127.0.0.1" port "/ws"

sendClient :: WS.Connection -> ClientFrame -> IO ()
sendClient conn = WS.sendTextData conn . encode

-- | Wait up to the given microseconds for a frame satisfying the predicate, discarding others.
awaitFrame :: Int -> WS.Connection -> (Value -> Bool) -> IO (Maybe Value)

frameType :: Value -> Maybe Text          -- the "type" field
frameField :: Text -> Value -> Maybe Value
```

The Milestone 1 cases, each on its own `withTestRuntime` with an env built by
`poolEnv pool Nothing` (no listener) and a config derived from `defaultInspectConfig` with
`pollIntervalUs = 200_000`:

- subscribe to a freshly created queue → the first frame is `snapshot` with `"queue"` equal to
  the name, `"push": false`, and `"metrics"."queue_length": 0`;
- send three messages through the pool → within three poll intervals (600 ms, allow 2 s) an
  `update` with `"source":"poll"` and `"metrics"."queue_length": 3` arrives; with nothing else
  happening, no further `update` arrives within one second (poll stays quiet when nothing moved);
- `ping` → `pong`;
- subscribe to a name no queue has → `error` with `"code":"queue_not_found"` and `"queue"` set;
  no snapshot follows;
- send the text `not json` → `error` with `"code":"invalid_frame"`;
- with `wsMaxSubscriptions = 1`, subscribe to two different queues → the second answer is
  `error` `too_many_subscriptions`;
- with `wsQueueCapacity = 2` and `pollIntervalUs = 50_000`: subscribe, then without reading from
  the socket send fifty messages one at a time with 60 ms pauses so many poll updates are
  generated, then resume reading → an `error` with `"code":"overflow"` is observed, and after it
  at least one `update` still arrives (the connection survives the overflow);
- `goodbye` on shutdown: run `withTestRuntime` in an `async` that parks on an `MVar` after
  handing out the port; connect and subscribe; fill the `MVar`; the client's next frames are
  `goodbye` and then a `WS.CloseRequest` exception from `receiveData`.

### Milestone 2: the LISTEN connection and push acceleration

Scope: the server-wide listener, channel registration from subscribe, notify-sourced updates,
reconnection, and the backend-kill test. At the end `snapshot` says `"push":true` for a
validated, non-partitioned queue when a listener is configured, an insert produces an `update`
with `"source":"notify"` within two seconds, and killing the listener's backend neither wedges
the feed nor permanently disables pushes. Acceptance: `WebSocketSpec`'s Milestone 2 group
passes, including the `pg_terminate_backend` sequence that is `IR-3` acceptance item 4.

Create `pgmq-inspect/src/Pgmq/Inspect/Listener.hs`:

```haskell
module Pgmq.Inspect.Listener
  ( Listener,
    withListener,
    SubscriberKey,
    subscribeChannel,
    unsubscribeChannel,
    listenerHealthy,
  )
where

newtype SubscriberKey = SubscriberKey Unique   -- Data.Unique; Eq and Ord
  deriving newtype (Eq, Ord)

data Listener = Listener
  { -- | channel -> the wake actions of every subscriber of that channel
    registry :: !(TVar (Map Text (Map SubscriberKey (STM ())))),
    -- | raised when the registry's key set changed, so the loop re-syncs LISTENs
    registryChanged :: !(TVar Bool),
    healthy :: !(TVar Bool)
  }

withListener :: Settings -> (Listener -> IO a) -> IO a
subscribeChannel :: Listener -> Text -> STM () -> IO SubscriberKey
unsubscribeChannel :: Listener -> Text -> SubscriberKey -> IO ()
listenerHealthy :: Listener -> STM Bool
```

`subscribeChannel` makes a `Unique`, inserts the wake action under the channel, and raises
`registryChanged` only if the channel had no subscribers before. `unsubscribeChannel` removes the
key and raises the flag only if the channel is now empty (and deletes the empty entry). The
registry is the single source of truth for which channels must be listened on, which is what
makes reconnection total.

`withListener settings k` allocates the three `TVar`s and runs `listenerLoop settings l` under
`withAsync`, so leaving the scope cancels the loop:

```haskell
listenerLoop :: Settings -> Listener -> IO ()
listenerLoop settings l = go initialBackoffUs
  where
    initialBackoffUs = 100_000
    maxBackoffUs = 5_000_000
    go backoff = do
      acquired <- Connection.acquire (settings <> Settings.applicationName "pgmq-inspect-listener")
      case acquired of
        Left _ -> threadDelay backoff >> go (min maxBackoffUs (backoff * 2))
        Right conn -> do
          becameHealthy <- (serve conn `finally` Connection.release conn) `catchSync` \_ -> pure False
          atomically (writeTVar (healthy l) False)
          let next = if becameHealthy then initialBackoffUs else min maxBackoffUs (backoff * 2)
          threadDelay next
          go next
```

`catchSync` catches `SomeException` but rethrows anything whose `fromException` is a
`SomeAsyncException`, so cancellation still cancels. Appending `applicationName` last makes it
win over any `application_name` the host's settings carried (rightmost wins), which is what the
test relies on.

`serve conn` first drains any notification libpq already buffered, then syncs LISTENs from the
registry, marks healthy, and loops on slices:

```haskell
serve :: Connection -> IO Bool
serve conn = do
  synced <- syncListens conn Set.empty
  case synced of
    Nothing -> pure False
    Just listening -> do
      atomically (writeTVar (healthy l) True)
      let loop live = do
            outcome <- Connection.use conn (Session.onLibpqConnection (slice l))
            case outcome of
              Right (Right SocketReadable) -> loop live
              Right (Right RegistryChanged) -> syncListens conn live >>= maybe (pure True) loop
              _ -> pure True          -- a fault after we were healthy: reconnect with a fresh backoff
      loop listening
```

`syncListens conn live` reads the registry's key set, runs
`Connection.use conn (Session.script ("LISTEN " <> quoteIdentifier c))` for every channel in the
registry but not in `live` and `UNLISTEN` for every channel in `live` but not in the registry,
returning the new live set, or `Nothing` on the first `Left`. `quoteIdentifier` double-quotes and
doubles embedded quotes. The slice:

```haskell
data Wake = SocketReadable | RegistryChanged
data Fault = SocketLost | InputFailed

slice :: Listener -> Pq.Connection -> IO (Either SessionError (Either Fault Wake), Pq.Connection)
slice l pq = do
  drainNotifies l pq                       -- notifications buffered by an earlier LISTEN round-trip
  mfd <- Pq.socket pq
  case mfd of
    Nothing -> pure (Right (Left SocketLost), pq)
    Just fd -> do
      (ready, cancelWait) <- threadWaitReadSTM fd
      wake <- atomically $
        (ready >> pure SocketReadable)
          `orElse` (readTVar (registryChanged l) >>= check >> writeTVar (registryChanged l) False >> pure RegistryChanged)
      cancelWait
      case wake of
        RegistryChanged -> pure (Right (Right RegistryChanged), pq)
        SocketReadable -> do
          ok <- Pq.consumeInput pq
          status <- Pq.status pq
          if not ok || status /= Pq.ConnectionOk
            then pure (Right (Left InputFailed), pq)
            else drainNotifies l pq >> pure (Right (Right SocketReadable), pq)

drainNotifies :: Listener -> Pq.Connection -> IO ()
drainNotifies l pq = Pq.notifies pq >>= \case
  Nothing -> pure ()
  Just n -> do
    let channel = decodeUtf8Lenient (Pq.notifyRelname n)
    atomically $ readTVar (registry l) >>= maybe (pure ()) (sequence_ . Map.elems) . Map.lookup channel
    drainNotifies l pq
```

`threadWaitReadSTM` (from `Control.Concurrent`, base) returns an STM action that completes when
the socket is readable and an IO action that cancels the wait; composing the STM action with the
registry flag through `orElse` is what lets one connection both deliver notifications and pick
up new `LISTEN`s without a second thread. It needs the threaded runtime; the test suite and plan
31's executable already build with `-threaded`. `Pq.consumeInput` returning `False`, or a status
other than `ConnectionOk`, is how a terminated backend or a dropped socket shows up: the
socket becomes readable at EOF, input fails, the slice returns a fault, `serve` returns, the
connection is released, and the loop reconnects after the backoff and re-`LISTEN`s everything
in the registry. Nothing is logged (the package has no logger); `listenerHealthy` is the
observable.

Wire the listener into the feed. In `Pgmq.Inspect.Server.withInspectRuntime`,
`withOptionalListener Nothing k = k Nothing` and
`withOptionalListener (Just settings) k = withListener settings (k . Just)`;
`runtimeListenerHealthy` is `maybe (pure False) listenerHealthy mListener`. In
`Pgmq.Inspect.WebSocket`, move `SubscriberKey` to the listener module and complete the
subscribe path: after `lookupQueue` succeeds, compute the registration

```haskell
registerPush :: Maybe Listener -> ConnectionState -> Text -> UnvalidatedQueue -> IO (Maybe (Text, SubscriberKey))
registerPush mListener st q row =
  case (mListener, parseQueueName q, unvalidatedIsPartitioned row) of
    (Just listener, Right qn, False) -> do
      let channel = notifyChannelName qn
      key <- subscribeChannel listener channel (modifyTVar' (hints st) (Set.insert q))
      pure (Just (channel, key))
    _ -> pure Nothing
```

and set `push = isJust registration`. The wake action inserts the queue name into the
connection's `hints` set, which the hint consumer from Milestone 1 turns into one debounced
metrics read and an `update` with `"source":"notify"`. `Unsubscribe`, the `42P01` path in
`refreshQueue`, and the connection's `finally` all call `unsubscribeChannel` for registrations
they drop. Because the registration is per connection and per queue, a hundred tabs watching one
queue share one `LISTEN` and each gets its own debounced update.

Tests for Milestone 2, in the same spec, with `poolEnv pool (Just (EphemeralPg.connectionSettings db))`
and a queue created through the pool with `enableNotifyInsert (EnableNotifyInsert q (Just 0))`
(`Just 0` disables the throttle so every insert notifies; the default `Nothing` means 250 ms):

- subscribe → `snapshot` with `"push":true`;
- send one message → within two seconds an `update` with `"source":"notify"` and
  `"queue_length": 1`;
- with `pollIntervalUs = 10_000_000` (so the poll cannot be what delivered it) send five
  messages in a tight loop → within two seconds at least one notify-sourced `update` arrives
  and its `queue_length` is `5` on the last of them (the debounce coalesced the burst; assert
  that fewer than five notify-sourced updates arrived within the following second);
- a queue created without `enableNotifyInsert` → `"push":true` (the registration does not know
  whether the trigger exists) and updates arrive from the poll, which documents that `push`
  means "listening", not "the queue notifies";
- a name `parseQueueName` rejects, created through a raw `select pgmq.create('Mixed_Case')`
  statement on a dedicated database (follow `pgmq-hasql/test/AliasingSpec.hs`'s pattern, because
  a mixed-case `pgmq.meta` row poisons `listQueues` for every concurrent test) → `snapshot` with
  `"push":false`, then a poll-sourced `update` after a raw insert;
- the backend kill, `IR-3` acceptance 4: with `pollIntervalUs = 500_000`, subscribe (`push:true`),
  wait until `runtimeListenerHealthy` reads `True`, then run through the pool

  ```sql
  select count(pg_terminate_backend(pid))::int
  from pg_stat_activity
  where application_name = 'pgmq-inspect-listener'
  ```

  and assert the count is at least one; send a message immediately; within two seconds an
  `update` arrives (its source may be `poll` or, if the reconnect won the race, `notify`; the
  assertion is that one arrives at all, which is the no-wedge guarantee); then poll
  `runtimeListenerHealthy` with `registerDelay` until it is `True` again (allow ten seconds);
  send another message; within two seconds an `update` with `"source":"notify"` arrives, which
  proves re-`LISTEN` after reconnection;
- optionally, when `PGMQ_REQUIRE_PARTMAN=1`, a partitioned queue → `"push":false`.

Add `postgresql-libpq >=0.10.1 && <0.12` to the library's `build-depends` (it is already a test
dependency elsewhere in the family); `stm`, `async`, `containers`, and `text` are needed too if
plan 29 did not add them.

### Milestone 3: documentation, capability evidence, changelogs

Scope: make the protocol findable and freeze it. Acceptance: `just docs-check` passes, the
design note's WebSocket section exists with examples, and the changelogs carry `Unreleased`
entries.

Finish the Haddock of `Pgmq.Inspect.WebSocket` (it is the protocol reference) and of
`Pgmq.Inspect.Listener` (why one connection, why `threadWaitReadSTM`, reconnect behaviour, the
`application_name`). Export the two modules and `InspectRuntime`/`withInspectRuntime` from the
umbrella `Pgmq.Inspect`.

In `docs/design/021-inspection-surface-wire-contract.md` (created by plan 29; if it has no
WebSocket heading yet, add `## The WebSocket live feed`), write: the endpoint `/ws`; every client
and server frame with the JSON examples above; the subscribe-then-snapshot discipline and that a
connection watches nothing until it subscribes; `push` and its four conditions (a listener is
configured, the name validates, the queue is not partitioned, and the trigger exists on the
server side, which the server does not check); the `source` tag; the debounce; the poll
interval and its cost; the overflow rule (bounded per-connection queue, drop-oldest, in-band
`error` with code `overflow` before the next frame, after which the client should re-read over
HTTP); the server-side 30-second ping; `goodbye` and the one-second drain; the listener's
reconnect with backoff and re-`LISTEN`; and, in its own paragraph, the poll-is-truth contract in
the words of design note 015 and `mori://shinzui/keiro-ui/okf/adrs/concepts/ADR-3`. End with the
freeze rule: these frames and fields are a published compatibility surface once released.

Extend the capability record plan 29 created for the HTTP surface (find it with
`grep -l pgmq-inspect docs/capabilities/*.md`): add the feed to the "What it provides" list, add
an evidence entry for `pgmq-inspect/test/WebSocketSpec.hs` stating what it proves (snapshot
then updates, notify acceleration, poll convergence after a backend kill, overflow signaling,
goodbye), add a Limits bullet that the feed carries metrics only and that partitioned and
foreign queues are poll-only, and append a log entry with `okf log add docs/capabilities --kind Update -m "…"`.
Run `just docs-check`.

Append to `pgmq-inspect/CHANGELOG.md` under `Unreleased` a paragraph describing the feed, the
listener, the configuration fields it consumes, and `withInspectRuntime`; append a sentence to the
root `CHANGELOG.md`'s `Unreleased` section. No version changes.


## Concrete Steps

All commands run from the repository root
`/Users/shinzui/Keikaku/bokuno/libraries/pgmq-hs-project/pgmq-hs`.

Verify the prerequisites with the grep block in Context and Orientation; every grep must print at
least one line, except the `queueMetricsUnvalidated` one, whose absence triggers the fallback in
Milestone 1.

Milestone 1. Create the module and tests, then build and run:

```bash
nix develop --command cabal build pgmq-inspect
nix develop --command cabal test pgmq-inspect:pgmq-inspect-test --test-show-details=direct --test-options='-p WebSocket'
```

Expected tail of the test output (names illustrative, the counts must be all-pass):

```text
pgmq-inspect
  WebSocket
    frame encodings
      ws-subscribe:        OK
      ws-snapshot:         OK
      ws-goodbye:          OK
    poll-only feed
      subscribe answers with a snapshot and push:false:   OK (0.41s)
      poll-sourced update after sends, quiet otherwise:   OK (1.62s)
      ping answers pong:                                  OK
      unknown queue answers queue_not_found:              OK
      undecodable frame answers invalid_frame:            OK
      subscription cap answers too_many_subscriptions:    OK
      overflow is signaled in band and the feed survives: OK (3.90s)
      goodbye precedes a server-initiated close:          OK (1.05s)

All 11 tests passed (8.37s)
```

The first run of the golden group creates the golden files when they are absent only if you
pass `--accept`; prefer writing them by hand from the examples in Milestone 1 so a wrong
encoding is caught rather than enshrined. Commit after `nix fmt`:

```text
feat(inspect): add the WebSocket frame protocol and the poll-driven live feed

Add Pgmq.Inspect.WebSocket with type-tagged subscribe/unsubscribe/ping frames
and snapshot/update/pong/error/goodbye answers, a bounded per-connection
outbox with drop-oldest overflow signaled in band, a poll loop that pushes
metrics when they change, and withInspectRuntime so a host can request a
graceful goodbye before closing. No LISTEN connection yet: every snapshot
reports push:false.

MasterPlan: docs/masterplans/7-expose-non-destructive-queue-inspection-through-typed-reads-json-codecs-and-the-pgmq-inspect-sister-package.md
ExecPlan: docs/plans/30-add-the-notify-accelerated-poll-authoritative-websocket-live-feed-to-pgmq-inspect.md
Intention: intention_01m3tcw9vmeeftdtqj53d1d6nb
```

Milestone 2. Create the listener, wire it, add the tests, run the same command. Expected
additions to the output:

```text
    listener-accelerated feed
      subscribe answers push:true for a validated queue:          OK (0.52s)
      an insert yields a notify-sourced update within 2s:         OK (0.71s)
      a burst coalesces into few notify-sourced updates:          OK (1.80s)
      a queue without the trigger converges through the poll:     OK (1.21s)
      a foreign name is poll-only:                                OK (2.03s)
      killing the LISTEN backend neither wedges nor disables push: OK (4.47s)
```

To watch the listener's backend come and go while a test runs, open a psql against the
ephemeral cluster (its port is printed by ephemeral-pg under `/tmp/ephpg-pgmq-hs-<uid>`) and run:

```sql
select pid, state, backend_start from pg_stat_activity where application_name = 'pgmq-inspect-listener';
```

A new `backend_start` after the kill is the reconnect. Commit:

```text
feat(inspect): accelerate the live feed with a reconnecting LISTEN connection

One server-wide LISTEN connection, acquired from the host's listener settings
with application_name pgmq-inspect-listener, carries a refcounted channel
registry, waits on the socket with threadWaitReadSTM inside
onLibpqConnection, debounces notifications per queue, and reconnects with
exponential backoff re-issuing every LISTEN. Snapshots report push:true for
validated, non-partitioned queues; the poll stays authoritative throughout.

MasterPlan: docs/masterplans/7-expose-non-destructive-queue-inspection-through-typed-reads-json-codecs-and-the-pgmq-inspect-sister-package.md
ExecPlan: docs/plans/30-add-the-notify-accelerated-poll-authoritative-websocket-live-feed-to-pgmq-inspect.md
Intention: intention_01m3tcw9vmeeftdtqj53d1d6nb
```

Milestone 3. Write the documents, then:

```bash
nix develop --command cabal haddock pgmq-inspect
just docs-check
nix fmt
```

`just docs-check` must end without a validation error for `docs/capabilities`. Commit:

```text
docs(inspect): document the WebSocket live feed contract

State the frame protocol, the push flag's four conditions, the debounce and
poll intervals, overflow signaling, goodbye, listener reconnection, and the
poll-is-truth rule in design note 021; record the feed as capability evidence;
add the Unreleased changelog entries.

MasterPlan: docs/masterplans/7-expose-non-destructive-queue-inspection-through-typed-reads-json-codecs-and-the-pgmq-inspect-sister-package.md
ExecPlan: docs/plans/30-add-the-notify-accelerated-poll-authoritative-websocket-live-feed-to-pgmq-inspect.md
Intention: intention_01m3tcw9vmeeftdtqj53d1d6nb
```

Finally run the whole family once to prove nothing else moved:

```bash
nix develop --command cabal test all
nix flake check
```


## Validation and Acceptance

Acceptance is behaviour a person can observe with a WebSocket client; the test suite automates
it, and plan 31's executable lets you repeat it by hand. With the executable (or
`withInspectServer`) running against a database holding a queue `orders` with insert
notifications enabled, a client such as `websocat ws://127.0.0.1:9092/ws` that sends
`{"type":"subscribe","queue":"orders"}` receives, in order, a `snapshot` frame with
`"push":true`, and after `select pgmq.send('orders', '{"hello":1}')` in psql, within about a
hundred milliseconds an `update` frame with `"source":"notify"` and `queue_length` one higher.
Sending `{"type":"ping"}` returns `{"type":"pong"}`. Stopping the server delivers
`{"type":"goodbye"}` before the socket closes.

The mapping from `IR-3`'s acceptance item 4 to tests: "receives a snapshot, then insert
notifications as they occur" is the `push:true` and notify-sourced-update cases; "killing the
LISTEN connection does not wedge the feed — the authoritative poll path converges" is the first
half of the backend-kill case plus the poll-only cases; "reconnection resumes pushes" is the
second half of the backend-kill case.

Non-functional checks: `wsMaxConnections` is enforced (open `wsMaxConnections + 1` clients; the
last upgrade is rejected with a 4xx handshake), one `LISTEN` backend exists in
`pg_stat_activity` no matter how many clients subscribe to how many queues, and `UNLISTEN` is
issued when the last subscriber of a channel leaves (observe with `select * from pg_listening_channels()`
on the listener's own session is not possible from outside; instead assert the registry is empty
through a test-only export or accept the refcount logic's unit test). `nix fmt` leaves the tree
unchanged, `cabal test all` and `nix flake check` pass, and `just docs-check` validates.


## Idempotence and Recovery

Every step is additive and can be re-run. Re-running the test suite is safe: each case creates
its own queues under random names on the shared ephemeral cluster, and the foreign-name case uses
a dedicated cluster it tears down. If a test run is killed, the next run reaps the abandoned
cluster from `/tmp/ephpg-pgmq-hs-<uid>`.

If the listener loop misbehaves in development (for example spins reconnecting), set
`listenerSettings = Nothing` in the environment to run the feed poll-only while you debug; every
Milestone 1 test still passes in that mode, which is the deliberate degradation path.

If a golden file is wrong, delete it and regenerate with `--accept` only after confirming the new
bytes by eye against the examples in Milestone 1; golden files are a published shape once
released, so a change after release is a breaking change and must not be accepted casually.

`withInspectRuntime`'s release phase requests shutdown and drains for at most one second, then
cancels Warp; a client that ignores `goodbye` is closed by the cancellation, never blocks
shutdown.

Commits are per milestone; if a milestone fails midway, the previous commit leaves the package
building and its tests green, because the seam stays rejecting until the real app is wired.


## Interfaces and Dependencies

Libraries: `websockets ^>=0.13` (server and, in tests, client), `wai-websockets ^>=3.0`
(already used by plan 29's `["ws"]` route), `postgresql-libpq >=0.10.1 && <0.12` (the raw
connection inside `onLibpqConnection`), `hasql` (`Hasql.Connection`, `Hasql.Connection.Settings`,
`Hasql.Session.onLibpqConnection`, `Hasql.Session.script`), `stm >=2.5 && <2.6`,
`async ^>=2.2`, `containers`, `text`, `aeson ^>=2.2`, `bytestring`, `base` (`Control.Concurrent.threadWaitReadSTM`,
`Data.Unique`), `pgmq-core`, `pgmq-hasql`, `pgmq-effectful` (`>=0.6 && <0.7`). Test suite
additions: `tasty-golden ^>=2.3`, `warp` (free-port binding), `websockets` (client), `stm`,
`async`, `aeson`, `hasql`, `hasql-pool`, `ephemeral-pg`, `postgresql-libpq` if the foreign-name
case issues raw statements through libpq rather than hasql.

At the end of Milestone 1, `Pgmq.Inspect.WebSocket` exports `ClientFrame (..)`,
`ServerFrame (..)`, `UpdateSource (..)`, `WebSocketState`, `newWebSocketState :: Int -> IO WebSocketState`,
`requestShutdown :: WebSocketState -> STM ()`, `activeConnections :: WebSocketState -> STM Int`,
and `websocketApp :: InspectConfig -> InspectEnv -> Maybe Listener -> WebSocketState -> WS.ServerApp`
(with `Listener` a placeholder type the module owns until Milestone 2). `Pgmq.Inspect.Server`
exports `InspectRuntime (..)` and `withInspectRuntime :: InspectConfig -> InspectEnv -> (InspectRuntime -> IO a) -> IO a`,
and `withInspectApplication`, `withInspectServer`, and `runInspectServer` keep plan 29's
signatures. `pgmq-inspect/test/TestServer.hs` exports
`withTestRuntime :: InspectConfig -> InspectEnv -> (Int -> InspectRuntime -> IO a) -> IO a`.

At the end of Milestone 2, `Pgmq.Inspect.Listener` exports `Listener`,
`withListener :: Hasql.Connection.Settings.Settings -> (Listener -> IO a) -> IO a`,
`SubscriberKey`, `subscribeChannel :: Listener -> Text -> STM () -> IO SubscriberKey`,
`unsubscribeChannel :: Listener -> Text -> SubscriberKey -> IO ()`, and
`listenerHealthy :: Listener -> STM Bool`; `websocketApp` takes the real `Maybe Listener`;
`InspectRuntime.runtimeListenerHealthy` reflects it. The listener connection carries
`application_name = pgmq-inspect-listener`.

At the end of Milestone 3, `Pgmq.Inspect` re-exports both modules and `withInspectRuntime`,
`docs/design/021-inspection-surface-wire-contract.md` has the WebSocket section, the capability
record lists `pgmq-inspect/test/WebSocketSpec.hs` as evidence, and both changelogs carry
`Unreleased` entries. Nothing in `pgmq-core`, `pgmq-hasql`, or `pgmq-effectful` changes unless
the `queueMetricsUnvalidated` fallback was needed, in which case the Decision Log says so and
those packages' changelogs gain an entry.
