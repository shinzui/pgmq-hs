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
  reviews:
    - model: "gpt-6-astra"
      harness: "codex-cli"
      at: 2026-10-19T18:23:59Z
      verdict: "changes-requested"
      note: "Replaced per-client scans with a shared per-queue coordinator and bounded lifecycle; tightened snapshot ordering, frame limits and listener cleanup."
  revisions:
    - model: "gpt-6-astra"
      harness: "codex-cli"
      at: 2026-10-19T18:23:59Z
      mode: "update"
      note: "Replaced per-client scans with a shared per-queue coordinator and bounded lifecycle; tightened snapshot ordering, frame limits and listener cleanup."
    - model: "gpt-6.1-sol"
      harness: "codex-cli"
      at: 2026-10-20T04:38:28Z
      mode: "implement"
      note: "Implement shared refresh scheduling, bounded WebSocket transport and reconnecting listener"
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

- [x] Review acceptance: query count scales with distinct queues, one in-flight refresh per queue, bounded admission and fair scheduling, atomic snapshots/generations, bounded frames, deterministic overflow and cancellation cleanup.

- [x] M1: `Pgmq.Inspect.WebSocket` exists with the frame types, hand-written JSON instances, and golden files for every frame under `pgmq-inspect/test/golden/`
- [x] M1: shared refresh coordinator plus per-connection bounded outbox, reader, writer and shutdown watcher and the shared `WebSocketState` with the connection cap
- [x] M1: `Pgmq.Inspect.Server` gains `withInspectRuntime`; `withInspectApplication` keeps its signature and delegates; `withInspectServer` signals shutdown and drains before cancelling Warp
- [x] M1: `WebSocketSpec` green with no listener configured: `snapshot` with `"push":false`, poll-sourced `update`, `pong`, `queue_not_found`, `invalid_frame`, `too_many_subscriptions`, `too_many_queues`, `frame_too_large`, `overflow`, `goodbye`
- [x] M2: `Pgmq.Inspect.Listener` exists: one server-wide `LISTEN` connection, refcounted channel registry, `threadWaitReadSTM` wait loop over `onLibpqConnection`, reconnect with exponential backoff and re-`LISTEN`, `listenerHealthy`
- [x] M2: subscribe registers validated, non-partitioned queues for push; `snapshot` says `"push":true`; notify-sourced updates arrive within two seconds; debounce coalesces bursts
- [x] M2: the backend-kill test proves the feed does not wedge and that pushes resume after reconnection
- [x] M3: Haddock on `Pgmq.Inspect.WebSocket` is the protocol reference; the WebSocket section of `docs/design/021-inspection-surface-wire-contract.md` is written
- [x] M3: the capability record plan 29 created lists the feed and `WebSocketSpec` as evidence; `pgmq-inspect/CHANGELOG.md` and root `CHANGELOG.md` carry `Unreleased` entries; `just docs-check` passes


## Surprises & Discoveries

M2 (2026-10-20 UTC): all 106 tests pass natively and on stock 1.12; twenty full
stock reruns pass (2,120 tests). One server-wide listener owns a refcounted channel
registry, 64-notification slices, exception-safe fd waits and reconnect backoff.
Fault tests identify a replacement backend PID before waiting for its LISTEN ack;
connection health alone can still describe the previous session. The runtime adds
runtimeListenerAcknowledged as a read-only host/test observable; it is not a frame
field or a change to push semantics. Raw frame tests reject a 1 GiB declared
payload before its body, and reject an oversized assembled fragmented message.
A deliberately uncancellable driver slice holds one real inspection connection;
shared admission rejects excess work, actual worker-pool SQL progresses, and
release allows all owned tasks to join. This models IR-6, not a hard network fix.

Repeated stock runs exposed why probing a freed ephemeral port is not a socket
ownership test: address-in-use occurred with no live owner reported by lsof.
A bounded rebind retry was insufficient. The public runner now composes private
Lifecycle socket/thread brackets; deterministic tests inspect the actual closed
Socket descriptor (-1), join the thread and cancel at an after-bind barrier.
No public runner signature changed. A due poll and hint share one read, reporting
notify when the hint is ready while also advancing the due poll deadline.

Retained workload evidence is docs/design/evidence/021-inspection-feed/README.md.
With 100 real subscribers, 50ms refreshes caused 25 metrics calls in the measured
window; 500ms caused two or three. Both on-modes still reduced observed worker
throughput and increased p95 versus adjacent off-modes on this shared host. No
negligible-overhead claim or universally safe polling interval follows.

Review (2026-10-19): the original one-poller/one-hint-consumer-per-client design repeated
full queue metrics scans per tab; one shared LISTEN connection did not prevent that load.
Set.lookupMin could starve other hot queues. Separate snapshot/state publication and
poll/notify reads could reorder snapshots/updates or publish after unsubscribe. The proposed
slow TCP reader test might never fill the server outbox. Nonempty idle queues change age
fields, contradicting the former quiet-after-send test. Listener socket wait cleanup was
not exception-safe, and push was inconsistently described as trigger availability.

Document unexpected behaviors, bugs, optimizations, or insights discovered during
implementation. Provide concise evidence.

M1 (2026-10-20 UTC): all 88 inspection tests pass without --accept. Nine new
hand-reviewed frame goldens and encoder parity cover every protocol constructor.
Deterministic clock/barrier tests show 100 subscribers share one in-flight read,
unchanged fingerprints suppress polls, hint bursts coalesce, a slow queue does
not overlap itself or starve another, stale completions are discarded, and HTTP
admission occupancy produces one bounded retry. Capacity-one/two outbox barriers
prove drop-oldest ordering; real WebSocket tests cover caps, invalid/oversized
frames, safe errors, poll convergence, goodbye and host-owned Warp cleanup.

The suite compiles the package's real src modules as home modules so internal
scheduler/outbox seams can be tested without exposing them as library API. Public
library compilation is checked separately. Minimal cooldown leases are retained
for at most one minimum-refresh window after the last unsubscribe; no metrics,
metadata or listener interest is retained. This prevents unsubscribe/re-subscribe
from bypassing the refresh floor without unbounded historical queue state.
Active queues stay capped at wsMaxQueues; active plus cooldown entries stay at
2*wsMaxQueues, with inspection_busy on exhausted cooldown admission.


## Decision Log

- Decision (2026-10-20 UTC): Share the one-second default graceful delivery window
  across convenience-runner and runtime finalizers, instead of restarting a full
  window in both phases. Keep database cancellation/cleanup outside that delivery
  guarantee. Document and test idle nonempty age updates and safe invalid-frame
  errors without echoing client payloads.

- Decision (2026-10-20 UTC): Expose per-channel LISTEN acknowledgement separately
  from connection health; keep push as registered interest. Coalesce coincident
  ready hints and due polls into one notify-tagged authoritative read, advancing
  the poll deadline without adding a second scan. Retain independent fairness
  and minimum start spacing.
- Decision (2026-10-20 UTC): Replace port-rebind cleanup inference with direct
  owned-socket and joined-thread assertions through private lifecycle combinators.
  Use an uninterruptible test gate with guaranteed release to demonstrate bounded
  live inspection and worker isolation under the known cancellation limitation.

- Decision (2026-10-20 UTC): Keep internal frame/outbox/scheduler modules hidden,
  and compile the same src tree in this package's tests for clock/barrier access.
  Retain bounded cooldown leases after unsubscribe to preserve per-queue start
  spacing under churn; otherwise deleting all timing state would reset the floor.
  Snapshot publication and generation activation remain one STM transaction.

- Decision (2026-10-19): Replace per-client polling/debounce with one bounded, fair coordinator
  keyed by distinct queue, using plan 29's shared admission gate. Keep snapshot publication
  atomic and generation-check refresh results. Use deterministic barriers for concurrency
  and overflow tests; preserve numeric metrics and the structural keiro-ui frame convention.
- Decision (2026-10-19): push means registered notification interest only. Listener health
  and per-channel acknowledgement are separate states; no trigger inspection or mutation is
  introduced. Require exception-safe fd wait/connection cleanup and bounded notification
  batches. These are durable constraints in the boundary ADR.

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

Complete (2026-10-20 UTC): shared read scheduling, generations, bounded frames and
outboxes, refcounted reconnecting listener and scoped shutdown are implemented.
The final suite passes 107 tests on native and stock schemas; an earlier 106-case
snapshot passed twenty full stock stress runs. The full family passes all six
suites (72 core, 111 hasql, 79 effectful, 11 migration, 70 config, 107 inspection).
The final aarch64-darwin Nix flake check, Haddock, source distribution, strict docs
and formatting checks pass. Nine frame goldens remain strict missing-fixture tests.

Haddock and design note 021 describe every frame, source/push meaning, coalescing,
limits, overflow, recovery and cleanup. CAP-12 and changelogs record the feed.
The boundary ADR distills scheduler/listener ownership and the shared one-second
graceful delivery window; workload evidence records both shared query counts and
material observed worker cost. IR-6 remains explicitly outside this work. EP-5 can
now deliver the executable, OpenAPI, guide and least-privilege acceptance.


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
  If this is absent, the prerequisite is incomplete; do not redefine it here.
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
explains. The interval trades freshness against total rows scanned; five seconds is only a default,
not a safe workload bound. The coordinator removes the client multiplier and budgets work;
the user guide must require measurement before tuning either polling or notification refresh.

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

Scope: the frame types and their JSON, the shared refresh coordinator, transport machinery, the shared state with the
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
frame), `too_many_subscriptions`, `too_many_queues`, `frame_too_large`, `overflow`, and, reusing plan 29's classifier, `queue_not_found`,
`inspection_busy`, `inspection_timeout`, `database_unavailable`, and `database_error`. No `FromJSON ServerFrame` is written, because
`QueueMetrics` is encode-only by plan 28; tests decode server frames to `Data.Aeson.Value`.

Define a server-wide refresh coordinator in an internal module
`Pgmq.Inspect.Refresh`, owned by this plan. It shares the `InspectionBudget` allocated by
plan 29 with HTTP work. Its state is keyed by the exact metadata queue name and contains the
queue row, latest metrics/fingerprint, subscribers, a generation, last refresh start, one
in-flight flag, one dirty flag, and next poll deadline. A runtime may watch at most
`wsMaxQueues` distinct queues (default 100); excess requests get `too_many_queues` without
allocating a new worker. A connection still has its own subscription cap. Remove queue state
and listener interest after its last subscriber leaves. Repeated subscriptions share current
metrics or the one in-flight read; they must not launch duplicate scans.

A fair coordinator scheduler processes due polls and hints. One refresh per queue may run
at a time, globally bounded by `maxConcurrentReads` (default 2). Notify bursts set one dirty
flag; they do not append jobs or fork threads. Enforce at least `metricsMinRefreshUs`
(default 1,000,000) between refresh starts and coalesce within `notifyDebounceUs`. Polls
remain scheduled independently of hints, so a constantly hot queue cannot starve others.
Use a fair ready queue or deadlines with rotation, never repeated Set.lookupMin of a hot
queue. If work exceeds an interval, skip/coalesce missed ticks; do not accumulate catch-up
jobs. Each completed refresh fans the same metrics out to every current subscriber.

HTTP metrics may remain explicit on-demand reads, but share the read admission gate; they
must not spawn their own periodic work. The coordinator calls the existing lenient metrics
operation and does not add SQL. Metadata lookup uses plan 29's exact-name resolver; a global
42P01 is not automatically a missing queue. Preserve subscriptions on transient errors,
report at most one such error per connection per cycle, and retry through the bounded
scheduler. Remove a queue only after the metadata recheck confirms it is gone.

Keep only transport state per connection: subscription generations and a bounded outbox.
The outbox is a TBQueue of ServerFrame with an overflow flag. A full enqueue drops the oldest
frame and raises the flag in the same STM transaction. One writer atomically takes the next
frame and flag, sends an overflow error first when needed, then sends that frame. A send
failure ends the connection instead of being swallowed in an infinite writer loop. All JSON
frames (including snapshots, pong, errors, goodbye) use this writer; WebSocket control ping
uses the library's supported ping mechanism. Capacity must be positive (plan 29 validates it).

Successful subscribe publishes the initial snapshot and activates the subscription in one
STM transaction, after obtaining the initial metrics. Updates cannot overtake it. Every
refresh completion checks the generation before publishing: an unsubscribe or re-subscribe
invalidates work already in flight. Re-subscribe obtains a fresh snapshot through the same
coalesced/rate-limited path; it is not an escape hatch around metricsMinRefreshUs. Remove
queued updates for an unsubscribed generation or tag internal outbox entries so the writer
can discard them. Already-written socket bytes cannot be recalled; document that boundary.
On overflow clients treat following updates as incomplete until they re-read/re-subscribe.

The fingerprint contains every QueueMetrics field except scrape_time. Age fields advance
with wall time on a nonempty queue, so unchanged row counts do not imply a quiet feed. Poll
refreshes emit when that fingerprint changes; notify refreshes may emit even when unchanged,
within the same shared minimum refresh interval.

The reader accepts type-tagged JSON and enforces wsMaxFrameBytes (default 16,384) at the
WebSocket library's frame and assembled-message limits, before allocating an unbounded
receive buffer. Invalid frames yield invalid_frame; oversized frames are rejected/closed
with frame_too_large and best-effort goodbye. Validate queue-name length and client frame
shape before allocating subscription state, while retaining server-accepted foreign names.
Verify the pinned websockets limit APIs through Mori/source. No payload is echoed into errors.

Retain WebSocketState for connection capacity and shutdown. Acquire slots under mask and
release them in finally, including failed handshakes. Run each reader/writer/shutdown watcher
with structured concurrency; poll and hint threads belong to the shared coordinator, not
each connection. Shutdown stops new subscriptions, prevents new refreshes, sends goodbye
through each outbox, waits at most one second for delivery, then cancels/cleans all owned
connection tasks. The embeddable application must clean up even when the host owns Warp;
cancelling the convenience runner alone is insufficient. A physically stalled socket cannot
be promised delivery; bound the attempted graceful drain.

The public runtime shape remains:

```haskell
data InspectRuntime = InspectRuntime
  { runtimeApplication :: !Application,
    runtimeRequestShutdown :: !(IO ()),
    runtimeAwaitDrained :: !(Int -> IO ()),
    runtimeListenerAcknowledged :: !(Text -> STM Bool),
    runtimeListenerHealthy :: !(STM Bool)
  }

withInspectRuntime :: InspectConfig -> InspectEnv -> (InspectRuntime -> IO a) -> IO a
withInspectApplication :: InspectConfig -> InspectEnv -> (Application -> IO a) -> IO a
```

`withInspectRuntime` validates config, brackets one budget, coordinator and optional listener,
then constructs the HTTP/WS application using those same resources. Keep an internal builder
that accepts already-allocated resources so `withInspectApplicationWith` does not allocate a
second budget. `withInspectApplication` delegates to the runtime. `withInspectServer` and
`runInspectServer` retain plan 29's bind acknowledgement and cleanup contract and add graceful
WS draining before Warp shutdown. Retain the rejecting seam export for HTTP-only hosts.

The existing queueMetricsUnvalidated function is an EP-1 prerequisite, already implemented.
If it is missing, fix dependency state; do not redefine this library API in this plan.

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
`pollIntervalUs = 200_000` and `metricsMinRefreshUs = 100_000` (test-only):

- subscribe to a freshly created queue → the first frame is `snapshot` with `"queue"` equal to
  the name, `"push": false`, and `"metrics"."queue_length": 0`;
- send three messages through the pool → within three poll intervals (600 ms, allow 2 s) an
  `update` with `"source":"poll"` and `"metrics"."queue_length": 3` arrives; age fields may keep changing even without writes. Separately test an empty queue or a
  fixed mock fingerprint to prove equal metrics suppress poll updates;
- `ping` → `pong`;
- subscribe to a name no queue has → `error` with `"code":"queue_not_found"` and `"queue"` set;
  no snapshot follows;
- send the text `not json` → `error` with `"code":"invalid_frame"`;
- with `wsMaxSubscriptions = 1`, subscribe to two different queues → the second answer is
  `error` `too_many_subscriptions`;
- deterministically pause the writer using a test barrier, enqueue three updates into a
  capacity-two outbox, then release it: observe overflow before the retained two updates,
  oldest dropped, bounded memory and continued delivery. Not reading fifty small frames from
  a TCP client is not sufficient: kernel buffering may prevent outbox overflow entirely;
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

Implement the listener as a bracketed reconnect state machine, not an unprotected
acquire-then-serve sequence:

1. Acquire with `bracket`, so cancellation cannot leak a connection between acquire and
   serve. Release on every exit. Clear `healthy` in a finalizer, including cancellation.
   Use exponential reconnect backoff from 100ms to 5s, resetting after a healthy session.
2. Snapshot the desired registry and clear its dirty flag in one STM transaction; synchronize
   quoted LISTEN/UNLISTEN statements from that snapshot. Changes arriving during database
   I/O must leave the flag set for another reconciliation. Maintain an acknowledged channel
   set for deterministic tests; connection health alone does not acknowledge a new channel.
3. Inside a serialized `Session.onLibpqConnection` slice, drain at most 64 buffered
   notifications, dispatching only STM dirty-flag callbacks. If the batch fills, yield and
   recheck registry/cancellation before draining again; never wait for socket readiness
   while libpq may still contain buffered notifications.
4. When no buffered work remains, register `threadWaitReadSTM`, wait atomically for registry
   changes (first `orElse` branch) or socket readiness, and unregister with `finally`,
   including cancellation. On readiness use `consumeInput` and check connection status;
   errors/EOF leave the bracket and reconnect. Registry changes must not be starved by a
   continuously readable socket.
5. Catch synchronous failures only; rethrow `SomeAsyncException`. Set
   `application_name=pgmq-inspect-listener` using the verified settings API so tests can
   identify and terminate precisely that backend. Confirm LISTEN acknowledgements before
   testing acceleration, without making acknowledgement part of the public `push` meaning.

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

Wire one listener registration per distinct eligible queue into the shared coordinator,
not per client. The callback only marks that queue dirty in STM. The last subscriber leaving
removes the registration. Eligible means a configured listener, parseQueueName success and
non-partitioned metadata; foreign or partitioned queues stay poll-only. `push = true` means
notification interest was registered. It does not claim LISTEN acknowledgement, current
connectivity, or installed triggers; runtimeListenerHealthy is a separate observable.

For notify-only tests, expose a test seam that waits for the target channel to be acknowledged
by the listener, then insert. A snapshot or healthy connection alone is not proof that the
new LISTEN completed. Reconnect reissues all desired channels, and polling remains active.

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
  wait until `runtimeListenerAcknowledged runtime (notifyChannelName queue)` is true,
  capture that listener PID, then run through the pool

  ```sql
  select count(pg_terminate_backend(pid))::int
  from pg_stat_activity
  where application_name = 'pgmq-inspect-listener'
  ```

  and assert the count is at least one; send a message immediately; within two seconds an
  `update` arrives (its source may be `poll` or, if the reconnect won the race, `notify`; the
  assertion is that one arrives at all, which is the no-wedge guarantee); then wait for a different listener PID and
  the channel acknowledgement on that replacement session (allow ten seconds);
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
connection watches nothing until it subscribes; `push` as registered interest (configured listener, valid name, non-partitioned queue);
it is independent of listener health, LISTEN acknowledgement, and trigger existence; the `source` tag; the debounce; the poll
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
least one line, except the `queueMetricsUnvalidated` one, whose absence means the EP-1 prerequisite must be repaired.

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

State the frame protocol, the push flag's registered-interest meaning, the debounce and
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

Use an instrumented mock runner with a controllable clock and barriers. One hundred clients
watching one queue must cause one scheduled metrics call per cycle, and a burst must cause
one shared debounced call, subject to metricsMinRefreshUs. Re-subscribe storms cannot bypass
that limit. Two hot queues must both progress; a slow read cannot overlap a second read for
that queue; outstanding reads never exceed maxConcurrentReads; distinct queue count never
exceeds wsMaxQueues. Full admission emits inspection_busy and coalesces retries. A configured
SQL deadline emits inspection_timeout without disabling future polls.

Pause refresh completion, unsubscribe/re-subscribe, then resume it: no stale generation may
publish. Pause between initial metrics and publication: snapshot must precede updates. Test
outbox capacity one and two with a writer barrier, oversized fragmented messages before full
allocation, listener loss/recovery, cancellation while waiting on the fd, and host-owned Warp
outliving withInspectApplication. No client slot, listener registration, socket wait, or owned
thread may survive the runtime scope. Run a representative concurrent-consumer workload and
record throughput/p95 plus query counts with inspection off and on; choose workload-specific
limits rather than claiming that a default interval proves negligible overhead.

Acceptance is behaviour a person can observe with a WebSocket client; the test suite automates
it, and plan 31's executable lets you repeat it by hand. With the executable (or
`withInspectServer`) running against a database holding a queue `orders` with insert
notifications enabled, a client such as `websocat ws://127.0.0.1:9092/ws` that sends
`{"type":"subscribe","queue":"orders"}` receives, in order, a `snapshot` frame with
`"push":true`, and after `select pgmq.send('orders', '{"hello":1}')` in psql, after the configured debounce and minimum refresh interval an `update` frame with `"source":"notify"` and `queue_length` one higher.
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
and the internal `websocketApp :: InspectConfig -> MetricsCoordinator -> WebSocketState -> WS.ServerApp`.
The shared coordinator owns `InspectEnv`, the shared `InspectionBudget`, per-queue state and
optional listener; it is allocated once by `withInspectRuntime`, not per connection. `Pgmq.Inspect.Server`
exports `InspectRuntime (..)` and `withInspectRuntime :: InspectConfig -> InspectEnv -> (InspectRuntime -> IO a) -> IO a`,
and `withInspectApplication`, `withInspectServer`, and `runInspectServer` keep plan 29's
signatures. `pgmq-inspect/test/TestServer.hs` exports
`withTestRuntime :: InspectConfig -> InspectEnv -> (Int -> InspectRuntime -> IO a) -> IO a`.

At the end of Milestone 2, `Pgmq.Inspect.Listener` exports `Listener`,
`withListener :: Hasql.Connection.Settings.Settings -> (Listener -> IO a) -> IO a`,
`SubscriberKey`, `subscribeChannel :: Listener -> Text -> STM () -> IO SubscriberKey`,
`unsubscribeChannel :: Listener -> Text -> SubscriberKey -> IO ()`, and
`listenerHealthy :: Listener -> STM Bool`; the coordinator takes the real `Maybe Listener`;
`InspectRuntime.runtimeListenerHealthy` reflects it. The listener connection carries
`application_name = pgmq-inspect-listener`.

At the end of Milestone 3, `Pgmq.Inspect` re-exports public frame types and `withInspectRuntime`
(the coordinator, listener registry and budget stay internal),
`docs/design/021-inspection-surface-wire-contract.md` has the WebSocket section, the capability
record lists `pgmq-inspect/test/WebSocketSpec.hs` as evidence, and both changelogs carry
`Unreleased` entries. The existing library operations are consumed unchanged; EP-1 owns their contract.


Revision note (2026-10-19): replaced per-client scans with shared bounded scheduling; specified
snapshot/generation ordering, push semantics, deterministic overflow, frame caps and cancellation gates.
