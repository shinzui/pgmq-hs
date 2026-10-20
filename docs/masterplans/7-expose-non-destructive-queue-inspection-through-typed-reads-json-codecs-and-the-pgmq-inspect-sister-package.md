---
id: 7
slug: expose-non-destructive-queue-inspection-through-typed-reads-json-codecs-and-the-pgmq-inspect-sister-package
title: "Expose non-destructive queue inspection through typed reads, JSON codecs, and the pgmq-inspect sister package"
kind: master-plan
created_at: 2026-10-01T00:12:35Z
intention: "intention_01m3tcw9vmeeftdtqj53d1d6nb"
provenance:
  created_by:
    model: "claude-fable-5-1"
    harness: "claude-code"
    at: 2026-10-01T00:12:35Z
  revisions:
    - model: "claude-opus-5-5"
      harness: "claude-code"
      at: 2026-10-01T21:23:58Z
      mode: "implement"
      note: "EP-1 (ExecPlan 27) marked complete; codec ownership handed to EP-2"
    - model: "gpt-6-astra"
      harness: "codex-cli"
      at: 2026-10-19T18:23:59Z
      mode: "update"
      note: "Reviewed implemented reads and keiro-ui contracts; reopened EP-1 cost gates and aligned all five plans around shared budgets, wire correctness and release validation."
    - model: "gpt-6.1-sol"
      harness: "codex-cli"
      at: 2026-10-20T02:35:44Z
      mode: "implement"
      note: "Coordinate reopened EP-1 M4 implementation and validation"
    - model: "gpt-6.1-sol"
      harness: "codex-cli"
      at: 2026-10-20T02:56:34Z
      mode: "implement"
      note: "Coordinate EP-2 JSON codec implementation"
    - model: "gpt-6.1-sol"
      harness: "codex-cli"
      at: 2026-10-20T04:18:09Z
      mode: "implement"
      note: "Coordinate HTTP/feed/standalone implementation; user authorized continuous execution between children"
  reviews:
    - model: "gpt-6-astra"
      harness: "codex-cli"
      at: 2026-10-19T18:23:59Z
      verdict: "changes-requested"
      note: "Reviewed implemented reads and keiro-ui contracts; reopened EP-1 cost gates and aligned all five plans around shared budgets, wire correctness and release validation."
---

# Expose non-destructive queue inspection through typed reads, JSON codecs, and the pgmq-inspect sister package

This MasterPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.
If durable project context changes, update or create ADRs in docs/adr/ in the same change.


## Vision & Scope

Three improvement requests filed from the keiro runtime UI initiative sit in
`docs/improvement-requests/` (`IR-1` is completed; `IR-2` and `IR-3` remain open): `IR-1` (expose non-destructive queue
inspection reads), `IR-2` (provide JSON codecs for inspection-facing records), and `IR-3` (add a
sister package with HTTP and WebSocket inspection endpoints). Together they describe what a
browser needs in order to look at pgmq queues without a process reaching into pgmq-owned tables.
This initiative implements all three, and it does so in a way that serves two audiences at once:
the keiro runtime console, which will mount the resulting surface beside kiroku's and shibuya's,
and a pgmq-hs user who has never heard of keiro and wants to point a browser, a dashboard, or a
script at their queues.

When the initiative is complete, the following is true.

A pgmq-hs user can observe a queue without disturbing it. Before EP-1, the library offered only consuming reads: `pgmq.read` and `read_with_poll` lease rows and increment `read_ct`; `pop` removes rows, so "just looking" steals
messages from consumers and inflates the counters that retry and dead-letter policies depend on.
After this initiative the `Pgmq` umbrella module, the `Pgmq.Hasql.Sessions` module, and the
`Pgmq` effect with both of its interpreters offer four reads that modify nothing: a keyset-paged
peek over a queue table, the same over an archive table, and a fetch-by-id in each, returning a
typed not-found instead of an error. They accept any name the server accepts, including names
that `parseQueueName` rejects, because an inspection surface must show what exists.

Every record an inspection surface serves has one JSON encoding, owned by pgmq-hs and pinned by
golden tests. Today only the newtypes carry `ToJSON`/`FromJSON`; `Queue`, `UnvalidatedQueue`,
`Message`, `QueueMetrics`, `TopicBinding`, `RoutingMatch`, `NotifyInsertThrottle`,
`TopicSendResult`, and pgmq-config's `ReconcileAction` derive only `Eq`/`Generic`/`Show`, so
every downstream wire format hand-rolls its own dialect. After this initiative each of them, plus
the new `ArchivedMessage`, encodes with pgmq's own snake_case column names under a documented
policy: a published encoding is a compatibility surface, fields are never removed or re-typed,
additions are allowed.

A new package, `pgmq-inspect`, exposes those reads over HTTP and WebSocket. It exports the bare
WAI `Application` so a host can mount it beside other surfaces in one process, a convenience
runner that binds a host-chosen address and port, and a standalone executable so a user with
nothing but a database URL can run it. The HTTP side lists queues (via the lenient listing, so
foreign names show), reports per-queue and all-queue metrics with total and visible depth clearly
distinguished and the all-queue polling cost documented, browses queue and archive contents and
single messages through the non-destructive reads, reports topic bindings, routing matches, and
notification throttles, answers liveness and readiness probes, and returns errors in a structured
envelope with machine-readable snake_case codes mapped from the typed error model. The WebSocket
side lets a client subscribe to queues and receive a snapshot followed by metric updates, woken
early by the queue's insert notifications when they exist but driven to correctness by an
authoritative poll, so a client that misses every notification still converges. CORS is
configurable with an explicit allowed-origins list and disabled by default. A served OpenAPI 3
document and a pgmq-hs-owned user guide describe the whole contract.

The second audience shapes several decisions, and the initiative calls this posture
*keiro-independence*. The package depends on nothing from keiro, kiroku, or shibuya; it adopts the
cross-project inspection conventions (`mori://shinzui/keiro-ui`, path
`docs/architecture/inspection-api-conventions.md`, artifact-level URI pending) because they are
sensible, but pgmq-hs records its own copy of the wire contract in its own design notes and user
guide, uses pgmq's own vocabulary (queues, messages, archive, visibility timeout), and freezes
the published shapes on its own authority. The surface must be usable with no keiro process in
front of it: the executable serves it directly, the application is path-prefix-agnostic so it can
be mounted anywhere or at the root, the service descriptor at `GET /` tells a client where the
OpenAPI document and the WebSocket endpoint are, and the OpenAPI document lets anyone generate a
client in any language. A browser UI shipped by pgmq-hs itself (a future `pgmq-ui`) is outside
this initiative, but every artifact it would consume exists at the end of it, and the Decision
Log records the extension points reserved for it.

Explicitly excluded: mutation endpoints of any kind (send, delete, archive, purge, visibility
changes), which `IR-3` defers to a separate request with its own safety discipline;
authentication and authorization beyond stating the trusted-network or authenticating-proxy
posture; Prometheus exposition; a LISTEN-based streaming API in `pgmq-core` or `pgmq-hasql` (the
listener loop lives in the sister package); a bundled browser UI; Hackage publication; and
version bumps, which belong to the family's release train (see Integration Points). The
transient-classification fix for disconnects (`IR-4`, `docs/plans/26-…`), the concurrent
reconciliation report (`IR-5`), and the blackholed-call deadline (`IR-6`) are separate work;
IR-4 is already completed; this initiative consumes the current `isTransient`.


## Decomposition Strategy

The work splits into five streams by functional concern, grouped into three waves by their
hard dependencies.

The first wave is the library foundation and has two independent streams. The first stream,
`EP-1`, is the non-destructive reads of `IR-1`: a new `ArchivedMessage` record in `pgmq-core`,
four hand-written statements and sessions in `pgmq-hasql`, four read constructors plus `QueueMetricsUnvalidated` on the `Pgmq`
effect with cases in both interpreters, the umbrella re-exports, and the tests that prove the
reads leave `vt` and `read_ct` byte-identical, page every message exactly once in a quiescent fixture without `OFFSET`,
and work for foreign and mixed-case names. The second stream, `EP-2`, is the codecs of `IR-2`:
hand-written aeson instances on the records listed above, the field-naming policy as a design
note and Haddock, and golden tests that pin every encoding. They touch different parts of the
same modules (`EP-1` adds a type and functions to `Pgmq.Types` and `Pgmq.Hasql.Statements.Types`;
`EP-2` adds instances to the same modules) and can proceed in parallel; the one record they both
care about, `ArchivedMessage`, is handled by the integration rule in Integration Points.

The second wave is the surface. `EP-3` creates the `pgmq-inspect` package with the HTTP side:
package skeleton, configuration including CORS, the interpreter-agnostic environment, the router,
the error envelope and its mapping from `PgmqRuntimeError`, the mountable `Application`, the
runner, the Nix and registry wiring, and an HTTP test harness. It hard-depends on both first-wave
streams because its handlers call the new reads and encode with the new instances. `EP-4` adds
the WebSocket live feed to that package: the frame protocol, the server-wide LISTEN connection
with reconnect and poll fallback, bounded per-connection queues with in-band overflow signaling,
and the fault test that kills the LISTEN backend. It hard-depends on `EP-3` because it fills a
seam `EP-3` leaves in the combined application and reuses its harness. The two are separate
streams because they are verified by different means (an HTTP client against a Warp server
versus a WebSocket client plus a PostgreSQL backend kill) and because the HTTP surface is
useful and shippable without the feed.

The third wave, `EP-5`, is what makes the surface independently adoptable: the standalone
executable, the OpenAPI document with its golden and route-coverage tests, the pgmq-hs-owned
user guide covering every endpoint and frame, the capability records and README changes, and
the closure of `IR-1` through `IR-3`. It hard-depends on `EP-3` and `EP-4` because it documents
their exact wire shapes and serves both over the executable.

Alternatives considered. A single ExecPlan was rejected: the work spans four existing packages
and one new one, has real ordering constraints, and would exceed any reasonable single-plan
scope. Folding the codecs into the reads plan was rejected because the codecs are a
self-standing deliverable (`IR-2` says so explicitly: any consumer that puts pgmq-hs state on a
wire needs them) and because keeping them separate lets two sessions work the first wave in
parallel. Folding the WebSocket feed into the HTTP plan was rejected for the verification
reason above and because the feed's LISTEN loop is the single riskiest piece of the initiative
and deserves its own fault test and retrospective. Building the HTTP surface on hasql sessions
directly (as `kiroku-metrics` does over its store) rather than on the `Pgmq` effect was
rejected: running handlers through an effect runner the host supplies lets a host choose the
plain or the traced interpreter, lets the router be tested against a mock interpreter without a
database, and reuses `isTransient` for the 503 mapping without re-deriving it. Using servant
for the router was rejected in favour of the plain WAI routing both existing sister packages
use; the OpenAPI document is built programmatically with the `openapi3` library and pinned by
a golden file instead of being derived from a servant API type. Naming the package
`pgmq-metrics` (the request's working name) was rejected because metrics are one of seven
endpoint groups and the name would mislead the second audience; `pgmq-inspect` names what the
package does and leaves `pgmq-ui` free for a future browser package.

ADRs consulted, all local and cited by repository-relative path:

- [docs/adr/haskell-dependency-bounds-and-nix-pin-policy.md](../adr/haskell-dependency-bounds-and-nix-pin-policy.md)
  governs how the new package declares bounds (a bound states compatibility, the Nix pin states
  what is tested) and records that `pgmq-effectful`'s test suite does not run under
  `nix flake check`; `EP-3` wires `pgmq-inspect` into the overlay and the flake checks under the
  same rules.
- [docs/adr/pgmq-1.12-1.13-compatibility.md](../adr/pgmq-1.12-1.13-compatibility.md) fixes the
  supported server range (PGMQ 1.12 and 1.13), the `QueueMetrics` projection with its nullable
  `defaultPartitionLength`, and the metrics-test isolation rule (each metrics case on its own
  disposable database); the codecs and the metrics endpoints inherit all three.
- [docs/adr/fifo-native-overrides-and-index-upgrade-boundary.md](../adr/fifo-native-overrides-and-index-upgrade-boundary.md)
  and design note [docs/design/012-vendor-upstream-pgmq-sql.md](../design/012-vendor-upstream-pgmq-sql.md)
  state that no upstream function body is overridden and that hand-written statements are
  permitted exactly for reads upstream has no equivalent for; the inspection reads are plain
  `SELECT`s over tables the upstream schema defines and add no migration.
- The new [docs/adr/queue-inspection-surface-boundary-and-wire-contract.md](../adr/queue-inspection-surface-boundary-and-wire-contract.md),
  written with this MasterPlan, records the package boundary, the keiro-independence posture,
  the lenient-name read contract, and the wire-freeze rule that every child plan obeys.

Cross-repository decisions that apply, cited by the canonical handles the registry returns:
`mori://shinzui/keiro-ui/okf/adrs/concepts/ADR-1` (queue-level views belong to pgmq-hs),
`mori://shinzui/keiro-ui/okf/adrs/concepts/ADR-2` (the WebSocket frame convention),
`mori://shinzui/keiro-ui/okf/adrs/concepts/ADR-3` (push is a hint, poll is truth),
`mori://shinzui/keiro-ui/okf/adrs/concepts/ADR-4` (inspection surfaces live in sister packages
exporting an embeddable `Application`), and `mori://shinzui/keiro-ui/okf/adrs/concepts/ADR-5`
(no backend-for-frontend; machine-readable specs regenerate hand-written clients). The
conventions document itself is `mori://shinzui/keiro-ui`, path
`docs/architecture/inspection-api-conventions.md` (artifact-level URI pending). keiro's own
mounting request, `mori://shinzui/keiro/okf/improvement-requests/concepts/IR-31`, expects to
mount this surface under a per-surface prefix such as `/pgmq/…` with WebSocket upgrades routed
through the same prefix, which is why the application routes on `pathInfo` and dispatches the
upgrade from the router rather than from the raw request path.

The design notes that carry the contracts the child plans extend are
[docs/design/002-queue-visible-length.md](../design/002-queue-visible-length.md) (total versus
visible depth), [docs/design/013-pgmq-effectful-error-model.md](../design/013-pgmq-effectful-error-model.md)
and [docs/design/017-transient-error-classification.md](../design/017-transient-error-classification.md)
(the typed error model and `isTransient`), [docs/design/015-notification-delivery-contract.md](../design/015-notification-delivery-contract.md)
(NOTIFY is a wake-up hint, never data; every listener keeps a poll fallback), and
[docs/design/016-queue-name-validation.md](../design/016-queue-name-validation.md) (the lenient
path for foreign and mixed-case names).


## Exec-Plan Registry

| # | Title | Path | Hard Deps | Soft Deps | Status |
|---|-------|------|-----------|-----------|--------|
| 1 | Add non-destructive peek, archive, and lookup reads across the pgmq layers | docs/plans/27-add-non-destructive-peek-archive-and-lookup-reads-across-the-pgmq-layers.md | None | EP-2 (integration: `ArchivedMessage` codec) | Complete |
| 2 | Provide stable JSON codecs for the inspection-facing records | docs/plans/28-provide-stable-json-codecs-for-the-inspection-facing-records.md | None | EP-1 (integration: `ArchivedMessage` codec) | Complete |
| 3 | Create the pgmq-inspect sister package with the HTTP inspection surface | docs/plans/29-create-the-pgmq-inspect-sister-package-with-the-http-inspection-surface.md | EP-1, EP-2 | None | In Progress |
| 4 | Add the NOTIFY-accelerated, poll-authoritative WebSocket live feed to pgmq-inspect | docs/plans/30-add-the-notify-accelerated-poll-authoritative-websocket-live-feed-to-pgmq-inspect.md | EP-3 | None | Not Started |
| 5 | Ship the standalone pgmq-inspect server, the OpenAPI document, and the inspection guide | docs/plans/31-ship-the-standalone-pgmq-inspect-server-the-openapi-document-and-the-inspection-guide.md | EP-3, EP-4 | None | Not Started |

EP-1 M1–M4 are complete. M4 adds the performance and concurrency evidence missing
from the original acceptance; `IR-1` retains its historical completed status.

Status values: Not Started, In Progress, Complete, Cancelled.
Hard Deps and Soft Deps reference other rows by their # prefix (e.g., EP-1, EP-3).


## Dependency Graph

Wave one is complete: EP-1 and EP-2 have passed their acceptance gates. EP-3
(the HTTP inspection surface) is the next ready plan.
The two foundation plans have no hard dependency on one another. The `ArchivedMessage`
record and its codecs now exist; EP-2 owns the codec contract. The integration
rule below records that ownership.

Wave two: `EP-3` hard-depends on `EP-1` (its browse, archive, and message-by-id handlers call
`peekMessages`, `peekArchivedMessages`, `lookupMessage`, and `lookupArchivedMessage` through the
`Pgmq` effect, and the `pgmq-inspect` test suite asserts the end-to-end non-destructive
guarantee those reads provide) and on `EP-2` (every response body is produced by the `ToJSON`
instances `EP-2` adds; the package deliberately writes no encoding of a `pgmq-core` or
`pgmq-hasql` record of its own). `EP-4` hard-depends on `EP-3`: it fills the WebSocket seam in
`EP-3`'s combined application, uses `EP-3`'s `InspectEnv`, `InspectConfig`, error codes, and
test harness, and its `snapshot` and `update` frames carry `QueueMetrics` encoded by `EP-2`'s
instance through `EP-3`'s wiring.

Wave three: `EP-5` hard-depends on `EP-3` and `EP-4`. The OpenAPI document must enumerate
exactly the routes `EP-3` serves (a test checks it against the exported route table), the user
guide must show the exact frames `EP-4` speaks, and the executable must serve both.

Nothing here blocks on the other open initiatives. `docs/plans/26-…` (transient classification)
is already completed; EP-3 consumes its current `isTransient` through one function call. MasterPlan 6's release
preparation (`docs/plans/25-…`) is the release train this initiative's changes ride on; see
the release coordination note in Integration Points.


## Integration Points

**`ArchivedMessage` (type owned by EP-1, codec now owned solely by EP-2).** `EP-1` adds
`data ArchivedMessage = ArchivedMessage { archivedMessage :: !Message, archivedAt :: !UTCTime }`
to `pgmq-core/src/Pgmq/Types.hs` and exports it from `Pgmq` and `Pgmq.Effectful`. Its JSON
encoding is the flattened `Message` encoding plus `"archived_at"`. EP-1 landed without codecs; EP-2 has now implemented the flattened codec and
reviewed goldens, including null normalization and exact int64 fixtures. The original
order-independent rule below describes the historical decomposition only. The rule: if `Pgmq.Types` already holds the policy
Haddock and instances when `EP-1` runs, `EP-1` adds the `ArchivedMessage` instances and golden
file; if `ArchivedMessage` already exists when `EP-2` runs, `EP-2` covers it like every other
record. Whichever runs second checks the working tree and does the work; neither waits.

**The inspection argument records and read names (owned by EP-1; consumed by EP-3, EP-4,
EP-5).** `pgmq-hasql/src/Pgmq/Hasql/Statements/Types.hs` gains
`PeekMessages { unvalidatedQueueName :: !Text, afterMessageId :: !(Maybe MessageId), limit :: !Int32 }`
and `LookupMessage { unvalidatedQueueName :: !Text, messageId :: !MessageId }`. The sessions in
`pgmq-hasql/src/Pgmq/Hasql/Sessions.hs` are `peekMessages :: PeekMessages -> Session (Vector Message)`,
`peekArchivedMessages :: PeekMessages -> Session (Vector ArchivedMessage)`,
`lookupMessage :: LookupMessage -> Session (Maybe Message)`, and
`lookupArchivedMessage :: LookupMessage -> Session (Maybe ArchivedMessage)`; the effect
constructors and smart constructors in `pgmq-effectful/src/Pgmq/Effectful/Effect.hs` carry the
same four names. A fifth operation, `queueMetricsUnvalidated :: Text -> Session QueueMetrics`
(effect constructor `QueueMetricsUnvalidated :: Text -> Pgmq m QueueMetrics`, traced label
`pgmq.metrics`), runs the existing `pgmq.metrics($1)` projection for any server-accepted name,
because `queueMetrics` is typed over `QueueName` and the per-queue metrics route and the live
feed must work for foreign names too. The name field is plain `Text` because the reads must
accept names `parseQueueName` rejects; a caller holding a `QueueName` passes
`queueNameToText`. Every page or lookup session
first asks the server `select pgmq.format_table_name($1, 'q')` (or `'a'`) and then runs an
`unpreparable` statement whose SQL embeds the double-quoted physical table name, orders by
`msg_id`, filters `msg_id > $1` when a cursor is given, and uses `LIMIT`, never `OFFSET`. The
page read returns at most `limit` rows; the HTTP layer requests `limit + 1` to learn whether a
next page exists. A missing queue surfaces as the server's SQLSTATE `42P01` inside a
`PgmqSessionError`; a missing message is `Nothing`. The traced interpreter labels the four
operations `pgmq.peek`, `pgmq.peek_archive`, `pgmq.lookup_message`, and
`pgmq.lookup_archived_message` with `OTel.Internal` kind and the text name as destination.

**The JSON field-naming policy (owned by EP-2; obeyed by EP-3, EP-4, EP-5).** Recorded in
`docs/design/020-json-wire-encodings.md` and in the Haddock of each instance. Every field is
snake_case and, where a pgmq SQL column exists, is the column's name: `queue_name`,
`is_partitioned`, `is_unlogged`, `created_at`; `msg_id`, `read_ct`, `enqueued_at`,
`last_read_at`, `vt`, `message`, `headers`, `archived_at`; `queue_length`, `queue_visible_length`,
`newest_msg_age_sec`, `oldest_msg_age_sec`, `total_messages`, `scrape_time`,
`default_partition_length`; `pattern`, `queue_name`, `bound_at`, `compiled_regex`;
`throttle_interval_ms`, `last_notified_at`. `Maybe` fields are always present and encode as
`null`. Timestamps are aeson's default ISO-8601 UTC form. Instances are hand-written with
`object` and `withObject`, never Generic-derived, so no deriving option can move a field.
`Queue` and `UnvalidatedQueue` encode identically; only decoding differs (`Queue` validates
through `parseQueueName`). `FromJSON` exists for `Queue`, `UnvalidatedQueue`, `Message`, and
`ArchivedMessage`; the rest are encode-only. `ReconcileAction` encodes as a tagged object with
an `"action"` key holding the snake_case constructor name. The wire types `EP-3` and `EP-4`
define in `pgmq-inspect` (pages, the error envelope, health bodies, frames) follow the same
rules and are pinned by the same kind of golden test.

**The `pgmq-inspect` package surface (owned by EP-3; extended by EP-4 and EP-5).** Directory
`pgmq-inspect/`, package `pgmq-inspect`, initial version `0.1.0.0`, bounds `pgmq-core`,
`pgmq-hasql`, `pgmq-effectful` all `>=0.6 && <0.7` like the rest of the family. Modules:
`Pgmq.Inspect` (umbrella), `Pgmq.Inspect.Config`, `Pgmq.Inspect.Env`, `Pgmq.Inspect.Wire`,
`Pgmq.Inspect.Error`, `Pgmq.Inspect.Cors`, `Pgmq.Inspect.Http`, `Pgmq.Inspect.Server`; `EP-4`
adds `Pgmq.Inspect.WebSocket` and `Pgmq.Inspect.Listener`; `EP-5` adds `Pgmq.Inspect.OpenApi`
and the executable `pgmq-inspect` under `pgmq-inspect/app/Main.hs`. The extension set follows
`pgmq-hasql` (`OverloadedLabels`, `DuplicateRecordFields`, `generic-lens` and `lens` for field
access; never `OverloadedRecordDot` or `NoFieldSelectors`). The key types:

```haskell
-- Pgmq.Inspect.Env
type Inspection a = Eff '[Pgmq, Error PgmqRuntimeError, IOE] a

data InspectEnv = InspectEnv
  { runInspection :: forall a. Inspection a -> IO (Either PgmqRuntimeError a),
    listenerSettings :: !(Maybe Hasql.Connection.Settings.Settings)  -- Nothing: poll-only feed
  }

poolEnv :: Pool -> Maybe Settings -> InspectEnv            -- plain interpreter
tracedPoolEnv :: Pool -> OTel.Tracer -> Maybe Settings -> InspectEnv

-- Pgmq.Inspect.Config
data CorsPolicy = CorsDisabled | CorsAllowOrigins !(NonEmpty ByteString)  -- no wildcard constructor
data InspectConfig = InspectConfig
  { corsPolicy :: !CorsPolicy,            -- default CorsDisabled
    defaultPageLimit :: !Int32,           -- default 50
    maxPageLimit :: !Int32,               -- default 500
    readinessTimeoutUs :: !Int,           -- default 1_000_000
    pollIntervalUs :: !Int,               -- default 5_000_000 (EP-4 feed)
    notifyDebounceUs :: !Int,             -- default 100_000 (EP-4 feed)
    wsMaxConnections :: !Int,             -- default 100 (EP-4)
    wsMaxSubscriptions :: !Int,           -- default 100 per connection (EP-4)
    wsQueueCapacity :: !Natural,          -- default 256 frames per connection (EP-4)
    maxConcurrentReads :: !Int,           -- default 2 across HTTP and WebSocket work
    metricsMinRefreshUs :: !Int,          -- default 1_000_000 per distinct queue
    wsMaxQueues :: !Int,                  -- default 100 distinct queues per runtime
    wsMaxFrameBytes :: !Int               -- default 16_384 inbound frame/message bytes
  }
data InspectServerConfig = InspectServerConfig { bindHost :: !String, bindPort :: !Int, inspect :: !InspectConfig }
defaultInspectConfig :: InspectConfig
defaultInspectServerConfig :: InspectServerConfig   -- 127.0.0.1, 9092

-- Pgmq.Inspect.Server
withInspectApplication :: InspectConfig -> InspectEnv -> (Application -> IO a) -> IO a
withInspectServer :: InspectServerConfig -> InspectEnv -> (RunningInspectServer -> IO a) -> IO a
runInspectServer :: InspectServerConfig -> InspectEnv -> IO ()   -- blocks
data RunningInspectServer = RunningInspectServer { serverPort :: !Int, serverThread :: !(Async ()) }

-- Pgmq.Inspect.Http
routeTable :: [(Method, [PathSegment])]   -- every served route, with placeholders; EP-5's coverage test reads it
```

The default port is 9092 (shibuya-metrics uses 9090, kiroku-metrics 9091). The application
routes on WAI `pathInfo`, so a host that mounts it under a prefix and strips the prefix needs
nothing else; the WebSocket upgrade is dispatched from the `["ws"]` route with
`Network.Wai.Handler.WebSockets.websocketsApp`, not from the raw request path, so the upgrade
works behind a prefix too. `EP-3` leaves the WebSocket seam as a `WS.ServerApp` that rejects
every upgrade with a JSON `error` body of code `websocket_unavailable`; `EP-4` replaces its
internals without changing `withInspectApplication`'s signature.

**The HTTP routes (owned by EP-3; documented by EP-5).** All `GET`, all JSON, all relative to
the mount root:

```text
GET /                                    service descriptor {"service":"pgmq-inspect","version":"…","websocket_path":"ws"}; EP-5 adds "openapi_path":"openapi.json"
GET /queues                              JSON array of queue objects (lenient listing)
GET /queues/{queue}                      one queue object, 404 queue_not_found
GET /queues/{queue}/metrics              QueueMetrics, 404 queue_not_found
GET /metrics                             JSON array of QueueMetrics (scans across all queue rows; explicit on-demand use)
GET /queues/{queue}/messages?from&limit  page {"items":[Message…],"next_cursor":N?}
GET /queues/{queue}/messages/{msg_id}    Message, 404 message_not_found
GET /queues/{queue}/archive?from&limit   page {"items":[ArchivedMessage…],"next_cursor":N?}
GET /queues/{queue}/archive/{msg_id}     ArchivedMessage, 404 message_not_found
GET /queues/{queue}/bindings             JSON array of TopicBinding (all bindings filtered by name, so foreign names work)
GET /bindings                            JSON array of TopicBinding
GET /routing/test?routing_key=…          JSON array of RoutingMatch, 400 invalid_routing_key
GET /notify/throttles                    JSON array of NotifyInsertThrottle
GET /health/live                         {"alive":true}
GET /health/ready                        {"ready":bool,"checks":[{"name":"postgres","healthy":bool,"latency_ms":n,"error":null|"…"}]} 200 or 503
GET /ws                                  WebSocket upgrade (EP-4); a plain GET gets 426 with code websocket_upgrade_required
GET /openapi.json                        (EP-5) the OpenAPI 3 document
```

`from` is an exclusive `msg_id` cursor; `limit` defaults to `defaultPageLimit` and is capped at
`maxPageLimit`; `next_cursor` is the last item's `msg_id` and is omitted entirely on the last
page. Unknown paths answer 404 `not_found`; non-`GET` methods answer 405 `method_not_allowed`.

**The error envelope and code vocabulary (owned by EP-3; reused by EP-4's `error` frames and
documented by EP-5).** Every error body is `{"error":{"code":"…","message":"…","details":{…}?}}`.
Codes: `inspection_busy` (503 when admission is full), `inspection_timeout` (503 for a configured database statement deadline), `not_found`, `method_not_allowed`, `queue_not_found` (queue-scoped metadata confirms absence; `42P01` alone is insufficient), `message_not_found`, `invalid_cursor`, `invalid_limit`, `invalid_message_id` (a
`{msg_id}` segment that is not an integer), `invalid_routing_key`,
`websocket_upgrade_required`, `websocket_unavailable`, `database_unavailable` (503 when
`isTransient` holds), and `database_error` (500 for remaining failures). Database errors use fixed public messages
and an allowlisted SQLSTATE only; raw server message/detail/hint, SQL, and parameters stay
out of browser responses. An over-cap `limit` is capped silently; a
malformed one is 400. An origin absent from the CORS list is served without CORS headers rather
than refused. `EP-4` adds `invalid_frame`, `too_many_subscriptions`, `too_many_queues`,
`frame_too_large`, and `overflow`. Queue-not-found mapping is contextual: only a
queue-scoped read with verified metadata may map a disappeared physical table to that code;
a global missing table or missing schema is `database_error`.

**The WebSocket protocol (owned by EP-4; documented by EP-5).** Endpoint `/ws`. Client frames:
`{"type":"subscribe","queue":"…"}`, `{"type":"unsubscribe","queue":"…"}`, `{"type":"ping"}`.
Server frames: `{"type":"snapshot","queue":"…","metrics":{QueueMetrics},"push":true|false}`
immediately after a successful subscribe (`push` is `false` when the name fails
`parseQueueName`, when the queue is partitioned, because partitioned queues receive no insert
notifications, or when no listener is configured, meaning updates come from polling only);
`{"type":"update","queue":"…","metrics":{QueueMetrics},"source":"notify"|"poll"}`;
`{"type":"pong"}`; `{"type":"error","code":"…","message":"…","queue":"…"|null}` (the `queue`
key is always present and is `null` for errors that are not about one queue, per the
explicit-null rule of the JSON policy);
`{"type":"goodbye"}` before any server-initiated close. The server pings idle connections every
30 seconds. One server-wide LISTEN connection, acquired from `listenerSettings` with
`applicationName "pgmq-inspect-listener"`, listens on `notifyChannelName` for every subscribed
validated queue (refcounted `LISTEN`/`UNLISTEN`), reconnects with exponential backoff when it
drops, and re-issues every `LISTEN` after reconnecting; subscribers keep receiving poll-sourced
updates throughout. The `push` flag means an eligible queue has registered notification interest; it does not
promise that LISTEN is ready, connected, or that a trigger exists. The poll is authoritative: on each poll cycle a server-wide coordinator reads
`queueMetricsUnvalidated` once per distinct subscribed queue and fans out the result and emits an `update` with `source` `poll` whenever
the metrics (ignoring `scrape_time`) changed.

**Inspection work budget (owned by EP-3; consumed by EP-4 and EP-5).** One runtime owns
an admission gate shared by HTTP and background reads (`maxConcurrentReads = 2`); a full gate
fails promptly with `inspection_busy`, rather than accumulating waiting request threads.
Deploy against a dedicated inspection pool/role with finite PostgreSQL `statement_timeout`
(default standalone 2000 ms) and `lock_timeout` (250 ms), separate from worker connections.
Map a configured statement deadline to `inspection_timeout`. Host-supplied runners must
honor this contract; do not change settings on arbitrary borrowed worker connections.
The existing blackholed-response limitation in IR-6 remains an explicit integration risk;
`System.Timeout` is not evidence of a hard network deadline. Failure tests must demonstrate
that stalled inspection cannot consume the worker pool or create unbounded detached work.

**Shared refresh coordinator (owned by EP-4).** The coordinator owns one in-flight refresh,
latest metrics, and one coalesced dirty flag per distinct queue. Polling, notification hints,
and repeated subscriptions share it; connection handlers do not poll independently. Enforce
`metricsMinRefreshUs` between starts, at most `wsMaxQueues` distinct queues, fair scheduling,
and the EP-3 admission budget. Full metrics cost O(rows scanned), not O(1); notify debounce
alone is not a load limit. An empty queue's fingerprint may stay unchanged, but ages advance
on a nonempty idle queue. Snapshot publication and subscription activation are atomic, and
generations prevent stale refresh results crossing unsubscribe/re-subscribe boundaries.

**Browser and specification acceptance (owned by EP-5; codecs owned by EP-2).** Keep the
published numeric `MessageId` encoding; require lossless parsing before JavaScript converts
64-bit tokens to `Number`, and exact decimal cursor echo. Test 9007199254740993 and
9223372036854775807 in a client fixture. Arbitrary JSON headers are allowed; `Nothing` and
`Just Null` intentionally share the JSON null representation and decoding normalizes to
`Nothing`. OpenAPI must use a relative server URL `.` so `/pgmq/openapi.json` describes
`/pgmq/queues`, validate actual encoded response fixtures, and describe readiness 503 with
the readiness body. Golden files alone do not prove schema agreement. Verify root and prefix
mounts including encoded foreign names, WebSocket upgrades, and trailing-slash base URLs.

**The test harness (owned by EP-3; reused by EP-4 and EP-5).** `pgmq-inspect/test/EphemeralDb.hs`
is a copy of `pgmq-effectful/test/EphemeralDb.hs` (the family keeps per-package copies
deliberately), and `pgmq-inspect/test/TestServer.hs` starts the application on an OS-assigned
free port with `Warp.openFreePort` and `Warp.runSettingsSocket`, returning the port for
`http-client` and `websockets` clients.

**Nix, registry, and build wiring (owned by EP-3; extended by EP-5).** `nix/haskell-overlay.nix`
gains `pgmq-inspect = dontCheck (doJailbreak (final.callCabal2nix "pgmq-inspect" ../pgmq-inspect { }))`
and, only if the build needs it, the `wai-websockets` executable-dependency override kiroku
uses; `flake.module.nix` adds the package and the library check; `cabal.project` lists
`pgmq-inspect`; `justfile`'s `nix-build` recipe adds it; `mori.dhall` adds the package and puts
it in the `pgmq-hs` bundle; the README's package table gains a row.

**Design-note numbering.** `EP-1` writes `docs/design/019-non-destructive-inspection-reads.md`,
`EP-2` writes `docs/design/020-json-wire-encodings.md`, `EP-3` writes
`docs/design/021-inspection-surface-wire-contract.md` and `EP-4` extends it with the WebSocket
section. The numbers are pre-assigned here so the parallel first-wave plans cannot collide.

**Capability records.** Handles are allocated with `okf id next docs/capabilities --profile docs/capabilities/profile.dhall CAP`
at the moment of writing, never hardcoded: `EP-1` records the inspection reads, `EP-2` the JSON
encodings, `EP-3` the HTTP surface (one record `EP-4` and `EP-5` extend with the feed and the
executable). The two first-wave plans may both see `CAP-10` as next; whichever writes second
takes `CAP-11`.

**Changelogs and the release train.** Every plan appends an `Unreleased` section to the root
`CHANGELOG.md` and to each changed package's changelog (`EP-3` creates
`pgmq-inspect/CHANGELOG.md`). No plan bumps a version. The `Pgmq` effect gains constructors in
`EP-1`, which is a breaking change for exhaustive interpreters and makes the family's next
release a major one; MasterPlan 6's `docs/plans/25-…` already prepares `0.7.0.0`, and whichever
release preparation runs after this initiative lands must include `pgmq-inspect`, move its
internal bounds with the family, and list the new constructors as breaking.

Cross-plan decisions that deserve ADR records: the package boundary, keiro-independence
posture, lenient-name read contract, and wire-freeze rule are recorded now in
`docs/adr/queue-inspection-surface-boundary-and-wire-contract.md`. The LISTEN-loop design
(one server-wide connection, refcounted channels, reconnect with re-`LISTEN`, poll authority) is
a candidate for promotion from `EP-4`'s Decision Log if its retrospective finds it durable.


## Progress

- [x] EP-1 M1: `ArchivedMessage`, the two argument records, four statements, and four sessions exist; a red-then-green `InspectionSpec` proves byte-identical `vt`/`read_ct`, exactly-once keyset paging without `OFFSET`, archive timestamps, and typed not-found (2026-10-01)
- [x] EP-1 M2: four `Pgmq` effect constructors with plain and traced cases; umbrella re-exports and compile witnesses; foreign and mixed-case names covered on a dedicated instance; partitioned queues covered (2026-10-01)
- [x] EP-1 M3: design note 019, capability record, Haddocks, changelogs; `IR-1` completed (2026-10-01)
- [x] EP-1 M4: real 100,000-row live/archive plans on both schemas with required pg_partman, deterministic moving-view tests, exception-safe consumer worker, and documented cost/isolation limits; full native/stock/partman and effectful validation passed (2026-10-20 UTC)
- [x] EP-2 M1: thirteen hand-written encoders and four record decoders with 53 reviewed goldens, arbitrary-header normalization and exact int64 fixtures; three full suites pass (2026-10-20 UTC)
- [x] EP-2 M2: design note 020, CAP-11, four changelogs and IR-2 closure; strict missing-fixture guards, build/Haddock/sdist, docs/format and final aarch64-darwin Nix checks passed (2026-10-20 UTC)
- [x] EP-3 M1: `pgmq-inspect` builds under cabal and Nix with config, env, wire types, error mapping, and a router serving the descriptor, listings, metrics, and health; mock-interpreter router tests
- [x] EP-3 M2: browse, archive, by-id, bindings, routing-test, and throttle routes; CORS middleware; the end-to-end non-destructive test and the error-mapping tests green against ephemeral PostgreSQL
- [ ] EP-3 M3: runner, mountable application with the rejecting WebSocket seam, registry and README wiring, design note 021, capability record, changelogs
- [ ] EP-4 M1: shared refresh coordinator and bounded admission; frame types with golden encodings; per-connection bounded queues with overflow signaling; subscribe/snapshot/unsubscribe/ping over a poll-only listener
- [ ] EP-4 M2: the server-wide LISTEN connection with refcounted channels, debounce, reconnect, and re-`LISTEN`; notify-sourced updates arrive; killing the listener backend does not wedge the feed
- [ ] EP-4 M3: Haddock protocol documentation, design note 021 WebSocket section, changelog
- [ ] EP-5 M1: the `pgmq-inspect` executable serves the surface from a database URL; CORS origins and bind address configurable from the command line
- [ ] EP-5 M2: `GET /openapi.json` serves the programmatically built document; golden and route-coverage tests green
- [ ] EP-5 M3: `docs/user/queue-inspection.md`, README section, capability records, `IR-3` completed, release handoff recorded


## Surprises & Discoveries

EP-3 M1/M2 (2026-10-20 UTC): the new HTTP package and scoped server pass 59 tests,
including non-destructive browsing, exact/capped parsing, metadata-contextual errors,
safe diagnostics, shared nonblocking admission, statement deadlines on separate
inspection capacity, prefix upgrades and startup/cleanup. Nix wiring and the HTTP
contract documents remain before EP-3 closes. Continuous execution between children
is explicitly authorized by the user.

EP-2 M1 (2026-10-20 UTC): a concrete Pair helper both increased allocation and
lost nested record encoding order. Shared polymorphic KeyValue fields preserve
one field definition while generating Value or Series directly; this keeps the
planned wire fields/order. Fifty 1 MiB encodes allocate 55,710,944 bytes in the
library, and 100,000 small encodes use about 46% less allocation than the Pair
prototype. Exact fixtures above JavaScript's safe integer range are now available
in `pgmq-core/test/golden/` for EP-5. Null-header normalization is explicit, and
all record encoders have toJSON/toEncoding parity checks. Final full suites pass 72/111/70 tests. Design note 020, CAP-11, changelogs,
IR-2 closure, source distributions and the boundary ADR are complete. The final
Nix gate passes on aarch64-darwin, including core golden tests with aeson 2.2.4.1
(Cabal uses 2.2.5.1). Golden suites now require tasty-golden 2.3.6's public
NoCreateFile option, so a missing fixture fails instead of silently being created.
EP-2 is complete and EP-3 is ready.

EP-1 M4 implementation (2026-10-20 UTC): real pg_partman fixtures confirm that
numeric archive late pages scan the remaining default (50,001 rows), while time
live/archive pages and lookups scan all 100,000 rows. Ordinary/unlogged and numeric
live reads use msg_id indexes. Complete plans for both schemas are retained in
`docs/design/evidence/019-inspection-reads/README.md`. This validates the existing
EP-3 database-budget and isolation gates; no interface or ownership change is needed.
Late producer commits and late archival demonstrably require first-page refresh.
Both required-partman targeted inspection runs passed all 19 cases without skips.
Full hasql runs passed natively (107), on stock 1.12 (106), and with required
pg_partman on both schemas (107/106); all 79 effectful tests and `cabal build all`
passed. EP-1 is complete; EP-2 owns the next implementation step. Mori registry
lookup is still unavailable because its local database is missing.

Review on 2026-10-19, against pgmq-hs `48a1ed0` and
`mori://shinzui/keiro-ui` commit `3472a9be2e3bc8fdb793e5549fe074450b44b1a7`:
Mori registry list/search/docs failed because the local `mori` database is absent. The
keiro-ui manifest, conventions, ADR-1 through ADR-5, and foundation MasterPlan were read
from its local checkout; canonical handles retain the manifest's namespace/bundle identity,
but live Mori resolution was not verified. That checkout contains planning documents, not
an implemented browser client, so compatibility is with its declared contract, not a tested UI.

The original MasterPlan and all five children were authored by `claude-fable-5-1` with no
recorded reviews. EP-1 and this MasterPlan also carry implementation revisions by
`claude-opus-5-5`. Historical test claims below are preserved, not represented as reruns.

The review found required changes, in descending operational impact. EP-4 planned metrics
queries per connection and a 100 ms debounce: 100 tabs watching one queue could perform
100 full queue scans per poll and up to about 1000 per second under sustained hints.
The coordinator above removes the client multiplier, but each metrics query still scans rows.

EP-1's implemented keyset SQL is read-only and uses unprepared table-specific statements,
but its partition test covers only a small numeric-partitioned live queue. Time-partitioned
parents index the timestamp, not `msg_id`; numeric archives also lack a guaranteed msg_id
index. On disposable PostgreSQL 17.11 with the native migrations and 100,000 rows, an ordinary
queue's late 50-row page used its primary-key index (7 buffer hits, 0.022 ms). A synthetic
parent reproducing the time-partitioned columns and timestamp-only index scanned 100,000
rows (99,950 filtered, 1140 buffer hits, 3.754 ms); its lookup filtered 99,999 rows. These are
warm-cache diagnostic observations, not production benchmarks or a real pg_partman test.

EP-3's `TR.decimal :: ... Int64` parser wrapped `18446744073709551617` to `1` in a direct
GHC check. The new parse contract uses `Integer` before range checks. Its non-destructive
HTTP test incorrectly expected read_ct zero from `pgmq.read`; the native SQL returned one.
Its raw database error details could also contain nested SQL or payloads despite filtering
keys named `sql` and `parameters`.

EP-2's round-trip property fails for `headers = Just Null`, whose encoding is identical to
`Nothing`. EP-5 omitted OpenAPI servers (which means origin root, losing a mounted prefix),
replaced the readiness 503 response with a generic error envelope, and restricted headers to
objects despite the domain accepting any JSON. The revised children add behavioral checks
for these cases and for exact 64-bit browser cursors. EP-4's snapshot race, stale refreshes,
listener cancellation cleanup, starvation, and nondeterministic overflow test also need the
explicit concurrency acceptance now recorded in that child.

Document cross-plan insights, dependency changes, scope adjustments, or unexpected
interactions between child plans. Provide concise evidence.

- EP-1 landed before EP-2, so under the `ArchivedMessage` integration rule the codec now
  belongs to EP-2: `ArchivedMessage` exists in `pgmq-core/src/Pgmq/Types.hs` with no JSON
  instances, and EP-2 must add them (flattened `Message` fields plus `archived_at`) with a
  golden file. Evidence: the M1 integration grep in ExecPlan 27 printed nothing on 2026-10-01.
- EP-1 took capability handle `CAP-10` (`docs/capabilities/non-destructive-queue-inspection.md`);
  EP-2's `okf id next` will now return `CAP-11`.
- `IR-1` was at `status: accepted` (not `proposed`, as EP-1's plan text said) when EP-1
  closed it; the closure is unaffected.
- Consumers that import `Pgmq` or `Pgmq.Effectful` unqualified without
  `DuplicateRecordFields` and use `messageId` as a function now see it ambiguous between
  `Message` and `LookupMessage`, as it already was with `MessageQuery`. EP-3's handlers
  should construct the records positionally or use qualified imports.


## Decision Log

- Decision (2026-10-20 UTC): At the user’s explicit request, continue automatically between remaining child ExecPlans, while retaining milestone validation and commits.

- Decision (2026-10-20 UTC): Close EP-2 after the three full suites, strict
  missing-fixture and incompatible-rename failures, packaging audits and final
  Nix gate pass. Preserve numeric MessageId tokens and normalize null headers,
  as planned; EP-5 inherits the committed exact-int64 fixtures. EP-3 must use
  the owning domain instances, while defining only its own surface envelopes.

- Decision (2026-10-20 UTC): Close EP-1 after real layout evidence and full
  validation rather than adding automatic indexes. Preserve EP-2 codec ownership,
  EP-3 deadline/admission ownership and all existing public read interfaces.
  Rationale: M4 confirms the existing operational limits; no decomposition or
  dependency change is required. Promote measured scan and moving-view constraints
  into the boundary ADR, leaving exact timings in the evidence report.

- Decision (2026-10-19): Keep the five functional work streams and their acyclic dependencies;
  reopen only EP-1's missing M4 evidence. EP-2 owns codecs now that the type exists. EP-3 owns
  runtime admission and configuration validation; EP-4 owns shared refresh scheduling; EP-5
  owns executable/prefix/client/schema acceptance. Rationale: these corrections fit existing
  ownership boundaries and prevent the same budget or interface being defined twice.
- Decision (2026-10-19): A row limit is not a query-work bound. Require partition cost evidence,
  finite database budgets and isolated capacity, without automatic index DDL or upstream SQL
  overrides. Any optional index recommendation requires measurements including write/storage
  cost. Record this and the shared polling contract in the boundary ADR.
- Decision (2026-10-19): Preserve numeric library encodings and keiro-ui's native-position
  convention, with explicit lossless browser parsing; freeze only after cross-surface fixture
  checks. Document moving-view pagination: concurrent late commits or archival of older IDs
  may be missed until a client restarts at the first page; this is not a replay feed.

- Decision: Name the sister package `pgmq-inspect`, not the request's working name
  `pgmq-metrics`. Rationale: metrics are one of seven endpoint groups; the second audience (a
  pgmq-hs user without keiro) should find the package by what it does; the cross-project
  conventions themselves suggest `pgmq-inspect` as an example; `pgmq-ui` stays free for a
  future browser package. Date: 2026-09-30
- Decision: The package runs its handlers through a host-supplied runner over the `Pgmq`
  effect (`InspectEnv.runInspection`) instead of over hasql sessions directly. Rationale: the
  host chooses the plain or traced interpreter without the package knowing about tracing
  configuration; the router is testable against a mock interpreter without a database; the 503
  mapping reuses `isTransient` from `pgmq-effectful` instead of re-deriving the classification;
  the cost is a dependency on `pgmq-effectful` and transitively on `effectful-core` and
  `hs-opentelemetry-api`, which the shibuya precedent already accepts. Date: 2026-09-30
- Decision: Inspection reads take the queue name as plain `Text` and resolve the physical table
  through `pgmq.format_table_name` on the server before running an `unpreparable` `SELECT` with
  the quoted identifier. Rationale: `IR-1` requires the reads to work for names
  `parseQueueName` rejects; upstream's function is the single source of truth for lowercasing;
  the extra round-trip is inside one session and acceptable for an inspection path; an
  unprepared statement keeps the per-connection prepared-statement cache from growing with the
  number of queues. Date: 2026-09-30
- Decision: `ArchivedMessage` nests a `Message` and adds `archivedAt`; its JSON encoding is
  flattened. Rationale: the decoder is `messageDecoder` plus one column; the wire shape is what
  a browser wants (one flat object); `pgmq-core` does not enable `DuplicateRecordFields`, so a
  flat Haskell record would need eight prefixed field names. Date: 2026-09-30
- Decision: JSON field names are pgmq's SQL column names, hand-written, `Maybe` as explicit
  `null`, published-is-frozen. Rationale: the conventions let pgmq-hs keep its own vocabulary,
  the column names already distinguish `queue_length` from `queue_visible_length`, hand-written
  instances cannot be moved by a deriving option, and explicit nulls keep the set of keys
  constant for clients. Date: 2026-09-30
- Decision: `CorsPolicy` has no wildcard constructor and never allows credentials. Rationale:
  the conventions forbid wildcard-with-credentials and require disabled-by-default; making both
  unrepresentable is cheaper than documenting a footgun; an operator who wants a development
  shortcut lists `http://localhost:<port>`. Date: 2026-09-30
- Decision: The WebSocket feed carries queue metrics and a `source` tag, not message bodies.
  Rationale: NOTIFY carries no payload and is throttled and lossy (design note 015), so the
  only honest live signal is "this queue changed; here are its metrics now"; the client re-reads
  messages through the paged HTTP reads, which keeps the HTTP path authoritative exactly as the
  conventions require. Date: 2026-09-30
- Decision: Names that fail `parseQueueName` are watchable but poll-only (`"push":false`).
  Rationale: `notifyChannelName` is typed over `QueueName` and design note 015 warns against
  re-deriving the channel formula elsewhere; foreign queues still converge through the poll
  path, which is the authoritative one anyway. Date: 2026-09-30
- Decision: `EP-5` ships a standalone executable, a served and golden-pinned OpenAPI 3
  document, and a pgmq-hs-owned user guide, as the concrete form of keiro-independence.
  Rationale: a user without keiro needs something to run, something to generate a client from,
  and somewhere to read the contract that is not another project's conventions document;
  `mori://shinzui/keiro-ui/okf/adrs/concepts/ADR-5` anticipates regenerating its planned client from a published
  spec. Date: 2026-09-30
- Decision: The HTTP surface (`EP-3`) and the WebSocket feed (`EP-4`) are separate plans with
  a hard dependency. Rationale: different verification (HTTP client versus WebSocket client
  plus a backend kill), and the HTTP surface is shippable alone; the LISTEN loop is the riskiest
  piece and deserves its own fault test and retrospective. Date: 2026-09-30
- Decision: No plan bumps a version; every plan writes `Unreleased` changelog sections and the
  family's next lockstep release (prepared by MasterPlan 6's plan 25 or its successor) carries
  them, including the new package. Rationale: the family releases in lockstep and a release
  preparation is already in flight; the `Pgmq` effect's new constructors make that release
  major regardless. Date: 2026-09-30
- Decision: Design-note numbers 019, 020, and 021 are pre-assigned to `EP-1`, `EP-2`, and
  `EP-3`; capability handles are allocated with `okf id next` at writing time. Rationale: the
  first wave runs in parallel and must not collide on numbers; the ADR guide forbids deriving
  handles by counting files. Date: 2026-09-30
- Decision: The ADR recording the package boundary, keiro-independence, the lenient-name read
  contract, and the wire-freeze rule is written with this MasterPlan rather than by a child
  plan. Rationale: the decisions are taken now and every child plan obeys them; the FIFO ADR
  set the precedent of recording a decision at planning time with implementation tracked by
  the MasterPlan. Date: 2026-09-30


## Outcomes & Retrospective

EP-2 completed (2026-10-20 UTC): thirteen encoders and four record decoders are
implemented with 53 reviewed goldens, arbitrary JSON header normalization, exact
int64 fixtures and encoder parity. The three full suites pass 72/111/70 tests;
build, Haddock, source-distribution audits, docs/format checks and the final
host Nix gate pass. Design note 020 and the boundary ADR distill the contract;
CAP-11 and IR-2 completion are recorded. Two of five child plans are complete.
EP-3's HTTP package is the next ready child; EP-4/EP-5 keep their existing dependencies.

EP-1 completed (2026-10-20 UTC): all four milestones are complete, including
real numeric/time live/archive plans, deterministic moving-view scenarios and
consumer failure propagation. Full hasql suites pass on native and stock schemas
with and without required pg_partman; both effectful interpreter witnesses pass.
The boundary ADR now distills the scan-cost and cursor lessons; 64 full EXPLAINs
are retained beside design note 019. At that milestone EP-2 was next; its
completion above now makes EP-3 ready. The HTTP/feed/server work remains unimplemented.

Summarize outcomes, gaps, and lessons learned at major milestones or at completion.
Compare the result against the original vision. Before marking the MasterPlan complete,
distill durable project context from this MasterPlan and its child ExecPlans into
docs/adr/. Keep task-local execution and coordination details here.

Review outcome (2026-10-19): changes requested for implementation acceptance. The existing
four inspection reads plus lenient metrics are wired consistently through sessions, effects,
and both interpreters. M1–M3's functional delivery remains recorded; the broad performance
claim is not yet established. This update changes planning/architecture documentation only.
At that review, EP-2 could proceed and EP-3 waited for EP-1 M4 and EP-2. The remaining functional, schema,
concurrency, and workload gates must pass before declaring the surface ready for keiro-ui.

Validation (2026-10-19): `just docs-check` and `git diff --check` passed. The SQL
EXPLAIN/read-counter probes and bounded-integer parser probe above were run; the full Haskell
and actual pg_partman suites were not rerun for this documentation-only update. Mori registry
lookup failed because its local database is missing, while repository-local `mori validate`
passed as part of docs-check. Dependency excerpts are historical; future implementation must
verify current authoritative package releases/tags before selecting bounds or workarounds.

Revision note (2026-10-19): audited the implemented reads and all five child designs against
keiro-ui's contract, corrected API/schema/test errors, and added partition cost evidence,
shared metrics scheduling, bounded admission, exact cursor handling, and release gates.
No package versions, dependency pins, migration bytes, or runtime implementation changed.

Revision note (2026-10-20 UTC): closed EP-1 M4 with real partition query evidence,
moving-view/failure-propagation tests and full supported-schema validation.
EP-2 is next; the remaining ownership and dependency graph are unchanged.

Revision note (2026-10-20 UTC): completed EP-2 with thirteen domain encoders,
four record decoders, 53 strict goldens, numeric/null boundary fixtures and all
required gates. EP-3 is next; the remaining dependency graph is unchanged.
