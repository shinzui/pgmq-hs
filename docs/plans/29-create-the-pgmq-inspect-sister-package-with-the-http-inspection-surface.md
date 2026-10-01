---
id: 29
slug: create-the-pgmq-inspect-sister-package-with-the-http-inspection-surface
title: "Create the pgmq-inspect sister package with the HTTP inspection surface"
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

# Create the pgmq-inspect sister package with the HTTP inspection surface

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.
If durable project context changes, update or create ADRs in docs/adr/ in the same change.


## Purpose / Big Picture

Today nothing outside a Haskell process can see a pgmq queue through pgmq-hs. The repository
is libraries only: no executable, no HTTP server, and no `wai`, `warp`, or `websockets` anywhere
in its dependency closure. A browser, a dashboard, or a shell script that wants to know how deep
a queue is, what its oldest message looks like, or which topic patterns route into it has exactly
one option, which is to query the `pgmq.*` tables directly, and that is the schema-boundary
violation every project around pgmq-hs forbids.

After this plan a new package, `pgmq-inspect`, turns the library's read-only operations into an
HTTP surface. It exports a WAI `Application` (a plain Haskell value any WAI-compatible server
such as Warp can serve, and that a host can mount beside other surfaces in one process), a
runner that binds a host-chosen address and port, and configuration for CORS and paging. With
a server running against a database that holds one queue called `orders`, this works:

```bash
curl -s http://127.0.0.1:9092/queues
```

```json
[{"queue_name":"orders","is_partitioned":false,"is_unlogged":false,"created_at":"2026-10-01T00:20:11.412Z"}]
```

and so does `GET /queues/orders/metrics`, `GET /queues/orders/messages?limit=10`,
`GET /queues/orders/messages/7`, `GET /queues/orders/archive`, `GET /queues/orders/bindings`,
`GET /metrics`, `GET /bindings`, `GET /routing/test?routing_key=orders.created`,
`GET /notify/throttles`, `GET /health/live`, and `GET /health/ready`. Browsing a queue over HTTP
leaves its consumers undisturbed: the message routes call the non-destructive reads that
`docs/plans/27-add-non-destructive-peek-archive-and-lookup-reads-across-the-pgmq-layers.md`
adds, so `vt` and `read_ct` never move. Every body is produced by the JSON instances that
`docs/plans/28-provide-stable-json-codecs-for-the-inspection-facing-records.md` adds, so the
package invents no encoding of a `pgmq-core` or `pgmq-hasql` record. Errors come back as a
structured envelope with a machine-readable code, and a database that is temporarily unreachable
answers 503 rather than 500 because the mapping reuses the library's own `isTransient` classifier.

The WebSocket live feed is not in this plan. The combined application leaves a seam for it, a
`WS.ServerApp` that rejects every upgrade with a JSON error whose code is
`websocket_unavailable`, and
`docs/plans/30-add-the-notify-accelerated-poll-authoritative-websocket-live-feed-to-pgmq-inspect.md`
replaces the seam without changing any signature this plan exports. The standalone executable,
the OpenAPI document, and the user guide are
`docs/plans/31-ship-the-standalone-pgmq-inspect-server-the-openapi-document-and-the-inspection-guide.md`.


## Progress

- [ ] M1: `pgmq-inspect/pgmq-inspect.cabal`, `LICENSE`, `CHANGELOG.md`, and the `src/Pgmq/Inspect/` tree exist; `cabal.project` lists the package; `nix develop --command cabal build pgmq-inspect` succeeds
- [ ] M1: `Pgmq.Inspect.Config`, `Pgmq.Inspect.Env`, `Pgmq.Inspect.Wire`, and `Pgmq.Inspect.Error` complete with Haddocks; `WireGoldenSpec` pins every wire type under `pgmq-inspect/test/golden/`
- [ ] M1: `Pgmq.Inspect.Http` serves `/`, `/queues`, `/queues/{queue}`, `/queues/{queue}/metrics`, `/metrics`, `/health/live`, `/health/ready`, 404 and 405; `routeTable` exported; `RouterSpec` green against the mock interpreter with no database
- [ ] M2: message, archive, by-id, bindings, routing-test, and throttle routes served; `from`/`limit`/`routing_key` parsing with 400 codes; `limit + 1` paging with `next_cursor`
- [ ] M2: `Pgmq.Inspect.Cors` middleware; `CorsSpec` green (no headers when disabled, headers for a listed origin, none for an unlisted one, preflight answered)
- [ ] M2: `HttpSpec` green against ephemeral PostgreSQL, including the end-to-end non-destructive check and the IR-3 acceptance 1 listing; `ErrorSpec` green (503, 404 from 42P01, 500)
- [ ] M3: `Pgmq.Inspect.Server` with `withInspectApplication`, `withInspectApplicationWith`, `rejectingWebSocketApp`, `withInspectServer`, `runInspectServer`, `RunningInspectServer`; the `["ws"]` route dispatches the seam and answers 426 to a plain GET
- [ ] M3: `nix/haskell-overlay.nix`, `flake.module.nix`, `justfile`, `mori.dhall`, README package table updated; `nix build .#pgmq-inspect` and `mori validate` succeed
- [ ] M3: `docs/design/021-inspection-surface-wire-contract.md` written (HTTP half, WebSocket heading left for plan 30); capability record allocated with `okf id next` and indexed; `just docs-check` green; root and package changelogs carry an `Unreleased` section


## Surprises & Discoveries

Document unexpected behaviors, bugs, optimizations, or insights discovered during
implementation. Provide concise evidence.

(None yet.)


## Decision Log

- Decision: Handlers run through `InspectEnv.runInspection`, a host-supplied runner over the
  `Pgmq` effect, rather than over `Hasql.Session` values and a `Pool`.
  Rationale: the host picks `runPgmq` or `runPgmqTraced` without this package knowing about
  tracers; the router is testable against a mock interpreter with no database (`RouterSpec`);
  the 503 mapping is one call to `isTransient` instead of a re-derived classification. The
  MasterPlan records the same decision; this plan inherits it.
  Date: 2026-09-30
- Decision: Error bodies render the hasql error through `Hasql.Errors.toMessage` into
  `message` and through `Hasql.Errors.toDetails` into `details`, with the `sql` and
  `parameters` keys removed.
  Rationale: `message` must be a human sentence safe to display verbatim and `details`
  structured context; `show` on a `StatementSessionError` would put the statement text and
  every parameter into a browser-facing body, which the traced interpreter already refuses to
  do for span status descriptions. The `code` field carries the SQLSTATE in `details` when the
  server reported one.
  Date: 2026-09-30
- Decision: A non-numeric or negative `{msg_id}` path segment answers 400 `invalid_message_id`,
  added to the code vocabulary this plan owns.
  Rationale: a message id that cannot be an id is a malformed request, not a missing message;
  `message_not_found` is reserved for a well-formed id the queue does not hold. The design note
  this plan writes is the vocabulary's source of truth.
  Date: 2026-09-30
- Decision: An `Origin` that is not in the configured list is served normally with no CORS
  headers (`corsIgnoreFailures = True`), not rejected with wai-cors's default 400.
  Rationale: the browser blocks the response either way; a non-browser client that happens to
  send an `Origin` header must keep working; "no headers" is also what the conventions describe
  for the unconfigured case, so the two behaviours agree.
  Date: 2026-09-30
- Decision: `limit` above `maxPageLimit` is capped silently; a non-integer or non-positive
  `limit` is 400 `invalid_limit`.
  Rationale: a client asking for too much is served the most the server allows, which is what
  every paging UI expects; a client sending nonsense is told so.
  Date: 2026-09-30
- Decision: `withInspectApplication` is continuation-shaped even though this plan allocates no
  resource inside it.
  Rationale: plan 30 allocates the LISTEN connection and the WebSocket connection registry
  inside it and must release them when the host's continuation returns; fixing the shape now
  means plan 30 changes no exported signature.
  Date: 2026-09-30
- Decision: Only `GET` is served; `HEAD` and everything else answer 405.
  Rationale: the surface is read-only by design; `OPTIONS` preflight is answered by the CORS
  middleware before the router sees it, and a bare `OPTIONS` with no CORS configuration has no
  meaning here.
  Date: 2026-09-30


## Outcomes & Retrospective

Summarize outcomes, gaps, and lessons learned at major milestones or at completion.
Compare the result against the original purpose. Before marking the plan complete,
distill durable project context from the Decision Log, Surprises & Discoveries, and
this section into docs/adr/. Keep task-local execution details here.

(To be filled during and after implementation.)


## Context and Orientation

### The repository and its toolchain

pgmq-hs is a multi-package Cabal project: `pgmq-core` (types), `pgmq-hasql` (statements and
sessions over the `hasql` PostgreSQL driver), `pgmq-effectful` (an `effectful` effect with a
plain and an OpenTelemetry-traced interpreter), `pgmq-migration` (the schema as a `pg-migrate`
component), `pgmq-config` (declarative queue reconciliation), and `pgmq-bench` (internal
benchmarks). `cabal.project` lists them under `packages:` and pins `hasql`, `hasql-pool`,
`hasql-transaction`, and `hs-opentelemetry` to git revisions. The toolchain comes from Nix:
run every build and test inside `nix develop` (GHC 9.12.4, cabal, PostgreSQL, HLS), run
`nix fmt` before every commit (the pre-commit hook otherwise fails and reformats, which means
re-staging and re-committing), and expect `nix flake check` to build every library and the test
suites listed in `flake.module.nix`. Three environment facts cost time if forgotten: GNU `sed`
shadows BSD `sed` in the dev shell, so in-place edits are `sed -i -e '…' file` and never
`sed -i '' …`; the session scratchpad path is too long for a PostgreSQL Unix socket, so a scratch
server must listen on TCP; and the test suites pin their temporary PostgreSQL clusters to
`/tmp/ephpg-pgmq-hs-<uid>` so a killed run can be reaped by the next.

Tests across the family use `tasty` with `tasty-hunit` and start a disposable PostgreSQL with
`ephemeral-pg` (see `pgmq-effectful/test/EphemeralDb.hs`, which installs the native schema
through `pgmq-migration`'s `pgmqMigrations` component and hands back a `Hasql.Pool.Pool` and
the `EphemeralPg.Database` handle whose `connectionSettings` are hasql connection settings).
Each package keeps its own copy of that helper deliberately; there is no shared test library.

Haskell style across the family: `GHC2024`, the warnings block you will copy from
`pgmq-hasql/pgmq-hasql.cabal`, `fourmolu` formatting (driven by `nix fmt`), strict record
fields, `DuplicateRecordFields` with `OverloadedLabels` and `generic-lens` (`view #field`,
`^. #field`) for field access, and never `OverloadedRecordDot` or `NoFieldSelectors`
(`pgmq-effectful` enables them as an exception; this package follows `pgmq-hasql`).

### What a WAI application is, and the two precedents

WAI (Web Application Interface) is the standard Haskell interface between web servers and
web applications. An `Application` from `Network.Wai` is a function
`Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived`; a `Middleware` is
`Application -> Application`. Warp (`Network.Wai.Handler.Warp`) serves an `Application` on a
socket. Because an `Application` is a value, a host can mount several of them under path
prefixes inside one server; that is what makes this package embeddable rather than
process-owning.

Two sister packages in sibling projects set the pattern this plan follows. kiroku's
`kiroku-metrics` (read `/Users/shinzui/Keikaku/bokuno/kiroku-project/kiroku/kiroku-metrics/src/Kiroku/Metrics/Server.hs`
and `…/Config.hs`) builds one `Application` with `websocketsOr` routing WebSocket upgrades to a
`WS.ServerApp` seam and everything else to an HTTP router that pattern-matches on
`pathInfo`; its config record carries the port and feature switches; its server start uses
`Warp.openFreePort` plus `Warp.runSettingsSocket` when the configured port is `0`, so tests
get an OS-assigned port, and `Warp.runSettings` otherwise; it runs Warp in an `Async` and
stops it with `cancel`. shibuya's `shibuya-metrics` (read
`/Users/shinzui/Keikaku/bokuno/shibuya-project/shibuya/shibuya-metrics/src/Shibuya/Metrics/Server.hs`
and `…/Config.hs`) does the same with a `host` field defaulting to loopback and a
`validateConfig` step, and returns 404 bodies as `{"error":"…"}`. Both are published and their
shapes are frozen; this package starts fresh and uses the structured envelope below. Neither
precedent sets CORS headers, which this package must.

### The `Pgmq` effect and its interpreters

`pgmq-effectful/src/Pgmq/Effectful/Effect.hs` defines `data Pgmq :: Effect` as a GADT with one
constructor per library operation (`ListQueuesUnvalidated :: Pgmq m [UnvalidatedQueue]`,
`AllQueueMetrics :: Pgmq m [QueueMetrics]`, `ListTopicBindings :: Pgmq m [TopicBinding]`,
`ListNotifyInsertThrottles :: Pgmq m [NotifyInsertThrottle]`,
`TestRouting :: RoutingKey -> Pgmq m [RoutingMatch]`, and so on) and a smart constructor per
operation (`listQueuesUnvalidated :: (Pgmq :> es) => Eff es [UnvalidatedQueue]`). Dispatch is
dynamic, so an interpreter is any handler for the GADT. Two ship:

```haskell
runPgmq :: (IOE :> es, Error PgmqRuntimeError :> es) => Pool -> Eff (Pgmq : es) a -> Eff es a
runPgmqTraced :: (IOE :> es, Error PgmqRuntimeError :> es) => Pool -> OTel.Tracer -> Eff (Pgmq : es) a -> Eff es a
```

Both surface failures through the `Error` effect as `PgmqRuntimeError`
(`pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs`), whose three constructors mirror
`hasql-pool`'s `UsageError`: `PgmqAcquisitionTimeout`, `PgmqConnectionError ConnectionError`,
and `PgmqSessionError SessionError`. Running a program is
`runEff . runError @PgmqRuntimeError . runPgmq pool`, which yields
`IO (Either (CallStack, PgmqRuntimeError) a)`. The umbrella `Pgmq.Effectful` re-exports the
effect, both interpreters, the error type, `fromUsageError`, and `isTransient`.

`isTransient :: PgmqRuntimeError -> Bool` answers "is this worth retrying". It is `True` for
pool acquisition timeouts, networking and unrecognized connection errors, session-level
connection drops, and server statement errors whose SQLSTATE names a transient condition
(`40001`, `40P01`, `55P03`, `57P01`, `57P02`, `57P03`, class `53`); everything else is
permanent. Its policy note is
[docs/design/017-transient-error-classification.md](../design/017-transient-error-classification.md),
and `docs/plans/26-classify-postgresql-disconnects-surfaced-as-statement-errors-as-transient.md`
is widening it for lost replies; this plan calls the function and needs no change whichever
lands first. The SQLSTATE lives at a fixed position, which this plan's classifier also reads:

```haskell
PgmqSessionError
  (HasqlErrors.StatementSessionError _total _index _sql _params _prepared
    (HasqlErrors.ServerStatementError (HasqlErrors.ServerError code _message _detail _hint _position)))
```

`Hasql.Errors` (in the pinned hasql) also exports a class `IsError` with
`toMessage :: a -> Text` (a human message with no dynamic details) and
`toDetails :: a -> [(Text, Text)]` (the dynamic details as key-value pairs), with instances
for `ConnectionError` and `SessionError`; `StatementSessionError`'s details include
`totalStatements`, `statementIndex`, `sql`, `parameters`, `prepared`, and then the inner
error's pairs (`code`, `message`, `detail`, `hint`, `position` for a server error). This plan's
error bodies use those two functions and drop the `sql` and `parameters` keys.

### The lenient listing and the reads this plan calls

The server's only check on a queue name is its length, so a queue created by another client
can carry a name that `Pgmq.Types.parseQueueName` rejects (uppercase, hyphens). The typed
`listQueues` fails to decode such a row; `listQueuesUnvalidated` returns
`UnvalidatedQueue { unvalidatedName :: Text, unvalidatedCreatedAt, unvalidatedIsPartitioned, unvalidatedIsUnlogged }`
for every row. An inspection surface must show what exists, so every listing and every
per-queue route here takes the name as text and uses the lenient operations. Design note
[docs/design/016-queue-name-validation.md](../design/016-queue-name-validation.md) explains
why mixed-case names are hazardous and why the library still shows them.

The reads below are added by `docs/plans/27-…` and must exist before this plan starts. Verify
with these commands from the repository root; every grep must print at least one line:

```bash
grep -n "data ArchivedMessage" pgmq-core/src/Pgmq/Types.hs
grep -n "data PeekMessages\|data LookupMessage" pgmq-hasql/src/Pgmq/Hasql/Statements/Types.hs
grep -n "^peekMessages\|^peekArchivedMessages\|^lookupMessage\|^lookupArchivedMessage\|^queueMetricsUnvalidated" pgmq-hasql/src/Pgmq/Hasql/Sessions.hs
grep -n "PeekMessages ::\|PeekArchivedMessages ::\|LookupMessage ::\|LookupArchivedMessage ::\|QueueMetricsUnvalidated ::" pgmq-effectful/src/Pgmq/Effectful/Effect.hs
```

The names this plan assumes, with their types:

```haskell
-- pgmq-core, Pgmq.Types
data ArchivedMessage = ArchivedMessage { archivedMessage :: !Message, archivedAt :: !UTCTime }

-- pgmq-hasql, Pgmq.Hasql.Statements.Types
data PeekMessages = PeekMessages { unvalidatedQueueName :: !Text, afterMessageId :: !(Maybe MessageId), limit :: !Int32 }
data LookupMessage = LookupMessage { unvalidatedQueueName :: !Text, messageId :: !MessageId }

-- pgmq-effectful, Pgmq.Effectful.Effect (smart constructors; GADT constructors have the same names capitalised)
peekMessages :: (Pgmq :> es) => PeekMessages -> Eff es (Vector Message)
peekArchivedMessages :: (Pgmq :> es) => PeekMessages -> Eff es (Vector ArchivedMessage)
lookupMessage :: (Pgmq :> es) => LookupMessage -> Eff es (Maybe Message)
lookupArchivedMessage :: (Pgmq :> es) => LookupMessage -> Eff es (Maybe ArchivedMessage)
queueMetricsUnvalidated :: (Pgmq :> es) => Text -> Eff es QueueMetrics
```

The page reads return at most `limit` rows ordered by `msg_id`, strictly after `afterMessageId`
when it is given, and never use `OFFSET`. A missing queue surfaces as the server's SQLSTATE
`42P01` (`undefined_table`) inside a `PgmqSessionError`; a missing message is `Nothing`.
`queueMetricsUnvalidated` is `queueMetrics` taking the name as text; if the last grep above
prints nothing because plan 27 named it differently, use the name it chose, and if plan 27 did
not add a lenient metrics read at all, add one in `pgmq-hasql` following `queueMetrics` in
`pgmq-hasql/src/Pgmq/Hasql/Statements/QueueObservability.hs` with `E.param (E.nonNullable E.text)`
as the encoder (the SQL is identical; `pgmq.metrics` already takes `TEXT`), wire it through
`Pgmq.Hasql.Sessions`, the `Pgmq` effect, and both interpreters exactly as plan 27 wired the
other four, and record that in Surprises & Discoveries.

### The JSON instances this plan relies on

`docs/plans/28-…` adds hand-written `ToJSON` instances whose field names are pgmq's SQL column
names in snake_case, with `Maybe` fields always present as `null`, timestamps in aeson's
default ISO-8601 UTC form, and the policy recorded in
`docs/design/020-json-wire-encodings.md`. Verify before starting:

```bash
grep -n "instance ToJSON Queue\b\|instance ToJSON UnvalidatedQueue\|instance ToJSON Message\b\|instance ToJSON ArchivedMessage\|instance ToJSON TopicBinding\|instance ToJSON RoutingMatch\|instance ToJSON NotifyInsertThrottle" pgmq-core/src/Pgmq/Types.hs
grep -n "instance ToJSON QueueMetrics" pgmq-hasql/src/Pgmq/Hasql/Statements/Types.hs
ls docs/design/020-json-wire-encodings.md
```

The shapes, which this plan's tests assert through the HTTP layer and which the design note
this plan writes repeats:

```json
{"queue_name":"orders","is_partitioned":false,"is_unlogged":false,"created_at":"2026-10-01T00:20:11.412Z"}
{"msg_id":7,"read_ct":0,"enqueued_at":"…","last_read_at":null,"vt":"…","message":{"id":1},"headers":null}
{"msg_id":7,"read_ct":1,"enqueued_at":"…","last_read_at":"…","vt":"…","message":{"id":1},"headers":null,"archived_at":"…"}
{"queue_name":"orders","queue_length":3,"newest_msg_age_sec":1,"oldest_msg_age_sec":9,"total_messages":12,"scrape_time":"…","queue_visible_length":2,"default_partition_length":null}
{"pattern":"orders.*","queue_name":"orders","bound_at":"…","compiled_regex":"^orders\\.[^.]+$"}
{"pattern":"orders.*","queue_name":"orders","compiled_regex":"^orders\\.[^.]+$"}
{"queue_name":"orders","throttle_interval_ms":250,"last_notified_at":"…"}
```

`queue_length` is every row in the queue table and `queue_visible_length` only the rows whose
`vt` has passed; the difference is messages currently leased. Design note
[docs/design/002-queue-visible-length.md](../design/002-queue-visible-length.md) is the
contract, and the metrics route documentation must say which is which.
`default_partition_length` is `null` on PGMQ 1.12 and for ordinary queues on 1.13, per
[docs/adr/pgmq-1.12-1.13-compatibility.md](../adr/pgmq-1.12-1.13-compatibility.md); `null`
must not be rendered as zero.

### The web libraries this plan adds

All are in the pinned `ghc9124` Nix package set and none is marked broken: `wai` 3.2.4,
`warp` 3.4.9, `wai-cors` 0.2.7, `wai-websockets` 3.0.1.2, `websockets` 0.13.0.0,
`http-types` 0.12.4, `http-client` 0.7.19 (tests only), `tasty-golden` 2.3.6 (tests only),
`async` 2.2.6, `stm`. The APIs this plan uses, verified against those versions:

`Network.Wai.Middleware.Cors.cors :: (Request -> Maybe CorsResourcePolicy) -> Middleware`,
with `simpleCorsResourcePolicy :: CorsResourcePolicy` whose fields are
`corsOrigins :: Maybe ([Origin], Bool)` (`Nothing` means any origin; the `Bool` allows
credentials), `corsMethods :: [Method]`, `corsRequestHeaders :: [HeaderName]`,
`corsExposedHeaders`, `corsMaxAge`, `corsVaryOrigin :: Bool`, `corsRequireOrigin :: Bool`,
and `corsIgnoreFailures :: Bool`. `type Origin = ByteString`. A request with no `Origin`
header passes through untouched. A request whose `Origin` is not in the list is answered with
a 400 unless `corsIgnoreFailures` is `True`, in which case it is served with no CORS headers.
An `OPTIONS` request with a listed origin is answered by the middleware itself with status 200
and the `Access-Control-Allow-*` headers. The middleware never touches a WebSocket upgrade.

`Network.Wai.Handler.WebSockets.websocketsApp :: WS.ConnectionOptions -> WS.ServerApp -> Request -> Maybe Response`
returns `Just` a raw response when the request carries `Upgrade: websocket` and `Nothing`
otherwise; `websocketsOr` is the same wrapped around a fallback application. This plan calls
`websocketsApp` from inside the router's `["ws"]` case so the upgrade is matched on `pathInfo`
(which a host that mounts the application under a prefix rewrites) rather than on the raw
request path (which it may not).

`Network.WebSockets.rejectRequestWith :: PendingConnection -> RejectRequest -> IO ()` with
`defaultRejectRequest { rejectCode :: Int, rejectMessage :: ByteString, rejectHeaders :: Headers, rejectBody :: ByteString }`
answers an upgrade with an ordinary HTTP response. The seam uses it to send a 503 JSON body.

`Network.Wai.Handler.Warp.openFreePort :: IO (Port, Socket)` binds an OS-assigned port on
loopback; `runSettingsSocket :: Settings -> Socket -> Application -> IO ()` serves on it;
`setHost (fromString "127.0.0.1")` and `setPort` configure `defaultSettings` for the ordinary
path.

`Test.Tasty.Golden.goldenVsString :: TestName -> FilePath -> IO ByteString -> TestTree`
compares a lazy `ByteString` with a file; when the file does not exist the test creates it and
passes with the message "Golden file did not exist; created", so the first run writes the
fixtures and you inspect and commit them.

### ADRs and decisions this plan obeys

- [docs/adr/queue-inspection-surface-boundary-and-wire-contract.md](../adr/queue-inspection-surface-boundary-and-wire-contract.md)
  is the record this initiative wrote: the surface is a sister package named `pgmq-inspect`
  that is runnable on its own; handlers run through a host-supplied runner over the `Pgmq`
  effect; pgmq-hs owns and documents its wire contract and freezes published shapes; no
  mutations and no authentication in the first iteration.
- [docs/adr/haskell-dependency-bounds-and-nix-pin-policy.md](../adr/haskell-dependency-bounds-and-nix-pin-policy.md):
  a Cabal bound states compatibility and the Nix pin states what is tested; the new package's
  bounds follow the family (`pgmq-* >=0.6 && <0.7`), the web libraries get caret bounds at the
  pinned versions, and the package is added to the Nix overlay and the flake checks so the pin
  exercises it. The same ADR records that the effect layer's test suite does not run under
  `nix flake check`; this package's test suite, which depends on `ephemeral-pg` like the others,
  is `dontCheck` in the overlay for the same reason and runs under `cabal test`.
- [docs/adr/pgmq-1.12-1.13-compatibility.md](../adr/pgmq-1.12-1.13-compatibility.md): the
  nullable metric and the rule that metrics tests get their own disposable database because
  `metrics_all()` enumerates metadata and then queries queue tables, so a concurrent drop can
  raise `42P01`. `HttpSpec`'s all-queue metrics case therefore runs on its own `withPgmqDb`.
- Design notes [013](../design/013-pgmq-effectful-error-model.md) and
  [017](../design/017-transient-error-classification.md) (the error model and classifier the
  503 mapping reuses), [015](../design/015-notification-delivery-contract.md) (NOTIFY is a
  wake-up hint; the WebSocket plan carries that contract, and the HTTP routes are its
  authoritative poll path), [016](../design/016-queue-name-validation.md) (the lenient path),
  and [002](../design/002-queue-visible-length.md) (total versus visible depth).

Cross-repository decisions, cited by the canonical handles the registry returns: queue-level
views belong to pgmq-hs (`mori://shinzui/keiro-ui/okf/adrs/concepts/ADR-1`); inspection
surfaces live in sister packages exporting an embeddable `Application` and a runner
(`mori://shinzui/keiro-ui/okf/adrs/concepts/ADR-4`); there is no backend-for-frontend, so the
browser calls this surface directly and will regenerate its client from a published spec
(`mori://shinzui/keiro-ui/okf/adrs/concepts/ADR-5`). keiro will mount this application under a
per-surface prefix such as `/pgmq/…` and route WebSocket upgrades through the same prefix
(`mori://shinzui/keiro/okf/improvement-requests/concepts/IR-31`), which is why routing is on
`pathInfo` and the upgrade is dispatched from the router. The conventions this surface adopts
(snake_case fields, exclusive-cursor paging with `next_cursor` omitted on the last page, the
error envelope, CORS disabled by default with an explicit origin list, the trusted-network
auth posture) are `mori://shinzui/keiro-ui`, path
`docs/architecture/inspection-api-conventions.md` (artifact-level URI pending); this plan
restates every rule it follows so the implementer never needs that document.

The request this plan implements the HTTP half of is
`docs/improvement-requests/add-a-pgmq-metrics-sister-package-with-http-and-websocket-inspection-endpoints.md`
(`IR-3`). Its acceptance items 1 (queue listing over HTTP), 2 (metrics label total versus
visible, all-queue cost documented), 3 (browsing leaves consumers undisturbed), 5 (CORS off by
default, on for a configured origin), and 6 (no library package gains a web dependency; the
bare `Application` is exported) are this plan's; item 4 (the WebSocket feed) is plan 30's.
Plan 31 marks the request `completed`; this plan does not touch its frontmatter.


## Plan of Work

### Milestone 1: the package builds and routes without a database

Scope: the package skeleton, the four foundation modules, the router with the routes that need
no path parameters beyond a queue name, and the mock-interpreter tests. At the end,
`nix develop --command cabal build pgmq-inspect` succeeds, `WireGoldenSpec` and `RouterSpec`
pass with no PostgreSQL running, and the package appears in `cabal.project`.

Create `pgmq-inspect/` with `pgmq-inspect.cabal` (full text in Concrete Steps), `LICENSE`
copied from `pgmq-hasql/LICENSE`, `CHANGELOG.md` with an `Unreleased` section, and
`src/Pgmq/Inspect/`. Add `pgmq-inspect` to the `packages:` list in `cabal.project`.

Write `Pgmq.Inspect.Config`: `CorsPolicy`, `InspectConfig`, `InspectServerConfig`, and the two
defaults. The poll and WebSocket fields are consumed only by plan 30 but are defined here so
that plan needs no configuration change; their Haddocks say so. Write `Pgmq.Inspect.Env`: the
`Inspection` alias, `InspectEnv`, `runnerFromInterpreter`, `poolEnv`, and `tracedPoolEnv`.
Write `Pgmq.Inspect.Wire`: `Page`, `ServiceDescriptor`, `LivenessBody`, `DependencyCheck`,
`ReadinessBody`, each with a hand-written `ToJSON`. Write `Pgmq.Inspect.Error`: `ErrorBody`,
its `ToJSON`, `errorResponse`, the named constructors for every code, `sqlState`,
`describeRuntimeError`, and `classifyRuntimeError`.

Write `Pgmq.Inspect.Http` with `PathSegment`, `routeTable`, `httpApp`, and the handlers for
`/`, `/queues`, `/queues/{queue}`, `/queues/{queue}/metrics`, `/metrics`, `/health/live`,
`/health/ready`, plus the 404 and 405 fallbacks. Leave the other routes as entries in
`routeTable` whose handlers answer 404 `not_found` until Milestone 2, so the table is complete
from the first commit.

Write the test suite's `EphemeralDb.hs` (a copy of `pgmq-effectful/test/EphemeralDb.hs` with
this package's header comment), `TestServer.hs`, `WireGoldenSpec.hs`, `RouterSpec.hs`, and
`Main.hs`. `RouterSpec` builds an `InspectEnv` whose runner interprets the `Pgmq` effect with
canned values and never opens a connection; it proves the descriptor, the listing, the
per-queue metrics, the all-queue metrics, the 404 for an unknown path, the 405 for `POST`, and
the `limit` validation, all without a database. Run `cabal test` with `--test-options=--accept`
once to create the golden files, inspect them, and commit.

Acceptance: `nix develop --command cabal build pgmq-inspect` prints `Build profile` lines and
no errors; `nix develop --command cabal test pgmq-inspect:pgmq-inspect-test` reports every
`RouterSpec` and `WireGoldenSpec` case `OK`; `git status` shows the new golden files.

### Milestone 2: every HTTP route, CORS, and the database-backed tests

Scope: the message, archive, by-id, bindings, routing-test, and throttle handlers; query
parsing; the CORS middleware; and the three database-backed specs. At the end every route in
`routeTable` is served and the `IR-3` acceptance items this plan owns are green.

Add to `Pgmq.Inspect.Http` the `parsePage` and `parseMessageId` helpers and the handlers for
`/queues/{queue}/messages`, `/queues/{queue}/messages/{msg_id}`, `/queues/{queue}/archive`,
`/queues/{queue}/archive/{msg_id}`, `/queues/{queue}/bindings`, `/bindings`, `/routing/test`,
and `/notify/throttles`. The page handlers ask the read for `limit + 1` rows, return the first
`limit`, and set `next_cursor` to the last returned `msg_id` only when the extra row existed.
The per-queue bindings handler lists every binding and keeps those whose `bindingQueueName`
equals the path segment, so foreign names work. The routing-test handler parses
`routing_key` with `parseRoutingKey` and answers 400 `invalid_routing_key` on failure.

Write `Pgmq.Inspect.Cors` with `corsMiddleware :: CorsPolicy -> Middleware`. Write
`CorsSpec.hs`, `HttpSpec.hs`, and `ErrorSpec.hs`. `HttpSpec` starts one ephemeral database
for the suite, creates queues with random names through the pool, and exercises every route;
its all-queue metrics case starts its own database. Its central case is the end-to-end
non-destructive guarantee: send five messages, page them twice over HTTP, fetch one by id, then
`readMessage` all five through the pool and assert every `readCount` is `0` and every message is
still returned. `ErrorSpec` proves the three outcomes of `classifyRuntimeError` through the
HTTP layer: a pool whose connection string points at `host=127.0.0.1 port=1` answers 503
`database_unavailable`; a mock interpreter throwing a `42P01` server error answers 404
`queue_not_found`; a mock interpreter throwing a `23505` server error answers 500
`database_error`.

Acceptance: `nix develop --command cabal test pgmq-inspect:pgmq-inspect-test` reports every
case `OK`, including `HttpSpec`'s "browsing over HTTP leaves read_ct at zero" and `CorsSpec`'s
three cases.

### Milestone 3: the mountable application, the runner, the wiring, and the documents

Scope: `Pgmq.Inspect.Server`, the `["ws"]` route with the rejecting seam, the Nix, registry,
and README wiring, design note 021, the capability record, and the changelogs. At the end
`nix build .#pgmq-inspect` succeeds, `mori validate` and `just docs-check` are green, and a
host can call `withInspectApplication` to get an `Application` or `runInspectServer` to serve it.

Write `Pgmq.Inspect.Server` and the umbrella `Pgmq.Inspect`. Add the `["ws"]` case to the
router: dispatch `websocketsApp WS.defaultConnectionOptions wsApp req`; when it returns
`Nothing` (a plain `GET`), answer 426 `websocket_upgrade_required`. Make `httpApp` take the
`WS.ServerApp`, and have `withInspectApplicationWith` pass it through. Switch `TestServer` to
`withInspectServer` with `bindPort = 0`. Add a `ServerSpec` case: a plain `GET /ws` answers 426
with the JSON envelope, and an upgrade request (`Connection: Upgrade`, `Upgrade: websocket`,
`Sec-WebSocket-Key`, `Sec-WebSocket-Version: 13`) answers 503 with code `websocket_unavailable`.

Wire the package: `nix/haskell-overlay.nix`, `flake.module.nix` (packages and the library
check), `justfile` (`nix-build`), `mori.dhall` (package entry and bundle membership), the README
package table and a short section after `## pgmq-config`. Run `nix build .#pgmq-inspect`; if it
fails inside `wai-websockets` with a `wai-app-static` build error, add the executable-dependency
override quoted in Concrete Steps. Run `mori validate`.

Write `docs/design/021-inspection-surface-wire-contract.md` and the capability record (handle
from `okf id next`), update `docs/capabilities/index.md`, append to `docs/capabilities/log.md`
with `okf log add`, and run `just docs-check`. Add the `Unreleased` section to the root
`CHANGELOG.md`.

Acceptance: `nix build .#pgmq-inspect` prints a store path; `mori validate` reports the
manifest valid; `just docs-check` exits 0; `ServerSpec` is green; `git grep -n "pgmq-inspect" README.md cabal.project flake.module.nix nix/haskell-overlay.nix mori.dhall justfile`
prints a line for each file.


## Concrete Steps

All commands run from the repository root
`/Users/shinzui/Keikaku/bokuno/libraries/pgmq-hs-project/pgmq-hs` inside `nix develop`
(prefix each with `nix develop --command` or enter the shell once).

### M1.1 Verify the dependencies on plans 27 and 28

Run the greps from Context and Orientation. If any prints nothing, stop and resolve it (plan
27 or 28 is not complete, or named something differently; adapt the names in this plan's code
and record the difference in Surprises & Discoveries).

### M1.2 The package skeleton

```bash
mkdir -p pgmq-inspect/src/Pgmq/Inspect pgmq-inspect/test/golden
cp pgmq-hasql/LICENSE pgmq-inspect/LICENSE
```

`pgmq-inspect/pgmq-inspect.cabal`, complete:

```cabal
cabal-version: 3.4
name: pgmq-inspect
version: 0.1.0.0
synopsis: Embeddable HTTP and WebSocket inspection surface for PGMQ queues
description:
  A sister package to pgmq-hs: an embeddable WAI application and a convenience
  runner that expose read-only queue inspection over HTTP and WebSocket. Queue
  listings, total and visible depth metrics, non-destructive message and archive
  browsing, message lookup by id, topic bindings, routing tests, notification
  throttles, and health probes, with configurable CORS. The core pgmq-hs
  libraries gain no web dependency; this package depends on them, never the
  other way around.

homepage: https://github.com/shinzui/pgmq-hs
license: MIT
license-file: LICENSE
author: Nadeem Bitar
maintainer: Nadeem Bitar
category: Database, Web
build-type: Simple
extra-doc-files: CHANGELOG.md
extra-source-files: test/golden/*.json

common warnings
  ghc-options:
    -Wall
    -Wcompat
    -Widentities
    -Wincomplete-uni-patterns
    -Wincomplete-record-updates
    -Wredundant-constraints
    -fhide-source-paths
    -Wmissing-export-lists
    -Wpartial-fields
    -Wmissing-deriving-strategies

library
  import: warnings
  exposed-modules:
    Pgmq.Inspect
    Pgmq.Inspect.Config
    Pgmq.Inspect.Cors
    Pgmq.Inspect.Env
    Pgmq.Inspect.Error
    Pgmq.Inspect.Http
    Pgmq.Inspect.Server
    Pgmq.Inspect.Wire

  other-modules: Paths_pgmq_inspect
  autogen-modules: Paths_pgmq_inspect
  default-extensions:
    DataKinds
    DeriveGeneric
    DuplicateRecordFields
    GeneralisedNewtypeDeriving
    ImportQualifiedPost
    LambdaCase
    NamedFieldPuns
    OverloadedLabels
    OverloadedStrings
    TypeOperators

  build-depends:
    aeson ^>=2.2,
    async ^>=2.2,
    base >=4.18 && <5,
    bytestring >=0.11 && <0.13,
    containers >=0.6 && <0.8,
    effectful-core ^>=2.6 || ^>=2.7,
    generic-lens ^>=2.2 || ^>=2.3,
    hasql ^>=1.10,
    hasql-pool ^>=1.4,
    hs-opentelemetry-api >=1.0 && <2,
    http-types ^>=0.12,
    lens ^>=5.3,
    pgmq-core >=0.6 && <0.7,
    pgmq-effectful >=0.6 && <0.7,
    pgmq-hasql >=0.6 && <0.7,
    stm >=2.5 && <2.6,
    text ^>=2.1,
    time ^>=1.14,
    vector ^>=0.13,
    wai ^>=3.2,
    wai-cors ^>=0.2.7,
    wai-websockets ^>=3.0,
    warp ^>=3.4,
    websockets ^>=0.13,

  hs-source-dirs: src
  default-language: GHC2024

test-suite pgmq-inspect-test
  import: warnings
  default-language: GHC2024
  type: exitcode-stdio-1.0
  hs-source-dirs: test
  main-is: Main.hs
  ghc-options:
    -threaded
    -rtsopts
    -with-rtsopts=-N

  other-modules:
    CorsSpec
    EphemeralDb
    ErrorSpec
    HttpSpec
    RouterSpec
    ServerSpec
    TestServer
    WireGoldenSpec

  default-extensions:
    DataKinds
    DuplicateRecordFields
    ImportQualifiedPost
    LambdaCase
    NamedFieldPuns
    OverloadedLabels
    OverloadedStrings
    TypeOperators

  build-depends:
    aeson,
    async,
    base >=4.18 && <5,
    bytestring,
    directory,
    effectful-core ^>=2.6 || ^>=2.7,
    ephemeral-pg >=0.3.1.0,
    generic-lens,
    hasql,
    hasql-pool ^>=1.4,
    http-client ^>=0.7,
    http-types,
    lens,
    pg-migrate,
    pgmq-core >=0.6 && <0.7,
    pgmq-effectful >=0.6 && <0.7,
    pgmq-hasql >=0.6 && <0.7,
    pgmq-inspect,
    pgmq-migration >=0.6 && <0.7,
    random ^>=1.2,
    tasty ^>=1.5,
    tasty-golden ^>=2.3,
    tasty-hunit ^>=0.10,
    text,
    time,
    unix,
    vector,
    wai,
    warp,
    websockets,
```

`ServerSpec` is listed from the start; until Milestone 3 it is a module exporting an empty
`testGroup`. Add the package to `cabal.project`:

```text
packages:
  pgmq-core
  pgmq-hasql
  pgmq-effectful
  pgmq-migration
  pgmq-config
  pgmq-inspect
  pgmq-bench
```

`pgmq-inspect/CHANGELOG.md`:

```markdown
# Revision history for pgmq-inspect

## Unreleased

Initial release: an embeddable WAI `Application` and a Warp runner exposing read-only queue
inspection over HTTP. Routes: the service descriptor, queue listing and lookup, per-queue and
all-queue metrics, non-destructive message and archive paging and lookup by id, topic bindings,
routing tests, notification throttles, and liveness and readiness probes. Errors use a
structured envelope with machine-readable codes; CORS is configurable with an explicit
allowed-origins list and disabled by default. The WebSocket endpoint rejects every upgrade in
this version.
```

### M1.3 `Pgmq.Inspect.Config`

`pgmq-inspect/src/Pgmq/Inspect/Config.hs`:

```haskell
-- | Configuration for the inspection surface and for the convenience runner.
--
-- The surface itself is configured by 'InspectConfig'; where it is served
-- from is 'InspectServerConfig'. A host that mounts the 'Network.Wai.Application'
-- into its own server needs only the former.
module Pgmq.Inspect.Config
  ( CorsPolicy (..),
    InspectConfig (..),
    defaultInspectConfig,
    InspectServerConfig (..),
    defaultInspectServerConfig,
  )
where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

-- | Which browser origins may call the surface cross-origin.
--
-- There is deliberately no \"any origin\" constructor and credentials are
-- never allowed: the browser forbids a wildcard origin together with
-- credentials, and an inspection surface that assumes a trusted network has
-- no business opting into either. List the origins you serve a browser UI
-- from, for example @CorsAllowOrigins ("http://localhost:5173" :| [])@.
data CorsPolicy
  = -- | Send no CORS headers at all. The default.
    CorsDisabled
  | -- | Answer requests whose @Origin@ header is exactly one of these with the
    -- CORS headers that let the page read the response; serve any other origin
    -- normally but without those headers.
    CorsAllowOrigins !(NonEmpty ByteString)
  deriving stock (Eq, Show, Generic)

-- | Behaviour of the surface. See 'defaultInspectConfig' for the defaults.
data InspectConfig = InspectConfig
  { corsPolicy :: !CorsPolicy,
    -- | Page size when a request omits @limit@.
    defaultPageLimit :: !Int32,
    -- | Largest page a request may ask for; larger values are capped to this.
    maxPageLimit :: !Int32,
    -- | Budget for the readiness probe's round trip, in microseconds.
    readinessTimeoutUs :: !Int,
    -- | WebSocket feed (see "Pgmq.Inspect.WebSocket"): how often each subscribed
    -- queue is polled for metrics, in microseconds.
    pollIntervalUs :: !Int,
    -- | WebSocket feed: how long to coalesce insert notifications for one
    -- queue before reading its metrics, in microseconds.
    notifyDebounceUs :: !Int,
    -- | WebSocket feed: concurrent connections accepted before rejecting.
    wsMaxConnections :: !Int,
    -- | WebSocket feed: queues one connection may watch at once.
    wsMaxSubscriptions :: !Int,
    -- | WebSocket feed: frames buffered per connection before the oldest are
    -- dropped and an in-band overflow error is sent.
    wsQueueCapacity :: !Natural
  }
  deriving stock (Eq, Show, Generic)

-- | CORS disabled, pages of 50 capped at 500, a one-second readiness budget,
-- a five-second poll, a 100 ms notify debounce, 100 connections, 100
-- subscriptions per connection, 256 buffered frames.
defaultInspectConfig :: InspectConfig
defaultInspectConfig =
  InspectConfig
    { corsPolicy = CorsDisabled,
      defaultPageLimit = 50,
      maxPageLimit = 500,
      readinessTimeoutUs = 1_000_000,
      pollIntervalUs = 5_000_000,
      notifyDebounceUs = 100_000,
      wsMaxConnections = 100,
      wsMaxSubscriptions = 100,
      wsQueueCapacity = 256
    }

-- | Where the convenience runner listens.
data InspectServerConfig = InspectServerConfig
  { -- | Interface to bind. Loopback by default; bind @*@ only behind an
    -- authenticating reverse proxy or on a trusted network.
    bindHost :: !String,
    -- | Port to bind. @0@ asks the operating system for a free port on loopback
    -- (ignoring 'bindHost'), which is what the tests use.
    bindPort :: !Int,
    inspect :: !InspectConfig
  }
  deriving stock (Eq, Show, Generic)

-- | @127.0.0.1:9092@ with 'defaultInspectConfig'. shibuya-metrics defaults to
-- 9090 and kiroku-metrics to 9091, so the three surfaces coexist on one host.
defaultInspectServerConfig :: InspectServerConfig
defaultInspectServerConfig =
  InspectServerConfig {bindHost = "127.0.0.1", bindPort = 9092, inspect = defaultInspectConfig}
```

### M1.4 `Pgmq.Inspect.Env`

`pgmq-inspect/src/Pgmq/Inspect/Env.hs`:

```haskell
-- | How the surface reaches the database: a runner for the @Pgmq@ effect,
-- supplied by the host, plus the connection settings the WebSocket feed's
-- listener uses.
module Pgmq.Inspect.Env
  ( Inspection,
    InspectEnv (..),
    runnerFromInterpreter,
    poolEnv,
    tracedPoolEnv,
  )
where

import Effectful (Eff, IOE, runEff)
import Effectful.Error.Static (Error, runError)
import Hasql.Connection.Settings (Settings)
import Hasql.Pool (Pool)
import OpenTelemetry.Trace.Core qualified as OTel
import Pgmq.Effectful (Pgmq, PgmqRuntimeError, runPgmq, runPgmqTraced)

-- | A database program the surface wants run: every handler is one of these.
type Inspection a = Eff '[Pgmq, Error PgmqRuntimeError, IOE] a

-- | The host's side of the contract.
data InspectEnv = InspectEnv
  { -- | Run one program. Which interpreter does so is the host's choice
    -- ('poolEnv' for the plain one, 'tracedPoolEnv' for the OpenTelemetry one,
    -- or anything else, including a mock for tests).
    runInspection :: forall a. Inspection a -> IO (Either PgmqRuntimeError a),
    -- | Settings for the dedicated connection the WebSocket feed LISTENs on
    -- (see "Pgmq.Inspect.WebSocket"). 'Nothing' makes the feed poll-only.
    listenerSettings :: !(Maybe Settings)
  }

-- | Turn an interpreter of the @Pgmq@ effect into a runner, discarding the
-- call stack that 'runError' pairs with the error.
runnerFromInterpreter ::
  (forall a. Eff '[Pgmq, Error PgmqRuntimeError, IOE] a -> Eff '[Error PgmqRuntimeError, IOE] a) ->
  (forall a. Inspection a -> IO (Either PgmqRuntimeError a))
runnerFromInterpreter interpretPgmq action =
  either (Left . snd) Right <$> runEff (runError @PgmqRuntimeError (interpretPgmq action))

-- | The plain interpreter over a connection pool.
poolEnv :: Pool -> Maybe Settings -> InspectEnv
poolEnv pool settings =
  InspectEnv
    { runInspection = runnerFromInterpreter (runPgmq pool),
      listenerSettings = settings
    }

-- | The OpenTelemetry-traced interpreter over a connection pool: every
-- handler's database work becomes a span under the host's tracer.
tracedPoolEnv :: Pool -> OTel.Tracer -> Maybe Settings -> InspectEnv
tracedPoolEnv pool tracer settings =
  InspectEnv
    { runInspection = runnerFromInterpreter (runPgmqTraced pool tracer),
      listenerSettings = settings
    }
```

`RankNTypes` is part of `GHC2024`, so the polymorphic field needs no pragma.

### M1.5 `Pgmq.Inspect.Wire`

`pgmq-inspect/src/Pgmq/Inspect/Wire.hs`:

```haskell
-- | Wire types that are the surface's own (not records of pgmq-core or
-- pgmq-hasql, which carry their own instances). Every encoding here follows
-- the policy in @docs/design/020-json-wire-encodings.md@: snake_case keys,
-- hand-written instances, published shapes frozen. 'Page' is the one place a
-- key may be absent: @next_cursor@ is omitted on the last page by convention.
module Pgmq.Inspect.Wire
  ( Page (..),
    ServiceDescriptor (..),
    LivenessBody (..),
    DependencyCheck (..),
    ReadinessBody (..),
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Pgmq.Types (MessageId)

-- | One page of a keyset-paged read.
data Page a = Page
  { items :: ![a],
    -- | The @msg_id@ to pass back as @from@ for the next page; 'Nothing' on the
    -- last page, in which case the key is omitted from the JSON.
    nextCursor :: !(Maybe MessageId)
  }
  deriving stock (Eq, Show, Generic)

instance (ToJSON a) => ToJSON (Page a) where
  toJSON page =
    object (("items" .= items page) : maybe [] (\c -> ["next_cursor" .= c]) (nextCursor page))

-- | What @GET /@ returns: enough for a client to find the rest.
data ServiceDescriptor = ServiceDescriptor
  { serviceName :: !Text,
    serviceVersion :: !Text,
    -- | Relative to the mount root, so it is right under any prefix.
    websocketPath :: !Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ServiceDescriptor where
  toJSON d =
    object
      [ "service" .= serviceName d,
        "version" .= serviceVersion d,
        "websocket_path" .= websocketPath d
      ]

newtype LivenessBody = LivenessBody {alive :: Bool}
  deriving stock (Eq, Show, Generic)

instance ToJSON LivenessBody where
  toJSON b = object ["alive" .= alive b]

-- | One dependency the readiness probe checked.
data DependencyCheck = DependencyCheck
  { checkName :: !Text,
    checkHealthy :: !Bool,
    checkLatencyMs :: !(Maybe Int64),
    checkError :: !(Maybe Text)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON DependencyCheck where
  toJSON c =
    object
      [ "name" .= checkName c,
        "healthy" .= checkHealthy c,
        "latency_ms" .= checkLatencyMs c,
        "error" .= checkError c
      ]

data ReadinessBody = ReadinessBody
  { ready :: !Bool,
    checks :: ![DependencyCheck]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ReadinessBody where
  toJSON b = object ["ready" .= ready b, "checks" .= checks b]
```

Plan 31 adds `"openapi_path"` to the descriptor; that is an additive field and allowed.

### M1.6 `Pgmq.Inspect.Error`

`pgmq-inspect/src/Pgmq/Inspect/Error.hs`:

```haskell
-- | The error envelope and the mapping from the library's typed runtime error
-- onto HTTP statuses and codes.
--
-- Every error body is @{"error":{"code":…,"message":…,"details":…}}@. @code@
-- is a stable snake_case identifier a client may switch on; @message@ is a
-- human sentence safe to show verbatim and free to change; @details@ is
-- optional structured context. The vocabulary is listed in
-- @docs/design/021-inspection-surface-wire-contract.md@.
module Pgmq.Inspect.Error
  ( ErrorBody (..),
    errorResponse,
    jsonResponse,
    notFound,
    methodNotAllowed,
    queueNotFound,
    messageNotFound,
    invalidCursor,
    invalidLimit,
    invalidMessageId,
    invalidRoutingKey,
    websocketUpgradeRequired,
    websocketUnavailable,
    sqlState,
    describeRuntimeError,
    classifyRuntimeError,
  )
where

import Data.Aeson (ToJSON (..), Value, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hasql.Errors qualified as HasqlErrors
import Network.HTTP.Types (Status, hContentType, status500, status503, status404)
import Network.Wai (Response, responseLBS)
import Pgmq.Effectful (PgmqRuntimeError (..), isTransient)

data ErrorBody = ErrorBody
  { errorCode :: !Text,
    errorMessage :: !Text,
    errorDetails :: !(Maybe Value)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ErrorBody where
  toJSON e =
    object
      [ "error"
          .= object
            ( ["code" .= errorCode e, "message" .= errorMessage e]
                <> maybe [] (\d -> ["details" .= d]) (errorDetails e)
            )
      ]

jsonResponse :: Status -> ByteString -> Response
jsonResponse status = responseLBS status [(hContentType, "application/json")]

errorResponse :: Status -> ErrorBody -> Response
errorResponse status = jsonResponse status . encode

notFound, methodNotAllowed, websocketUpgradeRequired, websocketUnavailable :: ErrorBody
notFound = ErrorBody "not_found" "No route matches this path." Nothing
methodNotAllowed = ErrorBody "method_not_allowed" "Only GET is served." Nothing
websocketUpgradeRequired =
  ErrorBody "websocket_upgrade_required" "This path serves WebSocket upgrades only." Nothing
websocketUnavailable =
  ErrorBody "websocket_unavailable" "The WebSocket feed is not available on this server." Nothing

queueNotFound :: Text -> ErrorBody
queueNotFound name =
  ErrorBody "queue_not_found" "No queue with that name exists." (Just (object ["queue_name" .= name]))

messageNotFound :: Text -> Text -> ErrorBody
messageNotFound name raw =
  ErrorBody
    "message_not_found"
    "No message with that id exists in that table."
    (Just (object ["queue_name" .= name, "msg_id" .= raw]))

invalidCursor, invalidLimit, invalidMessageId, invalidRoutingKey :: Text -> ErrorBody
invalidCursor raw =
  ErrorBody "invalid_cursor" "from must be a non-negative integer msg_id." (Just (object ["from" .= raw]))
invalidLimit raw =
  ErrorBody "invalid_limit" "limit must be a positive integer." (Just (object ["limit" .= raw]))
invalidMessageId raw =
  ErrorBody "invalid_message_id" "msg_id must be a non-negative integer." (Just (object ["msg_id" .= raw]))
invalidRoutingKey raw =
  ErrorBody "invalid_routing_key" "routing_key is not a valid pgmq routing key." (Just (object ["routing_key" .= raw]))

-- | The SQLSTATE the server reported, when the error is a server-reported
-- statement error. Mirrors the pattern 'isTransient' reads.
sqlState :: PgmqRuntimeError -> Maybe Text
sqlState = \case
  PgmqSessionError
    ( HasqlErrors.StatementSessionError
        _
        _
        _
        _
        _
        (HasqlErrors.ServerStatementError (HasqlErrors.ServerError code _ _ _ _))
      ) -> Just code
  PgmqSessionError (HasqlErrors.ScriptSessionError _ (HasqlErrors.ServerError code _ _ _ _)) -> Just code
  _ -> Nothing

-- | A human message and structured details for a runtime error, with the
-- statement text and parameters removed from the details so a browser-facing
-- body never carries SQL.
describeRuntimeError :: PgmqRuntimeError -> (Text, Maybe Value)
describeRuntimeError = \case
  PgmqAcquisitionTimeout ->
    ("Timed out waiting for a database connection from the pool.", Nothing)
  PgmqConnectionError e -> (HasqlErrors.toMessage e, detailsValue (HasqlErrors.toDetails e))
  PgmqSessionError e -> (HasqlErrors.toMessage e, detailsValue (HasqlErrors.toDetails e))
  where
    detailsValue pairs =
      case filter (\(k, _) -> k `notElem` ["sql", "parameters"]) pairs of
        [] -> Nothing
        kept -> Just (object [Key.fromText k .= v | (k, v) <- kept])

-- | Status and body for a runtime error: a missing table is a missing queue,
-- a transient failure is a 503 the client may retry, anything else is a 500.
classifyRuntimeError :: PgmqRuntimeError -> (Status, ErrorBody)
classifyRuntimeError err
  | sqlState err == Just "42P01" = (status404, ErrorBody "queue_not_found" "No queue with that name exists." details)
  | isTransient err = (status503, ErrorBody "database_unavailable" retryMessage details)
  | otherwise = (status500, ErrorBody "database_error" message details)
  where
    (message, details) = describeRuntimeError err
    retryMessage = "The database is temporarily unavailable; retry the request."
```

The import of `Pgmq.Effectful (PgmqRuntimeError (..), isTransient)` is the only place the
package depends on the classifier; when plan 26 widens `isTransient`, this mapping follows.

### M1.7 `Pgmq.Inspect.Http` (Milestone 1 subset)

`pgmq-inspect/src/Pgmq/Inspect/Http.hs`. The router matches `pathInfo` (the request path split
on `/` with the leading empty segment removed, so `/queues/orders` is `["queues","orders"]` and
`/` is `[]`). It takes the WebSocket seam from the first commit so Milestone 3 only adds a case.

```haskell
-- | The HTTP router: one case per route, matching on 'pathInfo' so the
-- application behaves identically at the root and under a host's prefix.
module Pgmq.Inspect.Http
  ( PathSegment (..),
    routeTable,
    httpApp,
  )
where

import Control.Lens ((^.))
import Data.Aeson (encode)
import Data.Generics.Labels ()
import Data.Int (Int32, Int64)
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Data.Version (showVersion)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Types
  ( Method,
    Status,
    methodGet,
    status200,
    status400,
    status404,
    status405,
    status426,
    status503,
  )
import Network.Wai (Application, Request, pathInfo, queryString, requestMethod)
import Network.Wai.Handler.WebSockets qualified as WaiWS
import Network.WebSockets qualified as WS
import Paths_pgmq_inspect (version)
import Pgmq.Effectful (QueueMetrics, allQueueMetrics, listNotifyInsertThrottles, listQueuesUnvalidated, listTopicBindings, testRouting)
import Pgmq.Effectful.Effect (lookupArchivedMessage, lookupMessage, peekArchivedMessages, peekMessages, queueMetricsUnvalidated)
import Pgmq.Hasql.Statements.Types (LookupMessage (..), PeekMessages (..))
import Pgmq.Inspect.Config (InspectConfig (..))
import Pgmq.Inspect.Env (InspectEnv (..), Inspection)
import Pgmq.Inspect.Error
import Pgmq.Inspect.Wire
import Pgmq.Types (ArchivedMessage (..), Message, MessageId (..), TopicBinding (..), UnvalidatedQueue (..), parseRoutingKey)
import System.Timeout (timeout)

data PathSegment = Literal !Text | Placeholder !Text
  deriving stock (Eq, Show)

-- | Every route the router serves. Plan 31's OpenAPI coverage test reads it.
routeTable :: [(Method, [PathSegment])]
routeTable =
  [ (methodGet, []),
    (methodGet, [Literal "queues"]),
    (methodGet, [Literal "queues", Placeholder "queue"]),
    (methodGet, [Literal "queues", Placeholder "queue", Literal "metrics"]),
    (methodGet, [Literal "queues", Placeholder "queue", Literal "messages"]),
    (methodGet, [Literal "queues", Placeholder "queue", Literal "messages", Placeholder "msg_id"]),
    (methodGet, [Literal "queues", Placeholder "queue", Literal "archive"]),
    (methodGet, [Literal "queues", Placeholder "queue", Literal "archive", Placeholder "msg_id"]),
    (methodGet, [Literal "queues", Placeholder "queue", Literal "bindings"]),
    (methodGet, [Literal "metrics"]),
    (methodGet, [Literal "bindings"]),
    (methodGet, [Literal "routing", Literal "test"]),
    (methodGet, [Literal "notify", Literal "throttles"]),
    (methodGet, [Literal "health", Literal "live"]),
    (methodGet, [Literal "health", Literal "ready"]),
    (methodGet, [Literal "ws"])
  ]

httpApp :: InspectConfig -> InspectEnv -> WS.ServerApp -> Application
httpApp cfg env wsApp req respond
  | requestMethod req /= methodGet = respond (errorResponse status405 methodNotAllowed)
  | otherwise = respond =<< route
  where
    route = case pathInfo req of
      [] -> pure descriptor
      ["queues"] -> listQueuesRoute env
      ["queues", q] -> queueRoute env q
      ["queues", q, "metrics"] -> queueMetricsRoute env q
      ["queues", q, "messages"] -> messagesPageRoute cfg env q req
      ["queues", q, "messages", mid] -> messageRoute env q mid
      ["queues", q, "archive"] -> archivePageRoute cfg env q req
      ["queues", q, "archive", mid] -> archivedMessageRoute env q mid
      ["queues", q, "bindings"] -> queueBindingsRoute env q
      ["metrics"] -> allMetricsRoute env
      ["bindings"] -> allBindingsRoute env
      ["routing", "test"] -> routingTestRoute env req
      ["notify", "throttles"] -> throttlesRoute env
      ["health", "live"] -> pure (jsonResponse status200 (encode (LivenessBody True)))
      ["health", "ready"] -> readinessRoute cfg env
      ["ws"] -> pure (maybe (errorResponse status426 websocketUpgradeRequired) id (WaiWS.websocketsApp WS.defaultConnectionOptions wsApp req))
      _ -> pure (errorResponse status404 notFound)

descriptor :: Response
descriptor =
  jsonResponse status200 . encode $
    ServiceDescriptor
      { serviceName = "pgmq-inspect",
        serviceVersion = T.pack (showVersion version),
        websocketPath = "ws"
      }

-- | Run a program and encode its result, or map its failure.
runJson :: (ToJSON a) => InspectEnv -> Inspection a -> IO Response
runJson env program = runWith env program (jsonResponse status200 . encode)

runWith :: InspectEnv -> Inspection a -> (a -> Response) -> IO Response
runWith env program onOk =
  either (uncurry errorResponse . classifyRuntimeError) onOk <$> runInspection env program

listQueuesRoute :: InspectEnv -> IO Response
listQueuesRoute env = runJson env listQueuesUnvalidated

queueRoute :: InspectEnv -> Text -> IO Response
queueRoute env name =
  runWith env listQueuesUnvalidated $ \queues ->
    case find ((== name) . unvalidatedName) queues of
      Nothing -> errorResponse status404 (queueNotFound name)
      Just q -> jsonResponse status200 (encode q)

queueMetricsRoute :: InspectEnv -> Text -> IO Response
queueMetricsRoute env name = runJson env (queueMetricsUnvalidated name)

allMetricsRoute :: InspectEnv -> IO Response
allMetricsRoute env = runJson env allQueueMetrics

readinessRoute :: InspectConfig -> InspectEnv -> IO Response
readinessRoute cfg env = do
  started <- getMonotonicTimeNSec
  outcome <- timeout (readinessTimeoutUs cfg) (runInspection env listQueuesUnvalidated)
  finished <- getMonotonicTimeNSec
  let latency = Just (fromIntegral ((finished - started) `div` 1_000_000))
      check = case outcome of
        Nothing -> DependencyCheck "postgres" False latency (Just "readiness probe timed out")
        Just (Left err) -> DependencyCheck "postgres" False latency (Just (fst (describeRuntimeError err)))
        Just (Right _) -> DependencyCheck "postgres" True latency Nothing
      body = ReadinessBody {ready = checkHealthy check, checks = [check]}
  pure (jsonResponse (if ready body then status200 else status503) (encode body))
```

Add the `Response` and `ToJSON` imports the snippet implies (`Network.Wai (Response)`,
`Data.Aeson (ToJSON)`). Until Milestone 2, define `messagesPageRoute`, `messageRoute`,
`archivePageRoute`, `archivedMessageRoute`, `queueBindingsRoute`, `allBindingsRoute`,
`routingTestRoute`, and `throttlesRoute` as functions returning
`pure (errorResponse status404 notFound)` with their final types, so the module compiles with
the full route table. The `timeout` around the readiness probe is best-effort: hasql's socket
I/O is not reliably interruptible, which is the subject of
`docs/improvement-requests/bound-pgmq-calls-when-a-response-is-blackholed.md`; the probe's
budget bounds the common case, not a blackholed reply.

### M1.8 The test harness and the Milestone 1 specs

`pgmq-inspect/test/EphemeralDb.hs`: copy `pgmq-effectful/test/EphemeralDb.hs` and replace its
header comment with:

```haskell
-- | Test database infrastructure using ephemeral-pg.
--
-- This is a copy of pgmq-effectful\'s EphemeralDb helper, adjusted for
-- pgmq-inspect\'s test suite. Keeping a local copy avoids cross-package
-- test-helper sharing (which would require a shared test-helper library
-- or fragile cross-package hs-source-dirs). It also exports the
-- 'EphemeralPg.Database' handle so a test can hand its connection settings
-- to the surface as listener settings.
```

and make sure it exports `withPgmqDb :: (Pool.Pool -> Database -> IO a) -> IO (Either StartError a)`
and `withPgmqPool` (the pgmq-hasql copy already has both shapes; take the `withPgmqDb` form
that passes the `Database`).

`pgmq-inspect/test/TestServer.hs`:

```haskell
-- | Start the surface on an OS-assigned loopback port and talk to it with
-- http-client.
module TestServer
  ( withTestServer,
    Reply (..),
    get,
    getWith,
    decodeReply,
  )
where

import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive (CI)
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types (Method, methodGet)
import Network.HTTP.Types.Header (HeaderName)
import Pgmq.Inspect.Config (InspectConfig, InspectServerConfig (..))
import Pgmq.Inspect.Env (InspectEnv)
import Pgmq.Inspect.Server (RunningInspectServer (..), withInspectServer)
import Test.Tasty.HUnit (assertFailure)

withTestServer :: InspectConfig -> InspectEnv -> (Int -> IO a) -> IO a
withTestServer cfg env k =
  withInspectServer InspectServerConfig {bindHost = "127.0.0.1", bindPort = 0, inspect = cfg} env $ \server ->
    k (serverPort server)

data Reply = Reply
  { replyStatus :: !Int,
    replyHeaders :: ![(CI ByteString, ByteString)],
    replyBody :: !LBS.ByteString
  }

get :: Int -> String -> IO Reply
get port = getWith port methodGet []

getWith :: Int -> Method -> [(HeaderName, ByteString)] -> String -> IO Reply
getWith port method headers path = do
  manager <- Http.newManager Http.defaultManagerSettings
  request <- Http.parseRequest ("http://127.0.0.1:" <> show port <> path)
  response <- Http.httpLbs request {Http.method = method, Http.requestHeaders = headers} manager
  pure
    Reply
      { replyStatus = fromEnum (Http.responseStatus response),
        replyHeaders = Http.responseHeaders response,
        replyBody = Http.responseBody response
      }

decodeReply :: (FromJSON a) => Reply -> IO a
decodeReply reply = either (assertFailure . ("undecodable body: " <>)) pure (eitherDecode (replyBody reply))
```

`fromEnum` on a `Status` is its code (`Status` has an `Enum` instance). Until Milestone 3
exists, `withTestServer` may call `withInspectApplication` and start Warp itself; the final
form above is what Milestone 3 leaves. Add `case-insensitive` to the test suite's
`build-depends` if the `CI` import needs it (it is a transitive dependency of `http-types`;
listing it explicitly is harmless).

`pgmq-inspect/test/RouterSpec.hs`, the mock-interpreter spec:

```haskell
module RouterSpec (tests, mockEnv) where

import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Time (UTCTime (..), fromGregorian)
import Effectful (runEff)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (runError)
import Pgmq.Effectful (PgmqRuntimeError)
import Pgmq.Effectful.Effect (Pgmq (..))
import Pgmq.Hasql.Statements.Types (QueueMetrics (..))
import Pgmq.Inspect.Config (defaultInspectConfig)
import Pgmq.Inspect.Env (InspectEnv (..))
import Pgmq.Types (UnvalidatedQueue (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestServer

-- | An environment whose runner never opens a connection: the handler answers
-- the four operations the Milestone 1 routes need and errors on any other.
mockEnv :: InspectEnv
mockEnv = InspectEnv {runInspection = runMock, listenerSettings = Nothing}
  where
    runMock action =
      either (Left . snd) Right
        <$> runEff
          ( runError @PgmqRuntimeError
              ( interpret
                  ( \_ -> \case
                      ListQueuesUnvalidated -> pure [cannedQueue]
                      QueueMetricsUnvalidated name -> pure (cannedMetrics name)
                      AllQueueMetrics -> pure [cannedMetrics "orders"]
                      ListTopicBindings -> pure []
                      _ -> error "RouterSpec: operation not canned"
                  )
                  action
              )
          )
    epoch = UTCTime (fromGregorian 2026 1 1) 0
    cannedQueue = UnvalidatedQueue "orders" epoch False False
    cannedMetrics name = QueueMetrics name 3 (Just 1) (Just 9) 12 epoch 2 Nothing

tests :: TestTree
tests =
  testGroup
    "Router (mock interpreter, no database)"
    [ testCase "GET / describes the service" $
        withTestServer defaultInspectConfig mockEnv $ \port -> do
          reply <- get port "/"
          replyStatus reply @?= 200
          body <- decodeReply reply
          lookupKey "service" body @?= Just (Aeson.String "pgmq-inspect")
          lookupKey "websocket_path" body @?= Just (Aeson.String "ws"),
      testCase "GET /queues lists the canned queue with pgmq column names" $
        withTestServer defaultInspectConfig mockEnv $ \port -> do
          reply <- get port "/queues"
          replyStatus reply @?= 200
          rows <- decodeReply reply
          map (lookupKey "queue_name") rows @?= [Just (Aeson.String "orders")],
      testCase "GET /queues/orders/metrics distinguishes total and visible depth" $
        withTestServer defaultInspectConfig mockEnv $ \port -> do
          body <- decodeReply =<< get port "/queues/orders/metrics"
          lookupKey "queue_length" body @?= Just (Aeson.Number 3)
          lookupKey "queue_visible_length" body @?= Just (Aeson.Number 2)
          lookupKey "default_partition_length" body @?= Just Aeson.Null,
      testCase "an unknown path is 404 not_found" $
        withTestServer defaultInspectConfig mockEnv $ \port -> do
          reply <- get port "/nope"
          replyStatus reply @?= 404
          code <- errorCodeOf reply
          code @?= "not_found",
      testCase "POST is 405 method_not_allowed" $
        withTestServer defaultInspectConfig mockEnv $ \port -> do
          reply <- getWith port "POST" [] "/queues"
          replyStatus reply @?= 405,
      testCase "limit=abc is 400 invalid_limit before any database work" $
        withTestServer defaultInspectConfig mockEnv $ \port -> do
          reply <- get port "/queues/orders/messages?limit=abc"
          replyStatus reply @?= 400
          code <- errorCodeOf reply
          code @?= "invalid_limit"
    ]

lookupKey :: Aeson.Key -> Value -> Maybe Value
lookupKey k = \case
  Aeson.Object o -> Aeson.KeyMap.lookup k o
  _ -> Nothing

errorCodeOf :: Reply -> IO Value
errorCodeOf reply = do
  body <- decodeReply reply
  maybe (fail "no error.code") pure (lookupKey "error" body >>= lookupKey "code")
```

Use `Data.Aeson.KeyMap` qualified as `Aeson.KeyMap`; adjust the `Value` constructor names to
aeson 2.2 (`Aeson.Number`, `Aeson.String`, `Aeson.Null`, `Aeson.Object`). The `limit=abc` case
belongs to Milestone 2's handler; in Milestone 1 it passes trivially only if `parsePage` runs
before the stubbed handler, so write `parsePage` in Milestone 1 and have the stub call it.

`pgmq-inspect/test/WireGoldenSpec.hs`:

```haskell
module WireGoldenSpec (tests) where

import Data.Aeson (encode)
import Pgmq.Inspect.Error (ErrorBody (..), invalidCursor)
import Pgmq.Inspect.Wire
import Pgmq.Types (MessageId (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

tests :: TestTree
tests =
  testGroup
    "Wire shapes (golden)"
    [ goldenVsString "page with a next cursor" "test/golden/page-more.json" $
        pure (encode (Page [1 :: Int, 2, 3] (Just (MessageId 3)))),
      goldenVsString "last page omits next_cursor" "test/golden/page-last.json" $
        pure (encode (Page [4 :: Int] Nothing)),
      goldenVsString "error envelope with details" "test/golden/error-invalid-cursor.json" $
        pure (encode (invalidCursor "x")),
      goldenVsString "error envelope without details" "test/golden/error-plain.json" $
        pure (encode (ErrorBody "not_found" "No route matches this path." Nothing)),
      goldenVsString "service descriptor" "test/golden/descriptor.json" $
        pure (encode (ServiceDescriptor "pgmq-inspect" "0.1.0.0" "ws")),
      goldenVsString "liveness" "test/golden/health-live.json" $
        pure (encode (LivenessBody True)),
      goldenVsString "readiness" "test/golden/health-ready.json" $
        pure (encode (ReadinessBody False [DependencyCheck "postgres" False (Just 12) (Just "timed out")]))
    ]
```

`pgmq-inspect/test/Main.hs` composes the groups; the database-backed groups take the pool and
database handle from one `withPgmqDb` for the suite:

```haskell
module Main (main) where

import CorsSpec qualified
import EphemeralDb (withPgmqDb)
import ErrorSpec qualified
import HttpSpec qualified
import RouterSpec qualified
import ServerSpec qualified
import Test.Tasty (defaultMain, testGroup)
import WireGoldenSpec qualified

main :: IO ()
main = do
  result <- withPgmqDb $ \pool db ->
    defaultMain $
      testGroup
        "pgmq-inspect"
        [ WireGoldenSpec.tests,
          RouterSpec.tests,
          ErrorSpec.tests,
          CorsSpec.tests pool db,
          HttpSpec.tests pool db,
          ServerSpec.tests pool db
        ]
  case result of
    Left err -> error ("Failed to start temp database: " <> show err)
    Right () -> pure ()
```

In Milestone 1 the three database-backed modules export empty groups.

Build and run:

```bash
cabal build pgmq-inspect
cabal test pgmq-inspect:pgmq-inspect-test --test-options=--accept
ls pgmq-inspect/test/golden
cat pgmq-inspect/test/golden/page-more.json
```

Expected tail of the first `cabal test` and the golden file:

```text
  Wire shapes (golden)
    page with a next cursor:          OK
      Golden file did not exist; created
    …
All 13 tests passed (0.41s)
```

```json
{"items":[1,2,3],"next_cursor":3}
```

Run the suite again without `--accept` and confirm it still passes, then commit:

```text
feat(inspect): add the pgmq-inspect package with config, env, wire types, and the router core

Create the sister package with its configuration, the interpreter-agnostic
InspectEnv, the structured error envelope with its mapping from
PgmqRuntimeError, and a pathInfo router serving the descriptor, listings,
metrics, and health probes. Golden tests pin the wire shapes and a mock
interpreter proves routing needs no database.

MasterPlan: docs/masterplans/7-expose-non-destructive-queue-inspection-through-typed-reads-json-codecs-and-the-pgmq-inspect-sister-package.md
ExecPlan: docs/plans/29-create-the-pgmq-inspect-sister-package-with-the-http-inspection-surface.md
Intention: intention_01m3tcw9vmeeftdtqj53d1d6nb
```

### M2.1 Query parsing and the paged routes

Add to `Pgmq.Inspect.Http`:

```haskell
-- | @from@ (exclusive msg_id cursor, optional) and @limit@ (optional, defaulted,
-- capped). Malformed values are rejected before any database work.
parsePage :: InspectConfig -> Request -> Either ErrorBody (Maybe MessageId, Int32)
parsePage cfg req = do
  from <- case queryParam "from" req of
    Nothing -> Right Nothing
    Just raw -> maybe (Left (invalidCursor raw)) (Right . Just . MessageId) (parseNonNegative raw)
  limit <- case queryParam "limit" req of
    Nothing -> Right (defaultPageLimit cfg)
    Just raw -> case parseNonNegative raw of
      Just n | n > 0 -> Right (fromIntegral (min (fromIntegral (maxPageLimit cfg)) n))
      _ -> Left (invalidLimit raw)
  pure (from, limit)

queryParam :: Text -> Request -> Maybe Text
queryParam key req =
  case lookup (TE.encodeUtf8 key) (queryString req) of
    Just (Just raw) -> Just (TE.decodeUtf8Lenient raw)
    Just Nothing -> Just ""
    Nothing -> Nothing

parseNonNegative :: Text -> Maybe Int64
parseNonNegative raw = case TR.decimal raw of
  Right (n, rest) | T.null rest -> Just n
  _ -> Nothing

parseMessageId :: Text -> Either ErrorBody MessageId
parseMessageId raw = maybe (Left (invalidMessageId raw)) (Right . MessageId) (parseNonNegative raw)

-- | Ask for one row more than the page, return the page, and point at the
-- next one only when that extra row existed.
toPage :: Int32 -> (a -> MessageId) -> Vector a -> Page a
toPage limit cursorOf rows =
  let n = fromIntegral limit
      page = V.toList (V.take n rows)
   in Page {items = page, nextCursor = if V.length rows > n then cursorOf <$> lastOf page else Nothing}
  where
    lastOf [] = Nothing
    lastOf xs = Just (last xs)

messagesPageRoute :: InspectConfig -> InspectEnv -> Text -> Request -> IO Response
messagesPageRoute cfg env name req =
  case parsePage cfg req of
    Left bad -> pure (errorResponse status400 bad)
    Right (from, limit) ->
      runWith env (peekMessages PeekMessages {unvalidatedQueueName = name, afterMessageId = from, limit = limit + 1}) $
        jsonResponse status200 . encode . toPage limit (^. #messageId)

archivePageRoute :: InspectConfig -> InspectEnv -> Text -> Request -> IO Response
archivePageRoute cfg env name req =
  case parsePage cfg req of
    Left bad -> pure (errorResponse status400 bad)
    Right (from, limit) ->
      runWith env (peekArchivedMessages PeekMessages {unvalidatedQueueName = name, afterMessageId = from, limit = limit + 1}) $
        jsonResponse status200 . encode . toPage limit (\a -> archivedMessage a ^. #messageId)

messageRoute :: InspectEnv -> Text -> Text -> IO Response
messageRoute env name raw =
  case parseMessageId raw of
    Left bad -> pure (errorResponse status400 bad)
    Right mid ->
      runWith env (lookupMessage LookupMessage {unvalidatedQueueName = name, messageId = mid}) $
        maybe (errorResponse status404 (messageNotFound name raw)) (jsonResponse status200 . encode)

archivedMessageRoute :: InspectEnv -> Text -> Text -> IO Response
archivedMessageRoute env name raw =
  case parseMessageId raw of
    Left bad -> pure (errorResponse status400 bad)
    Right mid ->
      runWith env (lookupArchivedMessage LookupMessage {unvalidatedQueueName = name, messageId = mid}) $
        maybe (errorResponse status404 (messageNotFound name raw)) (jsonResponse status200 . encode)

queueBindingsRoute :: InspectEnv -> Text -> IO Response
queueBindingsRoute env name =
  runWith env listTopicBindings $ \bindings ->
    jsonResponse status200 (encode (filter ((== name) . bindingQueueName) bindings))

allBindingsRoute :: InspectEnv -> IO Response
allBindingsRoute env = runJson env listTopicBindings

routingTestRoute :: InspectEnv -> Request -> IO Response
routingTestRoute env req =
  case queryParam "routing_key" req of
    Nothing -> pure (errorResponse status400 (invalidRoutingKey ""))
    Just raw -> case parseRoutingKey raw of
      Left _ -> pure (errorResponse status400 (invalidRoutingKey raw))
      Right key -> runJson env (testRouting key)

throttlesRoute :: InspectEnv -> IO Response
throttlesRoute env = runJson env listNotifyInsertThrottles
```

`(^. #messageId)` on a `Message` uses `generic-lens`; `Message` derives `Generic`. The
`limit = limit + 1` inside the record update refers to the local binding on the right and the
field on the left, which `DuplicateRecordFields` resolves because the constructor is named.

### M2.2 `Pgmq.Inspect.Cors`

`pgmq-inspect/src/Pgmq/Inspect/Cors.hs`:

```haskell
-- | CORS (Cross-Origin Resource Sharing) support: the browser mechanism that
-- lets a page served from one origin read responses from another. Off by
-- default; on only for an explicit list of origins; never with credentials.
module Pgmq.Inspect.Cors (corsMiddleware) where

import Data.List.NonEmpty qualified as NE
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors
  ( CorsResourcePolicy (..),
    cors,
    simpleCorsResourcePolicy,
  )
import Pgmq.Inspect.Config (CorsPolicy (..))

-- | 'CorsDisabled' adds no headers and intercepts nothing. 'CorsAllowOrigins'
-- answers preflight @OPTIONS@ requests from a listed origin and adds
-- @Access-Control-Allow-Origin@ (echoing the origin, with @Vary: Origin@) to
-- actual responses; a request whose @Origin@ is not listed is served normally
-- with no CORS headers, which the browser then refuses to expose to the page.
corsMiddleware :: CorsPolicy -> Middleware
corsMiddleware CorsDisabled = id
corsMiddleware (CorsAllowOrigins origins) = cors (const (Just policy))
  where
    policy =
      simpleCorsResourcePolicy
        { corsOrigins = Just (NE.toList origins, False),
          corsMethods = ["GET", "OPTIONS"],
          corsRequestHeaders = ["Content-Type"],
          corsVaryOrigin = True,
          corsIgnoreFailures = True
        }
```

### M2.3 The database-backed specs

`pgmq-inspect/test/HttpSpec.hs` (the shape; fill every route the same way). The suite shares
one pool; each case creates a queue with a random name through
`Pgmq.Hasql.Sessions.createQueue` and drops it afterwards. The non-destructive case:

```haskell
testCase "browsing over HTTP leaves read_ct at zero and every message available" $
  withQueue pool $ \queue -> do
    mapM_ (\i -> sendOne pool queue i) [1 .. 5 :: Int]
    withTestServer defaultInspectConfig (poolEnv pool (Just (connectionSettings db))) $ \port -> do
      let base = "/queues/" <> T.unpack (queueNameToText queue)
      first <- decodeReply =<< get port (base <> "/messages?limit=3")
      items first `shouldHaveLength` 3
      Just cursor <- pure (nextCursor first)
      second <- decodeReply =<< get port (base <> "/messages?from=" <> show (unMessageId cursor) <> "&limit=3")
      items second `shouldHaveLength` 2
      nextCursor second @?= Nothing
      one <- get port (base <> "/messages/" <> show (unMessageId cursor))
      replyStatus one @?= 200
    messages <- assertSession pool (Sessions.readMessage ReadMessage {queueName = queue, delay = 0, batchSize = Just 10, conditional = Nothing})
    V.length messages @?= 5
    V.toList (V.map (^. #readCount) messages) @?= [0, 0, 0, 0, 0]
```

where `Page` is decoded through a local `FromJSON` instance in the test (the library offers
only `ToJSON`; the test defines `data PageOf a = PageOf { items :: [a], nextCursor :: Maybe MessageId }`
with a `withObject` parser reading `items` and `next_cursor`). The `readMessage` with
`delay = 0` leases for zero seconds so it changes nothing the assertion depends on; the
assertion is on `readCount` before that read bumps it, which is exactly the value the HTTP
browsing must have left at zero.

The other cases, one `testCase` each: `GET /queues` contains the created queue (IR-3
acceptance 1); `GET /queues/{queue}` returns the row and an unknown name is 404
`queue_not_found`; `GET /queues/{queue}/metrics` after three sends reports `queue_length` 3
and `queue_visible_length` 3, then after one `readMessage` with `delay = 30` reports
`queue_visible_length` 2; `GET /metrics` on a dedicated database (`withPgmqDb` inside the
case) lists both of two created queues; `GET /queues/{queue}/archive` after archiving one
message returns one item carrying `archived_at`, and `GET /queues/{queue}/archive/{id}` returns
it; `GET /queues/{queue}/messages/999999` is 404 `message_not_found`; `…/messages/abc` is 400
`invalid_message_id`; `…/messages?from=-1` is 400 `invalid_cursor`; `…/messages?limit=10000`
returns at most `maxPageLimit` items (send `maxPageLimit + 1` small messages with
`batchSendMessage`); `GET /queues/{queue}/bindings` after `bindTopic` returns the binding and
`GET /bindings` contains it; `GET /routing/test?routing_key=orders.created` returns the match
and `?routing_key=bad key` is 400 `invalid_routing_key`; `GET /notify/throttles` after
`enableNotifyInsert` contains the row with `throttle_interval_ms` 250; `GET /health/ready`
is 200 with `ready` true and a `postgres` check whose `latency_ms` is a number; a queue
created by raw SQL under the name `MixedCase_q` (through `Session.script "select pgmq.create('MixedCase_q')"`,
on a dedicated database because a mixed-case `pgmq.meta` row breaks the typed listing for
every other case) appears in `GET /queues`, answers `GET /queues/MixedCase_q/metrics`, and
pages `GET /queues/MixedCase_q/messages`.

`pgmq-inspect/test/CorsSpec.hs`: three cases over the ephemeral pool and the real router.
With `defaultInspectConfig`, `getWith port methodGet [("Origin", "http://ui.example")] "/queues"`
has no header named `access-control-allow-origin`. With
`defaultInspectConfig {corsPolicy = CorsAllowOrigins ("http://ui.example" :| [])}`, the same
request carries `access-control-allow-origin: http://ui.example` and `vary: Origin`, and
`getWith port "OPTIONS" [("Origin", "http://ui.example"), ("Access-Control-Request-Method", "GET")] "/queues"`
answers 200 with `access-control-allow-methods` containing `GET`; with
`[("Origin", "http://other.example")]` the response is 200 with the body but no
`access-control-allow-origin` header.

`pgmq-inspect/test/ErrorSpec.hs`:

```haskell
testCase "an unreachable database answers 503 database_unavailable" $ do
  badPool <-
    Pool.acquire
      ( PoolConfig.settings
          [ PoolConfig.size 1,
            PoolConfig.staticConnectionSettings "host=127.0.0.1 port=1 user=nobody dbname=nonexistent connect_timeout=1"
          ]
      )
  withTestServer defaultInspectConfig (poolEnv badPool Nothing) $ \port -> do
    reply <- get port "/queues"
    replyStatus reply @?= 503
    code <- errorCodeOf reply
    code @?= "database_unavailable"
  Pool.release badPool
```

and two mock-interpreter cases built like `RouterSpec.mockEnv` but with the handler throwing
`throwError (serverStatementError "42P01")` for `QueueMetricsUnvalidated` (expect 404
`queue_not_found`) and `throwError (serverStatementError "23505")` (expect 500
`database_error` and a `details.code` of `"23505"`), where `serverStatementError` is copied
from `pgmq-effectful/test/ClassificationSpec.hs`:

```haskell
serverStatementError :: Text -> PgmqRuntimeError
serverStatementError code =
  PgmqSessionError
    ( HasqlErrors.StatementSessionError
        1
        0
        "select 1"
        []
        True
        (HasqlErrors.ServerStatementError (HasqlErrors.ServerError code "boom" Nothing Nothing Nothing))
    )
```

Run the suite:

```bash
cabal test pgmq-inspect:pgmq-inspect-test
```

Expected: every case `OK`, including the non-destructive case, and a final line like
`All 41 tests passed (6.83s)`. Commit:

```text
feat(inspect): serve every HTTP inspection route with CORS and the structured errors

Add the paged message and archive routes over the non-destructive reads,
lookup by id, bindings, routing tests, and notification throttles; parse
from, limit, and routing_key with 400 codes; add the explicit-origin CORS
middleware. Database-backed tests prove browsing over HTTP leaves read_ct
untouched and that a transient failure maps to 503.

MasterPlan: docs/masterplans/7-expose-non-destructive-queue-inspection-through-typed-reads-json-codecs-and-the-pgmq-inspect-sister-package.md
ExecPlan: docs/plans/29-create-the-pgmq-inspect-sister-package-with-the-http-inspection-surface.md
Intention: intention_01m3tcw9vmeeftdtqj53d1d6nb
```

### M3.1 `Pgmq.Inspect.Server` and the umbrella

`pgmq-inspect/src/Pgmq/Inspect/Server.hs`:

```haskell
-- | The mountable application and the convenience runner.
--
-- 'withInspectApplication' hands the host a WAI 'Application' to mount
-- anywhere; the host that mounts it under a prefix must strip the prefix
-- from 'Network.Wai.pathInfo' before delegating, as every WAI mounting helper
-- does. 'withInspectServer' and 'runInspectServer' serve it with Warp.
module Pgmq.Inspect.Server
  ( withInspectApplication,
    withInspectApplicationWith,
    rejectingWebSocketApp,
    RunningInspectServer (..),
    withInspectServer,
    runInspectServer,
  )
where

import Control.Concurrent.Async (Async, async, cancel)
import Control.Exception (bracket)
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as LBS
import Data.String (fromString)
import Network.Wai (Application)
import Network.Wai.Handler.Warp qualified as Warp
import Network.WebSockets qualified as WS
import Pgmq.Inspect.Config (InspectConfig (..), InspectServerConfig (..))
import Pgmq.Inspect.Cors (corsMiddleware)
import Pgmq.Inspect.Env (InspectEnv)
import Pgmq.Inspect.Error (websocketUnavailable)
import Pgmq.Inspect.Http (httpApp)

-- | The application with the WebSocket feed unavailable (every upgrade is
-- answered 503 @websocket_unavailable@). Continuation-shaped because the
-- feed, once wired, allocates a LISTEN connection and releases it when the
-- continuation returns.
withInspectApplication :: InspectConfig -> InspectEnv -> (Application -> IO a) -> IO a
withInspectApplication cfg env = withInspectApplicationWith cfg env rejectingWebSocketApp

-- | The application with an explicit WebSocket seam.
withInspectApplicationWith :: InspectConfig -> InspectEnv -> WS.ServerApp -> (Application -> IO a) -> IO a
withInspectApplicationWith cfg env wsApp k =
  k (corsMiddleware (corsPolicy cfg) (httpApp cfg env wsApp))

-- | Answer every upgrade with an ordinary 503 carrying the JSON error envelope.
rejectingWebSocketApp :: WS.ServerApp
rejectingWebSocketApp pending =
  WS.rejectRequestWith
    pending
    WS.defaultRejectRequest
      { WS.rejectCode = 503,
        WS.rejectMessage = "Service Unavailable",
        WS.rejectHeaders = [("Content-Type", "application/json")],
        WS.rejectBody = LBS.toStrict (encode websocketUnavailable)
      }

data RunningInspectServer = RunningInspectServer
  { -- | The bound port; informative when 'bindPort' was @0@.
    serverPort :: !Int,
    serverThread :: !(Async ())
  }

-- | Serve the application with Warp for the duration of the continuation.
withInspectServer :: InspectServerConfig -> InspectEnv -> (RunningInspectServer -> IO a) -> IO a
withInspectServer scfg env k =
  withInspectApplication (inspect scfg) env $ \app ->
    bracket (start app) (cancel . serverThread) k
  where
    start app
      | bindPort scfg == 0 = do
          (port, socket) <- Warp.openFreePort
          thread <- async (Warp.runSettingsSocket (Warp.setPort port Warp.defaultSettings) socket app)
          pure (RunningInspectServer port thread)
      | otherwise = do
          thread <- async (Warp.runSettings (settingsFor scfg) app)
          pure (RunningInspectServer (bindPort scfg) thread)

-- | Serve the application with Warp until killed.
runInspectServer :: InspectServerConfig -> InspectEnv -> IO ()
runInspectServer scfg env =
  withInspectApplication (inspect scfg) env (Warp.runSettings (settingsFor scfg))

settingsFor :: InspectServerConfig -> Warp.Settings
settingsFor scfg =
  Warp.setHost (fromString (bindHost scfg)) (Warp.setPort (bindPort scfg) Warp.defaultSettings)
```

`pgmq-inspect/src/Pgmq/Inspect.hs`:

```haskell
-- | Umbrella re-export: everything a host needs to configure, build, and run
-- the inspection surface. See @docs/user/queue-inspection.md@ for the guide.
module Pgmq.Inspect
  ( module Pgmq.Inspect.Config,
    module Pgmq.Inspect.Env,
    module Pgmq.Inspect.Server,
    module Pgmq.Inspect.Error,
    module Pgmq.Inspect.Wire,
    routeTable,
    PathSegment (..),
  )
where

import Pgmq.Inspect.Config
import Pgmq.Inspect.Env
import Pgmq.Inspect.Error
import Pgmq.Inspect.Http (PathSegment (..), routeTable)
import Pgmq.Inspect.Server
import Pgmq.Inspect.Wire
```

`pgmq-inspect/test/ServerSpec.hs`: two cases. A plain `get port "/ws"` answers 426 with code
`websocket_upgrade_required`. An upgrade attempt sent with
`getWith port methodGet [("Connection","Upgrade"),("Upgrade","websocket"),("Sec-WebSocket-Key","dGhlIHNhbXBsZSBub25jZQ=="),("Sec-WebSocket-Version","13")] "/ws"`
answers 503 whose body decodes to an envelope with code `websocket_unavailable`. (http-client
sends the upgrade headers as given and reads the ordinary 503 response the seam writes.)

### M3.2 Nix, registry, and README wiring

`nix/haskell-overlay.nix`, in the `Local packages` section after `pgmq-config`:

```nix
  pgmq-inspect = dontCheck (doJailbreak (final.callCabal2nix "pgmq-inspect" ../pgmq-inspect { }));
```

`flake.module.nix`, in `packages`, add `pgmq-inspect = haskellPackages.pgmq-inspect;` and in
`checks` add `pgmq-inspect` to the `inherit (haskellPackages) …;` list. Do not add a
`pgmq-inspect-tests` check: the suite depends on `ephemeral-pg` and the hs-opentelemetry SDK
chain the dependency-bounds ADR documents as unbuildable under `nix flake check`.

Build:

```bash
nix build .#pgmq-inspect
```

If the build fails while realising `wai-websockets` with an error inside `wai-app-static`, add
above the local packages in `nix/haskell-overlay.nix` the override kiroku uses (its comment
explains the cause: cabal2nix lists the optional example executable's dependencies
unconditionally):

```nix
  # wai-websockets' optional `wai-websockets-example` executable (cabal flag
  # `example`, default off) depends on wai-app-static. cabal2nix lists those
  # executable deps unconditionally, so nix realizes wai-app-static as a build
  # input even though the example is never compiled. Drop the executable deps
  # so the (fine) library builds.
  wai-websockets = pkgs.haskell.lib.compose.overrideCabal (_: {
    executableHaskellDepends = [ ];
  }) prev.wai-websockets;
```

Record whether it was needed in Surprises & Discoveries either way.

`justfile`, the `nix-build` recipe: add `nix build .#pgmq-inspect` after `pgmq-migration`.

`mori.dhall`: add after the `pgmq-config` package entry

```dhall
      , Schema.Package::{ name = "pgmq-inspect"
        , type = Schema.PackageType.Library
        , language = Schema.Language.Haskell
        , path = Some "./pgmq-inspect"
        , description = Some
            "Embeddable HTTP and WebSocket queue inspection surface"
        , dependencies =
          [ internalDep "pgmq-core"
          , internalDep "pgmq-hasql"
          , internalDep "pgmq-effectful"
          , thirdPartyDep "hasql/hasql:hasql"
          , thirdPartyDep "effectful/effectful:effectful-core"
          ]
        }
```

and add `"pgmq-inspect"` to the `pgmq-hs` bundle's `packages` list and to its description.
Validate with `mori validate` (expect no errors; a stale schema-hash warning is pre-existing
and benign).

`README.md`: add the row `| `pgmq-inspect` | Embeddable HTTP and WebSocket queue inspection surface (read-only, CORS-aware) |`
to the package table and, after the `## pgmq-config` section, a short `## pgmq-inspect`
section with this example:

```haskell
import Hasql.Pool qualified as Pool
import Pgmq.Inspect

main :: IO ()
main = do
  pool <- Pool.acquire poolConfig
  -- Serve on 127.0.0.1:9092 with CORS disabled; mount `withInspectApplication`
  -- into your own server instead if you already run one.
  runInspectServer defaultInspectServerConfig (poolEnv pool Nothing)
```

and the sentence that the guide (`docs/user/queue-inspection.md`, written by plan 31) covers
every route; until plan 31 lands, point at design note 021.

### M3.3 Documents

`docs/design/021-inspection-surface-wire-contract.md`, with these headings and content:
Status (adopted, date, introduced by this plan); The package (what `pgmq-inspect` exports and
the keiro-independence posture from the ADR); Routes (the table from Plan of Work, each with
its success body, its error codes, and for `/metrics` the sentence that `pgmq.metrics_all()`
loops over `pgmq.meta` running a `count(*)` scan per queue, so one call costs O(number of
queues) full counts and a client should poll it every 5 to 15 seconds, never per second);
Pagination (`from` exclusive, `limit` default and cap, `next_cursor` present only when a next
page exists, cursors opaque to clients); The error envelope (the shape and the complete code
vocabulary with status and meaning, including `invalid_message_id`); CORS (explicit list,
disabled by default, no credentials, unlisted origins served without headers); Mounting under
a prefix (routing on `pathInfo`, relative paths in the descriptor, upgrade dispatched from the
router); Authentication (none; trusted network or authenticating reverse proxy; do not expose
bare); Wire stability (published shapes frozen, additive only, incompatible changes as new
paths); WebSocket (one line: "Filled by `docs/plans/30-…`"); Related documents (020, 019,
015, 016, 017, the ADR).

Capability record: allocate the handle and write the file.

```bash
okf id next docs/capabilities --profile docs/capabilities/profile.dhall CAP
```

With the printed handle (`CAP-10` if plans 27 and 28 have not yet allocated; otherwise the
next free), create `docs/capabilities/http-queue-inspection.md` in the shape of
`docs/capabilities/message-queue-client.md`: `type: Capability`, `generated.by: process:claude-code`
with the current UTC time, `capabilityId`, `provider: mori://shinzui/pgmq-hs`,
`status: shipped`, `stability: experimental`, `since: "unreleased"`, `packages: [pgmq-inspect]`,
`requires:` the inspection-reads and JSON-encoding capabilities by their handles (read
`docs/capabilities/index.md` to find them), `interface: [Pgmq.Inspect]`, and `evidence` entries
for `pgmq-inspect/test/HttpSpec.hs` (browsing leaves `read_ct` untouched; every route's shape),
`pgmq-inspect/test/CorsSpec.hs`, `pgmq-inspect/test/ErrorSpec.hs`, `pgmq-inspect/test/RouterSpec.hs`,
and the design note. The body states what it provides (the route groups), the shape (the
README example), and the limits (read-only; no authentication; the WebSocket feed and the
executable arrive with plans 30 and 31; `/metrics` cost). Add the row to the table in
`docs/capabilities/index.md`, then:

```bash
okf log add docs/capabilities --kind Addition -m "<handle>: HTTP queue inspection surface in the new pgmq-inspect package (ExecPlan 29)"
just docs-check
```

Root `CHANGELOG.md`, a new `## Unreleased` section at the top:

```markdown
## Unreleased

New package `pgmq-inspect`: an embeddable WAI application and Warp runner exposing read-only
queue inspection over HTTP (listings, metrics, non-destructive browsing and lookup, bindings,
routing tests, throttles, health probes) with a structured error envelope and explicit-origin
CORS. No existing library package gains a web dependency. The WebSocket endpoint rejects every
upgrade in this change; see `docs/design/021-inspection-surface-wire-contract.md`.
```

If an `Unreleased` section already exists (plans 27 or 28 landed first), append a paragraph to
it instead.

Final commit of the plan:

```text
feat(inspect): export the mountable application and runner, wire the package, and record the contract

Add withInspectApplication, the Warp runner, and the rejecting WebSocket seam;
wire pgmq-inspect into Nix, the registry, the README, and the justfile; write
design note 021 and the capability record.

MasterPlan: docs/masterplans/7-expose-non-destructive-queue-inspection-through-typed-reads-json-codecs-and-the-pgmq-inspect-sister-package.md
ExecPlan: docs/plans/29-create-the-pgmq-inspect-sister-package-with-the-http-inspection-surface.md
Intention: intention_01m3tcw9vmeeftdtqj53d1d6nb
```


## Validation and Acceptance

Run everything from the repository root inside `nix develop`.

```bash
cabal build pgmq-inspect
cabal test pgmq-inspect:pgmq-inspect-test
cabal test all
nix build .#pgmq-inspect
nix fmt
mori validate
just docs-check
```

`cabal build pgmq-inspect` compiles the library with no warnings. The test suite prints one
`OK` per case and `All N tests passed`; the cases a reviewer should look for by name are
"browsing over HTTP leaves read_ct at zero and every message available", "GET /queues lists
the created queue", "an unreachable database answers 503 database_unavailable",
"a missing queue answers 404 queue_not_found", "no CORS headers when disabled",
"a listed origin gets Access-Control-Allow-Origin", "an unlisted origin gets none", and
"a plain GET /ws is 426". `cabal test all` proves nothing else in the family changed.
`nix build .#pgmq-inspect` prints a `/nix/store/…-pgmq-inspect-0.1.0.0` path.

To see the surface by hand, run a disposable PostgreSQL and the suite's own harness is not
needed: the shortest path is `cabal repl pgmq-inspect-test` is not required either; write a
ten-line throwaway in the scratchpad that acquires a pool against a running database with the
schema installed (the dev shell's `PG_CONNECTION_STRING` after `just create-database` and
running `pgmqMigrations` through `pg-migrate`, as the README's fresh-installation example
shows) and calls `runInspectServer defaultInspectServerConfig (poolEnv pool Nothing)`, then:

```bash
curl -s -i http://127.0.0.1:9092/queues
curl -s http://127.0.0.1:9092/queues/orders/messages?limit=2 | jq .
curl -s -i http://127.0.0.1:9092/queues/nope/metrics
curl -s -i -H 'Origin: http://ui.example' http://127.0.0.1:9092/queues
```

Expected: a `200` with `content-type: application/json` and a JSON array; a page object with
`items` and, if more than two messages exist, `next_cursor`; a `404` whose body is
`{"error":{"code":"queue_not_found","message":"No queue with that name exists.","details":{"code":"42P01",…}}}`;
and, with the default configuration, no `access-control-allow-origin` header on the last
response.

The acceptance items of `IR-3` this plan owns map to tests as follows: item 1 to "GET /queues
lists the created queue"; item 2 to the metrics cases (the two depths asserted separately) and
to the `/metrics` paragraph in design note 021; item 3 to the non-destructive case; item 5 to
`CorsSpec`; item 6 to `grep -n "wai\|warp\|websockets" pgmq-core/*.cabal pgmq-hasql/*.cabal pgmq-effectful/*.cabal pgmq-migration/*.cabal pgmq-config/*.cabal`
printing nothing and to `withInspectApplication` being exported.


## Idempotence and Recovery

Every step is additive and re-runnable. Creating the package twice is refused by `mkdir`
only if you use `mkdir` without `-p`; the cabal file and modules are plain files you overwrite.
Golden files are regenerated with `--test-options=--accept` after an intentional wire change
and reviewed in the diff; an unintentional change shows up as a failed golden test, which is
the point. The Nix overlay entry, the flake wiring, the `mori.dhall` entry, and the README row
are each a single addition that can be reverted with `git checkout -- <file>`.

If `nix build .#pgmq-inspect` fails for a reason other than `wai-app-static`, do not pin or
jailbreak around it blindly: read the failing derivation's name, check whether the version in
the `ghc9124` set matches the bound in the cabal file (the bounds above were written against
the versions the set provides today), and widen the bound only if the API is compatible,
recording the change in the Decision Log.

If `HttpSpec` fails only on the mixed-case case, confirm it runs on its own `withPgmqDb`; a
mixed-case row in a shared database breaks the typed listing other packages' tests rely on,
which is why design note 016's tests isolate it the same way.

If a test hangs on readiness or on the unreachable-pool case, the `connect_timeout=1` in the
connection string is what bounds it; check it is present.

Commits are three, one per milestone, each leaving `cabal test all` green; a partially done
milestone is recorded in Progress as "done" and "remaining" entries before stopping.


## Interfaces and Dependencies

Library dependencies added (none to any existing package): `wai ^>=3.2`, `warp ^>=3.4`,
`wai-cors ^>=0.2.7`, `wai-websockets ^>=3.0`, `websockets ^>=0.13`, `http-types ^>=0.12`,
`async ^>=2.2`, `stm`, `hs-opentelemetry-api >=1.0 && <2` (for the `OTel.Tracer` type in
`tracedPoolEnv`), plus the family's own `pgmq-core`, `pgmq-hasql`, `pgmq-effectful`, `hasql`,
`hasql-pool`, `effectful-core`, `generic-lens`, `lens`, `aeson`, `text`, `time`, `vector`,
`bytestring`, `containers`. Test-only: `http-client ^>=0.7`, `tasty-golden ^>=2.3`,
`ephemeral-pg`, `pg-migrate`, `pgmq-migration`, `random`, `unix`, `directory`.

At the end of Milestone 1, these exist with these signatures:

```haskell
-- Pgmq.Inspect.Config
data CorsPolicy = CorsDisabled | CorsAllowOrigins !(NonEmpty ByteString)
data InspectConfig = InspectConfig { corsPolicy :: !CorsPolicy, defaultPageLimit :: !Int32, maxPageLimit :: !Int32, readinessTimeoutUs :: !Int, pollIntervalUs :: !Int, notifyDebounceUs :: !Int, wsMaxConnections :: !Int, wsMaxSubscriptions :: !Int, wsQueueCapacity :: !Natural }
defaultInspectConfig :: InspectConfig
data InspectServerConfig = InspectServerConfig { bindHost :: !String, bindPort :: !Int, inspect :: !InspectConfig }
defaultInspectServerConfig :: InspectServerConfig

-- Pgmq.Inspect.Env
type Inspection a = Eff '[Pgmq, Error PgmqRuntimeError, IOE] a
data InspectEnv = InspectEnv { runInspection :: forall a. Inspection a -> IO (Either PgmqRuntimeError a), listenerSettings :: !(Maybe Settings) }
runnerFromInterpreter :: (forall a. Eff '[Pgmq, Error PgmqRuntimeError, IOE] a -> Eff '[Error PgmqRuntimeError, IOE] a) -> (forall a. Inspection a -> IO (Either PgmqRuntimeError a))
poolEnv :: Pool -> Maybe Settings -> InspectEnv
tracedPoolEnv :: Pool -> OTel.Tracer -> Maybe Settings -> InspectEnv

-- Pgmq.Inspect.Wire
data Page a = Page { items :: ![a], nextCursor :: !(Maybe MessageId) }
data ServiceDescriptor = ServiceDescriptor { serviceName :: !Text, serviceVersion :: !Text, websocketPath :: !Text }
newtype LivenessBody = LivenessBody { alive :: Bool }
data DependencyCheck = DependencyCheck { checkName :: !Text, checkHealthy :: !Bool, checkLatencyMs :: !(Maybe Int64), checkError :: !(Maybe Text) }
data ReadinessBody = ReadinessBody { ready :: !Bool, checks :: ![DependencyCheck] }

-- Pgmq.Inspect.Error
data ErrorBody = ErrorBody { errorCode :: !Text, errorMessage :: !Text, errorDetails :: !(Maybe Value) }
jsonResponse :: Status -> LBS.ByteString -> Response
errorResponse :: Status -> ErrorBody -> Response
sqlState :: PgmqRuntimeError -> Maybe Text
describeRuntimeError :: PgmqRuntimeError -> (Text, Maybe Value)
classifyRuntimeError :: PgmqRuntimeError -> (Status, ErrorBody)

-- Pgmq.Inspect.Http
data PathSegment = Literal !Text | Placeholder !Text
routeTable :: [(Method, [PathSegment])]
httpApp :: InspectConfig -> InspectEnv -> WS.ServerApp -> Application
```

At the end of Milestone 2, every route in `routeTable` except `["ws"]` is served by `httpApp`,
and `Pgmq.Inspect.Cors.corsMiddleware :: CorsPolicy -> Middleware` exists.

At the end of Milestone 3, `Pgmq.Inspect.Server` exports
`withInspectApplication :: InspectConfig -> InspectEnv -> (Application -> IO a) -> IO a`,
`withInspectApplicationWith :: InspectConfig -> InspectEnv -> WS.ServerApp -> (Application -> IO a) -> IO a`,
`rejectingWebSocketApp :: WS.ServerApp`,
`data RunningInspectServer = RunningInspectServer { serverPort :: !Int, serverThread :: !(Async ()) }`,
`withInspectServer :: InspectServerConfig -> InspectEnv -> (RunningInspectServer -> IO a) -> IO a`,
and `runInspectServer :: InspectServerConfig -> InspectEnv -> IO ()`; `Pgmq.Inspect` re-exports
all of the above plus `routeTable` and `PathSegment`; and the `["ws"]` route dispatches the
seam. Plan 30 replaces `rejectingWebSocketApp` inside `withInspectApplication` and consumes
`listenerSettings`, `pollIntervalUs`, `notifyDebounceUs`, `wsMaxConnections`,
`wsMaxSubscriptions`, and `wsQueueCapacity`; plan 31 reads `routeTable`, adds `openapi_path`
to `ServiceDescriptor`, and calls `runInspectServer` from the executable.
