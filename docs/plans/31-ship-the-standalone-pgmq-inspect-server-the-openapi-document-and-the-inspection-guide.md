---
id: 31
slug: ship-the-standalone-pgmq-inspect-server-the-openapi-document-and-the-inspection-guide
title: "Ship the standalone pgmq-inspect server, the OpenAPI document, and the inspection guide"
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

# Ship the standalone pgmq-inspect server, the OpenAPI document, and the inspection guide

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.
If durable project context changes, update or create ADRs in docs/adr/ in the same change.


## Purpose / Big Picture

After `docs/plans/29-create-the-pgmq-inspect-sister-package-with-the-http-inspection-surface.md`
and `docs/plans/30-add-the-notify-accelerated-poll-authoritative-websocket-live-feed-to-pgmq-inspect.md`
are complete, the repository holds a package, `pgmq-inspect`, that exposes pgmq queues over HTTP
and WebSocket as an embeddable WAI `Application`. A Haskell host that already runs a Warp server
can mount it. Nobody else can use it yet: there is no program to run, no machine-readable
description of the routes a client could generate code from, and the only prose describing the
wire contract lives in another repository's conventions document. This plan closes that gap. It
is the plan that makes the surface adoptable by a pgmq-hs user who has never heard of keiro,
which the parent MasterPlan calls *keiro-independence*.

When this plan is complete, three things exist that do not exist today.

First, an executable named `pgmq-inspect`. Given a database URL and nothing else, it serves the
whole surface:

```bash
pgmq-inspect --database-url "host=/path/to/socket dbname=pgmq_dev" --port 9092
```

prints `pgmq-inspect 0.1.0.0 listening on http://127.0.0.1:9092`, and from that moment
`curl http://127.0.0.1:9092/queues` returns the queue list as a JSON array, a browser on an
allowed origin can call every endpoint, and a WebSocket client can subscribe at `/ws`.

Second, an OpenAPI 3 document served at `GET /openapi.json`, built in Haskell from the same
route table the router serves and pinned by a golden file. Anyone can feed it to a client
generator and get a typed client for the HTTP side in their language of choice. A test proves
that every HTTP route the router serves is described in the document, so the two cannot drift
apart silently.

Third, a user guide at `docs/user/queue-inspection.md`, owned by pgmq-hs, that documents every
route, every error code, every WebSocket frame, pagination, CORS, the security posture, and how
to build an independent UI or dashboard on top of the surface. It is linked from the README and
registered in `mori.dhall`, and the capability record and improvement request `IR-3` are closed
against it.

You can see all three working by running the executable against the development shell's
PostgreSQL, issuing the `curl` commands in Validation and Acceptance, and running the package's
test suite, which now includes `OpenApiSpec`.


## Progress

- [ ] M1: `pgmq-inspect/app/Main.hs` and the `executable pgmq-inspect` stanza exist; `cabal run pgmq-inspect -- --version` prints the version; the server starts against the dev-shell database and answers `GET /queues`
- [ ] M1: `--cors-origin`, `--host`, `--port`, `--poll-interval-ms`, `--pool-size`, `--no-listener`, and the `PGMQ_INSPECT_DATABASE_URL` fallback behave as documented; a missing or unparsable URL exits with status 2 and a one-line message
- [ ] M1: `nix build .#pgmq-inspect` produces `result/bin/pgmq-inspect`
- [ ] M2: `Pgmq.Inspect.OpenApi.openApiDocument` exists, is served at `GET /openapi.json`, and the service descriptor at `GET /` carries `"openapi_path":"openapi.json"` with its golden updated
- [ ] M2: `pgmq-inspect/test/OpenApiSpec.hs` is green: golden `test/golden/openapi.json`, route coverage against `routeTable`, and the served document decodes back into `OpenApi`
- [ ] M3: `docs/user/queue-inspection.md` written; README gains the `pgmq-inspect` section; `mori.dhall` registers the guide; `mori validate` passes
- [ ] M3: the capability record from plan 29 lists the executable, the OpenAPI test, and the guide as evidence; `just docs-check` passes
- [ ] M3: `IR-3` set to `completed` with `completedAt` and a `resolution`; the improvement-requests log appended
- [ ] M3: root `CHANGELOG.md` and `pgmq-inspect/CHANGELOG.md` `Unreleased` sections written, including the release handoff paragraph


## Surprises & Discoveries

Document unexpected behaviors, bugs, optimizations, or insights discovered during
implementation. Provide concise evidence.

(None yet.)


## Decision Log

- Decision: The OpenAPI document is built programmatically with the `openapi3` library and
  pinned by a golden file, rather than hand-written as JSON or derived from a servant API type.
  Rationale: the router is plain WAI (the family precedent), so there is no API type to derive
  from; a hand-written JSON file drifts silently, whereas a Haskell value is type-checked, can
  be cross-checked against `routeTable` by a test, and still produces a committed golden file a
  reviewer can read in a diff. Date: 2026-10-01
- Decision: `Pgmq.Inspect.OpenApi` does not import `Pgmq.Inspect.Http`; the router imports the
  document to serve it, and the coverage test in the test suite is what ties the two together.
  Rationale: the router must serve the document (so `Http` imports `OpenApi`), and the document
  must not import the router or the modules form a cycle; declaring the documented paths
  explicitly in the `OpenApi` module and checking them against `routeTable` from the outside
  gives the same guarantee without the cycle. Date: 2026-10-01
- Decision: The golden file is the compact `encode` output, not a pretty-printed form.
  Rationale: pretty-printing needs `aeson-pretty`, an extra dependency for a cosmetic benefit;
  a reviewer who wants to read the document runs `jq .` on the golden file. Date: 2026-10-01
- Decision: The executable resolves the database URL from `--database-url` first and the
  environment variable `PGMQ_INSPECT_DATABASE_URL` second, and refuses to start without one.
  Rationale: a connection string often carries a password, and an environment variable keeps
  it out of shell history and process listings; refusing to start beats connecting to libpq's
  implicit defaults and surfacing the mistake as a readiness failure later. Date: 2026-10-01
- Decision: The executable prints its startup line from inside `withInspectServer` using the
  port the server actually bound, and then waits on the server thread. Rationale: `--port 0`
  asks the operating system for a free port, and the only truthful place to learn it is the
  running server handle. Date: 2026-10-01
- Decision: The guide is pgmq-hs's own document and restates the wire contract in full rather
  than linking to keiro-ui's conventions document for the rules. Rationale: the parent
  MasterPlan's keiro-independence posture; a pgmq-hs user must be able to build against the
  surface with this repository alone. The conventions document is cited once, as provenance,
  with its canonical project URI. Date: 2026-10-01


## Outcomes & Retrospective

Summarize outcomes, gaps, and lessons learned at major milestones or at completion.
Compare the result against the original purpose. Before marking the plan complete,
distill durable project context from the Decision Log, Surprises & Discoveries, and
this section into docs/adr/. Keep task-local execution details here.

(To be filled during and after implementation. The final entry must include a paragraph
headed "Release handoff" stating that the family's next lockstep release, prepared by
`docs/plans/25-create-the-ephemeral-root-with-owner-only-permissions-and-record-acceptance-on-the-current-postgresql.md`
under MasterPlan 6 or by its successor, must include `pgmq-inspect`, move its internal bounds
with the family, and list the `Pgmq` effect's new constructors as a breaking change.)


## Context and Orientation

### What this plan assumes already exists

This plan is the last of five under
`docs/masterplans/7-expose-non-destructive-queue-inspection-through-typed-reads-json-codecs-and-the-pgmq-inspect-sister-package.md`.
It hard-depends on plans 29 and 30 and must not start until both are marked Complete in the
MasterPlan's Exec-Plan Registry. Before touching anything, verify that the artifacts it builds
on exist, from the repository root:

```bash
cd /Users/shinzui/Keikaku/bokuno/libraries/pgmq-hs-project/pgmq-hs
test -f pgmq-inspect/pgmq-inspect.cabal && echo "package present"
grep -n "^routeTable\|^data PathSegment\|Literal\b\|Placeholder\b" pgmq-inspect/src/Pgmq/Inspect/Http.hs
grep -n "^poolEnv\|^tracedPoolEnv\|^data InspectEnv\|listenerSettings\|runInspection" pgmq-inspect/src/Pgmq/Inspect/Env.hs
grep -n "^runInspectServer\|^withInspectServer\|^withInspectApplication\|^data RunningInspectServer" pgmq-inspect/src/Pgmq/Inspect/Server.hs
grep -n "^data CorsPolicy\|CorsDisabled\|CorsAllowOrigins\|^data InspectConfig\|^data InspectServerConfig\|pollIntervalUs\|bindHost\|bindPort\|^defaultInspectServerConfig\|^defaultInspectConfig" pgmq-inspect/src/Pgmq/Inspect/Config.hs
grep -n "websocket_path\|ServiceDescriptor" pgmq-inspect/src/Pgmq/Inspect/Wire.hs
grep -rn "\"subscribe\"\|\"snapshot\"\|\"update\"\|\"goodbye\"\|\"push\"" pgmq-inspect/src/Pgmq/Inspect/WebSocket.hs | head
ls pgmq-inspect/test/
grep -n "pgmq-inspect" nix/haskell-overlay.nix flake.module.nix cabal.project justfile mori.dhall README.md
```

Every grep must print at least one line. If `PathSegment` is not in `Pgmq.Inspect.Http`, find
where plan 29 put it (`grep -rn "data PathSegment" pgmq-inspect/src`) and import it from there;
nothing else in this plan changes. If any other grep is silent, stop: a hard dependency is not
complete, and the MasterPlan registry is wrong.

The following facts about plans 29 and 30 are binding on this plan. They are copied from the
MasterPlan's Integration Points so this plan stands alone.

The package lives in `pgmq-inspect/`, is named `pgmq-inspect`, starts at version `0.1.0.0`,
and depends on `pgmq-core`, `pgmq-hasql`, and `pgmq-effectful` with bounds `>=0.6 && <0.7`.
Its library modules are `Pgmq.Inspect` (umbrella re-export), `Pgmq.Inspect.Config`,
`Pgmq.Inspect.Env`, `Pgmq.Inspect.Wire`, `Pgmq.Inspect.Error`, `Pgmq.Inspect.Cors`,
`Pgmq.Inspect.Http`, `Pgmq.Inspect.Server`, `Pgmq.Inspect.WebSocket`, and
`Pgmq.Inspect.Listener`. Its extension set follows `pgmq-hasql`: `OverloadedLabels`,
`DuplicateRecordFields`, `DeriveGeneric`, `ImportQualifiedPost`, `OverloadedStrings`, with
`generic-lens` and `lens` for field access, and never `OverloadedRecordDot` or
`NoFieldSelectors`. Its test suite `pgmq-inspect-test` already has `EphemeralDb.hs` (a copy of
`pgmq-effectful/test/EphemeralDb.hs`) and `TestServer.hs`, which starts the application on an
OS-assigned free port with `Warp.openFreePort` and `Warp.runSettingsSocket` and returns the port
for `http-client` and `websockets` clients.

The key types, as the MasterPlan fixes them:

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
    pollIntervalUs :: !Int,               -- default 5_000_000
    notifyDebounceUs :: !Int,             -- default 100_000
    wsMaxConnections :: !Int,             -- default 100
    wsMaxSubscriptions :: !Int,           -- default 100 per connection
    wsQueueCapacity :: !Natural           -- default 256 frames per connection
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
data PathSegment = Literal Text | Placeholder Text
routeTable :: [(Method, [PathSegment])]   -- every served route, with placeholders
```

The HTTP routes plan 29 serves, all `GET`, all JSON, all relative to the mount root:

```text
GET /                                    service descriptor {"service":"pgmq-inspect","version":"…","websocket_path":"ws"}
GET /queues                              JSON array of queue objects (lenient listing)
GET /queues/{queue}                      one queue object, 404 queue_not_found
GET /queues/{queue}/metrics              QueueMetrics, 404 queue_not_found
GET /metrics                             JSON array of QueueMetrics for every queue (O(queues) cost)
GET /queues/{queue}/messages?from&limit  page {"items":[Message…],"next_cursor":N?}
GET /queues/{queue}/messages/{msg_id}    Message, 400 invalid_message_id, 404 message_not_found
GET /queues/{queue}/archive?from&limit   page {"items":[ArchivedMessage…],"next_cursor":N?}
GET /queues/{queue}/archive/{msg_id}     ArchivedMessage, 400 invalid_message_id, 404 message_not_found
GET /queues/{queue}/bindings             JSON array of TopicBinding
GET /bindings                            JSON array of TopicBinding
GET /routing/test?routing_key=…          JSON array of RoutingMatch, 400 invalid_routing_key
GET /notify/throttles                    JSON array of NotifyInsertThrottle
GET /health/live                         {"alive":true}
GET /health/ready                        {"ready":bool,"checks":[{"name":"postgres","healthy":bool,"latency_ms":n,"error":null|"…"}]} 200 or 503
GET /ws                                  WebSocket upgrade; a plain GET gets 426 websocket_upgrade_required
```

This plan adds one route, `GET /openapi.json`, and one field, `"openapi_path":"openapi.json"`,
to the service descriptor.

`from` is an exclusive `msg_id` cursor; `limit` defaults to `defaultPageLimit` and is capped at
`maxPageLimit`; `next_cursor` is the last item's `msg_id` and is omitted entirely on the last
page. Unknown paths answer 404 `not_found`; non-`GET` methods answer 405 `method_not_allowed`.

Every error body is `{"error":{"code":"…","message":"…","details":{…}?}}`. The codes are
`not_found`, `method_not_allowed`, `queue_not_found` (the server's SQLSTATE `42P01`),
`message_not_found`, `invalid_cursor`, `invalid_limit`, `invalid_message_id` (a `{msg_id}`
segment that is not an integer), `invalid_routing_key`,
`websocket_upgrade_required`, `websocket_unavailable`, `database_unavailable` (503 when
`isTransient` holds for the `PgmqRuntimeError`), and `database_error` (500 for every other
`PgmqRuntimeError`, with the SQL text and parameters stripped from the message). The
WebSocket feed adds the frame-level codes `invalid_frame`,
`too_many_subscriptions`, and `overflow`.

The WebSocket protocol plan 30 speaks on `/ws`. Client frames: `{"type":"subscribe","queue":"…"}`,
`{"type":"unsubscribe","queue":"…"}`, `{"type":"ping"}`. Server frames:
`{"type":"snapshot","queue":"…","metrics":{QueueMetrics},"push":true|false}` immediately after
a successful subscribe (`push` is `false` when the name fails `parseQueueName`, when the queue
is partitioned, because partitioned queues receive no insert notifications, or when no listener
is configured, meaning updates come from polling only);
`{"type":"update","queue":"…","metrics":{QueueMetrics},"source":"notify"|"poll"}`;
`{"type":"pong"}`; `{"type":"error","code":"…","message":"…","queue":"…"|null}` (the `queue`
key is always present and `null` for errors not about one queue); and
`{"type":"goodbye"}` before any server-initiated close. The server pings idle connections every
30 seconds. One server-wide LISTEN connection, acquired from `listenerSettings`, listens on the
insert-notification channel of every subscribed validated queue, reconnects with exponential
backoff when it drops, and re-issues every `LISTEN` after reconnecting; subscribers keep
receiving poll-sourced updates throughout. The poll is authoritative: every `pollIntervalUs` the
server reads `queueMetrics` for each subscribed queue and emits an `update` with `source`
`poll` whenever the metrics (ignoring `scrape_time`) changed.

The JSON field names, fixed by `docs/design/020-json-wire-encodings.md` (plan 28) and pinned by
golden tests in `pgmq-core`, `pgmq-hasql`, `pgmq-config`, and `pgmq-inspect`: queues carry
`queue_name`, `is_partitioned`, `is_unlogged`, `created_at`; messages carry `msg_id`, `read_ct`,
`enqueued_at`, `last_read_at`, `vt`, `message`, `headers`, and archived messages additionally
`archived_at`; metrics carry `queue_name`, `queue_length`, `queue_visible_length`,
`newest_msg_age_sec`, `oldest_msg_age_sec`, `total_messages`, `scrape_time`,
`default_partition_length`; bindings carry `pattern`, `queue_name`, `bound_at`,
`compiled_regex`; routing matches carry `pattern`, `queue_name`, `compiled_regex`; throttles
carry `queue_name`, `throttle_interval_ms`, `last_notified_at`. `Maybe` fields are always
present and encode as `null`. Timestamps are aeson's default ISO-8601 UTC form, for example
`2026-10-01T00:15:41.123456Z`.

### Terms

An *OpenAPI document* is a JSON description of an HTTP API, listing each path, its methods,
parameters, response status codes, and the JSON shapes involved, in a format many tools read
to generate client code or interactive documentation. Version 3.0 of that format is what the
`openapi3` Haskell library produces. A *golden test* compares a value's rendering against a
committed file and fails on any difference, so a change to the rendering must be deliberate
and visible in a diff. A *client generator* is a tool that reads an OpenAPI document and
emits typed code for calling the API in some language. The *service descriptor* is the small
JSON object `GET /` returns so a client can discover where the other entry points are without
hard-coding them. *CORS* (Cross-Origin Resource Sharing) is the browser mechanism that blocks a
page served from one origin from calling an API on another origin unless the API opts in with
response headers. *libpq* is PostgreSQL's C client library; a *libpq connection string* is
either a `key=value` list such as `host=localhost dbname=pgmq_dev` or a URL such as
`postgres://user:pass@localhost:5432/pgmq_dev`.

### The development shell and its PostgreSQL

Everything runs inside `nix develop` at the repository root (`nix develop --command <cmd>` for
one-off commands). The shell hook in `flake.module.nix` sets `PGHOST="$PWD/.dev/db"`,
`PGDATA="$PGHOST/data"`, `PGLOG="$PGHOST/postgres.log"`, `PGDATABASE=pgmq_dev`, and
`PG_CONNECTION_STRING="host=$PGHOST dbname=$PGDATABASE"`, and runs `initdb` the first time the
shell is entered. The server listens on a Unix socket only (`listen_addresses=''`), which is why
the connection string names a directory as its host. Start it and create the database with
`just process-up` (which runs `process-compose` with the `postgres` and `create_schema`
processes from `process-compose.yaml`) or by hand:

```bash
pg_ctl start -w -l "$PGLOG" -o "--unix_socket_directories='$PGHOST'" -o "-c listen_addresses=''"
just create-database
```

A fresh database has no pgmq schema, and the executable assumes the schema is already
installed (by `pgmq-migration`, as every test suite in this repository does, or by the upstream
extension). `pgmq-migration` ships no runner and `pg-migrate` ships no standalone binary, so
install it once with a throwaway script. Write the following to
`/private/tmp/claude-501/-Users-shinzui-Keikaku-bokuno-libraries-pgmq-hs-project-pgmq-hs/90a95def-cc2d-4814-9797-14a8bb36f553/scratchpad/install-pgmq-schema.hs`
(any path outside the repository is fine):

```haskell
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as T
import Database.PostgreSQL.Migrate (defaultRunOptions, migrationPlan, runMigrationPlan)
import Hasql.Connection.Settings qualified as Settings
import Pgmq.Migration (pgmqMigrations)
import System.Environment (getArgs)

main :: IO ()
main = do
  [conninfo] <- getArgs
  component <- either (fail . show) pure pgmqMigrations
  plan <- either (fail . show) pure (migrationPlan (component :| []))
  report <- runMigrationPlan defaultRunOptions (Settings.connectionString (T.pack conninfo)) plan
  either (fail . show) print report
```

and run it through the project's package environment:

```bash
nix develop --command bash -c 'cabal build pgmq-migration && cabal exec -- runghc /private/tmp/claude-501/-Users-shinzui-Keikaku-bokuno-libraries-pgmq-hs-project-pgmq-hs/90a95def-cc2d-4814-9797-14a8bb36f553/scratchpad/install-pgmq-schema.hs "$PG_CONNECTION_STRING"'
```

The report prints one `AppliedNow` per migration (`0001` through `0006`, or `0007` if
MasterPlan 6's plan 23 has landed); running it again prints `AlreadyApplied` for each. If
`cabal exec -- runghc` cannot see `pgmq-migration` in your environment, the dev-only fallback
is to apply the files in `pgmq-migration/migrations/` with `psql -f` in manifest order, which
bypasses the migration ledger and is acceptable only for a throwaway database. Prefer the
script: it is what the user guide tells readers to do.

Two toolchain facts cost time if forgotten. GNU `sed` shadows BSD `sed` in the Nix profile, so
in-place edits are `sed -i -e 's/a/b/' file`, never `sed -i '' …`. `nix fmt` runs
`fourmolu`, `cabal-gild`, and `nixpkgs-fmt` and must run before every commit, because the
pre-commit hook otherwise fails, reformats, and requires a re-stage.

### Files this plan touches

New: `pgmq-inspect/app/Main.hs`, `pgmq-inspect/src/Pgmq/Inspect/OpenApi.hs`,
`pgmq-inspect/test/OpenApiSpec.hs`, `pgmq-inspect/test/golden/openapi.json`,
`docs/user/queue-inspection.md`.

Changed: `pgmq-inspect/pgmq-inspect.cabal` (the executable stanza, the new module, new
dependencies, `extra-source-files` for the golden), `pgmq-inspect/src/Pgmq/Inspect/Http.hs`
(the `openapi.json` route and the `routeTable` entry), `pgmq-inspect/src/Pgmq/Inspect/Wire.hs`
(the `openapi_path` field of the service descriptor), `pgmq-inspect/src/Pgmq/Inspect.hs`
(re-export `openApiDocument`), `pgmq-inspect/test/Main.hs` (register `OpenApiSpec`), the
service-descriptor golden plan 29 committed (its exact path is under `pgmq-inspect/test/golden/`;
find it with `grep -rln websocket_path pgmq-inspect/test/golden`), `README.md`, `mori.dhall`,
the capability record plan 29 created (find it with
`grep -ln "pgmq-inspect" docs/capabilities/*.md`), `docs/capabilities/log.md` (via `okf log add`),
`docs/improvement-requests/add-a-pgmq-metrics-sister-package-with-http-and-websocket-inspection-endpoints.md`,
`docs/improvement-requests/log.md` (via `okf log add`), `CHANGELOG.md`, and
`pgmq-inspect/CHANGELOG.md`.

Not touched: anything in `pgmq-core`, `pgmq-hasql`, `pgmq-effectful`, `pgmq-config`,
`pgmq-migration`; the design notes 019 through 021 (plan 29 and plan 30 own 021; this plan
only cites it); version numbers anywhere.

### ADRs and design notes consulted

[docs/adr/queue-inspection-surface-boundary-and-wire-contract.md](../adr/queue-inspection-surface-boundary-and-wire-contract.md)
is the decision this plan implements the visible half of: the surface is a sister package that
is runnable on its own, pgmq-hs owns and documents its wire contract, the contract is served as
an OpenAPI 3 document, and once a field, route, or frame ships in a release it is never removed
or re-typed. Every sentence in the guide about freezing, the executable, and the OpenAPI
document traces to that record; do not contradict it, and if implementation reveals it is
wrong, update it in the same change.

[docs/adr/haskell-dependency-bounds-and-nix-pin-policy.md](../adr/haskell-dependency-bounds-and-nix-pin-policy.md)
says a Cabal bound states compatibility and the Nix pin states what is tested. It matters here
because `nix/haskell-overlay.nix` overrides `optparse-applicative` to `0.19.0.0` while the
`ghc9124` package set ships `0.18.1.0`; the executable's bound must admit both, which
`>=0.18 && <0.20` does, and the Nix build is what exercises the upper end.

[docs/design/020-json-wire-encodings.md](../design/020-json-wire-encodings.md) (plan 28) is the
source of truth for every field name the OpenAPI schemas repeat, and
[docs/design/021-inspection-surface-wire-contract.md](../design/021-inspection-surface-wire-contract.md)
(plans 29 and 30) is the source of truth for routes, error codes, and frames. The guide and the
OpenAPI document restate them; they do not get to disagree.

Cross-repository: `mori://shinzui/keiro-ui/okf/adrs/concepts/ADR-5` records that the keiro
console has no backend-for-frontend and that, if the surfaces publish machine-readable specs,
its hand-written clients are regenerated from them; the OpenAPI document is that spec for
pgmq-hs. The cross-project conventions are `mori://shinzui/keiro-ui`, path
`docs/architecture/inspection-api-conventions.md` (artifact-level URI pending); cite them in
the guide exactly that way, once, as the origin of the conventions pgmq-hs adopted.


## Plan of Work

### Milestone 1: the standalone executable

Scope: a program that serves the surface from a database URL, with the bind address, port,
CORS origins, poll interval, pool size, and listener switch on the command line. At the end,
`cabal run pgmq-inspect -- --version` prints `0.1.0.0`, and against the dev-shell database the
server starts, logs one line, and answers `GET /queues`. Acceptance is the transcript in
Validation and Acceptance under "Milestone 1".

Add the executable stanza to `pgmq-inspect/pgmq-inspect.cabal`, after the library stanza and
before the test suite:

```cabal
executable pgmq-inspect
  import: warnings
  default-language: GHC2024
  hs-source-dirs: app
  main-is: Main.hs
  ghc-options:
    -threaded
    -rtsopts
    "-with-rtsopts=-N"

  autogen-modules: Paths_pgmq_inspect
  other-modules: Paths_pgmq_inspect
  default-extensions:
    DuplicateRecordFields
    ImportQualifiedPost
    OverloadedLabels
    OverloadedStrings

  build-depends:
    async ^>=2.2,
    base >=4.18 && <5,
    bytestring >=0.11 && <0.13,
    generic-lens ^>=2.2 || ^>=2.3,
    hasql ^>=1.10,
    hasql-pool ^>=1.4,
    lens ^>=5.3,
    optparse-applicative >=0.18 && <0.20,
    pgmq-inspect,
    text >=2.0 && <2.2,
```

`cabal-gild` (run by `nix fmt`) sorts and aligns the stanza; let it. The `-with-rtsopts=-N`
is quoted because of the `=`. `Paths_pgmq_inspect` is generated by Cabal from the package
version; listing it under both `autogen-modules` and `other-modules` is what Cabal 3.4 requires.

Write `pgmq-inspect/app/Main.hs`:

```haskell
module Main (main) where

import Control.Concurrent.Async (wait)
import Control.Exception (bracket)
import Control.Lens ((&), (.~))
import Control.Monad (when)
import Data.ByteString.Char8 qualified as B8
import Data.Generics.Labels ()
import Data.List.NonEmpty (nonEmpty)
import Data.Text qualified as T
import Data.Version (showVersion)
import Hasql.Connection.Settings qualified as Settings
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as PoolConfig
import Options.Applicative
import Paths_pgmq_inspect (version)
import Pgmq.Inspect
  ( CorsPolicy (..),
    InspectServerConfig,
    RunningInspectServer (..),
    defaultInspectServerConfig,
    poolEnv,
    withInspectServer,
  )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, stderr, stdout)

data Options = Options
  { databaseUrl :: !(Maybe String),
    host :: !String,
    port :: !Int,
    corsOrigins :: ![String],
    pollIntervalMs :: !Int,
    poolSize :: !Int,
    noListener :: !Bool
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> optional
      ( strOption
          ( long "database-url"
              <> metavar "CONNINFO"
              <> help "libpq connection string or postgres:// URL; defaults to $PGMQ_INSPECT_DATABASE_URL"
          )
      )
    <*> strOption (long "host" <> metavar "HOST" <> value "127.0.0.1" <> showDefault <> help "Bind address")
    <*> option auto (long "port" <> metavar "PORT" <> value 9092 <> showDefault <> help "Bind port (0 picks a free port)")
    <*> many
      ( strOption
          ( long "cors-origin"
              <> metavar "ORIGIN"
              <> help "Allowed CORS origin, for example http://localhost:5173; repeatable; CORS stays disabled when absent"
          )
      )
    <*> option auto (long "poll-interval-ms" <> metavar "MS" <> value 5000 <> showDefault <> help "WebSocket authoritative poll interval")
    <*> option auto (long "pool-size" <> metavar "N" <> value 4 <> showDefault <> help "Number of pooled database connections")
    <*> switch (long "no-listener" <> help "Do not open a LISTEN connection; the WebSocket feed is poll-only")

parserInfo :: ParserInfo Options
parserInfo =
  info
    (optionsParser <**> helper <**> versionOption)
    ( fullDesc
        <> header "pgmq-inspect - read-only HTTP and WebSocket inspection of pgmq queues"
        <> progDesc "Serves the pgmq-inspect surface. The pgmq schema must already be installed in the target database."
    )
  where
    versionOption = infoOption (showVersion version) (long "version" <> help "Print the version and exit")

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  opts <- execParser parserInfo
  url <- resolveDatabaseUrl (databaseUrl opts)
  let settings = Settings.connectionString (T.pack url)
  when (settings == mempty) $
    failWith "the database URL is empty or not a valid libpq connection string"
  when (pollIntervalMs opts <= 0) $ failWith "--poll-interval-ms must be positive"
  when (poolSize opts <= 0) $ failWith "--pool-size must be positive"
  let cors = maybe CorsDisabled CorsAllowOrigins (nonEmpty (map B8.pack (corsOrigins opts)))
      serverConfig :: InspectServerConfig
      serverConfig =
        defaultInspectServerConfig
          & #bindHost .~ host opts
          & #bindPort .~ port opts
          & #inspect . #corsPolicy .~ cors
          & #inspect . #pollIntervalUs .~ pollIntervalMs opts * 1000
      poolConfig =
        PoolConfig.settings
          [ PoolConfig.size (poolSize opts),
            PoolConfig.staticConnectionSettings settings
          ]
  bracket (Pool.acquire poolConfig) Pool.release $ \pool -> do
    let env = poolEnv pool (if noListener opts then Nothing else Just settings)
    withInspectServer serverConfig env $ \running -> do
      putStrLn
        ( "pgmq-inspect "
            <> showVersion version
            <> " listening on http://"
            <> host opts
            <> ":"
            <> show (serverPort running)
        )
      wait (serverThread running)

resolveDatabaseUrl :: Maybe String -> IO String
resolveDatabaseUrl (Just url) = pure url
resolveDatabaseUrl Nothing = do
  fromEnv <- lookupEnv "PGMQ_INSPECT_DATABASE_URL"
  case fromEnv of
    Just url | not (null url) -> pure url
    _ -> failWith "no database URL: pass --database-url or set PGMQ_INSPECT_DATABASE_URL"

failWith :: String -> IO a
failWith msg = do
  hPutStrLn stderr ("pgmq-inspect: " <> msg)
  exitWith (ExitFailure 2)
```

Three points about this program deserve explanation. `Settings.connectionString` is documented
to treat an unparsable string as empty rather than failing, so the program compares the result
with `mempty` (the `Settings` newtype derives `Eq` and `Monoid`) and refuses to start, instead
of connecting with libpq's implicit defaults and letting the mistake surface later as a
readiness failure. The listener settings passed to `poolEnv` are the same settings the pool
uses; plan 30's `Pgmq.Inspect.Listener` appends its own `applicationName
"pgmq-inspect-listener"` when it acquires the connection (verify with
`grep -n applicationName pgmq-inspect/src/Pgmq/Inspect/Listener.hs`; if it does not, append
`<> Settings.applicationName "pgmq-inspect-listener"` here and record the discrepancy in
Surprises & Discoveries). The startup line is printed from inside `withInspectServer` with
`serverPort running` rather than `port opts`, because `--port 0` asks the operating system for
a free port and the handle is the only place the real number is known; the program then blocks
on the server thread, and `Ctrl-C` ends it through the usual `UserInterrupt` exception, which
`bracket` turns into a pool release.

Record-update syntax through `generic-lens` labels (`& #bindHost .~ …`) is the family
convention for field access; the `Data.Generics.Labels ()` import is what makes `#bindHost`
resolve. If plan 29 named the nested config field differently from `inspect`, adjust the label
to the real name.

Build and smoke-test the binary against the dev-shell database (schema installed as described
in Context and Orientation):

```bash
nix develop --command cabal build pgmq-inspect:exe:pgmq-inspect
nix develop --command cabal run pgmq-inspect -- --version
nix develop --command bash -c 'cabal run pgmq-inspect -- --database-url "$PG_CONNECTION_STRING" --port 9092'
```

Confirm the Nix build includes the executable: `nix build .#pgmq-inspect` must produce
`result/bin/pgmq-inspect`. `callCabal2nix` lists executable dependencies and builds every
component, so nothing in `nix/haskell-overlay.nix` needs to change unless the build fails; if
`optparse-applicative` resolution fails under Nix, the bound in the stanza is wrong (the overlay
pins `0.19.0.0`), not the overlay.

Commit:

```text
feat(inspect): add the standalone pgmq-inspect executable

Serve the inspection surface from a database URL with the bind address,
port, CORS origins, poll interval, pool size, and listener switch on the
command line. The URL falls back to PGMQ_INSPECT_DATABASE_URL and the
program refuses to start without a parsable one.

MasterPlan: docs/masterplans/7-expose-non-destructive-queue-inspection-through-typed-reads-json-codecs-and-the-pgmq-inspect-sister-package.md
ExecPlan: docs/plans/31-ship-the-standalone-pgmq-inspect-server-the-openapi-document-and-the-inspection-guide.md
Intention: intention_01m3tcw9vmeeftdtqj53d1d6nb
```

### Milestone 2: the OpenAPI document

Scope: a Haskell value describing every HTTP route, served at `GET /openapi.json`, pinned by a
golden file, and cross-checked against the router's route table by a test. At the end,
`curl http://127.0.0.1:9092/openapi.json | jq .info` prints the title and version, the service
descriptor carries `openapi_path`, and `cabal test pgmq-inspect:pgmq-inspect-test` is green
with three new cases.

Add to the library's `build-depends` in `pgmq-inspect/pgmq-inspect.cabal`:
`openapi3 ^>=3.2` and `insert-ordered-containers ^>=0.2`. Add `Pgmq.Inspect.OpenApi` to
`exposed-modules`. Add `extra-source-files: test/golden/*.json` if plan 29 did not already
(it should have, for the service-descriptor golden; check). Add `tasty-golden ^>=2.3` to the
test suite's `build-depends` if absent.

The `openapi3` library (`3.2.4` in the pinned package set; its source is unpacked under
`/private/tmp/claude-501/-Users-shinzui-Keikaku-bokuno-libraries-pgmq-hs-project-pgmq-hs/90a95def-cc2d-4814-9797-14a8bb36f553/scratchpad/deps/openapi3-3.2.4/src/Data/OpenApi/`
if you want to read it) models the document as plain records with `Monoid` instances, so a
document is built as `mempty { … }` with record updates. The records and fields this plan
uses, quoted from `Data.OpenApi.Internal`:

```haskell
data OpenApi = OpenApi
  { _openApiInfo :: Info
  , _openApiServers :: [Server]                       -- left empty: paths are relative to the mount root
  , _openApiPaths :: InsOrdHashMap FilePath PathItem
  , _openApiComponents :: Components
  , … }                                               -- _openApiOpenapi defaults to 3.0.0 via mempty

data Info = Info { _infoTitle :: Text, _infoDescription :: Maybe Text, …, _infoVersion :: Text }
data Components = Components { _componentsSchemas :: Definitions Schema, … }   -- Definitions = InsOrdHashMap Text
data PathItem = PathItem { …, _pathItemGet :: Maybe Operation, …, _pathItemParameters :: [Referenced Param] }
data Operation = Operation { …, _operationSummary :: Maybe Text, _operationDescription :: Maybe Text,
                             _operationOperationId :: Maybe Text, _operationParameters :: [Referenced Param],
                             _operationResponses :: Responses, … }
data Responses = Responses { _responsesDefault :: Maybe (Referenced Response),
                             _responsesResponses :: InsOrdHashMap HttpStatusCode (Referenced Response) }
data Response = Response { _responseDescription :: Text, _responseContent :: InsOrdHashMap MediaType MediaTypeObject, … }
data MediaTypeObject = MediaTypeObject { _mediaTypeObjectSchema :: Maybe (Referenced Schema), … }
data Param = Param { _paramName :: Text, _paramDescription :: Maybe Text, _paramRequired :: Maybe Bool,
                     _paramIn :: ParamLocation, …, _paramSchema :: Maybe (Referenced Schema), … }
data ParamLocation = ParamQuery | ParamHeader | ParamPath | ParamCookie
data Schema = Schema { _schemaDescription :: Maybe Text, _schemaRequired :: [ParamName], _schemaNullable :: Maybe Bool,
                       _schemaProperties :: InsOrdHashMap Text (Referenced Schema), _schemaType :: Maybe OpenApiType,
                       _schemaFormat :: Maybe Format, _schemaItems :: Maybe OpenApiItems, _schemaEnum :: Maybe [Value], … }
data OpenApiType = OpenApiString | OpenApiNumber | OpenApiInteger | OpenApiBoolean | OpenApiArray | OpenApiNull | OpenApiObject
data OpenApiItems = OpenApiItemsObject (Referenced Schema) | OpenApiItemsArray [Referenced Schema]
data Referenced a = Ref Reference | Inline a
newtype Reference = Reference { getReference :: Text }
```

`MediaType` comes from `Network.HTTP.Media` (re-exported by `Data.OpenApi`); the literal
`"application/json"` works with `OverloadedStrings`. `Ref (Reference "Message")` renders as
`{"$ref":"#/components/schemas/Message"}`.

Write `pgmq-inspect/src/Pgmq/Inspect/OpenApi.hs`. The module exports `openApiDocument :: OpenApi`,
`openApiJson :: LBS.ByteString` (the document encoded once, as a top-level value, so the route
does not re-encode on every request), and `documentedPaths :: [FilePath]` (the keys of the
paths map, for the coverage test). It does not import `Pgmq.Inspect.Http`; the router imports
this module, and importing in both directions would be a cycle. The shape of the module:

```haskell
module Pgmq.Inspect.OpenApi
  ( openApiDocument,
    openApiJson,
    documentedPaths,
  )
where

import Data.Aeson (Value, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict.InsOrd qualified as InsOrd
import Data.OpenApi
import Data.Text (Text)
import Data.Text qualified as T
import Data.Version (showVersion)
import Paths_pgmq_inspect (version)

openApiDocument :: OpenApi
openApiDocument =
  mempty
    { _openApiInfo =
        mempty
          { _infoTitle = "pgmq-inspect",
            _infoVersion = T.pack (showVersion version),
            _infoDescription =
              Just
                "Read-only inspection of pgmq queues. Every path is relative to wherever the \
                \application is mounted. Field names are pgmq's own SQL column names in snake_case; \
                \once published they are never removed or re-typed. The WebSocket feed at ws is \
                \described in docs/user/queue-inspection.md, not here."
          },
      _openApiPaths = InsOrd.fromList paths,
      _openApiComponents = mempty {_componentsSchemas = InsOrd.fromList schemas}
    }

openApiJson :: LBS.ByteString
openApiJson = encode openApiDocument

documentedPaths :: [FilePath]
documentedPaths = map fst paths
```

`paths :: [(FilePath, PathItem)]` lists one entry per HTTP route, in the order of the route
table, with placeholders rendered as `{queue}` and `{msg_id}`:

```haskell
paths :: [(FilePath, PathItem)]
paths =
  [ ("/", getOp "serviceDescriptor" "Service descriptor" [] (ok "ServiceDescriptor") []),
    ("/queues", getOp "listQueues" "List every queue, including names this library would reject" [] (okArray "Queue") []),
    ("/queues/{queue}", getOp "getQueue" "One queue" [queueParam] (ok "Queue") [(404, "queue_not_found")]),
    ("/queues/{queue}/metrics", getOp "getQueueMetrics" "Metrics for one queue" [queueParam] (ok "QueueMetrics") [(404, "queue_not_found")]),
    ("/metrics", getOp "getAllMetrics" "Metrics for every queue; costs one count per queue, so poll it sparingly" [] (okArray "QueueMetrics") []),
    ("/queues/{queue}/messages", getOp "peekMessages" "Non-destructive keyset page of a queue" [queueParam, fromParam, limitParam] (ok "MessagePage") [(400, "invalid_cursor, invalid_limit"), (404, "queue_not_found")]),
    ("/queues/{queue}/messages/{msg_id}", getOp "lookupMessage" "One message by id" [queueParam, msgIdParam] (ok "Message") [(400, "invalid_message_id"), (404, "queue_not_found, message_not_found")]),
    ("/queues/{queue}/archive", getOp "peekArchive" "Keyset page of a queue's archive" [queueParam, fromParam, limitParam] (ok "ArchivedMessagePage") [(400, "invalid_cursor, invalid_limit"), (404, "queue_not_found")]),
    ("/queues/{queue}/archive/{msg_id}", getOp "lookupArchivedMessage" "One archived message by id" [queueParam, msgIdParam] (ok "ArchivedMessage") [(400, "invalid_message_id"), (404, "queue_not_found, message_not_found")]),
    ("/queues/{queue}/bindings", getOp "listQueueBindings" "Topic bindings of one queue" [queueParam] (okArray "TopicBinding") []),
    ("/bindings", getOp "listBindings" "Every topic binding" [] (okArray "TopicBinding") []),
    ("/routing/test", getOp "testRouting" "Queues a routing key would reach" [routingKeyParam] (okArray "RoutingMatch") [(400, "invalid_routing_key")]),
    ("/notify/throttles", getOp "listNotifyThrottles" "Insert-notification throttle rows" [] (okArray "NotifyInsertThrottle") []),
    ("/health/live", getOp "liveness" "Process liveness" [] (ok "Liveness") []),
    ("/health/ready", getOp "readiness" "Database readiness" [] (ok "Readiness") [(503, "not ready; same body")]),
    ("/openapi.json", getOp "openApi" "This document" [] (okAny "The OpenAPI 3 document") [])
  ]
```

Every operation also carries the two responses common to all routes, `503 database_unavailable`
and `500 database_error`, and the method-level `405 method_not_allowed`; `getOp` adds them.
`getOp` returns a `PathItem` with only `_pathItemGet` set; the error-response descriptions name
the codes so a reader of the document (or of a generated client's docs) knows what to switch on:

```haskell
getOp :: Text -> Text -> [Referenced Param] -> Referenced Response -> [(Int, Text)] -> PathItem
getOp opId summary params success errors =
  mempty
    { _pathItemGet =
        Just
          mempty
            { _operationOperationId = Just opId,
              _operationSummary = Just summary,
              _operationParameters = params,
              _operationResponses =
                mempty
                  { _responsesResponses =
                      InsOrd.fromList
                        ( (200, success)
                            : [(status, errorResponse codes) | (status, codes) <- errors]
                            <> [ (405, errorResponse "method_not_allowed"),
                                 (500, errorResponse "database_error"),
                                 (503, errorResponse "database_unavailable")
                               ]
                        )
                  }
            }
    }

errorResponse :: Text -> Referenced Response
errorResponse codes =
  Inline
    mempty
      { _responseDescription = "Error envelope; code is one of: " <> codes,
        _responseContent = InsOrd.singleton "application/json" (jsonOf (Ref (Reference "ErrorEnvelope")))
      }

ok :: Text -> Referenced Response
ok name = Inline mempty {_responseDescription = "OK", _responseContent = InsOrd.singleton "application/json" (jsonOf (Ref (Reference name)))}

okArray :: Text -> Referenced Response
okArray name = Inline mempty {_responseDescription = "OK", _responseContent = InsOrd.singleton "application/json" (jsonOf (Inline (arrayOf (Ref (Reference name)))))}

okAny :: Text -> Referenced Response
okAny description = Inline mempty {_responseDescription = description, _responseContent = InsOrd.singleton "application/json" (jsonOf (Inline mempty {_schemaType = Just OpenApiObject}))}

jsonOf :: Referenced Schema -> MediaTypeObject
jsonOf s = mempty {_mediaTypeObjectSchema = Just s}
```

The parameters:

```haskell
queueParam, msgIdParam, fromParam, limitParam, routingKeyParam :: Referenced Param
queueParam = Inline mempty {_paramName = "queue", _paramIn = ParamPath, _paramRequired = Just True, _paramDescription = Just "Queue name as stored in pgmq.meta; any server-accepted name, not only names this library validates", _paramSchema = Just (Inline (typed OpenApiString))}
msgIdParam = Inline mempty {_paramName = "msg_id", _paramIn = ParamPath, _paramRequired = Just True, _paramSchema = Just (Inline (typed OpenApiInteger) {_schemaFormat = Just "int64"})}
fromParam = Inline mempty {_paramName = "from", _paramIn = ParamQuery, _paramRequired = Just False, _paramDescription = Just "Exclusive msg_id cursor; the page starts strictly after it. Echo next_cursor here.", _paramSchema = Just (Inline (typed OpenApiInteger) {_schemaFormat = Just "int64"})}
limitParam = Inline mempty {_paramName = "limit", _paramIn = ParamQuery, _paramRequired = Just False, _paramDescription = Just "Page size; defaults to 50 and is capped at 500", _paramSchema = Just (Inline (typed OpenApiInteger) {_schemaFormat = Just "int32"})}
routingKeyParam = Inline mempty {_paramName = "routing_key", _paramIn = ParamQuery, _paramRequired = Just True, _paramSchema = Just (Inline (typed OpenApiString))}
```

If plan 29 made `defaultPageLimit` or `maxPageLimit` configurable with other defaults, the
descriptions must quote the real defaults from `Pgmq.Inspect.Config`.

The schemas are hand-declared, one per wire record, with the exact field names fixed by
`docs/design/020-json-wire-encodings.md` and plan 29's wire types. They are not a second source
of truth: plan 28's golden tests in `pgmq-core`, `pgmq-hasql`, and `pgmq-config` and plan 29's
goldens in `pgmq-inspect` already pin those names, so a field rename would break those goldens
and the OpenAPI golden together, and the two cannot drift independently without two deliberate
golden updates in one change. Helpers and the first few schemas:

```haskell
typed :: OpenApiType -> Schema
typed t = mempty {_schemaType = Just t}

nullable :: Schema -> Schema
nullable s = s {_schemaNullable = Just True}

dateTime :: Schema
dateTime = (typed OpenApiString) {_schemaFormat = Just "date-time"}

int64, int32 :: Schema
int64 = (typed OpenApiInteger) {_schemaFormat = Just "int64"}
int32 = (typed OpenApiInteger) {_schemaFormat = Just "int32"}

arrayOf :: Referenced Schema -> Schema
arrayOf item = (typed OpenApiArray) {_schemaItems = Just (OpenApiItemsObject item)}

objectOf :: [(Text, Schema)] -> Schema
objectOf fields =
  (typed OpenApiObject)
    { _schemaProperties = InsOrd.fromList [(name, Inline s) | (name, s) <- fields],
      _schemaRequired = map fst fields   -- every key is always present; nullable keys carry null
    }

messageFields :: [(Text, Schema)]
messageFields =
  [ ("msg_id", int64),
    ("read_ct", int32),
    ("enqueued_at", dateTime),
    ("last_read_at", nullable dateTime),
    ("vt", dateTime),
    ("message", mempty {_schemaDescription = Just "Any JSON value; a SQL NULL body is JSON null"}),
    ("headers", nullable (typed OpenApiObject))
  ]

schemas :: [(Text, Schema)]
schemas =
  [ ("Queue", objectOf [("queue_name", typed OpenApiString), ("is_partitioned", typed OpenApiBoolean), ("is_unlogged", typed OpenApiBoolean), ("created_at", dateTime)]),
    ("Message", objectOf messageFields),
    ("ArchivedMessage", objectOf (messageFields <> [("archived_at", dateTime)])),
    ("QueueMetrics", objectOf [("queue_name", typed OpenApiString), ("queue_length", int64 {_schemaDescription = Just "Every message in the queue table, visible or not"}), ("queue_visible_length", int64 {_schemaDescription = Just "Messages whose visibility timeout has expired, i.e. available to consumers now"}), ("newest_msg_age_sec", nullable int32), ("oldest_msg_age_sec", nullable int32), ("total_messages", int64), ("scrape_time", dateTime), ("default_partition_length", nullable int64 {_schemaDescription = Just "PGMQ 1.13 planner estimate of rows in the queue and archive default partitions; null on 1.12 and for ordinary queues"})]),
    ("TopicBinding", objectOf [("pattern", typed OpenApiString), ("queue_name", typed OpenApiString), ("bound_at", dateTime), ("compiled_regex", typed OpenApiString)]),
    ("RoutingMatch", objectOf [("pattern", typed OpenApiString), ("queue_name", typed OpenApiString), ("compiled_regex", typed OpenApiString)]),
    ("NotifyInsertThrottle", objectOf [("queue_name", typed OpenApiString), ("throttle_interval_ms", int32), ("last_notified_at", dateTime)]),
    ("MessagePage", page "Message"),
    ("ArchivedMessagePage", page "ArchivedMessage"),
    ("ErrorBody", (objectOf [("code", typed OpenApiString), ("message", typed OpenApiString)]) {_schemaProperties = …, _schemaRequired = ["code", "message"]}),   -- details is optional; add it as a non-required object property
    ("ErrorEnvelope", (typed OpenApiObject) {_schemaProperties = InsOrd.singleton "error" (Ref (Reference "ErrorBody")), _schemaRequired = ["error"]}),
    ("ServiceDescriptor", objectOf [("service", typed OpenApiString), ("version", typed OpenApiString), ("websocket_path", typed OpenApiString), ("openapi_path", typed OpenApiString)]),
    ("Liveness", objectOf [("alive", typed OpenApiBoolean)]),
    ("Readiness", objectOf [("ready", typed OpenApiBoolean), ("checks", arrayOf (Ref (Reference "DependencyCheck")))]),
    ("DependencyCheck", objectOf [("name", typed OpenApiString), ("healthy", typed OpenApiBoolean), ("latency_ms", nullable int64), ("error", nullable (typed OpenApiString))])
  ]
  where
    page item =
      (typed OpenApiObject)
        { _schemaProperties = InsOrd.fromList [("items", Inline (arrayOf (Ref (Reference item)))), ("next_cursor", Inline int64 {_schemaDescription = Just "msg_id of the last item; absent on the last page"})],
          _schemaRequired = ["items"]
        }
```

Write `ErrorBody` out fully rather than with the `…` above: properties `code`, `message`, and
`details` (an object), required `code` and `message`. Compare every schema against the real
wire types in `pgmq-inspect/src/Pgmq/Inspect/Wire.hs` and the real instances plan 28 wrote
before committing; where plan 29's health or descriptor bodies differ from the shapes above,
the code is right and this plan's text is a draft to correct, and the correction goes in
Surprises & Discoveries.

Serve the document. In `pgmq-inspect/src/Pgmq/Inspect/Http.hs`, import
`Pgmq.Inspect.OpenApi (openApiJson)`, add a case to the router for `pathInfo == ["openapi.json"]`
on `GET` that responds `200` with content type `application/json` and body `openApiJson`
(use whatever JSON-response helper plan 29 wrote for the other routes), and add
`(methodGet, [Literal "openapi.json"])` to `routeTable` next to the service-descriptor entry.
In `pgmq-inspect/src/Pgmq/Inspect/Wire.hs`, add the field `openapiPath :: !Text` to the
service-descriptor record and the key `"openapi_path"` (value `"openapi.json"`, relative to the
mount root like `websocket_path`) to its `ToJSON` instance; update the construction site in
`Http.hs` and the service-descriptor golden plan 29 committed. In `pgmq-inspect/src/Pgmq/Inspect.hs`,
re-export `openApiDocument`.

Write `pgmq-inspect/test/OpenApiSpec.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

-- | The OpenAPI document is the machine-readable half of the wire contract.
-- Three things must hold: its rendering is pinned (golden), it covers every
-- HTTP route the router serves (coverage), and the served copy is a document
-- the openapi3 library itself accepts (round trip).
module OpenApiSpec (tests) where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as LBS
import Data.List (intercalate, sort)
import Data.OpenApi (OpenApi)
import Data.Text qualified as T
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (methodGet, statusCode)
import Pgmq.Inspect.Http (PathSegment (..), routeTable)
import Pgmq.Inspect.OpenApi (documentedPaths, openApiJson)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (assertEqual, testCase, (@?=))
import TestServer (withTestServer)   -- plan 29's harness; adjust the name to the real export

tests :: TestTree
tests =
  testGroup
    "OpenAPI"
    [ goldenVsString "openapi.json is pinned" "test/golden/openapi.json" (pure openApiJson),
      testCase "every HTTP route is documented" $ do
        let served = sort [renderPath segs | (method, segs) <- routeTable, method == methodGet, segs /= [Literal "ws"]]
        assertEqual "documented paths" served (sort documentedPaths),
      testCase "GET /openapi.json serves a decodable document" $
        withTestServer $ \port -> do
          manager <- HTTP.newManager HTTP.defaultManagerSettings
          request <- HTTP.parseRequest ("http://127.0.0.1:" <> show port <> "/openapi.json")
          response <- HTTP.httpLbs request manager
          statusCode (HTTP.responseStatus response) @?= 200
          case eitherDecode (HTTP.responseBody response) :: Either String OpenApi of
            Left err -> fail ("served document does not decode: " <> err)
            Right _ -> pure ()
    ]

renderPath :: [PathSegment] -> FilePath
renderPath [] = "/"
renderPath segs = "/" <> intercalate "/" (map render segs)
  where
    render (Literal t) = T.unpack t
    render (Placeholder n) = "{" <> T.unpack n <> "}"
```

The harness name and signature come from plan 29's `TestServer.hs`; read it and use what it
exports (it may take an `InspectConfig` or an `InspectEnv`). The `/ws` route is excluded from
coverage because the document describes HTTP only; the guide describes the frames. Register
`OpenApiSpec` in `pgmq-inspect/test/Main.hs` and in the cabal `other-modules`.

Generate the golden the first time with tasty-golden's accept flag, then inspect it with `jq`
before committing it:

```bash
nix develop --command cabal test pgmq-inspect:pgmq-inspect-test --test-options='--accept -p OpenAPI'
jq '.info, (.paths | keys)' pgmq-inspect/test/golden/openapi.json
```

The golden is the compact `encode` output on one line; that is deliberate (see the Decision
Log), and `jq` is how a reviewer reads it. Note that the version string inside the golden is the
package version, so a future version bump regenerates it with the same `--accept` command and
the diff is one field.

Commit:

```text
feat(inspect): serve a golden-pinned OpenAPI document

Build the OpenAPI 3 document programmatically from the route table's
shape, serve it at GET /openapi.json, advertise it from the service
descriptor, and pin it with a golden file plus a route-coverage test so
the router and the document cannot drift apart silently.

MasterPlan: docs/masterplans/7-expose-non-destructive-queue-inspection-through-typed-reads-json-codecs-and-the-pgmq-inspect-sister-package.md
ExecPlan: docs/plans/31-ship-the-standalone-pgmq-inspect-server-the-openapi-document-and-the-inspection-guide.md
Intention: intention_01m3tcw9vmeeftdtqj53d1d6nb
```

### Milestone 3: the guide, the records, and the release handoff

Scope: the pgmq-hs-owned user guide, the README section, the registry entry, the capability
record, the closure of `IR-3`, and the changelog sections with the release handoff. At the end,
`just docs-check` and `mori validate` pass and a reader of the repository alone can run,
embed, call, subscribe to, and build a UI against the surface.

Write `docs/user/queue-inspection.md`. Follow the voice and shape of
`docs/user/queue-configuration.md`: a short orientation, then sections a reader can jump to,
code and transcripts in fenced blocks with language tags. Its sections and what each must say:

**What this is.** `pgmq-inspect` is a read-only HTTP and WebSocket surface over pgmq queues,
shipped as a sister package so adopting pgmq-hs never means adopting a web server. It serves
two readers: a Haskell host that mounts the `Application` beside other surfaces (the keiro
runtime console does this), and anyone who runs the executable and points a browser, a
dashboard, or a script at it. Cite
[the boundary ADR](../adr/queue-inspection-surface-boundary-and-wire-contract.md) and, once,
the origin of the conventions: `mori://shinzui/keiro-ui`, path
`docs/architecture/inspection-api-conventions.md` (artifact-level URI pending).

**Running the standalone server.** The prerequisite (the pgmq schema installed, by
`pgmq-migration` per [schema-migration.md](schema-migration.md) or by the extension), the
command, the startup line, the flags in prose (`--database-url` or `PGMQ_INSPECT_DATABASE_URL`;
`--host`, default loopback; `--port`, default 9092, `0` for a free port; `--cors-origin`,
repeatable; `--poll-interval-ms`; `--pool-size`; `--no-listener`; `--version`), and the
readiness probe as the way to confirm the database and schema are reachable:

```bash
pgmq-inspect --database-url "postgres://inspect:secret@db.internal:5432/app" --port 9092
# pgmq-inspect 0.1.0.0 listening on http://127.0.0.1:9092
curl -s http://127.0.0.1:9092/health/ready
# {"ready":true,"checks":[{"name":"postgres","healthy":true,"latency_ms":2,"error":null}]}
```

State that the database role needs only `SELECT` on the `pgmq` schema (pgmq grants
`pg_monitor` exactly that) plus `LISTEN`, which any role may issue.

**Embedding in your own server.** `withInspectApplication` for a host that already runs Warp,
`poolEnv` versus `tracedPoolEnv` (the traced variant records every database call as an
OpenTelemetry span through `pgmq-effectful`'s traced interpreter), and mounting under a prefix:
the application routes on WAI `pathInfo`, so a host strips its prefix before delegating. Show
the smallest honest example:

```haskell
import Network.Wai (Application, pathInfo)
import Network.Wai.Handler.Warp qualified as Warp
import Pgmq.Inspect (defaultInspectConfig, poolEnv, withInspectApplication)

main :: IO ()
main = do
  pool <- acquirePool                       -- your hasql-pool
  let env = poolEnv pool (Just connectionSettings)
  withInspectApplication defaultInspectConfig env $ \pgmqApp ->
    Warp.run 8080 (mountUnder "pgmq" pgmqApp yourApp)

-- Requests whose first path segment is the prefix go to the mounted app
-- with the prefix removed; everything else goes to your own application.
mountUnder :: Text -> Application -> Application -> Application
mountUnder prefix mounted fallback req respond =
  case pathInfo req of
    (p : rest) | p == prefix -> mounted req {pathInfo = rest} respond
    _ -> fallback req respond
```

Say that `rawPathInfo` is left untouched by this example and that the WebSocket upgrade still
works because the application dispatches it from `pathInfo`, not from the raw path. Note that a
host that mounts several surfaces this way serves one origin, which is the deployment that
needs no CORS at all.

**The HTTP API.** One subsection per route group, each with a `curl` transcript whose JSON is
realistic and uses the exact field names. The service descriptor:

```bash
curl -s http://127.0.0.1:9092/
# {"service":"pgmq-inspect","version":"0.1.0.0","websocket_path":"ws","openapi_path":"openapi.json"}
```

Say the two paths are relative to the mount root. Queues:

```bash
curl -s http://127.0.0.1:9092/queues
# [{"queue_name":"orders","is_partitioned":false,"is_unlogged":false,"created_at":"2026-10-01T00:15:41.123456Z"},
#  {"queue_name":"MyQueue","is_partitioned":false,"is_unlogged":true,"created_at":"2026-09-30T22:03:10.5Z"}]
```

and explain the second row: the listing is lenient, so a queue created by another client under
a name this library's `parseQueueName` rejects (mixed case here) is shown rather than hidden,
and the per-queue routes accept it as a path segment. Metrics, with the total-versus-visible
distinction spelled out (`queue_length` counts every row in the queue table; `queue_visible_length`
counts rows whose visibility timeout has expired, which is the work available to consumers
right now; their difference is the number of messages currently leased) and the cost warning
for `GET /metrics` (pgmq's `metrics_all()` loops over `pgmq.meta` and runs a `count(*)` per
queue, so it is O(number of queues) scans; poll it no more often than every five seconds, and
prefer `GET /queues/{queue}/metrics` for a single hot queue):

```bash
curl -s http://127.0.0.1:9092/queues/orders/metrics
# {"queue_name":"orders","queue_length":12,"queue_visible_length":9,"newest_msg_age_sec":3,"oldest_msg_age_sec":418,
#  "total_messages":10342,"scrape_time":"2026-10-01T00:16:02.001Z","default_partition_length":null}
```

Browsing, with pagination demonstrated end to end: a first page, the `next_cursor` echoed as
`from`, and a last page without `next_cursor`:

```bash
curl -s 'http://127.0.0.1:9092/queues/orders/messages?limit=2'
# {"items":[{"msg_id":10331,"read_ct":0,"enqueued_at":"…","last_read_at":null,"vt":"…","message":{"order_id":7},"headers":null},
#           {"msg_id":10332,"read_ct":2,"enqueued_at":"…","last_read_at":"…","vt":"…","message":{"order_id":8},"headers":{"x-pgmq-group":"eu"}}],
#  "next_cursor":10332}
curl -s 'http://127.0.0.1:9092/queues/orders/messages?from=10332&limit=2'
# {"items":[{"msg_id":10333,…}]}
```

State the rules a client must follow: the cursor is opaque (echo it, never add `limit` to it,
never assume contiguity), `from` is exclusive, `limit` defaults to 50 and is capped at 500, and
the absence of `next_cursor` is the end-of-data signal. State the non-destructive guarantee in
one sentence: browsing a queue over HTTP reads the rows and changes nothing, so `vt` and
`read_ct` are exactly what a consumer would see, and a consumer polling the same queue while
you browse observes no change in availability. Then the archive (same shape plus
`archived_at`), message-by-id in both tables with the 404 body, bindings, routing test, and
throttles, each with a short transcript. Health, with both status codes.

**Errors.** The envelope and a table of every code with its status and when it occurs; copy
the code list from Context and Orientation. Say that `message` is a sentence safe to display
and free to change, that `code` is stable and safe to switch on, and that `details` is present
only when there is something structured to add (for example the SQLSTATE under
`database_error`).

**The WebSocket feed.** Connect to `ws` (relative to the mount root; `ws://127.0.0.1:9092/ws`
standalone), then every frame with an example, in lifecycle order: `subscribe` → `snapshot`
(with `push` explained: `true` when the queue's name validates and the server has a listener
connection, `false` when updates can only come from polling) → `update` frames with `source`
`notify` or `poll` → `unsubscribe`; `ping`/`pong`; `error` with its codes (`queue_not_found`,
`invalid_frame`, `too_many_subscriptions`, `overflow`, `database_unavailable`); `goodbye`.
State the contract in the words of design note 015 and the boundary ADR: the feed carries
metrics, not messages; NOTIFY is a wake-up hint that is throttled and lost for disconnected
listeners, so the server polls every subscribed queue on an interval regardless, and a client
re-reads messages through the paged HTTP routes whenever it wants content. Explain overflow
recovery: delivery queues are bounded (256 frames by default) and drop the oldest frame on
overflow, and the client learns this through an `error` frame with code `overflow`, after which
it re-reads from HTTP rather than trusting what it has. A complete session:

```text
→ {"type":"subscribe","queue":"orders"}
← {"type":"snapshot","queue":"orders","metrics":{"queue_name":"orders","queue_length":12,…},"push":true}
← {"type":"update","queue":"orders","metrics":{…,"queue_length":13,…},"source":"notify"}
← {"type":"update","queue":"orders","metrics":{…,"queue_length":13,"queue_visible_length":11,…},"source":"poll"}
→ {"type":"ping"}
← {"type":"pong"}
→ {"type":"unsubscribe","queue":"orders"}
← {"type":"goodbye"}      (only when the server closes; a client closing receives nothing)
```

**CORS.** Disabled by default, which means no CORS header on any response and therefore no
browser page from another origin can call the API. Enable it by listing origins exactly
(`--cors-origin http://localhost:5173 --cors-origin https://ops.example.com`, or
`CorsAllowOrigins` in `InspectConfig`); there is no wildcard and credentials are never
allowed, by construction. The alternative that needs no CORS: serve the UI and reverse-proxy
the API from one origin.

**Security posture.** No authentication, no authorization, read-only. The surface assumes a
trusted network or an authenticating reverse proxy in front of it; never expose it bare to an
untrusted network; message bodies are returned verbatim, so anyone who can reach the surface
can read every payload. Say this plainly, as the conventions and the ADR require.

**Building your own UI or dashboard.** Discover the entry points from `GET /`, generate a
client from `openapi.json` (for example `openapi-typescript` for a TypeScript client, or any
OpenAPI 3 generator), subscribe over `ws` for liveness and re-read over HTTP for content, and
rely on the wire-freeze promise: a field, route, or frame that has shipped in a release is never
removed or re-typed, additions are always optional, and anything incompatible arrives as a new
path or frame type. Mention that `pgmq-ui` is the name reserved for a browser UI pgmq-hs may
ship later and that it will consume exactly these artifacts, so a UI built today does not
become a dead end.

Register the guide in `mori.dhall`, after the `queue-configuration` entry in `docs`:

```dhall
      , Schema.DocRef::{ key = "queue-inspection"
        , kind = Schema.DocKind.Guide
        , audience = Schema.DocAudience.User
        , description = Some
            "HTTP and WebSocket queue inspection with pgmq-inspect"
        , location =
            Schema.DocLocation.LocalFile "./docs/user/queue-inspection.md"
        }
```

and run `mori validate`.

Add a `## pgmq-inspect` section to `README.md` between the `pgmq-config` and `pgmq-migration`
sections (plan 29 already added the row to the package table), modelled on the `pgmq-config`
section: two sentences on what it is, the standalone command and the startup line, a four-line
`withInspectApplication` snippet, the sentence that it is read-only and assumes a trusted
network, and a link to `docs/user/queue-inspection.md`.

Extend the capability record plan 29 created (`grep -ln "pgmq-inspect" docs/capabilities/*.md`).
Add to its `evidence` list:

```yaml
  - kind: test
    resource: pgmq-inspect/test/OpenApiSpec.hs
    proves: The served OpenAPI document is golden-pinned, covers every HTTP route in the route table, and decodes with the openapi3 library.
  - kind: guide
    resource: docs/user/queue-inspection.md
    proves: The complete pgmq-hs-owned wire contract, standalone and embedded operation, and the independent-UI path.
```

and add a paragraph to its body naming the `pgmq-inspect` executable, `GET /openapi.json`, and
the guide, so the record says the surface is adoptable without a Haskell host. Append a log
entry with `okf log add docs/capabilities --kind Update -m "…"` (the message names the record's
handle, which `grep -n capabilityId <file>` prints). Run `just docs-check`.

Close `IR-3`. In
`docs/improvement-requests/add-a-pgmq-metrics-sister-package-with-http-and-websocket-inspection-endpoints.md`
set `status: completed`, add `completedAt: "<UTC now, RFC 3339>"`, and add a `resolution` that
walks the request's six acceptance items and names where each is proven: items 1 (queue
listing over HTTP), 2 (total versus visible depth and the documented O(queues) cost), 3 (the
non-destructive guarantee end to end), 5 (CORS off by default, on with an allowed origin), and
6 (no library package gains a web dependency; the bare `Application` is exported) in plan 29's
`pgmq-inspect/test/HttpSpec.hs` (confirm the file name with `ls pgmq-inspect/test`), and item
4 (snapshot then notifications; killing the LISTEN connection does not wedge the feed) in plan
30's `pgmq-inspect/test/WebSocketSpec.hs`. State that the final package name is `pgmq-inspect`,
not the working name in the title, and link the guide. The frontmatter line order and the
`okf` profile's field names are exactly as `docs/plans/26-…` describes for `IR-4`: `completedAt`
is required for `completed` and `resolution` is recommended. Append a body section under a
level-two heading named `Resolution` (the same heading level as the request's existing
`Problem`, `Requested Change`, `Acceptance`, and `Non-goals` sections) with this paragraph:

```text
Implemented as the `pgmq-inspect` package under
[MasterPlan 7](../masterplans/7-expose-non-destructive-queue-inspection-through-typed-reads-json-codecs-and-the-pgmq-inspect-sister-package.md).
The surface is documented in [docs/user/queue-inspection.md](../user/queue-inspection.md),
served as an OpenAPI document at `GET /openapi.json`, and runnable on its own as the
`pgmq-inspect` executable.
```

Then `okf log add docs/improvement-requests --kind Update -m "IR-3 completed: pgmq-inspect ships the HTTP and WebSocket inspection surface, a standalone executable, an OpenAPI document, and the queue inspection guide (ExecPlan 31)"`
and `just docs-check` again.

Changelogs. In `pgmq-inspect/CHANGELOG.md` (created by plan 29), under its `Unreleased`
heading, add entries for the executable, the OpenAPI route and the `openapi_path` descriptor
field, and the guide. In the root `CHANGELOG.md`, under the `Unreleased` heading plans 27
through 30 already extended (create it as `## Unreleased` above the newest dated entry if it is
somehow absent), add a paragraph for this plan and then the release handoff paragraph:

```markdown
Release handoff: the next lockstep release of the family must include the new `pgmq-inspect`
package (first release `0.1.0.0`), move its internal `pgmq-core`, `pgmq-hasql`, and
`pgmq-effectful` bounds with the family, and list the four constructors added to the `Pgmq`
effect (`PeekMessages`, `PeekArchivedMessages`, `LookupMessage`, `LookupArchivedMessage`) as a
breaking change for interpreters that match the effect exhaustively. That release is prepared
by MasterPlan 6's plan 25 or its successor; this initiative bumps no version.
```

Write the same paragraph, headed "Release handoff", into this plan's Outcomes & Retrospective.

Commit:

```text
docs(inspect): write the queue inspection guide and close IR-3

Document every route, error code, and WebSocket frame of pgmq-inspect in
a pgmq-hs-owned guide, add the README section and registry entry, extend
the capability record with the executable, OpenAPI, and guide evidence,
mark IR-3 completed, and record the release handoff.

MasterPlan: docs/masterplans/7-expose-non-destructive-queue-inspection-through-typed-reads-json-codecs-and-the-pgmq-inspect-sister-package.md
ExecPlan: docs/plans/31-ship-the-standalone-pgmq-inspect-server-the-openapi-document-and-the-inspection-guide.md
Intention: intention_01m3tcw9vmeeftdtqj53d1d6nb
```


## Concrete Steps

All commands run from the repository root,
`/Users/shinzui/Keikaku/bokuno/libraries/pgmq-hs-project/pgmq-hs`, inside `nix develop`
(prefix one-off commands with `nix develop --command`).

Preflight:

```bash
git status --short            # must be empty
git branch --show-current     # master; commit directly, no feature branch
# run every grep from "What this plan assumes already exists"; each must print
```

Start the development database and install the schema (once per cluster):

```bash
just process-up               # or the pg_ctl + just create-database pair from Context and Orientation
cabal build pgmq-migration
cabal exec -- runghc /private/tmp/claude-501/-Users-shinzui-Keikaku-bokuno-libraries-pgmq-hs-project-pgmq-hs/90a95def-cc2d-4814-9797-14a8bb36f553/scratchpad/install-pgmq-schema.hs "$PG_CONNECTION_STRING"
```

Expected on first run, one line per migration:

```text
MigrationReport {… 0001-install-v1.11.0 AppliedNow … 0006-preserve-partitioned-reentry-v1.13.0 AppliedNow …}
```

Milestone 1:

```bash
mkdir -p pgmq-inspect/app
# write pgmq-inspect/app/Main.hs and add the executable stanza (Plan of Work, M1)
nix fmt
cabal build pgmq-inspect:exe:pgmq-inspect
cabal run pgmq-inspect -- --version
cabal run pgmq-inspect -- --help
```

Expected:

```text
0.1.0.0
```

and a help screen listing `--database-url`, `--host`, `--port`, `--cors-origin`,
`--poll-interval-ms`, `--pool-size`, `--no-listener`, `--version`. Then, in one terminal:

```bash
cabal run pgmq-inspect -- --database-url "$PG_CONNECTION_STRING" --port 9092
```

Expected:

```text
pgmq-inspect 0.1.0.0 listening on http://127.0.0.1:9092
```

and in another terminal (same shell environment), the Validation transcript for Milestone 1.
Stop the server with `Ctrl-C`. Then:

```bash
cabal run pgmq-inspect                      # expect: "pgmq-inspect: no database URL: …" and exit status 2
echo $?
cabal run pgmq-inspect -- --database-url ""  # expect: "… empty or not a valid libpq connection string", status 2
PGMQ_INSPECT_DATABASE_URL="$PG_CONNECTION_STRING" cabal run pgmq-inspect -- --port 0
                                            # expect a startup line with an OS-assigned port; Ctrl-C
nix build .#pgmq-inspect && ./result/bin/pgmq-inspect --version
```

Commit with the Milestone 1 message.

Milestone 2:

```bash
# write pgmq-inspect/src/Pgmq/Inspect/OpenApi.hs; edit the cabal file, Http.hs, Wire.hs, Pgmq/Inspect.hs, test/Main.hs
# write pgmq-inspect/test/OpenApiSpec.hs
nix fmt
cabal build pgmq-inspect
cabal test pgmq-inspect:pgmq-inspect-test --test-options='--accept -p OpenAPI'   # first run writes the golden
cabal test pgmq-inspect:pgmq-inspect-test --test-options='--accept -p "service descriptor"'  # regenerates plan 29's descriptor golden; use its real test name
jq '.openapi, .info, (.paths | keys)' pgmq-inspect/test/golden/openapi.json
git diff --stat pgmq-inspect/test/golden/
cabal test pgmq-inspect:pgmq-inspect-test
```

Expected from `jq`:

```text
"3.0.0"
{
  "title": "pgmq-inspect",
  "version": "0.1.0.0",
  "description": "Read-only inspection of pgmq queues. …"
}
[
  "/",
  "/bindings",
  "/health/live",
  "/health/ready",
  "/metrics",
  "/notify/throttles",
  "/openapi.json",
  "/queues",
  "/queues/{queue}",
  "/queues/{queue}/archive",
  "/queues/{queue}/archive/{msg_id}",
  "/queues/{queue}/bindings",
  "/queues/{queue}/messages",
  "/queues/{queue}/messages/{msg_id}",
  "/queues/{queue}/metrics",
  "/routing/test"
]
```

(`jq` sorts keys in `keys`; the document itself preserves route-table order.) The full test run
must end with every case passing, including the three new `OpenAPI` cases. Commit with the
Milestone 2 message.

Milestone 3:

```bash
# write docs/user/queue-inspection.md; edit README.md, mori.dhall, the capability record, IR-3, both changelogs
mori validate
okf log add docs/capabilities --kind Update -m "<handle> extended: pgmq-inspect executable, OpenAPI document, and queue inspection guide (ExecPlan 31)"
okf log add docs/improvement-requests --kind Update -m "IR-3 completed: pgmq-inspect ships the HTTP and WebSocket inspection surface, a standalone executable, an OpenAPI document, and the queue inspection guide (ExecPlan 31)"
just docs-check
nix fmt
git add -A && git commit    # Milestone 3 message
```

`just docs-check` must print no errors for `mori validate`, the capabilities bundle, the
reviews bundle, the improvement-requests bundle, and (if plan 26 has landed) the bug-reports
bundle. Finally, run the whole family once:

```bash
cabal build all
cabal test all
```

and record the outcome in Progress and Outcomes & Retrospective.


## Validation and Acceptance

**Milestone 1.** With the server started as above against a database that holds at least one
queue (create one through `psql "$PG_CONNECTION_STRING" -c "select pgmq.create('demo')"` and
send a message with `-c "select pgmq.send('demo', '{\"hello\":\"world\"}')"`):

```bash
curl -s http://127.0.0.1:9092/ ; echo
curl -s http://127.0.0.1:9092/queues ; echo
curl -s http://127.0.0.1:9092/health/ready ; echo
curl -s -i http://127.0.0.1:9092/queues -H 'Origin: http://localhost:5173' | grep -i access-control
```

Expected, in order: the service descriptor with `"service":"pgmq-inspect"`; a JSON array
containing an object whose `queue_name` is `demo`; `{"ready":true,…}` with HTTP 200; and no
output from the last command, because CORS is disabled by default. Restart with
`--cors-origin http://localhost:5173` and repeat the last command: it prints
`access-control-allow-origin: http://localhost:5173`. Repeat with
`-H 'Origin: http://evil.example'`: no header. Start with `--no-listener` and connect a
WebSocket client (for example `websocat ws://127.0.0.1:9092/ws` if installed, or plan 30's test
client), send `{"type":"subscribe","queue":"demo"}`, and observe a `snapshot` frame with
`"push":false`. Stop the database (`pg_ctl stop -D "$PGDATA"` or `just process-down`) and
request `/health/ready`: HTTP 503 with `"ready":false`; `/queues`: HTTP 503 with code
`database_unavailable`. Restart the database before continuing.

Exit codes: `cabal run pgmq-inspect` with no URL and no environment variable exits `2` and
prints one line to stderr beginning `pgmq-inspect:`; so does an empty or unparsable URL.

**Milestone 2.** `cabal test pgmq-inspect:pgmq-inspect-test` passes; the `OpenAPI` group has
three cases: the golden, the coverage, and the served-document round trip. Against the running
server:

```bash
curl -s http://127.0.0.1:9092/openapi.json | jq '.info.title, (.paths | length)'
curl -s http://127.0.0.1:9092/ | jq .openapi_path
```

prints `"pgmq-inspect"`, `16`, and `"openapi.json"`. Negative check of the coverage test:
temporarily add a route `(methodGet, [Literal "nope"])` to `routeTable`, run the `OpenAPI`
group, and observe the coverage case fail with a diff that names `/nope`; revert. Negative
check of the golden: change any schema description, run the group without `--accept`, and
observe the golden case fail; revert.

**Milestone 3.** `just docs-check` and `mori validate` pass. `grep -n "status: completed" docs/improvement-requests/add-a-pgmq-metrics-sister-package-with-http-and-websocket-inspection-endpoints.md`
prints the line, and `okf show docs/improvement-requests IR-3` (if the installed `okf` has
`show`; otherwise open the file) shows `completedAt` and `resolution`. The README renders a
`pgmq-inspect` section with a working relative link to `docs/user/queue-inspection.md`. A
reader who follows the guide's "Running the standalone server" section on a fresh checkout,
with only the guide and the repository, reaches the `{"ready":true,…}` probe; a reader who
follows "Embedding in your own server" compiles the `mountUnder` example (paste it into a
scratch module under `pgmq-inspect/test/` and `cabal build pgmq-inspect:tests`, then delete
it). Every transcript in the guide was produced against the running server, not typed from
memory: re-run each `curl` once and compare.

The parent MasterPlan's acceptance for this plan is that a pgmq-hs user with no keiro process
can run, call, subscribe to, generate a client for, and read the contract of the surface from
this repository alone. The three milestones above are that acceptance, item by item.


## Idempotence and Recovery

Every step is additive and repeatable. Re-running `cabal build`, `cabal test`, `nix fmt`,
`mori validate`, and `just docs-check` is safe. The schema-install script reports
`AlreadyApplied` on a second run and changes nothing. The golden files are regenerated by the
same `--accept` command that created them; a wrong golden is fixed by correcting the Haskell
value and re-accepting, never by hand-editing the JSON. `okf log add` appends; if a log entry
was added with a wrong message, append a correcting entry rather than editing the log.

If Milestone 1's server starts but `GET /queues` returns 503, the pool cannot reach the
database or the schema is missing: run the readiness probe, check `$PG_CONNECTION_STRING`, and
re-run the schema script. If `nix build .#pgmq-inspect` fails resolving `optparse-applicative`,
the executable's bound excludes the overlay's `0.19.0.0`; fix the bound, not the overlay. If
`cabal exec -- runghc` cannot find `pgmq-migration`, run `cabal build pgmq-migration` first;
as a last resort for a throwaway database only, apply the files in `pgmq-migration/migrations/`
in manifest order with `psql -f`, which bypasses the ledger.

If Milestone 2's coverage test fails on a route plan 29 serves that this plan's `paths` list
omits, add the entry; if it fails on a route this plan documents that the router does not
serve, the router is right and the entry is removed. The `/ws` route is the only intentional
exclusion. If the served-document test fails to decode, the document is outside OpenAPI 3.0.0
to 3.0.3 (the library's accepted range); the usual cause is a `Referenced` pointing at a schema
name that is not in `_componentsSchemas`.

Frontmatter edits to `IR-3` and the capability record are plain text and can be corrected and
re-validated freely with `just docs-check`. A commit made without the three trailers is fixed
with `git commit --amend` before the next commit, never by rewriting history further back.
Nothing in this plan touches a database schema, a migration, or a version number, so there is
no rollback path to describe beyond `git revert`.


## Interfaces and Dependencies

Library dependencies added to `pgmq-inspect`'s `library` stanza: `openapi3 ^>=3.2` (the Nix
set has `3.2.4`; it transitively brings `insert-ordered-containers`, `http-media`, `lens`,
`scientific`, `aeson-pretty`, and `QuickCheck`, all present in the pinned `ghc9124` set) and
`insert-ordered-containers ^>=0.2` (imported directly for `Data.HashMap.Strict.InsOrd`).
Executable dependencies: `async ^>=2.2`, `base`, `bytestring`, `generic-lens`, `hasql ^>=1.10`,
`hasql-pool ^>=1.4`, `lens ^>=5.3`, `optparse-applicative >=0.18 && <0.20`, `pgmq-inspect`,
`text`. Test dependency added if absent: `tasty-golden ^>=2.3`; the test suite already depends
on `http-client`, `http-types`, `aeson`, `tasty`, and `tasty-hunit` from plan 29.

At the end of Milestone 1, `pgmq-inspect/app/Main.hs` defines `main :: IO ()` and the private
`Options`, `optionsParser :: Parser Options`, `parserInfo :: ParserInfo Options`,
`resolveDatabaseUrl :: Maybe String -> IO String`, and `failWith :: String -> IO a`. It uses
`Hasql.Connection.Settings.connectionString :: Text -> Settings`,
`Hasql.Pool.Config.settings :: [Setting] -> Config`, `Hasql.Pool.Config.size :: Int -> Setting`,
`Hasql.Pool.Config.staticConnectionSettings :: Settings -> Setting`,
`Hasql.Pool.acquire :: Config -> IO Pool`, `Hasql.Pool.release :: Pool -> IO ()`, and from
`Pgmq.Inspect`: `poolEnv :: Pool -> Maybe Settings -> InspectEnv`,
`withInspectServer :: InspectServerConfig -> InspectEnv -> (RunningInspectServer -> IO a) -> IO a`,
`defaultInspectServerConfig :: InspectServerConfig`, `CorsPolicy (..)`, and
`RunningInspectServer (..)` with `serverPort :: Int` and `serverThread :: Async ()`. If plan 29
exported fewer of these from the umbrella `Pgmq.Inspect`, import them from
`Pgmq.Inspect.Config`, `Pgmq.Inspect.Env`, and `Pgmq.Inspect.Server` directly and add the
re-exports to the umbrella in the same milestone.

At the end of Milestone 2, `Pgmq.Inspect.OpenApi` exports `openApiDocument :: Data.OpenApi.OpenApi`,
`openApiJson :: Data.ByteString.Lazy.ByteString`, and `documentedPaths :: [FilePath]`;
`Pgmq.Inspect.Http.routeTable` contains `(methodGet, [Literal "openapi.json"])`; the
service-descriptor record in `Pgmq.Inspect.Wire` has the field `openapiPath :: Text` encoded as
`openapi_path`; `Pgmq.Inspect` re-exports `openApiDocument`; and `pgmq-inspect/test/OpenApiSpec.hs`
exports `tests :: TestTree`. The document's `openapi` version is `3.0.0` (the library's
`Monoid` default), its `servers` list is empty, and its `paths` keys are exactly the sixteen
listed in Concrete Steps.

At the end of Milestone 3, `docs/user/queue-inspection.md` exists and is registered in
`mori.dhall` under the key `queue-inspection`; `README.md` has a `## pgmq-inspect` section;
the capability record for the surface carries the two new evidence entries; `IR-3` carries
`status: completed`, `completedAt`, and `resolution`; both bundle logs have a new entry; and
the root and package changelogs carry `Unreleased` entries including the release handoff.
No module in `pgmq-core`, `pgmq-hasql`, `pgmq-effectful`, `pgmq-config`, or `pgmq-migration`
changes, and no version number changes anywhere.
