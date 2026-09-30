---
id: 26
slug: classify-postgresql-disconnects-surfaced-as-statement-errors-as-transient
title: "Classify PostgreSQL disconnects surfaced as statement errors as transient"
kind: exec-plan
created_at: 2026-09-30T19:58:53Z
intention: "intention_01m3sybxw2ee0trbarpsszhkkc"
provenance:
  created_by:
    model: "claude-fable-5-1"
    harness: "claude-code"
    at: 2026-09-30T19:58:53Z
  revisions:
    - model: "claude-fable-5-1"
      harness: "claude-code"
      at: 2026-09-30T20:28:53Z
      mode: "update"
      note: "Add the isAmbiguousReply classifier for lost replies to non-idempotent operations"
    - model: "claude-fable-5-1"
      harness: "claude-code"
      at: 2026-09-30T21:21:28Z
      mode: "update"
      note: "Require bounded retry loops and a sticky ambiguous state; add the runtime-patterns follow-up"
---

# Classify PostgreSQL disconnects surfaced as statement errors as transient

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.
If durable project context changes, update or create ADRs in docs/adr/ in the same change.


## Purpose / Big Picture

`pgmq-effectful` exports one retry classifier, `isTransient :: PgmqRuntimeError -> Bool`.
Consumers gate their retry loops on it: the README's own example reads
`Left (_cs, err) | isTransient err -> retry`, and `shibuya-pgmq-adapter` gates every
acknowledge and poll retry on it. Bug report BUG-1
(`docs/bug-reports/1-disconnect-errors-are-classified-as-permanent.md`) says the classifier
answers `False` for three failures that are really a lost connection: an immediate PostgreSQL
shutdown and a TCP reset both surface as a statement error whose SQLSTATE is the empty
string, and a terminated backend surfaces as `UnexpectedRowCountStatementError 1 1 1`. In all
three cases the same connection pool completed a later operation, so a retry would have
succeeded, yet the classifier told the caller to give up.

The investigation recorded in this plan confirms the report and explains it. The hasql driver
only turns a failure into its `ConnectionSessionError` constructor when *sending* a query
fails. When the connection dies while the driver is *waiting for the reply*, libpq hands hasql
a locally manufactured error result that carries no SQLSTATE and no message, and hasql wraps it
in the ordinary server-error constructor. When the server manages to send its own fatal error
before the socket closes, libpq produces two results for one query, and hasql reports that as
`UnexpectedRowCountStatementError 1 1 1`, a value that can never be a genuine row-count
mismatch because the actual count sits inside its own bounds. Neither shape is in the
classifier's whitelist, so both are permanent today.

After this plan, a retry loop gated on `isTransient` keeps retrying through a server restart, a
backend termination, and a TCP reset, and the library's own test suite proves it: a new
`DisconnectSpec` starts a dedicated PostgreSQL, terminates the pool's backend during an in-flight
`readWithPoll`, kills it with SIGKILL, and restarts the whole server with an immediate shutdown,
asserting each surfaced error is transient and that the same pool completes a later send. The
pure `ClassificationSpec` pins the two new shapes in both directions.

Making those shapes transient has one consequence a caller must be able to see: a retry loop
now re-runs an operation whose reply was lost, and a `sendMessage` that had already committed
would be sent twice. So the plan also exports a second classifier, `isAmbiguousReply`, which
answers `True` exactly when a statement was delivered but no server verdict came back. A caller
gates the retry of non-idempotent operations on it and reconciles (for a send, checks the durable
message keys) before sending again; every ambiguous reply is also transient, so plain retry
loops keep working unchanged. Design note 017, capability CAP-5, the README example, and both
changelogs describe the precise rules, BUG-1 closes as fixed, and improvement request IR-4
closes as completed.


## Progress

- [ ] M1: `pgmq-effectful/test/EphemeralDb.hs` exports `ephemeralConfig` and a top-level `installPgmqNative`
- [ ] M1: `pgmq-effectful/test/DisconnectSpec.hs` written, listed in `pgmq-effectful.cabal` `other-modules`, and registered in `pgmq-effectful/test/Main.hs`
- [ ] M1: the disconnect suite runs against its own cluster; the observed error shape, `isTransient` verdict, and recovery attempt count for each of the three faults are recorded in Surprises & Discoveries (the transient assertions fail at this point, which is the reproduction)
- [ ] M2: `Pgmq.Effectful.Interpreter.isTransient` restructured around `isTransientStatementError` and `isTransientServerError`; empty SQLSTATE and the `1 1 1` shape classify transient; `ScriptSessionError` routed through the SQLSTATE rule
- [ ] M2: `isAmbiguousReply` exported from `Pgmq.Effectful.Interpreter` and re-exported from `Pgmq.Effectful`; `UmbrellaExportsSpec` gains a compile witness for both classifiers
- [ ] M2: `pgmq-effectful/test/ClassificationSpec.hs` pins the new shapes and their permanent neighbours, pins `isAmbiguousReply` in both directions, and checks that every ambiguous reply is transient
- [ ] M2: `DisconnectSpec` gains the ambiguity cases (implication for every fault; the SIGKILL reply is ambiguous)
- [ ] M2: `cabal test pgmq-effectful:test:pgmq-effectful-test` green, disconnect suite included; commit
- [ ] M3: `docs/design/017-transient-error-classification.md` revised with the client-synthesized-error and stray-result rules, the pool caveat, and the residual gap
- [ ] M3: `docs/capabilities/effectful-integration.md` limits and evidence revised; capability log entry added
- [ ] M3: `pgmq-effectful/CHANGELOG.md` and root `CHANGELOG.md` gain an Unreleased entry; the README retry example becomes a bounded loop with the sticky `isAmbiguousReply` branch
- [ ] Follow-up after the next release (other repository): revise `runtime-patterns/messaging/pgmq-connection-faults.md` in `mori://shinzui/keiro-runtime-patterns` to drop its pending note and cite the released version
- [ ] M3: BUG-1 set to `fixed` with `fixedVersion: unreleased` and a `resolution`; IR-4 set to `completed` with `completedAt` and a `resolution`; both bundle logs appended
- [ ] M3: `justfile` `docs-check` validates `docs/bug-reports`; `just docs-check` passes; `okf validate --strict` passes for bug-reports, improvement-requests, and capabilities
- [ ] M3: `docs/adr/transient-classification-is-value-based.md` written; commit
- [ ] Final: `nix fmt` and `git diff --check` clean; Outcomes & Retrospective written


## Surprises & Discoveries

Document unexpected behaviors, bugs, optimizations, or insights discovered during
implementation. Provide concise evidence.

(None yet. M1 must record here, for each of the three faults, the exact `show` of the error
that surfaced, whether `isTransient` returned `True` before the fix, and how many attempts the
same pool needed to complete a send afterwards.)


## Decision Log

- Decision: The report is accurate. Both error shapes are hasql's encoding of a connection
  lost while waiting for a reply, not decode failures, and the classifier treats them as
  permanent.
  Rationale: Traced in hasql 1.10.2.3 (the commit `cabal.project` pins). `Hasql.Comms.Roundtrip.toPipelineIO`
  only produces `ClientError` (which becomes `ConnectionSessionError`) when *sending* fails.
  A drop during receive reaches `Hasql.Comms.ResultDecoder.checkExecStatus`, whose `FatalError`
  branch builds `ServerError` from `PQresultErrorField`, and a libpq-manufactured result has no
  fields, so every field folds to empty. `Hasql.Comms.Recv.singleResult` reports a second result
  as `TooManyResultsError context 1`, which `Hasql.Engine.Errors.fromRecvError` renders as
  `UnexpectedRowCountStatementError 1 1 1`. The kenshou runs cited by BUG-1 show exactly those
  values, and the backend-termination run's `postgres.log` shows the server sent FATAL 57P01
  before closing, which is what makes a second result appear.
  Date: 2026-09-30
- Decision: Fix the classifier as a pure function of the error value. Do not add a
  connection-health probe (checking `PQstatus` after a failed session and rewriting the error
  to `ConnectionSessionError`).
  Rationale: `isTransient` is documented and tested as a pure predicate that consumers call on an
  already-surfaced error; a probe would live in the interpreters, would need a direct
  `postgresql-libpq` dependency, would change the error value consumers see, and would still
  leave `fromUsageError` callers with the unfixed shapes. The probe's one real benefit, evicting
  the dead connection from the pool immediately, is already delivered one call later by
  hasql-pool itself: the next use of a dead connection fails at send with `ConnectionSessionError`,
  which the pool evicts and the classifier already calls transient. That extra transient failure is
  documented rather than engineered away.
  Date: 2026-09-30
- Decision: A `ServerError` whose SQLSTATE is the empty string is transient.
  Rationale: The PostgreSQL wire protocol requires every `ErrorResponse` the server sends to carry
  the SQLSTATE field, so an empty code can only come from an error libpq manufactured on the
  client, and during a roundtrip those are connection loss, lost protocol synchronization, or
  client-side out-of-memory, all of which a fresh connection can outlive. This rule keeps every
  server-reported SQLSTATE policy exactly as it was.
  Date: 2026-09-30
- Decision: `UnexpectedRowCountStatementError 1 1 1`, and only that exact value, is transient.
  Rationale: In hasql 1.10 every construction of this constructor uses bounds `1 1`;
  `fromRecvError` maps "no results" to actual `0` and "too many results" to actual `1`, and the
  decoders' own row-count branches can only produce an actual count outside the bounds. So an
  actual count inside its own bounds is provably not a row-count mismatch: it is the driver
  saying a second result arrived for a single-command roundtrip, which for this library's
  extended-protocol statements only happens when libpq appends its connection-loss result after
  a server error. Every other actual count stays permanent, which is what IR-4 asked for.
  Date: 2026-09-30
- Decision: Apply the same SQLSTATE rule to `ScriptSessionError`.
  Rationale: Today every script error is permanent, including 57P01 and the empty-SQLSTATE
  disconnect shape, because the branch never looked at the code. `isTransient` is public and
  takes any `PgmqRuntimeError`, so leaving one server-error constructor outside the policy is an
  inconsistency rather than a choice. The change is one helper shared by both branches.
  Date: 2026-09-30
- Decision: `DriverSessionError` stays permanent; the one disconnect window that lands there is
  documented, not fixed.
  Rationale: If the connection dies after a statement's reply but before the pipeline sync
  reply, hasql reports it as `DriverSessionError` with the details only inside its message text.
  Matching message text would be fragile, the window is a few bytes of the same server write
  in practice, and no run has observed it. The design note names it as a residual gap that only
  an upstream hasql change (mapping receive-side connection loss to `ConnectionSessionError`)
  can close.
  Date: 2026-09-30
- Decision: Reproduce in this repository with three faults on a dedicated cluster (SIGKILL of
  the pool's backend, `pg_terminate_backend`, and an immediate-shutdown restart), not with a
  TCP proxy port of kenshou's network-partition scenario.
  Rationale: SIGKILL closes the socket with no server message, which deterministically yields the
  empty-SQLSTATE shape the report attributes to TCP reset and immediate shutdown;
  `pg_terminate_backend` makes the server send FATAL 57P01 and then close, which is the
  two-result path behind `1 1 1`; and the immediate restart mirrors the kenshou scenario that
  first reported the bug. All three need only `unix`, `ephemeral-pg`, and `hasql`, which the
  test suite already depends on, and the precedent for a dedicated crash cluster already exists
  in `pgmq-config/test/NotifyCrashSpec.hs`.
  Date: 2026-09-30
- Decision: Do not upgrade hasql as part of this plan.
  Rationale: The local hasql corpus head is 1.10.3.5 and its `serverError`, `singleResult`, and
  `fromRecvError` are unchanged from the pinned 1.10.2.3 (verified 2026-09-30), so an upgrade
  would not change either shape. Filing an upstream issue is a follow-up noted in Outcomes, not a
  dependency of the fix.
  Date: 2026-09-30
- Decision: Versions stay at 0.6.1.1; changelogs gain `Unreleased` sections; BUG-1 records
  `fixedVersion: unreleased`.
  Rationale: 0.6.1.1 shipped on 2026-09-25 and release bumps are coordinated across the family
  by their own plans. The bug-report profile defines `unreleased` precisely for a fix that is on
  the default branch only.
  Date: 2026-09-30
- Decision: Design note 017 stays the home of the classification table; a short ADR records the
  value-based classification decision, the rejected probe, and the pool caveat.
  Rationale: This repository keeps per-topic policy in `docs/design/` notes and cross-cutting
  decisions in plain-Markdown ADRs under `docs/adr/`. The rule table belongs with the existing
  note consumers already cite; the decision not to probe the connection and to accept one extra
  transient failure per dead pooled connection is durable project judgment that a future
  contributor could otherwise reverse without noticing.
  Date: 2026-09-30
- Decision: Export a second classifier, `isAmbiguousReply :: PgmqRuntimeError -> Bool`, that
  answers `True` exactly when the driver received no server verdict for a delivered statement:
  the empty-SQLSTATE server error (in a statement or a script) and the stray-result shape
  `UnexpectedRowCountStatementError 1 1 1`. Every ambiguous reply is also transient.
  Rationale: Making the two disconnect shapes transient means a naive retry loop now re-runs an
  operation whose reply was lost, and a `sendMessage` that had already committed is then sent
  twice. `isTransient` cannot express "safe to retry blindly" versus "retry only after
  checking", and callers should not have to pattern-match hasql constructors to tell the two
  apart. The helper is conservative on purpose: it reports the stray-result shape as ambiguous
  even though its usual producer is a server FATAL that aborted the transaction, because hasql
  discards that first result and the value alone cannot prove which result it was. A send-side
  failure (`ConnectionSessionError`) is not ambiguous, because the statement never reached the
  server; a real server error or a decode failure is not ambiguous, because the server's verdict
  arrived. Reconciling unnecessarily costs one read; duplicating a send costs correctness. Added
  at the user's request after the regression-risk review of this plan.
  Date: 2026-09-30
- Decision: Every retry loop the documentation shows is bounded, and an ambiguous reply is
  sticky: once `isAmbiguousReply` fires for a non-idempotent operation, the loop retries the
  reconciliation, never the operation, until reconciliation succeeds or the bound is hit.
  Rationale: `isTransient` is a verdict on one error, not an oracle for whether an outage will
  heal; during a permanent outage every attempt is individually transient, so an unbounded loop
  spins forever. That was already true for "connection refused" before this plan, but the README
  example showed no bound, and the classifier's new reach makes the omission more visible. The
  sticky rule closes a hole the helper alone leaves open: a lost send reply followed by a
  reconciliation read that fails transiently (the server is still down) would otherwise be
  handled as a plain transient error and re-send blindly, producing exactly the duplicate the
  helper exists to prevent. Both rules are stated in the design note and shown in the README
  loop; neither is enforced by the library, which still retries nothing itself.
  Date: 2026-09-30


## Outcomes & Retrospective

Summarize outcomes, gaps, and lessons learned at major milestones or at completion.
Compare the result against the original purpose. Before marking the plan complete,
distill durable project context from the Decision Log, Surprises & Discoveries, and
this section into docs/adr/. Keep task-local execution details here.

(To be filled during and after implementation.)


## Context and Orientation

### The repository and where the classifier lives

`pgmq-hs` is a multi-package Cabal project. The packages that matter here are `pgmq-core` (plain
types such as `QueueName`), `pgmq-hasql` (SQL statements and hasql sessions for every pgmq
function), and `pgmq-effectful` (an `effectful` effect over those sessions, two interpreters, and
the error model). The toolchain comes from Nix: run every build and test command inside
`nix develop`, which provides GHC (`cabal.project` says `with-compiler: ghc-9.12.4`), cabal, and
a PostgreSQL server binary that the test suites start themselves. Nothing needs an external
database.

The classifier is `isTransient` in `pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs`,
re-exported from `pgmq-effectful/src/Pgmq/Effectful.hs`. It pattern-matches on
`PgmqRuntimeError`, whose three constructors mirror hasql-pool's `UsageError` one to one
(`PgmqAcquisitionTimeout`, `PgmqConnectionError`, `PgmqSessionError`). The session constructor
wraps hasql's own `SessionError`, so the classifier's cases are hasql's cases. Today the
`StatementSessionError` branch delegates to `isTransientSqlState` for `ServerStatementError`
and answers `False` for every other statement error; the `ScriptSessionError`,
`MissingTypesSessionError`, and `DriverSessionError` branches answer `False` unconditionally.
Both interpreters, `runPgmq` in the same file and `runPgmqTraced` in
`pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs`, produce the error through the one
shared `fromUsageError`, so the classifier's behaviour is identical for both by construction.

The tests live in `pgmq-effectful/test/`. `Main.hs` starts one shared PostgreSQL through
`EphemeralDb.withPgmqPool` and hands its pool to `PlainInterpreterSpec` and
`TracedInterpreterSpec`; `ClassificationSpec` is pure and needs no database. The suite is
registered in `pgmq-effectful/pgmq-effectful.cabal` under `test-suite pgmq-effectful-test`, whose
`other-modules` list must name every test module and whose `build-depends` already include
`ephemeral-pg`, `pg-migrate`, `hasql`, `hasql-pool`, `unix`, `random`, `vector`, and `text`.

### Terms used in this plan

A *SQLSTATE* is the five-character error code PostgreSQL attaches to every error it reports,
such as `57P01` for "admin shutdown" or `23505` for a unique violation. *libpq* is
PostgreSQL's C client library; *hasql* is the Haskell driver built on it; *hasql-pool* is the
connection pool both interpreters run sessions through. A *backend* is the server process that
serves one client connection; `pg_terminate_backend(pid)` asks the server to end one, and
SIGKILL ends one without giving it a chance to say anything. An *immediate shutdown* stops the
whole server without a clean checkpoint, so the next start runs crash recovery. hasql executes
every statement in libpq's *pipeline mode*: it sends the query and a sync marker, then reads
results back one at a time. *Transient* means "worth retrying on a fresh connection";
*permanent* means retrying cannot help. *ephemeral-pg* is the library the test suites use to
start a throwaway PostgreSQL. An *OKF bundle* is a directory of Markdown records with YAML
frontmatter validated against a pinned profile; `docs/bug-reports`, `docs/improvement-requests`,
and `docs/capabilities` are three such bundles, and the `okf` command validates them.

### How hasql reports a lost connection (the mechanism)

The sources below are hasql 1.10.2.3, the commit `cabal.project` pins
(`aa3d6ae499e187c291422443f221f9f486c43a9e`). Two checkouts exist locally: cabal unpacks the
exact pin under `dist-newstyle/src/hasql-*/`, and the mori corpus for `mori://hasql/hasql`
(`mori registry show hasql/hasql --full` prints its path) is at 1.10.3.5, whose code at every
site named here is identical (verified 2026-09-30). Cite the project as
`mori://hasql/hasql/packages/hasql` and `mori://hasql/hasql/packages/hasql-pool` with the
project-relative paths below; artifact-level URIs are pending.

A statement runs through `hasql/src/library/Hasql/Engine/Contexts/Pipeline.hs`, which calls
`Hasql.Comms.Roundtrip.toPipelineIO` (`hasql/src/library/Hasql/Comms/Roundtrip.hs`). That
function has exactly two failure paths. If any *send* step fails (`enterPipelineMode`, the query
itself, `exitPipelineMode`), it returns `ClientError`, and `Pipeline.run` turns that into
`ConnectionSessionError`, which the classifier already calls transient and which hasql-pool
evicts. Everything that goes wrong while *receiving* becomes `ServerError recvError` and flows
through `Hasql.Engine.Errors.fromRecvError` (`hasql/src/library/Hasql/Engine/Errors.hs`).

Receiving one statement's reply is `singleResult` in `hasql/src/library/Hasql/Comms/Recv.hs`.
It calls `PQgetResult` once and expects a result, calls it again and expects `Nothing`, and only
then runs the decoder on the first result. When the second call returns a result instead, it
fails with `TooManyResultsError context 1` without ever looking at the first result; `fromRecvError`
renders that as `StatementSessionError ... (UnexpectedRowCountStatementError 1 1 1)`. The same
function renders "no result at all" as `1 1 0`, and the decoders' own row-count failures in
`hasql/src/library/Hasql/Comms/ResultDecoder.hs` (`single` and `maybe`) become `1 1 actual`
with an actual count that is never `1`. So `1 1 1` is reserved, by construction, for "a stray
second result".

Decoding a result begins with `checkExecStatus` in the same `ResultDecoder.hs`. When libpq's
result status is `FatalError`, it calls `serverError`, which reads the SQLSTATE, message,
detail, hint, and position with `PQresultErrorField` and folds each missing field to empty.
A result the *server* produced always has those fields. A result *libpq* produced on its own,
which is what `PQgetResult` returns when the socket closes while it is waiting ("server closed
the connection unexpectedly"), has none of them, because libpq keeps that text in the
connection's error buffer rather than in result fields. hasql therefore surfaces the disconnect as
`StatementSessionError ... (ServerStatementError (ServerError "" "" Nothing Nothing Nothing))`,
the exact value BUG-1 reports for immediate shutdown and TCP reset.

The two shapes differ by whether the server got a word in first. When a backend is terminated
with `pg_terminate_backend`, the server sends a FATAL `ErrorResponse` with SQLSTATE `57P01`
("terminating connection due to administrator command") and then closes. libpq returns that as
the first result; hasql's second `PQgetResult` then hits the closed socket and libpq returns its
own connection-loss result rather than `Nothing`, so `singleResult` reports a stray result and
the 57P01 is discarded. The kenshou backend-termination run's `postgres.log` shows precisely
that server message during `pgmq.read_with_poll`, and its verdict shows `1 1 1` on the client.
When the socket closes with no server message (SIGKILL, a TCP reset, or an immediate shutdown
whose farewell never arrives), the first `PQgetResult` already returns libpq's own result, the
second returns `Nothing`, and the decoder produces the empty-SQLSTATE `ServerError`.

### What the same pool does afterwards

`use` in hasql-pool 1.4.1 (`hasql-pool/src/library/exposed/Hasql/Pool.hs`, with
`Hasql/Pool/SessionErrorDestructors.hs`) evicts a connection only when the session failed with
`ConnectionSessionError`; on any other session error it returns the connection to the queue. After
either shape above, the dead connection therefore goes back into the pool. The next `use` that
draws it fails at the send step (libpq refuses to send on a bad connection), which *is* the
`ClientError` path, so that call surfaces `ConnectionSessionError`, the pool evicts the
connection, and the call after that gets a fresh one. A retry loop gated on `isTransient` thus
converges in at most one extra attempt per dead pooled connection, plus however long the server
takes to accept connections again. This is why the kenshou runs saw the same pool recover, and
it is the caveat the design note must state.

### What kenshou observed

The report's evidence lives in `mori://shinzui/keiro-runtime-kenshou` (local path from
`mori registry show shinzui/keiro-runtime-kenshou --full`). The three scenario runs are
`runs/01a0d6d6-cb25-7609-b1d1-0eeebbba50ec` (immediate restart, PostgreSQL 18), `runs/01a0d6d7-028e-73ac-b7a5-e582b4f85c36`
(backend termination), and `runs/01a0d6d7-1f5e-768c-9b95-822031453720` (TCP reset through a
proxy); each `verdicts/*.json` shows every check passing except the one transience check, and
each `logs/postgres.log` shows the server side. The scenarios are implemented in
`kenshou-pgmq/src/Kenshou/Suite/Pgmq/Concurrency/Runner.hs` (functions around the
`backend-termination`, `postgres-restart`, and `network-partition` verdicts) and are currently
declared as known defects pointing at `mori://shinzui/pgmq-hs/okf/bug-reports/concepts/BUG-1` in
`kenshou-pgmq/src/Kenshou/Suite/Pgmq/Concurrency/Outage.hs`. Flipping those declarations back
after a release is kenshou's follow-up, not this plan's. Artifact-level run URIs are pending.

### Test infrastructure this plan builds on

`pgmq-effectful/test/EphemeralDb.hs` pins every throwaway cluster to
`/tmp/ephpg-pgmq-hs-<uid>` through a private `ephemeralConfig` and installs the pgmq schema
with the native migrations in a private `installNative`. A dedicated-cluster fault test already
exists in `pgmq-config/test/NotifyCrashSpec.hs`: it exports `ephemeralConfig` from that
package's `EphemeralDb`, starts its own server with `Pg.startCached`, installs the schema,
runs `CHECKPOINT` (ephemeral-pg runs PostgreSQL with `fsync` and `synchronous_commit` off, so an
immediate shutdown would otherwise lose the schema itself), crashes and restarts it with
`Pg.restart db {shutdownMode = Pg.ShutdownImmediate}` (which keeps the same port and data
directory and returns a new handle), and does all of its work inside `withResource` so the tasty
test cases are pure assertions over recorded observations. That last point matters because the
suite runs with `-N` and tasty runs cases concurrently by default; a sequence of faults must run
inside one resource acquisition, not across several test cases.

The pgmq operation to keep in flight is `readWithPoll`. Its SQL function loops inside
PL/pgSQL calling `pg_sleep` until a message arrives or `maxPollSeconds` elapses, so on an empty
queue it holds the backend busy for as long as the test wants. The effect operation is
`Pgmq.Effectful.readWithPoll :: ReadWithPollMessage -> Eff es (Vector Message)`, and the record
(`ReadWithPollMessage`, re-exported by `Pgmq.Effectful`) has fields `queueName`, `delay`
(`Int32`, seconds), `batchSize :: Maybe Int32`, `maxPollSeconds :: Int32`,
`pollIntervalMs :: Int32`, and `conditional :: Maybe Value`.

### Documents that must change

`docs/design/017-transient-error-classification.md` is the adopted policy note for the
classifier and carries the SQLSTATE table. `docs/capabilities/effectful-integration.md` is
capability CAP-5 and its Limits section currently says "every other statement error, including
decode and row-count mismatches, is permanent". `pgmq-effectful/CHANGELOG.md` and the root
`CHANGELOG.md` have no `Unreleased` section yet (the top entry is 0.6.1.1). The bug report's
profile (`docs/bug-reports/profile.dhall`, okf-profiles v0.18.0 `coordination.bugReports`)
allows `status` values `reported`, `confirmed`, `in-progress`, `fixed`, `wont-fix`,
`duplicate`, `not-a-bug`, `cannot-reproduce`; `fixed` requires `fixedVersion` (a released
version, or `unreleased`) and recommends `resolution`. The improvement-request profile allows
`proposed`, `accepted`, `in-progress`, `completed`, `rejected`, `withdrawn`, `superseded`;
`completed` requires `completedAt` (RFC 3339 UTC) and recommends `resolution`. Both bundles keep
an append-only `log.md` that `okf log add <bundle> --kind <Kind> -m "<message>"` extends. The
`justfile` recipe `docs-check` validates capabilities, reviews, and improvement-requests but not
bug-reports; all three validate cleanly today.

### ADRs consulted

`docs/adr/` holds three plain-Markdown records: `fifo-native-overrides-and-index-upgrade-boundary.md`,
`haskell-dependency-bounds-and-nix-pin-policy.md`, and `pgmq-1.12-1.13-compatibility.md`. None
concerns error classification, so no existing ADR constrains this plan. The repository has no
profiled ADR bundle; new records follow the plain-Markdown convention with Status, Context,
Decision, and Consequences sections and no OKF frontmatter. The relevant decision records are
the design notes `docs/design/013-pgmq-effectful-error-model.md` (why the error type mirrors
hasql's and why exactly one classifier is exported) and `docs/design/017-transient-error-classification.md`
(the current whitelist). No cross-repository ADR applies.


## Plan of Work

### Milestone 1: reproduce the three faults inside this repository

The goal is a test that fails today for the reason BUG-1 describes and that will pass once the
classifier is fixed, so the fix is proven against real faults rather than hand-built values.
At the end of this milestone `pgmq-effectful/test/DisconnectSpec.hs` exists, runs against a
dedicated PostgreSQL, and its transience assertions fail with the observed error shapes in their
messages. Those messages, one per fault, are copied into Surprises & Discoveries. Nothing is
committed at the end of M1, because a red suite is not a working state; the test lands with the
fix in M2.

First make two private pieces of `pgmq-effectful/test/EphemeralDb.hs` reusable: export
`ephemeralConfig`, and lift the `installNative` local of `withPgmqDb` to a top-level exported
`installPgmqNative` (keep the type GHC already infers for the local, which takes the same
connection settings value that `withPgmqDb` passes to `runMigrationPlan`), then call it from
`withPgmqDb` so behaviour is unchanged.

Then write `DisconnectSpec` following the shape of `pgmq-config/test/NotifyCrashSpec.hs`. All
work happens in one `runDisconnectCycle :: IO DisconnectObservations` inside `withResource`;
the test cases only inspect the record. The cycle starts a server with `Pg.startCached`, installs
the schema, opens a size-1 pool (so every operation uses the one connection whose backend the
test will attack) and a second size-1 *control* pool for observing and terminating, creates a
uniquely named queue, and runs `CHECKPOINT`. It then performs three faults in order, each with
the same pattern: read the pool connection's backend pid with `select pg_backend_pid()`; fork a
thread that runs `readWithPoll` on the empty queue through `runPgmq` with `maxPollSeconds = 15`
and `pollIntervalMs = 100` and puts the outcome into an `MVar`; wait, through the control pool,
until `pg_stat_activity` shows that pid `active` with a query containing `read_with_poll`; inject
the fault; take the outcome; then drive a recovery loop that sends one message through the *same*
pool, retrying while the error is transient with a 100 ms pause, up to 100 attempts, recording
every intermediate error and the attempt count. The three faults are, in order:
`select pg_terminate_backend(pid)` through the control pool; `signalProcess sigKILL` on the pid
(the server then restarts every backend, so the control pool must be recreated afterwards, and
the recovery loop must tolerate connection errors while the server is in crash recovery); and
`Pg.restart` on a handle whose `shutdownMode` is `ShutdownImmediate`, storing the returned handle
in an `IORef` so teardown stops the right process. Every fault must produce a `Left`; a `Right`
means the poll finished before the fault landed and is a test failure.

For each fault the test cases assert four things: an error surfaced; the error is one of the
disconnect shapes (a `ServerStatementError` whose SQLSTATE is empty or is `57P01`/`57P02`, an
`UnexpectedRowCountStatementError 1 1 1`, or a `ConnectionSessionError`); the error is transient;
and the same pool completed a send within the deadline. The third assertion is the reproduction
and fails before M2. Its failure message must include `show err`, so the run itself documents
the shape. Copy those messages into Surprises & Discoveries together with the recovery attempt
counts.

Acceptance for M1: `cabal test pgmq-effectful:test:pgmq-effectful-test --test-options='-p /Disconnect/'`
shows every "surfaces an error", "is a disconnect shape", and "same pool recovers" case passing,
and the "is transient" cases failing for the backend-termination and SIGKILL faults (the
immediate-restart fault may pass or fail depending on whether the server's farewell arrived;
record which).

### Milestone 2: fix the classifier and pin the shapes

Rewrite the `PgmqSessionError` branch of `isTransient` in
`pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs` around two private "lost reply" predicates
that both public classifiers share. `isLostReplyServerError :: HasqlErrors.ServerError -> Bool`
answers `True` when the SQLSTATE is empty. `isLostReplyStatementError :: HasqlErrors.StatementError -> Bool`
routes `ServerStatementError` to it and answers `True` for exactly
`UnexpectedRowCountStatementError 1 1 1`. The `StatementSessionError` branch of `isTransient`
answers `True` when the statement error is a lost reply or a `ServerStatementError` whose
SQLSTATE passes the existing `isTransientSqlState`; the `ScriptSessionError` branch does the
same with the server-error predicate; `ConnectionSessionError` stays `True`, and
`MissingTypesSessionError` and `DriverSessionError` stay `False`.

Add the second public classifier, `isAmbiguousReply :: PgmqRuntimeError -> Bool`, next to
`isTransient`. It answers `True` for a `StatementSessionError` whose statement error is a lost
reply and for a `ScriptSessionError` whose server error has no SQLSTATE, and `False` for every
other value, including `ConnectionSessionError` (the send failed, so nothing reached the
server), acquisition timeouts, connection errors, real server errors, and decode failures. Its
haddock states the contract: the statement was delivered but no server verdict came back, so the
caller cannot tell whether PostgreSQL applied it; it implies `isTransient`; gate the retry of
non-idempotent operations such as sends on it and reconcile before re-sending. Export it from
`Pgmq.Effectful.Interpreter` and add it to the `Pgmq.Effectful` export list and import block
beside `isTransient`; add a compile witness for both classifiers to
`pgmq-effectful/test/UmbrellaExportsSpec.hs`. Extend the haddocks on `isTransient` and the
private predicates to say why each rule is safe, in the words of the Decision Log. Do not export
the private predicates.

In `pgmq-effectful/test/ClassificationSpec.hs` add a `scriptError :: Text -> PgmqRuntimeError`
helper mirroring `serverStatementError`, and a `rowCountError :: Int -> Int -> Int -> PgmqRuntimeError`
helper, then add cases: a statement error with SQLSTATE `""` is transient; a script error with
SQLSTATE `""` is transient; a script error `57P01` is transient; a script error `23505` is
permanent; `rowCountError 1 1 1` is transient; `rowCountError 1 1 0` and `rowCountError 1 1 2`
are permanent (keep the existing `1 1 0` case, it is the same fact); the existing
`DriverSessionError` and `MissingTypesSessionError` cases stay permanent unchanged. Then add an
`isAmbiguousReply` group: `serverStatementError ""`, `scriptError ""`, and `rowCountError 1 1 1`
are ambiguous; `serverStatementError "57P01"`, `serverStatementError "23505"`,
`scriptError "57P01"`, `rowCountError 1 1 0`, `rowCountError 1 1 2`,
`PgmqSessionError (ConnectionSessionError "dropped")`, `PgmqAcquisitionTimeout`, every
`PgmqConnectionError` sample, and the `DriverSessionError` and `MissingTypesSessionError`
samples are not. Finally add one case that walks every sample error the module constructs and
asserts `not (isAmbiguousReply e) || isTransient e`, so the implication is pinned rather than
assumed.

Add two cases to `DisconnectSpec` as well, now that the helper exists: inside `faultCases`, an
"ambiguous replies are transient" case asserting the same implication on the surfaced error, and
at the group level a "SIGKILL reply is ambiguous" case asserting `isAmbiguousReply` on the
SIGKILL round's error, since a killed backend can never have sent a verdict.

Acceptance for M2: the whole `pgmq-effectful-test` suite passes, including every
`DisconnectSpec` case. Commit with the message given in Concrete Steps.

### Milestone 3: state the policy truthfully and close the records

Revise `docs/design/017-transient-error-classification.md`. Keep the SQLSTATE table and add,
after it, a section on client-synthesized errors (why an empty SQLSTATE is by construction a
libpq-local failure, the exact hasql value it produces, and which faults produce it), a section
on stray results (the `1 1 1` derivation, why no other actual count can mean this, and the
`pg_terminate_backend` two-result path), a section on what the pool does afterwards (one extra
transient `ConnectionSessionError` per dead pooled connection, then a fresh connection), a
section naming the residual `DriverSessionError` window and the upstream change that would
close it, and an update to "Where this is enforced" naming `DisconnectSpec` and the new
`ClassificationSpec` cases. Add a section on lost replies and idempotence that introduces
`isAmbiguousReply`, states its contract and the values it covers, explains why the stray-result
shape counts as ambiguous even though its usual producer aborted the transaction, and shows the
recommended loop: an ambiguous reply to a non-idempotent operation is reconciled (for a send,
check the durable message keys) before any re-send; any other transient error is retried with
bounded backoff; everything else fails. State the two loop rules explicitly. First, every loop
is bounded by an attempt count or a deadline, because a permanent outage yields errors that
are each transient and the classifier cannot tell an outage that will heal from one that will
not. Second, the ambiguous state is sticky: after `isAmbiguousReply` fires, further transient
failures (typically the reconciliation read failing while the server is still down) retry the
reconciliation, never the original send, until it succeeds or the bound is hit. Show the same
loop the README will carry. Update the Status line to record this plan and the date.
Say explicitly, as IR-4 asks, that the library still does not retry anything itself and that a
lost reply to a non-idempotent send remains the caller's call, now with a helper that tells the
caller when that is the case.

Revise `docs/capabilities/effectful-integration.md`: replace the Limits bullet's last sentence so
it says that statement errors with no SQLSTATE and the driver's stray-result shape are
transient, and that genuine decode and row-count mismatches, unknown types, and driver errors
remain permanent; add `isAmbiguousReply` to the "Typed errors" bullet and to the frontmatter
`description` as the lost-reply classifier; add a Limits bullet saying the library still retries
nothing itself and that an ambiguous reply to a send is the caller's to reconcile; add an
evidence entry of kind `test` for `pgmq-effectful/test/DisconnectSpec.hs`
stating what it proves; set `generated.by` to your model identifier and `generated.at` to the
revision time. Append a capability log entry.

Add an `## Unreleased` section at the top of `pgmq-effectful/CHANGELOG.md` with a Bug Fixes
entry describing the two shapes, why they were permanent, the precise new rules, and the pool
caveat, plus a New Features entry for `isAmbiguousReply` that states its contract, the
implication, and the duplicate-send hazard it exists for; add a corresponding paragraph at the
top of the root `CHANGELOG.md`. Replace the README's retry example (the `run pool` snippet
under the error-handling paragraph) with a loop that is bounded and whose ambiguous state is
sticky. `lookupByKey` stands for whatever reconciliation the application can do, such as
finding the message by a dedupe key it put in the headers; `pause` is the caller's backoff:

```haskell
-- Bounded: stop after maxAttempts. Sticky: once a send's reply is lost, look
-- the message up instead of sending again, and keep looking it up through
-- further transient failures until the lookup succeeds.
sendOnce pool key body = go 1 False
  where
    go attempt reconciling = do
      result <-
        runEff . runError @PgmqRuntimeError . runPgmq pool $
          if reconciling then lookupByKey key else sendMessage (message key body)
      case result of
        Right outcome -> pure (Right outcome)
        Left (_cs, err)
          | attempt >= maxAttempts -> pure (Left err)
          | isAmbiguousReply err -> pause attempt >> go (attempt + 1) True
          | isTransient err -> pause attempt >> go (attempt + 1) reconciling
          | otherwise -> pure (Left err)
```

Follow the snippet with three sentences: an ambiguous reply means the statement was delivered
but its reply was lost, so a send may already have committed; the loop therefore switches to
reconciliation and never switches back; and the bound exists because a server that stays down
produces transient errors indefinitely. Keep the existing sentence about the three
`PgmqRuntimeError` constructors after that.

Close the records. In `docs/bug-reports/1-disconnect-errors-are-classified-as-permanent.md` set
`status: fixed`, add `fixedVersion: unreleased`, add a one-sentence `resolution` naming the
rule and this plan, and append a short "Resolution" paragraph to the body pointing at the
design note and the two test files. In
`docs/improvement-requests/classify-disconnects-during-postgresql-faults-as-transient.md` set
`status: completed`, add `completedAt` as the UTC time the suite went green, and a `resolution`
that walks its three acceptance items (fault tests, retained permanent cases, design note) and
names `isAmbiguousReply` as the answer to its non-goal about replaying ambiguous sends.
Append a log entry to each bundle with `okf log add`. Add
`okf validate docs/bug-reports --profile docs/bug-reports/profile.dhall --profile-enforce --log-enforce`
to the `docs-check` recipe in `justfile`.

One follow-up lives outside this repository and outside this plan's commits. The Keiro runtime
pattern corpus `mori://shinzui/keiro-runtime-patterns` carries a standard at
`runtime-patterns/messaging/pgmq-connection-faults.md` (document-level URI
`mori://shinzui/keiro-runtime-patterns/docs/messaging-pgmq-connection-faults`) that sizes the
`shibuya-pgmq-adapter` retry budgets for connection faults and states, truthfully for the
released 0.6.1.1, that the two disconnect shapes are still permanent. After the release that
carries this fix, that standard must be revised to cite the released version, drop its pending
note, and prescribe `isAmbiguousReply` for application-owned sends. Record that as the
post-release follow-up in Outcomes & Retrospective.

Write `docs/adr/transient-classification-is-value-based.md` in the existing plain-Markdown
style: Status (accepted, date, introduced by this plan), Context (hasql reports receive-side
loss through statement constructors; the pool keeps the dead connection), Decision (classify
by value with the two rules; no connection probe; accept one extra transient failure; script
errors follow the SQLSTATE rule; driver errors stay permanent; `isAmbiguousReply` is the
library's deliberately conservative answer to the lost-reply question), Consequences (what
consumers' retry loops can rely on, that non-idempotent operations must be gated on
`isAmbiguousReply` and reconciled before a re-send, and the upstream follow-up).

Acceptance for M3: `just docs-check` passes with the new bug-reports line; the three strict
validations in Concrete Steps pass; `nix fmt` and `git diff --check` are clean; commit.


## Concrete Steps

All commands run from the repository root
`/Users/shinzui/Keikaku/bokuno/libraries/pgmq-hs-project/pgmq-hs` inside `nix develop`
(either start the shell once or prefix each command with `nix develop --command`).

### M1: test support and the disconnect suite

Edit `pgmq-effectful/test/EphemeralDb.hs`: add `ephemeralConfig` and `installPgmqNative` to the
export list, move the body of the `installNative` local to a top-level `installPgmqNative`, and
make `withPgmqDb` call it. The diff is small:

```diff
 module EphemeralDb
   ( withPgmqDb,
     withPgmqPool,
+    ephemeralConfig,
+    installPgmqNative,
     StartError,
   )
```

Add `DisconnectSpec` to `other-modules` of `test-suite pgmq-effectful-test` in
`pgmq-effectful/pgmq-effectful.cabal` (alphabetically, after `ClassificationSpec`) and register it
in `pgmq-effectful/test/Main.hs` as a fourth entry of the `pgmq-effectful` group, after
`TracedInterpreterSpec.tests pool`; it takes no pool argument because it starts its own server.

Create `pgmq-effectful/test/DisconnectSpec.hs`. The skeleton below is complete enough to
implement from; keep the structure (one cycle, pure assertions) and the helper names.

```haskell
{-# LANGUAGE OverloadedStrings #-}

-- | BUG-1: a connection lost while a statement is in flight must classify as
-- transient, whichever hasql constructor it arrives in.
--
-- Drives three real faults against a dedicated PostgreSQL (never the
-- suite-shared one) while a 'readWithPoll' is blocked server-side, records
-- what surfaced, and asserts that 'isTransient' says retry and that the same
-- pool completes a later send.
module DisconnectSpec (tests) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, try)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32)
import Effectful (runEff)
import Effectful.Error.Static (runError)
import EphemeralDb (ephemeralConfig, installPgmqNative)
import EphemeralPg qualified as Pg
import EphemeralPg.Database qualified as PgDb
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Errors qualified as HasqlErrors
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as PoolConfig
import Hasql.Session qualified as Session
import Hasql.Statement (Statement, preparable)
import Pgmq.Effectful
  ( MessageBody (..),
    PgmqRuntimeError (..),
    QueueName,
    ReadWithPollMessage (..),
    SendMessage (..),
    createQueue,
    isTransient,
    parseQueueName,
    readWithPoll,
    runPgmq,
    sendMessage,
  )
import System.Posix.Signals (sigKILL, signalProcess)
import System.Random (randomRIO)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

-- | What one fault produced.
data FaultObservation = FaultObservation
  { obsLabel :: !String,
    -- | The error the in-flight poll surfaced; Nothing means it completed.
    obsError :: !(Maybe PgmqRuntimeError),
    -- | Attempts the same pool needed to complete a send afterwards.
    obsRecoveryAttempts :: !Int,
    -- | Errors seen on the way to recovery, in order.
    obsRecoveryErrors :: ![PgmqRuntimeError],
    -- | Whether the send eventually succeeded within the deadline.
    obsRecovered :: !Bool
  }

data DisconnectObservations = DisconnectObservations
  { obsTerminate :: !FaultObservation,
    obsKill :: !FaultObservation,
    obsRestart :: !FaultObservation
  }

tests :: TestTree
tests =
  withResource runDisconnectCycle (const (pure ())) $ \getObs ->
    testGroup
      "DisconnectSpec (BUG-1)"
      [ faultCases "backend termination" (obsTerminate <$> getObs),
        faultCases "backend SIGKILL" (obsKill <$> getObs),
        faultCases "immediate restart" (obsRestart <$> getObs)
      ]

faultCases :: String -> IO FaultObservation -> TestTree
faultCases label getObs =
  testGroup
    label
    [ testCase "surfaces an error" $ do
        obs <- getObs
        case obsError obs of
          Nothing -> assertFailure "the in-flight poll completed; the fault did not land"
          Just _ -> pure (),
      testCase "error is a disconnect shape" $ do
        obs <- getObs
        case obsError obs of
          Nothing -> pure ()
          Just err -> assertBool ("unexpected shape: " <> show err) (isDisconnectShape err),
      testCase "error is transient" $ do
        obs <- getObs
        case obsError obs of
          Nothing -> pure ()
          Just err -> assertBool ("expected transient, got " <> show err) (isTransient err),
      testCase "same pool recovers" $ do
        obs <- getObs
        assertBool
          ( "pool did not recover within the deadline; errors seen: "
              <> show (obsRecoveryErrors obs)
          )
          (obsRecovered obs)
    ]

-- | The values a lost connection is known to arrive in (see design note 017).
isDisconnectShape :: PgmqRuntimeError -> Bool
isDisconnectShape = \case
  PgmqSessionError (HasqlErrors.ConnectionSessionError _) -> True
  PgmqSessionError (HasqlErrors.StatementSessionError _ _ _ _ _ statementError) ->
    case statementError of
      HasqlErrors.ServerStatementError (HasqlErrors.ServerError code _ _ _ _) ->
        code `elem` ["", "57P01", "57P02"]
      HasqlErrors.UnexpectedRowCountStatementError 1 1 1 -> True
      _ -> False
  _ -> False
```

The cycle itself starts the server, installs the schema, and runs the three faults in order.
Pool size 1 is essential: it guarantees the backend pid read before the fault belongs to the
connection the poll will use.

```haskell
runDisconnectCycle :: IO DisconnectObservations
runDisconnectCycle = do
  db0 <- startOrFail
  ref <- newIORef db0
  bracket (pure ref) (\r -> readIORef r >>= Pg.stop) $ \dbRef -> do
    installPgmqNative (Pg.connectionSettings db0)
    queue <- genQueueName
    bracket (acquirePool db0) Pool.release $ \pool -> do
      runOrFail pool (createQueue queue)
      session pool (Session.script "checkpoint")

      terminate <- faultRound "backend termination" pool queue $ \pid ->
        bracket (acquirePool db0) Pool.release $ \control ->
          () <$ session control (Session.statement pid terminateBackend)

      kill <- faultRound "backend SIGKILL" pool queue $ \pid ->
        signalProcess sigKILL (fromIntegral pid)

      restart <- faultRound "immediate restart" pool queue $ \_pid -> do
        db <- readIORef dbRef
        db' <- crashAndRecover db
        writeIORef dbRef db'

      pure DisconnectObservations {obsTerminate = terminate, obsKill = kill, obsRestart = restart}
```

`faultRound` is the shared pattern. It reads the backend pid through the pool, forks the poll,
waits until the server shows that backend busy in `read_with_poll`, injects the fault, collects
the outcome, and runs the recovery loop.

```haskell
faultRound :: String -> Pool.Pool -> QueueName -> (Int32 -> IO ()) -> IO FaultObservation
faultRound label pool queue inject = do
  pid <- session pool (Session.statement () backendPid)
  outcomeVar <- newEmptyMVar
  _ <- forkIO $ do
    result <- try @SomeException (runPoll pool queue)
    putMVar outcomeVar (either (Left . Left) (either (Left . Right) Right) result)
  awaitPolling pool pid 100
  inject pid
  outcome <- takeMVar outcomeVar
  err <- case outcome of
    Left (Left exc) -> assertFailure (label <> ": poll thread threw " <> show exc)
    Left (Right runtimeErr) -> pure (Just runtimeErr)
    Right _ -> pure Nothing
  (attempts, errors, recovered) <- recover pool queue 100 []
  pure FaultObservation {obsLabel = label, obsError = err, obsRecoveryAttempts = attempts, obsRecoveryErrors = errors, obsRecovered = recovered}

runPoll :: Pool.Pool -> QueueName -> IO (Either PgmqRuntimeError ())
runPoll pool queue = do
  result <-
    runEff . runError @PgmqRuntimeError . runPgmq pool $
      readWithPoll
        ReadWithPollMessage
          { queueName = queue,
            delay = 30,
            batchSize = Just 1,
            maxPollSeconds = 15,
            pollIntervalMs = 100,
            conditional = Nothing
          }
  pure (either (Left . snd) (const (Right ())) result)
```

`awaitPolling` must not use the attacked pool (its one connection is busy), so it opens a
short-lived control connection through a second size-1 pool built from the same settings. After
the SIGKILL fault the whole server has restarted, so the control pool is created inside the
helper each time rather than shared across rounds. It polls
`select count(*)::int8 from pg_stat_activity where pid = $1 and state = 'active' and query like '%read_with_poll%'`
every 50 ms until it returns 1, failing after the given number of attempts. `recover` sends one
message through the attacked pool and, while the error is transient, sleeps 100 ms and tries
again, up to the attempt budget; a permanent error fails the test immediately with the error
shown. The statements are ordinary hasql values:

```haskell
backendPid :: Statement () Int32
backendPid = preparable "select pg_backend_pid()" E.noParams (D.singleRow (D.column (D.nonNullable D.int4)))

terminateBackend :: Statement Int32 Bool
terminateBackend = preparable "select pg_terminate_backend($1)" (E.param (E.nonNullable E.int4)) (D.singleRow (D.column (D.nonNullable D.bool)))

activePollCount :: Statement Int32 Int64
activePollCount =
  preparable
    "select count(*)::int8 from pg_stat_activity where pid = $1 and state = 'active' and query like '%read_with_poll%'"
    (E.param (E.nonNullable E.int4))
    (D.singleRow (D.column (D.nonNullable D.int8)))
```

`crashAndRecover`, `startOrFail`, `acquirePool` (size 1), `genQueueName`, and `session` (run a
hasql session through a pool, failing the test on `Left`) are the same helpers as in
`pgmq-config/test/NotifyCrashSpec.hs`, with the pool size changed to 1; copy them. Because the
server restarts during the SIGKILL and immediate-restart rounds, `acquirePool` inside
`awaitPolling` and `recover` must expect connection errors while the server is recovering; the
recovery loop already treats them as transient because `isTransient` classes every
`OtherConnectionError` and `NetworkingConnectionError` as retry-worthy.

Build and run only this suite:

```bash
cabal build pgmq-effectful:test:pgmq-effectful-test
cabal test pgmq-effectful:test:pgmq-effectful-test --test-show-details=direct --test-options='-p /Disconnect/'
```

Expected before the fix (the shapes are what the mechanism predicts; record the ones you
actually see):

```text
pgmq-effectful
  DisconnectSpec (BUG-1)
    backend termination
      surfaces an error:            OK
      error is a disconnect shape:  OK
      error is transient:           FAIL
        expected transient, got PgmqSessionError (StatementSessionError 1 0 "select * from pgmq.read_with_poll($1,$2,coalesce($3,1),$4,$5,coalesce($6,'{}'::jsonb))" [...] True (UnexpectedRowCountStatementError 1 1 1))
      same pool recovers:           OK
    backend SIGKILL
      surfaces an error:            OK
      error is a disconnect shape:  OK
      error is transient:           FAIL
        expected transient, got PgmqSessionError (StatementSessionError 1 0 "..." [...] True (ServerStatementError (ServerError "" "" Nothing Nothing Nothing)))
      same pool recovers:           OK
    immediate restart
      ...
```

Copy each `got ...` value and each round's recovery attempt count into Surprises & Discoveries.
If a round's poll completes instead of failing, increase the wait in `awaitPolling` or the
`maxPollSeconds`; if the immediate-restart round yields `ServerError "57P01"` the case passes
already, which is fine and worth recording.

### M2: the classifier

Replace the `PgmqSessionError` branch and add the helpers in
`pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs`:

```haskell
  PgmqSessionError e -> case e of
    HasqlErrors.ConnectionSessionError _ -> True
    HasqlErrors.StatementSessionError _ _ _ _ _ statementError ->
      isLostReplyStatementError statementError || isTransientStatementError statementError
    HasqlErrors.ScriptSessionError _ serverError ->
      isLostReplyServerError serverError || isTransientServerError serverError
    HasqlErrors.MissingTypesSessionError _ -> False
    HasqlErrors.DriverSessionError _ -> False

-- | Was the statement delivered but its reply lost, so that the caller cannot
-- tell whether PostgreSQL applied it?
--
-- 'True' for the two values hasql 1.10 produces when the connection dies while
-- it is waiting for a reply: a server error with no SQLSTATE (libpq's own
-- connection-loss result, which carries no fields) inside a statement or script
-- error, and @UnexpectedRowCountStatementError 1 1 1@ (a stray second result;
-- hasql discards the first, so the value cannot show whether that was a server
-- fatal error or a completed command). 'False' for everything else: a
-- 'HasqlErrors.ConnectionSessionError' means the send failed and nothing reached
-- the server, and every other error carries the server's verdict or the
-- driver's own decode verdict.
--
-- Every ambiguous reply is transient, so @isAmbiguousReply err@ implies
-- @isTransient err@. Gate the retry of non-idempotent operations on it: an
-- ambiguous reply to a send may have committed, so check the durable message
-- keys before sending again.
isAmbiguousReply :: PgmqRuntimeError -> Bool
isAmbiguousReply = \case
  PgmqSessionError (HasqlErrors.StatementSessionError _ _ _ _ _ statementError) ->
    isLostReplyStatementError statementError
  PgmqSessionError (HasqlErrors.ScriptSessionError _ serverError) ->
    isLostReplyServerError serverError
  _ -> False

-- | The statement-level shapes of a lost reply (see 'isAmbiguousReply').
--
-- hasql 1.10 reports a stray second result as @UnexpectedRowCountStatementError 1 1 1@
-- ('Hasql.Engine.Errors.fromRecvError', @TooManyResultsError@ branch). A genuine
-- row-count mismatch can never carry an actual count inside its own bounds, so
-- this exact value is reserved for the driver's stray-result case, which for the
-- single-command roundtrips this library issues only happens when libpq appends
-- its own connection-loss result after a server fatal error (for example
-- @57P01@ from @pg_terminate_backend@). Every other actual count is a real
-- decoder mismatch and stays permanent.
isLostReplyStatementError :: HasqlErrors.StatementError -> Bool
isLostReplyStatementError = \case
  HasqlErrors.ServerStatementError serverError -> isLostReplyServerError serverError
  HasqlErrors.UnexpectedRowCountStatementError 1 1 1 -> True
  _ -> False

-- | Every @ErrorResponse@ PostgreSQL sends carries a SQLSTATE, so an empty code
-- means libpq manufactured the error on the client — "server closed the
-- connection unexpectedly" and its relatives — which hasql 1.10 surfaces with
-- every field empty.
isLostReplyServerError :: HasqlErrors.ServerError -> Bool
isLostReplyServerError (HasqlErrors.ServerError code _ _ _ _) = T.null code

-- | Server-reported statement errors worth retrying: those whose SQLSTATE is in
-- the transient whitelist.
isTransientStatementError :: HasqlErrors.StatementError -> Bool
isTransientStatementError = \case
  HasqlErrors.ServerStatementError serverError -> isTransientServerError serverError
  _ -> False

isTransientServerError :: HasqlErrors.ServerError -> Bool
isTransientServerError (HasqlErrors.ServerError code _ _ _ _) = isTransientSqlState code
```

Update the `isTransient` haddock so its list of transient cases includes "server-reported
statement or script errors with no SQLSTATE (client-side connection faults)" and "the driver's
stray-result shape `UnexpectedRowCountStatementError 1 1 1`", and its permanent list says
"genuine decode and row-count mismatches" rather than "decode/row-count mismatches". Add
`isAmbiguousReply` to the module's export list under "Error Types", to the export list of
`pgmq-effectful/src/Pgmq/Effectful.hs`, and to that file's `import Pgmq.Effectful.Interpreter`
block, in each case directly after `isTransient`. In `pgmq-effectful/test/UmbrellaExportsSpec.hs`
add `classifiers` to the module's export list and this witness:

```haskell
classifiers :: PgmqRuntimeError -> (Bool, Bool)
classifiers err = (isTransient err, isAmbiguousReply err)
```

Extend `pgmq-effectful/test/ClassificationSpec.hs` with the helpers and cases described in the
Plan of Work, and add the two ambiguity cases to `DisconnectSpec`:

```haskell
      -- inside faultCases
      testCase "ambiguous replies are transient" $ do
        obs <- getObs
        case obsError obs of
          Nothing -> pure ()
          Just err ->
            assertBool
              ("ambiguous but not transient: " <> show err)
              (not (isAmbiguousReply err) || isTransient err)

      -- at the group level, after the three faultCases groups
      testCase "SIGKILL reply is ambiguous" $ do
        obs <- obsKill <$> getObs
        case obsError obs of
          Nothing -> pure ()
          Just err -> assertBool ("expected an ambiguous reply, got " <> show err) (isAmbiguousReply err)
```

Then run the whole suite:

```bash
cabal test pgmq-effectful:test:pgmq-effectful-test --test-show-details=direct
```

Every case must pass, including all twelve `DisconnectSpec` cases. Then format and commit:

```bash
nix fmt
git add pgmq-effectful
git commit -F - <<'EOF'
fix(pgmq-effectful): classify receive-side disconnects as transient

isTransient answered False for two shapes hasql uses when the connection is
lost while waiting for a reply: a ServerError with an empty SQLSTATE (libpq's
own connection-loss result, which carries no fields) and
UnexpectedRowCountStatementError 1 1 1 (hasql's encoding of a stray second
result, produced when the server sends FATAL 57P01 and then closes). Both are
now transient; every real SQLSTATE, decode, and row-count policy is unchanged.
Script errors now follow the same SQLSTATE rule as statement errors.

A second classifier, isAmbiguousReply, answers True for exactly those
lost-reply shapes so callers can reconcile a non-idempotent operation
before retrying it instead of re-sending blindly; it implies isTransient.

DisconnectSpec reproduces all three faults from BUG-1 (pg_terminate_backend,
SIGKILL, immediate restart) against a dedicated cluster and proves the same
pool recovers; ClassificationSpec pins the new shapes in both directions.

ExecPlan: docs/plans/26-classify-postgresql-disconnects-surfaced-as-statement-errors-as-transient.md
Intention: intention_01m3sybxw2ee0trbarpsszhkkc
EOF
```

If the pre-commit hook reformats anything, re-stage and re-run the commit.

### M3: documents, records, and the ADR

Edit the design note, the capability, both changelogs, the bug report, the improvement request,
the justfile, and the new ADR as described in the Plan of Work. Then append the bundle log
entries (each command appends one entry under today's date; run each exactly once):

```bash
okf log add docs/bug-reports --kind Update -m "BUG-1 fixed on the default branch: isTransient now classifies the empty-SQLSTATE and stray-result disconnect shapes as transient (ExecPlan 26)"
okf log add docs/improvement-requests --kind Update -m "IR-4 completed: disconnect shapes classify transient, fault tests added, design note 017 revised (ExecPlan 26)"
okf log add docs/capabilities --kind Revision -m "CAP-5: revise the isTransient limits and add DisconnectSpec as evidence (ExecPlan 26)"
```

Validate strictly and run the repository check:

```bash
okf validate docs/bug-reports --strict --profile docs/bug-reports/profile.dhall --profile-enforce --log-enforce
okf validate docs/improvement-requests --strict --profile docs/improvement-requests/profile.dhall --profile-enforce --log-enforce
okf validate docs/capabilities --strict --profile docs/capabilities/profile.dhall --profile-enforce --log-enforce
just docs-check
```

Each `okf validate` prints `OK: <n> concepts (okf_version 0.2)` (3, 6, and 9 concepts
respectively). If `--strict` reports a missing recommended field such as `resolution`, add it
rather than dropping `--strict`. Then:

```bash
nix fmt
git diff --check
git add docs justfile
git commit -F - <<'EOF'
docs(pgmq): record the disconnect classification rules and close BUG-1

Design note 017 now states why an empty SQLSTATE and hasql's stray-result
shape are connection faults, what hasql-pool does with the dead connection,
and the residual DriverSessionError window, and introduces isAmbiguousReply
for lost replies to non-idempotent operations. CAP-5's limits and evidence are
revised, both changelogs gain an Unreleased entry, the README retry example
gains the ambiguous branch, BUG-1 is fixed
(fixedVersion unreleased), IR-4 is completed, docs-check now validates the
bug-report bundle, and a new ADR records the value-based classification
decision.

ExecPlan: docs/plans/26-classify-postgresql-disconnects-surfaced-as-statement-errors-as-transient.md
Intention: intention_01m3sybxw2ee0trbarpsszhkkc
EOF
```

Update this plan's Progress, Surprises & Discoveries, and Outcomes & Retrospective at each
stopping point, and record a provenance revision entry once per session:

```bash
bun .claude/skills/exec-plan/record-provenance.ts revision \
  --plan docs/plans/26-classify-postgresql-disconnects-surfaced-as-statement-errors-as-transient.md \
  --model <your-model-id> --harness <your-harness> --mode implement --note "<one line>"
```


## Validation and Acceptance

The change is proven at three levels. The pure level is `ClassificationSpec`: running
`cabal test pgmq-effectful:test:pgmq-effectful-test --test-options='-p /classification/'` shows
the two new transient cases (`""` SQLSTATE in a statement and in a script; `1 1 1`) and their
permanent neighbours (`23505` script error; `1 1 0` and `1 1 2`; `DriverSessionError`;
`MissingTypesSessionError`) all passing, together with the `isAmbiguousReply` group (the three
lost-reply values ambiguous, every other sample not) and the implication case. The live level is
`DisconnectSpec`: running `--test-options='-p /Disconnect/'` shows, for each of the three faults,
that an error surfaced, that it was one of the documented disconnect shapes, that `isTransient`
returned `True`, that any ambiguous reply was also transient, and that the same pool completed a
send afterwards; the SIGKILL round's reply is additionally shown to be ambiguous. The regression
level is the whole suite:
`cabal test pgmq-effectful:test:pgmq-effectful-test` passes, which shows the interpreters, the
traced spans, and the umbrella exports are untouched.

The behavioural claim a reader can check by hand is the README example: a loop that retries
while `isTransient err` holds now survives `pg_terminate_backend` on its connection, a SIGKILL
of its backend, and an immediate server restart, completing once the server is back, whereas
before this plan it stopped at the first such fault with a "permanent" error. `DisconnectSpec`
is that loop, and its recorded attempt counts in Surprises & Discoveries show how many retries
each fault cost. The second claim is the README's ambiguous branch: a loop that checks
`isAmbiguousReply` first can tell a lost reply (reconcile, then retry) from a server refusal or
a failed send (retry or fail), which is what stops the fix from turning a lost send reply into
a duplicate message. Read the README loop and confirm it has an attempt bound and that the
`reconciling` flag is never reset to `False`.

The documentation claims are checked by `just docs-check` and the three strict `okf validate`
commands passing, and by reading `docs/design/017-transient-error-classification.md` and
confirming it names both shapes, the pool caveat, the residual gap, and `DisconnectSpec`.


## Idempotence and Recovery

Every step can be repeated. `DisconnectSpec` starts its own PostgreSQL under
`/tmp/ephpg-pgmq-hs-<uid>` and stops it on exit; a run killed part-way leaves a postmaster that
the next run's startup sweep reaps, because the root is stable. Queue names carry a random
suffix, and the cluster is discarded, so nothing accumulates. If a fault round's poll completes
before the fault lands (the "surfaces an error" case fails), the test is simply re-run after
raising the wait budget; nothing else is affected. If the SIGKILL round leaves the server in a
state the recovery loop cannot reach within its budget, the observation says so and the
`same pool recovers` case fails with the errors it saw; raise the budget or the pause, do not
weaken the assertion.

The classifier change is a pure edit with no data migration. Reverting M2 is a one-file
`git revert`. The `okf log add` commands append and must each run once per bundle; if one is
run twice, delete the duplicate line from that bundle's `log.md` by hand before validating.
Frontmatter edits to BUG-1 and IR-4 are plain text and can be corrected and re-validated freely;
`okf validate --strict` names the offending field.


## Interfaces and Dependencies

No library dependency changes. `pgmq-effectful`'s `build-depends` stay as they are; the test
suite already depends on `ephemeral-pg >=0.3.1.0` (for `EphemeralPg`, `EphemeralPg.Database`,
`Pg.startCached`, `Pg.restart`, `Pg.stop`, `Pg.ShutdownImmediate`), `unix` (for
`System.Posix.Signals.signalProcess` and `sigKILL`), `hasql` (for `Hasql.Statement`,
`Hasql.Decoders`, `Hasql.Encoders`, `Hasql.Session`, `Hasql.Errors`), `hasql-pool`, `random`,
`vector`, and `text`.

At the end of M1, `pgmq-effectful/test/EphemeralDb.hs` exports `ephemeralConfig :: IO Config`
and `installPgmqNative` (the connection-settings type `withPgmqDb` already passes to
`runMigrationPlan`, returning `IO ()`), and `pgmq-effectful/test/DisconnectSpec.hs` exports
`tests :: TestTree`.

At the end of M2, `Pgmq.Effectful.Interpreter` exports `runPgmq`, `PgmqRuntimeError (..)`,
`fromUsageError`, `isTransient`, the new `isAmbiguousReply :: PgmqRuntimeError -> Bool`, and the
deprecated `PgmqError (..)`; `Pgmq.Effectful` re-exports `isAmbiguousReply` beside `isTransient`.
The predicates `isLostReplyStatementError :: HasqlErrors.StatementError -> Bool`,
`isLostReplyServerError :: HasqlErrors.ServerError -> Bool`,
`isTransientStatementError :: HasqlErrors.StatementError -> Bool`, and
`isTransientServerError :: HasqlErrors.ServerError -> Bool` are private, as `isTransientSqlState`
already is. `isTransient :: PgmqRuntimeError -> Bool` keeps its type and its truth table except
for the three additions: empty-SQLSTATE server errors in statements and scripts,
transient-SQLSTATE script errors, and `UnexpectedRowCountStatementError 1 1 1`.
`isAmbiguousReply` is `True` for exactly the first and third of those and for nothing else, and
`isAmbiguousReply e` implies `isTransient e` for every `e`.

At the end of M3, `docs/adr/transient-classification-is-value-based.md` exists, the
`docs-check` recipe in `justfile` validates four bundles, and the three OKF records named in
the Plan of Work carry the frontmatter fields their profiles require for their new status.


## Revision Notes

- 2026-09-30: Added the `isAmbiguousReply` classifier at the user's request, after the
  regression-risk review of this plan flagged that making the lost-reply shapes transient lets
  a naive loop re-send a message that may already have committed. Touched Purpose, Progress,
  Decision Log, Plan of Work (M2 and M3), Concrete Steps (classifier code, ClassificationSpec,
  DisconnectSpec, UmbrellaExportsSpec, README, both commit messages), Validation and Acceptance,
  and Interfaces and Dependencies. Milestone count and order are unchanged.
- 2026-09-30: After the permanent-outage review, required every documented retry loop to be
  bounded and made the ambiguous state sticky (reconcile, never re-send, until reconciliation
  succeeds). Replaced the README snippet in Concrete Steps with the bounded sticky loop, added
  the two rules to the design-note guidance in Plan of Work M3, added a Decision Log entry,
  a Validation check, and a post-release follow-up for the Keiro runtime pattern corpus.
