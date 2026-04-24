# Surface Structured Errors Through the pgmq-effectful API

Intention: intention_01kpybay9hegps2fjt7tkwarz6

This MasterPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

This document is maintained in accordance with `.claude/skills/master-plan/MASTERPLAN.md`.


## Vision & Scope

After this initiative, users of the `pgmq-effectful` package can observe, classify,
and recover from every failure mode that can arise during a pgmq operation. No
error is silently discarded, stringified, or thrown outside the `Effectful` `Error`
channel.

Today, the package fails three ways at surfacing errors:

1. The plain `runPgmq` interpreter in `pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs`
   wraps every `hasql-pool` `UsageError` in a single opaque constructor
   `PgmqPoolError :: UsageError -> PgmqError`. Callers cannot match on whether the
   failure was an acquisition timeout, a dropped connection, a SQL state (e.g.,
   `23505` unique violation, `42P01` undefined table), or a decoder mismatch
   without importing hasql internals and pattern-matching three levels deep.

2. The traced interpreter `runPgmqTraced` / `runPgmqTracedWith` in
   `pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs:325-340` calls
   `fail $ "PgmqPoolError: " <> show err`. Inside `IO`, that throws a synchronous
   `IOError` carrying only a string. The function signature has **no** `Error
   PgmqError :> es` constraint, so `runError @PgmqError` around a traced program
   catches nothing. This is a direct violation of the project's guiding principle
   that errors must never be swallowed.

3. `pgmq-effectful`'s `PgmqError` name collides with `Pgmq.Types.PgmqError` from
   `pgmq-core/src/Pgmq/Types.hs`, which is a validation-error sum
   (`InvalidQueueName | InvalidRoutingKey | InvalidTopicPattern`). Any user who
   imports both modules unqualified hits an ambiguous-name error.

The **user-visible behaviors** enabled at the end of this initiative are:

- A program can wrap any effectful pgmq action with `runError @PgmqRuntimeError`
  and the resulting `Either PgmqRuntimeError a` carries the full hasql-pool error
  context — connection kind, SQL state, SQL text, acquisition-timeout flag, row /
  cell decoder details.

- The traced interpreter records the error on the active OpenTelemetry span
  (status + exception event) and *also* throws it into the same `Error` channel
  as the plain interpreter. Observability and recoverability are independent;
  neither hides the other.

- The plain `PgmqError` name is gone from `pgmq-effectful`'s public API
  (superseded by `PgmqRuntimeError`); the validation `PgmqError` in `pgmq-core`
  is unchanged. Users importing both packages together no longer collide.

- The `pgmq-effectful` package has its own test suite covering error propagation
  for both interpreters, demonstrably asserting each error variant is thrown.

- The `README.md` and `pgmq-effectful/CHANGELOG.md` describe the error model and
  how to migrate from 0.1.x.

**Excluded from scope:** Retry logic, circuit breakers, or runtime classification
of which errors should be retried — those can build *on top of* the surfaced
error type, but this initiative only exposes the information faithfully. Changes
to `pgmq-hasql`'s session layer (it already returns `IO (Either UsageError a)`
cleanly, so nothing to fix there). Changes to the validation `PgmqError` in
`pgmq-core`.


## Decomposition Strategy

The work naturally splits along two orthogonal axes:

- **Axis 1 — what the error type is.** Defining a structured runtime-error type,
  resolving the name collision, and deciding which hasql types to re-export.
  This is a pure type-design task with no runtime implications.

- **Axis 2 — how the interpreters propagate it.** The plain interpreter already
  uses `Error` but only throws an opaque constructor; the traced interpreter
  uses `fail` and needs to be rebuilt to (a) take an `Error PgmqRuntimeError` constraint,
  (b) record on the span, and (c) throw into the `Error` channel.

EP-1 handles Axis 1 alone. EP-2 handles Axis 2 alone, consuming the type EP-1
produced. EP-3 polishes the external API shape (re-exports, deprecation aliases
for one release cycle, classification helpers) — it requires both EP-1 and EP-2
to be implemented so the complete surface is visible. EP-4 tests behavior
end-to-end, and can only begin once EP-2 has landed because its whole purpose is
to assert that errors propagate from the traced interpreter too. EP-5 writes
user-facing documentation and cannot be authoritative until EP-3's API is
final.

Alternatives considered and rejected:

- **One combined plan.** Bundling the type definition, traced-interpreter fix,
  API curation, tests, and docs into one ExecPlan produces a plan with 8+
  milestones touching 10+ files across concerns that are independently
  verifiable. The ExecPlan specification explicitly advises breaking this up,
  and bundling loses the ability to ship fixes incrementally (e.g., the
  traced-interpreter fix alone is a CVE-adjacent correctness issue worth
  shipping before API-polish work).

- **Splitting EP-1 into "type" and "rename-collision".** The name collision
  cannot be meaningfully fixed without touching the type's shape, and the type
  cannot be designed without deciding the name. They are one decision.

- **Merging EP-4 into EP-2.** Testing *could* live inside the traced-interpreter
  fix, but EP-4 also tests the plain interpreter (which EP-2 does not touch),
  and EP-4 requires adding a new test suite to `pgmq-effectful.cabal` that no
  currently exists. That test-suite scaffolding is material enough to warrant
  its own plan.

- **Phases.** With 5 child plans and a clean linear-plus-one-branch dependency
  graph, phasing adds bureaucracy without clarity.


## Exec-Plan Registry

| #    | Title                                          | Path                                                          | Hard Deps     | Soft Deps | Status      |
|------|------------------------------------------------|---------------------------------------------------------------|---------------|-----------|-------------|
| EP-1 | Define `PgmqRuntimeError` runtime-error type   | docs/plans/1-pgmq-effectful-error-type.md                     | None          | None      | Complete    |
| EP-2 | Make the traced interpreter propagate typed errors | docs/plans/2-pgmq-effectful-traced-error-propagation.md   | EP-1          | None      | Complete    |
| EP-3 | Curate the pgmq-effectful error API surface    | docs/plans/3-pgmq-effectful-error-api-surface.md              | EP-1, EP-2    | None      | Complete    |
| EP-4 | Error-propagation test suite for pgmq-effectful | docs/plans/4-pgmq-effectful-error-tests.md                   | EP-2          | EP-1      | Complete    |
| EP-5 | Document the error model and migration path    | docs/plans/5-pgmq-effectful-error-docs.md                     | EP-3          | EP-4      | Complete    |

Status values: Not Started, In Progress, Complete, Cancelled.


## Dependency Graph

EP-1 is the foundation: the runtime error type it defines is imported by EP-2's
updated interpreter code, EP-3's re-exports, EP-4's tests, and EP-5's docs. It
has no prerequisites and must land first.

EP-2 hard-depends on EP-1 because the traced interpreter will `throwError` a
value of the EP-1 type. It cannot compile without EP-1's type in scope. EP-2
can proceed in parallel with EP-4 *planning* but not *implementation*, because
EP-4's tests exercise the behavior EP-2 establishes.

EP-3 hard-depends on EP-1 (to know what to re-export) and on EP-2 (because the
api surface includes both interpreters' consistent shape — the deprecation
aliases EP-3 introduces assume both interpreters already throw the new type).

EP-4 hard-depends on EP-2 (tests assert the traced interpreter throws the typed
error, which only becomes true after EP-2). It soft-depends on EP-1 — the
tests will pattern-match on the error constructors EP-1 chose, but EP-4 could
be drafted against the constructor shape EP-1 proposes before EP-1 is fully
merged.

EP-5 hard-depends on EP-3 (documentation of re-exports must describe the final
module surface) and soft-depends on EP-4 (CHANGELOG entries may cite the tests
as evidence of correctness, but this is not blocking).

**Parallelism opportunities.** After EP-1 lands, EP-2 and a first draft of
EP-4's test scaffolding can proceed in parallel if two contributors or
sessions are available; they converge before merging. EP-3 and EP-5 are
strictly serial after their prerequisites.


## Integration Points

Every child plan touches the same small set of modules. Inconsistency here is
the primary risk.

- **Shared artifact: the `PgmqRuntimeError` runtime-error type.** Owner: EP-1,
  defined in `pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs` (or a new
  internal module — EP-1 decides). Consumed by EP-2 (interpreter throws it),
  EP-3 (re-exports from `Pgmq.Effectful`), EP-4 (test assertions match on its
  constructors), EP-5 (docs reference its constructors). **Change protocol:** any
  constructor addition must update the pattern matches in EP-2's interpreter
  conversion helper and the tests in EP-4 simultaneously.

- **Shared artifact: the interpreter signatures `runPgmq`, `runPgmqTraced`,
  `runPgmqTracedWith`.** Owner: EP-2 (publishes the final shape). Before EP-2,
  `runPgmq` has `Error PgmqError :> es` and `runPgmqTraced*` has nothing. After
  EP-2, all three carry `Error PgmqRuntimeError :> es`. EP-3's re-exports assume
  this uniform shape.

- **Shared artifact: `Pgmq.Effectful` module export list.** Owner: EP-3. EP-1
  and EP-2 each touch internal modules but must not add surface-level exports
  to `Pgmq.Effectful`; those belong in EP-3. (EP-1 may provide an
  internal-only module if needed, but EP-3 decides what is public.)

- **Shared artifact: `pgmq-effectful.cabal`.** Owner: EP-4 adds the
  `pgmq-effectful-test` test-suite stanza. EP-1 may need to add a new
  `exposed-module` if it chooses to split out an Internal module. Both edits
  touch the same file; merge conflicts are possible but trivial.

- **Shared artifact: `pgmq-effectful/CHANGELOG.md`.** Owner: EP-5. Every other
  plan that makes a user-visible change describes the change in that plan's
  Surprises & Discoveries or Decision Log; EP-5 reads those and writes the
  final CHANGELOG entry.


## Progress

This section tracks milestone-level progress across all child plans. Each item
names the child plan and the milestone.

- [x] EP-1: New runtime error type designed, named, and implemented; legacy
  `PgmqError` retained for one release cycle (EP-3 finalizes its retirement,
  so the *collision* with `pgmq-core`'s `PgmqError` is not fully resolved
  until EP-3 removes or hides the effectful-side name).
- [x] EP-1: Plain `runPgmq` interpreter throws the new type (still in
  `Pgmq.Effectful.Interpreter`).
- [x] EP-2: `runPgmqTracedWith` and `runPgmqTraced` carry `Error
  PgmqRuntimeError :> es` and call `throwError`, never `fail`.
- [x] EP-2: OpenTelemetry span records the error (status + event) and
  propagates it — by code inspection; EP-4 will confirm at runtime with a
  stub tracer.
- [x] EP-3: `Pgmq.Effectful` module export list re-exports `PgmqRuntimeError`
  and its constructors; old `PgmqError` export retained with a `DEPRECATED`
  pragma (removal planned for 0.3.0).
- [x] EP-3: At least one classification helper (`isTransient`) exposed to
  help users decide retry policy without re-implementing error introspection.
- [x] EP-4: New `pgmq-effectful-test` test suite added to the cabal file and
  runs under `cabal test pgmq-effectful` (11 tests, all passing).
- [x] EP-4: Tests assert the plain interpreter throws `PgmqRuntimeError` for
  connection failure and SQL statement error. (Acquisition-timeout case
  deferred per EP-4's Surprises & Discoveries; the two implemented cases
  are the most common failure modes.)
- [x] EP-4: Tests assert the traced interpreter throws the typed error for
  the statement case. (Span-state inspection deferred — see EP-4's
  Surprises & Discoveries for reasoning and follow-up.)
- [x] EP-5: `README.md` pgmq-effectful section updated with an error-handling
  example.
- [x] EP-5: `pgmq-effectful/CHANGELOG.md` 0.2.0.0 entry describes the
  breaking rename, the traced-interpreter fix, the new features, and a
  before/after migration guide. Design doc
  `docs/design/013-pgmq-effectful-error-model.md` added.


## Surprises & Discoveries

Document cross-plan insights, dependency changes, scope adjustments, or
unexpected interactions between child plans.

- **Hasql public surface for error types (affects EP-1, EP-4).** The error
  types `ConnectionError` and `SessionError` are re-exported from
  `Hasql.Errors`, **not** from `Hasql.Session`. Several child plans'
  draft sketches assumed the latter. Use `import Hasql.Errors qualified as
  HasqlErrors` (or similar) wherever these types appear in signatures or
  pattern matches. EP-1 has already corrected this in the implementation;
  EP-4's test code and EP-5's docs must follow the same convention.
  (2026-04-23)

- **`pgmq-bench` is a downstream consumer of `Pgmq.Effectful.Interpreter`
  (affects EP-1, EP-3).** The benchmark harness at
  `pgmq-bench/bench/BenchSetup.hs` imports and specializes on `PgmqError`.
  EP-1 had to forward-migrate it to `PgmqRuntimeError` to keep
  `cabal build all` green. EP-3, when finalizing the deprecation of
  `PgmqError`, should verify `pgmq-bench` does not reintroduce the legacy
  name. (2026-04-23)

- **Build-env fix bundled with EP-1 (not a master-plan scope change).**
  `cabal.project`'s `hasql-migration` pin was bumped from
  `ab66f6ae93e40065f8532dd9d497ecb15c91122e` to
  `4aaff6c0919d1fe8e1c248c3ce4ce05775c59c8c` (current master of
  `shinzui/hasql-migration`). The old pin failed to compile against the
  current `memory`/`crypton` combination. This is recorded here so future
  readers don't think it's load-bearing for the error-propagation work.
  (2026-04-23)


## Decision Log

- Decision: Rename the pgmq-effectful error type away from `PgmqError`.
  Rationale: There is already a `PgmqError` in `pgmq-core` for validation. Keeping
  the name in two different packages causes ambiguous-name errors on unqualified
  import; `pgmq-core`'s error is widely published (it is part of the
  `parseQueueName`/`parseRoutingKey`/`parseTopicPattern` return types) and is
  the less disruptive of the two to keep. The effectful interpreter's error
  will be `PgmqRuntimeError` (EP-1 may refine to `PgmqHasqlError` or similar if
  that reads better, but the key point is "not `PgmqError`").
  Date: 2026-04-23

- Decision: The runtime error type lives in `pgmq-effectful`, not `pgmq-core`.
  Rationale: `pgmq-core` intentionally has no `hasql` dependency (it only
  depends on aeson/base/template-haskell/text/time). Pulling hasql types into
  the core would cascade transitively into every consumer of `pgmq-core` and
  would create a cycle for the pgmq-migration or future non-hasql backends. The
  price is that users of `pgmq-hasql` directly (not through effectful) continue
  to see raw `Hasql.Pool.UsageError`; that is acceptable because `pgmq-hasql`'s
  session layer is already thin-by-design and users choose it precisely because
  they want direct hasql access.
  Date: 2026-04-23

- Decision: Keep the plain and traced interpreters as two separate entry
  points, rather than unifying via a middleware.
  Rationale: The traced interpreter does meaningful work (OTel span creation
  with semantic conventions per `Pgmq.Effectful.Telemetry`) that would be
  awkward to layer after a plain interpreter. The two interpreters can share
  the session-to-error conversion helper that EP-1 will extract, which
  captures the meaningful commonality without forcing a runtime layer.
  Date: 2026-04-23

- Decision: OpenTelemetry recording happens *in addition to* typed-error
  propagation, not instead of it.
  Rationale: The current traced interpreter's use of `fail` was an attempt to
  keep the error observable through the span by turning it into an IO
  exception. That also made it *invisible* to `runError`. The correct pattern,
  which EP-2 will implement, is: on Left, call `setStatus s (Error ...)`, call
  `addEvent s exceptionEvent`, *then* `throwError` via the `Error` effect. The
  test in EP-4 will assert both channels carry information.
  Date: 2026-04-23

- Decision: No retry or classification beyond a single boolean helper
  (`isTransient`) in the initial release.
  Rationale: Retry policy is application-specific; baking it into the library
  invites users to depend on specific timing and then complain when we change
  it. We expose the raw structure and one convenience; users compose whatever
  they need.
  Date: 2026-04-23


## Outcomes & Retrospective

Outcome (2026-04-23): all five child plans landed. Against the original
vision:

- A program can wrap any effectful pgmq action with
  `runError @PgmqRuntimeError` and the resulting `Either PgmqRuntimeError a`
  carries full hasql context (connection kind, SQL state, session
  structure). ✅ Proven by `PlainInterpreterSpec` and
  `TracedInterpreterSpec` in the new test suite.
- The traced interpreter records the error on the active OpenTelemetry
  span and throws it into the same `Error` channel as the plain
  interpreter. ✅ Implemented in `runSessionIO`; runtime-asserted for
  error propagation; span-recording covered by code inspection (see EP-4
  Surprises for the reason span-inspection was deferred).
- The `PgmqError` name is deprecated (not yet removed) in
  `pgmq-effectful`; `pgmq-core`'s validation `PgmqError` is unchanged.
  Users importing both packages together use `PgmqRuntimeError` from
  `Pgmq.Effectful` and `PgmqError` from `Pgmq.Types` and no longer
  collide. ✅
- `pgmq-effectful` has its own test suite (`pgmq-effectful-test`, 11
  passing tests) covering error propagation for both interpreters. ✅
- `README.md` and `pgmq-effectful/CHANGELOG.md` describe the error model
  and 0.1.x → 0.2.x migration. `docs/design/013` records the design
  rationale. ✅

Cross-plan lessons:

- The plan sketches repeatedly assumed `Hasql.Session` re-exported
  `ConnectionError` / `SessionError`; it does not. The public module is
  `Hasql.Errors`. This came up in EP-1, EP-3, EP-4, and EP-5. Future
  plans that touch hasql error types should start from `Hasql.Errors`
  and note this upfront.
- Downstream consumers (`pgmq-bench`, `pgmq-config`) that specialize on
  a renamed/deprecated type need to be migrated in the same commit
  series as the rename, even if deprecation aliases exist — otherwise
  `cabal build all` (or a cabal file's upper bound) breaks the CI
  signal. EP-1 and EP-5 both hit this.
- Observability and recoverability are orthogonal. EP-2 recorded this
  explicitly in its Decision Log; the surprise was how easy it was to
  keep them separate by letting `inSpan'` run to completion with an
  `Either` payload, then throwing at the Eff layer after the bracket
  closes.

Version shipped: pgmq-effectful 0.2.0.0. The legacy `PgmqError` name
remains deprecated; removal planned for 0.3.0.0.
