# Error-propagation test suite for pgmq-effectful

MasterPlan: docs/masterplans/1-pgmq-effectful-error-propagation.md
Intention: intention_01kpybay9hegps2fjt7tkwarz6

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

This document is maintained in accordance with `.claude/skills/exec-plan/PLANS.md`.


## Purpose / Big Picture

After this plan lands, `cabal test pgmq-effectful` runs a Tasty test suite
that demonstrably proves:

1. The plain `runPgmq` interpreter throws a `PgmqRuntimeError` via the
   `Error` effect for three representative failure modes:
   - statement error (e.g., deleting from a non-existent queue produces a
     `ServerStatementError` with a specific SQL state);
   - connection error (e.g., bad connection string produces a
     `PgmqConnectionError`);
   - acquisition timeout (e.g., zero-size pool with a short acquisition
     timeout produces `PgmqAcquisitionTimeout`).

2. The traced interpreter `runPgmqTraced` throws the **same** errors in the
   **same** channel *and* records them on the OpenTelemetry span visible to
   a stub exporter.

3. `isTransient` returns the expected boolean for each error constructor.

The test suite is the durable proof that errors are not swallowed. Without
these tests, a future refactor could regress the traced interpreter to
`fail` and CI would not notice.

**How to see it working:**

    cabal test pgmq-effectful
    # ... output showing all tests pass ...

Output includes lines like:

    pgmq-effectful
      Plain interpreter error propagation
        statement error surfaces PgmqSessionError:         OK
        connection error surfaces PgmqConnectionError:     OK
        acquisition timeout surfaces PgmqAcquisitionTimeout: OK
      Traced interpreter error propagation
        statement error surfaces PgmqSessionError:         OK
        records the exception on the span:                 OK
      isTransient classification
        acquisition timeout is transient:                  OK
        auth failure is not transient:                     OK
        statement error is not transient:                  OK


## Progress

- [x] Verify EP-1 and EP-2 have landed. (2026-04-23)
- [x] Add a `test-suite pgmq-effectful-test` stanza to
  `pgmq-effectful/pgmq-effectful.cabal`. (2026-04-23)
- [x] Create `pgmq-effectful/test/` directory. (2026-04-23)
- [x] Create `pgmq-effectful/test/Main.hs` as the tasty entry point.
  (2026-04-23)
- [x] Create `pgmq-effectful/test/EphemeralDb.hs` (copy of pgmq-hasql's
  helper, slimmed down). (2026-04-23)
- [x] Create `pgmq-effectful/test/PlainInterpreterSpec.hs`. (2026-04-23)
- [x] Create `pgmq-effectful/test/TracedInterpreterSpec.hs`. (2026-04-23)
- [x] Create `pgmq-effectful/test/ClassificationSpec.hs`. (2026-04-23)
- [x] Run `cabal test pgmq-effectful` — "All 11 tests passed (0.01s)".
  (2026-04-23)
- [x] Run `cabal test pgmq-hasql` to confirm no regression — "All 55 tests
  passed (1.09s)". (2026-04-23)
- [x] `nix fmt` clean. (2026-04-23)
- [x] Commit with the three required trailers.


## Surprises & Discoveries

- The plan's `ClassificationSpec` import sketch used
  `import Hasql.Session qualified as Hasql` to access constructors like
  `NetworkingConnectionError`. As with EP-1 and EP-3, that module does not
  re-export them. Implementation uses
  `import Hasql.Errors qualified as HasqlErrors`. (2026-04-23)

- The plan's `TracedInterpreterSpec` sketch used
  `TracerName (..)` from `OpenTelemetry.Trace.Core`. The actual API takes
  an `InstrumentationLibrary`, which has an `IsString` instance, so a
  string literal with `OverloadedStrings` suffices:
  `OTel.makeTracer tp "pgmq-effectful-test" OTel.tracerOptions`.
  (2026-04-23)

- Span-state inspection deferred. The plan contemplated using
  `hs-opentelemetry-sdk`\'s `InMemorySpanExporter` to assert the traced
  interpreter records error status + exception event on the span. That
  would require adding `hs-opentelemetry-sdk` as a test-suite dependency
  and wiring up a `SpanProcessor`. The current test uses a `Tracer` built
  from an empty `TracerProvider` (no processors), which still exercises
  the `inSpan'` bracket and verifies the typed-error propagation path.
  The span-recording code in the traced interpreter is covered by hand
  inspection (EP-2's Outcomes) but not by a runtime assertion. Follow-up
  candidate if it becomes necessary. (2026-04-23)

- `parseQueueName` returns `Either PgmqError QueueName` (the pgmq-core
  validation error). In a test we cannot use lazy let-bindings with
  irrefutable patterns across IO actions without a `fail`, so the tests
  use an explicit `case` with `assertFailure` in the error branch.
  (2026-04-23)


## Decision Log

- Decision: Copy (not share) the `EphemeralDb` helper from pgmq-hasql's test
  suite into pgmq-effectful's test suite.
  Rationale: Sharing a test helper across packages would require either
  publishing a test-helper library, using a cross-package
  `hs-source-dirs` reference (fragile), or a submodule dance. Copying ~50
  lines of scaffolding is the lowest-friction choice. If we acquire a third
  test-suite needing the same helper, we promote it to a shared internal
  test-helper package.
  Date: 2026-04-23

- Decision: Use `ephemeral-pg` for integration tests (same as pgmq-hasql).
  Rationale: The existing test infrastructure is proven, uses `withCached`
  for fast startup, and avoids external Postgres dependencies. The user's
  global CLAUDE.md affirms that tests use `ephemeral-pg` to spin up temporary
  PostgreSQL instances and no external setup is required.
  Date: 2026-04-23

- Decision: For the "acquisition timeout" test, use a pool with size 1
  already holding a connection busy, plus a short acquisition timeout,
  rather than size 0.
  Rationale: `Hasql.Pool` requires `size >= 1` (verify empirically; if
  `size 0` is accepted, prefer it — simpler).
  Update this entry during implementation based on what works.
  Date: 2026-04-23

- Decision: For the "connection error" test, point at an unreachable host
  (e.g., `localhost:1`) with a short connect timeout.
  Rationale: Stable across environments; does not require manipulating an
  actual database's state. The exact ConnectionError constructor produced
  (`NetworkingConnectionError` vs `OtherConnectionError`) may vary; the test
  asserts only that `PgmqConnectionError _` fires, not the specific
  constructor.
  Date: 2026-04-23

- Decision: For the traced-interpreter span assertion, use the in-memory
  `SpanProcessor` that OpenTelemetry provides via the
  `hs-opentelemetry-sdk` or build a small stub.
  Rationale: The project already has `hs-opentelemetry-api` as a dep; we need
  the sdk's `InMemorySpanProcessor` or equivalent to inspect exported spans.
  If the sdk is not available in the project's nix closure, fall back to
  building a minimal `Tracer` that records span status into an `IORef` —
  record the choice here during implementation.
  Date: 2026-04-23


## Outcomes & Retrospective

Outcome (2026-04-23): `cabal test pgmq-effectful` runs 11 passing tests
across three groups:

    pgmq-effectful
      isTransient classification                   (8 tests, all pure)
      Plain interpreter error propagation          (2 tests: statement, connection)
      Traced interpreter error propagation         (1 test: statement)

All 11 pass. The pgmq-hasql suite also still passes ("All 55 tests
passed"), confirming no cross-package regression.

Gaps vs. plan:

- Acquisition-timeout test deferred (the plan already marked this as
  optional). Reliably producing `AcquisitionTimeoutUsageError` requires a
  forked busy holder + tuned timeouts, which is fiddly in a CI context.
- Span-state inspection deferred — documented in Surprises &
  Discoveries. The traced-interpreter test proves the typed-error
  propagation path but not the span-recording path.

Lessons: the combination of a `case ... of` pattern match on
`(CallStack, PgmqRuntimeError)` plus an assertion on the outer
constructor shape is an effective, low-noise way to verify error
propagation. Future tests can layer in SQL-state inspection by matching
deeper into `PgmqSessionError (StatementSessionError ...
(ServerStatementError (ServerError code ...)))`.


## Context and Orientation

**Prerequisites.** EP-2 must have landed: `runPgmqTraced` and
`runPgmqTracedWith` carry `Error PgmqRuntimeError :> es`. EP-1 has the
`PgmqRuntimeError` type and `fromUsageError`.

**`pgmq-effectful` currently has no test suite.** Confirm:

    ls pgmq-effectful/

shows only `CHANGELOG.md`, `LICENSE`, `pgmq-effectful.cabal`, and `src/`. This
plan creates the `test/` directory.

**Reference test suite.** The pgmq-hasql tests at `pgmq-hasql/test/` are the
pattern to follow. Key files:
- `pgmq-hasql/test/Main.hs` — Tasty entry point.
- `pgmq-hasql/test/EphemeralDb.hs` — `withPgmqPool` helper that spins up an
  ephemeral Postgres, installs the pgmq schema via `pgmq-migration`, and
  hands back a pool.
- `pgmq-hasql/test/QueueSpec.hs` — example test-module structure.

**ephemeral-pg usage.**
`pgmq-hasql/test/EphemeralDb.hs` shows the pattern:

    import EphemeralPg (StartError, connectionSettings, withCached)

    withPgmqDb :: (Pool.Pool -> IO a) -> IO (Either StartError a)
    withPgmqDb action = withCached $ \db -> do
      let connSettings = connectionSettings db
          poolConfig =
            PoolConfig.settings
              [ PoolConfig.size 3,
                PoolConfig.staticConnectionSettings connSettings
              ]
      pool <- Pool.acquire poolConfig
      installResult <- Pool.use pool Migration.migrate
      case installResult of
        Left poolErr -> error $ "Failed to install pgmq schema: " <> show poolErr
        Right (Left migrationErr) -> error $ "Migration failed: " <> show migrationErr
        Right (Right ()) -> action pool

**Effectful test pattern.** To run an effectful computation and inspect the
error, use:

    import Effectful (runEff)
    import Effectful.Error.Static (runError)
    import Pgmq.Effectful

    result :: IO (Either (CallStack, PgmqRuntimeError) a)
    result = runEff
           . runError @PgmqRuntimeError
           . runPgmq pool
           $ action

Then in the test:

    result <- runEff . runError @PgmqRuntimeError . runPgmq pool $ action
    case result of
      Left (_cs, PgmqSessionError _) -> pure ()  -- ok
      Left (_cs, other) ->
        assertFailure $ "Expected PgmqSessionError, got " <> show other
      Right _ ->
        assertFailure "Expected an error, got success"

**Triggering a statement error.** Call `deleteMessage` for a queue that does
not exist. The pgmq SQL function raises an exception, which becomes a
`StatementSessionError` wrapping a `ServerStatementError (ServerError sqlState
...)`. The exact SQL state depends on pgmq's implementation (it may be a
generic `P0001` raise-exception, or `42P01` undefined table, depending on how
pgmq's `pgmq.delete(text, bigint)` signals failure). The test asserts the
outer shape (`PgmqSessionError _`), not the inner SQL state — the exact
state is a property of pgmq, not of this package.

**Triggering a connection error.** Build a pool with connection settings
pointing at an unreachable host:

    badConnSettings = "host=127.0.0.1 port=1 user=nobody dbname=nonexistent connect_timeout=1"

A Hasql pool will report a `ConnectionUsageError (NetworkingConnectionError
_)` on first use. Converting via `fromUsageError` yields `PgmqConnectionError
(NetworkingConnectionError _)`.

**Triggering an acquisition timeout.** Use `PoolConfig.size 1` and
`PoolConfig.acquisitionTimeout (fromInteger 100 :: DiffTime)` (100ms).
Hold the only connection busy in a forked thread running a long-running
pg-sleep session, then from the main thread try a normal operation; it
blocks for the timeout and returns `AcquisitionTimeoutUsageError`.

**Inspecting OpenTelemetry span exports.** The `hs-opentelemetry-api` package
provides `TracerProvider` and `Tracer` types but not a batteries-included
in-memory exporter. The `hs-opentelemetry-sdk` package provides
`InMemorySpanExporter` (or equivalent). Check the project's dependency
closure:

    mori registry show iand675/hs-opentelemetry --full

The simpler alternative: use an `IORef [ExportedSpan]` stub. `hs-opentelemetry-api`'s
`Tracer` is constructed from a `TracerProvider`, which in turn wraps a
`SpanProcessor` that receives each span on end. A minimal custom
`SpanProcessor` that appends exported spans to an `IORef` is ~20 lines.

If neither approach is practical within the project's dep closure, shift
scope: the traced-interpreter test asserts the typed error is thrown (same as
the plain test) but does **not** assert span recording. Document the gap in
Surprises & Discoveries; open a follow-up issue. The error-propagation
guarantee is the priority; span-assertion coverage is a bonus.


## Plan of Work

Three milestones.

### Milestone 1 — Wire up a new test suite that builds

**Scope.** Add the `test-suite` stanza to the cabal file, create the four
scaffolding files (`Main.hs`, `EphemeralDb.hs`, and two stub specs), and
verify the suite builds and runs a trivial passing test.

**What will exist at the end:** an empty-but-building pgmq-effectful test
suite reachable via `cabal test pgmq-effectful`.

**Commands:**

    cabal test pgmq-effectful

**Acceptance:** output shows at least one `OK` line.

### Milestone 2 — Plain interpreter error-propagation tests

**Scope.** Implement `PlainInterpreterSpec.hs` with three tests: statement
error, connection error, acquisition timeout. Also implement
`ClassificationSpec.hs` with a handful of `isTransient` assertions.

**What will exist at the end:** all tests in
`PlainInterpreterSpec.hs` and `ClassificationSpec.hs` pass. The classification
test has no database dependency; it exercises pure functions.

**Commands:** `cabal test pgmq-effectful`.

**Acceptance:** each named test reports `OK`.

### Milestone 3 — Traced interpreter error-propagation tests

**Scope.** Implement `TracedInterpreterSpec.hs`. Minimum: assert the traced
interpreter throws the typed error for a statement error case. Bonus (if
the OTel exporter setup is practical): assert the span status is `Error` and
an exception event was recorded.

**What will exist at the end:** at least one traced-interpreter test passes.
Ideally all three error categories are covered, with span-state inspection
for at least the statement case.

**Commands:** `cabal test pgmq-effectful`.

**Acceptance:** traced-interpreter test(s) report `OK`. If span inspection
was deferred, the Outcomes section and/or Surprises section document why.


## Concrete Steps

From the repository root.

### Step 1 — Verify prerequisites.

    grep -n "Error PgmqRuntimeError" pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs
    grep -n "PgmqRuntimeError\|fromUsageError\|isTransient" pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs

Both must show matches. If not, an earlier plan is incomplete.

### Step 2 — Enter dev shell.

    nix develop

### Step 3 — Add test-suite stanza to
`pgmq-effectful/pgmq-effectful.cabal`.

Append after the `library` stanza:

    test-suite pgmq-effectful-test
      import:             warnings
      default-language:   GHC2024
      type:               exitcode-stdio-1.0
      hs-source-dirs:     test
      main-is:            Main.hs
      ghc-options:        -threaded -rtsopts -with-rtsopts=-N
      other-modules:
        ClassificationSpec
        EphemeralDb
        PlainInterpreterSpec
        TracedInterpreterSpec

      default-extensions:
        DataKinds
        ImportQualifiedPost
        OverloadedRecordDot
        OverloadedStrings
        TypeOperators

      build-depends:
        , aeson
        , base                              >=4.18  && <5
        , effectful-core
        , ephemeral-pg                      >=0.2.1
        , hasql
        , hasql-pool                        ^>=1.4
        , hs-opentelemetry-api
        , pgmq-core
        , pgmq-effectful
        , pgmq-hasql
        , pgmq-migration
        , random                            ^>=1.2
        , tasty                             ^>=1.5
        , tasty-hunit                       ^>=0.10
        , text
        , time                              ^>=1.14

### Step 4 — Create `pgmq-effectful/test/EphemeralDb.hs`.

Copy the contents of `pgmq-hasql/test/EphemeralDb.hs` verbatim. The module
provides `withPgmqPool` and `TestFixture`. Keep the module name as
`EphemeralDb` in the `pgmq-effectful` test suite's module namespace.

### Step 5 — Create `pgmq-effectful/test/Main.hs`.

    {-# LANGUAGE OverloadedStrings #-}

    module Main (main) where

    import ClassificationSpec qualified
    import EphemeralDb (withPgmqPool)
    import PlainInterpreterSpec qualified
    import Test.Tasty (defaultMain, testGroup)
    import TracedInterpreterSpec qualified

    main :: IO ()
    main = do
      -- Classification tests are pure and need no database.
      -- Interpreter tests need an ephemeral Postgres.
      result <- withPgmqPool $ \pool -> do
        let tree =
              testGroup
                "pgmq-effectful"
                [ ClassificationSpec.tests,
                  PlainInterpreterSpec.tests pool,
                  TracedInterpreterSpec.tests pool
                ]
        defaultMain tree
      case result of
        Left err -> error $ "Failed to start temp database: " <> show err
        Right () -> pure ()

### Step 6 — Create `pgmq-effectful/test/ClassificationSpec.hs`.

Unit tests for `isTransient`. No DB needed.

    {-# LANGUAGE OverloadedStrings #-}

    module ClassificationSpec (tests) where

    import Hasql.Session qualified as Hasql
    import Pgmq.Effectful
    import Test.Tasty (TestTree, testGroup)
    import Test.Tasty.HUnit (assertBool, testCase)

    tests :: TestTree
    tests =
      testGroup
        "isTransient classification"
        [ testCase "acquisition timeout is transient" $
            assertBool "" (isTransient PgmqAcquisitionTimeout),
          testCase "networking connection error is transient" $
            assertBool
              ""
              (isTransient (PgmqConnectionError (Hasql.NetworkingConnectionError "refused"))),
          testCase "authentication error is not transient" $
            assertBool
              ""
              (not (isTransient (PgmqConnectionError (Hasql.AuthenticationConnectionError "bad password")))),
          testCase "compatibility error is not transient" $
            assertBool
              ""
              (not (isTransient (PgmqConnectionError (Hasql.CompatibilityConnectionError "version mismatch")))),
          testCase "other connection error is treated as transient" $
            assertBool
              ""
              (isTransient (PgmqConnectionError (Hasql.OtherConnectionError "libpq says no"))),
          testCase "connection-drop session error is transient" $
            assertBool
              ""
              (isTransient (PgmqSessionError (Hasql.ConnectionSessionError "dropped"))),
          testCase "driver session error is not transient" $
            assertBool
              ""
              (not (isTransient (PgmqSessionError (Hasql.DriverSessionError "bug"))))
        ]

### Step 7 — Create `pgmq-effectful/test/PlainInterpreterSpec.hs`.

    {-# LANGUAGE OverloadedStrings #-}

    module PlainInterpreterSpec (tests) where

    import Control.Concurrent (forkIO, threadDelay)
    import Data.Text qualified as T
    import Data.Time (secondsToDiffTime)
    import Effectful (runEff)
    import Effectful.Error.Static (runError)
    import Hasql.Pool qualified as Pool
    import Hasql.Pool.Config qualified as PoolConfig
    import Pgmq.Effectful
    import Pgmq.Types (MessageId (..), parseQueueName)
    import Test.Tasty (TestTree, testGroup)
    import Test.Tasty.HUnit (assertFailure, testCase)

    tests :: Pool.Pool -> TestTree
    tests pool =
      testGroup
        "Plain interpreter error propagation"
        [ testCase "statement error surfaces PgmqSessionError" $ do
            let Right bogus = parseQueueName "queue_that_does_not_exist_xyz"
            result <-
              runEff
                . runError @PgmqRuntimeError
                . runPgmq pool
                $ deleteMessage (MessageQuery bogus (MessageId 1))
            case result of
              Left (_cs, PgmqSessionError _) -> pure ()
              Left (_cs, other) ->
                assertFailure $
                  "Expected PgmqSessionError, got " <> show other
              Right _ ->
                assertFailure
                  "Expected an error deleting from missing queue, got success",
          testCase "connection error surfaces PgmqConnectionError" $ do
            -- Build a deliberately bad pool.
            let badCfg =
                  PoolConfig.settings
                    [ PoolConfig.size 1,
                      PoolConfig.staticConnectionSettings
                        "host=127.0.0.1 port=1 user=nobody dbname=nonexistent connect_timeout=1"
                    ]
            badPool <- Pool.acquire badCfg
            result <-
              runEff
                . runError @PgmqRuntimeError
                . runPgmq badPool
                $ listQueues
            Pool.release badPool
            case result of
              Left (_, PgmqConnectionError _) -> pure ()
              Left (_, other) ->
                assertFailure $
                  "Expected PgmqConnectionError, got " <> show other
              Right _ ->
                assertFailure
                  "Expected a connection error, got success"
        ]

(Acquisition-timeout test deferred to a follow-up if the pool configuration
proves fiddly; the two tests above are the core proofs.)

### Step 8 — Create `pgmq-effectful/test/TracedInterpreterSpec.hs`.

Minimum viable form: assert the traced interpreter throws the typed error
for the statement-error case. Span-inspection layered in if practical.

    {-# LANGUAGE OverloadedStrings #-}

    module TracedInterpreterSpec (tests) where

    import Effectful (runEff)
    import Effectful.Error.Static (runError)
    import Hasql.Pool qualified as Pool
    import OpenTelemetry.Trace.Core qualified as OTel
    import OpenTelemetry.Trace.Core (tracerOptions, makeTracer, TracerProvider, TracerName (..))
    import Pgmq.Effectful
    import Pgmq.Types (MessageId (..), parseQueueName)
    import Test.Tasty (TestTree, testGroup)
    import Test.Tasty.HUnit (assertFailure, testCase)

    tests :: Pool.Pool -> TestTree
    tests pool =
      testGroup
        "Traced interpreter error propagation"
        [ testCase "statement error surfaces PgmqSessionError" $ do
            tracer <- noopTracer
            let Right bogus = parseQueueName "queue_that_does_not_exist_xyz2"
            result <-
              runEff
                . runError @PgmqRuntimeError
                . runPgmqTraced pool tracer
                $ deleteMessage (MessageQuery bogus (MessageId 1))
            case result of
              Left (_, PgmqSessionError _) -> pure ()
              Left (_, other) ->
                assertFailure $
                  "Expected PgmqSessionError, got " <> show other
              Right _ ->
                assertFailure
                  "Expected an error, got success"
        ]
      where
        noopTracer :: IO OTel.Tracer
        noopTracer = do
          tp <- OTel.createTracerProvider [] OTel.emptyTracerProviderOptions
          pure $ makeTracer tp (TracerName "pgmq-effectful-test") tracerOptions

The API names `createTracerProvider` / `emptyTracerProviderOptions` /
`TracerName` correspond to `OpenTelemetry.Trace` in the
`hs-opentelemetry-api` package. Verify by:

    mori registry show iand675/hs-opentelemetry

and reading the exposed modules list. If the exact names differ, adjust to
whatever the API in scope actually exports; the point of the test is the
`result` case-match on `PgmqSessionError`, not the tracer construction.

If a real tracer proves too fiddly, use the global tracer provider (which
defaults to no-op):

    tracer <- do
      tp <- OTel.getGlobalTracerProvider
      pure $ makeTracer tp (TracerName "pgmq-effectful-test") tracerOptions

### Step 9 — Build and run.

    cabal build pgmq-effectful-test
    cabal test pgmq-effectful

Expected: all tests pass. Tasty output shows the testgroup and each case.

If `cabal test` reports "Could not resolve dependencies," inspect the
build-depends section of the test stanza and verify every listed package is
already in the project's dep closure (mori registry list / cabal.project /
flake.nix).

### Step 10 — Format and commit.

    nix fmt

    git add pgmq-effectful/pgmq-effectful.cabal \
            pgmq-effectful/test/Main.hs \
            pgmq-effectful/test/EphemeralDb.hs \
            pgmq-effectful/test/ClassificationSpec.hs \
            pgmq-effectful/test/PlainInterpreterSpec.hs \
            pgmq-effectful/test/TracedInterpreterSpec.hs

    git commit -m "$(cat <<'EOF'
    test(pgmq-effectful): cover error propagation for both interpreters

    Adds a new test-suite with three groups:
     - PlainInterpreterSpec: statement and connection errors surface as
       typed PgmqRuntimeError values via runError.
     - TracedInterpreterSpec: the traced interpreter also throws typed
       errors (closes the gap where `fail` previously swallowed them).
     - ClassificationSpec: isTransient returns the expected boolean for
       every constructor.

    Tests use ephemeral-pg for DB setup, matching the pgmq-hasql pattern.

    MasterPlan: docs/masterplans/1-pgmq-effectful-error-propagation.md
    ExecPlan: docs/plans/4-pgmq-effectful-error-tests.md
    Intention: intention_01kpybay9hegps2fjt7tkwarz6
    EOF
    )"


## Validation and Acceptance

Complete when:

1. `cabal test pgmq-effectful` exits 0.
2. Tasty output lists ≥ 6 passing tests total across the three groups.
3. The pgmq-hasql and pgmq-migration test suites still pass:
   `cabal test pgmq-hasql pgmq-migration`.
4. Commit has the three trailers.


## Idempotence and Recovery

The test suite is additive; nothing outside `pgmq-effectful/test/` and the
test-suite stanza in the cabal file is modified. To abandon:

    git restore pgmq-effectful/pgmq-effectful.cabal
    rm -r pgmq-effectful/test


## Interfaces and Dependencies

New build-deps for the test suite: `ephemeral-pg >=0.2.1`, `hasql`,
`hasql-pool ^>=1.4`, `pgmq-migration`, `random ^>=1.2`, `tasty ^>=1.5`,
`tasty-hunit ^>=0.10`, `time ^>=1.14`. All are already used by the
pgmq-hasql test suite, so they are in the project's flake-managed dep
closure.

**The Tasty test tree shape:**

    "pgmq-effectful"
      "isTransient classification"                [pure; no DB]
      "Plain interpreter error propagation"       [needs pool]
      "Traced interpreter error propagation"      [needs pool]

No module outside `pgmq-effectful/test/` is touched (except the one-stanza
addition to the cabal file).
