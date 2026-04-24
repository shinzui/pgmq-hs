# Make the traced pgmq-effectful interpreter propagate typed errors

MasterPlan: docs/masterplans/1-pgmq-effectful-error-propagation.md
Intention: intention_01kpybay9hegps2fjt7tkwarz6

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

This document is maintained in accordance with `.claude/skills/exec-plan/PLANS.md`.


## Purpose / Big Picture

After this plan lands, running any pgmq operation under `runPgmqTraced` or
`runPgmqTracedWith` and wrapping with `runError @PgmqRuntimeError` produces a
`Left PgmqRuntimeError` carrying the full hasql context **while also**
recording the exception on the active OpenTelemetry span. Today, the traced
interpreter discards the error into a string via `fail`, which throws a
`userError` in `IO` that:

- is not caught by `runError @PgmqError` (the `Error` effect only catches
  values thrown via `throwError`, not arbitrary `IOError`s);
- stringifies the entire `UsageError` value, losing all structure;
- has no `Error _ :> es` constraint on the interpreter, so the type system
  silently lets you run `runPgmqTraced` without any error-handling layer at
  all.

This is a textbook "swallowed error" bug: structured information goes in, a
string-ified exception comes out, and nobody up the stack can do anything
about it except print it.

**How to see it working after this plan.** Write a small effectful program
that sends a message to a non-existent queue and wraps execution with
`runError`:

    import Effectful
    import Effectful.Error.Static (runError)
    import Pgmq.Effectful
    import OpenTelemetry.Trace.Core qualified as OTel

    main :: IO ()
    main = do
      tracer <- ...
      pool <- ...
      result <- runEff
              . runError @PgmqRuntimeError
              . runPgmqTraced pool tracer
              $ sendMessage $ SendMessage
                  { queueName = bogusQueue, ... }

      case result of
        Left (_callStack, PgmqSessionError sessErr) -> do
          putStrLn "Got the structured session error:"
          print sessErr                     -- e.g., StatementSessionError ... ServerStatementError (ServerError "42P01" "relation pgmq.q_bogus_queue does not exist" ...)
        Left (_, other) -> print other
        Right _ -> error "expected failure"

Before this plan: the `fail` inside the interpreter escapes as an uncaught
`IOError`. After this plan: the program prints the structured error and
exits normally.

Simultaneously, if a span-exporter is wired up, the same error appears on the
span as:
- `span.status = Error "PgmqSessionError ..."`,
- an `"exception"` event with attributes `exception.type` and
  `exception.message`.

Observability and recoverability are independent. The span sees the error; the
caller also sees it.


## Progress

- [x] Verify that EP-1 has landed — confirmed `PgmqRuntimeError`,
  `fromUsageError`, and the updated `runPgmq` signature exist in
  `pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs`. (2026-04-23)
- [x] Update `runPgmqTracedWith`'s type signature to add `Error
  PgmqRuntimeError :> es`. (2026-04-23)
- [x] Update `runPgmqTraced`'s type signature to match. (2026-04-23)
- [x] Rewrite `runSessionIO` to return `IO (Either PgmqRuntimeError a)`
  instead of `IO a`. (2026-04-23)
- [x] OTel recording calls (`setStatus`, `addEvent`) remain on the Left
  branch *before* the `pure $ Left ...` return. (2026-04-23)
- [x] Remove the `fail $ "PgmqPoolError: " <> show err` call entirely.
  (2026-04-23: confirmed by `grep -n "fail " ...` returning exit code 1 with
  no matches.)
- [x] Update all six `withTracedSession*` helpers to carry `Error
  PgmqRuntimeError :> es` and rethrow via a shared `throwOnLeft` helper.
  (2026-04-23: introduced `throwOnLeft :: Either PgmqRuntimeError a -> Eff
  es a` to keep the six call sites tidy.)
- [x] Update the module-level docstring example to `runError
  @PgmqRuntimeError`. (2026-04-23)
- [x] `cabal build pgmq-effectful` passes with no warnings. (2026-04-23)
- [x] `cabal build all` passes. (2026-04-23)
- [x] `nix fmt` passes. (2026-04-23: reported "formatted 4 files (0
  changed)".)
- [x] Commit with `MasterPlan:`, `ExecPlan:`, `Intention:` trailers.


## Surprises & Discoveries

- Introduced a small local helper `throwOnLeft :: (Error PgmqRuntimeError :>
  es) => Either PgmqRuntimeError a -> Eff es a` in
  `Pgmq.Effectful.Interpreter.Traced` to avoid duplicating the six-line
  "case result of Left -> throwError; Right -> pure" block across all six
  `withTracedSession*` helpers. Not strictly required by the plan — the
  plan's Concrete Step 5 shows the case-of inlined — but cutting the
  duplication kept every helper to two lines of post-`inSpan'` code.
  (2026-04-23)


## Decision Log

- Decision: Preserve the OpenTelemetry span recording (status + exception
  event), but move it to occur *before* throwing via the `Error` effect.
  Rationale: The current traced interpreter has observability as a side
  effect that is reached only because `fail` throws to IO and thus the span's
  `inSpan'` bracket catches and records it. We lose that automatic bracketing
  if we throw at the Eff layer, so we must explicitly record before throwing.
  This is also more correct: `inSpan'` catches IO exceptions, not Eff
  `Error`-effect throws, so even today the span recording works only by
  accident of `fail`-based propagation.
  Date: 2026-04-23

- Decision: Use `OTel.inSpan'` with a `do` action that always returns
  `Either PgmqRuntimeError a`, then lift the Either back into Eff via
  `throwError` on Left.
  Rationale: `inSpan'` takes a function that operates in `MonadIO`. Returning
  `Either` keeps the error in a pure value that we can inspect after the span
  closes. We cannot throw the Eff `Error` effect directly inside `inSpan'`'s
  continuation because the continuation runs in `IO`, not `Eff es`.
  Date: 2026-04-23

- Decision: Do not use `recordException` from `OpenTelemetry.Trace` — keep
  the current hand-rolled `addEvent` + `setStatus` approach.
  Rationale: The current code's `recordUsageError` function (lines 342-353 of
  `pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs`) records specific
  attributes (`exception.type`, `exception.message`) using `TextAttribute`.
  Switching to `recordException` would be a behavior change orthogonal to
  this plan; leave it alone.
  Date: 2026-04-23

- Decision: Drop the `config.recordExceptions` flag's special-case. The
  current code reads `when config.recordExceptions $ recordUsageError s err`.
  We keep the flag and the same guard; it still governs whether an exception
  event is added to the span. It does not govern whether the error is thrown
  to the caller — that now always happens.
  Rationale: `recordExceptions` is a span-level observability setting, not an
  error-propagation setting. Separating these concerns is the point of this
  plan.
  Date: 2026-04-23


## Outcomes & Retrospective

Outcome (2026-04-23): `runPgmqTraced` and `runPgmqTracedWith` now both carry
`(IOE :> es, Error PgmqRuntimeError :> es)` — the exact same effect-row
shape as the plain `runPgmq`. The traced interpreter no longer calls
`fail`; instead, on a hasql-pool error it records status and the exception
event on the active OTel span (subject to
`TracingConfig.recordExceptions`), then returns `Left PgmqRuntimeError`
from `runSessionIO`, which the caller throws via `throwError`.

Gaps vs. plan: none. The only structural deviation is the introduction of
a tiny `throwOnLeft` helper noted in Surprises & Discoveries.

Lessons: separating "record on span" from "throw at the Eff layer" made
the type story uniform (every traced helper has the same two-line
post-`inSpan'` tail) and let observability and recoverability coexist
without one hiding the other.


## Context and Orientation

Assume no prior knowledge of this repository. The reader sees the current
working tree, the referenced EP-1 plan, and this plan.

**This plan's subject file.**
`pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs` is a 410-line module
that defines a pgmq-effect interpreter wrapped with OpenTelemetry tracing. It
creates a span per pgmq operation, attaches semantic-convention attributes
(queue name, message ID, batch size, routing key, etc.), and — in its
current implementation — calls `fail` on hasql-pool errors.

**The bug this plan fixes.** Lines 325-340 of that file define:

    runSessionIO ::
      TracingConfig ->
      OTel.Span ->
      Pool ->
      Hasql.Session.Session a ->
      IO a
    runSessionIO config s pool session = do
      result <- Pool.use pool session
      case result of
        Left err -> do
          when config.recordExceptions $ recordUsageError s err
          OTel.setStatus s (OTel.Error $ T.pack $ show err)
          fail $ "PgmqPoolError: " <> show err
        Right a -> do
          OTel.setStatus s OTel.Ok
          pure a

The `fail` call runs inside `IO`, so it translates to `throwIO (userError
"PgmqPoolError: ...")`. That IO exception escapes out through `OTel.inSpan'`
and then out through `Effectful.liftIO`. Once it crosses `Eff`, it is a plain
runtime exception, *not* an `Error`-effect throw.

`Effectful.Error.Static`'s documentation is explicit
(`/Users/shinzui/Keikaku/hub/haskell/effectful-project/effectful/effectful-core/src/Effectful/Error/Static.hs:5-21`):

> The 'Error' effect is __not__ a general mechanism for handling regular
> exceptions. […] Regular exceptions of type @e@ are distinct from errors of
> type @e@ and will __not__ be caught by functions from this module.

So `runError @PgmqError` around `runPgmqTraced` today is a lie; the caller
thinks they are catching typed errors, but any actual failure bypasses them
entirely.

**EP-1's artifacts.** By the time you implement this plan, EP-1 has added to
`Pgmq.Effectful.Interpreter`:

    data PgmqRuntimeError
      = PgmqAcquisitionTimeout
      | PgmqConnectionError Hasql.Session.ConnectionError
      | PgmqSessionError Hasql.Session.SessionError
      deriving stock (Show, Eq, Generic)

    instance Exception PgmqRuntimeError

    fromUsageError :: Hasql.Pool.UsageError -> PgmqRuntimeError

You use `fromUsageError` in this plan to convert the `Left UsageError` before
throwing.

**The six `withTracedSession*` helpers.** The current file defines six
wrappers (`withTracedSession`, `withTracedSessionNoQueue`,
`withTracedSessionWithMsgId`, `withTracedSessionWithCount`,
`withTracedSessionWithRoutingKey`, `withTracedSessionWithRoutingKeyAndCount`)
that differ only in which attributes they attach to the span. Each currently
returns `Eff es a` with no `Error` constraint. All six must gain the
constraint; they all funnel through `runSessionIO`.

**The `inSpan'` API.** `OTel.inSpan' :: Tracer -> Text -> SpanArguments ->
(Span -> m a) -> m a` runs a monadic action inside a span bracket. The
continuation runs in whatever `m` the surrounding context uses; in our case
it's `IO` (because we `liftIO` into Effectful). If the continuation throws an
IO exception, `inSpan'` records the exception on the span and rethrows. If
the continuation returns a value, the span closes cleanly. This is why the
current code's error recording "works" — the `fail`'s IO exception reaches
`inSpan'`, which records it. We intentionally shift away from relying on
that.


## Plan of Work

One milestone. The edits are confined to a single file.

### Milestone 1 — Thread `Error PgmqRuntimeError` through every traced helper

**Scope.** Change the six `withTracedSession*` helpers to carry the `Error
PgmqRuntimeError :> es` constraint. Change `runSessionIO` to return `IO
(Either PgmqRuntimeError a)`. In each helper, after `inSpan'` closes, inspect
the Either and `throwError` on Left.

**What will exist at the end:**
- `runPgmqTraced`, `runPgmqTracedWith`, and all six `withTracedSession*`
  helpers carry `(IOE :> es, Error PgmqRuntimeError :> es)` constraints.
- `runSessionIO` no longer calls `fail`; it returns `Either PgmqRuntimeError a`.
- On Left: the span's status is set to `Error`, an exception event is
  recorded (subject to `config.recordExceptions`), and then the caller
  throws `throwError err` at the Eff layer.
- The docstring's example at lines 14-31 mentions `runError @PgmqError`;
  update to `runError @PgmqRuntimeError`.

**Commands to run:**

    cabal build pgmq-effectful
    cabal build all
    nix fmt

**Acceptance criteria:**
- `cabal build pgmq-effectful` exits 0, no warnings.
- Grep for `fail` in the module returns zero matches (`grep -n "fail " pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs`).
- A hand-inspected type signature of `runPgmqTracedWith` shows the new `Error
  PgmqRuntimeError :> es` constraint.


## Concrete Steps

All commands run from the repository root.

### Step 1 — Verify EP-1 is in.

    grep -n "PgmqRuntimeError\|fromUsageError" pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs

Expected output contains at least:

    data PgmqRuntimeError
    instance Exception PgmqRuntimeError
    fromUsageError :: ...

If not, **stop**. EP-1 is a hard dependency.

### Step 2 — Enter dev shell and verify baseline.

    nix develop
    cabal build pgmq-effectful

Must succeed. If not, fix before continuing.

### Step 3 — Edit
`pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs`.

Make the following changes. The file uses `{-# LANGUAGE LambdaCase #-}` via
`default-extensions`, so `\case` is available.

1. Add imports (top of the file, in the existing import block):

       import Effectful.Error.Static (Error, throwError)
       import Pgmq.Effectful.Interpreter
         ( PgmqRuntimeError (..),
           fromUsageError,
         )

   Remove any import that is no longer used (`Hasql.Pool (UsageError)` is
   still referenced by `recordUsageError`'s signature, so keep it).

2. Update `runPgmqTraced` (line ~80) from:

       runPgmqTraced ::
         (IOE :> es) =>
         Pool ->
         OTel.Tracer ->
         Eff (Pgmq : es) a ->
         Eff es a

   to:

       runPgmqTraced ::
         (IOE :> es, Error PgmqRuntimeError :> es) =>
         Pool ->
         OTel.Tracer ->
         Eff (Pgmq : es) a ->
         Eff es a

3. Update `runPgmqTracedWith` (line ~89) analogously.

4. Rewrite `runSessionIO` (lines ~325-340) to:

       runSessionIO ::
         TracingConfig ->
         OTel.Span ->
         Pool ->
         Hasql.Session.Session a ->
         IO (Either PgmqRuntimeError a)
       runSessionIO config s pool session = do
         result <- Pool.use pool session
         case result of
           Left err -> do
             when config.recordExceptions $ recordUsageError s err
             OTel.setStatus s (OTel.Error $ T.pack $ show err)
             pure $ Left $ fromUsageError err
           Right a -> do
             OTel.setStatus s OTel.Ok
             pure $ Right a

5. Update every `withTracedSession*` helper to add the `Error` constraint
   and `throwError` on Left. Pattern to apply to each of the six:

   Before (example, `withTracedSession`):

       withTracedSession ::
         (IOE :> es) =>
         TracingConfig ->
         Text ->
         OTel.SpanKind ->
         QueueName ->
         Pool ->
         Hasql.Session.Session a ->
         Eff es a
       withTracedSession config spanName kind queueName pool session = do
         let args = OTel.defaultSpanArguments {OTel.kind = kind}
         Effectful.liftIO $ OTel.inSpan' config.tracer spanName args $ \s -> do
           addQueueAttributes s queueName
           runSessionIO config s pool session

   After:

       withTracedSession ::
         (IOE :> es, Error PgmqRuntimeError :> es) =>
         TracingConfig ->
         Text ->
         OTel.SpanKind ->
         QueueName ->
         Pool ->
         Hasql.Session.Session a ->
         Eff es a
       withTracedSession config spanName kind queueName pool session = do
         let args = OTel.defaultSpanArguments {OTel.kind = kind}
         result <- Effectful.liftIO $ OTel.inSpan' config.tracer spanName args $ \s -> do
           addQueueAttributes s queueName
           runSessionIO config s pool session
         case result of
           Left err -> throwError err
           Right a -> pure a

   Apply the same shape change to `withTracedSessionNoQueue`,
   `withTracedSessionWithMsgId`, `withTracedSessionWithCount`,
   `withTracedSessionWithRoutingKey`,
   `withTracedSessionWithRoutingKeyAndCount`.

6. Update the docstring example at lines 14-31. Current text:

       -- @
       -- import OpenTelemetry.Trace qualified as OTel
       -- import Pgmq.Effectful.Interpreter.Traced
       --
       -- main :: IO ()
       -- main = do
       --   tracerProvider <- OTel.getGlobalTracerProvider
       --   let tracer = OTel.makeTracer tracerProvider "my-app" OTel.tracerOptions
       --   pool <- ...
       --
       --   runEff
       --     . runError @PgmqError
       --     . runPgmqTraced pool tracer
       --     $ do
       --       createQueue "my-queue"
       --       sendMessage $ SendMessage "my-queue" (MessageBody "hello") Nothing
       -- @

   Change `runError @PgmqError` to `runError @PgmqRuntimeError`.

### Step 4 — Build.

    cabal build pgmq-effectful

Expected: compiles with zero warnings. Common failure modes:

- "Variable not in scope: throwError" — forgot the import
  `Effectful.Error.Static (Error, throwError)`.
- "Couldn't match type 'IO a' with 'IO (Either PgmqRuntimeError a)'" — one
  of the six helpers still treats `runSessionIO`'s result as bare `a`.
  Grep for `runSessionIO` to find all call sites and update each one.
- "The constraint 'Error PgmqRuntimeError :> es' is no smaller than …" —
  add `HasCallStack` or review the constraint syntax; the existing plain
  interpreter's `runPgmq` is the reference shape.

### Step 5 — Grep for any remaining `fail`.

    grep -n "fail " pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs

Expected: no output. `fail` must not appear anywhere in this file after the
plan.

### Step 6 — Full build.

    cabal build all

Must succeed.

### Step 7 — Format.

    nix fmt

### Step 8 — Hand-verify the signature in ghci.

    cabal repl pgmq-effectful
    ghci> :t runPgmqTraced

Expected output:

    runPgmqTraced
      :: (IOE :> es, Error PgmqRuntimeError :> es) =>
         Pool
         -> OTel.Tracer
         -> Eff (Pgmq : es) a
         -> Eff es a

### Step 9 — Commit.

    git add pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs

    git commit -m "$(cat <<'EOF'
    fix(pgmq-effectful): propagate typed errors from traced interpreter

    The traced interpreter previously called `fail` on hasql-pool errors,
    which escaped as an IOError outside the Error effect channel. That made
    runError @PgmqError a no-op for traced programs and stringified the
    error context.

    The traced interpreter now carries an Error PgmqRuntimeError constraint
    and throws the same structured error that the plain runPgmq
    interpreter throws. OpenTelemetry span recording (status + exception
    event, gated by TracingConfig.recordExceptions) still happens before
    the throw, so observability and recoverability are both preserved.

    MasterPlan: docs/masterplans/1-pgmq-effectful-error-propagation.md
    ExecPlan: docs/plans/2-pgmq-effectful-traced-error-propagation.md
    Intention: intention_01kpybay9hegps2fjt7tkwarz6
    EOF
    )"


## Validation and Acceptance

The plan is complete when:

1. `cabal build all` exits 0 with zero warnings.
2. `grep -n "fail " pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs`
   returns no matches.
3. `:t runPgmqTraced` in ghci shows the `Error PgmqRuntimeError :> es`
   constraint.
4. A minimal program wrapping `runPgmqTraced` with `runError
   @PgmqRuntimeError` compiles. (EP-4 turns this into an automated test.)
5. The commit contains all three required trailers.

Full end-to-end runtime acceptance (sending a message, seeing the structured
error, seeing the span attributes) is the province of EP-4's tests.


## Idempotence and Recovery

All edits are confined to one file. If partway through Step 3 the build
breaks, read the diff
(`git diff pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs`) and
confirm:
- All six `withTracedSession*` helpers consistently either all have the new
  constraint and all return via `case result`, or none do. Mixed state is
  what breaks the build.
- `runSessionIO`'s return type is `IO (Either PgmqRuntimeError a)` and its
  two branches return `Left` / `Right` accordingly.

To abandon cleanly:

    git restore pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs


## Interfaces and Dependencies

No new dependencies. The module already depends on `hasql`, `hasql-pool`,
`effectful-core`, `hs-opentelemetry-api`, `text`, `unordered-containers`, and
other packages per `pgmq-effectful/pgmq-effectful.cabal`.

**Signatures that must exist at the end:**

In `Pgmq.Effectful.Interpreter.Traced`:

    runPgmqTraced ::
      (IOE :> es, Error PgmqRuntimeError :> es) =>
      Pool -> OTel.Tracer -> Eff (Pgmq : es) a -> Eff es a

    runPgmqTracedWith ::
      (IOE :> es, Error PgmqRuntimeError :> es) =>
      Pool -> TracingConfig -> Eff (Pgmq : es) a -> Eff es a

    -- (six private withTracedSession* helpers, each with the new constraint)

    runSessionIO ::
      TracingConfig -> OTel.Span -> Pool -> Hasql.Session.Session a ->
      IO (Either PgmqRuntimeError a)

No changes to the Pgmq effect GADT, to the plain interpreter, or to
`Pgmq.Effectful.Telemetry`.
