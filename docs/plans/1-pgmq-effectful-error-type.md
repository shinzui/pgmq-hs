# Define `PgmqRuntimeError` runtime-error type for pgmq-effectful

MasterPlan: docs/masterplans/1-pgmq-effectful-error-propagation.md
Intention: intention_01kpybay9hegps2fjt7tkwarz6

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

This document is maintained in accordance with `.claude/skills/exec-plan/PLANS.md`.


## Purpose / Big Picture

After this plan lands, a user of the `pgmq-effectful` package who runs a pgmq
operation through the plain interpreter (`runPgmq`) and catches the error with
`runError` receives a **structured** error value they can pattern-match on
directly — not an opaque wrapper around `Hasql.Pool.UsageError`.

Concretely, today this code:

    import Pgmq.Effectful (PgmqError (..), runPgmq)

    main :: IO ()
    main = do
      result <- runEff . runError @PgmqError . runPgmq pool $ ...
      case result of
        Left (_cs, PgmqPoolError err) ->
          -- `err` is `Hasql.Pool.UsageError`; user must import hasql-pool internals
          -- and traverse three levels of sum types to see the SQL state
          print err
        Right a -> ...

becomes:

    import Pgmq.Effectful
      (PgmqRuntimeError (..), runPgmq)

    main :: IO ()
    main = do
      result <- runEff . runError @PgmqRuntimeError . runPgmq pool $ ...
      case result of
        Left (_cs, PgmqAcquisitionTimeout) -> ...
        Left (_cs, PgmqConnectionError connErr) -> ...     -- Hasql.ConnectionError
        Left (_cs, PgmqSessionError sessErr) -> ...         -- Hasql.SessionError
        Right a -> ...

The field shapes directly reuse hasql's public error types
(`Hasql.Engine.Errors.ConnectionError`, `Hasql.Engine.Errors.SessionError`), so
the user's subsequent inspection (matching on `ServerStatementError (ServerError
code _ _ _ _)` to recover from SQL state `23505`, for example) uses
hasql's existing, documented types.

**How to see it working after this plan:** after the changes compile, build the
package and in a `ghci` session evaluate:

    cabal repl pgmq-effectful
    ghci> :t PgmqAcquisitionTimeout
    PgmqAcquisitionTimeout :: PgmqRuntimeError
    ghci> :t PgmqSessionError
    PgmqSessionError :: Hasql.Engine.Errors.SessionError -> PgmqRuntimeError

Until EP-2 ships, only `runPgmq` (not `runPgmqTraced`) uses this type;
EP-2 completes the picture.


## Progress

- [x] Read the hasql error-type definitions in
  `/Users/shinzui/Keikaku/hub/haskell/hasql-project/hasql/src/library/Hasql/Engine/Errors.hs`
  to confirm which types to import (`ConnectionError`, `SessionError`).
  (2026-04-23: confirmed; public re-export lives in `Hasql.Errors`, not
  `Hasql.Session` — see Surprises & Discoveries.)
- [x] Add a `build-depends` entry or confirm existing one for hasql (already
  present via `hasql ^>=1.10`) in `pgmq-effectful/pgmq-effectful.cabal`.
  (2026-04-23: existing dependency is sufficient; no cabal change.)
- [x] Design decision: where does `PgmqRuntimeError` live? (see Decision Log)
- [x] Implement `PgmqRuntimeError` data type with constructors:
  `PgmqAcquisitionTimeout`, `PgmqConnectionError HasqlErrors.ConnectionError`,
  `PgmqSessionError HasqlErrors.SessionError`. (2026-04-23)
- [x] Implement `fromUsageError :: Hasql.Pool.UsageError -> PgmqRuntimeError`
  conversion helper. (2026-04-23)
- [x] Update the plain `runPgmq` interpreter in
  `pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs` to use the new type:
  change the constraint from `Error PgmqError :> es` to `Error PgmqRuntimeError
  :> es`, change `throwError $ PgmqPoolError err` to `throwError $
  fromUsageError err`. (2026-04-23)
- [x] Legacy `PgmqError`/`PgmqPoolError` names kept in module, unused by
  `runPgmq`. (EP-3 finalizes deprecation policy.) (2026-04-23)
- [x] Derive `Exception`, `Show`, `Eq`, `Generic` on `PgmqRuntimeError`.
  (2026-04-23)
- [x] Run `cabal build pgmq-effectful` — succeeds with zero warnings under the
  package's `-Wall -Wmissing-export-lists -Wpartial-fields` ghc-options.
  (2026-04-23)
- [x] Update downstream consumer `pgmq-bench/bench/BenchSetup.hs` to use
  `PgmqRuntimeError` (not in EP-1's original scope but required to keep
  `cabal build all` green; see Surprises & Discoveries). (2026-04-23)
- [x] Bump `cabal.project` `hasql-migration` pin to the current master of
  `shinzui/hasql-migration` to restore a working build (old pin fails on a
  `memory`/`crypton` typeclass mismatch; see Surprises & Discoveries).
  (2026-04-23)
- [x] Run `nix fmt` to format. (2026-04-23: reported "formatted 3 files (0
  changed)" — nothing needed reformatting.)
- [x] Verify ghci type signatures per Step 9. (2026-04-23)
- [x] Commit with `MasterPlan:`, `ExecPlan:`, and `Intention:` trailers.


## Surprises & Discoveries

- `Hasql.Session` does **not** re-export `ConnectionError` and `SessionError`.
  The public module for the error types is `Hasql.Errors` (confirmed by
  reading `hub/haskell/hasql-project/hasql/src/library/Hasql/Session.hs`,
  which only exports `Session`, `pipeline`, `script`, `statement`,
  `onLibpqConnection`; the types live in `Hasql.Errors` which re-exports from
  the internal `Hasql.Engine.Errors`). The plan's Step 3 showed
  `import Hasql.Session qualified as Hasql (ConnectionError, SessionError)`;
  that import would fail. The implementation instead uses
  `import Hasql.Errors qualified as HasqlErrors`, matching the plan's
  "Interfaces and Dependencies" fallback note. (2026-04-23)

- `pgmq-bench/bench/BenchSetup.hs` imports `PgmqError (..)` from
  `Pgmq.Effectful.Interpreter` and wraps its benchmark runner with
  `runErrorNoCallStack @PgmqError . runPgmq pool`. After changing `runPgmq`'s
  constraint to `Error PgmqRuntimeError`, that file no longer type-checks:

      bench/BenchSetup.hs:182:55: error: [GHC-64725]
        • There is no handler for 'Error
                                     Pgmq.Effectful.Interpreter.PgmqRuntimeError'
          in the context

  Fix: import `PgmqRuntimeError` instead, and use `@PgmqRuntimeError` in the
  `runErrorNoCallStack` type application. The plan's Step 5 grep scope
  (`grep -rn "PgmqError" pgmq-effectful pgmq-hasql pgmq-migration pgmq-core
  pgmq-config pgmq-bench`) would have caught this; the plan noted "leave it
  alone — EP-3 will sort out deprecation" but in this case leaving it alone
  breaks the build, so the only viable fix is a forward-migration of the
  bench harness. (2026-04-23)

- `cabal build all` initially failed in `hasql-migration-0.3.1` with a
  `memory-0.18.0` / `crypton-1.1.2` `ByteArrayAccess (Digest MD5)` typeclass
  mismatch — unrelated to pgmq-effectful. The project's `cabal.project`
  already pinned `shinzui/hasql-migration` at an older SHA
  (`ab66f6ae93e40065f8532dd9d497ecb15c91122e`); bumping the pin to the
  current master (`4aaff6c0919d1fe8e1c248c3ce4ce05775c59c8c`) restored a
  working build. Recorded here because the pin bump lands in the same commit
  series as EP-1 and a future reader should not mistake it for an EP-1
  concern. (2026-04-23)


## Decision Log

- Decision: The new type lives in `pgmq-effectful`, specifically in
  `pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs` (same module as the
  existing `PgmqError`).
  Rationale: Keeps the error type co-located with the function that throws it.
  An "Internal" module split can happen later if the module grows; right now,
  the interpreter file is 103 lines and adding ~30 lines of type definition +
  helper does not justify a new module. Also avoids churn in `pgmq-effectful`'s
  public `exposed-modules` list in the cabal file, which means EP-3 has less
  to coordinate.
  Date: 2026-04-23

- Decision: Name is `PgmqRuntimeError`. Constructors: `PgmqAcquisitionTimeout`,
  `PgmqConnectionError`, `PgmqSessionError`.
  Rationale: "Runtime" distinguishes it from `pgmq-core`'s validation
  `PgmqError`. Constructors mirror `Hasql.Pool.UsageError`'s own constructors
  (`AcquisitionTimeoutUsageError`, `ConnectionUsageError`, `SessionUsageError`)
  so the mapping is 1-to-1 and obvious. The `Pgmq` prefix on each constructor
  avoids collisions with hasql's own error-type constructor names if a user
  imports both unqualified.
  Date: 2026-04-23

- Decision: Reuse hasql's `ConnectionError` and `SessionError` directly rather
  than defining mirror types in pgmq-effectful.
  Rationale: Hasql already has excellent documentation on these types (each
  constructor has a paragraph explaining causes and whether the error is
  transient). Duplicating would force us to keep two sets of docs in sync,
  lose the direct correspondence with hasql's behavior, and force users to
  learn two vocabularies. The `pgmq-effectful` package already depends on
  `hasql ^>=1.10`, so no new dependencies are added.
  Date: 2026-04-23

- Decision: Derive an `Exception` instance, not only `Show`.
  Rationale: Users of the `Error` effect get typed errors, but users layering
  with `Effectful.Exception` or interop code expecting `MonadThrow` benefit
  from a direct `Exception` instance. `Hasql.Pool.UsageError` already derives
  `Exception` (per
  `/Users/shinzui/Keikaku/hub/haskell/hasql-project/hasql-pool/src/library/exposed/Hasql/Pool.hs:259`),
  so precedent is set.
  Date: 2026-04-23

- Decision: Leave the legacy `PgmqError`/`PgmqPoolError` names in place for
  this plan (but unused in `runPgmq`'s signature). EP-3 decides whether to add
  a `DEPRECATED` pragma, a module-level alias, or remove them outright.
  Rationale: This plan's scope is "get the new type working inside the plain
  interpreter." Policy decisions about public API belong in EP-3.
  Date: 2026-04-23


## Outcomes & Retrospective

Outcome (2026-04-23): `PgmqRuntimeError` is in place with the designed shape
and `runPgmq` throws it via the `Error` effect. All downstream library and
benchmark build targets (`cabal build all`) compile. Legacy `PgmqError`
remains in `Pgmq.Effectful.Interpreter` for one more release cycle; EP-3
owns its retirement.

Gaps vs. plan:

- The plan's Step 3 import sketch (`Hasql.Session qualified as Hasql
  (ConnectionError, SessionError)`) does not compile; used `Hasql.Errors`
  instead per the "Interfaces and Dependencies" fallback note. This is a
  plan-accuracy fix, not a behavioral change.
- The plan's grep scope in Step 5 excluded the possibility that leaving
  `PgmqError` legacy names would break a downstream benchmark; it did, so
  `pgmq-bench/bench/BenchSetup.hs` had to migrate inside this plan.
- Unrelated build-environment fix: `hasql-migration` pin bumped in
  `cabal.project`. Recorded in Surprises & Discoveries; not strictly EP-1
  scope but landed in the same commit series for bisect hygiene.

Lessons for future plans: when a rename renders a legacy name unused but
the legacy name is still exported, downstream files that re-export or
specialize to that name will still typecheck against the old signature
specialization but break at the use site of the new interpreter. Consider
the downstream import graph when marking deprecations.


## Context and Orientation

Assume no prior knowledge of this repository. The reader sees the current
working tree and this plan — nothing else.

**`pgmq-effectful`** is one of four Haskell packages in this multi-package
Cabal project (see the project root's `cabal.project` file and the top-level
`CLAUDE.md`). The four packages are:

- `pgmq-core` (`pgmq-core/pgmq-core.cabal`) — zero-deps types shared across
  backends. Contains `Pgmq.Types.PgmqError`, a validation error sum
  (`InvalidQueueName | InvalidRoutingKey | InvalidTopicPattern`) used by
  `parseQueueName`, `parseRoutingKey`, `parseTopicPattern`. This is the
  `PgmqError` we *keep*, with that name, in that module.
- `pgmq-hasql` (`pgmq-hasql/pgmq-hasql.cabal`) — Hasql sessions that execute
  pgmq SQL. Returns `Session a` values; error handling is whatever `Hasql.Pool.use`
  or `Hasql.Session.run` surface. Not modified by this plan.
- `pgmq-effectful` (`pgmq-effectful/pgmq-effectful.cabal`) — **this plan's
  subject**. Effectful effect + interpreters.
- `pgmq-migration` — unrelated schema migrations.

**"pgmq"** is a Postgres extension
(<https://github.com/tembo-io/pgmq>) that implements a message queue using
SQL functions like `pgmq.create(text)`, `pgmq.send(text, jsonb)`,
`pgmq.read(text, int, int)`, etc.

**"Effectful"** is a Haskell effect system
(<https://hackage.haskell.org/package/effectful>). In this codebase, effects
are declared as GADTs (`data Pgmq :: Effect where CreateQueue :: ... -> Pgmq m
();`) and interpreted by functions like `runPgmq` that turn the effect into
IO via a `Hasql.Pool.Pool`.

**"Hasql"** is a Haskell Postgres client library. `Hasql.Session.Session a`
represents a batch of SQL statements returning `a`. `Hasql.Pool.use :: Pool ->
Session a -> IO (Either UsageError a)` runs the session. `UsageError` is
defined in `/Users/shinzui/Keikaku/hub/haskell/hasql-project/hasql-pool/src/library/exposed/Hasql/Pool.hs:250-259`:

    data UsageError
      = ConnectionUsageError Errors.ConnectionError
      | SessionUsageError Errors.SessionError
      | AcquisitionTimeoutUsageError
      deriving (Show, Eq)
    instance Exception UsageError

The `Errors.ConnectionError` and `Errors.SessionError` types are re-exported
from `Hasql.Session` (and defined in
`/Users/shinzui/Keikaku/hub/haskell/hasql-project/hasql/src/library/Hasql/Engine/Errors.hs:18`
and `:78` respectively). They are rich sum types:

- `ConnectionError`: `NetworkingConnectionError Text |
  AuthenticationConnectionError Text | CompatibilityConnectionError Text |
  OtherConnectionError Text`.
- `SessionError`: `StatementSessionError Int Int Text [Text] Bool
  StatementError | ScriptSessionError Text ServerError |
  ConnectionSessionError Text | MissingTypesSessionError (HashSet (Maybe Text,
  Text)) | DriverSessionError Text`.
- `StatementError`, nested inside, exposes `ServerStatementError ServerError`
  where `ServerError` carries SQL state (`Text`), message, detail, hint, and
  position.

Users who want to retry on SQL state `23505` (unique violation) can do it by
pattern-matching through the new `PgmqRuntimeError`:

    case err of
      PgmqSessionError (StatementSessionError _ _ _ _ _ (ServerStatementError (ServerError "23505" _ _ _ _))) ->
        -- handle unique violation
      _ -> ...

**The file you will edit.** `pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs`
is 103 lines long and exports the plain `runPgmq` interpreter and the existing
`PgmqError` newtype. Its current shape (abbreviated):

    module Pgmq.Effectful.Interpreter (runPgmq, PgmqError (..)) where

    import Effectful.Error.Static (Error, throwError)
    import Hasql.Pool (Pool, UsageError)

    newtype PgmqError = PgmqPoolError UsageError
      deriving stock (Show)

    runPgmq ::
      (IOE :> es, Error PgmqError :> es) =>
      Pool ->
      Eff (Pgmq : es) a ->
      Eff es a
    runPgmq pool = interpret $ \_ -> \case
      CreateQueue q -> runSession pool $ Sessions.createQueue q
      ...

    runSession pool session = do
      result <- Effectful.liftIO $ Pool.use pool session
      case result of
        Left err -> throwError $ PgmqPoolError err
        Right a -> pure a

**The traced interpreter** at `pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs`
is not touched by this plan. EP-2 fixes that one; it currently calls `fail`
inside `runSessionIO` (line 337) and will be rewritten to use the new error
type + `throwError`.

**Public API.** `pgmq-effectful/src/Pgmq/Effectful.hs` re-exports
`PgmqError (..)` at line 7 and imports it from `Pgmq.Effectful.Interpreter` at
line 188. This plan adds the new `PgmqRuntimeError` re-export; EP-3 will clean
up the old one.


## Plan of Work

There is a single milestone. The work is small enough (~50 lines of code)
that subdividing would be artificial.

### Milestone 1 — Introduce `PgmqRuntimeError` and rewire the plain interpreter

**Scope.** Add the new type, the conversion helper, and update `runPgmq`'s
signature and throw site. Preserve the old `PgmqError` / `PgmqPoolError` names
(still exported) so nothing else in the package breaks. Re-export the new
type from `Pgmq.Effectful`.

**What will exist at the end:**
- A new data type `PgmqRuntimeError` with three constructors in
  `Pgmq.Effectful.Interpreter`.
- A new helper `fromUsageError :: Hasql.Pool.UsageError -> PgmqRuntimeError` in
  the same module.
- `runPgmq`'s constraint changed from `Error PgmqError` to `Error
  PgmqRuntimeError`.
- `Pgmq.Effectful`'s export list re-exporting `PgmqRuntimeError (..)`.
- The package builds cleanly (`cabal build pgmq-effectful`) with zero
  warnings and the existing `-Wall -Wmissing-export-lists -Wpartial-fields`
  ghc-options still enforced.

**Commands to run:**

    cabal build pgmq-effectful
    nix fmt
    git status     # verify only the intended files changed

**Acceptance criteria:**
- `cabal build pgmq-effectful` exits 0.
- `cabal build all` also exits 0 (pgmq-hasql, pgmq-migration, pgmq-config
  must still build; they do not import `PgmqError` from pgmq-effectful so they
  are unaffected, but verify).
- `ghci` session confirms the new type exists with the designed shape.


## Concrete Steps

All commands run from the repository root
(`/Users/shinzui/Keikaku/bokuno/libraries/pgmq-hs-project/pgmq-hs`).

### Step 1 — Enter the development shell.

    nix develop

Expected: shell prompt changes, `cabal --version` reports 3.x, `ghc --version`
reports 9.12.2.

### Step 2 — Verify the current state compiles.

    cabal build pgmq-effectful

Expected: build succeeds. If it fails, stop — something is off before this
plan has touched anything, and the plan's diffs will only make diagnosis
harder.

### Step 3 — Edit `pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs`.

Change the module from the current version to the following. Preserve the
`DetachArchive _q -> pure ()` and every other effect handler exactly — only
the type, helper, and throwing site change.

The new top of the module:

    module Pgmq.Effectful.Interpreter
      ( -- * Interpreters
        runPgmq,

        -- * Error Types
        PgmqRuntimeError (..),
        fromUsageError,

        -- * Legacy Error Type (deprecated; remove in a future release)
        PgmqError (..),
      )
    where

    import Control.Exception (Exception)
    import Effectful (Eff, IOE, (:>))
    import Effectful qualified
    import Effectful.Dispatch.Dynamic (interpret)
    import Effectful.Error.Static (Error, throwError)
    import GHC.Generics (Generic)
    import Hasql.Pool (Pool, UsageError)
    import Hasql.Pool qualified as Pool
    import Hasql.Session qualified
    import Hasql.Session qualified as Hasql
      ( ConnectionError,
        SessionError,
      )
    import Pgmq.Effectful.Effect (Pgmq (..))
    import Pgmq.Hasql.Sessions qualified as Sessions

    -- | Structured runtime error for pgmq-effectful operations.
    --
    -- Constructors mirror 'Hasql.Pool.UsageError' so the mapping is direct.
    -- The inner 'Hasql.ConnectionError' and 'Hasql.SessionError' types are
    -- sum types with detailed constructors; see the Hasql documentation for
    -- their full shapes.
    data PgmqRuntimeError
      = -- | Timed out waiting for a connection from the pool.
        PgmqAcquisitionTimeout
      | -- | Failed to establish a connection to PostgreSQL.
        PgmqConnectionError Hasql.ConnectionError
      | -- | Error during session execution (SQL statement, decoding, etc.).
        PgmqSessionError Hasql.SessionError
      deriving stock (Show, Eq, Generic)

    instance Exception PgmqRuntimeError

    -- | Convert hasql-pool's internal 'UsageError' into the pgmq-effectful
    -- 'PgmqRuntimeError'.
    fromUsageError :: UsageError -> PgmqRuntimeError
    fromUsageError = \case
      Pool.AcquisitionTimeoutUsageError -> PgmqAcquisitionTimeout
      Pool.ConnectionUsageError e -> PgmqConnectionError e
      Pool.SessionUsageError e -> PgmqSessionError e

    -- | Legacy error type. Retained for one release cycle to ease migration.
    -- Prefer 'PgmqRuntimeError' for new code.
    newtype PgmqError = PgmqPoolError UsageError
      deriving stock (Show)

And change `runPgmq`'s signature + `runSession`:

    runPgmq ::
      (IOE :> es, Error PgmqRuntimeError :> es) =>
      Pool ->
      Eff (Pgmq : es) a ->
      Eff es a
    runPgmq pool = interpret $ \_ -> \case
      -- ... every case unchanged ...

    runSession ::
      (IOE :> es, Error PgmqRuntimeError :> es) =>
      Pool ->
      Hasql.Session.Session a ->
      Eff es a
    runSession pool session = do
      result <- Effectful.liftIO $ Pool.use pool session
      case result of
        Left err -> throwError $ fromUsageError err
        Right a -> pure a

Note that `Pool.AcquisitionTimeoutUsageError`, `Pool.ConnectionUsageError`,
`Pool.SessionUsageError` require `Hasql.Pool qualified as Pool` (already
imported). Qualified import of the error types themselves
(`Hasql.Session qualified as Hasql`) is used in the type signatures.

### Step 4 — Update `pgmq-effectful/src/Pgmq/Effectful.hs`.

Add `PgmqRuntimeError (..)` and `fromUsageError` to the export list (and its
import), keeping the legacy `PgmqError (..)` export for now:

Locate the current line near line 8:

        runPgmq,
        PgmqError (..),

Change to:

        runPgmq,
        PgmqRuntimeError (..),
        fromUsageError,
        PgmqError (..),

And the import block around line 188:

    import Pgmq.Effectful.Interpreter (PgmqError (..), runPgmq)

becomes:

    import Pgmq.Effectful.Interpreter
      ( PgmqError (..),
        PgmqRuntimeError (..),
        fromUsageError,
        runPgmq,
      )

### Step 5 — Build and verify.

    cabal build pgmq-effectful

Expected output ends with something like:

    Building library for pgmq-effectful-0.1.3.0..
    [compiling ...]
    [1 of N] Compiling Pgmq.Effectful.Interpreter
    [N of N] Compiling Pgmq.Effectful

If the build fails with "orphan instance" complaints about `Exception`, the
instance `instance Exception PgmqRuntimeError` must be in the same module as
the type definition (it is — verify).

If the build fails with an ambiguous-name warning, the legacy `PgmqError`
export is fine (it's a different identifier from `PgmqRuntimeError`), but
double-check nothing imports `PgmqError (..)` from `Pgmq.Effectful.Interpreter`
and expects its `runPgmq` signature to use it:

    grep -rn "PgmqError" pgmq-effectful pgmq-hasql pgmq-migration pgmq-core pgmq-config pgmq-bench

Only hits should be in `pgmq-core/src/Pgmq/Types.hs` (the validation type; no
conflict), `pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs` (the legacy
newtype), and `pgmq-effectful/src/Pgmq/Effectful.hs` (re-export). If any
other file uses the legacy name, leave it alone — EP-3 will sort out
deprecation.

### Step 6 — Build the full project.

    cabal build all

Expected: all four packages build cleanly.

### Step 7 — Run the existing pgmq-hasql and pgmq-migration tests.

They do not depend on pgmq-effectful, but running them confirms the workspace
is healthy:

    cabal test pgmq-hasql
    cabal test pgmq-migration

### Step 8 — Format.

    nix fmt

Expected: exit 0, possibly silent, possibly with "files reformatted: …" —
either way, re-stage.

### Step 9 — Verify the ghci shape.

    cabal repl pgmq-effectful

In the ghci prompt:

    ghci> import Pgmq.Effectful
    ghci> :t PgmqAcquisitionTimeout
    PgmqAcquisitionTimeout :: PgmqRuntimeError
    ghci> :t PgmqConnectionError
    PgmqConnectionError :: Hasql.Engine.Errors.ConnectionError -> PgmqRuntimeError
    ghci> :t PgmqSessionError
    PgmqSessionError :: Hasql.Engine.Errors.SessionError -> PgmqRuntimeError
    ghci> :t fromUsageError
    fromUsageError :: Hasql.Pool.UsageError -> PgmqRuntimeError

If any of these differ, the type hasn't been imported or exposed correctly —
go back to Steps 3 and 4.

### Step 10 — Commit.

    git add pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs \
            pgmq-effectful/src/Pgmq/Effectful.hs

    git commit -m "$(cat <<'EOF'
    feat(pgmq-effectful): add PgmqRuntimeError with full Hasql context

    Introduce PgmqRuntimeError, a structured error type that exposes the
    three distinct hasql-pool failure modes (acquisition timeout,
    connection error, session error) directly instead of behind the opaque
    PgmqPoolError wrapper. The plain runPgmq interpreter now throws
    PgmqRuntimeError via the Error effect; the legacy PgmqError newtype is
    retained (unused by runPgmq) for one release cycle to ease migration.

    MasterPlan: docs/masterplans/1-pgmq-effectful-error-propagation.md
    ExecPlan: docs/plans/1-pgmq-effectful-error-type.md
    Intention: intention_01kpybay9hegps2fjt7tkwarz6
    EOF
    )"

The pre-commit hook may re-format; if it does, `git add` the reformatted
files and re-commit. Do not pass `--no-verify`.


## Validation and Acceptance

The plan is complete when all of the following are true:

1. `cabal build all` exits 0 with no warnings.
2. `cabal test pgmq-hasql pgmq-migration` passes (pgmq-effectful has no tests
   yet; EP-4 adds them).
3. In a `cabal repl pgmq-effectful` session, `:t PgmqRuntimeError`,
   `:t PgmqAcquisitionTimeout`, `:t PgmqConnectionError`,
   `:t PgmqSessionError`, and `:t fromUsageError` produce the signatures
   shown in Step 9.
4. `runPgmq` has the signature `(IOE :> es, Error PgmqRuntimeError :> es) =>
   Pool -> Eff (Pgmq : es) a -> Eff es a`.
5. The `PgmqError` newtype and `PgmqPoolError` constructor still exist and are
   still re-exported — they are simply unused by `runPgmq`. (EP-3 decides
   their fate.)
6. The commit contains the three required trailers.


## Idempotence and Recovery

The edits are additive where possible. If Step 5 fails, re-read the diff in
`git diff pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs` and confirm:
- No `PgmqError` occurrences remain inside `runPgmq`'s constraint or `runSession`'s conversion.
- All three `UsageError` constructors are handled in `fromUsageError` (GHC's
  `-Wincomplete-uni-patterns` will catch a missing case if so).
- Imports are complete: `Hasql.Pool qualified as Pool`, `Hasql.Session
  qualified as Hasql`, `Control.Exception (Exception)`, `GHC.Generics
  (Generic)`.

To abandon the work cleanly: `git restore
pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs
pgmq-effectful/src/Pgmq/Effectful.hs`. No other files are touched.


## Interfaces and Dependencies

No new dependencies. All required modules are already in
`pgmq-effectful/pgmq-effectful.cabal`'s `build-depends` (hasql, hasql-pool,
effectful-core, base).

**Types that must exist at the end of this plan:**

In `Pgmq.Effectful.Interpreter`:

    data PgmqRuntimeError
      = PgmqAcquisitionTimeout
      | PgmqConnectionError Hasql.Session.ConnectionError
      | PgmqSessionError Hasql.Session.SessionError

    instance Exception PgmqRuntimeError
    instance Show PgmqRuntimeError
    instance Eq PgmqRuntimeError
    instance Generic PgmqRuntimeError

    fromUsageError :: Hasql.Pool.UsageError -> PgmqRuntimeError

    runPgmq ::
      (IOE :> es, Error PgmqRuntimeError :> es) =>
      Pool ->
      Eff (Pgmq : es) a ->
      Eff es a

Re-exported by `Pgmq.Effectful` with `PgmqRuntimeError (..)` so constructors
are visible.

**Note on the `Hasql.Session` qualifier.** Hasql re-exports the error types
from `Hasql.Session`; importing from there (rather than the internal
`Hasql.Engine.Errors`) uses the library's intended public surface. Verify:

    grep -n "ConnectionError\|SessionError" /Users/shinzui/Keikaku/hub/haskell/hasql-project/hasql/src/library/Hasql/Session.hs

should show re-exports. If not, fall back to `Hasql.Errors` or whatever
public module does export them; do **not** import `Hasql.Engine.Errors`
directly as that is internal.
