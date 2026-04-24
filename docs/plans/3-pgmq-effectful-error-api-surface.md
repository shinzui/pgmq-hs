# Curate the pgmq-effectful error API surface

MasterPlan: docs/masterplans/1-pgmq-effectful-error-propagation.md
Intention: intention_01kpybay9hegps2fjt7tkwarz6

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

This document is maintained in accordance with `.claude/skills/exec-plan/PLANS.md`.


## Purpose / Big Picture

After this plan lands, everything a user needs to handle pgmq errors is
available from a single import (`import Pgmq.Effectful`):

- The structured error type `PgmqRuntimeError` with all constructors.
- The conversion helper `fromUsageError` (for interop with code using raw
  `Hasql.Pool.UsageError`).
- At least one classification helper — `isTransient` — so users can decide
  retry policy without duplicating hasql-specific knowledge in each
  application.
- A clean deprecation path for the old opaque `PgmqError` / `PgmqPoolError`
  names. After one release cycle, they go away entirely.

Also after this plan, the public module list is consistent: `Pgmq.Effectful`
is the one-stop import, and no user code should need to import
`Pgmq.Effectful.Interpreter` directly for the error type.

**How to see it working:** `cabal haddock pgmq-effectful` generates
documentation where:

- `Pgmq.Effectful` lists `PgmqRuntimeError (..)`, `fromUsageError`,
  `isTransient` under an "Errors" section.
- `PgmqError` and `PgmqPoolError` still appear but carry a `Deprecated`
  badge with guidance to migrate.

And in a usage example:

    import Pgmq.Effectful
      ( PgmqRuntimeError (..),
        isTransient,
        runPgmq,
      )

    handlePgmqError :: PgmqRuntimeError -> IO ()
    handlePgmqError err
      | isTransient err = putStrLn "Transient, will retry"
      | otherwise       = putStrLn $ "Permanent: " <> show err


## Progress

- [ ] Verify EP-1 and EP-2 have landed (read the interpreter and traced
  interpreter files). If not, stop.
- [ ] Add `isTransient :: PgmqRuntimeError -> Bool` to
  `Pgmq.Effectful.Interpreter`, with a precise rule derived from hasql's
  error documentation (see Decision Log and the Context section).
- [ ] Decide deprecation policy for `PgmqError`/`PgmqPoolError` (Decision Log
  entry); apply `{-# DEPRECATED #-}` pragmas.
- [ ] Curate `Pgmq.Effectful`'s export list: add a visible "Errors" section
  comment; move the new exports; deprecate-annotate the legacy ones.
- [ ] `cabal build pgmq-effectful` passes with no unexpected warnings.
  (Deprecation warnings are expected if anything internal still uses the
  legacy names; fix those uses to use the new names.)
- [ ] `cabal haddock pgmq-effectful` renders successfully; inspect the
  generated HTML under `dist-newstyle/build/*/pgmq-effectful-*/doc/html/`
  to confirm the errors section reads well.
- [ ] `nix fmt` passes.
- [ ] Commit with the three required trailers.


## Surprises & Discoveries

(None yet.)


## Decision Log

- Decision: Keep `PgmqError` and `PgmqPoolError` exported, but mark both
  with `{-# DEPRECATED #-}` pragmas pointing users at
  `PgmqRuntimeError`/`fromUsageError`. Remove entirely in the next
  major-version bump (planned for pgmq-effectful 0.3.0).
  Rationale: Removing immediately would force every existing user to update
  imports in lock-step with this release. Deprecation gives one release cycle
  of warning. The `{-# DEPRECATED #-}` pragma emits a compile-time warning
  pointing at the new name, which is the right level of nudge.
  Date: 2026-04-23

- Decision: Ship exactly one classification helper, `isTransient`, in this
  plan. Do not ship `isUniqueViolation`, `isDuplicateObject`, retryable-by-
  SQL-state helpers, etc.
  Rationale: `isTransient` covers the single biggest use case (retry policy
  for a background worker). More specific helpers are easy to write once
  users hit a specific need, and premature abstraction there would lock us
  into a classification scheme we might regret. The helper's source doubles
  as an example for users who need custom classification.
  Date: 2026-04-23

- Decision: `isTransient` returns `True` for:
  - `PgmqAcquisitionTimeout` — pool contention is transient by nature.
  - `PgmqConnectionError (NetworkingConnectionError _)` — network blips.
  - `PgmqConnectionError (OtherConnectionError _)` — hasql documents this as
    "typically non-transient" but it is the catch-all for unrecognized libpq
    errors; classing it as transient errs on the side of letting retries
    happen for unfamiliar errors. Document this in the haddock.
  - `PgmqSessionError (ConnectionSessionError _)` — connection dropped
    mid-session; hasql documents these as transient.
  And `False` for all other constructors (authentication failure, missing
  types, statement errors, driver bugs, compatibility issues).
  Rationale: This mirrors the annotations in hasql's own error-type
  haddocks (see `Hasql.Engine.Errors.ConnectionError` constructors —
  `NetworkingConnectionError` is annotated "transient and the operation can
  be retried," `AuthenticationConnectionError` is annotated "not transient",
  etc.). We encode hasql's published intent; if hasql changes its mind, we
  update accordingly.
  Date: 2026-04-23

- Decision: Do not export `PgmqError` / `PgmqPoolError` from any submodule;
  only `Pgmq.Effectful` (the top-level module) re-exports them with the
  deprecation annotation. This avoids users importing
  `Pgmq.Effectful.Interpreter (PgmqError (..))` and not seeing the warning.
  Rationale: `{-# DEPRECATED #-}` emits on *use* of the name, but if a user
  imports from an internal module, the warning still fires (the pragma
  attaches to the identifier, not the module). This is belt-and-braces —
  prefer a single public entry point.
  Date: 2026-04-23


## Outcomes & Retrospective

(To be filled during and after implementation.)


## Context and Orientation

**EP-1 and EP-2 are prerequisites.** This plan assumes:
- `Pgmq.Effectful.Interpreter` exports `PgmqRuntimeError (..)`, `fromUsageError`,
  and the legacy `PgmqError (..)`.
- `Pgmq.Effectful.Interpreter.Traced`'s `runPgmqTraced` and
  `runPgmqTracedWith` carry the `Error PgmqRuntimeError :> es` constraint.
- The plain `runPgmq` also carries `Error PgmqRuntimeError :> es`.

If any of these is false, stop and complete the prerequisite plan first.

**Hasql's transience annotations.** The file
`/Users/shinzui/Keikaku/hub/haskell/hasql-project/hasql/src/library/Hasql/Engine/Errors.hs`
defines error types with haddocks that explicitly call out transience:

- `NetworkingConnectionError` — "These errors are transient and the operation
  can be retried." (line 28)
- `AuthenticationConnectionError` — "These errors are not transient and
  require fixing the credentials or permissions." (line 40)
- `CompatibilityConnectionError` — "These errors are not transient and
  require upgrading/downgrading the server or client." (line 52)
- `OtherConnectionError` — "These errors are not transient by default." (line
  62) — our `isTransient` overrides this to `True` for the reason in the
  Decision Log.
- `StatementSessionError` — not annotated; statement errors are typically
  caused by bad SQL or bad data, not retryable.
- `ConnectionSessionError` — "These errors are transient and the operation
  can be retried with a new connection." (line 117)
- `MissingTypesSessionError` — schema mismatch; not transient.
- `DriverSessionError` — "a bug in Hasql or the PostgreSQL server
  misbehaving"; not retryable.

`isTransient`'s rule is a direct encoding of these annotations.

**`{-# DEPRECATED #-}` semantics in GHC.** A pragma of the form:

    {-# DEPRECATED PgmqError "Use PgmqRuntimeError instead (available from Pgmq.Effectful)." #-}

attached at module scope emits a warning every time a user mentions
`PgmqError` outside its defining module (or outside a binding that uses the
same identifier). The warning is user-visible but does not break the build.
Users can silence with `{-# OPTIONS_GHC -Wno-deprecations #-}` if they know
what they are doing.

**The `Pgmq.Effectful` export list.** The current top-level re-export (EP-1's
final state) looks like:

    module Pgmq.Effectful
      ( -- * Effect
        Pgmq,

        -- * Interpreters
        runPgmq,
        PgmqRuntimeError (..),
        fromUsageError,
        PgmqError (..),

        -- ** Traced Interpreters
        runPgmqTraced,
        runPgmqTracedWith,
        TracingConfig (..),
        defaultTracingConfig,
        ...

This plan refactors the "Interpreters" section to split out errors:

    module Pgmq.Effectful
      ( -- * Effect
        Pgmq,

        -- * Interpreters
        runPgmq,

        -- ** Traced Interpreters
        runPgmqTraced,
        runPgmqTracedWith,
        TracingConfig (..),
        defaultTracingConfig,

        -- * Errors
        PgmqRuntimeError (..),
        fromUsageError,
        isTransient,

        -- ** Deprecated Error Types
        PgmqError (..),
        ...


## Plan of Work

One milestone.

### Milestone 1 — Classification helper, deprecation pragmas, export curation

**Scope.**
1. Add `isTransient` to `Pgmq.Effectful.Interpreter`.
2. Apply `{-# DEPRECATED #-}` pragmas to `PgmqError` and `PgmqPoolError`.
3. Reshape the `Pgmq.Effectful` export list per the Context section.

**What will exist at the end:**
- `isTransient :: PgmqRuntimeError -> Bool` with the semantics described in
  the Decision Log.
- `PgmqError` and `PgmqPoolError` emit GHC deprecation warnings when used.
- `Pgmq.Effectful` has a dedicated "Errors" section in its export list and
  a "Deprecated Error Types" sub-section.

**Commands to run:**

    cabal build pgmq-effectful
    cabal haddock pgmq-effectful
    nix fmt

**Acceptance criteria:**
- Build + haddock succeed.
- `grep -rn "PgmqError\|PgmqPoolError" pgmq-effectful/src` shows them only in
  `Pgmq.Effectful.Interpreter` (definition + pragma) and `Pgmq.Effectful`
  (re-export). No internal use; the interpreter code itself uses
  `PgmqRuntimeError`.
- A minimal test: write `foo :: PgmqError; foo = undefined` in ghci —
  expect a deprecation warning.


## Concrete Steps

From the repository root.

### Step 1 — Confirm prerequisites.

    grep -n "PgmqRuntimeError\|fromUsageError" pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs
    grep -n "Error PgmqRuntimeError" pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs

Both must show matches. If not, EP-1 or EP-2 is incomplete.

### Step 2 — Enter dev shell.

    nix develop

### Step 3 — Edit
`pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs`.

Add the classification helper. Place it immediately after `fromUsageError`:

    -- | Is this error plausibly transient — i.e., worth retrying?
    --
    -- Returns 'True' for acquisition timeouts, networking connection errors,
    -- uncategorized libpq connection errors, and session-level connection
    -- drops. All other errors (authentication failure, compatibility
    -- mismatches, missing types, statement errors, driver bugs) are
    -- treated as permanent.
    --
    -- Note: 'OtherConnectionError' is classed as transient here despite
    -- hasql's documentation calling it "not transient by default", because it
    -- is the catch-all for unrecognized libpq errors and classing the
    -- unknown as transient errs toward letting retries happen.
    isTransient :: PgmqRuntimeError -> Bool
    isTransient = \case
      PgmqAcquisitionTimeout -> True
      PgmqConnectionError e -> case e of
        Hasql.NetworkingConnectionError _ -> True
        Hasql.AuthenticationConnectionError _ -> False
        Hasql.CompatibilityConnectionError _ -> False
        Hasql.OtherConnectionError _ -> True
      PgmqSessionError e -> case e of
        Hasql.ConnectionSessionError _ -> True
        Hasql.StatementSessionError {} -> False
        Hasql.ScriptSessionError {} -> False
        Hasql.MissingTypesSessionError _ -> False
        Hasql.DriverSessionError _ -> False

This requires importing the constructors for pattern-matching. Update the
import:

    import Hasql.Session qualified as Hasql
      ( ConnectionError (..),
        SessionError (..),
      )

Add the deprecation pragmas immediately after each legacy identifier's
definition:

    -- | Legacy error type. Retained for one release cycle; migrate to
    -- 'PgmqRuntimeError'.
    newtype PgmqError = PgmqPoolError UsageError
      deriving stock (Show)
    {-# DEPRECATED PgmqError, PgmqPoolError "Use PgmqRuntimeError instead (available from Pgmq.Effectful). The legacy types will be removed in pgmq-effectful 0.3.0." #-}

Update the export list in the `module Pgmq.Effectful.Interpreter` head:

    module Pgmq.Effectful.Interpreter
      ( -- * Interpreters
        runPgmq,

        -- * Error Types
        PgmqRuntimeError (..),
        fromUsageError,
        isTransient,

        -- * Legacy Error Types (deprecated; will be removed in 0.3.0)
        PgmqError (..),
      )
    where

### Step 4 — Edit `pgmq-effectful/src/Pgmq/Effectful.hs`.

Reshape the top-level export list per the Context section:

    module Pgmq.Effectful
      ( -- * Effect
        Pgmq,

        -- * Interpreters
        runPgmq,

        -- ** Traced Interpreters
        runPgmqTraced,
        runPgmqTracedWith,
        TracingConfig (..),
        defaultTracingConfig,

        -- * Errors
        PgmqRuntimeError (..),
        fromUsageError,
        isTransient,

        -- ** Deprecated Error Types
        PgmqError (..),

        -- ** Traced Operations
        sendMessageTraced,
        readMessageWithContext,
        MessageWithContext,

        -- (... rest unchanged ...)

Update the `import Pgmq.Effectful.Interpreter` block to pull `isTransient`
too:

    import Pgmq.Effectful.Interpreter
      ( PgmqError (..),
        PgmqRuntimeError (..),
        fromUsageError,
        isTransient,
        runPgmq,
      )

### Step 5 — Confirm no internal module uses the deprecated names.

    grep -rn "PgmqPoolError\|PgmqError" pgmq-effectful/src

Expected matches:
- `pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs` — definitions and
  pragma.
- `pgmq-effectful/src/Pgmq/Effectful.hs` — re-exports.

No other hits. If there are, fix them to use `PgmqRuntimeError`. (EP-1 and
EP-2 should have already done this; any remaining use is a bug.)

### Step 6 — Build.

    cabal build pgmq-effectful

Expected: zero warnings. If a deprecation warning fires, locate the user of
the deprecated name and migrate it.

### Step 7 — Haddock.

    cabal haddock pgmq-effectful

Expected: succeeds. Open the generated HTML and confirm:

    open dist-newstyle/build/*/ghc-*/pgmq-effectful-0.1.3.0/doc/html/pgmq-effectful/Pgmq-Effectful.html

In the rendered page, the "Errors" section lists `PgmqRuntimeError`,
`fromUsageError`, `isTransient`. The "Deprecated Error Types" section lists
`PgmqError` with a deprecation note.

### Step 8 — Test the deprecation warning manually.

    cabal repl pgmq-effectful
    ghci> :set -Wdeprecations
    ghci> import Pgmq.Effectful
    ghci> let _ = undefined :: PgmqError

Expected output includes:

    <interactive>:X:Y: warning: [-Wdeprecations]
        In the use of ‘PgmqError’ (imported from Pgmq.Effectful):
        Deprecated: "Use PgmqRuntimeError instead (available from Pgmq.Effectful). The legacy types will be removed in pgmq-effectful 0.3.0."

### Step 9 — Format and commit.

    nix fmt

    git add pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs \
            pgmq-effectful/src/Pgmq/Effectful.hs

    git commit -m "$(cat <<'EOF'
    refactor(pgmq-effectful): curate the error API and deprecate legacy types

    Add isTransient classification helper derived from hasql's own transience
    annotations. Apply DEPRECATED pragmas to PgmqError/PgmqPoolError pointing
    users at PgmqRuntimeError; the legacy types will be removed in 0.3.0.
    Reorganize the public export list so Pgmq.Effectful is a one-stop import
    for the full error handling surface.

    MasterPlan: docs/masterplans/1-pgmq-effectful-error-propagation.md
    ExecPlan: docs/plans/3-pgmq-effectful-error-api-surface.md
    Intention: intention_01kpybay9hegps2fjt7tkwarz6
    EOF
    )"


## Validation and Acceptance

Complete when:

1. `cabal build pgmq-effectful` and `cabal build all` exit 0 with no
   warnings.
2. `cabal haddock pgmq-effectful` exits 0.
3. In ghci, `let _ = undefined :: PgmqError` emits the expected deprecation
   warning.
4. `:t isTransient` in ghci reports `isTransient :: PgmqRuntimeError -> Bool`.
5. `grep -rn "PgmqPoolError\|PgmqError" pgmq-effectful/src` produces only the
   two expected files (Interpreter.hs for definition + pragma;
   Effectful.hs for re-export).
6. Commit contains the three required trailers.


## Idempotence and Recovery

Edits touch two files. To abandon cleanly:

    git restore pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs \
                pgmq-effectful/src/Pgmq/Effectful.hs

If Step 6's build fails with a deprecation warning (treated as error by
some `-Werror` policy), check that the deprecation pragma's target names
are exact — `PgmqError` and `PgmqPoolError` — and that the module scoping
is correct (pragma attaches at the type/constructor name, not the module).


## Interfaces and Dependencies

No new dependencies.

**Signatures that must exist at the end:**

In `Pgmq.Effectful.Interpreter` (and re-exported by `Pgmq.Effectful`):

    isTransient :: PgmqRuntimeError -> Bool

`PgmqError`/`PgmqPoolError` continue to exist, but each carries a
`{-# DEPRECATED #-}` pragma.
