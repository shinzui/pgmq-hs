# Document the pgmq-effectful error model and migration path

MasterPlan: docs/masterplans/1-pgmq-effectful-error-propagation.md
Intention: intention_01kpybay9hegps2fjt7tkwarz6

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

This document is maintained in accordance with `.claude/skills/exec-plan/PLANS.md`.


## Purpose / Big Picture

After this plan lands, a user upgrading from pgmq-effectful 0.1.x has a
single, clear source of guidance:

- The repository `README.md` explains what `PgmqRuntimeError` is, shows a
  short end-to-end example of handling it, and points at a retry pattern
  using `isTransient`.
- `pgmq-effectful/CHANGELOG.md` has a 0.2.0 entry (or whatever the next
  version is — decided during this plan) describing:
  - the breaking change in `runPgmq`'s `Error` constraint;
  - the **non-breaking** addition of `Error PgmqRuntimeError` to
    `runPgmqTraced`/`runPgmqTracedWith` (non-breaking because previously
    the constraint was absent; anyone using `runError @PgmqError` was not
    catching anything anyway — see the incident described in the MasterPlan);
  - the deprecation of `PgmqError`/`PgmqPoolError`;
  - a concrete before/after migration diff.
- A design doc `docs/design/013-pgmq-effectful-error-model.md` records the
  rationale, alternatives considered, and the link to the MasterPlan, so
  future contributors understand why the package settled on this shape.

**How to see it working:**

    cat pgmq-effectful/CHANGELOG.md              # shows the new entry
    cat docs/design/013-pgmq-effectful-error-model.md
    grep -A 30 "error handling" README.md


## Progress

- [x] Verify EP-3 has landed. (2026-04-23)
- [x] Next version is 0.2.0.0 (PVP breaking bump). (2026-04-23)
- [x] Bump `pgmq-effectful/pgmq-effectful.cabal` to 0.2.0.0. (2026-04-23)
- [x] Write the 0.2.0.0 entry in `pgmq-effectful/CHANGELOG.md`.
  (2026-04-23)
- [x] Add an "Error handling" subsection under "## pgmq-effectful" in
  `README.md`. (2026-04-23)
- [x] Write `docs/design/013-pgmq-effectful-error-model.md`.
  (2026-04-23)
- [x] Update `pgmq-config/pgmq-config.cabal`'s
  `pgmq-effectful >=0.1.3 && <0.2` to `>=0.2 && <0.3`. `pgmq-bench.cabal`
  had no upper bound on `pgmq-effectful`, so no change needed.
  (2026-04-23)
- [x] `cabal build all` succeeds after the version bump. (2026-04-23)
- [x] `nix fmt` clean. (2026-04-23)
- [x] Commit with the three required trailers.


## Surprises & Discoveries

- The plan's CHANGELOG template used `Hasql.ConnectionError` /
  `Hasql.SessionError` as the qualified type names. Throughout the
  implementation we import from `Hasql.Errors qualified as HasqlErrors`
  (see EP-1 Surprises). The CHANGELOG entry uses
  `Hasql.Errors.ConnectionError` / `Hasql.Errors.SessionError` so the
  entry is directly copy-paste-able into a user's imports. (2026-04-23)

- Only one workspace cabal file needed an upper-bound bump:
  `pgmq-config/pgmq-config.cabal` pinned `pgmq-effectful >=0.1.3 && <0.2`.
  `pgmq-bench/pgmq-bench.cabal` lists `pgmq-effectful` with no version
  bound, so it picks up 0.2.0.0 automatically. (2026-04-23)


## Decision Log

- Decision: Bump `pgmq-effectful` version to 0.2.0 (minor bump following the
  PVP's "breaking change in public API" rule).
  Rationale: Per PVP, renaming a public type (`PgmqError` →
  `PgmqRuntimeError`) and changing a function's effect-row constraint are
  breaking — even if a deprecation alias is retained. 0.1.3.0 → 0.2.0.0
  signals this cleanly. Dependent packages in this workspace (`pgmq-config`
  if it depends on pgmq-effectful, and `pgmq-bench` if it does) need their
  upper bound updated in the same commit.
  Date: 2026-04-23

- Decision: Do not rename `pgmq-core`'s `PgmqError` in this or any child
  plan.
  Rationale: The name collision is resolved by renaming pgmq-effectful's
  error type, not pgmq-core's. `pgmq-core.PgmqError` is the stable public
  validation error type used by `parseQueueName` / `parseRoutingKey` /
  `parseTopicPattern`; changing its name would cascade to every consumer of
  those parsers. Documented here so future readers don't wonder.
  Date: 2026-04-23

- Decision: Keep the README example short (≤ 15 lines) and point at the
  design doc for the full rationale.
  Rationale: The README is a quick-start; readers who want depth click
  through. Long examples in READMEs go stale the fastest.
  Date: 2026-04-23

- Decision: The design doc is `013-pgmq-effectful-error-model.md`.
  Rationale: `docs/design/` is numbered sequentially; the current highest is
  `012-vendor-upstream-pgmq-sql.md`. Next is 013.
  Date: 2026-04-23


## Outcomes & Retrospective

Outcome (2026-04-23): a user upgrading from pgmq-effectful 0.1.x now has
three coherent sources of guidance:

- `pgmq-effectful/CHANGELOG.md` with a 0.2.0.0 entry describing the
  breaking changes (type rename + tightened traced-interpreter
  constraint), the new features (`fromUsageError`, `isTransient`, the
  test suite), and a before/after migration code block.
- `README.md`'s pgmq-effectful section has a short "Error handling"
  subsection with a runnable example using `runError
  @PgmqRuntimeError` and `isTransient`.
- `docs/design/013-pgmq-effectful-error-model.md` records the design
  rationale and the alternatives considered.

`cabal build all` succeeds with the 0.2.0.0 bump and the updated
`pgmq-config` upper bound.

Gaps vs. plan: none material. The only deviations from the plan's
concrete-step drafts are cosmetic — using `Hasql.Errors.ConnectionError`
rather than the plan's abbreviated `Hasql.ConnectionError` in CHANGELOG
prose, to match the package's actual import convention.


## Context and Orientation

**Prerequisites.** EP-3 has landed. `isTransient` and `fromUsageError` are
exported from `Pgmq.Effectful`. `PgmqRuntimeError` has the final shape:

    data PgmqRuntimeError
      = PgmqAcquisitionTimeout
      | PgmqConnectionError Hasql.Session.ConnectionError
      | PgmqSessionError Hasql.Session.SessionError

**The existing CHANGELOG entries** at `pgmq-effectful/CHANGELOG.md`:

    # Revision history for pgmq-effectful

    ## 0.1.3.0 -- 2026-03-12

    ### Other Changes

    * Update repository homepage URL to shinzui/pgmq-hs

    ## 0.1.2.0 -- 2026-03-03

    * Version bump only (no changes)

    ## 0.1.1.0 -- 2026-02-23
    ...

The new entry goes at the top, following the same two-dash-date format.

**The existing README structure.** The pgmq-effectful section currently says
only:

    ## pgmq-effectful

    The `pgmq-effectful` package provides an [Effectful](https://hackage.haskell.org/package/effectful)
    effect layer over `pgmq-hasql`. It includes traced interpreters with
    OpenTelemetry support for distributed tracing across message producers
    and consumers.

This plan extends it with an "Error handling" paragraph + small example.

**Existing design docs** at `docs/design/`:

    001-message-headers.md
    002-queue-visible-length.md
    003-conditional-read.md
    004-pop-quantity.md
    005-batch-set-vt.md
    006-queue-notifications.md
    007-deprecate-detach-archive.md
    008-fifo-read.md
    009-send-delay-null-handling.md
    010-read-conditional-null-handling.md
    011-pgmq-1.11.0-upgrade.md
    012-vendor-upstream-pgmq-sql.md

They are short prose documents (typically 1-3 pages) that describe a design
decision with rationale. This plan adds `013-pgmq-effectful-error-model.md`
in the same style.

**Version-bound cascades.** Other packages in this workspace may depend on
`pgmq-effectful` with an upper bound like `<0.2`. Check:

    grep -rn "pgmq-effectful" . --include="*.cabal"

If any package pins `pgmq-effectful >=0.1.x && <0.2`, update to `<0.3` as
part of this plan's commit. Dependent packages in this workspace at the time
of writing (from `mori.dhall` survey): pgmq-bench, pgmq-config (if either
depends).


## Plan of Work

Three small milestones.

### Milestone 1 — Version bump + CHANGELOG entry

**Scope.** Bump `pgmq-effectful.cabal` from `0.1.3.0` to `0.2.0.0`. Write a
CHANGELOG entry at the top of `pgmq-effectful/CHANGELOG.md`. Update any
workspace package that pins an upper bound of `<0.2`.

**What will exist at the end:** a coherent versioned package that builds.

**Commands:**

    cabal build all

**Acceptance:** build passes.

### Milestone 2 — README update

**Scope.** Add an "Error handling" subsection under the existing
`## pgmq-effectful` heading in `README.md`.

**Commands:**

    head -80 README.md    # eyeball the change

**Acceptance:** the new subsection is present, renders sensibly as
Markdown, and contains a minimal working example with `runError
@PgmqRuntimeError`.

### Milestone 3 — Design doc

**Scope.** Write `docs/design/013-pgmq-effectful-error-model.md` explaining
what the error types are, why they are shaped this way, and how the pieces
fit across EP-1..EP-5. Link back to the MasterPlan.

**Commands:**

    ls docs/design/

**Acceptance:** `013-pgmq-effectful-error-model.md` exists and is
internally consistent with the other numbered docs.


## Concrete Steps

From the repository root.

### Step 1 — Verify prerequisites.

    grep -n "isTransient\|fromUsageError\|PgmqRuntimeError" pgmq-effectful/src/Pgmq/Effectful.hs

All three names must be exported.

### Step 2 — Enter dev shell.

    nix develop

### Step 3 — Bump the version.

Edit `pgmq-effectful/pgmq-effectful.cabal`:

Change line `version:         0.1.3.0` to `version:         0.2.0.0`.

### Step 4 — Check dependency upper bounds.

    grep -rn "pgmq-effectful" . --include="*.cabal"

If any package pins `<0.2`, update to `<0.3`. Re-run after each edit to
verify.

### Step 5 — Write the CHANGELOG entry.

Prepend to `pgmq-effectful/CHANGELOG.md` (replacing the top of file):

    # Revision history for pgmq-effectful

    ## 0.2.0.0 -- 2026-04-23

    ### Breaking Changes

    * Renamed the interpreter error type from `PgmqError` to
      `PgmqRuntimeError` and replaced its opaque `PgmqPoolError UsageError`
      constructor with three structured constructors:

          data PgmqRuntimeError
            = PgmqAcquisitionTimeout
            | PgmqConnectionError Hasql.ConnectionError
            | PgmqSessionError Hasql.SessionError

      The old `PgmqError`/`PgmqPoolError` names are retained with a
      DEPRECATED pragma and will be removed in 0.3.0.0.

    * `runPgmq`'s error constraint changed from `Error PgmqError :> es` to
      `Error PgmqRuntimeError :> es`. Adjust type annotations on
      `runError @PgmqError` to `runError @PgmqRuntimeError`.

    * `runPgmqTraced` and `runPgmqTracedWith` now require `Error
      PgmqRuntimeError :> es`. Previously they had *no* error constraint
      and threw a `fail`-derived `IOError` outside the Error effect, which
      meant any `runError` wrapper was a no-op. Code that was relying on
      that silent swallowing now receives typed errors; update call sites
      to wrap with `runError @PgmqRuntimeError`.

    ### New Features

    * `fromUsageError :: Hasql.Pool.UsageError -> PgmqRuntimeError` — convert
      raw hasql-pool errors into the pgmq-effectful error type. Useful when
      layering `pgmq-effectful` over code that already calls `Pool.use`
      directly.

    * `isTransient :: PgmqRuntimeError -> Bool` — classification helper for
      retry logic. Returns True for acquisition timeouts, networking
      connection errors, unrecognized libpq connection errors, and
      session-level connection drops; False for authentication,
      compatibility, missing-types, statement, script, and driver errors.

    ### Migration Guide

    Before:

        import Pgmq.Effectful (PgmqError (..), runPgmq)

        handler = runEff . runError @PgmqError . runPgmq pool $ ...

    After:

        import Pgmq.Effectful (PgmqRuntimeError (..), runPgmq)

        handler = runEff . runError @PgmqRuntimeError . runPgmq pool $ ...

    For retry logic:

        import Pgmq.Effectful (isTransient)

        retryIfTransient action = do
          result <- runError @PgmqRuntimeError action
          case result of
            Right a -> pure (Right a)
            Left (_, err) | isTransient err -> retryIfTransient action
                          | otherwise       -> pure (Left err)

    ## 0.1.3.0 -- 2026-03-12

    ### Other Changes

    * Update repository homepage URL to shinzui/pgmq-hs

    ## 0.1.2.0 -- 2026-03-03
    ...

(Keep the rest of the existing file contents intact; only prepend the new
entry.)

### Step 6 — Update `README.md`.

Locate the `## pgmq-effectful` heading and append a new subsection
immediately after the existing paragraph:

    ### Error handling

    Both interpreters (`runPgmq` and `runPgmqTraced`) surface failures
    through the `Error` effect as a `PgmqRuntimeError`:

        import Effectful (runEff)
        import Effectful.Error.Static (runError)
        import Pgmq.Effectful

        main = do
          pool <- ...
          result <- runEff . runError @PgmqRuntimeError . runPgmq pool $ do
            createQueue myQueue
            sendMessage $ SendMessage myQueue body Nothing
          case result of
            Right msgId                       -> print msgId
            Left (_cs, err) | isTransient err -> retry
                            | otherwise       -> logFatal err

    `PgmqRuntimeError` has three constructors — `PgmqAcquisitionTimeout`,
    `PgmqConnectionError`, `PgmqSessionError` — each exposing the full
    hasql context (SQL state, connection-error kind, etc.) so you can
    pattern-match on whatever granularity your application needs. See
    `docs/design/013-pgmq-effectful-error-model.md` for the design rationale.

### Step 7 — Write the design doc.

Create `docs/design/013-pgmq-effectful-error-model.md`:

    # 013 — pgmq-effectful Error Model

    Status: accepted
    Date: 2026-04-23
    See also: docs/masterplans/1-pgmq-effectful-error-propagation.md

    ## Context

    Prior to 0.2.0.0, `pgmq-effectful` exposed a single opaque error
    constructor `PgmqPoolError UsageError`. The traced interpreter made
    things worse: it called `fail` on errors, turning them into string-
    ified IOExceptions that escaped the Error effect entirely. Callers
    who wrote `runError @PgmqError` around a traced program caught
    nothing, because `fail`'s exception does not propagate through the
    Error effect channel.

    This document records the decisions made while fixing those issues.

    ## Design

    The interpreter error type is now:

        data PgmqRuntimeError
          = PgmqAcquisitionTimeout
          | PgmqConnectionError Hasql.ConnectionError
          | PgmqSessionError Hasql.SessionError

    The three constructors mirror `Hasql.Pool.UsageError` 1-to-1, so the
    conversion (`fromUsageError`) is total and obvious. Reusing hasql's
    own `ConnectionError` and `SessionError` (rather than wrapping them in
    pgmq-effectful-specific types) means users who already know hasql
    don't learn a new vocabulary, and we don't need to keep two sets of
    haddocks in sync with hasql's behavior.

    Both interpreters (plain and traced) now carry `Error
    PgmqRuntimeError :> es`. The traced interpreter records the error on
    the span (status + event) before throwing; observability and
    recoverability are independent channels.

    ## Alternatives considered

    **Keep `PgmqPoolError UsageError` and export `UsageError` directly
    from `Pgmq.Effectful`.** Rejected: users would have to import
    hasql-pool internals to pattern-match, and the wrapper added no
    value.

    **Define a new pgmq-specific error vocabulary (e.g.,
    `QueueNotFound`, `UniqueViolation`, etc.) translated from SQL states.**
    Rejected: classifying by SQL state is application-specific; what
    counts as "queue not found" depends on which pgmq function you
    called and how you interpret its raise-exception. The library
    exposes the raw structure and leaves classification to users. The
    single `isTransient` helper encodes hasql's own transience
    annotations and is the only classification we commit to.

    **Move the error type to `pgmq-core`.** Rejected: `pgmq-core` has
    no hasql dependency. Pulling hasql types into the core would cascade
    and break the package's role as the hasql-agnostic type layer.

    **Rename `pgmq-core`'s `PgmqError` instead of the effectful one.**
    Rejected: that type is public API used by `parseQueueName`,
    `parseRoutingKey`, `parseTopicPattern`. Renaming it cascades to every
    downstream parser user.

    ## Consequences

    - Users of `runPgmq` with existing code using `runError @PgmqError`
      must change the type annotation to `@PgmqRuntimeError` (or keep
      using the deprecated alias for one release cycle).
    - Users of `runPgmqTraced` who relied on errors escaping as IO
      exceptions (or who hoped their `runError` wrapper was catching
      them — it wasn't) now receive typed errors through the Error effect.
      This is a bug fix, not a regression.
    - `Hasql.ConnectionError` and `Hasql.SessionError` are now part of
      pgmq-effectful's public surface (they appear inside the exported
      `PgmqRuntimeError` constructors). Upgrading hasql to a version that
      changes these types is a breaking change for pgmq-effectful.

### Step 8 — Build.

    cabal build all

If the build complains about an upper-bound mismatch (e.g., pgmq-config
pins `pgmq-effectful <0.2`), update that package's cabal file to `<0.3`.

### Step 9 — Format and commit.

    nix fmt

    git add pgmq-effectful/pgmq-effectful.cabal \
            pgmq-effectful/CHANGELOG.md \
            README.md \
            docs/design/013-pgmq-effectful-error-model.md

    # add any cabal files whose upper bounds were updated:
    # git add pgmq-config/pgmq-config.cabal  # if applicable

    git commit -m "$(cat <<'EOF'
    docs(pgmq-effectful): document the 0.2.0.0 error model and migration

    Adds a 0.2.0.0 CHANGELOG entry describing the PgmqRuntimeError
    rename, the traced-interpreter error-propagation fix, isTransient,
    and a before/after migration guide. Extends the README's
    pgmq-effectful section with a short error-handling example. Adds
    a design doc (docs/design/013) recording the rationale and
    alternatives considered.

    Bumps pgmq-effectful to 0.2.0.0 (breaking rename of the public error
    type and a tightened Error constraint on the traced interpreter).

    MasterPlan: docs/masterplans/1-pgmq-effectful-error-propagation.md
    ExecPlan: docs/plans/5-pgmq-effectful-error-docs.md
    Intention: intention_01kpybay9hegps2fjt7tkwarz6
    EOF
    )"


## Validation and Acceptance

Complete when:

1. `cabal build all` exits 0.
2. `cat pgmq-effectful/CHANGELOG.md` shows the 0.2.0.0 entry at the top
   with breaking-change, new-feature, and migration-guide sub-sections.
3. `grep -A 30 "Error handling" README.md` shows the new subsection.
4. `ls docs/design/013-pgmq-effectful-error-model.md` succeeds.
5. `cat pgmq-effectful/pgmq-effectful.cabal | head -5` shows
   `version: 0.2.0.0`.
6. Commit has the three required trailers.


## Idempotence and Recovery

All edits are to human-readable docs or to one version-bump field.
Rolling back:

    git restore pgmq-effectful/pgmq-effectful.cabal \
                pgmq-effectful/CHANGELOG.md \
                README.md
    rm docs/design/013-pgmq-effectful-error-model.md


## Interfaces and Dependencies

No code dependencies added. One artifact file created
(`docs/design/013-pgmq-effectful-error-model.md`). Three files edited
(`pgmq-effectful.cabal`, `pgmq-effectful/CHANGELOG.md`, `README.md`).
Possibly one or more cabal files in other packages for upper-bound
updates.
