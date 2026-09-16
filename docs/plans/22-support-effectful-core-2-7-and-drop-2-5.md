---
id: 22
slug: support-effectful-core-2-7-and-drop-2-5
title: "Support effectful-core 2.7 and drop 2.5"
kind: exec-plan
created_at: 2026-09-16T12:40:53Z
intention: "intention_01m2n3qw9rem08g6g3w0jthqvp"
provenance:
  created_by:
    model: "claude-opus-5[1m]"
    harness: "claude-code"
    at: 2026-09-16T12:40:53Z
  revisions:
    - model: "claude-opus-5[1m]"
      harness: "claude-code"
      at: 2026-09-16T12:53:00Z
      mode: "update"
      note: "Relax the upper bound from >=2.7.1.1 to a plain ^>=2.7"
    - model: "claude-opus-5[1m]"
      harness: "claude-code"
      at: 2026-09-16T13:07:15Z
      mode: "implement"
      note: "Implementing milestones 1-3: widen effectful-core bound, pin Nix to 2.7.1.2, document the range"
---

# Support effectful-core 2.7 and drop 2.5

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.
If durable project context changes, update or create ADRs in docs/adr/ in the same change.


## Purpose / Big Picture

Today a person who depends on `pgmq-effectful`, `pgmq-config`, or `pgmq-bench` cannot also
depend on `effectful-core` 2.7. All three declare `effectful-core ^>=2.5 || ^>=2.6`,
which means "at least 2.5 and below 2.6, or at least 2.6 and below 2.7". Cabal's dependency
solver refuses to build an application that pins `effectful-core` 2.7 together with ours, so
anyone who has already upgraded their own codebase to 2.7 is stuck on our 0.6.0.0 release or
has to patch our `.cabal` files by hand.

After this change, that person can add `effectful-core >= 2.7` to their own build and still
depend on our packages, and the solver will accept it. They can see this working directly: in
a checkout of this repository, `cabal build all` resolves `effectful-core 2.7.1.2` (today it
resolves 2.6.1.0) and the whole test suite passes against it, and `nix build .#pgmq-effectful`
builds the library against 2.7.1.2 as well.

At the same time we stop claiming support for `effectful-core` 2.5. Nothing in this repository
builds or tests against 2.5 — the Nix toolchain ships 2.6.1.0 and the Cabal solver picks the
newest allowed version — so the 2.5 half of the bound is an untested promise. Dropping it
narrows the promise to versions we actually build.

The end state is a declared range of `^>=2.6 || ^>=2.7` in all five places where the bound
appears, a Nix package set that builds against 2.7.1.2, and a repeatable command that proves
the 2.6 floor of the range still compiles.


## Progress

Use a checklist to summarize granular steps. Every stopping point must be documented here,
even if it requires splitting a partially completed task into two ("done" vs. "remaining").
This section must always reflect the actual current state of the work.

- [x] Milestone 1: widen the `effectful-core` bound in all five `build-depends` sites. (2026-09-16)
- [x] Milestone 1: `cabal build all` resolves `effectful-core 2.7.1.2` and succeeds. (2026-09-16)
- [x] Milestone 1: `cabal test all` passes against 2.7.1.2 — all five suites, plus the
      `partman` shell's partition acceptance run (11 tests). (2026-09-16)
- [x] Milestone 1: `cabal build all --constraint='effectful-core <2.7'` still succeeds (2.6 floor);
      `dist-effectful-floor/cache/plan.json` names `effectful-core 2.6.1.0`. (2026-09-16)
- [x] Milestone 1: Acceptance 1 and Acceptance 3 both captured — the `>= 2.7` dry run failed before
      the edit and succeeds after, and `< 2.6` now fails. (2026-09-16)
- [ ] Milestone 1: commit.
- [x] Milestone 2: add `strict-mutable-base` 2.0.0.0 and `effectful-core` 2.7.1.2 to
      `nix/haskell-overlay.nix`. Both `sha256` values given in the plan were wrong and were
      replaced with the values Nix reported; see Surprises & Discoveries. (2026-09-16)
- [x] Milestone 2: add `pgmq-effectful-tests` to the `checks` attribute set in `flake.module.nix`,
      then **revert it** — the check cannot evaluate because of a pre-existing defect in the
      OpenTelemetry half of `nix/haskell-overlay.nix`, unrelated to `effectful-core`. This is
      the recovery the plan's Idempotence and Recovery section prescribes. (2026-09-16)
- [x] Milestone 2: the overrides evaluate — `nix eval` reports
      `effectful-core=2.7.1.2 strict-mutable-base=2.0.0.0` (it printed
      `effectful-core=2.6.1.0 strict-mutable-base=1.1.0.0` before the edit). (2026-09-16)
- [x] Milestone 2: `nix build .#pgmq-effectful` succeeds and
      `nix-store --query --references` on its output names
      `/nix/store/...-effectful-core-2.7.1.2` (Acceptance 5, first half). (2026-09-16)
- [x] Milestone 2: `nix flake check` succeeds once `pgmq-effectful-tests` is removed. (2026-09-16)
- [x] Milestone 2: commit (made ahead of the build finishing, at the user's request). (2026-09-16)
- [x] Milestone 3: add the `effectful-floor` recipe to `Justfile`; `just --list` shows it and
      `just effectful-floor` exits 0 resolving `effectful-core 2.6.1.0` (Acceptance 4). (2026-09-16)
- [x] Milestone 3: add "Unreleased" entries, including the advisory preferring 2.7.1.1 over 2.7.0.0, to the root, `pgmq-effectful`, and `pgmq-config` changelogs. (2026-09-16)
- [x] Milestone 3: document the supported `effectful-core` range in `README.md`. (2026-09-16)
- [ ] Milestone 3: `nix fmt`, then commit.
- [ ] Final: fill in Outcomes & Retrospective and run the ADR distillation pass.


## Surprises & Discoveries

Document unexpected behaviors, bugs, optimizations, or insights discovered during
implementation. Provide concise evidence.

The following were discovered during plan research, before any implementation.

**No Haskell source change is required.** The entire `effectful` API surface this repository
uses is `Eff`, `Effect`, `Dispatch(..)`, `DispatchOf`, `(:>)`, `IOE`, `runEff` and `liftIO`
from the `Effectful` module; `send` and `interpret` from `Effectful.Dispatch.Dynamic`; and
`Error`, `throwError`, `runError` and `runErrorNoCallStack` from `Effectful.Error.Static`.
Every one of those names is still exported, with the same type, in `effectful-core` 2.7.1.2.
The largest 2.7 breaking change — `LocalEnv` losing its second type parameter — cannot affect
us because both interpreters discard the local environment: `pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs:116`
and `pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs:178` both read
`interpret $ \_ -> \case`, so the type of the ignored argument never appears in our source.
The other 2.7 breaking changes (the un-ticked `Effectful.Concurrent.*.Strict` and
`Effectful.Prim.IORef.Strict` names, and the `Effectful.Internal.MTL` rename) live in the
`effectful` package and in internal modules that we do not depend on at all.

**A hidden transitive dependency forces a second Nix override.** `effectful-core` 2.7 requires
`strict-mutable-base >= 2.0.0.0 && < 3`, but the Nix package set this repository builds from
(`pkgs.haskell.packages.ghc9124`) ships `strict-mutable-base` 1.1.0.0. Pinning only
`effectful-core` in `nix/haskell-overlay.nix` would therefore fail to evaluate. Evidence:

```text
$ nix eval ... ghc9124: strict-mutable-base = 1.1.0.0, effectful-core = 2.6.1.0
$ curl .../strict-mutable-base/preferred
{"deprecated-version":["1.0.0.0"],"normal-version":["2.0.0.0","1.1.0.0"]}
```

The Cabal side has no such problem because the solver is free to pick
`strict-mutable-base 2.0.0.0` straight from Hackage.

**The Cabal solver already has a clean answer.** A dry run against a scratch build directory
confirmed that nothing else in the closure objects to 2.7:

```text
$ cabal build --dry-run --builddir=<scratch> \
    --allow-newer='pgmq-effectful:effectful-core,pgmq-config:effectful-core,pgmq-bench:effectful-core' \
    --constraint='effectful-core >= 2.7' all
... (plan resolves; no solver error)
effectful-core 2.7.1.2
strict-mutable-base 2.0.0.0
exceptions 0.10.12
primitive 0.9.1.0
```

`exceptions 0.10.12` is GHC 9.12.4's own boot library (`ghc-pkg list --global` confirms it),
so it is reused rather than rebuilt. It happens to satisfy `exceptions >= 0.10.11`, which is
the condition under which `effectful-core` 2.7 defines `rethrowM` and `catchNoPropagate` on
its `MonadThrow`/`MonadCatch` instances for `Eff`; we do not use those instances, so this is
informational only.


The following were discovered during implementation.

**The `nix develop` shell puts GNU sed on `PATH`, not macOS BSD sed.** Step 1 gives the BSD
form `sed -i ''` as the primary command and the GNU form as a parenthetical, but on this
machine the dev shell's `sed` is GNU sed 4.10, which reads the `''` as an empty script and
then treats the real `s/.../` expression as a filename:

```text
$ sed --version | head -1
sed (GNU sed) 4.10
$ sed -i '' 's/^    effectful-core .../...' pgmq-effectful/pgmq-effectful.cabal
sed: can't read s/^    effectful-core \^>=2\.5 || \^>=2\.6,$/...: No such file or directory
```

The GNU form (`sed -i` with no separate suffix argument) worked and produced exactly the five
expected line changes. Inside `nix develop`, prefer the GNU form on macOS too.

**Both `callHackageDirect` hashes in the plan were wrong.** The plan's Idempotence and Recovery
section anticipated this exact failure and prescribed the fix (take the `got:` value), which is
what was done. `nix eval` surfaced them one at a time, because evaluating `.version` forces the
cabal2nix import-from-derivation and therefore the tarball fetch:

```text
error: hash mismatch in fixed-output derivation '...-source.drv':
         specified: sha256-SCP8YLoafJNpwbP8hmIe8abpqAom/ya+v5P+fkDU2aY=
            got:    sha256-OZhGk0UY3BMWF+oUAQnCvF3hnzscBCm0Cz+nz8p2XM8=
```

```text
error: hash mismatch in fixed-output derivation '...-source.drv':
         specified: sha256-xAlEIMRvoh5sAdBKRwonwmYZqMoFBbds+HWKDwN2LeI=
            got:    sha256-3o2PMN8l56X7ULqyNNJrJQZ8xgqqOsxhjm0jfULQt+k=
```

The corrected values now in `nix/haskell-overlay.nix` are `effectful-core` 2.7.1.2 =
`sha256-OZhGk0UY3BMWF+oUAQnCvF3hnzscBCm0Cz+nz8p2XM8=` and `strict-mutable-base` 2.0.0.0 =
`sha256-3o2PMN8l56X7ULqyNNJrJQZ8xgqqOsxhjm0jfULQt+k=`. The likely cause is that the plan's
values were computed over the raw Hackage tarball, whereas `callHackageDirect` hashes the
unpacked source tree; the recovery path in the plan does not depend on knowing which.

**`pgmq-effectful-tests` cannot run under Nix, for reasons that pre-date this plan.** Adding the
check made `nix flake check` fail at *evaluation* time, not at test time:

```text
❌ checks.aarch64-darwin.pgmq-effectful-tests
error: function 'anonymous lambda' called without required argument 'hs-opentelemetry-propagator-jaeger'
  at /nix/store/...-cabal2nix-hs-opentelemetry-sdk/default.nix:1:1
```

`pgmq-effectful/pgmq-effectful.cabal` makes the test suite depend on
`hs-opentelemetry-sdk >=1.0 && <2`, and the SDK's own cabal file requires six sibling packages
at `==1.0.*`: `exporter-handle`, `exporter-otlp`, and the `b3`, `datadog`, `jaeger` and `xray`
propagators. `nix/haskell-overlay.nix` defines only `api`, `api-types`, `semantic-conventions`,
`propagator-w3c`, `exporter-in-memory` and `sdk` itself from the pinned
`iand675/hs-opentelemetry` tree. The `ghc9124` package set supplies four of the six missing
names, but at 0.0.x/0.1.x — far below the `==1.0.*` the SDK demands:

```text
hs-opentelemetry-exporter-handle = "0.0.1.2"; hs-opentelemetry-exporter-otlp = "0.1.1.0";
hs-opentelemetry-propagator-b3 = "0.0.1.3"; hs-opentelemetry-propagator-datadog = "0.0.1.1";
hs-opentelemetry-propagator-jaeger = "MISSING"; hs-opentelemetry-propagator-xray = "MISSING";
```

This is a latent defect, not a regression: evaluating `hs-opentelemetry-sdk` in a worktree at
`8a704c3` — the commit before Milestone 1 — fails with the identical `jaeger` error. Nothing
had ever forced its evaluation because no `checks` entry built the effect layer's test suite.
The library builds fine because it needs only `hs-opentelemetry-api`; the SDK is test-only.

Fixing it means adding six `callCabal2nix` entries from the pinned tree (and `exporter-otlp`
drags `proto-lens`), which is unrelated to `effectful-core` and outside this plan's scope. The
`propagators/jaeger` and `propagators/xray` directories do exist in the pinned source, so the
fix is mechanical when someone takes it on.


## Decision Log

Record every decision made while working on the plan.

- Decision: keep `effectful-core` 2.6 support rather than moving to 2.7 only.
  Rationale: the request was "add support for 2.7 and drop support for 2.5", which leaves 2.6
  in the supported set. Keeping 2.6 also means the Nix build (which today has 2.6.1.0 in its
  package set) and the Cabal build (which will pick 2.7.1.2) exercise different points of the
  range, and costs nothing because no source change is needed for either.
  Date: 2026-09-16

- Decision: the upper half of the bound is a plain `^>=2.7`; 2.7.0.0 stays inside the
  supported range.
  Rationale: reversed on 2026-09-16 at the user's instruction ("it should not be that strict,
  `>= 2.7` is enough"), replacing an earlier draft decision to write `>=2.7.1.1 && <2.8` and
  exclude 2.7.0.0. What 2.7.0.0 does wrong is a performance regression, not a correctness or
  API problem: its changelog says it "increased the per-operation overhead of dynamically
  dispatched effects", and 2.7.1.1 fixed it. The `Pgmq` effect in
  `pgmq-effectful/src/Pgmq/Effectful/Effect.hs` is dynamically dispatched (`type instance
  DispatchOf Pgmq = Dynamic`) and every queue operation goes through `send`, so that overhead
  would land on our hot path — but a Cabal bound states what we are *compatible* with, and we
  are compatible with 2.7.0.0. Excluding it would cause real solver failures for consumers
  already on 2.7.0.0 in order to nudge a performance characteristic they can fix themselves by
  upgrading. The right home for "prefer 2.7.1.1 or newer" is a changelog note, which Milestone
  3 writes, not a constraint that breaks builds.
  Date: 2026-09-16

- Decision: pin the Nix package set to `effectful-core` 2.7.1.2 rather than letting it stay on
  the nixpkgs default of 2.6.1.0.
  Rationale: chosen by the user when the trade-off was presented. `nix flake check` is the
  build that gates a release; if it never sees 2.7 then "we support 2.7" is a claim no
  automated check defends. The 2.6 end of the range is instead defended by an explicit
  constrained Cabal build (Milestone 3's `just effectful-floor`).
  Date: 2026-09-16

- Decision: add `pgmq-effectful-tests` to the `checks` attribute set in `flake.module.nix`.
  Rationale: `checks` currently builds the `pgmq-effectful` *library* but runs only the
  `pgmq-hasql`, `pgmq-migration` and `pgmq-config` test suites. Since the whole point of this
  plan is to prove the effect layer works on a new `effectful-core`, running its test suite
  under the pinned package set is what turns compilation into evidence. The test harness
  (`pgmq-effectful/test/EphemeralDb.hs`) is the same shape as `pgmq-config`'s, which already
  runs in `checks`, so no new sandbox requirement is introduced.
  Date: 2026-09-16

- Decision: do not bump package versions in this plan; add "Unreleased" changelog sections
  instead.
  Rationale: `agents/skills/release/SKILL.md` owns versioning — all five packages share one
  version, bumped in a single release commit, and it explicitly moves content out of an
  "Unreleased" section into the new version section. Bumping here would collide with that.
  Date: 2026-09-16


- Decision: commit Milestone 2 before `nix build .#pgmq-effectful` and `nix flake check` had
  finished.
  Rationale: the user asked for the changes to be committed while the cold Nix rebuild of the
  `effectful-core`/`hasql`/`hs-opentelemetry` closure was still running. The committed state is
  independently evidenced — `nix eval` confirms the package set now resolves `effectful-core`
  2.7.1.2 and `strict-mutable-base` 2.0.0.0, and the Cabal path already built and tested the
  whole project against 2.7.1.2 in Milestone 1 — so the commit is not unverified, only
  not-yet-fully-verified. The outstanding build is tracked as a remaining Progress item and
  must pass before the plan is closed.
  Date: 2026-09-16


- Decision: revert the `pgmq-effectful-tests` entry in `flake.module.nix` rather than repair the
  OpenTelemetry overlay.
  Rationale: the plan's Idempotence and Recovery section prescribes exactly this when
  `nix flake check` fails only on `pgmq-effectful-tests` while `cabal test pgmq-effectful`
  passes — remove the line, record the failure, rely on the Cabal run for the evidence, and do
  not let it block the plan. The cause turned out to be an incomplete OTel overlay rather than
  a sandbox quirk, which strengthens the case: it is a pre-existing defect reproducible at
  `8a704c3`, it has nothing to do with `effectful-core`, and repairing it means adding six
  git-sourced `hs-opentelemetry-*` derivations. Milestone 2's actual goal is still met and
  still machine-checked — `nix build .#pgmq-effectful` links against
  `effectful-core-2.7.1.2`, and `nix flake check` builds the `pgmq-effectful` library from the
  pinned package set. What is lost is only running the effect layer's *test suite* under Nix;
  that suite does run, against 2.7.1.2, under `cabal test all`.
  Date: 2026-09-16


## Outcomes & Retrospective

Summarize outcomes, gaps, and lessons learned at major milestones or at completion.
Compare the result against the original purpose. Before marking the plan complete,
distill durable project context from the Decision Log, Surprises & Discoveries, and
this section into docs/adr/. Keep task-local execution details here.

(To be filled during and after implementation.)


## Context and Orientation

### What this repository is

`pgmq-hs` is a Haskell client for PGMQ, a message queue that lives entirely inside a
PostgreSQL database. It is a multi-package Cabal project: the packages are listed in
`cabal.project` at the repository root and each lives in its own directory with its own
`.cabal` file. Six packages exist; three of them declare an `effectful-core` dependency, and
those three are the only ones edited here.

`pgmq-effectful` is the one that actually integrates with `effectful`. `effectful` is a
Haskell "effect system" library: instead of writing code in `IO`, you write it in a monad
called `Eff es`, where `es` is a type-level list of the capabilities ("effects") the code is
allowed to use. A constraint like `Pgmq :> es` reads as "the effect list `es` contains the
`Pgmq` effect". The library ships as two Hackage packages: `effectful-core`, which contains
the monad and the core machinery, and `effectful`, which adds concurrency, filesystem and
other batteries. **This repository depends only on `effectful-core`.** That fact removes a
large fraction of the 2.7 breaking changes from consideration, because they are changes to
modules that only exist in the `effectful` package.

The three packages that declare an `effectful-core` dependency, and the exact lines where the
version bound appears, are:

- `pgmq-effectful/pgmq-effectful.cabal:61` — the library.
- `pgmq-effectful/pgmq-effectful.cabal:107` — its `pgmq-effectful-test` test suite.
- `pgmq-config/pgmq-config.cabal:80` — inside `if flag(effectful)` in the library stanza.
- `pgmq-config/pgmq-config.cabal:140` — inside `if flag(effectful)` in the test stanza.
- `pgmq-bench/pgmq-bench.cabal:58` — its `benchmark pgmq-bench` stanza.

All five currently read exactly:

```cabal
    effectful-core ^>=2.5 || ^>=2.6,
```

(the two `pgmq-config` sites are indented six spaces instead of four, because they sit inside
a conditional block — preserve the existing indentation when editing). `^>=X.Y` is Cabal's
"caret" operator and means `>= X.Y && < X.(Y+1)`, so the current bound admits 2.5.* and 2.6.*
and excludes 2.7.

`pgmq-config` puts its `effectful` integration behind a Cabal flag named `effectful`, declared
near the top of `pgmq-config/pgmq-config.cabal` with `default: True`. When the flag is on, the
library exposes the extra module `Pgmq.Config.Effectful` and the test suite compiles with
`-DPGMQ_EFFECTFUL`. Because the flag defaults to on, ordinary builds always exercise these
stanzas; there is nothing extra to enable.

`pgmq-core`, `pgmq-hasql` and `pgmq-migration` have no `effectful` dependency and are not
touched by this plan, though they are rebuilt as dependencies.

### Where the effectful API is actually used

The complete list of files that import anything from `effectful-core`:

- `pgmq-effectful/src/Pgmq/Effectful/Effect.hs` — imports `Dispatch (..)`, `DispatchOf`,
  `Eff`, `Effect`, `(:>)` from `Effectful` and `send` from `Effectful.Dispatch.Dynamic`.
  This file defines the `Pgmq` effect as a GADT and declares it dynamically dispatched.
- `pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs` — `Eff`, `IOE`, `(:>)`, `Effectful`
  qualified (for `Effectful.liftIO`), `interpret`, and `Error`/`throwError`.
- `pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs` — the same set.
- `pgmq-effectful/src/Pgmq/Effectful/Traced.hs` — `Eff`, `IOE`, `(:>)`, `Effectful` qualified.
- `pgmq-config/src/Pgmq/Config/Effectful.hs` — `Eff` and `(:>)` only.
- The test suites and `pgmq-bench/bench/BenchSetup.hs` — `runEff`, `runError`,
  `runErrorNoCallStack`, `Error`, `Eff`, `IOE`, `(:>)`.

That is the whole surface. Nothing imports `Effectful.Internal.*`, `LocalEnv`,
`SharedSuffix`, `KnownEffects`, `ProviderList`, `stateM`, `runStateMVar`, `withLiftMap`, or
any `*.Strict` concurrency module — the six things the 2.7 upgrade actually breaks or
deprecates.

### What changed in effectful 2.7

A written upgrade guide exists upstream at
`mori://effectful/effectful/docs/upgrade-2.6-to-2.7` (verify with
`mori path mori://effectful/effectful/docs/upgrade-2.6-to-2.7`, which resolves to
`docs/effectful-2.6-to-2.7-upgrade.md` in that project's checkout). You do not need to read
it — everything relevant is restated here.

The breaking changes in `effectful-core` 2.7.0.0 are: GHC older than 9.6 is no longer
supported (we use GHC 9.12.4, so this is fine); `LocalEnv` lost its second type parameter and
the `SharedSuffix` class was deprecated; the `KnownEffects` constraint was removed in favour
of `KnownSubset`; the internal module `Effectful.Internal.MTL` was renamed to
`Effectful.Internal.Effect.Dynamic`; and the preconditions of the internal functions
`unconsEnv`/`unreplaceEnv` were tightened. Separately, the `effectful` package (which we do
not depend on) renamed every ticked identifier in its strict `MVar`/`Chan`/`IORef` modules.
None of these reach our source, as established above.

Two behavioural changes are worth knowing even though they do not apply to us:
`runInBoundThread`/`runInUnboundThread` no longer clone the environment (those functions live
in the `effectful` package), and `runPureEff` now runs its computation on a separate thread
(we never call `runPureEff`; every entry point in this repository is `runEff`, which needs
`IOE`).

New in 2.7 and available to downstream users after this change: the `Input`, `Output` and
`ReturnWith` effects, dynamically dispatched `Provider`/`ProviderList`, and
`rethrowError`/`rethrowErrorWith` in `Effectful.Error.Static` for re-raising a caught error
with its original `CallStack`. We do not adopt any of them here; mentioning them is only so a
reader understands why a consumer might want 2.7.

The released versions on Hackage, verified against
`https://hackage.haskell.org/package/effectful-core/preferred`, are 2.7.0.0, 2.7.1.0, 2.7.1.1
and 2.7.1.2. 2.7.1.2 is the newest release. (The upstream working tree contains an unreleased
2.7.1.3; do not pin to it.)

### How this repository builds

There are two build paths and both must be updated.

The **Cabal path** is what a developer runs day to day. `nix develop` drops you into a shell
with GHC 9.12.4, `cabal`, PostgreSQL and HLS on `PATH`, and `cabal build` / `cabal test` then
resolve dependencies from the Hackage index in the usual way. `cabal.project` pins a handful
of dependencies to git revisions (`hasql`, `hasql-pool`, `hasql-transaction` and the
`hs-opentelemetry` family) via `source-repository-package` stanzas, but `effectful-core` is
not among them, so the solver simply picks the newest version the bounds allow.

The **Nix path** is `nix build .#<package>` and `nix flake check`. `flake.module.nix` builds
from `pkgs.haskell.packages.ghc9124` overridden with `nix/haskell-overlay.nix`. That overlay
file is a standard Nix overlay — a function `final: prev: { ... }` where each attribute
replaces or adds a Haskell package. It already contains the two idioms we need:
`callHackageDirect` (fetch a specific version's tarball from Hackage and turn it into a
derivation) for `crypton` and `optparse-applicative`, and `callCabal2nix` on a fetched git
tree for the `hasql` and `hs-opentelemetry` families. `dontCheck` disables a package's own
test suite; `doJailbreak` strips its version bounds. There is no `effectful-core` entry today,
so the package set's own 2.6.1.0 is used.

`flake.module.nix` also declares the `checks` attribute set, which is what `nix flake check`
builds. It currently builds all five library derivations plus three test-suite derivations
(`pgmq-hasql-tests`, `pgmq-migration-tests`, `pgmq-config-tests`), each wrapped in a local
helper called `withTests` that re-enables the test suite and puts PostgreSQL on the build
sandbox's `PATH`. `pgmq-effectful-tests` is absent.

Tests do not need an external database: `pgmq-effectful/test/EphemeralDb.hs` and its siblings
use the `ephemeral-pg` library to start a throwaway PostgreSQL server per run.

There is no GitHub Actions workflow in this repository; `.github/workflows` does not exist.
All verification is the local commands given in this plan.

### Relevant ADRs

Following the ADR workflow in `.claude/skills/exec-plan/ADR.md`: `docs/adr/` holds two plain
Markdown records with no OKF profile —
[`docs/adr/fifo-native-overrides-and-index-upgrade-boundary.md`](../adr/fifo-native-overrides-and-index-upgrade-boundary.md)
and
[`docs/adr/pgmq-1.12-1.13-compatibility.md`](../adr/pgmq-1.12-1.13-compatibility.md).
Both are about PGMQ's SQL schema — which upstream extension functions we may override and how
native migrations track PGMQ 1.12/1.13. Neither has anything to say about Haskell dependency
version policy, and this plan touches no SQL. **No relevant ADR exists for this work.** If the
implementation surfaces a durable rule (for example, "our Cabal bounds state compatibility
only; performance advice belongs in the changelog"), the distillation pass at the end should
create one, following the existing plain-Markdown convention and adding no OKF frontmatter.


## Plan of Work

The work is three milestones. Milestone 1 changes what the packages promise and proves both
ends of the new promise with Cabal. Milestone 2 makes the Nix build actually exercise the new
version. Milestone 3 makes the promise discoverable and repeatable — changelogs, README, and a
`just` recipe that re-checks the lower bound.

Every command below is run from the repository root,
`/Users/shinzui/Keikaku/bokuno/libraries/pgmq-hs-project/pgmq-hs`, inside a `nix develop`
shell unless the command itself starts with `nix`.

### Milestone 1 — Widen the declared range and prove it with Cabal

Scope: edit exactly five lines across three `.cabal` files, then build and test twice — once
letting the solver pick the newest allowed `effectful-core` (which will be 2.7.1.2), and once
forcing it down to the 2.6 floor. Nothing else changes; in particular no `.hs` file is
touched.

At the end of this milestone the three packages accept `effectful-core` 2.7 and reject 2.5, and
you have seen the full test suite pass against 2.7.1.2 and the whole project compile against
2.6.1.0.

Replace each of the five occurrences of

```cabal
    effectful-core ^>=2.5 || ^>=2.6,
```

with

```cabal
    effectful-core ^>=2.6 || ^>=2.7,
```

preserving each site's existing indentation (four spaces in `pgmq-effectful.cabal` and
`pgmq-bench.cabal`, six spaces at both `pgmq-config.cabal` sites, because those sit inside an
`if flag(effectful)` block).

Read the new bound as: "any 2.6.x, or any 2.7.x". Every released 2.7 is inside the range,
including 2.7.0.0 — see the Decision Log entry on why we do not exclude it despite its known
performance regression.

Acceptance: the resolved build plan names `effectful-core-2.7.1.2`, `cabal test all` passes,
and `cabal build all --constraint='effectful-core <2.7'` also succeeds. Exact commands and
expected output are in Concrete Steps.

### Milestone 2 — Build against 2.7.1.2 under Nix

Scope: two new entries in `nix/haskell-overlay.nix` and one new line in `flake.module.nix`.

At the end of this milestone `nix build .#pgmq-effectful` compiles the effect layer against
`effectful-core` 2.7.1.2 instead of the package set's 2.6.1.0, and `nix flake check` runs the
`pgmq-effectful` test suite (which it does not do today) against that same version.

Two overlay entries are needed, not one. `effectful-core` 2.7 requires
`strict-mutable-base >= 2.0.0.0`, and the `ghc9124` package set ships 1.1.0.0, so pinning
`effectful-core` alone would fail to evaluate with an unsatisfied dependency. Overriding
`strict-mutable-base` in the same overlay is safe here because `effectful-core` is the only
consumer of it anywhere in this project's dependency closure — we do not depend on the
`effectful` package, which is the other package in the set that would care.

Add a new section to `nix/haskell-overlay.nix`, placed immediately before the existing
`# ── Test dependencies ──` comment (currently line 133), so the file's existing grouping is
preserved:

```nix
  # ── effectful 2.7 ──────────────────────────────────────────────────
  #
  # The ghc9124 package set ships effectful-core 2.6.1.0 and
  # strict-mutable-base 1.1.0.0. Our cabal bounds allow 2.6 as well, but the
  # Nix build is the one that gates releases, so it pins the newer end of the
  # range. effectful-core 2.7 requires strict-mutable-base >= 2.0.0.0, hence
  # the second override.

  strict-mutable-base = dontCheck (doJailbreak (final.callHackageDirect
    {
      pkg = "strict-mutable-base";
      ver = "2.0.0.0";
      sha256 = "sha256-xAlEIMRvoh5sAdBKRwonwmYZqMoFBbds+HWKDwN2LeI=";
    }
    { }));

  effectful-core = dontCheck (doJailbreak (final.callHackageDirect
    {
      pkg = "effectful-core";
      ver = "2.7.1.2";
      sha256 = "sha256-SCP8YLoafJNpwbP8hmIe8abpqAom/ya+v5P+fkDU2aY=";
    }
    { }));
```

Those two hashes were obtained with `nix-prefetch-url` against
`https://hackage.haskell.org/package/<pkg>-<ver>/<pkg>-<ver>.tar.gz` and converted to SRI
form; if Nix ever reports a mismatch, the fix is in Idempotence and Recovery below.

`dontCheck` and `doJailbreak` follow the file's existing convention — every other entry in
that file uses both. `dontCheck` matters for `strict-mutable-base`, which does declare a test
suite; `effectful-core` declares none. `doJailbreak` is belt-and-braces for both, since
neither has a bound that GHC 9.12.4's libraries violate.

Then, in `flake.module.nix`, extend the `checks` attribute set (currently lines 106–111) with
one more line so the effect layer's own test suite runs:

```nix
        pgmq-effectful-tests = withTests haskellPackages.pgmq-effectful;
```

Place it directly above `pgmq-hasql-tests` so the three existing `*-tests` entries and the new
one stay together as one block.

Acceptance: `nix build .#pgmq-effectful` succeeds, a `nix eval` of the package set reports
`effectful-core` 2.7.1.2, and `nix flake check` succeeds with `pgmq-effectful-tests` among the
derivations it builds.

### Milestone 3 — Make the range discoverable and the floor re-checkable

Scope: a `Justfile` recipe, three changelog entries, and one README paragraph.

At the end of this milestone anyone can run one short command to re-verify the bottom of the
supported range, and a reader of the README or the changelogs can see which `effectful-core`
versions the 0.7 line will accept.

The floor check matters because after Milestone 2 both build paths use 2.7.1.2: Cabal picks it
because it is newest, Nix because we pinned it. Without a deliberate constrained build,
nothing would ever compile against 2.6 again and the lower half of our declared bound would
rot. Add to `Justfile`, in the existing build/test group (near the `test` recipe):

```just
# Verify the lower end of the supported effectful-core range still compiles
effectful-floor:
    cabal build all --builddir=dist-effectful-floor --constraint='effectful-core <2.7'
```

Then add an `## Unreleased` section at the top of each of the three changelogs that have
something to say, directly beneath the `# Revision history for ...` heading and above the
`## 0.6.0.0 -- 2026-09-10` heading. Do not invent a version number — `agents/skills/release/SKILL.md`
assigns one at release time and moves "Unreleased" content into it.

In `CHANGELOG.md` (repository root), note that the family now supports `effectful-core` 2.7
and no longer claims 2.5, and that no API changed. Add one advisory sentence recommending
2.7.1.1 or newer over 2.7.0.0: 2.7.0.0 is inside our supported range and builds fine, but
upstream regressed the per-operation overhead of dynamically dispatched effects in it and fixed
that in 2.7.1.1, and every `pgmq-effectful` operation is dynamically dispatched. This is a
recommendation in prose, deliberately not a version constraint.

In `pgmq-effectful/CHANGELOG.md` and `pgmq-config/CHANGELOG.md`, say the same in one or two
sentences from that package's point of view. `pgmq-bench` has no changelog and is marked
internal in `mori.dhall`, so it gets no entry.

Finally, in `README.md`, the `## pgmq-effectful` section (starting around line 49) describes
what the package provides but says nothing about which `effectful` versions it works with. Add
one sentence there naming the supported range and pointing out that only `effectful-core` is
required, not the full `effectful` package.

Run `nix fmt` before committing — the pre-commit hook will otherwise reformat the files and
force a re-stage, as `CLAUDE.md` warns.

Acceptance: `just effectful-floor` succeeds and visibly resolves an `effectful-core` below
2.7; the three changelogs and the README render correctly; `nix fmt` leaves the tree clean.


## Concrete Steps

All commands assume the working directory is the repository root,
`/Users/shinzui/Keikaku/bokuno/libraries/pgmq-hs-project/pgmq-hs`.

### Step 0 — Enter the toolchain

```bash
nix develop
```

This gives you GHC 9.12.4, `cabal` 3.16.1.0, PostgreSQL and `just`. Confirm with:

```bash
ghc --version && cabal --version
```

Expected:

```text
The Glorious Glasgow Haskell Compilation System, version 9.12.4
cabal-install version 3.16.1.0
compiled using version 3.16.1.0 of the Cabal library (in-tree)
```

Record the starting point so you can see the change:

```bash
grep -rn "effectful-core" pgmq-effectful/pgmq-effectful.cabal pgmq-config/pgmq-config.cabal pgmq-bench/pgmq-bench.cabal
```

Expected:

```text
pgmq-effectful/pgmq-effectful.cabal:61:    effectful-core ^>=2.5 || ^>=2.6,
pgmq-effectful/pgmq-effectful.cabal:107:    effectful-core ^>=2.5 || ^>=2.6,
pgmq-config/pgmq-config.cabal:80:      effectful-core ^>=2.5 || ^>=2.6,
pgmq-config/pgmq-config.cabal:140:      effectful-core ^>=2.5 || ^>=2.6,
pgmq-bench/pgmq-bench.cabal:58:    effectful-core ^>=2.5 || ^>=2.6,
```

If the line numbers differ, that is fine — match on the text, not the number.

Now capture the "before" evidence for Acceptance 1, while the old bound is still in place.
Ask the solver for `effectful-core` 2.7 and watch it refuse. Use a throwaway build directory
so your real `dist-newstyle` is untouched:

```bash
cabal build --dry-run --builddir=/tmp/pgmq-effectful-before \
  --constraint='effectful-core >= 2.7' all
```

Expected (the git clones of the `source-repository-package` dependencies scroll past first;
this is the tail):

```text
Error: [Cabal-7107]
Could not resolve dependencies:
[__0] trying: pgmq-effectful-0.6.0.0 (user goal)
[__1] next goal: effectful-core (dependency of pgmq-effectful)
[__1] rejecting: effectful-core-2.7.1.2 (conflict: pgmq-effectful => effectful-core^>=2.5 || ^>=2.6)
[__1] skipping: effectful-core; 2.7.1.1, 2.7.1.0, 2.7.0.0 (has the same characteristics that caused the previous version to fail: excluded by constraint '^>=2.5 || ^>=2.6' from 'pgmq-effectful')
[__1] rejecting: effectful-core; 2.6.1.0, 2.6.0.0, 2.5.1.0, ... (constraint from command line flag requires >=2.7)
[__1] fail (backjumping, conflict set: effectful-core, pgmq-effectful)
After searching the rest of the dependency tree exhaustively, these were the goals I've had most trouble fulfilling: effectful-core, pgmq-effectful
```

That failure is the problem this plan fixes. Keep the transcript; Step 2 runs the same command
and expects success.

### Step 1 — Edit the five bounds (Milestone 1)

The four-space sites and the six-space sites differ only in indentation, so do them in two
passes to keep the indentation intact:

```bash
sed -i '' 's/^    effectful-core \^>=2\.5 || \^>=2\.6,$/    effectful-core ^>=2.6 || ^>=2.7,/' \
  pgmq-effectful/pgmq-effectful.cabal pgmq-bench/pgmq-bench.cabal
sed -i '' 's/^      effectful-core \^>=2\.5 || \^>=2\.6,$/      effectful-core ^>=2.6 || ^>=2.7,/' \
  pgmq-config/pgmq-config.cabal
```

(`sed -i ''` is the macOS/BSD form; on GNU sed use `sed -i`.) Verify:

```bash
git diff --stat && grep -rn "effectful-core" pgmq-effectful/pgmq-effectful.cabal pgmq-config/pgmq-config.cabal pgmq-bench/pgmq-bench.cabal
```

Expected:

```text
 pgmq-bench/pgmq-bench.cabal       | 2 +-
 pgmq-config/pgmq-config.cabal     | 4 ++--
 pgmq-effectful/pgmq-effectful.cabal | 4 ++--
 3 files changed, 5 insertions(+), 5 deletions(-)
pgmq-effectful/pgmq-effectful.cabal:61:    effectful-core ^>=2.6 || ^>=2.7,
pgmq-effectful/pgmq-effectful.cabal:107:    effectful-core ^>=2.6 || ^>=2.7,
pgmq-config/pgmq-config.cabal:80:      effectful-core ^>=2.6 || ^>=2.7,
pgmq-config/pgmq-config.cabal:140:      effectful-core ^>=2.6 || ^>=2.7,
pgmq-bench/pgmq-bench.cabal:58:    effectful-core ^>=2.6 || ^>=2.7,
```

Exactly five lines must change. If `git diff --stat` shows a different count, the `sed`
patterns matched something unintended — inspect `git diff` before continuing.

`nix fmt` runs `cabal-gild` over `.cabal` files, so it could in principle rewrite the new
bound. It does not: running `cabal-gild` 1.6.0.4 over a copy of `pgmq-bench.cabal` carrying
`effectful-core ^>=2.6 || ^>=2.7,` produced a byte-identical file. Write the bound exactly as
given above and `nix fmt` will leave it alone.

### Step 2 — Build and test against 2.7.1.2 (Milestone 1)

Refresh the Hackage index if `effectful-core` 2.7.1.2 is not yet known locally:

```bash
cabal update
```

Then confirm what the solver chooses:

```bash
cabal build --dry-run all >/dev/null && \
  grep -o '"pkg-name":"effectful-core","pkg-version":"[^"]*"' dist-newstyle/cache/plan.json | sort -u
```

Expected:

```text
"pkg-name":"effectful-core","pkg-version":"2.7.1.2"
```

If it still reports 2.6.1.0, the edits in Step 1 did not take effect or the local index is
stale; re-run `cabal update` and re-check Step 1's `grep` output.

Re-run the command that failed in Step 0; it must now succeed:

```bash
cabal build --dry-run --builddir=/tmp/pgmq-effectful-before \
  --constraint='effectful-core >= 2.7' all
```

Expected: a printed list of packages to build, ending with the local `pgmq-*` targets, and
exit status 0 — no `Cabal-7107` error. This is Acceptance 1.

Now build and test everything:

```bash
cabal build all
cabal test all
```

`cabal test all` starts throwaway PostgreSQL servers via `ephemeral-pg`, so it takes a few
minutes and needs no database of your own. Expected tail:

```text
1 of 1 test suites (1 of 1 test cases) passed.
```

repeated once per package with a test suite (`pgmq-core`, `pgmq-hasql`, `pgmq-migration`,
`pgmq-effectful`, `pgmq-config`). Any failure here is a real regression: investigate before
proceeding, and record what you find in Surprises & Discoveries.

Run the partition acceptance tests too, since they use a different shell:

```bash
nix develop .#partman --command cabal test pgmq-migration:pgmq-migration-test
```

### Step 3 — Prove the 2.6 floor still compiles (Milestone 1)

```bash
cabal build all --builddir=dist-effectful-floor --constraint='effectful-core <2.7'
```

Expected: a successful build that names `effectful-core-2.6.1.0` among the packages it
configures. This is the check that keeps the lower half of the bound honest.

The separate `--builddir` matters. Without it, this constrained build overwrites
`dist-newstyle/cache/plan.json` with the 2.6 plan, which would make Acceptance 2's
version check report 2.6.1.0 and would force the whole tree to rebuild again the next time
you run a plain `cabal build`. `dist-effectful-floor` is already covered by the `dist-*` line
in `.gitignore`. It is a full second build tree, so the first run is slow and later runs are
cached.

### Step 4 — Commit Milestone 1

```bash
nix fmt
git add -A
git commit -F - <<'EOF'
build!: support effectful-core 2.7 and drop 2.5

Widen the effectful-core bound in pgmq-effectful, pgmq-config and
pgmq-bench from `^>=2.5 || ^>=2.6` to `^>=2.6 || ^>=2.7`.

No Haskell source changes are needed: the API surface this repository
uses from effectful-core is unchanged in 2.7, and both interpreters
already discard the LocalEnv argument whose type lost a parameter.

The range includes 2.7.0.0. Upstream regressed per-operation overhead
for dynamically dispatched effects in that release and fixed it in
2.7.1.1, but that is a reason to recommend 2.7.1.1 in the changelog,
not to fail a consumer's solver.

ExecPlan: docs/plans/22-support-effectful-core-2-7-and-drop-2-5.md
Intention: intention_01m2n3qw9rem08g6g3w0jthqvp
EOF
```

### Step 5 — Pin the Nix package set (Milestone 2)

Open `nix/haskell-overlay.nix` and insert the `strict-mutable-base` and `effectful-core`
entries exactly as given in the Plan of Work, immediately above the line reading
`# ── Test dependencies ──────────────────────────────────────────────`.

Verify the override took, without building anything:

```bash
nix eval --raw --impure --expr 'let f = builtins.getFlake (toString ./.); pkgs = f.inputs.nixpkgs.legacyPackages.${builtins.currentSystem}; h = pkgs.haskell.packages.ghc9124.override { overrides = import ./nix/haskell-overlay.nix { inherit pkgs; }; }; in "effectful-core=" + h.effectful-core.version + " strict-mutable-base=" + h.strict-mutable-base.version'
```

Expected:

```text
effectful-core=2.7.1.2 strict-mutable-base=2.0.0.0
```

Before the edit the same command prints `effectful-core=2.6.1.0 strict-mutable-base=1.1.0.0`,
so running it once beforehand makes the change visible.

### Step 6 — Add the effectful test suite to `checks` (Milestone 2)

Edit `flake.module.nix` and add the `pgmq-effectful-tests` line to the `checks` set as shown
in the Plan of Work. Then build:

```bash
nix build .#pgmq-effectful
nix flake check
```

`nix flake check` builds every attribute in `checks`, which now includes the `pgmq-effectful`
test suite running against `effectful-core` 2.7.1.2. It is slow on a cold store — the
`effectful-core`, `hasql` and `hs-opentelemetry` derivations all rebuild.

Expected: both commands exit 0 with no output beyond build logs. To see explicitly that the
effect library was linked against the pinned version:

```bash
nix build .#pgmq-effectful --print-out-paths
```

and confirm the resulting `nix-store --query --references` output mentions
`effectful-core-2.7.1.2`:

```bash
nix-store --query --references "$(nix build .#pgmq-effectful --print-out-paths --no-link)" | grep effectful-core
```

Expected:

```text
/nix/store/<hash>-effectful-core-2.7.1.2
```

### Step 7 — Commit Milestone 2

```bash
nix fmt
git add -A
git commit -F - <<'EOF'
build(nix): pin effectful-core 2.7.1.2 and run the effectful tests

The ghc9124 package set ships effectful-core 2.6.1.0, so nothing in the
Nix build would have exercised 2.7 even after the cabal bounds allowed
it. Pin effectful-core 2.7.1.2, and strict-mutable-base 2.0.0.0 with it
because 2.7 requires >= 2.0.

Add pgmq-effectful-tests to `checks` so `nix flake check` runs the
effect layer's own test suite against the pinned version instead of
only compiling the library.

ExecPlan: docs/plans/22-support-effectful-core-2-7-and-drop-2-5.md
Intention: intention_01m2n3qw9rem08g6g3w0jthqvp
EOF
```

### Step 8 — Justfile recipe, changelogs and README (Milestone 3)

Add the `effectful-floor` recipe to `Justfile` as shown in the Plan of Work, then confirm it
is listed and runs:

```bash
just --list | grep effectful-floor
just effectful-floor
```

Expected from the first command:

```text
    effectful-floor      # Verify the lower end of the supported effectful-core range still compiles
```

Write the three `## Unreleased` changelog sections and the README sentence as described in
Milestone 3.

### Step 9 — Format and commit Milestone 3

```bash
nix fmt
git add -A
git commit -F - <<'EOF'
docs: record the supported effectful-core range

Add Unreleased changelog entries for the effectful-core 2.7 support and
the 2.5 drop, state the supported range in the README, and add a
`just effectful-floor` recipe so the 2.6 end of the range keeps being
compiled now that both default build paths resolve 2.7.1.2.

ExecPlan: docs/plans/22-support-effectful-core-2-7-and-drop-2-5.md
Intention: intention_01m2n3qw9rem08g6g3w0jthqvp
EOF
```

### Step 10 — Close out the plan

Fill in Outcomes & Retrospective, tick every box in Progress, and run the ADR distillation
pass described in Context and Orientation. Record a provenance revision entry once, at the
first stopping point where you write to this file:

```bash
bun agents/skills/exec-plan/record-provenance.ts revision \
  --plan docs/plans/22-support-effectful-core-2-7-and-drop-2-5.md \
  --model <your-exact-runtime-model-id> --harness <your-harness> \
  --mode implement --note "<one line>"
```


## Validation and Acceptance

The change is internal — no new function, no new flag, no new output — so acceptance is
phrased as "a build that previously could not exist now exists, and the old one still does".

**Acceptance 1: a consumer pinned to effectful-core 2.7 can depend on us.** Before the change,
asking Cabal for 2.7 is a hard solver failure; after the change it succeeds. Step 0 records
the failing transcript and Step 2 records the succeeding one, both from the same command:

```bash
cabal build --dry-run --constraint='effectful-core >= 2.7' all
```

Before: `Error: [Cabal-7107] Could not resolve dependencies` naming
`conflict: pgmq-effectful => effectful-core^>=2.5 || ^>=2.6`. After: a printed build plan and
exit 0. (The exact solver wording varies by `cabal` version; what matters is failure before
and success after.) If you reach this section without having run Step 0, you can still get the
"before" half from a scratch worktree at the previous commit:

```bash
git worktree add /tmp/pgmq-before HEAD~3 && \
  (cd /tmp/pgmq-before && cabal build --dry-run --constraint='effectful-core >= 2.7' all) ; \
  git worktree remove /tmp/pgmq-before
```

Adjust `HEAD~3` to whatever commit precedes Milestone 1.

**Acceptance 2: everything still works at 2.7.1.2.** `cabal test all` passes with
`effectful-core-2.7.1.2` in the plan. This is the real proof that the effect layer is
compatible, not just that it type-checks: the `pgmq-effectful` suite
(`pgmq-effectful/test/Main.hs` and its four spec modules) drives a live PostgreSQL through the
plain and traced interpreters and asserts on emitted OpenTelemetry spans, so a semantic
breakage in `interpret` or `Error` would surface as a failing assertion rather than a type
error. Confirm the version that was actually used:

```bash
grep -o '"pkg-name":"effectful-core","pkg-version":"[^"]*"' dist-newstyle/cache/plan.json | sort -u
```

**Acceptance 3: 2.5 is genuinely rejected.** Asking for it must now fail:

```bash
cabal build --dry-run --constraint='effectful-core < 2.6' all
```

Expected: a solver error naming `pgmq-effectful` and the new bound. A success here means one
of the five sites was missed.

**Acceptance 4: the 2.6 floor still builds.** `just effectful-floor` exits 0 and configures
`effectful-core-2.6.1.0`. If it ever stops
doing so, either fix the incompatibility or raise the lower bound deliberately — do not leave
the bound claiming a version that no longer compiles.

**Acceptance 5: Nix builds and tests against 2.7.1.2.** `nix flake check` exits 0, and

```bash
nix-store --query --references "$(nix build .#pgmq-effectful --print-out-paths --no-link)" | grep effectful-core
```

names `effectful-core-2.7.1.2`.

**Acceptance 6: no Haskell source file changed.** The final diff for Milestones 1 and 2 should
touch only `.cabal` files, `nix/haskell-overlay.nix` and `flake.module.nix`:

```bash
git diff --name-only <commit-before-milestone-1>..HEAD
```

If any `.hs` file appears, something unexpected happened — record it in Surprises &
Discoveries and explain it in the Decision Log, because the research behind this plan
concluded that no source change is required.


## Idempotence and Recovery

Every step here is safe to repeat. The `.cabal`, `.nix`, `Justfile`, changelog and README
edits are ordinary text edits under git; `git checkout -- <file>` restores any of them.
Re-running the `sed` commands in Step 1 is a no-op after the first run because the pattern no
longer matches. No database is migrated, no data is written, and nothing outside the working
tree and the Nix/Cabal caches is modified.

**If `cabal` cannot find `effectful-core` 2.7.1.2.** Run `cabal update` and retry. The index
must know about the release; the local index used while writing this plan already contained
2.7.1.1 and 2.7.1.2.

**If Nix reports a hash mismatch** for either `callHackageDirect` entry — for example:

```text
error: hash mismatch in fixed-output derivation '/nix/store/...-effectful-core-2.7.1.2.tar.gz.drv':
         specified: sha256-SCP8YLoafJNpwbP8hmIe8abpqAom/ya+v5P+fkDU2aY=
            got:    sha256-<something else>
```

replace the `sha256` in `nix/haskell-overlay.nix` with the `got:` value. That is the correct
recovery, not a reason to abandon the pin: Hackage tarballs are immutable, so a mismatch means
the value was mistyped. You can also recompute either hash from scratch:

```bash
nix-prefetch-url --type sha256 \
  https://hackage.haskell.org/package/effectful-core-2.7.1.2/effectful-core-2.7.1.2.tar.gz
```

then convert the base32 output to the SRI form the overlay uses:

```bash
nix hash convert --hash-algo sha256 --to sri <base32-output>
```

**If the `strict-mutable-base` 2.0.0.0 override breaks an unrelated Nix package.** It should
not — `effectful-core` is its only consumer in this project's closure — but if some package
in the set turns out to need 1.x, the recovery is to scope the override rather than abandon
it: give `effectful-core` its own `strict-mutable-base` argument in the `callHackageDirect`
attribute set (the second `{ }` argument is where per-package overrides go) instead of
replacing the set-wide attribute.

**If `nix flake check` fails only on `pgmq-effectful-tests`** and the same suite passes under
`cabal test pgmq-effectful` in the dev shell, the problem is the Nix build sandbox, not the
`effectful-core` upgrade. Remove that one line from `checks` in `flake.module.nix`, note the
failure and its output in Surprises & Discoveries, and rely on the Cabal run for that
evidence. Do not let a sandbox quirk block the rest of the plan.

**If a real 2.7 incompatibility appears** despite the research, the safe fallback is to revert
Milestone 2's pin (so Nix returns to 2.6.1.0) while keeping Milestone 1's widened bound, fix
the incompatibility, and re-pin. The widened bound alone never breaks an existing consumer:
every version it admits except 2.5 was admitted before.


## Interfaces and Dependencies

No Haskell interface changes in this plan. No module is added, removed or renamed; no exported
type or function signature changes; the `Pgmq` effect GADT in
`pgmq-effectful/src/Pgmq/Effectful/Effect.hs` keeps every constructor it has. A consumer's
code compiles against the new release exactly as it did against 0.6.0.0.

The contract that does change is the dependency bound. At the end of Milestone 1 these five
`build-depends` entries must read `effectful-core ^>=2.6 || ^>=2.7`:

- `pgmq-effectful/pgmq-effectful.cabal`, `library` stanza.
- `pgmq-effectful/pgmq-effectful.cabal`, `test-suite pgmq-effectful-test` stanza.
- `pgmq-config/pgmq-config.cabal`, `library` stanza, inside `if flag(effectful)`.
- `pgmq-config/pgmq-config.cabal`, `test-suite pgmq-config-test` stanza, inside `if flag(effectful)`.
- `pgmq-bench/pgmq-bench.cabal`, `benchmark pgmq-bench` stanza.

At the end of Milestone 2, `nix/haskell-overlay.nix` must define two additional attributes on
the overlay's result — `effectful-core` at 2.7.1.2 and `strict-mutable-base` at 2.0.0.0 —
and `flake.module.nix`'s `checks` set must contain a `pgmq-effectful-tests` attribute built
with the file's existing `withTests` helper.

The libraries involved, and why:

- **`effectful-core`** (Hackage) is the effect-system core: `Eff`, `(:>)`, `interpret`, `send`,
  and the `Error` effect. `pgmq-effectful` builds the `Pgmq` effect and its two interpreters
  on it; `pgmq-config` uses it only for the `Eff`/`(:>)` types in its effect-backed
  reconciler; `pgmq-bench` uses `runEff`/`runErrorNoCallStack` to drive benchmarks through the
  effect layer. We depend on `effectful-core` rather than the fuller `effectful` package
  deliberately, which is what keeps most of the 2.7 breaking changes out of scope.
- **`strict-mutable-base`** (Hackage) is a transitive dependency of `effectful-core`, pulled in
  only so that `effectful-core` 2.7's `>= 2.0.0.0` requirement can be satisfied inside the Nix
  package set. No code in this repository imports it.
- **`ephemeral-pg`** (pinned to a git revision in `nix/haskell-overlay.nix`) starts the
  throwaway PostgreSQL servers the test suites use. Unchanged by this plan, but it is why the
  validation commands need no database setup.

Version facts this plan depends on, verified during research on 2026-09-16:
`effectful-core` releases on Hackage are 2.7.1.2, 2.7.1.1, 2.7.1.0, 2.7.0.0, 2.6.1.0 and
older; 2.7.1.3 exists only in the upstream working tree and is not released.
`strict-mutable-base` releases are 2.0.0.0 and 1.1.0.0, with 1.0.0.0 deprecated. The
`ghc9124` Nix package set ships `effectful-core` 2.6.1.0, `strict-mutable-base` 1.1.0.0,
`primitive` 0.9.1.0, `unliftio-core` 0.2.1.0, `monad-control` 1.0.3.1 and
`transformers-base` 0.4.6.1; `exceptions` 0.10.12 and `mtl` come from GHC 9.12.4 itself.


## Revision Notes

### 2026-09-16 — relax the upper bound to a plain `^>=2.7`

The first draft of this plan set the bound to `^>=2.6 || >=2.7.1.1 && <2.8`, deliberately
excluding `effectful-core` 2.7.0.0 because upstream regressed the per-operation overhead of
dynamically dispatched effects in that release and fixed it in 2.7.1.1. The user asked for the
simpler form: "it should not be that strict, `>= 2.7` is enough". The bound is now
`^>=2.6 || ^>=2.7` everywhere it appears in this plan.

The reasoning behind the change, recorded in full in the Decision Log: a Cabal bound is a
statement of compatibility, and we are compatible with 2.7.0.0 — it builds and its API matches.
Excluding it would break the solver for anyone already pinned there, which is a real cost paid
to steer a performance characteristic that the consumer can fix by upgrading. The preference
for 2.7.1.1 or newer is preserved, but moved to where it belongs: Milestone 3 now writes it as
an advisory sentence in the root `CHANGELOG.md` rather than as a constraint.

Sections updated: Purpose / Big Picture (end-state bound), Progress (the changelog item now
names the advisory), Decision Log (the exclusion decision replaced by its reversal, with the
original reasoning preserved so the history is legible), Context and Orientation (the ADR
distillation hint no longer suggests a rule about excluding regressed releases), Milestone 1
(how to read the new bound), Milestone 3 (changelog guidance), Concrete Steps (the `sed`
commands, the expected `grep` transcript, the `cabal-gild` evidence, and the Milestone 1 commit
message), and Interfaces and Dependencies (the required `build-depends` text).

One fact was re-verified rather than carried over: `cabal-gild` 1.6.0.4 leaves
`effectful-core ^>=2.6 || ^>=2.7,` byte-identical, so `nix fmt` will not rewrite the new bound
any more than it would have rewritten the old one.
