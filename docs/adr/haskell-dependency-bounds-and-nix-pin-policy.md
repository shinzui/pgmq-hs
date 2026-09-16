# Haskell dependency bounds state compatibility; the Nix pin states what we test

## Status

Accepted, 2026-09-16. Introduced by
[ExecPlan 22](../plans/22-support-effectful-core-2-7-and-drop-2-5.md), which widened the
`effectful-core` bound to `^>=2.6 || ^>=2.7`. This repository has no profiled ADR bundle; this
record follows its existing plain-Markdown decision convention and introduces no OKF metadata.

## Context

Three packages declare an `effectful-core` dependency: `pgmq-effectful`, `pgmq-config` (behind
its `effectful` flag) and `pgmq-bench`. Before ExecPlan 22 they declared
`effectful-core ^>=2.5 || ^>=2.6`, which made two claims that pulled in opposite directions.

The 2.5 half was an untested promise. Nothing in this repository has ever built against 2.5:
the Cabal solver picks the newest allowed version, and the `ghc9124` Nix package set shipped
2.6.1.0. A declared bound that no build path exercises rots silently.

The 2.7 exclusion was the mirror problem. `effectful-core` 2.7 is API-compatible with our
usage — we import only `Eff`, `Effect`, `DispatchOf`, `(:>)`, `IOE`, `runEff`, `send`,
`interpret` and the `Error` effect, all unchanged — yet the bound made the solver reject any
consumer already pinned to 2.7.

A separate question arose within 2.7. Upstream increased the per-operation overhead of
dynamically dispatched effects in 2.7.0.0 and fixed it in 2.7.1.1. The `Pgmq` effect is
dynamically dispatched and every queue operation goes through `send`, so the regression lands
on our hot path. The tempting response was to write `>=2.7.1.1` and exclude 2.7.0.0.

## Decision

**A Cabal bound states what we are compatible with, not what we recommend.** We are compatible
with 2.7.0.0 — it builds and its API matches — so it stays inside the range. Excluding a
release to steer a performance characteristic breaks the solver for consumers already pinned
there, which is a real cost paid for advice they can act on themselves. Performance preferences
belong in the changelog and README as prose. The root `CHANGELOG.md` accordingly recommends
2.7.1.1 or newer without constraining it.

**A declared range must be exercised at both ends, by different build paths.** The Nix package
set pins the newer end (`nix/haskell-overlay.nix` overrides `effectful-core` to 2.7.1.2, and
`strict-mutable-base` to 2.0.0.0 because 2.7 requires `>= 2.0`), because `nix flake check` is
what gates a release and an unexercised claim is not a supported version. The older end is
defended by an explicit constrained Cabal build, `just effectful-floor`, which builds
everything against `effectful-core <2.7` in its own `dist-effectful-floor` build tree.

**Drop version claims we do not build.** 2.5 was removed rather than carried forward.

## Consequences

Widening a bound is safe for existing consumers: every version the new range admits except the
dropped 2.5 was admitted before. Narrowing one is not, and needs the same justification as a
breaking change.

Adding a supported version means three edits, not one: the bound, the Nix pin or the floor
recipe (whichever end moved), and a changelog entry. If `just effectful-floor` ever stops
passing, the correct response is to fix the incompatibility or raise the lower bound
deliberately — never to leave the bound claiming a version that no longer compiles.

The two build paths deliberately resolve different versions. `cabal build` and `nix build` both
land on 2.7.1.2 today, so `just effectful-floor` is the only thing keeping 2.6 honest; it is
not optional maintenance.

## Note: the effect layer's test suite does not run under Nix

`nix flake check` builds the `pgmq-effectful` library but not its test suite. Adding
`pgmq-effectful-tests` to `checks` in `flake.module.nix` fails at evaluation time:

```text
error: function 'anonymous lambda' called without required argument 'hs-opentelemetry-propagator-jaeger'
```

The suite depends on `hs-opentelemetry-sdk >=1.0`, whose cabal file requires six sibling
`hs-opentelemetry-*` packages at `==1.0.*` — `exporter-handle`, `exporter-otlp`, and the `b3`,
`datadog`, `jaeger` and `xray` propagators. `nix/haskell-overlay.nix` defines only `api`,
`api-types`, `semantic-conventions`, `propagator-w3c`, `exporter-in-memory` and `sdk` from the
pinned `iand675/hs-opentelemetry` tree, and the `ghc9124` set supplies four of the six missing
names only at 0.0.x/0.1.x. This is a pre-existing defect, reproducible at commit `8a704c3`; it
stayed latent because no `checks` entry had ever forced the SDK's evaluation.

Until someone adds those six `callCabal2nix` entries (all the directories exist in the pinned
tree; `exporter-otlp` additionally drags `proto-lens`), the effect layer's behavioural evidence
comes from `cabal test all`, not from `nix flake check`. Do not re-add the check without
fixing the overlay first.
