---
id: 8
slug: upgrade-hs-opentelemetry-to-1-0-0
title: "Upgrade hs-opentelemetry to 1.0.0"
kind: exec-plan
created_at: 2026-05-31T21:50:14Z
---

# Upgrade hs-opentelemetry to 1.0.0

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.


## Purpose / Big Picture

After this change, `pgmq-effectful` users can build and test against the `hs-opentelemetry` 1.0 package family while the traced interpreter continues to emit OpenTelemetry attributes that observability backends understand. The visible behavior is still a span for each PGMQ operation, but the attributes should be compatible with the latest semantic conventions available to this project: `hs-opentelemetry-semantic-conventions` 1.40.0.0 from the local `iand675/hs-opentelemetry` corpus, plus the OpenTelemetry 1.41.0 guidance verified on 2026-05-31.

The most important observable change is semantic-convention compatibility. The existing interpreter currently emits the older v1.24 keys `db.system`, `db.operation`, and `messaging.operation`. With the new `hs-opentelemetry` APIs, it should emit the stable database keys `db.system.name` and `db.operation.name` when `OTEL_SEMCONV_STABILITY_OPT_IN=database` or `database/dup` is selected, and it should emit the stable messaging keys `messaging.operation.name` and `messaging.operation.type` when `OTEL_SEMCONV_STABILITY_OPT_IN=messaging` or `messaging/dup` is selected. The default behavior should remain compatible with the old v1.24 attributes unless the implementation deliberately decides to make a major-version-only break.


## Progress

- [x] Milestone 1: Update dependency pins and version bounds to `hs-opentelemetry` 1.0 and the semantic-conventions package available in the local corpus. (2026-05-31)
- [x] Milestone 2: Migrate imports and API usage that changed between `hs-opentelemetry` 0.x and 1.0. (2026-05-31)
- [x] Milestone 3: Add semantic-convention stability handling for database and messaging attributes. (2026-05-31)
- [x] Milestone 4: Update tests so they prove old, stable, and duplicate semantic-convention modes. (2026-05-31)
- [x] Milestone 5: Update docs and changelogs, then run the build, tests, formatter, and Nix checks. (2026-05-31)


## Surprises & Discoveries

- 2026-05-31: `hs-opentelemetry` 1.0 changed configured propagators to the `TextMapPropagator` carrier. `Pgmq.Effectful.Telemetry` now converts between `Network.HTTP.Types.RequestHeaders` and `OpenTelemetry.Propagator.TextMap` at the pgmq JSON header boundary. Evidence: the first `cabal build lib:pgmq-effectful` failed with type errors expecting `TextMap`; the final `cabal test all` passed after adding the conversions.

- 2026-05-31: `OpenTelemetry.SemanticsConfig.getSemanticsOptions` is memoized, and process environment changes inside parallel tests can race. The traced interpreter uses `getSemanticsOptions'` so it observes the current environment for each operation, and the tests serialize semantic-convention environment changes with an `MVar`. Evidence: the first focused test run failed the duplicate-mode assertion because another test's environment was visible; after adding the lock, `cabal test pgmq-effectful` passed all 17 tests.

- 2026-05-31: Nixpkgs' available `thread-utils-context` package was too old for `hs-opentelemetry-api` 1.0 even though the Cabal version range allowed it. Evidence: `nix flake check` failed building `hs-opentelemetry-api-1.0.0.0` with missing `ensureRef`, `ensureRefFast`, and `lookupRefFast`. Overlaying `iand675/thread-utils` main for `thread-utils-context` 0.4.1.0 and `thread-utils-finalizers` fixed the check.


## Decision Log

- Decision: Use the local `mori`-registered `iand675/hs-opentelemetry` corpus as the API source of truth and use official OpenTelemetry docs only to verify the current semantic-conventions version and attribute stability rules.
  Rationale: The repository instructions require `mori` for dependency source and documentation lookup before guessing APIs. The local corpus at `/Users/shinzui/Keikaku/hub/haskell/hs-opentelemetry-project/hs-opentelemetry` contains `hs-opentelemetry-api` 1.0.0.0 and `hs-opentelemetry-semantic-conventions` 1.40.0.0 source. The OpenTelemetry website currently identifies semantic conventions 1.41.0 as the latest published spec and explains the stability opt-in rules.
  Date: 2026-05-31

- Decision: Preserve old v1.24 attributes by default and use `OTEL_SEMCONV_STABILITY_OPT_IN` to select stable or duplicate attributes.
  Rationale: The OpenTelemetry messaging and database migration notes say existing instrumentations using v1.24 or prior should not change defaults in the existing major version, should support `messaging`, `messaging/dup`, `database`, and `database/dup`, and may drop the compatibility environment variable in the next major version. `pgmq-effectful` is version 0.2.0.0, so keeping default compatibility is the lower-risk path.
  Date: 2026-05-31

- Decision: Treat `hs-opentelemetry-semantic-conventions` 1.40.0.0 as the latest Haskell semantic-conventions package unless implementation research finds a newer local or Hackage release before coding.
  Rationale: The local corpus package description says it is generated from OpenTelemetry semantic-conventions v1.40, while official docs show v1.41.0 as current. The plan must target what Haskell code can compile against, and it should explicitly verify whether v1.41 has a Haskell package before implementation.
  Date: 2026-05-31

- Decision: Pin `hs-opentelemetry` to upstream commit `46a42cdf80405fdb36fbb48a309254b2332617b4`, not the local-only corpus commit `8cd70c18c58d2ac9772e11713669130632bc1108`.
  Rationale: `git ls-remote` showed the local corpus commit was not directly advertised by GitHub, while `46a42cdf80405fdb36fbb48a309254b2332617b4` is the dereferenced upstream release commit shared by the 1.0 package tags and contains `hs-opentelemetry-semantic-conventions` 1.40.0.0.
  Date: 2026-05-31

- Decision: Keep `TraceHeaders` as `RequestHeaders` in the public pgmq API and adapt to `TextMap` internally.
  Rationale: `hs-opentelemetry` 1.0 propagators standardize on `TextMap`, but pgmq stores trace headers as JSON and the existing public API uses HTTP-style case-insensitive byte-string headers. Internal conversion preserves the public API while satisfying the 1.0 propagator interface.
  Date: 2026-05-31

- Decision: Overlay `thread-utils-context` and `thread-utils-finalizers` from `iand675/thread-utils` for Nix builds.
  Rationale: Cabal selected `thread-utils-context` 0.4.1.0, but the Nix package index used by this flake did not contain that Hackage version. The `hs-opentelemetry-api` 1.0 source uses functions present in the 0.4 series, so the overlay is required for `nix flake check`.
  Date: 2026-05-31


## Outcomes & Retrospective

Implemented on 2026-05-31. `pgmq-effectful` now builds against the `hs-opentelemetry` 1.0 package family and `hs-opentelemetry-semantic-conventions` 1.40.0.0. The traced interpreter keeps old v1.24-style `messaging.operation`, `db.system`, and `db.operation` attributes by default, emits stable `messaging.operation.name`, `messaging.operation.type`, `db.system.name`, and `db.operation.name` when `OTEL_SEMCONV_STABILITY_OPT_IN=messaging,database`, and emits both sets for `messaging/dup,database/dup`.

Validation completed:

```text
cabal build lib:pgmq-effectful --dry-run
cabal build lib:pgmq-effectful
cabal test pgmq-effectful
cabal build all
nix fmt
cabal test all
nix flake check
```

`nix flake check` passed on `aarch64-darwin` and reported that incompatible systems `aarch64-linux`, `x86_64-darwin`, and `x86_64-linux` were omitted, which is the normal flake output on this machine.


## Context and Orientation

This repository is a multi-package Haskell workspace for PGMQ, a PostgreSQL-backed message queue. `mori show --full` identifies the repository as `shinzui/pgmq-hs` with packages `pgmq-core`, `pgmq-hasql`, `pgmq-effectful`, `pgmq-migration`, `pgmq-config`, and `pgmq-bench`. The OpenTelemetry work lives in `pgmq-effectful`, which provides an Effectful effect interpreter over `pgmq-hasql`.

The relevant repository files are:

- `cabal.project`, which lists workspace packages, uses `ghc-9.12.2`, and currently pins `iand675/hs-opentelemetry` at commit `894c77f92ae2d429f342522f4cc6f90e9f883f3d` with subdirectories `api`, `propagators/w3c`, `semantic-conventions`, `sdk`, and `exporters/in-memory`.
- `pgmq-effectful/pgmq-effectful.cabal`, where the library currently bounds `hs-opentelemetry-api >=0.2 && <0.4` and `hs-opentelemetry-semantic-conventions >=0.1 && <0.2`; the test suite also depends on `hs-opentelemetry-exporter-in-memory >=0.0 && <0.1`, `hs-opentelemetry-propagator-w3c >=0.0 && <0.2`, `hs-opentelemetry-sdk >=0.0 && <0.2`, and semantic conventions `>=0.1 && <0.2`.
- `pgmq-effectful/src/Pgmq/Effectful/Telemetry.hs`, which handles trace-context propagation and re-exports typed semantic-convention `AttributeKey` values such as `messaging_operation`, `db_system`, and `db_operation`.
- `pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs`, which creates spans around each PGMQ operation. `operationAttributes` currently inserts `db_system`, `db_operation`, `messaging_system`, `messaging_operation`, `messaging_destination_name`, `messaging_message_id`, and `messaging_batch_messageCount`.
- `pgmq-effectful/test/TracedInterpreterSpec.hs`, which creates an in-memory tracer provider and asserts that publish and receive spans contain v1.24 attribute names.
- `nix/haskell-overlay.nix`, which currently overlays `hs-opentelemetry-semantic-conventions` and `hs-opentelemetry-exporter-in-memory` from an older `iand675/hs-opentelemetry` commit `adc464b0a45e56a983fa1441be6e432b50c29e0e`, with comments saying the revision targets OpenTelemetry v1.24.
- `README.md`, `CHANGELOG.md`, `pgmq-effectful/CHANGELOG.md`, and `docs/OPENTELEMETRY_INSTRUMENTATION.md`, which mention OpenTelemetry semantic conventions v1.24 and need to stop claiming v1.24 as current once the upgrade lands.

The local dependency source code and docs are available through `mori registry show iand675/hs-opentelemetry --full` and `mori registry docs iand675/hs-opentelemetry`. At `/Users/shinzui/Keikaku/hub/haskell/hs-opentelemetry-project/hs-opentelemetry`, the current local checkout is commit `8cd70c18c58d2ac9772e11713669130632bc1108` and contains clean source with `hs-opentelemetry-api` 1.0.0.0, `hs-opentelemetry-sdk` 1.0.0.0, `hs-opentelemetry-propagator-w3c` 1.0.0.0, `hs-opentelemetry-exporter-in-memory` 1.0.0.0, and `hs-opentelemetry-semantic-conventions` 1.40.0.0.

Important terms used in this plan:

An OpenTelemetry span is a timed unit of work in a trace. The traced interpreter creates spans such as `publish my_queue` and `receive my_queue`.

A semantic convention is a standard name and value type for telemetry attributes. For example, `messaging.system = "pgmq"` tells backends the span belongs to the PGMQ messaging system.

An `AttributeKey a` is a typed wrapper from `hs-opentelemetry` that records the canonical attribute name and the Haskell value type expected for that attribute. Use typed keys from `OpenTelemetry.SemanticConventions` instead of hand-typed strings when keys exist.

`OTEL_SEMCONV_STABILITY_OPT_IN` is an OpenTelemetry environment variable used during migrations from experimental to stable semantic conventions. In `hs-opentelemetry` 1.0, `OpenTelemetry.SemanticsConfig.lookupStability` returns `Old`, `Stable`, or `StableAndOld` for areas such as `database` and `messaging`.


## Plan of Work

Milestone 1 updates the dependency graph. Edit `cabal.project` so the `source-repository-package` for `https://github.com/iand675/hs-opentelemetry` points at a revision that actually contains the 1.0 package family. Start with the local corpus commit `8cd70c18c58d2ac9772e11713669130632bc1108`; if implementation discovers that commit is not reachable from GitHub, use the nearest upstream commit containing the same package versions and record the substitution in this plan. Keep the same subdirectories unless the 1.0 dependency solver requires adding `api-types`; `hs-opentelemetry-semantic-conventions` 1.40.0.0 depends on `hs-opentelemetry-api-types ^>= 1.0`, so include `api-types` if Cabal cannot otherwise solve it from Hackage. In `pgmq-effectful/pgmq-effectful.cabal`, change the library bounds to `hs-opentelemetry-api >=1.0 && <2` and `hs-opentelemetry-semantic-conventions >=1.40 && <2`. Change the test-suite bounds for `hs-opentelemetry-api`, `hs-opentelemetry-exporter-in-memory`, `hs-opentelemetry-propagator-w3c`, `hs-opentelemetry-sdk`, and `hs-opentelemetry-semantic-conventions` to the same 1.0 or 1.40 major ranges. In `nix/haskell-overlay.nix`, update the `hs-opentelemetry` fetch revisions and hashes for `semantic-conventions` and `exporters/in-memory`, update comments from v1.24 to v1.40 or later, and add overlays for any 1.0 subpackages that nixpkgs does not provide or marks broken.

Milestone 2 migrates code to the 1.0 API. Read the 1.0 source under `/Users/shinzui/Keikaku/hub/haskell/hs-opentelemetry-project/hs-opentelemetry/api/src` before changing imports. The likely important changes are that the semantic-conventions package now depends on `hs-opentelemetry-api-types`, `OpenTelemetry.Attributes.Map` exposes `insertAttributeByKey` in local examples, and 1.0 adds `OpenTelemetry.SemanticsConfig`. Replace imports and calls only where compilation requires it. Keep trace-context propagation behavior in `pgmq-effectful/src/Pgmq/Effectful/Telemetry.hs` unchanged: `injectTraceContext` and `extractTraceContext` should still use the tracer provider's configured propagator, not hard-code W3C.

Milestone 3 updates emitted attributes while preserving compatibility. In `pgmq-effectful/src/Pgmq/Effectful/Telemetry.hs`, re-export both old and stable keys needed by the interpreter: old keys `messaging_operation`, `db_system`, `db_operation`, and stable keys `messaging_operation_name`, `messaging_operation_type`, `db_system_name`, `db_operation_name`. Keep `messaging_system`, `messaging_destination_name`, `messaging_message_id`, and `messaging_batch_messageCount`, because those names still exist in the 1.40 generated module and are still relevant to messaging spans. In `pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs`, import `OpenTelemetry.SemanticsConfig (StabilityOpt (..), databaseOption, getSemanticsOptions, lookupStability)`. Change `operationAttributes` so it queries semantic options once per operation, then builds attributes according to these rules:

- `database` unset: emit old `db.system = "postgresql"` and `db.operation = info.opDbFunction`.
- `database`: emit stable `db.system.name = "postgresql"` and `db.operation.name = info.opDbFunction`.
- `database/dup`: emit both old and stable database keys.
- `messaging` unset: emit old `messaging.operation = "publish"` or `"receive"` when `opMessagingKind` is present.
- `messaging`: emit stable `messaging.operation.name = "publish"` or `"receive"` and `messaging.operation.type = "publish"` or `"receive"`.
- `messaging/dup`: emit old and stable messaging operation keys.

The operation values should remain low-cardinality strings. Do not record message bodies by default. For database spans, keep the current span names unless tests or OpenTelemetry guidance require a change; the database docs say database span names should use `db.operation.name` and a target when a low-cardinality operation name is available, and `pgmq.<fn> <queue>` already matches the existing lifecycle span pattern.

Milestone 4 strengthens tests. Extend `pgmq-effectful/test/TracedInterpreterSpec.hs` to assert all three semantic-convention modes without relying on global memoization order. `OpenTelemetry.SemanticsConfig.getSemanticsOptions` is memoized, so tests that mutate `OTEL_SEMCONV_STABILITY_OPT_IN` in the same process can be brittle. Prefer adding a small pure helper in `Pgmq.Effectful.Interpreter.Traced` or an internal test-only helper that accepts `StabilityOpt` values directly, then test the attribute builder without changing the process environment. Keep the existing integration tests that run real PGMQ operations through the traced interpreter, but update their expectations to the default old mode. Add focused unit tests for:

- default old mode emits `messaging.operation`, `db.system`, and `db.operation`, and does not emit stable replacements;
- stable messaging and database mode emits `messaging.operation.name`, `messaging.operation.type`, `db.system.name`, and `db.operation.name`, and does not emit old replacements;
- duplicate mode emits both old and stable keys;
- publish and receive spans still have `Producer` and `Consumer` span kinds and the same trace-context propagation behavior.

Milestone 5 updates documentation and validates. In `README.md`, replace the claim that `runPgmqTraced` emits Semantic Conventions v1.24 with a sentence explaining default compatibility and the `OTEL_SEMCONV_STABILITY_OPT_IN=messaging,database` path for stable attributes. In `pgmq-effectful/CHANGELOG.md` and root `CHANGELOG.md`, add an unreleased entry for the dependency upgrade and semantic-conventions compatibility behavior. In `docs/OPENTELEMETRY_INSTRUMENTATION.md`, either update the stale dependency plan or add a note that the checked-in implementation now targets `hs-opentelemetry` 1.0 and newer semantic conventions. Then run formatting, Cabal builds/tests, and Nix checks listed below. If a command cannot complete because the environment lacks PostgreSQL or network access, record the exact failure in this plan and run the narrower command that still proves the edited code compiles.


## Concrete Steps

Start from the repository root:

```bash
cd /Users/shinzui/Keikaku/bokuno/libraries/pgmq-hs-project/pgmq-hs
```

Confirm the project identity and dependency source locations:

```bash
mori show --full
mori registry show iand675/hs-opentelemetry --full
mori registry docs iand675/hs-opentelemetry
```

Expected evidence includes `pgmq-effectful` in this repository and the dependency path `/Users/shinzui/Keikaku/hub/haskell/hs-opentelemetry-project`.

Inspect the 1.0 package versions and relevant APIs:

```bash
sed -n '1,80p' /Users/shinzui/Keikaku/hub/haskell/hs-opentelemetry-project/hs-opentelemetry/api/hs-opentelemetry-api.cabal
sed -n '1,80p' /Users/shinzui/Keikaku/hub/haskell/hs-opentelemetry-project/hs-opentelemetry/semantic-conventions/hs-opentelemetry-semantic-conventions.cabal
sed -n '1,190p' /Users/shinzui/Keikaku/hub/haskell/hs-opentelemetry-project/hs-opentelemetry/api/src/OpenTelemetry/SemanticsConfig.hs
rg -n "db_system_name|db_operation_name|messaging_operation_name|messaging_operation_type" /Users/shinzui/Keikaku/hub/haskell/hs-opentelemetry-project/hs-opentelemetry/semantic-conventions/src/OpenTelemetry/SemanticConventions.hs
```

Expected evidence includes `version: 1.0.0.0` for `hs-opentelemetry-api`, `version: 1.40.0.0` for `hs-opentelemetry-semantic-conventions`, and exported keys `db_system_name`, `db_operation_name`, `messaging_operation_name`, and `messaging_operation_type`.

Apply dependency edits in `cabal.project`, `pgmq-effectful/pgmq-effectful.cabal`, and `nix/haskell-overlay.nix`. Then run Cabal's solver before deeper code edits:

```bash
cabal build lib:pgmq-effectful --dry-run
```

If Cabal reports a missing `hs-opentelemetry-api-types` package, add the `api-types` subdir to the `source-repository-package` block and rerun the dry run. The implemented solution includes `api-types`.

After code edits, run the focused package tests:

```bash
cabal test pgmq-effectful
```

The expected result is a successful `pgmq-effectful-test` run. The trace tests should include cases proving old, stable, and duplicate semantic-convention modes.

Run broader validation:

```bash
cabal build all
cabal test all
nix fmt
nix flake check
```

If `nix flake check` needs new fetch hashes, run the failing Nix command once to get the expected hash, update `nix/haskell-overlay.nix`, and rerun `nix flake check`.

Commit the implementation after validation. The commit message must be Conventional Commits and include this trailer:

```text
ExecPlan: docs/plans/8-upgrade-hs-opentelemetry-to-1-0-0.md
```


## Validation and Acceptance

The implementation is accepted when `pgmq-effectful` builds against `hs-opentelemetry-api` 1.0.0.0 and the test suite proves the semantic-convention behavior with real spans.

In default mode, a publish operation should still produce a `Producer` span named like `publish publish_attrs_123456`, with attributes including:

```text
messaging.system = pgmq
messaging.operation = publish
messaging.destination.name = publish_attrs_123456
db.system = postgresql
db.operation = pgmq.send
```

In stable mode, represented by `database` and `messaging` stability options, a publish operation should produce stable replacements:

```text
messaging.system = pgmq
messaging.operation.name = publish
messaging.operation.type = publish
messaging.destination.name = publish_attrs_123456
db.system.name = postgresql
db.operation.name = pgmq.send
```

In duplicate mode, represented by `database/dup` and `messaging/dup`, the same span should contain both old and stable keys. Receive operations should mirror the same behavior with `receive` and `pgmq.read`. Trace propagation remains accepted only if the existing W3C traceparent round-trip test still passes.

The final validation commands are:

```bash
cabal test pgmq-effectful
cabal build all
cabal test all
nix flake check
```

If full `cabal test all` or `nix flake check` cannot run in the current environment, the plan implementer must record the blocker and at minimum provide successful `cabal build lib:pgmq-effectful` and `cabal test pgmq-effectful` output or the exact compile/test failure that remains.


## Idempotence and Recovery

All source edits in this plan are ordinary text edits and are safe to repeat. Re-running Cabal build and test commands is safe. Re-running Nix checks is safe; if Nix fetch hashes are wrong, Nix will print the expected hash without modifying source files.

Do not delete build directories as a first response to failures. If Cabal's solver appears stale after dependency edits, run:

```bash
cabal update
cabal build lib:pgmq-effectful --dry-run
```

If a dependency revision is unavailable from GitHub, do not guess a new API. Use `mori registry show iand675/hs-opentelemetry --full` and the local Git history under `/Users/shinzui/Keikaku/hub/haskell/hs-opentelemetry-project/hs-opentelemetry` to find a reachable revision with the required package versions, then update this plan's Decision Log with the chosen revision and reason.

If tests involving `OTEL_SEMCONV_STABILITY_OPT_IN` fail inconsistently, suspect `OpenTelemetry.SemanticsConfig.getSemanticsOptions` memoization. Prefer testing pure attribute-building helpers with explicit `StabilityOpt` arguments rather than relying on mutating the environment inside a long-lived test process.


## Interfaces and Dependencies

The implementation depends on the following Haskell modules from the `hs-opentelemetry` 1.0 family:

- `OpenTelemetry.Trace.Core` from `hs-opentelemetry-api` 1.0.0.0 for `Tracer`, `Span`, `SpanKind`, `inSpan'`, `defaultSpanArguments`, `addAttributesToSpanArguments`, `recordException`, and span status operations.
- `OpenTelemetry.Attributes.Map` from `hs-opentelemetry-api` 1.0.0.0 for inserting typed attributes. Use whichever of `insertByKey` or `insertAttributeByKey` exists in the 1.0 API after reading the local source; do not hand-type semantic attribute names where generated keys exist.
- `OpenTelemetry.SemanticsConfig` from `hs-opentelemetry-api` 1.0.0.0 for `StabilityOpt (Old, Stable, StableAndOld)`, `getSemanticsOptions`, `lookupStability`, and `databaseOption`.
- `OpenTelemetry.SemanticConventions` from `hs-opentelemetry-semantic-conventions` 1.40.0.0 for `messaging_system`, `messaging_destination_name`, `messaging_message_id`, `messaging_batch_messageCount`, `messaging_operation`, `messaging_operation_name`, `messaging_operation_type`, `db_system`, `db_system_name`, `db_operation`, and `db_operation_name`.
- `OpenTelemetry.Exporter.InMemory.Span`, `OpenTelemetry.Trace.Id.Generator.Default`, and `OpenTelemetry.Propagator.W3CTraceContext` from the 1.0 test dependencies for `TracedInterpreterSpec`.

At the end of the work, `Pgmq.Effectful.Telemetry` must still export trace-context helpers:

```haskell
injectTraceContext :: MonadIO m => TracerProvider -> Ctxt.Context -> m TraceHeaders
extractTraceContext :: MonadIO m => TracerProvider -> TraceHeaders -> Ctxt.Context -> m Ctxt.Context
traceHeadersToJson :: TraceHeaders -> Value
jsonToTraceHeaders :: Value -> TraceHeaders
mergeTraceHeaders :: TraceHeaders -> Maybe Value -> Value
```

At the end of the work, `Pgmq.Effectful.Interpreter.Traced` must still export:

```haskell
runPgmqTraced :: (IOE :> es, Error PgmqRuntimeError :> es) => Pool -> OTel.Tracer -> Eff (Pgmq : es) a -> Eff es a
runPgmqTracedWith :: (IOE :> es, Error PgmqRuntimeError :> es) => Pool -> TracingConfig -> Eff (Pgmq : es) a -> Eff es a
defaultTracingConfig :: OTel.Tracer -> TracingConfig
```

Do not change public PGMQ operation types to complete this upgrade. The behavior change belongs inside tracing dependency compatibility and emitted span attributes.
