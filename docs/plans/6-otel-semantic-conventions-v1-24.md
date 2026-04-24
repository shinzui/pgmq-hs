# Align pgmq-effectful OpenTelemetry instrumentation with v1.24 semantic conventions

Intention: intention_01kh0aq8xhednarw44bptt8xqm

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

This document is maintained in accordance with `.claude/skills/exec-plan/PLANS.md`.


## Purpose / Big Picture

After this change, anyone using `pgmq-effectful`'s OpenTelemetry-instrumented
interpreter will get spans whose attribute names and span-name format match the
OpenTelemetry **Semantic Conventions v1.24** specification for messaging and
database clients. This is the version baked into the
`hs-opentelemetry-semantic-conventions` package (0.1.0.0) shipped with the
`hs-opentelemetry` corpus.

Today, `pgmq-effectful/src/Pgmq/Effectful/Telemetry.hs` declares attribute
names as hand-written `Text` constants, some of which drift from the v1.24
spec (for example, `messaging.operation.type` and `db.operation.name`, which
are names introduced in later revisions and not in the v1.24 YAML). The
traced interpreter in `pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs`
does not emit the v1.24-required `messaging.operation` attribute at all, and
its span names (for example `"pgmq send"`) do not use the
`<operation_name> <destination_name>` form that v1.24 specifies for messaging
spans. Error handling uses a hand-built `exception` event rather than the
`recordException` helper the API already provides, and W3C context
propagation is wired directly against the W3C encoder instead of the tracer
provider's configurable propagator.

The user-visible improvement is concrete: after this change, a pgmq publisher
that wraps its send in the traced interpreter will emit a span named
`publish my-queue` carrying `messaging.system=pgmq`, `messaging.operation=publish`,
`messaging.destination.name=my-queue`, and `db.system=postgresql`, and any
downstream consumer running against an OTel backend (Jaeger, Honeycomb,
Tempo, etc.) will see a trace that groups, filters, and visualises
correctly against the standard messaging dashboards those backends ship.
Exceptions from pgmq operations will appear as proper `exception` span events
with `exception.stacktrace` alongside `exception.type` and
`exception.message`, and the span status will be `ERROR`.

You can see it working end-to-end by running the new tests in
`pgmq-effectful/test/TracedInterpreterSpec.hs`, which install an in-memory
exporter and assert on the exact attribute names, values, and span-name shape.


## Progress

Progress is a granular list of actual work. Every stopping point must appear
here, with a timestamp when checked off. If a step is partly done, split it
into a "done" row and a remaining row rather than overwriting.

- [x] Milestone 1: Adopt typed AttributeKeys from `hs-opentelemetry-semantic-conventions`. (2026-04-23)
    - [x] Added `hs-opentelemetry-semantic-conventions` to
      `pgmq-effectful/pgmq-effectful.cabal` library `build-depends`, to the
      project-wide `cabal.project` as a `source-repository-package` on the
      same iand675 revision the api/w3c packages already use, and to
      `nix/haskell-overlay.nix` (not in nixpkgs). Test-suite bump lands with
      Milestone 5 where the tests actually use it. (2026-04-23)
    - [x] Rewrote `pgmq-effectful/src/Pgmq/Effectful/Telemetry.hs` to
      re-export typed `AttributeKey` values (`messaging_system`,
      `messaging_operation`, `messaging_destination_name`,
      `messaging_message_id`, `messaging_batch_messageCount`, `db_system`,
      `db_operation`) from `OpenTelemetry.SemanticConventions`. Dropped the
      string constant block and the `messagingRoutingKey` alias entirely.
      (2026-04-23)
    - [x] Updated `pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs`
      to call `OTel.addAttribute` with `unkey <typedKey>` values instead of
      hand-written strings (minimal rename per plan; AttributeMap-at-start
      refactor lands in Milestone 2). (2026-04-23)
    - [x] Removed the incorrect-for-v1.24 names. `messaging.operation.type`
      and `db.operation.name` are gone (replaced by `messaging.operation` /
      `db.operation` via the typed keys); `messaging.destination.routing_key`
      is gone. Topic-send operations now emit the routing key as
      `messaging.destination.name` via new `withTracedTopicSend` /
      `withTracedTopicSendWithCount` helpers; `ValidateRoutingKey`,
      `ValidateTopicPattern`, and `TestRouting` now route through
      `withTracedSessionNoQueue` (no routing-key attribute). (2026-04-23)
- [x] Milestone 2: Conform to v1.24 required attributes and span-name form. (2026-04-23)
    - [x] Populated `messaging_operation` on every messaging span:
      @"publish"@ for every @send*@ / @send_topic*@ variant, @"receive"@
      for every @read*@ and @pop@ variant. Lifecycle and observability
      ops do not carry a @messaging.operation@ attribute (Decision Log
      entry confirmed). (2026-04-23)
    - [x] Populated `db_operation` on every span with the pgmq SQL
      function name — @"pgmq.send"@, @"pgmq.send_batch"@, @"pgmq.read"@,
      @"pgmq.read_with_poll"@, @"pgmq.pop"@, @"pgmq.read_grouped"@,
      @"pgmq.read_grouped_rr"@, @"pgmq.archive"@, @"pgmq.delete"@,
      @"pgmq.set_vt"@, @"pgmq.purge_queue"@, @"pgmq.create"@,
      @"pgmq.create_partitioned"@, @"pgmq.create_unlogged"@,
      @"pgmq.drop_queue"@, @"pgmq.bind_topic"@, @"pgmq.unbind_topic"@,
      @"pgmq.send_topic"@, @"pgmq.send_batch_topic"@,
      @"pgmq.list_queues"@, @"pgmq.metrics"@, @"pgmq.metrics_all"@, …
      (see `operationInfo` dispatch in
      `pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs`).
      (2026-04-23)
    - [x] Span names now follow
      @"<operation_name> <destination_name>"@: for example
      @"publish my-queue"@ / @"receive my-queue"@ for messaging spans,
      @"pgmq.archive my-queue"@ / @"pgmq.set_vt my-queue"@ for
      single-queue lifecycle spans, and @"pgmq.list_queues"@ /
      @"pgmq.metrics_all"@ / @"pgmq.validate_routing_key"@ for spans
      without a destination. (2026-04-23)
    - [x] Span `kind` aligned with v1.24 guidance. Notable behaviour
      change: Queue Management (`CreateQueue`, `DropQueue`,
      `CreatePartitionedQueue`, `CreateUnloggedQueue`) previously
      emitted Producer spans. They are now Internal, per v1.24 — queue
      creation is not a message publish. Message send variants stay
      Producer; reads and @pop@ stay Consumer; everything else Internal.
      (2026-04-23)
    - [x] Refactor: the traced interpreter now computes one `OpInfo`
      per dispatch and builds an @AttributeMap@ once via
      `operationAttributes`, then layers it into 'SpanArguments' via
      `addAttributesToSpanArguments` (matching the Kafka reference
      idiom). Replaces the prior per-operation helper-function fleet
      (`withTracedSession*`) with a single `withTracedOp`. (2026-04-23)
- [x] Milestone 3: Pluggable propagator and real context linking. (2026-04-23)
    - [x] Replaced direct `OpenTelemetry.Propagator.W3CTraceContext`
      calls in `pgmq-effectful/src/Pgmq/Effectful/Telemetry.hs` with
      `OpenTelemetry.Propagator.inject` / `extract` keyed on the tracer
      provider's configured propagator via
      `getTracerProviderPropagators`. (2026-04-23)
    - [x] Reworked `pgmq-effectful/src/Pgmq/Effectful/Traced.hs`.
      `sendMessageTraced` and `readMessageWithContext` now take a
      `TracerProvider` (not a `Tracer`), fetch its propagator, and
      inject/extract through it. W3C is the default (the SDK installs
      it); B3 / Datadog users get correct behaviour by configuring
      their propagator on the provider. (2026-04-23)
    - [x] `readMessageWithContext` now returns
      @Vector (Message, OTel.Context)@. The 'OTel.Context' is the
      propagator-extracted context, ready to be passed to
      `OpenTelemetry.Context.ThreadLocal.attachContext` or to an
      @inSpan''@-shaped helper when the caller opens its @process@
      span. The previous @Maybe SpanContext@ shape is gone — callers
      that specifically need the raw 'SpanContext' can recover it via
      `OpenTelemetry.Context.lookupSpan >>= OTel.getSpanContext`.
      (2026-04-23)
    - [x] Swapped the `TraceHeaders` carrier from
      @[(ByteString, ByteString)]@ to the ecosystem-standard
      @Network.HTTP.Types.RequestHeaders@ (case-insensitive header
      names). Propagators emit and consume this shape natively, so we
      no longer convert between two carrier types. Added `http-types`
      and `case-insensitive` to the library `build-depends` and
      dropped the now-unused `hs-opentelemetry-propagator-w3c`.
      (2026-04-23)
    - [x] Added `traceHeadersToJson` / `jsonToTraceHeaders` for the
      pgmq-over-jsonb serialization step, since pgmq stores headers as
      a JSON object rather than as HTTP headers. (2026-04-23)
- [ ] Milestone 4: Error recording via `recordException` and span status.
    - [ ] In `Pgmq.Effectful.Interpreter.Traced`, replace the hand-rolled
      `exception` event with `OpenTelemetry.Trace.Core.recordException` for
      `Hasql.Pool.UsageError` failures.
    - [ ] Ensure span status is set to `Error` with a short, non-PII
      description (`T.pack $ show (classifyError err)`) and to `Ok` on
      success.
- [ ] Milestone 5: Tests exercising the new conventions.
    - [ ] Add an in-memory span exporter to
      `pgmq-effectful/test/TracedInterpreterSpec.hs` (depends on
      `hs-opentelemetry-sdk` and `hs-opentelemetry-exporter-in-memory` via
      the test-suite only).
    - [ ] Add tests that assert, for a successful publish and a successful
      receive, the expected span name, span kind, and the presence of
      v1.24-required attributes with correct values.
    - [ ] Add a test that asserts error path emits an `exception` event via
      `recordException` and sets span status to `Error`.
- [ ] Milestone 6: Documentation and changelog.
    - [ ] Update the `Pgmq.Effectful.Telemetry` module haddock to link to the
      semantic-conventions guide and list the attributes emitted per span
      kind.
    - [ ] Add a bullet to `pgmq-effectful/CHANGELOG.md` for the next version
      describing the attribute renames as a breaking observability change.


## Surprises & Discoveries

Empty until implementation uncovers something.

- 2026-04-23: `hs-opentelemetry-semantic-conventions` is not currently in
  nixpkgs' ghc9122 haskell set (verified via `nix eval`), so the nix
  overlay in `nix/haskell-overlay.nix` had to add a manual
  `callCabal2nix` pointing at the same iand675/hs-opentelemetry commit
  the `cabal.project` already pins for api and propagator-w3c
  (`adc464b0...`). The cabal build picks the package up directly via
  `source-repository-package` with `subdir: semantic-conventions`.
- 2026-04-23: `nix build .#pgmq-effectful` fails on both master and this
  branch with `mkdir: cannot create directory '...-pgmq-migration-src/vendor':
  File exists`. The failure is inside the `pgmq-migration` source-combining
  derivation, unrelated to this plan's changes. Left as-is; cabal builds
  cleanly and is the documented build for the project (see `flake.nix`
  `devShells.default`).
- 2026-04-23: `OpenTelemetry.SemanticConventions.messaging_batch_messageCount`
  is typed `AttributeKey Int64` (generator verbatim — note the mixed snake/
  camel casing mirroring the spec's dotted name `messaging.batch.message_count`),
  `messaging_message_id :: AttributeKey Text`, and `db_operation /
  messaging_operation / messaging_system / messaging_destination_name` are
  all `AttributeKey Text`. No surprises, but worth recording because the
  phantom types determine what `ToAttribute` instance fires at call sites.


## Decision Log

Record every decision made while working on the plan. Include date, rationale,
and the affected files or sections when relevant.

- Decision: Target OpenTelemetry semantic-conventions **v1.24**, not a newer
  revision.
  Rationale: The `hs-opentelemetry-semantic-conventions` package shipped with
  the `iand675/hs-opentelemetry` corpus (see
  `/Users/shinzui/Keikaku/hub/haskell/hs-opentelemetry-project/hs-opentelemetry/semantic-conventions/hs-opentelemetry-semantic-conventions.cabal`)
  pins to v1.24. Using the library's typed `AttributeKey` values means any
  attribute we touch is guaranteed to be a v1.24 name; chasing a newer spec
  would require either hand-typing strings (what we are leaving behind) or
  regenerating the module, which is out of scope for this plan.
  Date: 2026-04-23.

- Decision: Map pgmq operations onto the v1.24 `messaging.operation`
  enumeration as follows.
  - `send`, `send_batch`, `send_topic`, `send_batch_topic` → `publish`.
  - `read`, `read_with_poll`, `pop`, `read_grouped*` → `receive`.
  - There is no pgmq operation that inherently represents the `process`
    phase; consumer applications that process a received message should open
    their own `process` span using `readMessageWithContext`'s extracted
    parent context. The instrumentation will not emit `process` itself.
  Rationale: v1.24 defines three operation values for messaging client
  instrumentation (publish, receive, process). `publish` and `receive` both
  happen inside pgmq's client-side calls; `process` is a step the consumer
  does outside the client call, so the consumer is the right place for it.
  Date: 2026-04-23.

- Decision: Keep lifecycle and observability operations (delete, archive,
  `set_vt`, `list_queues`, `metrics`, ...) as **internal** spans without a
  `messaging.operation` attribute (since none of `publish`/`receive`/`process`
  applies) but with `db.system=postgresql` and `db.operation=<pgmq.fn>`.
  Rationale: The v1.24 messaging convention only has vocabulary for
  publish/receive/process. Forcing a value onto a lifecycle op like `archive`
  would be lying. Treating it as a plain database call is honest.
  Date: 2026-04-23.

- Decision: Drop the ad-hoc attribute `messaging.destination.routing_key`
  used by the current `addRoutingKeyAttribute` helper. Instead, emit topic
  routing keys as `messaging.destination.name` (the spec's conditionally
  required destination name) for topic `send` operations, and do not emit
  any routing-key attribute for queue `send` operations.
  Rationale: v1.24 has no generic `messaging.destination.routing_key`
  attribute. It has `messaging.rabbitmq.destination.routing_key` (RabbitMQ
  specific), but pgmq is not RabbitMQ, so using that name would mislead
  backends that specialise dashboards on it. For pgmq topics, the routing
  key *is* the logical destination, so using `messaging.destination.name`
  for it is consistent with how the spec treats destinations.
  Date: 2026-04-23.

- Decision: Switch from direct `OpenTelemetry.Propagator.W3CTraceContext`
  calls to the tracer provider's configured propagator
  (`OpenTelemetry.Propagator.inject` / `extract`).
  Rationale: Hard-coding W3C prevents users from swapping in B3 or Datadog
  propagators. The tracer provider already carries a propagator; respecting
  it matches what
  `/Users/shinzui/Keikaku/hub/haskell/hs-opentelemetry-project/hs-opentelemetry/instrumentation/hw-kafka-client/src/OpenTelemetry/Instrumentation/Kafka.hs`
  does for Kafka.
  Date: 2026-04-23.

- Decision: Queue Management operations (`CreateQueue`, `DropQueue`,
  `CreatePartitionedQueue`, `CreateUnloggedQueue`) move from
  `OTel.Producer` span kind to `OTel.Internal`.
  Rationale: The v1.24 convention reserves `Producer` for message
  publishes. Creating, dropping, or altering a queue is administrative,
  not a publish, and should not be counted by backends' producer-throughput
  panels. This is a behaviour change visible to anyone filtering by span
  kind; called out in the CHANGELOG entry (Milestone 6).
  Date: 2026-04-23.

- Decision: Use typed `OpenTelemetry.Attributes.Map.insertByKey` (type-safe
  variant that accepts the value directly via 'ToAttribute') rather than
  `insertAttributeByKey` (which takes a pre-wrapped 'Attribute').
  Rationale: Values flow through the 'AttributeKey a' phantom type, which
  catches accidental type mismatches at compile time. For example,
  `messaging_batch_messageCount :: AttributeKey Int64` refuses an `Int`
  and forces a conversion — surfaced as a compile-time error during M2
  and fixed by using `fromIntegral n :: Int64`.
  Date: 2026-04-23.


## Outcomes & Retrospective

Fill in as milestones complete. Compare actual behaviour against the Purpose
section's description, and record any deltas.

(To be filled during and after implementation.)


## Context and Orientation

This section describes the repository as if the reader has never opened it
before. All paths are repository-relative to the pgmq-hs root
(`/Users/shinzui/Keikaku/bokuno/libraries/pgmq-hs-project/pgmq-hs/`).

### Repository layout

This is a multi-package Haskell project built with cabal. The top-level
`cabal.project` lists every package. The packages relevant to this plan are:

- `pgmq-core/` — shared domain types (`QueueName`, `MessageId`, `Message`,
  `RoutingKey`, and so on), used by every other package.
- `pgmq-hasql/` — a Hasql-based implementation of the pgmq SQL surface.
  Every SQL call goes through this package, regardless of whether the
  consumer uses the effectful interface or raw Hasql sessions.
- `pgmq-effectful/` — the `effectful` effect definition and interpreters.
  This is the package the plan edits.

Inside `pgmq-effectful/` the files that matter are:

- `pgmq-effectful.cabal` — declares dependencies and exposed modules. We
  must add `hs-opentelemetry-semantic-conventions` here.
- `src/Pgmq/Effectful/Effect.hs` — defines the `Pgmq` effect as a GADT with
  one constructor per pgmq operation (send, read, delete, archive, ...) and
  a helper function per constructor. No change expected here.
- `src/Pgmq/Effectful/Interpreter.hs` — the plain, non-traced interpreter.
  No change expected here.
- `src/Pgmq/Effectful/Interpreter/Traced.hs` — the OpenTelemetry-instrumented
  interpreter. Uses `OTel.inSpan'` to wrap each operation, calls helper
  functions `addQueueAttributes`, `addBaseAttributes`, `addMessageIdAttribute`,
  `addBatchCountAttribute`, `addRoutingKeyAttribute`. We rewrite these.
- `src/Pgmq/Effectful/Telemetry.hs` — today defines `Text` constants for
  attribute names and helpers for W3C trace-context propagation. We rewrite
  the attribute-name half to use typed `AttributeKey` imports and rework
  the propagation half to use the pluggable propagator.
- `src/Pgmq/Effectful/Traced.hs` — higher-level helpers `sendMessageTraced`
  and `readMessageWithContext`. We rework these to use the pluggable
  propagator.
- `test/TracedInterpreterSpec.hs` — currently tests error propagation with
  a no-op tracer. We expand this to install an in-memory span exporter and
  assert on span content.

### Terms of art

- **Span**: the OpenTelemetry unit of work. Every pgmq operation wrapped by
  the traced interpreter produces one span.
- **Span kind**: a tag on the span saying whether it represents a message
  `Producer` (publish), a `Consumer` (receive), an outgoing/internal call
  (`Internal`), and so on.
- **Attribute**: a key-value pair attached to a span. The OpenTelemetry spec
  fixes standard attribute names (like `messaging.system`) in the
  *semantic-conventions* document so dashboards across backends understand
  them.
- **`AttributeKey a`**: a newtype `AttributeKey { unkey :: Text }` defined in
  `hs-opentelemetry-api`. The type parameter `a` records the expected
  Haskell value type (`Text`, `Int64`, `Bool`, ...). The
  `hs-opentelemetry-semantic-conventions` package exports one
  `AttributeKey` per attribute in the spec, so instrumentation code does not
  hand-type the dotted string at every call site.
- **Tracer provider / tracer**: factory objects in the OpenTelemetry API; a
  `Tracer` creates spans. The user owns the tracer provider and passes a
  tracer into `runPgmqTraced`.
- **Propagator**: the component that encodes/decodes trace context into
  transport-specific headers. The three common ones are W3C Trace Context
  (the default), B3, and Datadog. The tracer provider carries the user's
  configured propagator; instrumentation should respect that rather than
  hard-wiring W3C.
- **`messaging.operation`** (v1.24): a required attribute on messaging
  spans taking one of `publish`, `receive`, `process`. See the Decision Log
  for the pgmq mapping.
- **`db.system` / `db.operation`** (v1.24): the required and recommended
  attributes identifying the database and the SQL operation. For pgmq the
  values are `postgresql` and the pgmq SQL function name
  (`pgmq.send`, `pgmq.read`, ...).
- **`recordException`**: `OpenTelemetry.Trace.Core.recordException s attrs ts
  e` adds a standard `exception` event to span `s` with
  `exception.type`, `exception.message`, and (when callable stacks are
  available) `exception.stacktrace` attributes. Backends recognise this
  event.

### Reference implementations in the hs-opentelemetry corpus

Two concrete files serve as ground-truth patterns.

1. `/Users/shinzui/Keikaku/hub/haskell/hs-opentelemetry-project/hs-opentelemetry/instrumentation/hw-kafka-client/src/OpenTelemetry/Instrumentation/Kafka.hs`
   shows the producer/consumer pattern. Important idioms it uses:
   - Attribute names via typed imports like `messaging_operation`,
     `messaging_destination_name`.
   - An `AttributeMap` built with `insertAttributeByKey`, fed into
     `addAttributesToSpanArguments`, then into `inSpan''`.
   - `SpanArguments { kind = Producer }` / `kind = Consumer`.
   - `getTracerProviderPropagators <$> getGlobalTracerProvider` followed by
     `inject` / `extract` for headers, so user-configured propagators work.
   - Span names of the form `"send " <> topicName` and `"process " <> topicName`.

2. `/Users/shinzui/Keikaku/hub/haskell/hs-opentelemetry-project/docs/OpenTelemetry-Semantic-Conventions-Guide.md`
   is the narrative guide. Key points repeated here so the reader does not
   have to open it:
   - `hs-opentelemetry-semantic-conventions` only depends on
     `hs-opentelemetry-api` and `text`, so adding it is safe.
   - `AttributeKey`s are imported directly; `unkey` unwraps to `Text` if you
     ever need the raw string (you usually should not).
   - The package currently targets spec v1.24. Newer attribute names like
     `messaging.operation.type` are not present yet.


## Plan of Work

The work proceeds as six milestones. Each milestone is independently
verifiable: after each, the project builds and tests pass, and observable
behaviour has moved closer to v1.24 compliance.

### Milestone 1 — Adopt typed AttributeKeys

**Scope.** Introduce the dependency and swap every attribute name in the
traced interpreter from a hand-written string to the typed import from
`OpenTelemetry.SemanticConventions`. Delete the constants block in
`Pgmq.Effectful.Telemetry` and replace it with a re-export of the keys the
package uses. This is purely a rename; behaviour does not change yet, but
downstream compilation errors guarantee we have swapped every call site.

**At the end of the milestone.** The project builds against v1.24 typed keys.
The traced interpreter emits the same attributes as before, except for the
three that are not in v1.24: `messaging.operation.type` becomes
`messaging.operation` (still with the string `"send"` etc., to be tightened
in M2); `db.operation.name` becomes `db.operation`; the ad-hoc
`messaging.destination.routing_key` is dropped and routing-key attributes
move to `messaging.destination.name` when appropriate.

**Files changed.**

1. `pgmq-effectful/pgmq-effectful.cabal`: add
   `hs-opentelemetry-semantic-conventions >=0.1 && <0.2` to the `library`
   section's `build-depends` (next to `hs-opentelemetry-api`), and also to
   the `test-suite pgmq-effectful-test` section for upcoming tests.

2. `pgmq-effectful/src/Pgmq/Effectful/Telemetry.hs`: delete the
   `messagingSystem`, `messagingOperationType`, `messagingDestinationName`,
   `messagingMessageId`, `messagingBatchMessageCount`,
   `messagingRoutingKey`, `dbSystem`, `dbOperationName` constants. Replace
   the `-- Semantic Conventions` export block with explicit re-exports from
   `OpenTelemetry.SemanticConventions`:

        module Pgmq.Effectful.Telemetry
          ( -- * Trace Context Operations
            injectTraceContext,
            extractTraceContext,
            mergeTraceHeaders,
            TraceHeaders,

            -- * Semantic Convention Keys (re-exported from
            -- "OpenTelemetry.SemanticConventions")
            messaging_system,
            messaging_operation,
            messaging_destination_name,
            messaging_message_id,
            messaging_batch_messageCount,
            db_system,
            db_operation,
          )
        where

   Note the exact Haskell identifier spellings, which are generated by the
   semantic-conventions generator: `messaging_batch_messageCount` is the
   identifier for the spec name `messaging.batch.message_count`, and
   `db_operation` corresponds to `db.operation` (v1.24 spec). Verify these
   by grepping the generated module:

        grep -n 'messaging_batch_messageCount\|db_operation\s' \
          /Users/shinzui/Keikaku/hub/haskell/hs-opentelemetry-project/hs-opentelemetry/semantic-conventions/src/OpenTelemetry/SemanticConventions.hs

3. `pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs`: change the
   four helpers `addQueueAttributes`, `addBaseAttributes`,
   `addMessageIdAttribute`, `addBatchCountAttribute`, `addRoutingKeyAttribute`
   to use typed keys. For example:

        addBaseAttributes :: OTel.Span -> IO ()
        addBaseAttributes s = do
          OTel.addAttribute s (unkey messaging_system) ("pgmq" :: Text)
          OTel.addAttribute s (unkey db_system) ("postgresql" :: Text)

   However, a cleaner idiom (matching the Kafka reference) is to build an
   `AttributeMap` once per operation and set it when starting the span, via
   `addAttributesToSpanArguments`. Prefer this idiom for Milestones 2–4
   where attributes are known at span start. For now, a minimal rename that
   keeps existing `addAttribute` calls is acceptable.

   Delete `addRoutingKeyAttribute` and replace its call sites. For topic
   sends (`SendTopic`, `BatchSendTopic`, `SendTopicWithHeaders`,
   `BatchSendTopicWithHeaders`, `BatchSendTopicForLater`,
   `BatchSendTopicWithHeadersForLater`), call `addQueueAttributes`
   substituting the routing key text into `messaging_destination_name`.
   For the non-send topic routing helpers (`ValidateRoutingKey`,
   `TestRouting`), drop the routing-key attribute entirely in M1; it is
   back in M2 as an `Internal` span.

**Commands to run.**

From `/Users/shinzui/Keikaku/bokuno/libraries/pgmq-hs-project/pgmq-hs/`:

    cabal build pgmq-effectful

Expected output: successful compilation of `pgmq-effectful-0.2.0.0`.

**Acceptance.** The project compiles. Running `cabal test pgmq-effectful`
still passes (the existing tests do not assert attribute names).

### Milestone 2 — v1.24 required attributes and span-name form

**Scope.** Teach the traced interpreter the v1.24 vocabulary. Every messaging
span receives `messaging.operation` with the right vocabulary value, and
every pgmq call receives `db.operation` with the pgmq SQL function name.
Span names shift to `<operation_name> <destination_name>` to match the v1.24
naming guidance.

**At the end of the milestone.** A publish to queue `my-queue` produces a
span named `publish my-queue` of kind `Producer` carrying
`messaging.system=pgmq`, `messaging.operation=publish`,
`messaging.destination.name=my-queue`, `db.system=postgresql`,
`db.operation=pgmq.send`. A `read` from the same queue produces
`receive my-queue` of kind `Consumer` with `messaging.operation=receive` and
`db.operation=pgmq.read`. Lifecycle operations produce spans named
`pgmq.archive my-queue`, `pgmq.set_vt my-queue`, and so on, with only
`db.system`, `db.operation`, and where present `messaging.destination.name`.

**Implementation sketch.** In
`pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs`, define a small
value type describing each operation:

    data OpInfo = OpInfo
      { opMessagingKind :: Maybe Text   -- ^ "publish"/"receive" or Nothing
      , opDbFunction   :: Text          -- ^ e.g. "pgmq.send"
      , opSpanKind     :: OTel.SpanKind
      }

Then one function per constructor maps `Pgmq m a` to `OpInfo` and a
destination (`Maybe QueueName` or `Maybe RoutingKey`, normalised to
`Maybe Text`). Rebuild the interpreter to:

1. Compute `OpInfo` and destination up front.
2. Build an `AttributeMap` via `insertAttributeByKey` calls for the
   required attributes.
3. Build `SpanArguments` by starting from
   `defaultSpanArguments { kind = opSpanKind info }` and layering
   `addAttributesToSpanArguments attrs`.
4. Compute span name: if destination is `Just name`, use
   `opDbFunction <> " " <> name`; otherwise use `opDbFunction`. For
   messaging spans, use the messaging operation vocabulary instead of the
   db function name, i.e. `"publish" <> " " <> name`, `"receive" <> " " <> name`.
5. Start the span with `OTel.inSpan'`.

**Files changed.** Only
`pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs`.

**Commands to run.**

    cabal build pgmq-effectful

**Acceptance.** Compiles and existing tests still pass. Manually invoke
the interpreter against a fresh pgmq (see Concrete Steps) and observe
the attribute/name shape in stdout via a handle exporter — see Concrete
Steps below for the exact recipe.

### Milestone 3 — Pluggable propagator

**Scope.** Replace direct W3C encode/decode calls with the tracer
provider's configured propagator. This touches `Pgmq.Effectful.Telemetry`
and `Pgmq.Effectful.Traced`.

**At the end of the milestone.** `sendMessageTraced` obtains the propagator
via `getTracerProviderPropagators <$> getGlobalTracerProvider`, then
`inject propagator (insertSpan newSpan ctxt) []` to get HTTP-style request
headers, which are then written to message headers as a JSON object.
`readMessageWithContext` does the inverse. W3C is still the default
behaviour when the user installs the W3C propagator (which the SDK does
out of the box), but users who configure B3 or Datadog now work.

**Files changed.**

- `pgmq-effectful/src/Pgmq/Effectful/Telemetry.hs`: replace
  `injectTraceContext` and `extractTraceContext` with propagator-based
  equivalents. Drop the `hs-opentelemetry-propagator-w3c` dependency from
  `pgmq-effectful.cabal` (the SDK already depends on it).
- `pgmq-effectful/src/Pgmq/Effectful/Traced.hs`: call the new helpers.

**Commands to run.**

    cabal build pgmq-effectful

**Acceptance.** Compiles; existing error-propagation test still passes.
Milestone 5 adds an explicit test that an injected W3C traceparent round-trips
through pgmq headers.

### Milestone 4 — `recordException` and span status

**Scope.** Replace the hand-rolled `exception` event in
`recordUsageError` with `OpenTelemetry.Trace.Core.recordException`, and
use the `PgmqRuntimeError` classifier from
`Pgmq.Effectful.Interpreter.isTransient` to set the span status.

**At the end of the milestone.** On error, the span carries a standard
`exception` event with `exception.type`, `exception.message`, and
`exception.stacktrace` (when a call stack is available), and the span
status is `Error` with a description string. Backends that special-case
the `exception` event (every major one does) now light up.

**Files changed.** Only
`pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs`.

**Commands to run.**

    cabal build pgmq-effectful
    cabal test pgmq-effectful

**Acceptance.** Existing error-propagation test still passes.

### Milestone 5 — Tests

**Scope.** Install an in-memory exporter in
`pgmq-effectful/test/TracedInterpreterSpec.hs` and assert on span content.

**New test-suite dependencies.** Add to the `test-suite` section of
`pgmq-effectful.cabal`:

    , hs-opentelemetry-sdk                 ^>=0.2
    , hs-opentelemetry-exporter-in-memory  ^>=0.2
    , hs-opentelemetry-semantic-conventions >=0.1 && <0.2

The exact version bounds can be adjusted to match what is available in the
flake's pinned package set; the generous ranges above are a starting point.

**New tests (in `TracedInterpreterSpec.hs`).**

1. `"publish emits v1.24 attributes"`:
   - Create an in-memory exporter and tracer provider.
   - Run `sendMessageTraced` for a fresh queue.
   - Flush the provider, fetch the exported spans.
   - Assert: exactly one span, name `publish my-queue`, kind `Producer`,
     attributes contain keys `messaging.system`, `messaging.operation`,
     `messaging.destination.name`, `db.system`, `db.operation` with the
     expected values.

2. `"receive emits v1.24 attributes"`:
   - Similar to above but for `readMessage`.

3. `"error propagation records exception event and sets Error status"`:
   - Expand the existing test to also flush the exporter and assert that
     the span has an `exception` event and status `Error`.

4. `"traceparent round-trips through message headers"`:
   - Seed a parent context, call `sendMessageTraced`, read the message
     back, extract context, assert the TraceId matches.

**Commands to run.**

    cabal test pgmq-effectful

Expected output: all four new tests plus the existing tests pass.

### Milestone 6 — Documentation

**Scope.** Update module haddock and CHANGELOG.

**Files changed.**

- `pgmq-effectful/src/Pgmq/Effectful/Telemetry.hs` haddock banner:
  explain the v1.24 target, link to the guide.
- `pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs` haddock:
  tabulate the attributes emitted per operation kind.
- `pgmq-effectful/CHANGELOG.md`: add a bullet under the next version
  describing the attribute renames. Call out that
  `messaging.operation.type` was never a v1.24 name, so this is the first
  version with compliant messaging attributes; backends filtering on
  the old name will need updating.


## Concrete Steps

All commands are run from
`/Users/shinzui/Keikaku/bokuno/libraries/pgmq-hs-project/pgmq-hs/` unless
otherwise stated.

Enter the development shell once:

    nix develop

This gives you GHC 9.12.2, cabal-install, PostgreSQL, and the
haskell-language-server.

### Setup before any milestone

Verify the current tree builds and tests pass:

    cabal build all
    cabal test pgmq-effectful

Expected: both succeed.

### After Milestone 1

    cabal build pgmq-effectful

Expected transcript ends with:

    [n of n] Compiling Pgmq.Effectful.Interpreter.Traced ...
    Linking /.../pgmq-effectful ...

Grep to confirm no stale attribute names remain:

    grep -n 'messaging\.operation\.type\|db\.operation\.name\|messaging\.destination\.routing_key' \
      pgmq-effectful/src

Expected: no matches.

### After Milestone 2 — manual end-to-end check

Create a tiny ad-hoc script at `pgmq-effectful/test-manual/ManualCheck.hs`
that:

1. Starts `ephemeral-pg` (already a test dep).
2. Runs the pgmq migration.
3. Creates a tracer provider with a **handle exporter** writing JSON to
   stderr.
4. Calls `createQueue`, `sendMessage`, `readMessage` via the traced
   interpreter.
5. Flushes and exits.

Invoke via:

    cabal run pgmq-effectful:test-manual-check 2>&1 | head -40

Expected: JSON lines containing
`"name":"publish my-queue"` and attribute entries
`"messaging.operation":"publish"`, `"messaging.system":"pgmq"`,
`"messaging.destination.name":"my-queue"`, `"db.system":"postgresql"`,
`"db.operation":"pgmq.send"`.

If `hs-opentelemetry-exporter-handle` is not available in the flake pin,
skip the script and move the check to Milestone 5's in-memory exporter.

### After Milestone 3

    cabal build pgmq-effectful

No specific shell-level check; the propagator change is verified in
Milestone 5 via the round-trip test.

### After Milestone 4

    cabal test pgmq-effectful

Expected: all tests pass, including the existing error-propagation case.

### After Milestone 5

    cabal test pgmq-effectful

Expected: all four new tests pass. If a test fails, read the failure —
missing attributes usually indicate the typed key used is not what the
spec expects (double-check via grep against the generated
`SemanticConventions.hs`).

### After Milestone 6

Format everything before committing:

    nix fmt

Expected: zero drift on the tracked files.

Then commit per milestone with conventional-commit messages:

    git commit -m "feat(pgmq-effectful): adopt typed OTel semantic-conventions keys

    Replace hand-written attribute strings in Pgmq.Effectful.Telemetry
    with typed AttributeKey imports from hs-opentelemetry-semantic-conventions
    v0.1.0.0 (OpenTelemetry spec v1.24).

    ExecPlan: docs/plans/6-otel-semantic-conventions-v1-24.md
    Intention: intention_01kh0aq8xhednarw44bptt8xqm"


## Validation and Acceptance

The overall plan is accepted when:

1. `cabal build all` succeeds at every milestone boundary.
2. `cabal test pgmq-effectful` passes, including all four new tests in
   `TracedInterpreterSpec.hs`.
3. `grep -rn 'messaging\.operation\.type\|db\.operation\.name\|messaging\.destination\.routing_key' pgmq-effectful/src`
   returns no hits.
4. The manual exporter check (Milestone 2) shows JSON spans matching the
   expected attribute layout.
5. The CHANGELOG entry describes the attribute renames.

Acceptance is defined in terms of user-observable behaviour: after this
plan lands, someone running `pgmq-effectful`'s traced interpreter and
looking at their OTel backend sees spans that populate the standard
messaging dashboards those backends ship. They do not need to write custom
transforms or dashboards — the emitted attribute names are the same names
the backends already expect.


## Idempotence and Recovery

Every step in this plan is additive and safe to repeat. The one
potentially destructive action is the grep-and-rename in Milestone 1; if
it leaves the tree in a non-building state, `git checkout -- pgmq-effectful/src`
discards the rename and the plan can be restarted. Dependency additions
in `pgmq-effectful.cabal` are trivially reverted by editing the cabal
file back.

The plan is incremental: if any milestone fails, the earlier milestones'
commits remain valid on their own. The traced interpreter continues to
produce spans throughout; only the attribute names shift.


## Interfaces and Dependencies

### New dependency

`hs-opentelemetry-semantic-conventions` is added to
`pgmq-effectful/pgmq-effectful.cabal` under both the library and test
`build-depends` stanzas. The package is at version 0.1.0.0 in the
`iand675/hs-opentelemetry` corpus mirror. It depends only on
`hs-opentelemetry-api` and `text`, so no new transitive risk.

### Modules

In `pgmq-effectful/src/Pgmq/Effectful/Telemetry.hs`, after Milestone 1
the module exports (no definitions, just re-exports for the keys used
elsewhere in the package):

    module Pgmq.Effectful.Telemetry
      ( -- * Trace Context Operations
        injectTraceContext,
        extractTraceContext,
        mergeTraceHeaders,
        TraceHeaders,

        -- * Semantic Convention Keys (re-exported)
        messaging_system,
        messaging_operation,
        messaging_destination_name,
        messaging_message_id,
        messaging_batch_messageCount,
        db_system,
        db_operation,
      )

After Milestone 3, `injectTraceContext` and `extractTraceContext`
change signature to take a tracer provider (or accept an implicit
global provider):

    injectTraceContext
      :: TracerProvider -> Context -> IO TraceHeaders

    extractTraceContext
      :: TracerProvider -> TraceHeaders -> Context -> IO Context

The exact shape is the Kafka instrumentation's shape; adjust during
implementation to minimise churn.

In `pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs`, after
Milestone 2 the new internal helpers are:

    data OpInfo = OpInfo
      { opMessagingKind :: !(Maybe Text)
      , opDbFunction   :: !Text
      , opSpanKind     :: !OTel.SpanKind
      }

    operationInfo :: Pgmq m a -> (OpInfo, Maybe Text)
    operationInfo = \case
      SendMessage (Types.SendMessage qn _ _) ->
        (OpInfo (Just "publish") "pgmq.send" OTel.Producer,
         Just (queueNameToText qn))
      -- ... one arm per constructor ...

    spanNameFor :: OpInfo -> Maybe Text -> Text
    spanNameFor info md = case (opMessagingKind info, md) of
      (Just mk, Just d) -> mk <> " " <> d
      (Just mk, Nothing) -> mk
      (Nothing, Just d) -> opDbFunction info <> " " <> d
      (Nothing, Nothing) -> opDbFunction info

    operationAttributes :: OpInfo -> Maybe Text -> AttributeMap
    operationAttributes info md =
      let base = insertAttributeByKey db_system ("postgresql" :: Text)
               . insertAttributeByKey db_operation (opDbFunction info)
          withMsgKind = case opMessagingKind info of
            Just mk -> insertAttributeByKey messaging_system ("pgmq" :: Text)
                     . insertAttributeByKey messaging_operation mk
            Nothing -> id
          withDest = case md of
            Just d  -> insertAttributeByKey messaging_destination_name d
            Nothing -> id
       in (base . withMsgKind . withDest) mempty

The interpreter body is a single `interpret` that dispatches on the
`Pgmq` constructor, builds `OpInfo` and destination, assembles the
attribute map, starts the span via `OTel.inSpan'`, runs the underlying
`Hasql.Session.Session`, records the outcome, and returns.


## Revision Protocol

When this plan is revised, the revision must be reflected across every
section — Progress (split or expand), Surprises & Discoveries (record the
motivation), Decision Log (append a dated entry), and Outcomes &
Retrospective (record deltas) — and a "Revision note" appended at the
bottom with date and reason.


## Revision Notes

None yet.
