---
title: "OpenTelemetry-instrumented interpreter"
type: Capability
description: "A traced `Pgmq` interpreter that emits OpenTelemetry spans per operation and propagates W3C (or configured) trace context through pgmq message headers."
generated:
  by: adopt-capabilities/0.9.2
  at: "2026-08-08T00:00:00Z"
capabilityId: CAP-6
provider: mori://shinzui/pgmq-hs
status: shipped
stability: experimental
since: "0.1.0.0"
packages:
  - pgmq-effectful
requires:
  - CAP-5
interface:
  - Pgmq.Effectful.Interpreter.Traced
  - Pgmq.Effectful.Telemetry
evidence:
  - kind: test
    resource: pgmq-effectful/test/TracedInterpreterSpec.hs
    proves: The traced interpreter emits spans with the expected semantic-convention attributes and round-trips trace context through message headers via an in-memory exporter.
  - kind: guide
    resource: docs/OPENTELEMETRY_INSTRUMENTATION.md
    proves: How to wire the traced interpreter, the semantic-convention opt-in, and context propagation.
---

# OpenTelemetry-instrumented interpreter

A consumer already using the [Effectful integration](effectful-integration.md) can swap in
`runPgmqTraced` to get OpenTelemetry spans for every pgmq operation and end-to-end trace
context carried across the queue. This is a distinct adoption on top of the plain
interpreter — you opt into tracing, and it is proven by its own span-assertion test — so it
is its own capability.

What it provides:

- **Traced interpreter** — `runPgmqTraced`, `runPgmqTracedWith`, `TracingConfig`,
  `defaultTracingConfig`, emitting spans that follow OpenTelemetry messaging and database
  semantic conventions.
- **Context propagation** — `Pgmq.Effectful.Telemetry.injectTraceContext` /
  `extractTraceContext` (through the tracer provider's configured propagator — W3C by
  default), plus `traceHeadersToJson` / `jsonToTraceHeaders` / `mergeTraceHeaders` for the
  pgmq-over-jsonb boundary, and re-exported typed semantic-convention attribute keys.
- **Traced helpers** — `sendMessageTraced`, `readMessageWithContext`.

## Shape

```haskell
import Pgmq.Effectful (runPgmqTraced, defaultTracingConfig)

runEff . runError @PgmqRuntimeError . runPgmqTraced tracerProvider defaultTracingConfig pool $
  sendMessage (SendMessage q body)
```

## Limits

- **Message bodies are not traced by default.** `TracingConfig.includeMessageBodies`
  defaults to `False` because bodies may contain PII; enable it deliberately. Span-status
  failure descriptions are coarse non-PII labels, with full detail on the `exception`
  event.
- **The `process` messaging span is intentionally not emitted** — it belongs to the
  consumer, which should open its own `process` span around message handling.
- **Semantic-convention attribute and span names changed across releases** (v1.24 in
  0.2.0.0, the 1.40 family in 0.3.0.0). By default the interpreter keeps the older v1.24
  attributes; set `OTEL_SEMCONV_STABILITY_OPT_IN=messaging,database` for stable names or
  `.../dup` to emit both during migration. Dashboards keyed on the old names need updating
  when opting in.
- Pre-1.0 and uniformly `experimental`.
