---
type: Improvement Request
title: Provide JSON codecs for inspection-facing records
description: >-
  Add aeson instances with a documented, stable field-naming policy to the domain records an
  inspection wire format needs — Queue, UnvalidatedQueue, Message, QueueMetrics, TopicBinding,
  RoutingMatch, NotifyInsertThrottle, ReconcileAction — so downstream surfaces stop
  re-implementing pgmq-hs's vocabulary field by field.
generated:
  by: anthropic/claude-fable-5
  at: "2026-08-19T00:00:00Z"
requestId: IR-2
status: proposed
origin: mori://shinzui/keiro-ui
---

# Improvement Request: Provide JSON Codecs for Inspection-Facing Records

## Status

Proposed by the keiro runtime UI initiative
(`mori://shinzui/keiro-ui/masterplans/1-keiro-runtime-ui-foundations`, filed under
`mori://shinzui/keiro-ui/plans/3-audit-pgmq-hs-and-file-ui-endpoint-improvement-requests`).
Implementation is pgmq-hs's own downstream work. This request is a prerequisite of the
sister-package request (`mori://shinzui/pgmq-hs/okf/improvement-requests/concepts/IR-3`) but
stands on its own: any consumer that puts pgmq-hs state on a wire needs these codecs.

## Problem

Only the newtypes (`QueueName`, `MessageId`, and friends) carry `ToJSON`/`FromJSON` today. The
records an inspection surface actually serves — `Queue`, `UnvalidatedQueue`, `Message`,
`QueueMetrics`, `TopicBinding`, `RoutingMatch`, `NotifyInsertThrottle`, `ReconcileAction` —
derive only `Eq`/`Generic`/`Show` (audited 2026-08-19 at HEAD `9ee9a2f`, re-confirmed at filing
time). Every downstream wire format must therefore hand-roll encodings of pgmq-hs's own
vocabulary, and two consumers will inevitably encode the same record differently — field names,
casing, timestamp formats — turning one library type into several incompatible wire dialects.

## Requested Change

1. `ToJSON` instances for the records above, and `FromJSON` where round-tripping is meaningful
   (`Queue`/`UnvalidatedQueue` and `Message` for test fixtures and tooling; encode-only is
   acceptable where a decode has no consumer).
2. A documented, stable field-naming policy for these encodings, consistent with the
   cross-project inspection conventions the initiative maintains
   (project `mori://shinzui/keiro-ui`, path `docs/architecture/inspection-api-conventions.md`,
   artifact-level URI pending): new wire fields are snake_case, and once published the encoding
   is a compatibility surface — fields are never removed or re-typed, additive is allowed.
3. The policy note lives with the instances (Haddock on the types or a short design note in
   `docs/design/`), so a future field addition knows the casing rule and the frozen-ness rule
   without archaeology.

## Acceptance

1. Golden encoding tests pin the JSON output of every record listed above; a field rename or
   re-typing breaks a test.
2. The field-naming policy is documented where implementers will see it, and states explicitly
   that the encodings are a published compatibility surface once released.
3. Existing newtype instances are unchanged; no downstream consumer of the current instances
   breaks.
4. `QueueMetrics` encodings distinguish total and visible depth per
   `docs/design/002-queue-visible-length.md` — the two numbers mean different things and the
   field names must not invite confusing them.

## Non-goals

No breaking change to any existing instance. No HTTP surface (that is IR-3). No commitment to a
generic-derivation strategy — hand-written or derived instances are both fine as long as the
golden tests pin the output.
