---
title: "JSON wire encodings of the domain records"
type: Capability
description: "Stable, golden-pinned aeson encodings for queues, messages, metrics, bindings, routing matches, throttles and reconciliation reports using pgmq's snake_case column names."
generated:
  by: openai/gpt-6.1-sol
  at: "2026-10-20T03:03:48Z"
capabilityId: CAP-11
provider: mori://shinzui/pgmq-hs
status: shipped
stability: experimental
since: "unreleased"
packages:
  - pgmq-core
  - pgmq-hasql
  - pgmq-config
requires:
  - CAP-1
interface:
  - Pgmq.Types
  - Pgmq.Hasql.Statements.Types
  - Pgmq.Config.Types
evidence:
  - kind: test
    resource: pgmq-core/test/JsonGoldenSpec.hs
    proves: Core records have pinned encodings, validated/lenient queue decoders, exact int64 fixtures, arbitrary headers and normalized live/archive round trips.
  - kind: test
    resource: pgmq-hasql/test/JsonGoldenSpec.hs
    proves: Metrics preserve total/visible depth and nullable estimates; arbitrary message bodies and headers round-trip with encoder parity.
  - kind: test
    resource: pgmq-config/test/JsonGoldenSpec.hs
    proves: Every reconciliation action and supporting queue shape has a pinned encoding and encoder parity.
  - kind: guide
    resource: docs/design/020-json-wire-encodings.md
    proves: Field names, null normalization, timestamps, numeric IDs and the published-is-frozen policy.
---

# JSON wire encodings of the domain records

Consumers can encode queues, live/archive messages, metrics, topic bindings,
routing matches, topic-send results, notification throttles and reconciliation
reports without inventing another wire dialect. The owning package supplies the
instances; web dependencies are unnecessary. Fifty-three golden files pin the
shapes and explicit null fields.

## Shape

```haskell
import Data.Aeson (encode)
import Data.ByteString.Lazy (ByteString)
import Pgmq.Types (Message)

messageJson :: Message -> ByteString
messageJson = encode
```

`Queue`, `UnvalidatedQueue`, `Message` and `ArchivedMessage` also decode. Queue
names validate only when decoding as Queue; UnvalidatedQueue preserves the text.
See [design note 020](../design/020-json-wire-encodings.md) for every field and
report action.

## Limits

These instances are unreleased and experimental. Once published, fields are
never removed or re-typed; additions are permitted. The remaining records are
encode-only. Headers accept arbitrary JSON, with `Just Null` normalized to
`Nothing` on decode. Message IDs remain numeric JSON; browser clients need
lossless integer parsing for 64-bit positions. Record order is pinned, while
payload object order is aeson's own. Large payloads still allocate their output.
