---
title: "Topic routing (publish/subscribe)"
type: Capability
description: "AMQP-like topic routing on pgmq: bind pattern-to-queue, publish by routing key, and inspect or test routing, over pgmq 1.11.0+."
generated:
  by: adopt-capabilities/0.9.2
  at: "2026-08-08T00:00:00Z"
capabilityId: CAP-3
provider: mori://shinzui/pgmq-hs
status: shipped
stability: experimental
since: "0.1.1.0"
packages:
  - pgmq-hasql
  - pgmq-core
requires:
  - CAP-1
interface:
  - Pgmq
  - Pgmq.Hasql.Statements.TopicManagement
evidence:
  - kind: test
    resource: pgmq-hasql/test/TopicSpec.hs
    proves: bind/unbind, send_topic and its batch/header variants, routing-key and pattern validation, test_routing, and binding listings work against pgmq 1.11.0.
  - kind: guide
    resource: docs/design/011-pgmq-1.11.0-upgrade.md
    proves: The pgmq 1.11.0 topic-routing surface this capability wraps and its schema requirements.
---

# Topic routing (publish/subscribe)

Building on the [core client](message-queue-client.md), pgmq-hasql exposes pgmq's
AMQP-like topic routing: bind a topic pattern to a queue, publish a message by routing key,
and let pgmq fan it out to the matching queues. A consumer adopts topic routing as a
distinct decision from plain queueing — it needs pgmq 1.11.0 and is proven by its own test
suite — so it is its own capability.

What it provides:

- **Bindings** — `bindTopic`, `unbindTopic`, `listTopicBindings`,
  `listTopicBindingsForQueue`.
- **Publishing** — `sendTopic`, `sendTopicWithHeaders`, and the `batchSendTopic*` family
  (immediate, delayed, and with-headers).
- **Validation / inspection** — `validateRoutingKey`, `validateTopicPattern`,
  `testRouting`, plus the `parseRoutingKey` / `parseTopicPattern` smart constructors in
  `pgmq-core`.

## Shape

```haskell
import Pgmq
import Pgmq.Types (parseRoutingKey, parseTopicPattern)

Pool.use pool $ do
  bindTopic (BindTopic ordersQueue pattern)          -- pattern e.g. "orders.*"
  _ <- sendTopic (SendTopic routingKey body)          -- routingKey e.g. "orders.eu"
  pure ()
```

## Limits

- **`parseTopicPattern` validates only length and non-emptiness** (≤255 chars), not
  wildcard structure — unlike `parseRoutingKey`. A structurally invalid pattern is
  accepted by the client and only rejected, if at all, by pgmq server-side. Use
  `validateTopicPattern` / `testRouting` against the database when the pattern shape
  matters.
- **Requires pgmq 1.11.0 or later.** Against an older schema these functions do not exist.
- Pre-1.0 and uniformly `experimental`.
