---
okf_version: "0.2"
title: "pgmq-hs capability catalog"
---

# What pgmq-hs provides today

pgmq-hs is a Haskell client for [pgmq](https://github.com/tembo-io/pgmq), a message queue
built on PostgreSQL. What a consumer adopts, one concept per capability, each backed by
evidence you can open:

| Handle | Capability | Packages | Since |
|---|---|---|---|
| [CAP-1](message-queue-client.md) | PostgreSQL message-queue client (hasql) | pgmq-hasql, pgmq-core | 0.1.0.0 |
| [CAP-2](fifo-message-group-reads.md) | FIFO message-group reads | pgmq-hasql | 0.1.0.0 |
| [CAP-3](topic-routing.md) | Topic routing (publish/subscribe) | pgmq-hasql, pgmq-core | 0.1.1.0 |
| [CAP-4](insert-notifications.md) | Insert notifications (LISTEN/NOTIFY) | pgmq-hasql, pgmq-core | 0.1.0.0 |
| [CAP-5](effectful-integration.md) | Effectful integration + typed error model | pgmq-effectful | 0.1.0.0 |
| [CAP-6](opentelemetry-tracing.md) | OpenTelemetry-instrumented interpreter | pgmq-effectful | 0.1.0.0 |
| [CAP-7](extension-free-schema-install.md) | Extension-free pgmq schema installation | pgmq-migration | 0.1.0.0 |
| [CAP-8](predecessor-history-import.md) | Predecessor-history ledger import + schema contract | pgmq-migration | 0.4.0.0 |
| [CAP-9](declarative-queue-reconciliation.md) | Declarative queue reconciliation | pgmq-config | 0.1.3.0 |

Every capability is `stability: experimental`: pgmq-hs is pre-1.0 and makes no
cross-version compatibility promise yet. The field is uniform because the promise is
uniform, not because it is unconsidered.

## Deliberately excluded

- **`pgmq-bench`** — an internal benchmark tool (`visibility: Internal`), not something a
  consumer depends on. Excluded under the provision rule.
- **Grouped-head reads (pgmq 1.12.0)** — explicitly not in the current release. A
  capability with no shipped evidence is an improvement request, not a catalog entry; there
  is no `planned` status.
- **Individual message operations, batch variants, and topic/notify subcommands** — folded
  into their parent capability under the granularity rule. A consumer adopts "the hasql
  client" or "topic routing", not each function separately.
- **Cross-repository behaviour** — none is claimed. Everything here is something
  pgmq-hs's own code does against a PostgreSQL database.
