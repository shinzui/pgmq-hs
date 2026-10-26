---
title: "Declarative queue reconciliation"
type: Capability
description: "Declare a queue topology as Haskell values and additively reconcile it at startup: create what is missing, report drift, and truthfully account for every action taken."
generated:
  by: adopt-capabilities/0.9.2
  at: "2026-08-08T00:00:00Z"
capabilityId: CAP-9
provider: mori://shinzui/pgmq-hs
status: shipped
stability: experimental
since: "0.1.3.0"
packages:
  - pgmq-config
requires:
  - CAP-1
  - CAP-4
interface:
  - Pgmq.Config
  - Pgmq.Config.Types
evidence:
  - kind: test
    resource: pgmq-config/test/CollisionSpec.hs
    proves: A declared name colliding with foreign mixed-case metadata is reported without creating aliases or sub-resources, through both backends and schema selections.
  - kind: test
    resource: pgmq-config/test/PartitionSpec.hs
    proves: Partition notifications are reported unsupported without installing or changing a throttle; effective observed shape controls drift behavior.
  - kind: test
    resource: pgmq-config/test/ConfigSpec.hs
    proves: ensureQueues creates standard/unlogged/partitioned queues, is convergent on a second run, and reports the actions it took.
  - kind: test
    resource: pgmq-config/test/ForeignQueueSpec.hs
    proves: A queue another client created under a name parseQueueName rejects does not fail reconciliation.
  - kind: test
    resource: pgmq-config/test/NotifyCrashSpec.hs
    proves: Notify-throttle reconciliation behaves across the crash fail-open semantics.
  - kind: guide
    resource: docs/design/018-reconciliation-contract.md
    proves: The real reconciliation contract — additive, one in-place mutation, drift reported not repaired.
  - kind: guide
    resource: docs/user/queue-configuration.md
    proves: How to declare a topology and call ensureQueues at startup.
---

# Declarative queue reconciliation

`pgmq-config` lets a consumer declare its whole queue topology — standard, unlogged, and
partitioned queues, FIFO indexes, insert-notification throttles, and topic bindings — as
Haskell values, then call one function at startup to create whatever is missing. It builds
on the [core client](message-queue-client.md) and drives [insert
notifications](insert-notifications.md) as part of a topology. Because it is a separate
package with its own adoption decision and its own reconciliation test suite, it is its own
capability.

What it provides:

- **DSL** — `standardQueue`, `unloggedQueue`, `partitionedQueue`; modifiers
  `withNotifyInsert`, `withFifoIndex`, `withTopicBinding`.
- **Reconciliation** — `ensureQueues`, `ensureQueuesWithPool`, `ensureQueuesReport`,
  returning a `ReconcileAction` list; `ObservedQueueType`, `defaultThrottleMs`.
- **Effectful variant** — the same reconciler over the `Pgmq` effect in
  `Pgmq.Config.Effectful` (behind the `effectful` flag).

## Shape

```haskell
import Pgmq.Config

ensureQueuesWithPool pool
  [ standardQueue ordersQueue & withNotifyInsert Nothing
  , partitionedQueue eventsQueue partitionCfg
  ]
```

## Limits

- **Name collisions are reported, not repaired.** A missing exact row with foreign aliases
  stops that config's creation and sub-resources. An exact row allows sub-resources to proceed.
  Detection is snapshot-based, not serialization against foreign clients.
- **Partition notifications are reported, not repaired.** Neither enable nor update runs
  on an observed/newly created partitioned queue; existing foreign rows remain unchanged.

- **Reconciliation is additive, not blanket-idempotent.** The reconciler never drops,
  converts, or disables anything, and leaves queues absent from the config untouched. The
  authoritative contract is the `ensureQueues` Haddock and
  `docs/design/018-reconciliation-contract.md`.
- **Exactly one mutation of existing state.** A declared notify-throttle interval that
  differs from the stored one is written in place — which also resets the throttle's
  `last_notified_at` to the epoch. Nothing else about an existing queue is changed.
- **Queue-type drift is reported, never repaired.** A declared type contradicting the live
  queue surfaces as `DetectedQueueTypeDrift`; converting a type would drop and recreate the
  queue, destroying its messages. Partition interval and retention are not drift-checked at
  all, because `pgmq.list_queues` does not report them.
- **Not transactional.** Each call autocommits; a failure part-way leaves completed work in
  place and the next run continues from there.
- **Concurrent multi-replica startup can race on stock extension installs.** Two replicas
  reconciling the same fresh queue can fail one with SQLSTATE 42710 (`duplicate_object`) on
  a stock upstream 1.11.0 extension install. Databases installed through
  [extension-free schema installation](extension-free-schema-install.md) (migration `0003`)
  are free of that race; otherwise retry the reconcile or serialize startup.
- Pre-1.0 and uniformly `experimental`.
