# Upgrading to pgmq-hs 0.6

The five libraries move together to 0.6.0.0. This repository prepares the candidate;
publishing to Hackage is a separate operation. Use family bounds `>=0.6 && <0.7`.

## Server compatibility

| Operation | PGMQ 1.12 | PGMQ 1.13 |
|-----------|-----------|-----------|
| All six grouped reads | Supported | Supported |
| Existing three-argument partition creation | Supported, default premake 4 | Supported, default premake 4 |
| Explicit premake creation | Unsupported; SQL error | Supported, count at least 1 |
| `defaultPartitionLength` | `Nothing` (unavailable) | `Nothing` for ordinary queues; `Just n` for partition estimates |

Native `pgmq-migration` installs and upgrades to 1.13.0. Complete all pending migrations
before admitting queue-creation traffic. Imported 1.11 histories retain their original
checksum and schema-validation requirements; see [schema migration](schema-migration.md).

## Public imports and grouped heads

Import `Pgmq` for sessions or `Pgmq.Effectful` for effects. Both expose `readGrouped`,
`readGroupedWithPoll`, `readGroupedRoundRobin`, `readGroupedRoundRobinWithPoll`,
`readGroupedHead`, `readGroupedHeadWithPoll`, and both argument constructors.

```haskell
headRequest :: QueueName -> ReadGrouped
headRequest q = ReadGrouped q 30 100

pollRequest :: QueueName -> ReadGroupedWithPoll
pollRequest q = ReadGroupedWithPoll q 30 100 5 100
```

The `x-pgmq-group` header identifies the group; missing headers share one implicit group.
Head reads return at most one message per group even when `qty` exceeds the group count.
The absolute lowest message ID is the head. An invisible head blocks its group; expiration
makes that same head eligible again. Deleting or archiving it permits the next message.
These are visibility leases, so consumers must tolerate repeated delivery. Polling waits
inside PostgreSQL and occupies a connection. See [effectful usage](effectful-grouped-reads.md)
for tracing and interpreter examples.

## Source migrations

`CreatePartitionedQueue` keeps its three fields. Explicit premake is a separate argument:

```haskell
partitionRequest :: QueueName -> CreatePartitionedQueue
partitionRequest q = CreatePartitionedQueue q "100" "1000"

-- createPartitionedQueue (partitionRequest q) uses server defaults.
-- createPartitionedQueueWithPremake (partitionRequest q) 8 requires PGMQ 1.13.
```

When constructing `QueueMetrics`, append `defaultPartitionLength = Nothing` if the value is
unavailable, or `Just estimate` if supplied by the server. The existing seven fields remain.
Patterns that name only existing fields continue to work; positional patterns/constructors
must accept the eighth field. Never substitute zero for `Nothing`.

When constructing `PartitionConfig`, add `premake = Nothing` for the default, or `Just n`:

```haskell
partitioning :: PartitionConfig
partitioning = PartitionConfig
  { partitionInterval = "100"
  , retentionInterval = "1000"
  , premake = Just 8
  }
```

Import `PartitionConfig(..)` from `Pgmq.Config` for that example. Premake controls partitions
created ahead for both queue and archive. Configuration applies it only when creating a
queue. Updating an existing queue's configuration neither changes pg_partman settings nor
checks them for drift. The shared `ReconcileOps` backend remains internal.

## Default-partition monitoring and recovery

`defaultPartitionLength` estimates spilled rows across queue and archive default partitions
using planner statistics. Positive estimates warrant maintenance attention; zero can lag
writes until ANALYZE or autovacuum refreshes statistics. `Nothing` means unavailable or
inapplicable. Upgrading alone does not drain defaults or schedule maintenance.

Follow the [operator recovery sequence](schema-migration.md#partition-maintenance-after-upgrading):
locate pg_partman's schema, call `partition_data_proc` outside a transaction for each affected
queue and archive, run maintenance, then ANALYZE both defaults. The upgrade's `BY DEFAULT`
identity permits reinsertion with preserved IDs. Producers should use pgmq's send API;
arbitrary supplied IDs can leave the identity sequence behind.
