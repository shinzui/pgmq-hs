# Grouped heads and partition controls through effects

`Pgmq.Effectful.Effect` exposes the PGMQ 1.12 grouped-head operations. Both
`runPgmq` and `runPgmqTraced` execute the same database operations:

```haskell
import Pgmq.Effectful.Effect qualified as Pgmq
import Pgmq.Hasql.Statements.Types qualified as Types

-- Inside an Eff computation with the Pgmq effect:
heads <- Pgmq.readGroupedHead (Types.ReadGrouped queue 30 10)
-- Or wait up to one second, checking every ten milliseconds:
heads <- Pgmq.readGroupedHeadWithPoll
  (Types.ReadGroupedWithPoll queue 30 10 1 10)
```

Groups are identified by the `x-pgmq-group` message header. A call leases at most
one absolute head from each group, even when the requested quantity exceeds the
number of groups. An invisible head blocks later messages in its group; lease
expiry can deliver that head again. Delete or archive a processed head to allow
the next message through. Messages without the header share one implicit group.
This does not provide exactly-once processing.

Polling occupies a database connection while waiting. The traced interpreter
records that wait in a Consumer span named `receive <queue>`. SQL labels are
`pgmq.read_grouped_head` and `pgmq.read_grouped_head_with_poll`. They appear in
`db.operation` by default, `db.operation.name` in stable database mode, and both
with `OTEL_SEMCONV_STABILITY_OPT_IN=messaging/dup,database/dup`.

PGMQ 1.13 additionally supports explicit partition premake:

```haskell
Pgmq.createPartitionedQueueWithPremake
  (Types.CreatePartitionedQueue queue "daily" "7 days") 2
```

Both queue and archive parents receive the count. Counts below one produce a
`PgmqRuntimeError`; an explicit count on PGMQ 1.12 is an unsupported SQL operation.
The existing `createPartitionedQueue` operation still uses the server default and
works on both versions. Partition creation emits the existing Internal span named
`pgmq.create_partitioned <queue>` with SQL label `pgmq.create_partitioned`.

`queueMetrics` and `allQueueMetrics` preserve `defaultPartitionLength :: Maybe Int64`
in both interpreters. `Nothing` means unavailable on PGMQ 1.12 or inapplicable to
an ordinary queue on PGMQ 1.13. `Just n` is the planner's estimate of rows in the
queue and archive default partitions; it is not an exact live count.

For startup configuration, see [declarative partitioned queues](queue-configuration.md#partitioned-queue).
Premake, partition interval, and retention are creation-only options there.
