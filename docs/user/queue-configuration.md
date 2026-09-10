# Declarative Queue Configuration

The `pgmq-config` package lets you declare your queue topology as Haskell values and ensure all queues exist at application startup. Every operation is idempotent — calling it repeatedly is safe and produces no errors or duplicate side effects.

## Quick Start

Add `pgmq-config` to your `build-depends`:

```cabal
build-depends:
  , pgmq-config
  , pgmq-core
```

Define your queues and call `ensureQueuesWithPool` before your workers start:

```haskell
import Pgmq.Config
import Pgmq.Types (parseQueueName, parseTopicPattern)

myQueues :: [QueueConfig]
myQueues =
  let Right orderQueue = parseQueueName "order_events"
      Right taskQueue = parseQueueName "background_tasks"
      Right auditQueue = parseQueueName "audit_log"
      Right orderPattern = parseTopicPattern "orders.*"
   in [ standardQueue orderQueue
          & withNotifyInsert (Just 1000)
          & withFifoIndex
          & withTopicBinding orderPattern
      , unloggedQueue taskQueue
      , standardQueue auditQueue
      ]

main :: IO ()
main = do
  pool <- acquirePool -- your hasql-pool setup
  result <- ensureQueuesWithPool pool myQueues
  case result of
    Left err -> error $ "Failed to configure queues: " <> show err
    Right () -> startWorkers pool
```

## Queue Types

### Standard Queue

The default. Uses PostgreSQL write-ahead logging (WAL), so data survives crashes.

```haskell
standardQueue :: QueueName -> QueueConfig
```

### Unlogged Queue

Faster writes because PostgreSQL skips WAL. Data is **lost on crash or unclean shutdown**. Good for ephemeral work queues where speed matters more than durability.

```haskell
unloggedQueue :: QueueName -> QueueConfig
```

### Partitioned Queue

For high-throughput scenarios. Partitions the underlying tables by time or row count.

```haskell
partitionedQueue :: QueueName -> PartitionConfig -> QueueConfig

-- Example: partition by day, retain for 7 days
let pc = PartitionConfig
      { partitionInterval = "daily"
      , retentionInterval = "7 days"
      , premake = Nothing
      }
    cfg = partitionedQueue myQueue pc
```

`premake = Nothing` uses the existing three-argument creation call, preserving the
server default of four premade partitions on both PGMQ 1.12 and 1.13. Set
`premake = Just 2` to request two premade partitions for both queue and archive;
this requires PGMQ 1.13 and real pg_partman. Zero and negative counts are server
errors, and an explicit count on 1.12 fails rather than being ignored.

Partition interval, retention interval, and premake apply only when creating a
missing queue, through either the direct or effectful adapter. Changing any of
these settings for an existing partitioned queue reports `SkippedQueue`; its
settings are neither checked nor changed. Manage existing partition settings
separately through pg_partman.

When migrating record constructions to the next package release, add
`premake = Nothing` to preserve the previous behavior. Positional constructions
become `PartitionConfig "daily" "7 days" Nothing`.

## Modifiers

Modifiers compose with `&` (from `Data.Function`) to add features to any queue config.

### Notify Insert

Enables PostgreSQL `LISTEN/NOTIFY` when messages are inserted. Workers can block on notifications instead of polling. The optional throttle (in milliseconds) limits how often notifications fire.

```haskell
-- Notify at most once per 500ms
standardQueue myQueue & withNotifyInsert (Just 500)

-- Notify on every insert (use pgmq default throttle of 250ms)
standardQueue myQueue & withNotifyInsert Nothing
```

### FIFO Index

Creates a GIN index on the `headers` column to improve performance of FIFO (first-in, first-out) grouped reads.

```haskell
standardQueue myQueue & withFifoIndex
```

### Topic Binding

Binds a topic pattern to the queue for AMQP-style message routing. Patterns support wildcards: `*` matches one segment, `#` matches zero or more segments.

```haskell
let Right pattern = parseTopicPattern "orders.*.created"
in standardQueue myQueue & withTopicBinding pattern
```

Multiple bindings can be added:

```haskell
standardQueue myQueue
  & withTopicBinding pat1
  & withTopicBinding pat2
```

## Reconciliation Functions

### `ensureQueues`

Runs as a hasql `Session`. Creates missing queues, enables notifications, creates indexes, and binds topics. Does not drop or modify existing queues (additive only).

```haskell
ensureQueues :: [QueueConfig] -> Session ()
```

### `ensureQueuesWithPool`

Convenience wrapper that runs against a `hasql-pool` `Pool`:

```haskell
ensureQueuesWithPool :: Pool -> [QueueConfig] -> IO (Either UsageError ())
```

### `ensureQueuesReport`

Like `ensureQueues` but returns a list of actions taken. Queries existing state first and skips operations that are already satisfied. Useful for startup logging.

```haskell
ensureQueuesReport :: [QueueConfig] -> Session [ReconcileAction]
```

`ReconcileAction` values tell you exactly what happened:

```haskell
data ReconcileAction
  = CreatedQueue QueueName QueueType
  | EnabledNotify QueueName (Maybe Int32)
  | CreatedFifoIndex QueueName
  | BoundTopic QueueName TopicPattern
  | SkippedQueue QueueName
  | SkippedNotify QueueName
  | SkippedFifoIndex QueueName
  | SkippedTopicBinding QueueName TopicPattern
```

Example: log what happened at startup:

```haskell
actions <- runSession pool (ensureQueuesReport myQueues)
for_ actions $ \case
  CreatedQueue qn _ -> putStrLn $ "Created queue: " <> show qn
  SkippedQueue qn   -> putStrLn $ "Queue exists: " <> show qn
  _                 -> pure ()
```

## Effectful Integration

If you use the `effectful` effect system, `pgmq-config` provides wrappers that operate through the `Pgmq` effect (enabled by default via a cabal flag):

```haskell
import Pgmq.Config.Effectful

ensureQueuesEff :: (Pgmq :> es) => [QueueConfig] -> Eff es ()
ensureQueuesReportEff :: (Pgmq :> es) => [QueueConfig] -> Eff es [ReconcileAction]
```

These use the same `Pgmq` effect as the rest of your application, so the reconciliation participates in the same interpreter and tracing setup.

## Idempotency Guarantees

Every underlying pgmq operation is idempotent:

- `pgmq.create()` uses `CREATE TABLE IF NOT EXISTS` and `INSERT ... ON CONFLICT DO NOTHING`
- `pgmq.enable_notify_insert()` upserts the throttle configuration
- `pgmq.bind_topic()` uses `ON CONFLICT DO NOTHING` for duplicate bindings
- `pgmq.create_fifo_index()` uses `CREATE INDEX IF NOT EXISTS`

You can safely call `ensureQueues` on every application startup without guards or version checks.

For the coordinated family upgrade and both public record migrations, see
[upgrading to 0.6](pgmq-0.6-upgrade.md).
