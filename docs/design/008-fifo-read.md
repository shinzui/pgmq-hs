# Design: FIFO Read Functions (pgmq 1.8.0-1.9.0)

## Overview

pgmq 1.8.0 added FIFO (First-In-First-Out) queue support with message grouping, similar to AWS SQS FIFO queues. Messages can be assigned to groups using the `x-pgmq-group` header, and the FIFO read functions ensure ordered processing within groups.

pgmq 1.9.0 added round-robin FIFO functions for fair distribution across message groups, and **removed** the `conditional` parameter from all FIFO functions (commit 9e9c3dc).

## Upstream Commits

- FIFO support: commit 730f679 "Consider supporting FIFO + message keys (#442)"
- Round-robin FIFO: commit cb5dd01 "update fifo test and migration sql (#475)"
- Remove conditional from FIFO: commit 9e9c3dc "Remove `conditional` param from FIFO (#480)"
- Fix message ordering in round-robin: commit 2129a38 "fix message ordering in `read_grouped_rr` (#477)"

## Upstream API

### FIFO Read Functions (SQS-style batch filling)

```sql
-- Read messages from the same group (fills batch from one group)
pgmq.read_grouped(
    queue_name TEXT,
    vt INTEGER,
    qty INTEGER
) -> SETOF pgmq.message_record

-- Same with polling support
pgmq.read_grouped_with_poll(
    queue_name TEXT,
    vt INTEGER,
    qty INTEGER,
    max_poll_seconds INTEGER DEFAULT 5,
    poll_interval_ms INTEGER DEFAULT 100
) -> SETOF pgmq.message_record
```

### Round-Robin FIFO Functions (pgmq 1.9.0+)

```sql
-- Read with fair distribution across groups (layered round-robin)
pgmq.read_grouped_rr(
    queue_name TEXT,
    vt INTEGER,
    qty INTEGER
) -> SETOF pgmq.message_record

-- Same with polling support
pgmq.read_grouped_rr_with_poll(
    queue_name TEXT,
    vt INTEGER,
    qty INTEGER,
    max_poll_seconds INTEGER DEFAULT 5,
    poll_interval_ms INTEGER DEFAULT 100
) -> SETOF pgmq.message_record
```

### FIFO Index Functions

```sql
-- Create FIFO index for better performance
pgmq.create_fifo_index(queue_name TEXT) -> void

-- Create FIFO indexes on all queues
pgmq.create_fifo_indexes_all() -> void
```

## Message Grouping

Messages are assigned to FIFO groups using the `x-pgmq-group` header:

```sql
-- Send to a specific group
SELECT pgmq.send(
    'my_queue',
    '{"order_id": 123}'::jsonb,
    '{"x-pgmq-group": "customer_456"}'::jsonb
);
```

If no group is specified, messages go to `_default_fifo_group`.

## Behavioral Differences

### `read_grouped` (SQS-style)
- Fills the entire batch from a single message group
- Ensures strict ordering within a group
- Best for processing related messages together

### `read_grouped_rr` (Round-Robin)
- Distributes messages fairly across all groups
- Uses layered round-robin algorithm
- Best for fair processing when multiple groups have pending messages

## Breaking Change in pgmq 1.9.0

The `conditional` parameter was **removed** from all FIFO functions. This was intentional because filtering at read time could break FIFO ordering guarantees.

## Proposed Changes

### 1. Types (`src/Pgmq/Hasql/Statements/Types.hs`)

```haskell
-- | Parameters for FIFO grouped read (pgmq 1.8.0+)
-- Note: conditional parameter was removed in pgmq 1.9.0
data ReadGrouped = ReadGrouped
  { queueName :: !QueueName,
    visibilityTimeout :: !Int32,
    qty :: !Int32
  }
  deriving stock (Generic)

-- | Parameters for FIFO grouped read with polling (pgmq 1.8.0+)
-- Note: conditional parameter was removed in pgmq 1.9.0
data ReadGroupedWithPoll = ReadGroupedWithPoll
  { queueName :: !QueueName,
    visibilityTimeout :: !Int32,
    qty :: !Int32,
    maxPollSeconds :: !Int32,
    pollIntervalMs :: !Int32
  }
  deriving stock (Generic)
```

### 2. Encoders (`src/Pgmq/Hasql/Encoders.hs`)

```haskell
readGroupedEncoder :: E.Params ReadGrouped
readGroupedEncoder =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #visibilityTimeout >$< E.param (E.nonNullable E.int4))
    <> (view #qty >$< E.param (E.nonNullable E.int4))

readGroupedWithPollEncoder :: E.Params ReadGroupedWithPoll
readGroupedWithPollEncoder =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #visibilityTimeout >$< E.param (E.nonNullable E.int4))
    <> (view #qty >$< E.param (E.nonNullable E.int4))
    <> (view #maxPollSeconds >$< E.param (E.nonNullable E.int4))
    <> (view #pollIntervalMs >$< E.param (E.nonNullable E.int4))
```

### 3. Statements (`src/Pgmq/Hasql/Statements/Message.hs`)

```haskell
-- | FIFO read - fills batch from same group (pgmq 1.8.0+)
readGrouped :: Statement ReadGrouped (Vector Message)
readGrouped = Statement sql readGroupedEncoder decoder True
  where
    sql = "select * from pgmq.read_grouped($1,$2,$3)"
    decoder = D.rowVector messageDecoder

-- | FIFO read with polling (pgmq 1.8.0+)
readGroupedWithPoll :: Statement ReadGroupedWithPoll (Vector Message)
readGroupedWithPoll = Statement sql readGroupedWithPollEncoder decoder True
  where
    sql = "select * from pgmq.read_grouped_with_poll($1,$2,$3,$4,$5)"
    decoder = D.rowVector messageDecoder

-- | Round-robin FIFO read (pgmq 1.9.0+)
readGroupedRoundRobin :: Statement ReadGrouped (Vector Message)
readGroupedRoundRobin = Statement sql readGroupedEncoder decoder True
  where
    sql = "select * from pgmq.read_grouped_rr($1,$2,$3)"
    decoder = D.rowVector messageDecoder

-- | Round-robin FIFO read with polling (pgmq 1.9.0+)
readGroupedRoundRobinWithPoll :: Statement ReadGroupedWithPoll (Vector Message)
readGroupedRoundRobinWithPoll = Statement sql readGroupedWithPollEncoder decoder True
  where
    sql = "select * from pgmq.read_grouped_rr_with_poll($1,$2,$3,$4,$5)"
    decoder = D.rowVector messageDecoder
```

### 4. Queue Management (`src/Pgmq/Hasql/Statements/QueueManagement.hs`)

```haskell
-- | Create FIFO index for a queue (pgmq 1.8.0+)
createFifoIndex :: Statement QueueName ()
createFifoIndex = Statement sql queueNameEncoder D.noResult True
  where
    sql = "select from pgmq.create_fifo_index($1)"

-- | Create FIFO indexes for all queues (pgmq 1.8.0+)
createFifoIndexesAll :: Statement () ()
createFifoIndexesAll = Statement sql E.noParams D.noResult True
  where
    sql = "select from pgmq.create_fifo_indexes_all()"
```

### 5. Sessions (`src/Pgmq/Hasql/Sessions.hs`)

Add session functions:
- `readGrouped :: ReadGrouped -> Session (Vector Message)`
- `readGroupedWithPoll :: ReadGroupedWithPoll -> Session (Vector Message)`
- `readGroupedRoundRobin :: ReadGrouped -> Session (Vector Message)`
- `readGroupedRoundRobinWithPoll :: ReadGroupedWithPoll -> Session (Vector Message)`
- `createFifoIndex :: QueueName -> Session ()`
- `createFifoIndexesAll :: Session ()`

### 6. Effectful Integration

Add corresponding GADT constructors and functions to:
- `Pgmq.Effectful.Effect`
- `Pgmq.Effectful.Interpreter`

## Usage Example

```haskell
import Pgmq

-- Send messages with FIFO grouping
sendMessageWithHeaders $ SendMessageWithHeaders
  { queueName = "orders"
  , messageBody = MessageBody $ object ["order_id" .= (123 :: Int)]
  , messageHeaders = MessageHeaders $ object ["x-pgmq-group" .= ("customer_456" :: Text)]
  , delay = Nothing
  }

-- Read with SQS-style batching (all from same group)
msgs <- readGrouped $ ReadGrouped
  { queueName = "orders"
  , visibilityTimeout = 30
  , qty = 10
  }

-- Read with round-robin fairness across groups
msgs <- readGroupedRoundRobin $ ReadGrouped
  { queueName = "orders"
  , visibilityTimeout = 30
  , qty = 10
  }
```

## Version Requirements

- `readGrouped`, `readGroupedWithPoll`: pgmq 1.8.0+
- `readGroupedRoundRobin`, `readGroupedRoundRobinWithPoll`: pgmq 1.9.0+
- `createFifoIndex`, `createFifoIndexesAll`: pgmq 1.8.0+
