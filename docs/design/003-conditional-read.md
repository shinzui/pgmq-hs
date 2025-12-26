# Design: Conditional Read Filtering (pgmq 1.5.0+)

## Overview

pgmq 1.5.0 added support for conditional filtering when reading messages. This allows consumers to filter messages by their body content using JSONB containment operators.

## Upstream API Changes

### Function Signature Changes
```sql
-- read now accepts optional conditional parameter
pgmq.read(
    queue_name TEXT,
    vt INTEGER,
    qty INTEGER,
    conditional JSONB DEFAULT '{}'  -- NEW parameter
) -> SETOF pgmq.message_record

-- read_with_poll also accepts conditional
pgmq.read_with_poll(
    queue_name TEXT,
    vt INTEGER,
    qty INTEGER,
    max_poll_seconds INTEGER DEFAULT 5,
    poll_interval_ms INTEGER DEFAULT 100,
    conditional JSONB DEFAULT '{}'  -- NEW parameter
) -> SETOF pgmq.message_record
```

### Filtering Semantics
The conditional parameter uses PostgreSQL's JSONB containment operator (`@>`):
```sql
WHERE message @> conditional
```

Example filters:
```sql
-- Filter by status
'{"status": "pending"}'

-- Filter by type and priority
'{"type": "email", "priority": 1}'

-- Empty = no filter (match all)
'{}'
```

## Current Implementation Analysis

Looking at the current `ReadWithPollMessage` type in `src/Pgmq/Db/Statements/Types.hs`:
```haskell
data ReadWithPollMessage = ReadWithPollMessage
  { queueName :: !QueueName,
    delay :: !Delay,
    batchSize :: !(Maybe Int32),
    maxPollSeconds :: !Int32,
    pollIntervalMs :: !Int32,
    conditional :: !(Maybe Value)  -- Already exists!
  }
```

The `conditional` field already exists in `ReadWithPollMessage`. However, looking at the encoder and SQL:
- `readWithPollEncoder` already encodes the conditional parameter
- The SQL uses 6 parameters including conditional

For `ReadMessage`, the conditional parameter is **missing**.

## Proposed Changes

### 1. Types (`src/Pgmq/Db/Statements/Types.hs`)

Add `conditional` field to `ReadMessage`:

```haskell
data ReadMessage = ReadMessage
  { queueName :: !QueueName,
    delay :: !Delay,
    batchSize :: !(Maybe Int32),
    conditional :: !(Maybe Value)  -- NEW: optional JSONB filter
  }
  deriving stock (Generic)
```

### 2. Encoders (`src/Pgmq/Db/Encoders.hs`)

Update `readMessageEncoder` to include conditional:

```haskell
readMessageEncoder :: E.Params ReadMessage
readMessageEncoder =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #delay >$< E.param (E.nonNullable E.int4))
    <> (view #batchSize >$< E.param (E.nullable E.int4))
    <> (view #conditional >$< E.param (E.nullable E.jsonb))  -- NEW
```

### 3. Statements (`src/Pgmq/Db/Statements/Message.hs`)

Update `readMessage` SQL to use 4 parameters:

```haskell
readMessage :: Statement ReadMessage (Vector Message)
readMessage = Statement sql readMessageEncoder decoder True
  where
    sql = "select * from pgmq.read($1,$2,$3,$4)"  -- Changed from 3 to 4 params
    decoder = D.rowVector messageDecoder
```

## Backwards Compatibility

- The `ReadMessage` type gains a new field `conditional :: Maybe Value`
- This is a **breaking change** for pattern matching
- Existing code can set `conditional = Nothing` to maintain previous behavior
- Default value `'{}'` in SQL means no filter when NULL is passed

## Usage Examples

```haskell
-- Read all messages (no filter)
let query = ReadMessage
      { queueName = qn
      , delay = 30
      , batchSize = Just 10
      , conditional = Nothing
      }

-- Read only pending messages
let query = ReadMessage
      { queueName = qn
      , delay = 30
      , batchSize = Just 10
      , conditional = Just (object ["status" .= ("pending" :: Text)])
      }

-- Read high priority emails
let query = ReadMessage
      { queueName = qn
      , delay = 30
      , batchSize = Just 10
      , conditional = Just (object ["type" .= ("email" :: Text), "priority" .= (1 :: Int)])
      }
```

## Notes

- The `ReadWithPollMessage` type already has `conditional` support - no changes needed there
- Only `ReadMessage` needs to be updated
