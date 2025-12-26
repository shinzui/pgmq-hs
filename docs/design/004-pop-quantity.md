# Design: Pop with Quantity (pgmq 1.7.0+)

## Overview

pgmq 1.7.0 enhanced the `pop` function to support popping multiple messages at once via a `qty` parameter. Previously, `pop` could only return a single message.

## Upstream API Changes

### Function Signature Change
```sql
-- Before (pgmq < 1.7.0)
pgmq.pop(queue_name TEXT) -> SETOF pgmq.message_record

-- After (pgmq 1.7.0+)
pgmq.pop(queue_name TEXT, qty INTEGER DEFAULT 1) -> SETOF pgmq.message_record
```

The `qty` parameter defaults to 1, maintaining backwards compatibility.

## Current Implementation

Looking at `src/Pgmq/Db/Statements/Message.hs`:
```haskell
pop :: Statement QueueName Message
pop = Statement sql queueNameEncoder decoder True
  where
    sql = "select * from pgmq.pop($1)"
    decoder = D.singleRow messageDecoder
```

Current issues:
1. Only passes queue name, no qty parameter
2. Uses `D.singleRow` - can only decode one message
3. Return type is `Message`, not `Vector Message`

## Proposed Changes

### 1. Types (`src/Pgmq/Db/Statements/Types.hs`)

Add a new `PopMessage` type:

```haskell
-- | Parameters for popping messages from a queue (pgmq 1.7.0+)
data PopMessage = PopMessage
  { queueName :: !QueueName,
    qty :: !(Maybe Int32)  -- Nothing = default to 1
  }
  deriving stock (Generic)
```

### 2. Encoders (`src/Pgmq/Db/Encoders.hs`)

Add encoder for `PopMessage`:

```haskell
popMessageEncoder :: E.Params PopMessage
popMessageEncoder =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #qty >$< E.param (E.nullable E.int4))
```

### 3. Statements (`src/Pgmq/Db/Statements/Message.hs`)

Update `pop` statement to use new type and return vector:

```haskell
-- | Pop messages from a queue (atomic read + delete)
-- https://tembo.io/pgmq/api/sql/functions/#pop
-- Note: qty parameter added in pgmq 1.7.0
pop :: Statement PopMessage (Vector Message)
pop = Statement sql popMessageEncoder decoder True
  where
    sql = "select * from pgmq.pop($1,$2)"
    decoder = D.rowVector messageDecoder
```

### 4. Sessions (`src/Pgmq/Db/Sessions.hs`)

Update session function:

```haskell
pop :: PopMessage -> Session (Vector Message)
pop query = statement query Msg.pop
```

### 5. Re-exports (`src/Pgmq.hs`)

Export `PopMessage` type.

## Backwards Compatibility

This is a **breaking change**:
- `pop` now takes `PopMessage` instead of `QueueName`
- `pop` returns `Vector Message` instead of `Message`

Users need to update their code:
```haskell
-- Before
msg <- pop queueName

-- After
msgs <- pop (PopMessage queueName (Just 1))
let msg = V.head msgs  -- or handle empty vector
```

## Alternative: Keep Old Function

We could keep the old `pop` signature and add a new `popN` function:
- `pop :: QueueName -> Session Message` (old behavior)
- `popN :: PopMessage -> Session (Vector Message)` (new behavior)

This would be more backwards compatible but adds API surface.

## Recommendation

Go with the breaking change approach since:
1. It aligns with the upstream API
2. The new behavior is strictly more general
3. Users can easily adapt by wrapping with `V.head` if they only want one message
