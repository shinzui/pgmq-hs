# Design: Batch Visibility Timeout (pgmq 1.8.0+)

## Overview

pgmq 1.8.0 added support for updating visibility timeout on multiple messages at once via a batch variant of `set_vt`.

## Upstream API Changes

### New Function Overload
```sql
-- Existing single message
pgmq.set_vt(queue_name TEXT, msg_id BIGINT, vt INTEGER) -> SETOF pgmq.message_record

-- NEW batch variant (pgmq 1.8.0+)
pgmq.set_vt(queue_name TEXT, msg_ids BIGINT[], vt INTEGER) -> SETOF pgmq.message_record
```

## Proposed Changes

### 1. Types (`src/Pgmq/Db/Statements/Types.hs`)

Add batch visibility timeout type:

```haskell
-- | Batch visibility timeout update (pgmq 1.8.0+)
data BatchVisibilityTimeoutQuery = BatchVisibilityTimeoutQuery
  { queueName :: !QueueName,
    messageIds :: ![MessageId],
    visibilityTimeoutOffset :: !Int32
  }
  deriving stock (Generic)
```

### 2. Encoders (`src/Pgmq/Db/Encoders.hs`)

Add encoder:

```haskell
batchVisibilityTimeoutQueryEncoder :: E.Params BatchVisibilityTimeoutQuery
batchVisibilityTimeoutQueryEncoder =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #messageIds >$< E.param (E.nonNullable (E.array (E.dimension foldl' (E.element (E.nonNullable messageIdValue))))))
    <> (view #visibilityTimeoutOffset >$< E.param (E.nonNullable E.int4))
```

### 3. Statements (`src/Pgmq/Db/Statements/Message.hs`)

Add batch statement:

```haskell
-- | Batch update visibility timeout (pgmq 1.8.0+)
batchChangeVisibilityTimeout :: Statement BatchVisibilityTimeoutQuery (Vector Message)
batchChangeVisibilityTimeout = Statement sql batchVisibilityTimeoutQueryEncoder decoder True
  where
    sql = "select * from pgmq.set_vt($1,$2,$3)"
    decoder = D.rowVector messageDecoder
```

### 4. Sessions & Exports

Add session function and re-export.

## Backwards Compatibility

This is an additive change - the existing single-message `changeVisibilityTimeout` is unchanged.
