# Design: Queue Notifications (pgmq 1.7.0-1.8.0)

## Overview

pgmq 1.7.0 added support for PostgreSQL LISTEN/NOTIFY to receive real-time notifications when messages are inserted into a queue. pgmq 1.8.0 added throttling support.

## Upstream API Changes

### Functions
```sql
-- Enable notifications for a queue (pgmq 1.7.0+)
-- throttle_interval_ms added in pgmq 1.8.0
pgmq.enable_notify_insert(queue_name TEXT, throttle_interval_ms INTEGER DEFAULT 250) -> VOID

-- Disable notifications for a queue
pgmq.disable_notify_insert(queue_name TEXT) -> VOID
```

### Schema
```sql
-- New table for throttling (pgmq 1.8.0)
CREATE UNLOGGED TABLE pgmq.notify_insert_throttle (
    queue_name VARCHAR UNIQUE NOT NULL,
    throttle_interval_ms INTEGER NOT NULL DEFAULT 0,
    last_notified_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT to_timestamp(0)
);
```

## Proposed Changes

### 1. Types (`src/Pgmq/Db/Statements/Types.hs`)

Add notification configuration type:

```haskell
-- | Enable queue notifications (pgmq 1.7.0+)
data EnableNotifyInsert = EnableNotifyInsert
  { queueName :: !QueueName,
    throttleIntervalMs :: !(Maybe Int32) -- ^ Nothing = default 250ms
  }
  deriving stock (Generic)
```

### 2. Encoders (`src/Pgmq/Db/Encoders.hs`)

```haskell
enableNotifyInsertEncoder :: E.Params EnableNotifyInsert
enableNotifyInsertEncoder =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #throttleIntervalMs >$< E.param (E.nullable E.int4))
```

### 3. Statements (`src/Pgmq/Db/Statements/QueueManagement.hs`)

```haskell
enableNotifyInsert :: Statement EnableNotifyInsert ()
enableNotifyInsert = Statement sql enableNotifyInsertEncoder D.noResult True
  where
    sql = "SELECT FROM pgmq.enable_notify_insert($1, $2)"

disableNotifyInsert :: Statement QueueName ()
disableNotifyInsert = Statement sql queueNameEncoder D.noResult True
  where
    sql = "SELECT FROM pgmq.disable_notify_insert($1)"
```

### 4. Sessions & Exports

Add session functions:
- `enableNotifyInsert :: EnableNotifyInsert -> Session ()`
- `disableNotifyInsert :: QueueName -> Session ()`

## Note on LISTEN/NOTIFY

This implementation only enables/disables notifications at the database level.
Actual listening for notifications requires using hasql's notification support
or raw PostgreSQL connection handling, which is outside the scope of this library.

The channel name for a queue is `pgmq_<queue_name>`.
