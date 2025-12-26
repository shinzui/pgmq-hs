# Design: Queue Visible Length in Metrics (pgmq 1.5.0+)

## Overview

pgmq 1.5.0 added `queue_visible_length` to the `metrics_result` type. This field represents the count of messages currently visible (i.e., not being processed by another consumer).

## Upstream API Changes

### Schema Change
The `pgmq.metrics_result` type now includes `queue_visible_length`:
```sql
CREATE TYPE pgmq.metrics_result AS (
    queue_name text,
    queue_length bigint,
    newest_msg_age_sec int,
    oldest_msg_age_sec int,
    total_messages bigint,
    scrape_time timestamp with time zone,
    queue_visible_length bigint  -- NEW: 7th column
);
```

The `queue_visible_length` is computed as:
```sql
count(CASE WHEN vt <= NOW() THEN 1 END) as queue_visible_length
```

This counts only messages where the visibility timeout has expired (i.e., they are available for reading).

## Proposed Changes

### 1. Types (`src/Pgmq/Db/Statements/Types.hs`)

Update `QueueMetrics` to include `queueVisibleLength`:

```haskell
data QueueMetrics = QueueMetrics
  { queueName :: !Text,
    queueLength :: !Int64,
    newestMsgAgeSec :: !(Maybe Int32),
    oldestMsgAgeSec :: !(Maybe Int32),
    totalMessages :: !Int64,
    scrapeTime :: !UTCTime,
    queueVisibleLength :: !Int64  -- NEW: visible message count
  }
  deriving stock (Generic, Show)
```

### 2. Decoders (`src/Pgmq/Db/Decoders.hs`)

Update `queueMetricsDecoder` to decode the 7th column:

```haskell
queueMetricsDecoder :: D.Row QueueMetrics
queueMetricsDecoder =
  QueueMetrics
    <$> D.column (D.nonNullable D.text)
    <*> D.column (D.nonNullable D.int8)
    <*> D.column (D.nullable D.int4)
    <*> D.column (D.nullable D.int4)
    <*> D.column (D.nonNullable D.int8)
    <*> D.column (D.nonNullable D.timestamptz)
    <*> D.column (D.nonNullable D.int8)  -- NEW: queue_visible_length
```

## Backwards Compatibility

- The `QueueMetrics` type gains a new field `queueVisibleLength :: Int64`
- This is a **breaking change** for pattern matching
- Semantic meaning is additive (more information, no behavior change)

## Use Cases

- Monitor queue backlog: `queueLength - queueVisibleLength` = messages currently being processed
- Autoscaling: scale consumers based on visible (available) work
- Health checks: alert if visible length grows beyond threshold
