# Design Document 009: Fix sendMessage NULL delay handling

## Status
**Resolved** - Fixed with COALESCE approach (Option C)

## Problem Statement

When `sendMessage` is called with `delay = Nothing`, the hasql encoder sends `null` as the third parameter to `pgmq.send($1, $2, $3)`. This causes PostgreSQL to match the wrong function overload, resulting in a NOT NULL constraint violation on the `vt` column.

### Error Message
```
null value in column "vt" of relation "q_test_..." violates not-null constraint
Failing row contains (1, 0, 2026-01-30 18:34:04.641686+00, null, "test", null).
```

### Root Cause

The pgmq schema has multiple overloaded `pgmq.send` functions:

1. `pgmq.send(queue_name TEXT, msg JSONB)` - 2 args, no delay
2. `pgmq.send(queue_name TEXT, msg JSONB, delay INTEGER)` - 3 args with delay
3. `pgmq.send(queue_name TEXT, msg JSONB, headers JSONB)` - 3 args with headers

When `null` is passed as the third argument, PostgreSQL's function resolution picks the headers version (since JSONB accepts null), which then forwards to the 4-arg internal implementation with null headers, causing the `vt` timestamp to be null.

### Current Code (pgmq-hasql)

```haskell
-- pgmq-hasql/src/Pgmq/Hasql/Statements/Message.hs
sendMessage :: Statement SendMessage MessageId
sendMessage = Statement sql sendMessageEncoder decoder True
  where
    sql = "select * from pgmq.send($1, $2, $3)"  -- Always 3 args
    decoder = D.singleRow messageIdDecoder
```

```haskell
-- SendMessage type
data SendMessage = SendMessage
  { queueName :: !QueueName,
    messageBody :: !MessageBody,
    delay :: !(Maybe Delay)  -- When Nothing, encoder sends null
  }
```

## Proposed Solutions

### Option A: Conditional SQL Based on Delay

Create two statements and use the appropriate one based on whether delay is provided:

```haskell
sendMessage :: Statement SendMessage MessageId
sendMessage = Statement sql sendMessageEncoder decoder True
  where
    -- Use 2-arg version when no delay
    sql = "select * from pgmq.send($1, $2) where $3 is null \
          \union all \
          \select * from pgmq.send($1, $2, $3) where $3 is not null"
```

**Pros:** Single statement
**Cons:** Complex SQL, may have performance implications

### Option B: Separate Statements (Recommended)

Use `sendMessage` for no-delay case and `sendMessageForLater` for delayed messages:

1. Change `sendMessage` to use 2-arg SQL:
```haskell
sendMessage :: Statement SendMessageNoDelay MessageId
sendMessage = Statement sql sendMessageNoDelayEncoder decoder True
  where
    sql = "select * from pgmq.send($1, $2)"

data SendMessageNoDelay = SendMessageNoDelay
  { queueName :: !QueueName,
    messageBody :: !MessageBody
  }
```

2. Keep `sendMessageForLater` as-is for delayed messages

**Pros:** Clean separation, explicit API
**Cons:** API change, users must choose which function to call

### Option C: COALESCE in SQL

Default null delay to 0:

```haskell
sendMessage :: Statement SendMessage MessageId
sendMessage = Statement sql sendMessageEncoder decoder True
  where
    sql = "select * from pgmq.send($1, $2, coalesce($3, 0))"
```

**Pros:** Minimal change, backwards compatible
**Cons:** Hides the API intention (null means "no delay" which equals 0)

### Option D: Cast to INTEGER

Force the type resolution to use the delay overload:

```haskell
sendMessage :: Statement SendMessage MessageId
sendMessage = Statement sql sendMessageEncoder decoder True
  where
    sql = "select * from pgmq.send($1, $2, $3::integer)"
```

**Pros:** Explicit type, PostgreSQL will handle null as expected
**Cons:** Still passes null to the delay version (needs verification)

## Recommendation

**Option C (COALESCE)** is recommended for immediate fix as it:
1. Requires minimal code change
2. Is backwards compatible with existing API
3. Correctly handles the semantic meaning (no delay = 0 delay)

Long-term, consider **Option B** for a cleaner API that makes the delay optional at the type level.

## Workaround (Current)

Until fixed, callers should use `delay = Just 0` instead of `delay = Nothing`:

```haskell
Sessions.sendMessage $
  SendMessage
    { queueName = queueName,
      messageBody = MessageBody payload,
      delay = Just 0  -- Use explicit 0 instead of Nothing
    }
```

## Affected Versions

- pgmq-hasql: All versions
- pgmq-migration: v1.9.0 schema

## Related

- Similar issue may affect `batchSendMessage` with null delay
