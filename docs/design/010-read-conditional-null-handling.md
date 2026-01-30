# Design Document 010: Fix readMessage NULL conditional handling

## Status
**Resolved** - Fixed by using 3-param pgmq.read() version

## Problem Statement

When `readMessage` is called with `conditional = Nothing`, the hasql encoder sends `null` as the fourth parameter to `pgmq.read($1, $2, $3, $4)`. This causes the SQL query to return 0 rows because `message @> NULL` evaluates to `NULL` (not `TRUE`), so the WHERE clause filters out all messages.

### Symptoms

- `readMessage` with `conditional = Nothing` returns empty results
- No error is thrown - it simply appears that the queue is empty
- Sending messages works, but reading them back returns nothing

### Root Cause

The pgmq 1.9.0 schema has multiple overloaded `pgmq.read` functions:

1. `pgmq.read(queue_name TEXT, vt INTEGER, qty INTEGER)` - 3 args
2. `pgmq.read(queue_name TEXT, vt INTEGER, qty INTEGER, conditional JSONB)` - 4 args

The 4-arg version includes this WHERE clause:
```sql
WHERE vt <= clock_timestamp() AND message @> $2
```

When `conditional` is `NULL`, the expression `message @> NULL` evaluates to `NULL` (not `TRUE` or `FALSE`), causing the WHERE clause to effectively filter out ALL rows.

### Previous Code (pgmq-hasql)

```haskell
-- pgmq-hasql/src/Pgmq/Hasql/Statements/Message.hs
readMessage :: Statement ReadMessage (Vector Message)
readMessage = Statement sql readMessageEncoder decoder True
  where
    sql = "select * from pgmq.read($1,$2,$3,$4)"  -- Always 4 args
    decoder = D.rowVector messageDecoder
```

The encoder always sent 4 parameters including the nullable conditional:
```haskell
readMessageEncoder =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #delay >$< E.param (E.nonNullable E.int4))
    <> (view #batchSize >$< E.param (E.nullable E.int4))
    <> (view #conditional >$< E.param (E.nullable E.jsonb)) -- pgmq 1.5.0+
```

## Solution Implemented

Changed `readMessage` to use the 3-param version which doesn't have conditional filtering:

```haskell
-- pgmq-hasql/src/Pgmq/Hasql/Statements/Message.hs
readMessage :: Statement ReadMessage (Vector Message)
readMessage = Statement sql readMessageEncoder decoder True
  where
    sql = "select * from pgmq.read($1,$2,$3)"  -- 3 args, no conditional
    decoder = D.rowVector messageDecoder
```

And updated the encoder to only send 3 parameters:
```haskell
readMessageEncoder :: E.Params ReadMessage
readMessageEncoder =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #delay >$< E.param (E.nonNullable E.int4))
    <> (view #batchSize >$< E.param (E.nullable E.int4))
```

A separate encoder was added for 4-param conditional reads:
```haskell
readMessageConditionalEncoder :: E.Params ReadMessage
readMessageConditionalEncoder =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #delay >$< E.param (E.nonNullable E.int4))
    <> (view #batchSize >$< E.param (E.nullable E.int4))
    <> (view #conditional >$< E.param (E.nullable E.jsonb))
```

## Alternatives Considered

### Option A: Two Separate Functions
Create `readMessage` (no conditional) and `readMessageConditional` (with conditional filter).

**Pros:** Explicit API, type-safe
**Cons:** API surface change, users must choose which function

### Option B: COALESCE in SQL (not viable)
Unlike the delay case, we cannot use `COALESCE($4, '{}')` because an empty object `{}` would match all messages (every JSON object contains `{}`), which is different semantics than "no filter".

### Option C: Dynamic SQL selection (not viable)
Hasql uses static SQL statements, so we cannot conditionally change the SQL at runtime based on whether conditional is NULL.

## Impact

- The `conditional` field in `ReadMessage` is now ignored by `readMessage`
- Users who need conditional filtering should use a separate statement (to be added)
- This is a backwards-compatible change for users who weren't using conditional filtering

## Related Issues

- Design Document 009: Similar NULL handling issue with `sendMessage` delay parameter
