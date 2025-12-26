# Design: Message Headers Support (pgmq 1.5.0+)

## Overview

pgmq 1.5.0 added support for message headers - a separate JSONB column for metadata that is distinct from the message body. This is useful for routing, tracing, and other metadata that shouldn't pollute the message payload.

## Upstream API Changes

### Schema Change
The `pgmq.message_record` type now includes `headers`:
```sql
CREATE TYPE pgmq.message_record AS (
    msg_id BIGINT,
    read_ct INTEGER,
    enqueued_at TIMESTAMP WITH TIME ZONE,
    vt TIMESTAMP WITH TIME ZONE,
    message JSONB,
    headers JSONB  -- NEW: 6th column
);
```

### Send Function Overloads
```sql
-- New overloads with headers parameter
pgmq.send(queue_name, msg, headers JSONB) -> SETOF BIGINT
pgmq.send(queue_name, msg, headers JSONB, delay INTEGER) -> SETOF BIGINT

-- Batch equivalents
pgmq.send_batch(queue_name, msgs[], headers[]) -> SETOF BIGINT
pgmq.send_batch(queue_name, msgs[], headers[], delay INTEGER) -> SETOF BIGINT
```

## Proposed Changes

### 1. Types (`src/Pgmq/Types.hs`)

Add `headers` field to `Message`:

```haskell
-- | https://tembo.io/pgmq/api/sql/types/
data Message = Message
  { messageId :: !MessageId,
    visibilityTime :: !UTCTime,
    enqueuedAt :: !UTCTime,
    readCount :: !Int64,
    body :: !MessageBody,
    headers :: !(Maybe Value)  -- NEW: nullable JSONB
  }
  deriving stock (Eq, Generic, Show)
```

Add `MessageHeaders` newtype for type safety (optional but consistent with `MessageBody`):

```haskell
newtype MessageHeaders = MessageHeaders {unMessageHeaders :: Value}
  deriving newtype (Eq, Ord, FromJSON, ToJSON)
  deriving stock (Show, Generic)
```

### 2. Types (`src/Pgmq/Db/Statements/Types.hs`)

Add new types for header-aware send operations:

```haskell
data SendMessageWithHeaders = SendMessageWithHeaders
  { queueName :: !QueueName,
    messageBody :: !MessageBody,
    messageHeaders :: !MessageHeaders,
    delay :: !(Maybe Delay)
  }
  deriving stock (Generic)

data SendMessageWithHeadersForLater = SendMessageWithHeadersForLater
  { queueName :: !QueueName,
    messageBody :: !MessageBody,
    messageHeaders :: !MessageHeaders,
    scheduledAt :: !UTCTime
  }
  deriving stock (Generic)

data BatchSendMessageWithHeaders = BatchSendMessageWithHeaders
  { queueName :: !QueueName,
    messageBodies :: ![MessageBody],
    messageHeaders :: ![MessageHeaders],
    delay :: !(Maybe Delay)
  }
  deriving stock (Generic)

data BatchSendMessageWithHeadersForLater = BatchSendMessageWithHeadersForLater
  { queueName :: !QueueName,
    messageBodies :: ![MessageBody],
    messageHeaders :: ![MessageHeaders],
    scheduledAt :: !UTCTime
  }
  deriving stock (Generic)
```

### 3. Decoders (`src/Pgmq/Db/Decoders.hs`)

Update `messageDecoder` to decode the 6th column (headers):

```haskell
messageDecoder :: D.Row Message
messageDecoder =
  Message
    <$> messageIdDecoder
    <*> D.column (D.nonNullable D.timestamptz)  -- vt
    <*> D.column (D.nonNullable D.timestamptz)  -- enqueued_at
    <*> D.column (D.nonNullable D.int8)         -- read_ct
    <*> (MessageBody <$> D.column (D.nonNullable D.jsonb))  -- message
    <*> D.column (D.nullable D.jsonb)           -- headers (NEW)
```

### 4. Encoders (`src/Pgmq/Db/Encoders.hs`)

Add encoder for `MessageHeaders`:

```haskell
messageHeadersValue :: E.Value MessageHeaders
messageHeadersValue = unMessageHeaders >$< E.jsonb

-- Encoder for SendMessageWithHeaders
sendMessageWithHeadersEncoder :: E.Params SendMessageWithHeaders
sendMessageWithHeadersEncoder =
  (view #queueName >$< E.param (E.nonNullable queueNameValue))
    <> (view #messageBody >$< E.param (E.nonNullable messageBodyValue))
    <> (view #messageHeaders >$< E.param (E.nonNullable messageHeadersValue))
    <> (view #delay >$< E.param (E.nullable E.int4))

-- Similar encoders for other header-aware types...
```

### 5. Statements (`src/Pgmq/Db/Statements/Message.hs`)

Add new statements for header-aware sends:

```haskell
-- Send with headers (uses 4-param overload)
sendMessageWithHeaders :: Statement SendMessageWithHeaders MessageId
sendMessageWithHeaders = Statement sql sendMessageWithHeadersEncoder decoder True
  where
    sql = "SELECT * FROM pgmq.send($1, $2, $3, $4)"
    decoder = D.singleRow messageIdDecoder

-- Batch send with headers
batchSendMessageWithHeaders :: Statement BatchSendMessageWithHeaders [MessageId]
batchSendMessageWithHeaders = Statement sql batchSendMessageWithHeadersEncoder decoder True
  where
    sql = "SELECT * FROM pgmq.send_batch($1, $2, $3, $4)"
    decoder = D.rowList messageIdDecoder
```

### 6. Sessions (`src/Pgmq/Db/Sessions.hs`)

Export new session functions:

```haskell
sendMessageWithHeaders :: SendMessageWithHeaders -> Session MessageId
sendMessageWithHeaders = statement M.sendMessageWithHeaders

batchSendMessageWithHeaders :: BatchSendMessageWithHeaders -> Session [MessageId]
batchSendMessageWithHeaders = statement M.batchSendMessageWithHeaders
```

### 7. Main Module (`src/Pgmq.hs`)

Re-export new types and functions:

```haskell
module Pgmq
  ( -- ... existing exports
    -- Message Headers (NEW)
    MessageHeaders (..),
    SendMessageWithHeaders (..),
    SendMessageWithHeadersForLater (..),
    BatchSendMessageWithHeaders (..),
    BatchSendMessageWithHeadersForLater (..),
    sendMessageWithHeaders,
    sendMessageWithHeadersForLater,
    batchSendMessageWithHeaders,
    batchSendMessageWithHeadersForLater,
  )
```

## Backwards Compatibility

- The `Message` type gains a new field `headers :: Maybe Value`
  - This is a **breaking change** for pattern matching
  - Library users will need to update pattern matches
- Existing `sendMessage` functions continue to work (headers will be NULL)
- New functions are additive

## Testing Considerations

1. Test sending messages with and without headers
2. Test reading messages returns correct headers
3. Test batch operations with headers arrays
4. Test that headers are preserved through archive operations

## SQL Parameter Mapping

| Haskell Function | SQL Call | Parameters |
|-----------------|----------|------------|
| `sendMessage` | `pgmq.send($1, $2, $3)` | queue, msg, delay |
| `sendMessageWithHeaders` | `pgmq.send($1, $2, $3, $4)` | queue, msg, headers, delay |
| `batchSendMessage` | `pgmq.send_batch($1, $2, $3)` | queue, msgs[], delay |
| `batchSendMessageWithHeaders` | `pgmq.send_batch($1, $2, $3, $4)` | queue, msgs[], headers[], delay |
