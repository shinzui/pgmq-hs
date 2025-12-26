# Design: Deprecate detach_archive (pgmq 2.0)

## Overview

The `pgmq.detach_archive()` function is now a no-op and will be removed in pgmq 2.0. We should deprecate it in the Haskell library.

## Upstream Status

From the pgmq source:
```sql
-- This function is a no-op. It is only here for backwards compatibility
-- with previous versions of pgmq. It will be removed in a future version.
CREATE FUNCTION pgmq.detach_archive(queue_name TEXT)
RETURNS VOID AS $$
BEGIN
    -- no-op
END;
$$ LANGUAGE plpgsql;
```

## Proposed Changes

### 1. Add GHC Deprecation Pragma

In `src/Pgmq/Db/Sessions.hs`:

```haskell
{-# DEPRECATED detachArchive "detach_archive is a no-op in pgmq and will be removed in pgmq 2.0" #-}
detachArchive :: QueueName -> Session ()
detachArchive q = statement q Db.detachArchive
```

### 2. Update Module Exports

In `src/Pgmq.hs`, add a comment noting deprecation:

```haskell
module Pgmq
  ( -- * Queue Management
    createQueue,
    dropQueue,
    createPartitionedQueue,
    createUnloggedQueue,
    detachArchive,  -- DEPRECATED: no-op, will be removed in pgmq 2.0
    ...
```

## Implementation Notes

- The function still works (it's a no-op in upstream)
- Users will see a deprecation warning when compiling
- Can be removed when pgmq 2.0 is released

## Alternative

We could also remove the function entirely, but since it's still present in upstream
(even as a no-op), keeping it with a deprecation warning is more graceful.
