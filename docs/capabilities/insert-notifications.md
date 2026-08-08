---
title: "Insert notifications (LISTEN/NOTIFY)"
type: Capability
description: "Enable throttled PostgreSQL LISTEN/NOTIFY on queue inserts and compute the exact channel name, so consumers can wake on arrival instead of polling tightly."
generated:
  by: adopt-capabilities/0.9.2
  at: "2026-08-08T00:00:00Z"
capabilityId: CAP-4
provider: mori://shinzui/pgmq-hs
status: shipped
stability: experimental
since: "0.1.0.0"
packages:
  - pgmq-hasql
  - pgmq-core
requires:
  - CAP-1
interface:
  - Pgmq
  - Pgmq.Types
evidence:
  - kind: test
    resource: pgmq-hasql/test/NotifyChannelSpec.hs
    proves: A real notification arrives on exactly the channel notifyChannelName computes, and the previously-documented name receives nothing.
  - kind: test
    resource: pgmq-hasql/test/NotifyRaceSpec.hs
    proves: Concurrent enable/notify paths behave under contention.
  - kind: guide
    resource: docs/design/015-notification-delivery-contract.md
    proves: The full delivery contract — throttling, poll-fallback requirement, and crash fail-open semantics.
  - kind: guide
    resource: docs/design/006-queue-notifications.md
    proves: The corrected channel-name derivation and the notification design.
---

# Insert notifications (LISTEN/NOTIFY)

On top of the [core client](message-queue-client.md), pgmq-hasql lets a consumer enable
PostgreSQL insert notifications on a queue and listen for arrivals, with a configurable
throttle so a burst of inserts does not become a storm of notifications. The channel name
is computed for you by `pgmq-core`'s `notifyChannelName`. This is a distinct adoption —
notify-driven consumption, proven by its own channel and race tests — so it is its own
capability.

What it provides:

- `enableNotifyInsert`, `disableNotifyInsert` — turn insert notifications on/off for a
  queue, with an optional throttle interval (defaults to 250 ms when `Nothing`).
- `listNotifyInsertThrottles`, `updateNotifyInsert` — inspect and change the throttle.
- `Pgmq.Types.notifyChannelName :: QueueName -> Text` — the one correct way to compute the
  `pgmq.q_<lowercased name>.INSERT` channel to `LISTEN` on.

## Shape

```haskell
import Pgmq (enableNotifyInsert, EnableNotifyInsert (..))
import Pgmq.Types (notifyChannelName)

Pool.use pool $ enableNotifyInsert (EnableNotifyInsert q Nothing)   -- 250 ms throttle
-- LISTEN on: notifyChannelName q
```

## Limits

- **Notifications are fire-and-forget; a poll fallback is mandatory.** PostgreSQL does not
  queue NOTIFY for disconnected listeners, and the throttle interval suppresses
  notifications by design. A consumer that relies on LISTEN alone will miss messages; keep
  `readWithPoll` as the backstop.
- **The channel name was wrong before 0.5.0.0.** Earlier releases documented
  `pgmq_<queue_name>`, a channel nothing ever publishes to. `notifyChannelName` (0.5.0.0)
  is now the contract; do not assemble the name by hand.
- **Crash-safe delivery depends on the pgmq-migration schema.** On a database installed
  through [extension-free schema installation](extension-free-schema-install.md), migration
  `0003` makes the trigger fail open after a crash truncates the `UNLOGGED` throttle table.
  On a stock upstream 1.11.0 extension install, notifications can stop silently after a
  crash until a reconcile re-enables them, and concurrent enable calls can race
  (SQLSTATE 42710).
- Pre-1.0 and uniformly `experimental`.
