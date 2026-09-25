---
okf_version: "0.2"
---

# Files

- [profile.dhall](profile.dhall)

# Bug Report

- [Recoverable PostgreSQL disconnects are classified as permanent](1-disconnect-errors-are-classified-as-permanent.md) - PostgreSQL faults can produce statement errors that isTransient calls permanent even though the same pool recovers.
- [Concurrent reconciliation misreports resource creators](2-concurrent-reconciliation-misreports-creators.md) - Multiple reconcilers can each claim they created the same queue, notification rule, or FIFO index.
- [Partitioned queue notifications bypass the throttle and documented channel](3-partitioned-queue-notifications-bypass-throttle-and-channel.md) - A partitioned queue emits a notification per insert on leaf-partition channels instead of the advertised throttled queue channel.
