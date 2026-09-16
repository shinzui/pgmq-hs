---
okf_version: "0.2"
---

# Files

- [profile.dhall](profile.dhall)

# Review

- [pgmq-config from v0.4.0.1 to v0.6.1.0 — the reconciler can create over a mixed-case alias and overclaims on drift](pgmq-config-v0-4-0-1-to-v0-6-1-0.md) - The single-core reconciler, throttle drift, FIFO presence, and foreign-name handling are correct, but a declared lowercase name colliding with a foreign mixed-case row is created over the shared table, and the drift Haddock and user guide overclaim.
- [pgmq-core from v0.4.0.1 to v0.6.1.0 — validation and the channel helper are correct, with one documentation gap](pgmq-core-v0-4-0-1-to-v0-6-1-0.md) - Lowercase-only queue-name validation and the hand-written FromJSON match the server's behavior, and notifyChannelName matches the trigger for ordinary queues; its Haddock does not say that a partitioned queue's channel is never published to.
- [pgmq-effectful from v0.4.0.1 to v0.6.1.0 — new operations, transient classification, and spans are correct](pgmq-effectful-v0-4-0-1-to-v0-6-1-0.md) - Every new effect constructor is dispatched by both interpreters with the right session and span label, and the transient SQLSTATE whitelist matches design note 017; nothing to act on.
- [pgmq-hasql from v0.4.0.1 to v0.6.1.0 — NULL coalescing, grouped heads, and metrics projection are correct](pgmq-hasql-v0-4-0-1-to-v0-6-1-0.md) - Every client SQL change matches the vendored function it calls and the encoders bind in the right order; the only remark is that the test suites create the shared ephemeral root with the umask rather than the 0700 the changelog claims.
- [pgmq-migration from v0.4.0.1 to v0.6.1.0 — the notify fail-open storms on partitioned queues](pgmq-migration-v0-4-0-1-to-v0-6-1-0.md) - Migrations 0004 and 0005 match upstream byte-for-byte and 0006 preserves the partition guards, but 0003's fail-open notifies unthrottled on every insert into a partitioned queue, on a per-partition channel.

