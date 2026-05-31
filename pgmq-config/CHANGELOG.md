# Revision history for pgmq-config

## 0.3.0.0 -- 2026-05-31

* Version bump only — coordinated release with pgmq-effectful 0.3.0.0.
  No source-level changes since 0.2.0.0.
* Bumped `pgmq-core`, `pgmq-hasql`, and `pgmq-effectful` dependency
  bounds to `>=0.3 && <0.4`.

## 0.2.0.0 -- 2026-04-23

Version unified with the rest of the pgmq-hs packages; no pgmq-config
0.1.4.0 was published to Hackage.

### Bug Fixes

* `ensureQueues` and `ensureQueuesEff` are now truly idempotent. Previously they
  issued `pgmq.create`, `pgmq.enable_notify_insert`, `pgmq.create_fifo_index`, and
  `pgmq.bind_topic` unconditionally on every call, which worked by accident for
  standard/unlogged queues but caused trigger recreation on every boot and failed outright
  for partitioned queues (because `pg_partman.create_parent` raises on re-registration).
  Both entry points now route through the existing state-checking reconciliation path.

## 0.1.3.0 -- 2026-03-12

* Initial release
* Declarative queue configuration DSL (QueueConfig, QueueType, etc.)
* Smart constructors: standardQueue, unloggedQueue, partitionedQueue
* Modifiers: withNotifyInsert, withFifoIndex, withTopicBinding
* Reconciliation: ensureQueues, ensureQueuesWithPool, ensureQueuesReport
* Optional effectful integration (behind flag)
