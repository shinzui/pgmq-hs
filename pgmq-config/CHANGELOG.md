# Revision history for pgmq-config

## 0.1.0.0 -- 2026-03-12

* Initial release
* Declarative queue configuration DSL (QueueConfig, QueueType, etc.)
* Smart constructors: standardQueue, unloggedQueue, partitionedQueue
* Modifiers: withNotifyInsert, withFifoIndex, withTopicBinding
* Reconciliation: ensureQueues, ensureQueuesWithPool, ensureQueuesReport
* Optional effectful integration (behind flag)
