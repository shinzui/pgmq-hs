### pgmq-hs

Haskell client for [pgmq](https://github.com/tembo-io/pgmq)

**Requires pgmq 1.5.0+** for full functionality.

## Supported API

- [x] [Sending Messages](https://tembo.io/pgmq/api/sql/functions/#sending-messages)
  - [x] [send](https://tembo.io/pgmq/api/sql/functions/#send) - with headers support (1.5.0+)
  - [x] [send_batch](https://tembo.io/pgmq/api/sql/functions/#send_batch) - with headers support (1.5.0+)
- [x] [Reading Messages](https://tembo.io/pgmq/api/sql/functions/#reading-messages)
  - [x] [read](https://tembo.io/pgmq/api/sql/functions/#read) - with conditional filtering (1.5.0+)
  - [x] [read_with_poll](https://tembo.io/pgmq/api/sql/functions/#read_with_poll)
  - [x] [pop](https://tembo.io/pgmq/api/sql/functions/#pop) - with quantity parameter (1.7.0+)
- [x] [Deleting/Archiving Messages](https://tembo.io/pgmq/api/sql/functions/#deletingarchiving-messages)
  - [x] [delete (single)](https://tembo.io/pgmq/api/sql/functions/#delete-single)
  - [x] [delete (batch)](https://tembo.io/pgmq/api/sql/functions/#delete-batch)
  - [x] [purge_queue](https://tembo.io/pgmq/api/sql/functions/#purge_queue)
  - [x] [archive (single)](https://tembo.io/pgmq/api/sql/functions/#archive-single)
  - [x] [archive (batch)](https://tembo.io/pgmq/api/sql/functions/#archive-batch)
- [x] [Queue Management](https://tembo.io/pgmq/api/sql/functions/#queue-management)
  - [x] [create](https://tembo.io/pgmq/api/sql/functions/#create)
  - [x] [create_partitioned](https://tembo.io/pgmq/api/sql/functions/#create_partitioned)
  - [x] [create_unlogged](https://tembo.io/pgmq/api/sql/functions/#create_unlogged)
  - [x] [detach_archive](https://tembo.io/pgmq/api/sql/functions/#detach_archive) - **DEPRECATED** (no-op in pgmq 2.0)
  - [x] [drop_queue](https://tembo.io/pgmq/api/sql/functions/#drop_queue)
  - [x] enable_notify_insert (1.7.0+) - with throttling (1.8.0+)
  - [x] disable_notify_insert (1.7.0+)
- [x] [Utilities](https://tembo.io/pgmq/api/sql/functions/#utilities)
  - [x] [set_vt](https://tembo.io/pgmq/api/sql/functions/#set_vt) - single and batch (1.8.0+)
  - [x] [list_queues](https://tembo.io/pgmq/api/sql/functions/#list_queues)
  - [x] [metrics](https://tembo.io/pgmq/api/sql/functions/#metrics) - includes queue_visible_length (1.5.0+)
  - [x] [metrics_all](https://tembo.io/pgmq/api/sql/functions/#metrics_all)
- Partition
  - [x] [create_partitioned](https://tembo.io/pgmq/api/sql/functions/#create_partitioned)
  - [ ] [show_partitions](https://github.com/pgpartman/pg_partman/blob/development/doc/pg_partman.md#show_partitions)
  - [ ] [run_maintenance](https://github.com/pgpartman/pg_partman/blob/development/doc/pg_partman.md#run_maintenance)
