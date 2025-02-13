### pgmq-hs

Haskell client for [pgmq](https://github.com/tembo-io/pgmq)


## Supported API

- [x] [Sending Messages](https://tembo.io/pgmq/api/sql/functions/#sending-messages)
  - [x] [send](https://tembo.io/pgmq/api/sql/functions/#send)
  - [x] [send_batch](https://tembo.io/pgmq/api/sql/functions/#send_batch)
- [] [Reading Messages](https://tembo.io/pgmq/api/sql/functions/#reading-messages)
  - [x] [read](https://tembo.io/pgmq/api/sql/functions/#read) _does not implement experimental filtering_ 
  - [] [read_with_poll](https://tembo.io/pgmq/api/sql/functions/#read_with_poll)
  - [] [pop](https://tembo.io/pgmq/api/sql/functions/#pop)
- [] [Deleting/Archiving Messages](https://tembo.io/pgmq/api/sql/functions/#deletingarchiving-messages)
  - [] [delete (single)](https://tembo.io/pgmq/api/sql/functions/#delete-single)
  - [] [delete (batch)](https://tembo.io/pgmq/api/sql/functions/#delete-batch)
  - [] [purge_queue](https://tembo.io/pgmq/api/sql/functions/#purge_queue)
  - [] [archive (single)](https://tembo.io/pgmq/api/sql/functions/#archive-single)
  - [] [archive (batch)](https://tembo.io/pgmq/api/sql/functions/#archive-batch)
- [] [Queue Management](https://tembo.io/pgmq/api/sql/functions/#queue-management)
  - [x] [create](https://tembo.io/pgmq/api/sql/functions/#create)
  - [] [create_partitioned](https://tembo.io/pgmq/api/sql/functions/#create_partitioned) see `Partition` 
  - [] [create_unlogged](https://tembo.io/pgmq/api/sql/functions/#create_unlogged)
  - [] [detach_archive](https://tembo.io/pgmq/api/sql/functions/#detach_archive)
  - [x] [drop_queue](https://tembo.io/pgmq/api/sql/functions/#drop_queue)
- [] [Utilities](https://tembo.io/pgmq/api/sql/functions/#utilities)
  - [] [set_vt](https://tembo.io/pgmq/api/sql/functions/#set_vt)
  - [] [list_queues](https://tembo.io/pgmq/api/sql/functions/#list_queues)
  - [] [metrics](https://tembo.io/pgmq/api/sql/functions/#metrics)
  - [] [metrics_all](https://tembo.io/pgmq/api/sql/functions/#metrics_all)
- Partition
  - [] [create_partitioned](https://tembo.io/pgmq/api/sql/functions/#create_partitioned)
  - [] [show_partitions](https://github.com/pgpartman/pg_partman/blob/development/doc/pg_partman.md#show_partitions)
  - [] [run_maintenance](https://github.com/pgpartman/pg_partman/blob/development/doc/pg_partman.md#run_maintenance)
