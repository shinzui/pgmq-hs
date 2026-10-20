# Inspection query-plan evidence

Captured 2026-10-20 UTC by `InspectionCostSpec` on PostgreSQL 18.6 (aarch64 Darwin)
with pg_partman 5.5.0. The schema is either the stock PGMQ 1.12.0 fixture or the
complete native 1.13.0 migration ledger. These are warm-cache diagnostic runs,
not production throughput benchmarks or latency promises.

Each layout has 100,000 live rows and 100,000 archive rows, populated directly
into the actual parents created by the public queue-creation sessions. Each body
contains a number and 128 repeated characters; headers contain one boolean. The
archive fixture is independently populated, not a benchmark of `pgmq.archive`.
Ordinary and unlogged-queue layouts retain their upstream indexes. The archive
associated with an unlogged queue is logged, as upstream defines it. Numeric partitions
use interval 10000 and retention 1000000; time partitions use 1 day and retention
1000 days. Default premake settings are unchanged. Numeric rows span five bounded
leaves plus the default; time rows span yesterday, today and the default (100
years ahead). Tests assert at least three occupied leaves and an occupied default
on both live and archive parents. No maintenance or optional index is installed.
The separate small numeric functional test retains interval 10.

Each JSON file retains the SQL and complete EXPLAIN (ANALYZE, BUFFERS, FORMAT JSON),
including plan nodes, actual loops/rows, filtering, sort methods, planning and
execution times, and buffer counters. Projections come from `toSql` of the real
inspection builders: all seven message columns, plus `archived_at` for archives.
Parameters are substituted as typed SQL literals for an unprepared custom plan.
First page: NULL cursor and limit 50. Late page: cursor 99950 and limit 50.
Present lookup: 99999. Absent lookup: 100001. Actual public sessions also verify
those pages and present/absent lookups before collecting plans.

Reproduce from the repository root (the output directory must be absolute because
Cabal runs the test from the package directory):

```bash
PGMQ_INSPECTION_PLANS="$PWD/docs/design/evidence/019-inspection-reads" nix develop .#partman --command cabal test pgmq-hasql:pgmq-hasql-test --test-options='-p Inspection'
PGMQ_TEST_SCHEMA_VERSION=1.12.0 PGMQ_INSPECTION_PLANS="$PWD/docs/design/evidence/019-inspection-reads" nix develop .#partman --command cabal test pgmq-hasql:pgmq-hasql-test --test-options='-p Inspection'
```

Both runs passed all 19 inspection cases with pg_partman required (no partition
skips). The foreign-name cases independently install the native ledger in their
own database, as documented in their test module.

## Observations

On both schemas, ordinary and unlogged live/archive reads use the msg_id primary
key. Numeric live reads use the partition msg_id indexes and prune late/by-id
queries to the default. Numeric archives prune by msg_id but lack that index:
their first page scans all 100,000 rows; late pages and lookups scan the 50,001-row
default. Time live/archive reads cannot prune on msg_id and scan all 100,000 rows;
pages require sorting. Returning 50 rows therefore does not bound work to 50 rows.

No supplemental index is recommended from this experiment: write overhead,
storage and maintenance behavior were not measured. Inspection must create no
indexes. Use separate inspection connections, finite statement/lock deadlines,
and admission limits; choose refresh rates from measured workload. Future HTTP
budget acceptance belongs to ExecPlan 29. Database planning remains sensitive to
statistics, payload size, partitions and PostgreSQL version.

## Measured plans

`q` means live, `a` archive. Node names show the distinct traversal order, not a
lossless tree (see each JSON for the full tree). Buffers are root shared hits plus
reads, not the sum across nodes, which would double count. Filtered rows sum leaf
filter counts multiplied by actual loops; EXPLAIN averages can introduce rounding
in parallel plans. Milliseconds are observations only and are never test gates.

| Schema | Layout/table | Case | Nodes | Filtered rows | Buffers | Plan ms | Execute ms |
|---|---|---|---|---:|---:|---:|---:|
| 1.12.0 | [numeric/a](1.12.0-numeric-a.json) | first | Limit → Gather Merge → Sort → Append → Seq Scan | 0 | 3304 | 0.084 | 33.954 |
| 1.12.0 | [numeric/a](1.12.0-numeric-a.json) | late | Limit → Sort → Seq Scan | 49951 | 1613 | 0.070 | 34.227 |
| 1.12.0 | [numeric/a](1.12.0-numeric-a.json) | present | Seq Scan | 50000 | 1613 | 0.065 | 33.597 |
| 1.12.0 | [numeric/a](1.12.0-numeric-a.json) | absent | Seq Scan | 50001 | 1613 | 0.054 | 33.901 |
| 1.12.0 | [numeric/q](1.12.0-numeric-q.json) | first | Limit → Merge Append → Index Scan | 0 | 19 | 0.051 | 0.035 |
| 1.12.0 | [numeric/q](1.12.0-numeric-q.json) | late | Limit → Index Scan | 0 | 5 | 0.102 | 0.010 |
| 1.12.0 | [numeric/q](1.12.0-numeric-q.json) | present | Index Scan | 0 | 3 | 0.029 | 0.003 |
| 1.12.0 | [numeric/q](1.12.0-numeric-q.json) | absent | Index Scan | 0 | 2 | 0.015 | 0.003 |
| 1.12.0 | [ordinary/a](1.12.0-ordinary-a.json) | first | Limit → Index Scan | 0 | 4 | 0.007 | 0.011 |
| 1.12.0 | [ordinary/a](1.12.0-ordinary-a.json) | late | Limit → Index Scan | 0 | 4 | 0.011 | 0.009 |
| 1.12.0 | [ordinary/a](1.12.0-ordinary-a.json) | present | Index Scan | 0 | 3 | 0.009 | 0.004 |
| 1.12.0 | [ordinary/a](1.12.0-ordinary-a.json) | absent | Index Scan | 0 | 2 | 0.007 | 0.003 |
| 1.12.0 | [ordinary/q](1.12.0-ordinary-q.json) | first | Limit → Index Scan | 0 | 4 | 0.154 | 0.017 |
| 1.12.0 | [ordinary/q](1.12.0-ordinary-q.json) | late | Limit → Index Scan | 0 | 4 | 0.037 | 0.015 |
| 1.12.0 | [ordinary/q](1.12.0-ordinary-q.json) | present | Index Scan | 0 | 3 | 0.011 | 0.004 |
| 1.12.0 | [ordinary/q](1.12.0-ordinary-q.json) | absent | Index Scan | 0 | 2 | 0.009 | 0.003 |
| 1.12.0 | [time/a](1.12.0-time-a.json) | first | Limit → Gather Merge → Sort → Append → Seq Scan | 0 | 3304 | 0.109 | 15.055 |
| 1.12.0 | [time/a](1.12.0-time-a.json) | late | Limit → Sort → Append → Seq Scan | 99950 | 3228 | 0.105 | 17.856 |
| 1.12.0 | [time/a](1.12.0-time-a.json) | present | Append → Seq Scan | 99999 | 3228 | 0.104 | 22.317 |
| 1.12.0 | [time/a](1.12.0-time-a.json) | absent | Append → Seq Scan | 100000 | 3228 | 0.160 | 26.205 |
| 1.12.0 | [time/q](1.12.0-time-q.json) | first | Limit → Gather Merge → Sort → Append → Seq Scan | 0 | 3202 | 0.185 | 32.045 |
| 1.12.0 | [time/q](1.12.0-time-q.json) | late | Limit → Sort → Append → Seq Scan | 99950 | 3126 | 0.121 | 63.345 |
| 1.12.0 | [time/q](1.12.0-time-q.json) | present | Append → Seq Scan | 99999 | 3126 | 0.118 | 66.626 |
| 1.12.0 | [time/q](1.12.0-time-q.json) | absent | Append → Seq Scan | 100000 | 3126 | 0.688 | 63.238 |
| 1.12.0 | [unlogged/a](1.12.0-unlogged-a.json) | first | Limit → Index Scan | 0 | 4 | 0.009 | 0.012 |
| 1.12.0 | [unlogged/a](1.12.0-unlogged-a.json) | late | Limit → Index Scan | 0 | 4 | 0.015 | 0.010 |
| 1.12.0 | [unlogged/a](1.12.0-unlogged-a.json) | present | Index Scan | 0 | 3 | 0.008 | 0.003 |
| 1.12.0 | [unlogged/a](1.12.0-unlogged-a.json) | absent | Index Scan | 0 | 2 | 0.008 | 0.003 |
| 1.12.0 | [unlogged/q](1.12.0-unlogged-q.json) | first | Limit → Index Scan | 0 | 4 | 0.009 | 0.070 |
| 1.12.0 | [unlogged/q](1.12.0-unlogged-q.json) | late | Limit → Index Scan | 0 | 7 | 0.017 | 0.013 |
| 1.12.0 | [unlogged/q](1.12.0-unlogged-q.json) | present | Index Scan | 0 | 3 | 0.054 | 0.011 |
| 1.12.0 | [unlogged/q](1.12.0-unlogged-q.json) | absent | Index Scan | 0 | 2 | 0.011 | 0.004 |
| 1.13.0 | [numeric/a](1.13.0-numeric-a.json) | first | Limit → Gather Merge → Sort → Append → Seq Scan | 0 | 3304 | 0.773 | 31.677 |
| 1.13.0 | [numeric/a](1.13.0-numeric-a.json) | late | Limit → Sort → Seq Scan | 49951 | 1613 | 0.062 | 43.626 |
| 1.13.0 | [numeric/a](1.13.0-numeric-a.json) | present | Seq Scan | 50000 | 1613 | 0.083 | 43.816 |
| 1.13.0 | [numeric/a](1.13.0-numeric-a.json) | absent | Seq Scan | 50001 | 1613 | 0.029 | 39.820 |
| 1.13.0 | [numeric/q](1.13.0-numeric-q.json) | first | Limit → Merge Append → Index Scan | 0 | 19 | 0.054 | 0.042 |
| 1.13.0 | [numeric/q](1.13.0-numeric-q.json) | late | Limit → Index Scan | 0 | 5 | 0.044 | 0.011 |
| 1.13.0 | [numeric/q](1.13.0-numeric-q.json) | present | Index Scan | 0 | 3 | 0.019 | 0.004 |
| 1.13.0 | [numeric/q](1.13.0-numeric-q.json) | absent | Index Scan | 0 | 2 | 0.041 | 0.010 |
| 1.13.0 | [ordinary/a](1.13.0-ordinary-a.json) | first | Limit → Index Scan | 0 | 4 | 0.049 | 0.040 |
| 1.13.0 | [ordinary/a](1.13.0-ordinary-a.json) | late | Limit → Index Scan | 0 | 4 | 0.030 | 0.016 |
| 1.13.0 | [ordinary/a](1.13.0-ordinary-a.json) | present | Index Scan | 0 | 3 | 0.022 | 0.010 |
| 1.13.0 | [ordinary/a](1.13.0-ordinary-a.json) | absent | Index Scan | 0 | 2 | 0.018 | 0.006 |
| 1.13.0 | [ordinary/q](1.13.0-ordinary-q.json) | first | Limit → Index Scan | 0 | 4 | 0.036 | 0.098 |
| 1.13.0 | [ordinary/q](1.13.0-ordinary-q.json) | late | Limit → Index Scan | 0 | 7 | 0.228 | 0.030 |
| 1.13.0 | [ordinary/q](1.13.0-ordinary-q.json) | present | Index Scan | 0 | 3 | 0.068 | 0.010 |
| 1.13.0 | [ordinary/q](1.13.0-ordinary-q.json) | absent | Index Scan | 0 | 2 | 0.035 | 0.009 |
| 1.13.0 | [time/a](1.13.0-time-a.json) | first | Limit → Gather Merge → Sort → Append → Seq Scan | 0 | 3304 | 0.096 | 29.738 |
| 1.13.0 | [time/a](1.13.0-time-a.json) | late | Limit → Sort → Append → Seq Scan | 99950 | 3228 | 0.122 | 77.634 |
| 1.13.0 | [time/a](1.13.0-time-a.json) | present | Append → Seq Scan | 99999 | 3228 | 0.103 | 75.694 |
| 1.13.0 | [time/a](1.13.0-time-a.json) | absent | Append → Seq Scan | 100000 | 3228 | 0.117 | 71.422 |
| 1.13.0 | [time/q](1.13.0-time-q.json) | first | Limit → Gather Merge → Sort → Append → Seq Scan | 0 | 3202 | 0.284 | 29.736 |
| 1.13.0 | [time/q](1.13.0-time-q.json) | late | Limit → Sort → Append → Seq Scan | 99950 | 3126 | 0.274 | 43.008 |
| 1.13.0 | [time/q](1.13.0-time-q.json) | present | Append → Seq Scan | 99999 | 3126 | 0.120 | 40.374 |
| 1.13.0 | [time/q](1.13.0-time-q.json) | absent | Append → Seq Scan | 100000 | 3126 | 0.268 | 47.859 |
| 1.13.0 | [unlogged/a](1.13.0-unlogged-a.json) | first | Limit → Index Scan | 0 | 4 | 0.035 | 0.113 |
| 1.13.0 | [unlogged/a](1.13.0-unlogged-a.json) | late | Limit → Index Scan | 0 | 4 | 0.100 | 0.015 |
| 1.13.0 | [unlogged/a](1.13.0-unlogged-a.json) | present | Index Scan | 0 | 3 | 0.038 | 0.015 |
| 1.13.0 | [unlogged/a](1.13.0-unlogged-a.json) | absent | Index Scan | 0 | 2 | 0.012 | 0.004 |
| 1.13.0 | [unlogged/q](1.13.0-unlogged-q.json) | first | Limit → Index Scan | 0 | 4 | 0.013 | 0.012 |
| 1.13.0 | [unlogged/q](1.13.0-unlogged-q.json) | late | Limit → Index Scan | 0 | 7 | 0.142 | 0.125 |
| 1.13.0 | [unlogged/q](1.13.0-unlogged-q.json) | present | Index Scan | 0 | 3 | 0.039 | 0.007 |
| 1.13.0 | [unlogged/q](1.13.0-unlogged-q.json) | absent | Index Scan | 0 | 2 | 0.024 | 0.006 |
