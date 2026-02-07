# pgmq-bench

Benchmarks for the pgmq-hs library, measuring performance of queue operations across different abstraction layers.

## Prerequisites

- PostgreSQL server running with a database for benchmarks
- The pgmq schema will be automatically installed via pgmq-migration

## Running Benchmarks

### Quick Start

```bash
# Start PostgreSQL (if using process-compose from project root)
just process-up

# Create benchmark database
createdb -h $PGHOST pgmq_bench

# Run benchmarks
cabal run pgmq-bench -- +RTS -N
```

### Configuration

Benchmarks are configured via environment variables:

| Variable | Default | Description |
|----------|---------|-------------|
| `PG_CONNECTION_STRING` | `host=localhost port=5432 dbname=pgmq_bench` | PostgreSQL connection string |
| `BENCH_MESSAGE_COUNT` | `1000` | Number of messages for throughput tests |
| `BENCH_BATCH_SIZES` | `1,10,50,100` | Comma-separated batch sizes to test |
| `BENCH_SKIP_CLEANUP` | `false` | Skip queue cleanup after benchmarks |
| `BENCH_ENABLE_POLL` | `false` | Enable read-with-poll benchmarks (slow) |
| `BENCH_ENABLE_THROUGHPUT` | `false` | Enable throughput benchmarks (slow) |

### Examples

```bash
# Run with custom connection
PG_CONNECTION_STRING="host=localhost port=5432 dbname=mydb" cabal run pgmq-bench

# Run with more messages for throughput tests
BENCH_MESSAGE_COUNT=5000 cabal run pgmq-bench

# Run with custom batch sizes
BENCH_BATCH_SIZES="10,100,500" cabal run pgmq-bench

# Enable slow poll benchmarks
BENCH_ENABLE_POLL=true cabal run pgmq-bench

# Run with RTS options for better performance
cabal run pgmq-bench -- +RTS -N4 -A32m

# Output benchmark results to CSV
cabal run pgmq-bench -- --csv results.csv
```

## Benchmark Suites

### Layer Comparison (`layer-comparison`)

Compares performance across abstraction layers:
- **raw-sql**: Direct hasql statements with minimal abstraction
- **hasql**: Using pgmq-hasql Sessions API
- **effectful**: Using pgmq-effectful effects

Operations benchmarked:
- `send-single`: Send a single message
- `send-batch-100`: Send 100 messages in a batch
- `read-batch-10`: Read 10 messages
- `delete-single`: Delete a single message
- `pop-batch-10`: Pop 10 messages (atomic read + delete)
- `full-cycle`: Complete send → read → delete cycle

### Send Benchmarks (`send`)

- **single**: Single message send performance
- **batch-sizes**: Batch send with 10, 50, 100 messages
- **payload-sizes**: Different payload sizes (100B, 10KB, 100KB)

### Read Benchmarks (`read`)

- **single**: Single message read
- **batch-sizes**: Batch read with 10, 50, 100 messages
- **with-poll**: Read with polling (1s/10msg, 5s/50msg)

### Ack Benchmarks (`ack`)

- **delete/single**: Single message deletion
- **delete/batch**: Batch deletion (10, 50, 100 messages)
- **archive/single**: Single message archival
- **archive/batch**: Batch archival (10, 50, 100 messages)

### Throughput Benchmarks (`throughput`)

- **send_n/sequential**: Send N messages one at a time
- **send_n/batched**: Send N messages in batches of 100
- **full_cycle_n/sequential**: Full cycle with sequential operations
- **full_cycle_n/batched**: Full cycle with batched operations

## Interpreting Results

Benchmarks use [tasty-bench](https://hackage.haskell.org/package/tasty-bench) which reports:
- **Mean time**: Average execution time
- **Allocated**: Memory allocated per operation

Lower times are better. The layer comparison benchmarks help identify:
- Overhead of the effectful layer vs direct hasql
- Impact of batch sizes on throughput
- Memory allocation patterns

## Running Specific Benchmarks

Use tasty's pattern matching to run specific benchmarks:

```bash
# Run only layer-comparison benchmarks
cabal run pgmq-bench -- -p layer-comparison

# Run only send benchmarks
cabal run pgmq-bench -- -p send

# Run a specific benchmark
cabal run pgmq-bench -- -p "send-single"
```

## Benchmark Results

Results from running on:
- **Hardware**: Apple M3 Max, 36GB RAM
- **PostgreSQL**: 17.2 (via Unix socket)
- **GHC**: 9.12.2

### Layer Comparison Summary

| Operation | raw-sql | hasql | effectful | Overhead |
|-----------|---------|-------|-----------|----------|
| send-single | 13.9 μs | 13.5 μs | 15.3 μs | ~10% |
| send-batch-100 | 169 μs | 174 μs | 188 μs | ~11% |
| read-batch-10 | 14.2 μs | 16.0 μs | 17.3 μs | ~22% |
| delete-single | 26.7 μs | 26.0 μs | 29.5 μs | ~10% |
| pop-batch-10 | 86.3 μs | 86.2 μs | 102 μs | ~18% |
| full-cycle | 48.8 μs | 50.0 μs | 55.0 μs | ~13% |

### Send Benchmarks

| Operation | raw-sql | hasql | effectful |
|-----------|---------|-------|-----------|
| single | 14.5 μs | 14.4 μs | 15.9 μs |
| batch-10 | 30.3 μs | 30.3 μs | 34.1 μs |
| batch-50 | 101 μs | 102 μs | 114 μs |
| batch-100 | 185 μs | 188 μs | 215 μs |
| small (100B) | 14.1 μs | 13.9 μs | 15.9 μs |
| medium (10KB) | 39.6 μs | 39.9 μs | 42.9 μs |
| large (100KB) | 218 μs | 223 μs | 223 μs |

### Read Benchmarks

| Operation | raw-sql | hasql | effectful |
|-----------|---------|-------|-----------|
| single | 13.2 μs | 13.3 μs | 14.2 μs |
| batch-10 | 13.4 μs | 12.4 μs | 14.8 μs |
| batch-50 | 14.3 μs | 14.5 μs | 16.5 μs |
| batch-100 | 14.4 μs | 14.5 μs | 16.3 μs |

### Ack (Delete/Archive) Benchmarks

| Operation | raw-sql | hasql | effectful |
|-----------|---------|-------|-----------|
| delete-single | 28.9 μs | 28.5 μs | 30.3 μs |
| delete-batch-10 | 58.3 μs | 58.5 μs | 61.2 μs |
| delete-batch-50 | 180 μs | 178 μs | 186 μs |
| delete-batch-100 | 335 μs | 333 μs | 341 μs |
| archive-single | 29.2 μs | 29.2 μs | 31.8 μs |
| archive-batch-10 | 61.9 μs | 60.8 μs | 63.7 μs |
| archive-batch-50 | 188 μs | 188 μs | 199 μs |
| archive-batch-100 | 348 μs | 373 μs | 388 μs |

### Key Observations

1. **Layer overhead is minimal**: The effectful layer adds ~10-15% overhead compared to raw SQL
2. **Batching scales linearly**: Batch-100 is roughly 10x batch-10, showing good efficiency
3. **Reads are fast**: Read operations are faster than sends due to simpler locking
4. **Large payloads**: 100KB payloads take ~15x longer than 100B payloads
5. **Full cycle timing**: A complete send → read → delete cycle takes ~50-55 μs
