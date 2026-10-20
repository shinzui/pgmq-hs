# Shared feed workload evidence

Observed 2026-10-20 UTC on a shared aarch64-darwin development host, GHC 9.12.4,
PostgreSQL 18.6, native PGMQ 1.13. These are diagnostic observations, not timing
gates, production benchmarks or a promise about defaults. Other development work
was running on the host; the first run overlapped test activity. The interleaved
mode order (off/on/on/off) limits but does not eliminate environmental noise.

Each trial creates a fresh ordinary queue with 100,000 messages containing a
128-character JSON string. Two concurrent consumers lease/delete 20 messages per
batch, 1,000 batches each (40,000 messages). Batch latency includes pool waiting,
read and delete. The worker pool has three connections. Inspection uses a separate
two-connection pool, statement_timeout 2000ms, lock_timeout 250ms,
acquisition timeout 1s and connect_timeout 2s. The probe runs with two RTS capabilities.

Inspection-on trials open 100 real WebSocket clients to one queue, wait for every
snapshot, then drain every frame while measuring consumers. Poll and minimum
refresh intervals are both 50ms or both 500ms, deliberately below the default 5s
poll/1s minimum. No listener is configured. Query counts cover the measured
consumer window, excluding startup snapshots: each counted metrics effect calls
pgmq.metrics once; each counted metadata effect lists exact lenient queue names.
The counters do not count pgmq.metrics' internal SQL statements separately.

| Interval ms | Trial | Subscribers | Messages/s | Batch p95 ms | Metrics calls | Metadata calls |
|---|---|---|---|---|---|---|
| 50 | 1 | 0 | 34360 | 1.837 | 0 | 0 |
| 50 | 2 | 100 | 27964 | 3.259 | 25 | 25 |
| 50 | 3 | 100 | 27508 | 3.578 | 25 | 25 |
| 50 | 4 | 0 | 36052 | 1.807 | 0 | 0 |
| 500 | 1 | 0 | 33421 | 1.896 | 0 | 0 |
| 500 | 2 | 100 | 23990 | 3.891 | 3 | 3 |
| 500 | 3 | 100 | 27607 | 3.577 | 2 | 2 |
| 500 | 4 | 0 | 34151 | 1.879 | 0 | 0 |

The 50ms run performs 25 metrics reads with 100 clients, rather than multiplying
reads per tab. The 500ms run performs only two or three reads in its window.
Both on-modes still have materially lower observed throughput and higher p95
than their adjacent off-modes. This short shared-host sample does not isolate
CPU, GC, network fan-out and PostgreSQL scan effects, and cannot attribute that
change to any one component. Lower query counts alone did not establish lower
latency here. In particular, this is not evidence that default intervals or
100 connections have negligible overhead.

Choose connection/distinct-queue caps and refresh periods against measured worker
latency/throughput SLOs on the target workload; repeat the probe after changing
limits. Keep a dedicated inspection role/pool and finite deadlines. Those isolate
connection capacity, not PostgreSQL CPU/IO. Use HTTP metrics on demand when the
workload cannot afford continuous observation. Partitioned queues may have still
higher scan costs; see the separate 019 layout evidence.

## Reproduce

From the repository root, build the library, then compile the retained probe and
its repository-local ephemeral database helper. Explicit unit IDs from Cabal's
plan avoid ambiguity when its store contains multiple builds of one dependency.

```bash
nix develop --command cabal build pgmq-inspect:lib:pgmq-inspect
nix develop --command cabal exec -- python3 - <<'PYPROBE'
import json, subprocess
names = set("base bytestring directory effectful-core ephemeral-pg hasql hasql-pool pg-migrate pgmq-core pgmq-hasql pgmq-migration pgmq-inspect aeson async stm text unix vector wai warp websockets".split())
args = ["ghc", "-hide-all-packages", "-O2", "-threaded", "-rtsopts", "-ipgmq-inspect/test", "-odir", ".dev/ep4-probe", "-hidir", ".dev/ep4-probe", "docs/design/evidence/021-inspection-feed/WorkloadProbe.hs", "-o", ".dev/ep4-workload"]
for item in json.load(open("dist-newstyle/cache/plan.json"))["install-plan"]:
    if item["pkg-name"] in names and item.get("component-name", "lib") == "lib":
        args += ["-package-id", item["id"]]
subprocess.run(args, check=True)
PYPROBE
nix develop --command .dev/ep4-workload 50000 +RTS -N2 -RTS
nix develop --command .dev/ep4-workload 500000 +RTS -N2 -RTS
```

Raw observations are retained in `workload-50ms.jsonl` and `workload-500ms.jsonl`.
The probe writes no timing assertions and owns/tears down its disposable database.
