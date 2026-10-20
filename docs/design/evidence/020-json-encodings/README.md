# JSON encoding allocation evidence

Captured 2026-10-20 UTC on aarch64 Darwin, GHC 9.12.4 and aeson 2.2.5.1.
The library uses Cabal's normal `-O1`; the standalone probe uses `-O2`.
These are allocation observations, not latency promises or test thresholds.

`AllocationProbe.hs` compares the originally proposed concrete `[Pair]` conversion,
a hand-written direct `Series` encoder, and the implemented library encoder. All
encode the same seven message fields. Each iteration varies the message ID and
forces the complete lazy byte string length. The payload is a JSON object with a
single string, reused after a warm-up. RTS allocated-byte deltas include loop and
GC bookkeeping; the payload construction and warm-up precede the measurement.

| String bytes | Iterations | Pair conversion bytes | Direct Series bytes | Library KeyValue bytes |
|---:|---:|---:|---:|---:|
| 64 | 100000 | 2288739088 | 1176000888 | 1228800128 |
| 1048576 | 50 | 56240992 | 55685304 | 55710944 |

For small messages the implemented encoder allocates about 46% less than the
Pair conversion and about 4.5% more than the direct baseline in this setup.
The large-message results are dominated by the payload output. The shared field
list still allocates a small list of Series; it does not imply allocation-free
encoding or a bound on encoded payload size.

The Pair implementation had a separate behavioral problem: converting a nested
`QueueType` through Value changed its field order, producing the keys
`partition_interval,premake,retention_interval,type` inside `created_queue`.
The polymorphic KeyValue list uses the nested `toEncoding` directly and retains
`type,partition_interval,retention_interval,premake`, matching the intended golden.
No wire field or meaning changed. Payload object key order remains aeson's own;
the record-field order is the one pinned by our goldens.

Reproduce from the repository root after `nix develop --command cabal build pgmq-core`.
The package IDs come from the active plan, so documentation-enabled copies of the
same aeson version cannot make the GHC import ambiguous:

```python
import json
import subprocess
from pathlib import Path

plan = json.loads(Path("dist-newstyle/cache/plan.json").read_text())["install-plan"]
aeson = next(p["id"] for p in plan if p["pkg-name"] == "aeson")
core = next(p["id"] for p in plan if p["pkg-name"] == "pgmq-core" and p.get("component-name") == "lib")
subprocess.run([
    "nix", "develop", "--command", "cabal", "exec", "--", "ghc", "-O2", "-rtsopts",
    "-hide-all-packages", "-package", "base", "-package", "bytestring",
    "-package", "text", "-package", "time", "-package-id", aeson, "-package-id", core,
    "docs/design/evidence/020-json-encodings/AllocationProbe.hs", "-outputdir", ".dev",
    "-o", ".dev/json-allocation-probe",
], check=True)
for size, count in [(64, 100000), (1048576, 50)]:
    for mode in ["pairs", "direct", "actual"]:
        result = subprocess.check_output([
            ".dev/json-allocation-probe", mode, str(size), str(count), "+RTS", "-T"
        ], text=True).strip()
        print(size, count, mode, result)
```
