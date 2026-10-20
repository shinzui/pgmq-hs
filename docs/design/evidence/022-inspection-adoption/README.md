# Inspection adoption evidence

Captured 2026-10-20 UTC by ExecPlan 31 on PostgreSQL 18.6 with the native PGMQ 1.13
ledger. [standalone-transcript.txt](standalone-transcript.txt) is produced by
[`SmokeSpec`](../../../../pgmq-inspect/test/SmokeSpec.hs), using a disposable database,
the actual `pgmq-inspect` executable, curl and a WebSocket client. It covers all HTTP
groups, non-destructive pages, a 400 envelope, allowed-origin headers, a poll-only
snapshot, ping/pong and Ctrl-C goodbye/process cleanup. Header CRLF is normalized
to LF in this text artifact; response bodies are retained. Ports, timestamps, ages,
latency and HTTP Date naturally vary. No credentials/connection string are recorded.
The test prepares three messages in `demo`, archives ID 1 and enables a binding and
notification trigger. The executable starts with `--no-listener` and an explicit
origin; the snapshot correctly reports push false despite the installed trigger.

To regenerate from the repository root:

```sh
nix develop --command env \
  PGMQ_INSPECT_RECORD_TRANSCRIPT="$PWD/docs/design/evidence/022-inspection-adoption/standalone-transcript.txt" \
  cabal test pgmq-inspect-test --test-option=-p --test-option=/transcript/
```

Default tests do not rewrite evidence. Review changed response shapes before
committing any regenerated transcript. Run the full native and stock-schema suites
with `cabal test pgmq-inspect-test` and
`PGMQ_TEST_SCHEMA_VERSION=1.12.0 cabal test pgmq-inspect-test` inside the dev shell.
The stock run's schema validation checks null 1.12 partition estimates too.

[`OpenApiSpec`](../../../../pgmq-inspect/test/OpenApiSpec.hs) compares all HTTP methods
and paths with the router, round-trips the served document, and validates actual
nonempty responses against each operation/status schema. Headers include null,
object, array, boolean and string; bodies include scalar, array and null. Both
cursor-bearing and final pages, live/archive IDs, readiness success/failure/busy,
and error details are exercised. Deliberate route and description drift were run
without acceptance and failed the intended coverage/golden assertions, then restored.

openapi3 3.2.5's JSON validator ignores its nullable keyword. The test translates
nullable only in its validation view to the validator's supported union/null form;
negative checks prove non-null fields still reject null and nullable integers still
reject strings. The published document retains valid OpenAPI 3.0 nullable schemas.
This workaround is confined to tests and documented against the dependency source.

The dependency-free [client fixture](../../../../pgmq-inspect/test/client/inspection-client.mjs)
generates GET functions from the served document and executes them at both root and
`/pgmq/`. It verifies that the relative server `.` retains the mount for `/queues`,
that descriptor links resolve from trailing-slash URLs, and that percent-encoded
foreign queue names and actual WebSocket upgrades work under either mount. IDs
9007199254740993 and 9223372036854775807 survive pages, cursor echo and lookup using
original JSON tokens from the Node 22 reviver's source context. This is evidence for
the supplied client fixture, not an external SDK generator or a downstream UI.

[`PrivilegesSpec`](../../../../pgmq-inspect/test/PrivilegesSpec.hs) executes the exact
[role recipe](../../../../pgmq-inspect/test/fixtures/inspection-role.sql) on an isolated
cluster, checks all HTTP groups and a live snapshot, and creates another queue after
the default grants to verify future tables/sequences. The actual login's role
settings are checked. After disabling its read-only transaction default, calls to
read/pop/send/archive/delete/purge/create/drop, direct UPDATE and nextval still fail.
The owner-wide PUBLIC function defaults and existing worker grants require operator
review; the CLI never applies this SQL.

[`CliSpec`](../../../../pgmq-inspect/test/CliSpec.hs) checks every one of four pooled
connections while holding all four simultaneously at a barrier, plus an independent
connection using listener settings. Statement and lock deadlines override supplied
unbounded values while an unrelated work_mem option survives. The suite's existing
lock/statement/isolation cases verify worker progress, bounded admission and the
explicit IR-6 cancellation limitation. These are correctness tests, not a promise
of bounded blackholed network calls or negligible database load.
