---
id: 10
slug: add-grouped-head-read-statements-and-sessions-to-pgmq-hasql
title: "Add grouped heads, premake, and compatible metrics to pgmq-hasql"
kind: exec-plan
created_at: 2026-07-14T14:55:11Z
intention: "intention_01kxgh9geke2dayhx57qp6g9ye"
master_plan: "docs/masterplans/2-support-pgmq-1-12-0-grouped-head-reads.md"
provenance:
  revisions:
    - model: "unknown"
      harness: "codex"
      at: 2026-09-10T16:47:51Z
      mode: "update"
      note: "Refresh for released PGMQ 1.12/1.13, partition controls and metrics, safe native upgrades, and the 0.6.0.0 release."
    - model: "gpt-6-astra"
      harness: "codex-cli"
      at: 2026-09-10T17:02:21Z
      mode: "update"
      note: "Correct prior unknown attribution: the 2026-09-10 PGMQ planning refresh was authored by gpt-6-astra, verified from this session turn_context metadata."
    - model: "gpt-6-astra"
      harness: "codex-cli"
      at: 2026-09-10T17:42:17Z
      mode: "implement"
      note: "Implement grouped heads, explicit premake, and version-compatible nullable metrics."
---
# Add grouped heads, premake, and compatible metrics to pgmq-hasql


This ExecPlan is a living document. Keep Progress, Surprises & Discoveries, Decision Log,
and Outcomes & Retrospective current.


## Purpose / Big Picture


Let direct Haskell clients use the PGMQ 1.12 grouped-head operations and 1.13 partition controls
and metrics. After this plan, callers can lease one absolute head per group, poll for work,
create a partitioned queue with an explicit premake count, and observe the estimated number
of messages stranded in queue/archive default partitions.

A group is the value of the `x-pgmq-group` JSON header; absent headers share one implicit
group. The head is its lowest message ID regardless of visibility. An invisible head blocks
its group; expiry makes the same head eligible again. Only deleting/archiving the head advances
the group. A visibility lease does not guarantee exactly-once processing.

Premake is the number of partitions created ahead of the active one. Its new explicit API
requires 1.13.0; the original three-argument creation API and grouped reads work on both 1.12
and 1.13. Metrics return `Nothing` when the new field is unavailable (1.12) or inapplicable
(ordinary 1.13 queues), and `Just n` for a partitioned queue's estimate on 1.13.

This plan requires [EP-9](9-vendor-pgmq-1-12-0-and-add-the-native-schema-migration.md).
Its SQL and test environment must be complete. [EP-11](11-add-grouped-head-read-effects-and-traced-spans-to-pgmq-effectful.md)
consumes these names and types; [EP-12](12-expose-grouped-reads-on-the-umbrella-api-and-release-0-5-0-0.md)
owns umbrella exports and the 0.6.0.0 release.


## Progress


- [x] (2026-09-10) Milestone 1: add grouped-head statements/sessions and demonstrate absolute-head behavior.
- [x] (2026-09-10) Milestone 2: add the explicit premake statement/session while preserving the original API.
- [x] (2026-09-10) Milestone 3: extend QueueMetrics and both metrics projections/decoders for 1.12 and 1.13.
- [x] (2026-09-10) Milestone 4: verify polling, partition creation, metrics and version compatibility; hand off clean interfaces.


## Surprises & Discoveries


EP-10 implementation: `MetricsSpec` was listed in Cabal but absent from `Main.hs`, so its
existing tests never ran. Registered it and added real default-partition cases. The native
1.13 required-partman run passes all 88 tests, including the six new grouped-head cases,
three partition compatibility cases and five metrics cases. The 1.12 required-partman
selection also passes all 14 grouped, metrics and partition cases, including after metrics
fixture isolation under the default parallel runner. The round-robin mutation fails two of six grouped tests (six IDs instead of three,
and two implicit-group messages instead of one). The non-null metric mutation fails all
five metrics tests with `UnexpectedNullCellError` at column 7. Both edits were restored;
the final all-package build passes, including effectful/config tests and the benchmark binary.


A default parallel run exposed a metrics_all table-lookup race when unrelated tests dropped
queues after its metadata enumeration (SQLSTATE 42P01). Each metrics case now provisions its
own disposable database, closes its pool, and keeps the database-wide catalog stable while
both metrics APIs run. This preserves parallel execution of the suite.

The old plan's round-robin mutation check was invalid: `qty = 3` with three groups can return
one per group under either algorithm. Use `qty = 6` with two messages in each of three groups;
grouped-head returns three while round-robin can fill six.

At implementation start, `QueueMetrics` had seven fields in `Pgmq.Hasql.Statements.Types`, not pgmq-core.
Both observability statements used SELECT * and the same seven-column decoder.
The eighth upstream field needs deliberate handling for both SQL versions.

`Pgmq.Hasql.Statements.Message` currently imports `preparable` from `Hasql.Statement`;
the old plan's `Pgmq.Hasql.Quasi` description is stale. Use the actual neighboring code.

The package family is already 0.5.0.0. New record shape changes belong to a new release,
not to a rewritten 0.5.0.0 entry.


## Decision Log


On 2026-09-10, isolate unsupported-signature error checks in fresh pools. The repository's
pinned driver, `mori://hasql/hasql` at `aa3d6ae499e187c291422443f221f9f486c43a9e`, reports
SQLSTATE 42883 for the initial 1.12 explicit-premake call but SQLSTATE 26000 on a repeated
call through its cached prepared statement. The failure fixtures now observe each original
server error independently. Do not change dependency pins or silently discard premake.


Retain the July 14 decisions to reuse `ReadGrouped`/`ReadGroupedWithPoll` and leave umbrella
exports to EP-12. These records carry exactly the arguments of the two new grouped functions.

On 2026-09-10, keep `CreatePartitionedQueue` unchanged and add
`createPartitionedQueueWithPremake :: CreatePartitionedQueue -> Int32 -> Session ()`.
A tuple-input statement adds the fourth parameter without breaking existing record construction
or requiring every 1.12 caller to switch SQL signatures.

On 2026-09-10, append `defaultPartitionLength :: Maybe Int64` to `QueueMetrics` and project
the optional field through the SQL record's JSON representation. This avoids an absent-column
error on 1.12, exposes the real 1.13 value, and preserves NULL rather than reporting false zero.
See [the compatibility ADR](../adr/pgmq-1.12-1.13-compatibility.md).


## Outcomes & Retrospective


Completed on 2026-09-10. The direct API exposes both grouped-head sessions, the explicit
premake statement/session and the nullable eighth QueueMetrics field. The original three-field
CreatePartitionedQueue record and grouped argument records remain intact. Existing record
construction of QueueMetrics must supply the new field; release migration guidance remains
EP-12's responsibility.

The final native 1.13 suite passes all 88 cases with default parallel execution. The 1.12
selection passes all 14 grouped, metrics and partition cases with default parallel execution.
Both require actual pg_partman through the locked `partman` shell (PostgreSQL 17.10,
pg_partman 5.4.3, established by EP-9). The all-package build succeeds, including downstream
libraries, test binaries and the benchmark. Formatting and git diff whitespace checks pass.
The planned round-robin and non-null decoder mutations both failed behavioral assertions
before restoration. Implementation is committed as `b756f7e`.

No effect constructors, umbrella exports, versions or dependency pins changed here. EP-11
can consume the exact session signatures below. Durable metrics isolation, nullable semantics,
and the pinned-driver unsupported-call limitation are recorded in the compatibility ADR.


## Context and Orientation


Run from the repository root inside `nix develop`, using EP-9's pg_partman-capable environment
for partition tests. Dependency APIs must be inspected through Mori before modifying their use.

The direct interface is layered. `pgmq-hasql/src/Pgmq/Hasql/Statements/Message.hs` defines
SQL statements; `Statements/QueueManagement.hs` defines creation statements;
`Statements/QueueObservability.hs` defines metrics statements. `Encoders.hs` maps argument
records to positional SQL parameters, `Decoders.hs` maps returned rows, and `Sessions.hs`
wraps statements with `statement`. `Statements.hs` re-exports whole statement modules.
`pgmq-hasql/src/Pgmq.hs` is the curated umbrella, reserved for EP-12.

`ReadGrouped` has `queueName :: QueueName`, `visibilityTimeout :: Int32` and `qty :: Int32`.
`ReadGroupedWithPoll` adds `maxPollSeconds :: Int32` and `pollIntervalMs :: Int32`.
Reuse their encoders and `messageDecoder`. `CreatePartitionedQueue` has three fields:
`queueName :: QueueName`, `partitionInterval :: Text` and `retentionInterval :: Text`.
Its existing encoder and statement retain three parameters.

`QueueMetrics` contains queueName Text, queueLength Int64, nullable newest/oldest
ages Int32, totalMessages Int64, scrapeTime UTCTime and queueVisibleLength Int64, in that order,
followed by the new defaultPartitionLength (Maybe Int64). Neither Message nor any pgmq-core type changes.

`pgmq-hasql/test/Main.hs` registers `AdvancedOpsSpec`, `QueueSpec` and `MetricsSpec`.
`test/EphemeralDb.hs` provisions the database and supplies `withTestFixture`;
`TestUtils.hs` supplies `assertSession` and `cleanupQueue`. EP-9 adds
`PGMQ_TEST_SCHEMA_VERSION=1.12.0|1.13.0` and `PGMQ_REQUIRE_PARTMAN=1`. Default mode is the
complete native 1.13 ledger. Required-partman mode fails instead of skipping absent pg_partman.

The source contract was inspected at released tags in `mori://pgmq/pgmq`. Its
`pgmq-extension/sql/pgmq.sql` and `docs/partitioned-queues.md` describe the SQL changes;
individual source artifact URIs are pending. Local decisions are
[NULL handling](../design/014-null-parameter-contract.md) and
[1.12/1.13 compatibility](../adr/pgmq-1.12-1.13-compatibility.md).


## Plan of Work


### Milestone 1 — Grouped-head statements, sessions and ordering proof


Copy the existing round-robin statement structure, export the two new names from
`Statements/Message.hs`, and add sessions/exports in `Sessions.hs`:

```haskell
readGroupedHead :: Statement ReadGrouped (Vector Message)
readGroupedHead = preparable sql readGroupedEncoder (D.rowVector messageDecoder)
  where
    sql = "select * from pgmq.read_grouped_head($1,$2,$3)"

readGroupedHeadWithPoll :: Statement ReadGroupedWithPoll (Vector Message)
readGroupedHeadWithPoll = preparable sql readGroupedWithPollEncoder (D.rowVector messageDecoder)
  where
    sql = "select * from pgmq.read_grouped_head_with_poll($1,$2,$3,$4,$5)"
```

The sessions have types `ReadGrouped -> Session (Vector Message)` and
`ReadGroupedWithPoll -> Session (Vector Message)` and call these statements. Do not create
new grouped argument records or redefine the decoder. Document the version floor, absolute
head rule, group-count meaning of qty, lease expiry, and connection occupation during polling.
Avoid promising that worker count implies exclusive permanent ownership of groups.

In `AdvancedOpsSpec.hs`, use unique queues and FIFO indexes. Send bodies 1..6 into A,A,B,B,C,C.
With qty 6 and a positive visibility timeout, require exactly the ID/body set for 1,3,5 and
three distinct group headers; do not depend on UPDATE RETURNING order. A second call before
expiry returns empty. Delete/archive one returned head and prove only that group's next
message becomes available. In another fixture, move a head's visibility into the past through
the existing set_vt API and prove the same ID is redelivered with increased read count.
Test ungrouped messages as one implicit group. With qty smaller than the number of groups,
the result never exceeds qty.

Acceptance: `cabal build pgmq-hasql` and grouped tests pass. Temporarily substituting
`read_grouped_rr` in the statement must fail the qty-6 assertion. Restore only that edit.
The identity assertions must also fail if a blocked group's second message is returned.


### Milestone 2 — Explicit premake without changing legacy creation


Keep the existing record, encoder, three-argument statement and session. Add and export:

```haskell
-- In Statements.QueueManagement:
createPartitionedQueueWithPremake :: Statement (CreatePartitionedQueue, Int32) ()

-- In Sessions:
createPartitionedQueueWithPremake :: CreatePartitionedQueue -> Int32 -> Session ()
```

Add a tuple encoder in `Encoders.hs` that composes the existing three record fields with
a non-null fourth int4. Use `select from pgmq.create_partitioned($1,$2,$3,$4)` and the existing
no-result convention. Inspect the actual Hasql encoder combinators in Mori if needed; do not
invent their API. There is no optional bound SQL NULL here: callers supply a concrete count.
Counts below one surface the upstream error rather than being clamped.

Use real pg_partman in `QueueSpec.hs`. An old API call on both versions has premake 4;
an explicit count 2 on 1.13 yields 2 in both queue and archive parent configurations.
Zero and negative values error and leave no created queue metadata/tables. The explicit
operation on 1.12 is unsupported and must produce a clear database undefined-function error;
do not fall back and silently discard the requested value. The full native 1.13 path must
retain sequential/concurrent re-entry from EP-9.

Acceptance: the old record still compiles unmodified, both SQL arities behave as documented,
and required-partman tests execute rather than skip. No effect constructor is added here.


### Milestone 3 — Expose default-partition estimates without a version cliff


Append `defaultPartitionLength :: !(Maybe Int64)` to `QueueMetrics` in
`Statements/Types.hs`. Add `D.column (D.nullable D.int8)` to `queueMetricsDecoder` in
`Decoders.hs`. Update any local record construction or test expectation affected by the
new field; use repository search, including benchmarks, not assumptions about constructors.

Replace SELECT * in both metrics statements with the same explicit eight-column projection:

```sql
select m.queue_name, m.queue_length, m.newest_msg_age_sec, m.oldest_msg_age_sec,
       m.total_messages, m.scrape_time, m.queue_visible_length,
       (to_jsonb(m)->>'default_partition_length')::bigint
from pgmq.metrics($1) as m
```

For allQueueMetrics use `from pgmq.metrics_all() as m`. The final expression reads a JSON key
from the composite result: an absent key on 1.12 yields NULL without referring to an absent
SQL attribute. Keep the existing named fields and order so neither result shape depends on
SELECT * expansion. This compatibility choice is tested rather than assumed.

Extend `MetricsSpec.hs` for both metrics operations. On stock 1.12 all rows have Nothing.
On 1.13 ordinary and unlogged queues also have Nothing, healthy partitioned queues have
Just 0, and a burst into a default partition yields Just a positive estimate after ANALYZE.
Archive a spilled message and ANALYZE both default partitions; assert that the estimate includes
the archive, using a tiny controlled fixture where refreshed statistics give known counts.
Preserve all existing queue length/age/visibility assertions.

Document that Nothing cannot be interpreted as healthy zero, and that estimates lag writes.
The API does not become a general metrics server. The exported field addition needs 0.6 source
migration guidance even though existing projection callers often require no changes.

Acceptance: both metrics operations decode on real 1.12 and 1.13 schemas with correct existing
fields and new nullable values. A non-null decoder must fail an ordinary-queue case; restoring
it to nullable must pass.


### Milestone 4 — Polling, compatibility and final handoff


Polling tests must cover immediately available heads, empty timeout, and a message arriving
during the wait. With work already present, maxPollSeconds 5 should return comfortably before
the deadline; an empty queue with maxPollSeconds 1 should wait about a second and return empty.
Use generous timing margins or monotonic time, plus an outer timeout to prevent hanging tests.
For arrival during the wait, use a separate committed sender connection; sending on the
connection held by the poll would deadlock the test. Keep all fixtures isolated.

Run the grouped and metrics selections against the 1.12 fixture and full native 1.13. Run
partition tests in required-partman mode, separating the intentionally unsupported explicit
1.12 call from 1.13 success cases. Verify selected test counts are nonzero. Run the default
full package suite to catch downstream SELECT * or constructor assumptions missed earlier.

Acceptance: the version matrix and required-partman checks pass, old statement/session types
remain available, and EP-11 receives the exact signatures below. Update the plan evidence and
ADR only for new findings; leave version bumps and umbrella edits to EP-12.


## Concrete Steps


From the repository root in the development environment:

```bash
cat pgmq-migration/migrations/manifest
rg -n 'read_grouped_head|premake|default_partition_length' pgmq-migration/migrations
cabal build pgmq-hasql
cabal test pgmq-hasql --test-show-details=direct
PGMQ_TEST_SCHEMA_VERSION=1.12.0 cabal test pgmq-hasql --test-options='--pattern "GroupedHead"'
PGMQ_TEST_SCHEMA_VERSION=1.12.0 cabal test pgmq-hasql --test-options='--pattern "Metrics"'
PGMQ_REQUIRE_PARTMAN=1 PGMQ_TEST_SCHEMA_VERSION=1.13.0 cabal test pgmq-hasql --test-show-details=direct
cabal build all
git diff --check
```

The manifest must contain the three new suffix entries from EP-9; numbers are read from the
live manifest rather than assumed to be 0003. The required-partman command uses EP-9's
documented environment. The implemented named selection and final commands, run from the
repository root, are:

```bash
nix develop .#partman --command cabal test pgmq-hasql --test-show-details=direct
nix develop .#partman --command env PGMQ_TEST_SCHEMA_VERSION=1.12.0 cabal test pgmq-hasql --test-show-details=direct --test-options='--pattern "/GroupedHead/ || /Metrics/ || /PartitionCompatibility/"'
nix develop .#partman --command cabal build all
git diff --check
```

These produce 88 passing native tests, 14 passing 1.12 tests and a successful package build.
`PartitionCompatibility` includes legacy premake 4 on both parents, explicit premake 2 on
1.13 or SQLSTATE 42883 on 1.12, and rejected zero/negative values with no residual objects.

Format changed Haskell files using the repository formatter. Commit scoped files with a
Conventional Commit and these trailers:

```text
MasterPlan: docs/masterplans/2-support-pgmq-1-12-0-grouped-head-reads.md
ExecPlan: docs/plans/10-add-grouped-head-read-statements-and-sessions-to-pgmq-hasql.md
Intention: intention_01kxgh9geke2dayhx57qp6g9ye
```


## Validation and Acceptance


The behavioral tests prove at most one absolute head per group, invisible-head blocking,
same-head redelivery after expiry, advancement after removal, and bounded polling.
The tuple-input premake statement changes both pg_partman parents on 1.13 while leaving the
original three-argument API usable on 1.12. Both metrics APIs decode eight Haskell fields on
both SQL versions with truthful Nothing/Just semantics and archive-inclusive estimates.

The library and dependent packages compile. No grouped/core type duplication or umbrella
changes belong here; the expected edits include statement modules, Sessions, Types, Encoders,
Decoders, AdvancedOpsSpec, QueueSpec and MetricsSpec plus any actually affected construction
sites. Unlike the original plan, Types/Encoders/Decoders are now deliberately in scope.


## Idempotence and Recovery


Tests use disposable databases and unique queues. In compatibility mode discard/reset the
database between versions; do not downgrade the native ledger. Close/return extra polling
connections even on failure. Restore only intentional mutation-test edits, preserving user
work. If SQL appears stale, force recompilation of pgmq-migration and verify the chosen fixture
mode before weakening assertions. Missing pg_partman in required mode is an environment failure
that must be resolved, not suppressed.


## Interfaces and Dependencies


EP-10 defines the following session contract:

```haskell
readGroupedHead :: ReadGrouped -> Session (Vector Message)
readGroupedHeadWithPoll :: ReadGroupedWithPoll -> Session (Vector Message)
createPartitionedQueue :: CreatePartitionedQueue -> Session ()
createPartitionedQueueWithPremake :: CreatePartitionedQueue -> Int32 -> Session ()
queueMetrics :: QueueName -> Session QueueMetrics
allQueueMetrics :: Session [QueueMetrics]
```

`QueueMetrics` appends `defaultPartitionLength :: Maybe Int64`. The other named argument
records retain their fields. EP-11 uses these same types. Existing Hasql, vector, aeson, time
and test dependencies should suffice; verify Mori sources before changing API usage or bounds.
EP-12 owns the final 0.6.0.0 family bounds.


## Revision Note


2026-09-10: Expanded the direct-layer plan to the complete 1.12/1.13 surface: explicit premake,
nullable metrics and compatible projections. Corrected the old API location and migration
preflight, strengthened grouped-head discrimination and polling coverage, and specified both
server versions and mandatory partition validation.

2026-09-10 (provenance correction): The session's recorded model for the planning refresh was
`gpt-6-astra`. Added a corrective revision entry with the verified model and `codex-cli`
harness; retained the earlier `unknown` entry to preserve append-only provenance history.

2026-09-10 implementation: completed all four milestones, added registered and isolated
metrics tests, verified both version runs with required pg_partman and planned mutations,
and built all packages. Promoted database-wide metrics isolation and the unsupported-call
fixture constraint into the existing compatibility ADR.
