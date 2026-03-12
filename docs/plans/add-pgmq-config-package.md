# Add pgmq-config: Declarative Queue Configuration Package

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

This document is maintained in accordance with `.claude/skills/exec-plan/PLANS.md`.


## Purpose / Big Picture

Today, a pgmq-hs user who needs three queues with notifications and topic bindings must write imperative setup code: call `createQueue` for each queue, then `enableNotifyInsert`, then `bindTopic`, handling errors at each step. If a new queue is added weeks later, someone must remember to update the startup sequence. There is no single place that says "these are the queues this application needs."

After this change, users declare their queue topology once as a Haskell value and call a single function at startup. The library compares the declared state against what exists in PostgreSQL and creates any missing queues, enables notifications, creates FIFO indexes, and binds topics — all idempotently. The user gains a single source of truth for their queue infrastructure, safety that workers never start without their queues, and less boilerplate.

Observable outcome: a user writes a `QueueConfig` list, calls `ensureQueues pool configs`, and the function returns successfully with all queues ready. Running it again is a no-op. Adding a new queue to the list and restarting creates only the new queue.


## Progress

- [x] Milestone 0: Feasibility — verify pgmq.create() is idempotent and listQueues provides enough info (2026-03-12)
- [x] Milestone 1: Create pgmq-config package scaffold (cabal file, module structure, add to cabal.project) (2026-03-12)
- [x] Milestone 2: Implement QueueConfig DSL types (2026-03-12)
- [x] Milestone 3: Implement reconciliation logic (ensureQueues) (2026-03-12)
- [x] Milestone 4: Add effectful integration (optional Pgmq.Config.Effectful module) (2026-03-12)
- [x] Milestone 5: Write tests using ephemeral-pg (2026-03-12)
- [x] Milestone 6: Validate end-to-end — all 68 tests pass (7 config + 6 migration + 55 hasql), nix fmt clean (2026-03-12)


## Surprises & Discoveries

- **pgmq.create() is fully idempotent**: `create_non_partitioned()` uses `CREATE TABLE IF NOT EXISTS` for both queue and archive tables, and `INSERT INTO pgmq.meta ... ON CONFLICT DO NOTHING`. Same pattern for `create_unlogged()`. Confirmed in `vendor/pgmq/pgmq-extension/sql/pgmq.sql` lines 1088-1153.
- **enable_notify_insert is idempotent**: Uses `ON CONFLICT ON CONSTRAINT notify_insert_throttle_queue_name_key DO UPDATE` (upsert). Line 1613-1617.
- **bind_topic is idempotent**: Uses `ON CONFLICT ON CONSTRAINT topic_bindings_unique_pattern_queue DO NOTHING`. Line 1817-1819.
- **create_fifo_index is idempotent**: Uses `CREATE INDEX IF NOT EXISTS`. Line 1437-1441.
- **Notification state IS queryable**: `listNotifyInsertThrottles :: Session [NotifyInsertThrottle]` exists in Sessions.hs line 298-299. The `NotifyInsertThrottle` type has `throttleQueueName`, `throttleIntervalMs`, and `throttleLastNotifiedAt`.
- **Topic bindings ARE queryable**: `listTopicBindings :: Session [TopicBinding]` and `listTopicBindingsForQueue :: QueueName -> Session [TopicBinding]` both exist. `TopicBinding` has `bindingPattern`, `bindingQueueName`, `bindingBoundAt`, `bindingCompiledRegex`.


## Decision Log

- Decision: Package name `pgmq-config` rather than extending `pgmq-hasql` or `pgmq-effectful`.
  Rationale: Follows the project's existing multi-package pattern where each package has a single responsibility. `pgmq-hasql` is the low-level client; config/reconciliation is a higher-level concern. Users who don't need declarative config shouldn't pay for the dependency.
  Date: 2026-03-12

- Decision: Additive-only reconciliation — never drop queues that are not in the config.
  Rationale: Dropping queues is destructive and loses messages. A declarative "ensure these exist" semantic is safe for startup; users who want to drop queues can call `dropQueue` explicitly. This matches the pgmq philosophy where `pgmq.create()` is itself idempotent (uses `IF NOT EXISTS` internally).
  Date: 2026-03-12

- Decision: The DSL should be a plain Haskell data type, not a monadic builder.
  Rationale: Queue configs are static data — there is no sequencing or effects involved in declaring them. A record type with smart constructors is simpler, more composable, and easier to serialize if needed later. Monadic builders add complexity without benefit here.
  Date: 2026-03-12

- Decision: Depend on `pgmq-hasql` (not `pgmq-effectful`) for the core reconciliation.
  Rationale: The reconciliation function should work at the `Session` level so it can be used from both plain hasql code and effectful programs. An optional effectful wrapper can live in the same package as a separate module (or in `pgmq-effectful` later). This avoids forcing an effectful dependency on users who only use hasql.
  Date: 2026-03-12

- Decision: Use generic-lens + lens for field access instead of OverloadedRecordDot.
  Rationale: The project convention is to use generic-lens with OverloadedLabels (`^. #fieldName`) for record field access. This matches the pattern used in pgmq-hasql and pgmq-effectful.
  Date: 2026-03-12


## Outcomes & Retrospective

All milestones completed successfully on 2026-03-12.

The pgmq-config package delivers on its purpose: users can declare queue topology as Haskell values and call `ensureQueues` or `ensureQueuesReport` at startup. All operations are idempotent — verified by tests that run reconciliation twice and confirm the second pass skips all actions.

Key outcomes:
- 3 source modules: `Pgmq.Config.Types` (DSL types), `Pgmq.Config` (hasql Session reconciliation), `Pgmq.Config.Effectful` (effectful wrapper behind flag)
- 7 tests covering: standard/unlogged queue creation, idempotency, incremental adds, notify insert, FIFO index, topic bindings
- No regressions: all 68 tests across all packages pass
- Uses generic-lens + lens for field access, matching project conventions

Lesson: The effectful module is nearly a copy of the hasql Session module with `Eff.` calls instead of `Sessions.` calls. If this pattern recurs, a shared abstraction (type class or higher-order function taking the operations) could reduce duplication. Left as-is for now since the duplication is small and explicit.


## Context and Orientation

The pgmq-hs project is a Haskell client for pgmq, a message queue built on PostgreSQL. It is organized as a multi-package Cabal project at the repository root. The key packages are:

**pgmq-core** (`pgmq-core/src/Pgmq/Types.hs`) defines core types: `QueueName` (a validated newtype over `Text`, max 47 characters, alphanumeric plus underscore), `Queue` (with fields `name`, `createdAt`, `isPartitioned`, `isUnlogged`), and types for topic routing (`RoutingKey`, `TopicPattern`, `TopicBinding`).

**pgmq-hasql** (`pgmq-hasql/src/Pgmq.hs`, `pgmq-hasql/src/Pgmq/Hasql/Sessions.hs`) provides the Session-level API. Queue creation functions include `createQueue :: QueueName -> Session ()`, `createUnloggedQueue :: QueueName -> Session ()`, `createPartitionedQueue :: CreatePartitionedQueue -> Session ()`. Notification functions: `enableNotifyInsert :: EnableNotifyInsert -> Session ()`, `disableNotifyInsert :: QueueName -> Session ()`. Topic functions: `bindTopic :: BindTopic -> Session ()`, `unbindTopic :: UnbindTopic -> Session Bool`. FIFO: `createFifoIndex :: QueueName -> Session ()`. Querying: `listQueues :: Session [Queue]`, `listTopicBindings :: Session [TopicBinding]`.

**pgmq-effectful** (`pgmq-effectful/src/Pgmq/Effectful/Effect.hs`) wraps all operations in an `Effectful` effect type `Pgmq`. The interpreter (`pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs`) uses a hasql connection pool.

**pgmq-migration** (`pgmq-migration/src/Pgmq/Migration.hs`) handles schema setup — creating the pgmq schema in PostgreSQL without the extension. It provides `migrate :: Session (Either MigrationError ())`.

The workspace is configured in `cabal.project`, which lists all packages and pins several dependencies to specific GitHub commits (hasql 1.10 ecosystem, hs-opentelemetry). Tests use `ephemeral-pg` to spin up temporary PostgreSQL instances, as seen in `pgmq-hasql/test/EphemeralDb.hs`.

A "queue" in pgmq is a pair of PostgreSQL tables (a message table and an archive table) plus associated indexes and functions. `pgmq.create(queue_name)` is idempotent — calling it for an existing queue is a no-op. A "topic binding" links a queue to a topic pattern for AMQP-style routing (pgmq 1.11.0+). "Notify insert" enables PostgreSQL LISTEN/NOTIFY when messages arrive, with an optional throttle interval in milliseconds. A "FIFO index" creates an index that enables strict first-in-first-out ordering for message reads.

The statement parameter types live in `pgmq-hasql/src/Pgmq/Hasql/Statements/Types.hs`. Key types relevant to configuration:

    data CreatePartitionedQueue = CreatePartitionedQueue
      { queueName :: !QueueName
      , partitionInterval :: !Text
      , retentionInterval :: !Text
      }

    data EnableNotifyInsert = EnableNotifyInsert
      { queueName :: !QueueName
      , throttleIntervalMs :: !(Maybe Int32)
      }

    data BindTopic = BindTopic
      { topicQueueName :: !QueueName
      , topicPattern :: !TopicPattern
      }


## Plan of Work

The work breaks into six milestones.


### Milestone 0: Feasibility Check

Before writing any code, verify two assumptions that underpin the design.

First, confirm that `pgmq.create()` is truly idempotent. Read the vendored SQL at `vendor/pgmq/pgmq-extension/sql/pgmq.sql` and search for the `create` function definition. It should use `IF NOT EXISTS` or equivalent when creating the queue tables. If it raises an error on duplicate creation, the reconciliation logic must catch and handle it instead of relying on idempotency.

Second, confirm that `listQueues` returns enough information to determine what already exists. The `Queue` type has `name`, `createdAt`, `isPartitioned`, and `isUnlogged`. This is sufficient to skip creation for existing queues but does not tell us about notification settings or topic bindings. Check whether `listTopicBindings` (in `pgmq-hasql/src/Pgmq/Hasql/Sessions.hs`) returns all bindings across all queues, and whether there is a way to query current notification settings. If notification state is not queryable, the reconciliation must be "apply and ignore if already enabled" rather than "compare and skip."

The milestone is complete when both assumptions are documented in Surprises & Discoveries with evidence.


### Milestone 1: Package Scaffold

Create the `pgmq-config` directory and its minimal structure. At the end of this milestone, `cabal build pgmq-config` succeeds with an empty library.

Create the directory `pgmq-config/src/Pgmq/Config.hs` as the main module and `pgmq-config/pgmq-config.cabal` as the package descriptor. Add `pgmq-config` to the `packages:` list in `cabal.project`.

The cabal file should declare:
- `build-depends: base, pgmq-core, pgmq-hasql, hasql, hasql-pool, text, containers`
- GHC options matching the other packages (warnings, etc.)
- `default-language: GHC2024`
- `default-extensions: ImportQualifiedPost, OverloadedStrings` (matching project style)

The main module `Pgmq.Config` should export nothing yet — just a module header.

Acceptance: `cabal build pgmq-config` in the repository root succeeds.


### Milestone 2: QueueConfig DSL Types

Define the configuration types in `pgmq-config/src/Pgmq/Config/Types.hs` and re-export from `Pgmq.Config`.

The central type is `QueueConfig`, which describes the desired state of a single queue:

    data QueueConfig = QueueConfig
      { configQueueName :: !QueueName
      , configQueueType :: !QueueType
      , configNotifyInsert :: !(Maybe NotifyConfig)
      , configFifoIndex :: !Bool
      , configTopicBindings :: ![TopicPattern]
      }

    data QueueType
      = StandardQueue
      | UnloggedQueue
      | PartitionedQueue !PartitionConfig

    data PartitionConfig = PartitionConfig
      { partitionInterval :: !Text
      , retentionInterval :: !Text
      }

    data NotifyConfig = NotifyConfig
      { notifyThrottleMs :: !(Maybe Int32)
      }

Provide smart constructors for common cases:

    -- Create a standard queue with no extras
    standardQueue :: QueueName -> QueueConfig

    -- Create an unlogged queue (faster, no WAL, lost on crash)
    unloggedQueue :: QueueName -> QueueConfig

    -- Create a partitioned queue
    partitionedQueue :: QueueName -> PartitionConfig -> QueueConfig

And modifier functions for a builder-style API:

    -- Enable LISTEN/NOTIFY on message insert
    withNotifyInsert :: Maybe Int32 -> QueueConfig -> QueueConfig

    -- Add a FIFO index for strict ordering
    withFifoIndex :: QueueConfig -> QueueConfig

    -- Bind a topic pattern for AMQP-style routing
    withTopicBinding :: TopicPattern -> QueueConfig -> QueueConfig

This lets users write:

    myQueues :: [QueueConfig]
    myQueues =
      [ standardQueue [queueName|order_events|]
          & withNotifyInsert (Just 1000)
          & withFifoIndex
          & withTopicBinding [topicPattern|orders.*|]
      , unloggedQueue [queueName|ephemeral_tasks|]
      ]

Acceptance: `cabal build pgmq-config` succeeds, and the types are importable.


### Milestone 3: Reconciliation Logic

Implement the core `ensureQueues` function in `pgmq-config/src/Pgmq/Config.hs`. This function takes a list of `QueueConfig` values and ensures all declared queues exist with their desired settings.

The reconciliation strategy is additive-only and idempotent:

1. For each `QueueConfig`, call the appropriate create function (`createQueue`, `createUnloggedQueue`, or `createPartitionedQueue`). Since pgmq's create is idempotent, this is safe to call even if the queue already exists.

2. If `configNotifyInsert` is `Just config`, call `enableNotifyInsert`. If `Nothing`, do nothing (do not disable — additive only).

3. If `configFifoIndex` is `True`, call `createFifoIndex`. The underlying SQL uses `IF NOT EXISTS` for index creation.

4. For each topic pattern in `configTopicBindings`, call `bindTopic`. If the binding already exists, pgmq handles it gracefully.

The function signature should be:

    ensureQueues :: [QueueConfig] -> Session ()

This operates in the hasql `Session` monad so callers can run it however they like — directly with a connection, through a pool, or wrapped in effectful.

Provide a convenience wrapper that works with a pool:

    ensureQueuesWithPool :: Pool -> [QueueConfig] -> IO (Either Pool.UsageError ())

And a report variant that returns what actions were taken:

    data ReconcileAction
      = CreatedQueue QueueName QueueType
      | EnabledNotify QueueName (Maybe Int32)
      | CreatedFifoIndex QueueName
      | BoundTopic QueueName TopicPattern
      | Skipped QueueName Text  -- reason

    ensureQueuesReport :: [QueueConfig] -> Session [ReconcileAction]

The report variant queries `listQueues` first, compares against the desired state, and only takes actions for what is missing. It returns a log of what it did. This is useful for startup logging.

Acceptance: `cabal build pgmq-config` succeeds.


### Milestone 4: Effectful Integration

Add an optional module `pgmq-config/src/Pgmq/Config/Effectful.hs` that provides effectful-friendly wrappers. This module depends on `pgmq-effectful` and `effectful-core`.

    ensureQueuesEff :: (Pgmq :> es) => [QueueConfig] -> Eff es ()

    ensureQueuesReportEff :: (Pgmq :> es) => [QueueConfig] -> Eff es [ReconcileAction]

These use the `Pgmq` effect constructors (`CreateQueue`, `EnableNotifyInsert`, etc.) instead of hasql sessions directly. This lets the reconciliation participate in the same effect stack as the rest of the application.

Because this introduces a dependency on `pgmq-effectful` and `effectful-core`, guard it behind a cabal flag:

    flag effectful
      description: Enable effectful integration
      default: True
      manual: False

    library
      if flag(effectful)
        exposed-modules: Pgmq.Config.Effectful
        build-depends: pgmq-effectful, effectful-core

Acceptance: `cabal build pgmq-config` succeeds with and without the effectful flag.


### Milestone 5: Tests

Add tests in `pgmq-config/test/` using the same ephemeral-pg pattern as `pgmq-hasql/test/EphemeralDb.hs`. The test suite should:

1. Create an ephemeral PostgreSQL instance and run pgmq migrations.
2. Define a set of `QueueConfig` values.
3. Call `ensureQueuesReport` and verify the correct actions were taken.
4. Call `ensureQueuesReport` again and verify all actions are `Skipped` (idempotency).
5. Add a new queue to the config list, call again, and verify only the new queue is created.
6. Test each queue type (standard, unlogged, partitioned).
7. Test notification, FIFO index, and topic binding configuration.

Test commands (from repository root):

    cabal test pgmq-config

Acceptance: All tests pass. The test output shows queue creation on first run and skipping on second run.


### Milestone 6: End-to-End Validation

Build all packages together and run the full test suite:

    cabal build all
    cabal test all

Verify no regressions in existing packages. Format the code:

    nix fmt

Acceptance: All packages build, all tests pass, code is formatted.


## Concrete Steps

All commands run from the repository root: `/Users/shinzui/Keikaku/bokuno/libraries/pgmq-hs-project/pgmq-hs/`.

Milestone 0 — read `vendor/pgmq/pgmq-extension/sql/pgmq.sql` and search for the `create` function. Expected: find `CREATE OR REPLACE FUNCTION pgmq.create(...)` that internally uses `IF NOT EXISTS`. Verify `listQueues` session in `pgmq-hasql/src/Pgmq/Hasql/Sessions.hs`. Check `enableNotifyInsert` and `bindTopic` for idempotency behavior.

Milestone 1:

    mkdir -p pgmq-config/src/Pgmq/Config
    mkdir -p pgmq-config/test

Create `pgmq-config/pgmq-config.cabal`, `pgmq-config/src/Pgmq/Config.hs`, and `pgmq-config/src/Pgmq/Config/Types.hs`. Add `pgmq-config` to `cabal.project`. Run:

    cabal build pgmq-config

Expected: build succeeds with no errors.

Milestones 2–4: edit the files created in Milestone 1. After each:

    cabal build pgmq-config

Expected: build succeeds.

Milestone 5: create test files and run:

    cabal test pgmq-config

Expected output (approximate):

    Test suite pgmq-config-test: RUNNING...
    Queue Configuration Tests
      ensureQueues creates all queues:         OK
      ensureQueues is idempotent:              OK
      ensureQueues handles incremental adds:   OK
      standard queue config:                   OK
      unlogged queue config:                   OK
      partitioned queue config:                OK
      notify insert config:                    OK
      FIFO index config:                       OK
      topic binding config:                    OK
    All 9 tests passed.

Milestone 6:

    cabal build all && cabal test all && nix fmt


## Validation and Acceptance

The package is accepted when:

1. `cabal build all` succeeds — pgmq-config integrates cleanly with the existing workspace.
2. `cabal test pgmq-config` passes all tests, demonstrating:
   - Queue creation from config (standard, unlogged, partitioned).
   - Notification, FIFO index, and topic binding setup.
   - Idempotency — running `ensureQueues` twice produces no errors and no duplicate actions.
   - Incremental — adding a new config and re-running creates only the new queue.
3. `cabal test all` passes — no regressions in existing packages.
4. `nix fmt` produces no changes — code is properly formatted.

To manually verify beyond tests, a user can open a GHCi session:

    cabal repl pgmq-config

And evaluate:

    > import Pgmq.Config
    > import Pgmq.Types (parseQueueName)
    > let Right qn = parseQueueName "my_queue"
    > let cfg = standardQueue qn & withNotifyInsert Nothing & withFifoIndex
    > configQueueName cfg
    QueueName "my_queue"
    > configFifoIndex cfg
    True


## Idempotence and Recovery

Every step in this plan is idempotent:

- `cabal build` is always safe to re-run.
- Queue creation via `pgmq.create()` uses `IF NOT EXISTS` internally — calling it for an existing queue is a no-op.
- `enableNotifyInsert` can be called multiple times safely (it upserts the throttle config).
- `createFifoIndex` uses `CREATE INDEX IF NOT EXISTS`.
- `bindTopic` — needs verification in Milestone 0; if not idempotent, the reconciliation must query existing bindings first and skip duplicates.

If any milestone fails partway through, the implementer can re-run from the beginning of that milestone. No destructive operations are involved.


## Interfaces and Dependencies

**Dependencies:**
- `pgmq-core` — for `QueueName`, `TopicPattern`, `Queue`, and other types
- `pgmq-hasql` — for `Session`-level queue management functions (`createQueue`, `enableNotifyInsert`, etc.) and the parameter types (`CreatePartitionedQueue`, `EnableNotifyInsert`, `BindTopic`)
- `hasql` — for the `Session` monad type
- `hasql-pool` — for the `Pool` type and `Pool.use` in the convenience wrapper
- `text` — for `Text` in partition config
- `pgmq-effectful` (optional, behind flag) — for the `Pgmq` effect type
- `effectful-core` (optional, behind flag) — for `Eff`, `:>`, effect dispatch

**Types and signatures that must exist at the end of Milestone 3:**

In `pgmq-config/src/Pgmq/Config/Types.hs`:

    data QueueConfig
    data QueueType = StandardQueue | UnloggedQueue | PartitionedQueue PartitionConfig
    data PartitionConfig
    data NotifyConfig
    data ReconcileAction

    standardQueue :: QueueName -> QueueConfig
    unloggedQueue :: QueueName -> QueueConfig
    partitionedQueue :: QueueName -> PartitionConfig -> QueueConfig
    withNotifyInsert :: Maybe Int32 -> QueueConfig -> QueueConfig
    withFifoIndex :: QueueConfig -> QueueConfig
    withTopicBinding :: TopicPattern -> QueueConfig -> QueueConfig

In `pgmq-config/src/Pgmq/Config.hs`:

    ensureQueues :: [QueueConfig] -> Session ()
    ensureQueuesWithPool :: Pool -> [QueueConfig] -> IO (Either Pool.UsageError ())
    ensureQueuesReport :: [QueueConfig] -> Session [ReconcileAction]

**Types and signatures that must exist at the end of Milestone 4:**

In `pgmq-config/src/Pgmq/Config/Effectful.hs`:

    ensureQueuesEff :: (Pgmq :> es) => [QueueConfig] -> Eff es ()
    ensureQueuesReportEff :: (Pgmq :> es) => [QueueConfig] -> Eff es [ReconcileAction]
