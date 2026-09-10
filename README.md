# pgmq-hs

Haskell client for [pgmq](https://github.com/tembo-io/pgmq)

**PGMQ 1.12/1.13 supported**: grouped-head reads require 1.12; explicit premake and default-partition estimates require 1.13. `pgmq-migration` installs the complete 1.13 native schema.

See [upgrading to 0.6](docs/user/pgmq-0.6-upgrade.md) for the two record changes, public grouped APIs, compatibility matrix, and partition recovery.

The API may evolve before 1.0.

## Packages

| Package | Description |
|---------|-------------|
| `pgmq-core` | Core types and type classes |
| `pgmq-hasql` | Hasql-based implementation |
| `pgmq-effectful` | Effectful effects for pgmq, with OpenTelemetry-traced interpreters |
| `pgmq-config` | Declarative queue configuration (idempotent reconciliation at startup) |
| `pgmq-migration` | Schema migrations without pgmq extension |

## pgmq-hasql

The main package for interacting with pgmq from Haskell, built on [hasql](https://hackage.haskell.org/package/hasql).

```haskell
import Data.Aeson (object, (.=))
import Hasql.Pool qualified as Pool
import Pgmq

main :: IO ()
main = do
  pool <- Pool.acquire poolConfig

  let Right queue = parseQueueName "my_queue"

  -- Create a queue
  Right () <- Pool.use pool (createQueue queue)

  -- Send a message
  let body = MessageBody (object ["hello" .= ("world" :: String)])
  Right msgId <- Pool.use pool (sendMessage SendMessage {queueName = queue, messageBody = body, delay = Nothing})
  print msgId

  -- Read messages (visibility timeout 30s, batch size 1)
  Right msgs <- Pool.use pool (readMessage ReadMessage {queueName = queue, delay = 30, batchSize = Just 1, conditional = Nothing})
  print msgs
```

## pgmq-effectful

The `pgmq-effectful` package provides an [Effectful](https://hackage.haskell.org/package/effectful) effect layer over `pgmq-hasql`. It ships a plain interpreter (`runPgmq`) and a traced interpreter (`runPgmqTraced`) built against `hs-opentelemetry` 1.0. By default the traced interpreter preserves the older v1.24 attribute names for compatibility; set `OTEL_SEMCONV_STABILITY_OPT_IN=messaging,database` to emit stable messaging and database semantic-convention attributes, or `messaging/dup,database/dup` to emit both old and stable attributes during migration. Trace-context propagation remains pluggable through whichever propagator the `TracerProvider` is configured with (W3C, B3, Datadog, …).

See [effectful grouped heads and partition controls](docs/user/effectful-grouped-reads.md) for PGMQ 1.12/1.13 operations, tracing labels, and nullable metrics.

See [`pgmq-effectful/CHANGELOG.md`](pgmq-effectful/CHANGELOG.md) for the 0.2.0.0 migration notes — attribute names, span-name format, and the error-type rename all changed.

### Error handling

Both interpreters (`runPgmq` and `runPgmqTraced`) surface failures through the `Error` effect as a `PgmqRuntimeError`:

```haskell
import Effectful (runEff)
import Effectful.Error.Static (runError)
import Pgmq.Effectful

run pool = do
  result <- runEff . runError @PgmqRuntimeError . runPgmq pool $ do
    createQueue myQueue
    sendMessage SendMessage {queueName = myQueue, messageBody = body, delay = Nothing}
  case result of
    Right msgId -> print msgId
    Left (_cs, err)
      | isTransient err -> retry
      | otherwise -> logFatal err
```

`PgmqRuntimeError` has three constructors — `PgmqAcquisitionTimeout`, `PgmqConnectionError`, `PgmqSessionError` — each exposing the full hasql context (SQL state, connection-error kind, etc.) so applications can pattern-match on whatever granularity they need. See [`docs/design/013-pgmq-effectful-error-model.md`](docs/design/013-pgmq-effectful-error-model.md) for the rationale.

## pgmq-config

Declare your queue topology as Haskell values and reconcile it at startup. Every operation is idempotent, so `ensureQueues` is safe to call on every boot:

```haskell
import Data.Function ((&))
import Pgmq.Config
import Pgmq.Types (parseQueueName, parseTopicPattern)

myQueues :: [QueueConfig]
myQueues =
  let Right orders = parseQueueName "order_events"
      Right tasks  = parseQueueName "background_tasks"
      Right pat    = parseTopicPattern "orders.*"
   in [ standardQueue orders
          & withNotifyInsert (Just 1000)
          & withFifoIndex
          & withTopicBinding pat
      , unloggedQueue tasks
      ]

main :: IO ()
main = do
  pool <- acquirePool
  Right () <- ensureQueuesWithPool pool myQueues
  startWorkers pool
```

Supports standard, unlogged, and partitioned queues; `LISTEN/NOTIFY` throttles; FIFO indexes; and topic bindings. `ensureQueuesReport` returns a list of `ReconcileAction` values so you can log exactly what changed. An `effectful` integration (`Pgmq.Config.Effectful`) is enabled by default via a cabal flag. See [`docs/user/queue-configuration.md`](docs/user/queue-configuration.md) for the full reference.

## pgmq-migration

The `pgmq-migration` package allows you to install the PGMQ schema into PostgreSQL without requiring the pgmq extension. This is useful when you don't have superuser access or can't install extensions.

`pgmq-migration` does not ship a migration runner of its own. It exposes the PGMQ schema
as a [`pg-migrate`](https://hackage.haskell.org/package/pg-migrate) *component*, and you
run it with `pg-migrate`. Depend on both:

```cabal
build-depends:
  , pgmq-migration
  , pg-migrate
  -- only if importing an existing hasql-migration ledger:
  , pg-migrate-import-hasql-migration
```

`pgmqMigrations` comes from `Pgmq.Migration`; everything else below (`migrationPlan`,
`runMigrationPlan`, `defaultRunOptions`, …) comes from `pg-migrate`'s
`Database.PostgreSQL.Migrate`.

### Fresh Installation

Build a one-component plan and run it:

```haskell
import Data.List.NonEmpty (NonEmpty (..))
import Database.PostgreSQL.Migrate
  (defaultRunOptions, migrationPlan, runMigrationPlan)
import Pgmq.Migration (pgmqMigrations)

main :: IO ()
main = do
  component <- either (fail . show) pure pgmqMigrations
  plan <- either (fail . show) pure (migrationPlan (component :| []))
  result <- runMigrationPlan defaultRunOptions connectionSettings plan
  case result of
    Right report -> print report
    Left err     -> print err
```

`connectionSettings` is a `Hasql.Connection.Settings.Settings`. Running the plan again is
idempotent — every migration reports `AlreadyApplied`.

The component is named `pgmq`, has no dependencies, and contains the exact vendored PGMQ
1.11 baseline `0001-install-v1.11.0`, schema marker 0002, local hardening 0003,
upstream 1.12/1.13 upgrades 0004/0005, and local partition re-entry preservation 0006.
Compose it with other components in their dependency-ordered plan.

### Importing Existing Installations

Databases whose PGMQ schema was installed by **pgmq-migration 0.3.0.0 or earlier** are
tracked in a `hasql-migration` ledger (`public.schema_migrations`). The native runner does
not read that table, so you must import the ledger **once** before running the plan —
otherwise the runner would try to reinstall a schema that is already there.

Pick exactly one policy, matching how the database was originally installed:

- **`DirectFullInstallHistory`** — installed in one step as `pgmq_v1.11.0`. The adapter
  reproduces the stored base64 MD5 and requires exact payload equality with the native
  baseline.
- **`EquivalentTwoStepUpgradeHistory`** — installed at v1.10.0 and upgraded through
  v1.10.1 to v1.11.0. The payloads differ from the native baseline by construction, so
  this route additionally verifies a read-only PGMQ 1.11 catalog contract. It is refused
  unless you opt in with `withEquivalentHistory AllowEquivalentHistory`.

The existing `pgmqHasqlMigrationSourceConfig` helper requires every predecessor-ledger row
to belong to PGMQ. If `public.schema_migrations` is intentionally shared with application
migrations, use `pgmqHasqlMigrationSourceConfigWithPolicy` and the explicit
`AllowUnselectedSourceRows` policy. Run `readHasqlMigrationHistory` first and review its
`unselectedRows`; selected PGMQ rows still require exact checksum evidence, and unrelated
rows remain untouched. The full guide below includes the complete example.

```haskell
import Data.List.NonEmpty (NonEmpty (..))
import Database.PostgreSQL.Migrate
  ( EquivalentHistoryPolicy (AllowEquivalentHistory)
  , connectionProviderFromSettings
  , defaultImportOptions
  , defaultRunOptions
  , migrationPlan
  , runMigrationPlan
  , withEquivalentHistory
  )
import Database.PostgreSQL.Migrate.History.HasqlMigration (importHasqlMigrationHistory)
import Pgmq.Migration (pgmqMigrations)
import Pgmq.Migration.History.HasqlMigration
  ( AlternativeHistoryPolicy (..)
  , pgmqHasqlMigrationMappings
  , pgmqHasqlMigrationSourceConfig
  )

main :: IO ()
main = do
  let policy   = DirectFullInstallHistory   -- or EquivalentTwoStepUpgradeHistory
      provider = connectionProviderFromSettings connectionSettings
      options  = defaultImportOptions
      -- for EquivalentTwoStepUpgradeHistory, instead:
      -- options = withEquivalentHistory AllowEquivalentHistory defaultImportOptions

  component <- either (fail . show) pure pgmqMigrations
  plan      <- either (fail . show) pure (migrationPlan (component :| []))
  config    <- either (fail . show) pure (pgmqHasqlMigrationSourceConfig provider policy)
  mappings  <- either (fail . show) pure (pgmqHasqlMigrationMappings policy)

  -- 1. Import the old ledger. Verifies checksums; runs no schema SQL.
  importReport <- importHasqlMigrationHistory options config provider plan mappings
  either (fail . show) print importReport

  -- 2. Now run the native plan. The baseline is AlreadyApplied and is not replayed;
  --    pending migrations 0002 through 0006 are AppliedNow.
  runReport <- runMigrationPlan defaultRunOptions connectionSettings plan
  either (fail . show) print runReport
```

The import is itself idempotent — a second call reports `AlreadyImported`. It verifies
rather than trusts: an altered payload, a tampered checksum, a duplicate ledger row, or
(on the equivalent route) a PGMQ schema that fails the 1.11 contract all abort the import
without touching your schema.

See [`docs/user/schema-migration.md`](docs/user/schema-migration.md) for the full guide,
including how to choose a policy and what each failure means.

## Nix Build

All packages can be built with Nix via `callCabal2nix`. This provides reproducible builds and makes the packages consumable as flake inputs by other Nix projects.

### Building

```bash
# Build a specific package
nix build .#pgmq-core
nix build .#pgmq-hasql
nix build .#pgmq-effectful
nix build .#pgmq-config
nix build .#pgmq-migration

# Build the default package (pgmq-hasql)
nix build
```

### Checks

`nix flake check` verifies formatting, pre-commit hooks, library compilation, and test suites:

```bash
# Run all checks
nix flake check
```

Individual checks can be built directly:

```bash
# Library compilation (no tests)
nix build .#checks.$(nix eval --impure --raw --expr builtins.currentSystem).pgmq-core
nix build .#checks.$(nix eval --impure --raw --expr builtins.currentSystem).pgmq-hasql
nix build .#checks.$(nix eval --impure --raw --expr builtins.currentSystem).pgmq-effectful
nix build .#checks.$(nix eval --impure --raw --expr builtins.currentSystem).pgmq-config
nix build .#checks.$(nix eval --impure --raw --expr builtins.currentSystem).pgmq-migration

# Test suites (compile + run tests with ephemeral PostgreSQL)
nix build .#checks.$(nix eval --impure --raw --expr builtins.currentSystem).pgmq-hasql-tests
nix build .#checks.$(nix eval --impure --raw --expr builtins.currentSystem).pgmq-config-tests
nix build .#checks.$(nix eval --impure --raw --expr builtins.currentSystem).pgmq-migration-tests
```

### Development Shell

```bash
# Enter the dev shell (GHC 9.12.2, cabal, PostgreSQL, HLS)
nix develop

# Then use cabal as usual
cabal build all
cabal test all
```

### Consuming as a Flake Input

```nix
{
  inputs.pgmq-hs.url = "github:shinzui/pgmq-hs";

  outputs = { self, pgmq-hs, ... }: {
    # Access packages
    # pgmq-hs.packages.${system}.pgmq-core
    # pgmq-hs.packages.${system}.pgmq-hasql
    # pgmq-hs.packages.${system}.pgmq-effectful
    # pgmq-hs.packages.${system}.pgmq-config
    # pgmq-hs.packages.${system}.pgmq-migration
  };
}
```
