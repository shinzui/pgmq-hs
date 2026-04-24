# pgmq-hs

Haskell client for [pgmq](https://github.com/tembo-io/pgmq)

**Requires pgmq 1.11.0+** for full functionality (or use `pgmq-migration` to install the schema without the extension).

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

The `pgmq-effectful` package provides an [Effectful](https://hackage.haskell.org/package/effectful) effect layer over `pgmq-hasql`. It ships a plain interpreter (`runPgmq`) and a traced interpreter (`runPgmqTraced`) that emits OpenTelemetry spans conforming to [Semantic Conventions v1.24](https://github.com/open-telemetry/semantic-conventions/tree/v1.24.0), with trace-context propagation pluggable through whichever propagator the `TracerProvider` is configured with (W3C, B3, Datadog, …).

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

### Fresh Installation

For new projects, use `migrate` to install the complete PGMQ schema:

```haskell
import Hasql.Connection (acquire)
import Hasql.Session (run)
import Pgmq.Migration (migrate)

main :: IO ()
main = do
  Right conn <- acquire connectionSettings
  result <- run migrate conn
  case result of
    Right (Right ()) -> putStrLn "Migration successful"
    Right (Left err) -> print err
    Left sessionErr  -> print sessionErr
```

### Upgrading Existing Installations

For projects that previously installed PGMQ via this package, use `upgrade` to apply only the incremental changes needed to reach the current version:

```haskell
import Hasql.Connection (acquire)
import Hasql.Session (run)
import Pgmq.Migration (upgrade)

main :: IO ()
main = do
  Right conn <- acquire connectionSettings
  result <- run upgrade conn
  case result of
    Right (Right ()) -> putStrLn "Upgrade successful"
    Right (Left err) -> print err
    Left sessionErr  -> print sessionErr
```

### Which Function Should I Use?

| Scenario | Function |
|----------|----------|
| New project, fresh database | `migrate` |
| Existing project using pgmq-migration | `upgrade` |
| Not sure | `upgrade` (safe on fresh databases too) |

Both functions are idempotent - migrations that have already been applied will be skipped. The `hasql-migration` library tracks applied migrations in the `schema_migrations` table.

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
    # pgmq-hs.packages.${system}.pgmq-migration
  };
}
```
