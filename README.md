### pgmq-hs

Haskell client for [pgmq](https://github.com/tembo-io/pgmq)

**Requires pgmq 1.10.0+** for full functionality (or use `pgmq-migration` to install the schema without the extension).

## Packages

| Package | Description |
|---------|-------------|
| `pgmq-core` | Core types and type classes |
| `pgmq-hasql` | Hasql-based implementation |
| `pgmq-effectful` | Effectful effects for pgmq |
| `pgmq-migration` | Schema migrations without pgmq extension |

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

## Supported API

- [x] [Sending Messages](https://tembo.io/pgmq/api/sql/functions/#sending-messages)
  - [x] [send](https://tembo.io/pgmq/api/sql/functions/#send) - with headers support (1.5.0+)
  - [x] [send_batch](https://tembo.io/pgmq/api/sql/functions/#send_batch) - with headers support (1.5.0+)
- [x] [Reading Messages](https://tembo.io/pgmq/api/sql/functions/#reading-messages)
  - [x] [read](https://tembo.io/pgmq/api/sql/functions/#read) - with conditional filtering (1.5.0+)
  - [x] [read_with_poll](https://tembo.io/pgmq/api/sql/functions/#read_with_poll)
  - [x] [pop](https://tembo.io/pgmq/api/sql/functions/#pop) - with quantity parameter (1.7.0+)
  - [x] read_fifo - strict FIFO ordering (1.8.0+)
  - [x] read_fifo_with_poll - strict FIFO ordering with polling (1.9.0+)
- [x] [Deleting/Archiving Messages](https://tembo.io/pgmq/api/sql/functions/#deletingarchiving-messages)
  - [x] [delete (single)](https://tembo.io/pgmq/api/sql/functions/#delete-single)
  - [x] [delete (batch)](https://tembo.io/pgmq/api/sql/functions/#delete-batch)
  - [x] [purge_queue](https://tembo.io/pgmq/api/sql/functions/#purge_queue)
  - [x] [archive (single)](https://tembo.io/pgmq/api/sql/functions/#archive-single)
  - [x] [archive (batch)](https://tembo.io/pgmq/api/sql/functions/#archive-batch)
- [x] [Queue Management](https://tembo.io/pgmq/api/sql/functions/#queue-management)
  - [x] [create](https://tembo.io/pgmq/api/sql/functions/#create)
  - [x] [create_partitioned](https://tembo.io/pgmq/api/sql/functions/#create_partitioned)
  - [x] [create_unlogged](https://tembo.io/pgmq/api/sql/functions/#create_unlogged)
  - [x] [detach_archive](https://tembo.io/pgmq/api/sql/functions/#detach_archive) - **DEPRECATED** (no-op in pgmq 2.0)
  - [x] [drop_queue](https://tembo.io/pgmq/api/sql/functions/#drop_queue)
  - [x] enable_notify_insert (1.7.0+) - with throttling (1.8.0+)
  - [x] disable_notify_insert (1.7.0+)
- [x] [Utilities](https://tembo.io/pgmq/api/sql/functions/#utilities)
  - [x] [set_vt](https://tembo.io/pgmq/api/sql/functions/#set_vt) - single and batch (1.8.0+)
  - [x] [list_queues](https://tembo.io/pgmq/api/sql/functions/#list_queues)
  - [x] [metrics](https://tembo.io/pgmq/api/sql/functions/#metrics) - includes queue_visible_length (1.5.0+)
  - [x] [metrics_all](https://tembo.io/pgmq/api/sql/functions/#metrics_all)
- Partition
  - [x] [create_partitioned](https://tembo.io/pgmq/api/sql/functions/#create_partitioned)
  - [ ] [show_partitions](https://github.com/pgpartman/pg_partman/blob/development/doc/pg_partman.md#show_partitions)
  - [ ] [run_maintenance](https://github.com/pgpartman/pg_partman/blob/development/doc/pg_partman.md#run_maintenance)

## Nix Build

All packages can be built with Nix via `callCabal2nix`. This provides reproducible builds and makes the packages consumable as flake inputs by other Nix projects.

### Building

```bash
# Build a specific package
nix build .#pgmq-core
nix build .#pgmq-hasql
nix build .#pgmq-effectful
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
nix build .#checks.$(nix eval --impure --raw --expr builtins.currentSystem).pgmq-migration

# Test suites (compile + run tests with ephemeral PostgreSQL)
nix build .#checks.$(nix eval --impure --raw --expr builtins.currentSystem).pgmq-hasql-tests
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
  inputs.pgmq-hs.url = "github:topagentnetwork/pgmq-hs";

  outputs = { self, pgmq-hs, ... }: {
    # Access packages
    # pgmq-hs.packages.${system}.pgmq-core
    # pgmq-hs.packages.${system}.pgmq-hasql
    # pgmq-hs.packages.${system}.pgmq-effectful
    # pgmq-hs.packages.${system}.pgmq-migration
  };
}
```
