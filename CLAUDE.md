# CLAUDE.md

This file provides guidance for Claude Code when working with this repository.

## Project Overview

pgmq-hs is a Haskell client library for [pgmq](https://github.com/tembo-io/pgmq), a lightweight message queue built on PostgreSQL. The project is organized as a multi-package Cabal project.

## Packages

- **pgmq-core**: Core types and type classes
- **pgmq-hasql**: Hasql-based implementation
- **pgmq-effectful**: Effectful effects for pgmq
- **pgmq-migration**: Schema migrations without pgmq extension

## Build Commands

```bash
# Build all packages
cabal build all

# Build a specific package
cabal build pgmq-hasql

# Run tests
cabal test all

# Run tests for a specific package
cabal test pgmq-migration
```

## Formatting

**Important**: Run `nix fmt` before committing to ensure code is properly formatted.

```bash
nix fmt
```

The project uses treefmt with pre-commit hooks. If you commit without formatting, the pre-commit hook will fail and format the files automatically - you'll need to stage the formatted files and commit again.

## Development Environment

Enter the development shell with:

```bash
nix develop
```

This provides GHC 9.12.2, cabal-install, PostgreSQL, and haskell-language-server.

## Code Style

- Use `ImportQualifiedPost` for qualified imports
- Use `OverloadedStrings` for string literals
- Follow existing patterns in the codebase for module structure

## Testing

Tests use `ephemeral-pg` to spin up temporary PostgreSQL instances. No external database setup is required.

## Adding New pgmq Functions

1. Add the SQL statement to `pgmq-hasql/src/Pgmq/Hasql/Statements.hs`
2. Add the session wrapper to `pgmq-hasql/src/Pgmq/Hasql/Sessions.hs`
3. Export from `pgmq-hasql/src/Pgmq.hs`
4. Add corresponding types to `pgmq-core` if needed
5. Update the effectful wrapper in `pgmq-effectful` if needed

## pgmq-migration

When updating the pgmq schema version:

1. Create a new version directory under `pgmq-migration/database/`
2. Add SQL files following the numbered naming convention
3. Create a new version module under `Pgmq.Migration.Migrations`
4. Update `Pgmq.Migration.Migrations` to export the new version
