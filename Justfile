# Justfile for pgmq-hs development

# Default recipe to display help
default:
    @just --list


# --- Services ---
[group("services")]
process-up:
  process-compose --tui=false --unix-socket .dev/process-compose.sock up

[group("services")]
process-down:
  process-compose --unix-socket .dev/process-compose.sock down || true


# Create the development database
create-database:
    createdb -h $PGHOST $PGDATABASE 2>/dev/null || echo "Database already exists"

# Drop and recreate the development database
reset-database:
    dropdb -h $PGHOST $PGDATABASE 2>/dev/null || true
    createdb -h $PGHOST $PGDATABASE

# Connect to development database with psql
psql:
    psql -h $PGHOST $PGDATABASE

# Build all packages
build:
    cabal build all

# Run all tests
test:
    cabal test all

# Run tests for a specific package
test-package PACKAGE:
    cabal test {{PACKAGE}}

# Run benchmarks (requires running PostgreSQL)
bench:
    cabal bench pgmq-bench

# Format code
fmt:
    nix fmt

# Clean build artifacts
clean:
    cabal clean

# Check database status
db-status:
    pg_isready -h $PGHOST && echo "PostgreSQL is running" || echo "PostgreSQL is not running"

# --- Claude ---

# Symlink claude skills into .claude/commands
[group("claude")]
setup-skills:
    mkdir -p .claude
    ln -sfn ../claude/skills .claude/skills

# --- Nix ---

# Build all packages with Nix
[group("nix")]
nix-build:
    nix build .#pgmq-core
    nix build .#pgmq-hasql
    nix build .#pgmq-effectful
    nix build .#pgmq-migration

# Build a specific package with Nix
[group("nix")]
nix-build-package PACKAGE:
    nix build .#{{PACKAGE}}

# Run all Nix checks (formatting, builds, tests)
[group("nix")]
nix-check:
    nix flake check

# Run test suites with Nix (ephemeral PostgreSQL)
[group("nix")]
nix-test:
    nix build .#checks.$(nix eval --impure --raw --expr builtins.currentSystem).pgmq-hasql-tests
    nix build .#checks.$(nix eval --impure --raw --expr builtins.currentSystem).pgmq-migration-tests
