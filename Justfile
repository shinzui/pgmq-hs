# Justfile for pgmq-hs development

# Default recipe to display help
default:
    @just --list

# Start PostgreSQL and other services with process-compose
process-up:
    process-compose up

# Start services in the background
process-up-background:
    process-compose up -d

# Stop all services
process-down:
    process-compose down

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

# Format code
fmt:
    nix fmt

# Clean build artifacts
clean:
    cabal clean

# Check database status
db-status:
    pg_isready -h $PGHOST && echo "PostgreSQL is running" || echo "PostgreSQL is not running"
