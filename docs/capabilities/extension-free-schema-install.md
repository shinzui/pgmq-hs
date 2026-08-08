---
title: "Extension-free pgmq schema installation"
type: Capability
description: "Install the pgmq schema into PostgreSQL without the pgmq extension, as a composable pg-migrate component, for deployments that cannot install extensions."
generated:
  by: adopt-capabilities/0.9.2
  at: "2026-08-08T00:00:00Z"
capabilityId: CAP-7
provider: mori://shinzui/pgmq-hs
status: shipped
stability: experimental
since: "0.1.0.0"
packages:
  - pgmq-migration
interface:
  - Pgmq.Migration
evidence:
  - kind: test
    resource: pgmq-migration/test/Main.hs
    proves: The native pgmqMigrations component installs and re-runs idempotently against an ephemeral PostgreSQL, with a plan-derived ledger.
  - kind: guide
    resource: docs/user/schema-migration.md
    proves: How to compose pgmqMigrations into a pg-migrate plan and run it under a single ledger.
---

# Extension-free pgmq schema installation

`pgmq-migration` installs the pgmq schema into a plain PostgreSQL database **without**
requiring the `pgmq` extension — the case that matters when you lack superuser access or
your platform forbids extensions. It ships the schema as a `pg-migrate` *component*, so
pgmq composes into your application's own migration plan under one ledger, in dependency
order. This is a single, self-contained adoption decision proven by its own test suite.

What it provides:

- `pgmqMigrations :: Either DefinitionError MigrationComponent` — the native migration
  component a consumer folds into a `pg-migrate` plan.
- `MigrationComponent`, `DefinitionError` — the composition types.

The SQL itself is vendored verbatim from upstream pgmq (via git subtree), not hand-written.

## Shape

```haskell
import Pgmq.Migration (pgmqMigrations)
import Database.PostgreSQL.Migrate (runMigrations, defaultRunOptions)

case pgmqMigrations of
  Left err  -> error (show err)
  Right cmp -> runMigrations defaultRunOptions provider plan   -- plan includes cmp
```

## Limits

- **No runner is included.** `pgmq-migration` contains only the component; the consumer
  composes and executes a `pg-migrate` plan. This is deliberate (pgmq shares your ledger)
  but means adopting it pulls in `pg-migrate`.
- **The public API was reshaped in 0.4.0.0.** Releases before 0.4.0.0 exposed a
  `hasql-migration` runner surface (`migrate`, `upgrade`, `validate`, and the
  `Pgmq.Migration.Migrations.*` / `.Sessions` / `.Statements` modules); all of that was
  removed in favour of the native component. A ledger written by a pre-0.4 release must be
  imported before the native runner takes over — see
  [predecessor-history import](predecessor-history-import.md).
- Pre-1.0 and uniformly `experimental`.
