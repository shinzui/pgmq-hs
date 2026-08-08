---
title: "Predecessor-history ledger import and schema contract"
type: Capability
description: "Import an existing hasql-migration pgmq ledger onto the native baseline with exact checksum verification, guarded by a read-only PGMQ 1.11 schema contract."
generated:
  by: adopt-capabilities/0.9.2
  at: "2026-08-08T00:00:00Z"
capabilityId: CAP-8
provider: mori://shinzui/pgmq-hs
status: shipped
stability: experimental
since: "0.4.0.0"
packages:
  - pgmq-migration
requires:
  - CAP-7
interface:
  - Pgmq.Migration.History.HasqlMigration
  - Pgmq.Migration.SchemaContract
evidence:
  - kind: test
    resource: pgmq-migration/test/Main.hs
    proves: The direct full-install and opted-in equivalent two-step import routes verify checksums and the schema contract before the native runner takes over.
  - kind: module
    resource: pgmq-migration/src/Pgmq/Migration/SchemaContract.hs
    proves: pgmqV1_11StateValidator checks the PGMQ 1.11 schemas, tables, columns, constraints, types, and function identities pgmq-hs depends on without mutating state.
---

# Predecessor-history ledger import and schema contract

A deployment that installed pgmq through a pre-0.4 release of this package holds a
`hasql-migration` ledger the native runner does not read on its own. This capability maps
that predecessor ledger onto the [native baseline](extension-free-schema-install.md) so the
cutover to the native `pg-migrate` runner is safe, verified by exact checksum and — for the
riskier route — a read-only schema contract. It is only relevant when migrating an existing
installation, is proven by its own import tests, and arrived in 0.4.0.0, so it is a separate
capability that requires [extension-free schema installation](extension-free-schema-install.md).

What it provides:

- `pgmqHasqlMigrationMappings`, `pgmqHasqlMigrationSourceConfig`,
  `pgmqHasqlMigrationSourceConfigWithPolicy` — the adapter that imports a predecessor
  ledger.
- `AlternativeHistoryPolicy` (`DirectFullInstallHistory` |
  `EquivalentTwoStepUpgradeHistory`) and `SourceLedgerPolicy`
  (`RequireExclusiveSourceLedger` | `AllowUnselectedSourceRows`).
- `Pgmq.Migration.SchemaContract` — `pgmqV1_11StateValidator` and
  `pgmqV1_11StateEvidenceKey`, a non-mutating PGMQ 1.11 schema check that guards the
  equivalent-history route.

## Limits

- **Two import shapes only.** A `pgmq_v1.11.0` full-install ledger (`DirectFullInstallHistory`,
  verified by reproducing the exact base64 MD5) and a `v1.10.0 → v1.10.1 → v1.11.0` upgrade
  ledger (`EquivalentTwoStepUpgradeHistory`). Any other predecessor shape is unsupported.
- **Equivalent history is never selected implicitly** and is additionally guarded by the
  schema contract; you must opt into it explicitly.
- **The schema contract checks structure, not behaviour.** `pgmqV1_11StateValidator`
  verifies object existence, type, and not-null constraints against pgmq 1.11.0; it does
  not inspect function bodies, so a database whose pgmq function *definitions* were altered
  in place would still pass.
- Pre-1.0 and uniformly `experimental`.
