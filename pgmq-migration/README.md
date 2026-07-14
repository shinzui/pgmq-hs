# pgmq-migration

`pgmq-migration` exposes the PGMQ schema as a native `pg-migrate` component. Applications
compose `pgmqMigrations` into their migration plan; the package intentionally ships no
standalone runner.

Fresh databases run the two native migrations normally. Databases installed by
`pgmq-migration` 0.3 or earlier must import their `public.schema_migrations` predecessor
history before the first native `up`, so the PGMQ baseline is recorded rather than replayed.

The existing `pgmqHasqlMigrationSourceConfig` helper requires an exclusive predecessor
ledger. When an application deliberately shares that table with its own migrations, use the
explicit policy-aware constructor:

```haskell
pgmqHasqlMigrationSourceConfigWithPolicy
  provider
  DirectFullInstallHistory
  AllowUnselectedSourceRows
```

Call `readHasqlMigrationHistory` with that configuration first and review `unselectedRows`.
The policy accepts but never claims or modifies those rows. It does not relax selected PGMQ
evidence: the baseline must still reproduce its exact stored base64 MD5, and the equivalent
two-step history still requires its state validator plus explicit equivalent-history opt-in.

The repository guide at `docs/user/schema-migration.md` contains complete fresh-install,
history-import, validation, and recovery examples.
