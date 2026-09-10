# CLAUDE.md

pgmq-hs: Haskell client for [pgmq](https://github.com/tembo-io/pgmq), a multi-package Cabal project (`pgmq-core`, `pgmq-hasql`, `pgmq-effectful`, `pgmq-migration`).

- Use `nix develop` for the toolchain (GHC 9.12.2, cabal, PostgreSQL, HLS).
- Run `nix fmt` before committing; the pre-commit hook otherwise fails and reformats, requiring a re-stage and re-commit.
- Tests spin up temporary PostgreSQL via `ephemeral-pg`; no external database setup.

## pgmq-migration

SQL is vendored from mori://pgmq/pgmq via git subtree at `vendor/pgmq/`, pinned to
v1.13.0 (`32c075bb6dbed66a303d1a792393c93e36c09a97`). Preserve pristine upstream bytes.
Document approved local hardening in separate append-only migrations, never vendor edits.

When updating the pgmq schema version:

1. `git subtree pull --prefix vendor/pgmq https://github.com/tembo-io/pgmq.git <new-tag> --squash`
2. Verify the new migration file has no extension-specific patterns (`ALTER EXTENSION`, `@extschema@`)
3. Keep `0001-install-v1.11.0.sql` immutable; append a new native SQL migration and manifest entry
4. Compare the final schema with the tagged fresh SQL; keep the 1.11 predecessor validator unchanged
5. Refresh predecessor-history payloads only when adding an explicitly supported import route
6. Run `cabal test pgmq-migration:pgmq-migration-test`

Run partition acceptance with `nix develop .#partman --command cabal test pgmq-migration:pgmq-migration-test`.
