# Design: Vendor upstream PGMQ SQL with an immutable native ledger

## Status

Adopted. Updated 2026-09-10 for PGMQ 1.13.0 and the manifest-based migration component.
This supersedes the old module-per-version implementation proposal.

## Context

Hand-maintained schema snapshots diverged from upstream, including metrics that incorrectly
used transaction-scoped counters and partition functions that lost upstream behavior. The
repository now vendors pristine sources from mori://pgmq/pgmq at `vendor/pgmq/` using a Git
subtree. Upstream provenance and native migration history serve different purposes: vendor
sources advance with releases, while successfully applied migration bytes never change.

## Decision

`Pgmq.Migration.Internal.Definition` embeds `pgmq-migration/migrations/manifest` through
`embedMigrationManifest` and constructs the native `pgmq` component. The ordered manifest
is authoritative; unlisted SQL files, duplicate names and changed applied checksums are errors.
There are no per-version Haskell modules or alternate public fresh-install paths.

The ledger contains the immutable 1.11 baseline, the schema-management comment, notification
and partition hardening, the 1.12 upstream upgrade, the 1.13 upstream upgrade, and a local
four-argument partition re-entry override. Existing installations apply only pending entries.
Predecessor-history import still validates the original 1.11 state and bytes before recording
that baseline; it must not validate the new final state as its predecessor.

Vendor is pinned to v1.13.0 at `32c075bb6dbed66a303d1a792393c93e36c09a97`.
The test-only 1.12 fresh fixture is extracted from v1.12.0 at
`08ace4087dbf00e51704c5a3d9df2e15fd566127`. Its SHA-256 is
`be087bfcb0ec5e65abb76610249750f2ec8dc757956125a1b40430ce95fc7f0f`.
Both sources belong to mori://pgmq/pgmq; the project-relative extraction path is
`pgmq-extension/sql/pgmq.sql` and its artifact-level URI is pending.

## Upgrade workflow

Verify upstream tags before updating the subtree. Preserve all existing migration payloads.
Append new upstream scripts as new manifest entries; combine consecutive upstream scripts
only with an explicitly tested byte recipe. Migration 0004 is the 1.11→1.11.1 script, one
newline, then the 1.11.1→1.12 script. Migration 0005 is byte-identical to the 1.12→1.13 script.
Package the source files through Cabal's `extra-source-files`.

Inspect extension-dependent operations before using scripts natively. The guarded
`pg_extension_config_dump` in the 1.12 chain is inert when PGMQ is installed natively and is
retained. Do not assume every future upstream script is native-compatible.

Approved local hardening is appended separately and documented in an ADR; never change
vendor bytes to disguise a divergence. Migration 0006 preserves the new premake argument and
identity mode while restoring queue/archive parent-registration guards. Replaying 0003 would
reintroduce the wrong signature. The whole suffix must finish before queue-creation traffic.

## FIFO work must not override upstream functions

The FIFO initiative must not replace extension-owned SQL functions on either native or
extension installs. The user permits adding a separately owned index, not replacing function
bodies. See the [FIFO boundary ADR](../adr/fifo-native-overrides-and-index-upgrade-boundary.md).
Client query ordering and measured, explicitly managed supplemental indexes are the permitted
local approaches. They introduce no new FIFO migrations, convergence body exceptions or
override-maintenance machinery. Upstream server fixes belong in the ordinary pristine-source
upgrade workflow. Historical notification/partition migrations remain immutable; changing or
removing those existing overrides is separate work.

A supplemental index should have a distinct name and documented ownership, coexist with
upstream's index, and be remeasured when relevant upstream queries change. It may become
redundant or require attention after schema changes, but does not replace upstream function
logic or require replaying local SQL bodies.

## Verification

Pin historical payload digests, compare upstream migration bytes exactly, and compare catalog
snapshots at both the 1.12 and 1.13 checkpoints with their corresponding tagged fresh installs.
Compare function identities, arguments/defaults, results, bodies, ordered composite fields,
relations, columns and constraints. Only bodies of the three documented local overrides may
differ; their signatures remain compared and their behavior requires separate regression tests.

Run populated upgrades and recovery with actual pg_partman:

```bash
nix develop .#partman --command cabal test pgmq-migration:pgmq-migration-test --test-show-details=direct
```

The shell requires the extension; a skipped partition test is not release acceptance.
See [the compatibility ADR](../adr/pgmq-1.12-1.13-compatibility.md),
[notification contract](015-notification-delivery-contract.md), and
[operator migration guide](../user/schema-migration.md).
