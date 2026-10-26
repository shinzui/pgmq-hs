---
id: 25
slug: create-the-ephemeral-root-with-owner-only-permissions-and-record-acceptance-on-the-current-postgresql
title: "Create the ephemeral root with owner-only permissions and record acceptance on the current PostgreSQL"
kind: exec-plan
created_at: 2026-09-16T20:42:28Z
intention: "intention_01m2nz9a82ejh91yg9sk7t6a7a"
master_plan: "docs/masterplans/6-close-the-notification-reconciler-and-evidence-gaps-surfaced-by-the-0-6-1-0-review.md"
provenance:
  created_by:
    model: "claude-fable-5-1"
    harness: "claude-code"
    at: 2026-09-16T20:42:28Z
  revisions:
    - model: "gpt-6-astra"
      harness: "codex-cli"
      at: 2026-10-25T20:33:49Z
      mode: "update"
      note: "Included six-package inspection handoff, safe existing-root upgrades, final-candidate acceptance ordering, and runtime evidence."
    - model: "gpt-6-astra"
      harness: "codex-cli"
      at: 2026-10-26T03:51:08Z
      mode: "update"
      note: "Format before archiving and distinguish native-only tests inside stock-selected acceptance."
  reviews:
    - model: "gpt-6-astra"
      harness: "codex-cli"
      at: 2026-10-25T20:33:49Z
      verdict: "approved"
      note: "Reviewed revised planning document; implementation evidence remains pending. Included six-package inspection handoff, safe existing-root upgrades, final-candidate acceptance ordering, and runtime evidence."
---

# Create the ephemeral root with owner-only permissions and record acceptance on the current PostgreSQL

This ExecPlan is a living document. Keep Progress, Surprises & Discoveries, Decision Log,
and Outcomes & Retrospective current. Promote durable decisions into `docs/adr/`.


## Purpose / Big Picture

Make every shared ephemeral PostgreSQL root owner-only, including roots created by earlier
versions, and produce truthful acceptance evidence for the 0.7.0.0 release candidate.
The 0.6.1.0 changelog promised mode `0700`, but current helpers use
`createDirectoryIfMissing True root`, which follows the process umask. A fresh directory
usually becomes `0755`, and simply changing the creation mode leaves existing roots exposed.

The original draft also predates `pgmq-inspect`. The current root and inspection changelogs
explicitly hand its initial release to this plan. Prepare six libraries at 0.7.0.0, including
inspection, preserve all accumulated Unreleased changes, and validate that actual candidate
after changing versions and internal bounds. The five older libraries currently say 0.6.1.1;
`pgmq-inspect` is provisionally 0.1.0.0 and `pgmq-bench` remains independently 0.1.0.0.

Acceptance records the running server's PostgreSQL and pg_partman versions, rather than
copying either the historical 17.10 / 5.4.3 evidence or the draft's observed 18.6. The candidate
includes migration `0007`, both new reconciliation reports and their JSON encodings, and the
already implemented inspection and effect-layer additions. Publication, release tags, external
consumer edits, and deployment remain outside scope.


## Progress

- [ ] M1: all five root creators establish `0700`; fresh, existing, repeated, and unsafe-path cases are verified without deleting shared clusters.
- [ ] M2: all six library versions, internal bounds, changelogs, and source archives form the 0.7.0.0 candidate; published history is unchanged.
- [ ] M3: the prepared candidate passes native/stock, both-backend, flag-off, floor, Nix, and packaging acceptance; observed versions and consumer inventory are recorded.

EP-1 and EP-2 are whole-child hard prerequisites. Do not start M1 until both are Complete.


## Surprises & Discoveries

The 2026-10-25 review found a fifth root creator in `pgmq-inspect/test/EphemeralDb.hs`,
explicit six-package release handoffs in the changelogs, and new JSON/effect API changes
already waiting for this release. `Justfile`'s `nix-test` runs only hasql and migration tests;
it is not evidence for every suite. The migration suite prints
`Partition test versions: <server_version> / pg_partman <extversion>`. The pre-implementation
refresh found that legacy config fixtures remain native-only during stock-selected runs,
and moved formatting ahead of archive creation to keep archive identities current.


## Decision Log

- Decision (2026-10-25, supersedes the original create-only policy): validate the root's
  type and effective-uid ownership, then ensure `0700` for new and existing directories.
  A uid in a filename does not prove ownership. Do not silently accept an existing file,
  symlink, or foreign-owned directory. This makes the changelog's claim true on upgrade.
- Decision (2026-10-25): prepare the six-package candidate before final acceptance. The
  inspection handoff already requires this release to include that package and its bounds;
  testing old versions and then bumping cannot establish candidate acceptance.
- Decision (2026-09-16, retained): keep historical server evidence, add a dated run with
  observed versions, and let the existing locked development shell determine the tested
  server. Do not alter a dependency pin merely to match an older evidence paragraph.
- Decision (2026-10-25, clarifies the inventory-only scope): inventory external consumers'
  retained bounds, exhaustive `ReconcileAction` matches, exhaustive `Pgmq` interpreters, and
  orphan JSON instances. Record compatibility blockers explicitly; no consumer rollout or
  claim of consumer acceptance is implied. Local `pgmq-inspect` is tested as part of the family.


## Outcomes & Retrospective

Implementation and candidate acceptance remain pending. Record actual results and remaining
limitations here when implementing; do not convert planned test counts into evidence.


## Context and Orientation

The six release packages are `pgmq-core`, `pgmq-hasql`, `pgmq-effectful`, `pgmq-migration`,
`pgmq-config`, and `pgmq-inspect`. `cabal.project` also includes `pgmq-bench`, whose dependency
bounds move with the libraries even though its own version remains 0.1.0.0. The root
`CHANGELOG.md` already has an Unreleased section covering inspection reads, JSON instances,
transient-error fixes, the inspection HTTP/WebSocket/CLI surface, and five new `Pgmq`
constructors. Preserve it and each package's existing entries. The release must mention
exhaustive report matchers, exhaustive effect interpreters, and potential downstream orphan
JSON instances, not only migration `0007`.

Five modules independently build `/tmp/ephpg-pgmq-hs-<effective uid>`:
`pgmq-hasql/test/EphemeralDb.hs`, `pgmq-effectful/test/EphemeralDb.hs`,
`pgmq-config/test/EphemeralDb.hs`, `pgmq-inspect/test/EphemeralDb.hs`, and
`pgmq-migration/test/Main.hs`. They each compute `ephemeralRoot`, create it in
`ephemeralConfig`, and pass it as `temporaryRoot` to `ephemeral-pg`. Their `unix` test
dependency supplies POSIX directory and file-status operations; no new dependency is needed.
Use Mori to inspect those APIs before implementation. The dependency
`mori://shinzui/ephemeral-pg` owns `src/EphemeralPg/Internal/Instance.hs` (artifact-level URI
pending), whose private registry checks directory ownership and rejects unsafe paths. Its
registry and process records must survive normal suite runs so abandoned clusters remain
reapable. Never remove this shared root to test permissions or recover a live server.

`flake.module.nix` defines `partitionPostgres` from locked nixpkgs' `pkgs.postgresql` with
pg_partman and exports `PGMQ_REQUIRE_PARTMAN=1` in `nix develop .#partman`. The migration
suite queries the running server and installed extension version in `testPartitions`.
`nix develop` supplies the compiler and Cabal; invoke Cabal/Just build recipes inside that
shell rather than assuming they are installed on the host. Cabal `-j1` limits build jobs;
Tasty needs its own `--test-options='-j1'` for a suite sharing a connection.

Relevant ADRs are [PGMQ compatibility](../adr/pgmq-1.12-1.13-compatibility.md), which preserves
migration history and requires serial migration acceptance;
[dependency bounds and pins](../adr/haskell-dependency-bounds-and-nix-pin-policy.md), which
requires an explicit older-effectful build and explains why Nix does not run the effectful
suite; and [inspection ownership](../adr/queue-inspection-surface-boundary-and-wire-contract.md),
which establishes the sister package and wire contract. Preserve the historical 17.10 / 5.4.3
record in `docs/releases/0.6.0.0-candidate.md`; new evidence belongs in
`docs/releases/0.7.0.0-candidate.md`. There is no profiled ADR bundle.

This child belongs to
[MasterPlan 6](../masterplans/6-close-the-notification-reconciler-and-evidence-gaps-surfaced-by-the-0-6-1-0-review.md).
Its prerequisites are the complete
[notification plan](23-gate-the-notification-fail-open-on-a-real-queue-row-and-state-the-partitioned-queue-contract.md)
and [reconciler plan](24-report-name-collisions-and-unsupported-notifications-instead-of-acting-on-them.md).
They supply migration `0007`, reports, JSON fixtures, and Unreleased entries; this plan owns
root permissions, release metadata, and integrated acceptance.


## Plan of Work

### Milestone 1: owner-only roots on fresh and existing installations

Replace each root-creation call with the same local `ensureOwnerOnlyRoot :: FilePath -> IO ()`
helper. Keep the established per-suite copies; a new shared library is unnecessary.
Create missing roots with `System.Posix.Directory.createDirectory root 0o700`, handling only
already-exists as a possible normal result. On either path use a non-following status check
(`getSymbolicLinkStatus`), require an actual directory owned by the effective uid, then set
and verify mode `0700` before starting the database. Do not chmod an unvalidated path or
follow a symlink. Propagate other filesystem errors with useful context. Leave root contents
and process-registration records intact. An untrusted path fails before any database starts;
resolving foreign ownership is an operator action, not recursive deletion or ownership repair.

Add focused helper tests in each affected suite using a private disposable parent and an
explicit path argument, never the production shared root. Cover missing-directory creation,
an existing owned `0755` directory with a sentinel file that survives tightening, a second
invocation, a regular file, and a symlink whose target is unchanged. A foreign-owned-path test
may run only when the environment can construct one without privileges; record its availability.
Use the same helper body in all five files and register local test modules in their Cabal
files if introduced. Add `unix` only if a site's current test stanza lacks it.

Observe the real root after a normal suite run, without deleting it. A second suite should
reuse it and leave its registry intact. `just nix-test` checks the hasql and migration paths
under the sandbox uid; Cabal acceptance in M3 covers all five creators.

### Milestone 2: prepare the candidate and its archives

Set all six library `.cabal` versions to 0.7.0.0, including `pgmq-inspect`; leave
`pgmq-bench` at 0.1.0.0. Update every internal family dependency range to the matching 0.7
range, in libraries, tests, executables, and the benchmark. Inspect all `.cabal` files, not
only the original five. Do not narrow or bump third-party dependencies as part of this task.

Merge each existing Unreleased section into one dated 0.7.0.0 entry. Preserve unrelated
Unreleased work and all published sections (including 0.6.1.1) byte-for-byte. Add the root
permission fix to every affected package entry. Root release notes aggregate the entire
candidate: migration `0007`, both report constructors and their JSON shapes, the five
already-added `Pgmq` constructors (`PeekMessages`, `PeekArchivedMessages`, `LookupMessage`,
`LookupArchivedMessage`, `QueueMetricsUnvalidated`), JSON instance/orphan guidance, and the
initial inspection package, in addition to the other accumulated changes. Replace pending
inspection release-handoff wording with the concrete inclusion in this candidate.

Finalize source, versions, bounds, changelogs, and `nix fmt` before creating archives.
Build all packages, run `cabal check` from each of the six library directories, and produce
source archives for those six explicit targets. Do not use `sdist all` while asserting that
it emits only six archives: `pgmq-bench` is also a project package. Inspect migration payloads
and new report goldens inside the archives, and record archive hashes. If any packaged file
changes afterward, including a formatter change, regenerate and revalidate those archives
before recording their hashes. Extract these six
archives into a fresh temporary validation directory and compile/test using an explicit
Cabal project whose packages are only the extracted candidates. Carry the repository's
compiler, dependency-source pins, constraints, and dependency test exclusions into that
project, replacing its local package list; no original workspace package may satisfy a
candidate dependency. This catches missing SQL, fixtures, and inspection assets.

Finalize code, versions, bounds, formatting, and changelog content before M3. A meaningful
candidate commit is permitted during implementation; record its SHA in the evidence. A later
evidence-only commit can refer to that SHA. If executable/package content changes after it,
repeat affected checks and identify the new tested candidate rather than retaining stale evidence.

### Milestone 3: accept that candidate on the actual server

Run the validation matrix below after M2. The native run covers all six packages and both
config backends; stock 1.12 covers all schema-selectable client suites, including inspection.
The separate `-f-effectful` config run must remain independent of the enabled run. Confirm
partition cases actually ran under `PGMQ_REQUIRE_PARTMAN=1`; record skipped tests as skips.
Use Tasty serial execution for the migration suite and two workers for client suites.
Set `PGMQ_TEST_SCHEMA_VERSION=1.13.0` explicitly for native client runs so an inherited
shell setting cannot silently select stock SQL. The migration suite always tests its native
ledger and catalog fixtures. `pgmq-config`'s existing `NotifyCrashSpec` and `ForeignQueueSpec`
likewise always install native SQL even in a stock-selected suite run; list those separately
from stock acceptance. New collision and partition tests must honor the schema selector.

Capture `Partition test versions: <server_version> / pg_partman <extversion>` from the
migration test output. If absent, query `current_setting('server_version')` and
`pg_extension.extversion` on the actual disposable server before teardown. A `postgres
--version` binary or extension filename is not proof of the version installed in the server.
Do not traverse `/nix/store` to infer versions.

Write `docs/releases/0.7.0.0-candidate.md`: tested candidate SHA, flake lock identity, unchanged
vendored pgmq v1.13.0 identity, six package versions and archive hashes, migration `0007`
MD5, archive inspection/build results, exact commands, pass/skip counts, runtime versions,
and retained consumer state. Run `mori registry dependents shinzui/pgmq-hs --packages`,
inspect the consumers it identifies, and cite their canonical Mori package/project URIs.
Registry data may lag; check actual bounds and relevant matches/instances. Explicitly list
consumers still excluding 0.7 and explain that no external rollout or consumer acceptance
was performed. The five effect constructors and JSON instances mean the impact is broader
than report matchers alone.

Append dated observed-version evidence to the compatibility ADR, retaining its historical
17.10 / 5.4.3 sentence. Add the PostgreSQL application of the pin policy to the dependency ADR:
the shell's locked server is the test target and evidence reports its observed version.
Do not change the old release document to imply it ran on the new server. Finish with docs
validation and review the final diff for candidate/evidence consistency.


## Concrete Steps

Run from the repository root. These are implementation acceptance commands, not evidence
that the review update has executed them.

For M1, after adding the focused tests, run the affected suites in the matrix below and
inspect the root without removing it:

```bash
stat -f '%Lp %N' "/tmp/ephpg-pgmq-hs-$(id -u)"  # macOS
# Linux equivalent:
# stat -c '%a %n' "/tmp/ephpg-pgmq-hs-$(id -u)"
```

For M2:

```bash
rg -n '^version:|pgmq-.*(>=|\^>=)' --glob '*.cabal'
nix fmt
git diff --check
nix develop --command cabal build all
nix develop --command cabal sdist pgmq-core pgmq-hasql pgmq-effectful pgmq-migration pgmq-config pgmq-inspect --output-directory=dist-sdist-0.7.0.0
tar tzf dist-sdist-0.7.0.0/pgmq-migration-0.7.0.0.tar.gz
git diff --check
```

Run `cabal check` with each library directory as the working directory. Generate and run the
extracted-archive project described in M2 before recording packaging acceptance. Its explicit
`--project-file` and working directory must be retained in the evidence, using a portable
placeholder for the temporary directory, not a committed local pin.

For M3:

```bash
nix develop .#partman --command cabal build all
nix develop .#partman --command cabal test pgmq-migration:pgmq-migration-test --test-show-details=direct -j1 --test-options='-j1'
PGMQ_TEST_SCHEMA_VERSION=1.13.0 nix develop .#partman --command cabal test pgmq-core pgmq-hasql pgmq-effectful pgmq-config pgmq-inspect -feffectful --test-show-details=direct --test-options='-j2'
PGMQ_TEST_SCHEMA_VERSION=1.12.0 nix develop .#partman --command cabal test pgmq-hasql pgmq-effectful pgmq-config pgmq-inspect -feffectful --test-show-details=direct --test-options='-j2'
nix develop .#partman --command cabal build pgmq-config -f-effectful
PGMQ_TEST_SCHEMA_VERSION=1.13.0 nix develop .#partman --command cabal test pgmq-config -f-effectful --test-show-details=direct --test-options='-j2'
nix develop --command just effectful-floor
nix develop --command just nix-test
nix flake check
just docs-check
git diff --check
```

Expected: all selected suites run and pass, partition tests do not skip, focused root tests
verify both creation and upgrade, and final evidence reports actual counts. Nix builds do
not substitute for the explicit effectful and inspection Cabal runs. Existing documented
Nix coverage limitations remain explicit. Do not mask failed checks with output-filter pipes.

Implementation commits use Conventional Commits and these trailers:

```text
MasterPlan: docs/masterplans/6-close-the-notification-reconciler-and-evidence-gaps-surfaced-by-the-0-6-1-0-review.md
ExecPlan: docs/plans/25-create-the-ephemeral-root-with-owner-only-permissions-and-record-acceptance-on-the-current-postgresql.md
Intention: intention_01m2nz9a82ejh91yg9sk7t6a7a
```


## Validation and Acceptance

All five helpers create and reuse a real owned `0700` directory, tighten an existing `0755`
directory without losing contents, and reject unsafe path types before database startup.
No shared-root deletion is part of acceptance.

Six library versions and their internal bounds are 0.7-compatible, the benchmark remains
0.1.0.0, and the candidate builds and passes the matrix after these changes. Six explicit
source archives contain the manifest, seven SQL payloads, all old/new JSON goldens, and
inspection resources; the extracted candidate builds/tests without workspace-package fallback.
All prior published changelog sections remain unchanged.

The release evidence includes the populated six-entry-to-seven-entry notification upgrade,
and distinguishes schema-selectable stock cases from dedicated native-only fixtures.
It ties those results to the tested candidate and archive identities,
records the running server/extension versions, and distinguishes external inventory from
consumer acceptance. Both ADR updates reference that evidence and retain historical facts.


## Idempotence and Recovery

Repeated helper calls preserve contents and establish the same mode. Use private disposable
test directories for helper failure scenarios. Never remove a shared root to free a port:
that can erase live clusters and the registry needed to reap them. Let the existing
`ephemeral-pg` lifecycle reclaim abandoned instances; investigate a specific remaining
process and use its lifecycle cleanup before any targeted directory removal.

Re-run failed checks after addressing their actual error. A bound failure may involve the
new inspection package as well as the original five; inspect all local stanzas. Rebuild
archives after any package-content change and refresh candidate identity and affected
evidence. No step changes a production database or publishes a package.


## Interfaces and Dependencies

Each of five test modules gains the same `ensureOwnerOnlyRoot :: FilePath -> IO ()` behavior,
using existing `unix`/`base` facilities. Library APIs are unchanged by this child. The six
family packages become 0.7.0.0; local family dependency bounds move to `>=0.7 && <0.8` (or
the corresponding caret range). Existing third-party bounds and pins stay unchanged.

This child owns `docs/releases/0.7.0.0-candidate.md` and evidence updates to the compatibility
and dependency-policy ADRs. EP-1 owns the SQL ledger and EP-2 owns report constructors and
JSON fields; this plan consumes their accepted artifacts without changing their semantics.

Revision note (2026-10-25): included the inspection handoff, existing-root correction and safe
verification, six-package archives, complete release notes, Tasty worker controls, and
candidate-before-evidence ordering. Removed unsafe shared-root deletion and installed-version
guesses. Implementation remains pending.

Pre-implementation refresh (2026-10-25): Moved formatting before archives and made native/stock
evidence selection explicit, including the existing-trigger upgrade result.
