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
---

# Create the ephemeral root with owner-only permissions and record acceptance on the current PostgreSQL

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.
If durable project context changes, update or create ADRs in docs/adr/ in the same change.


## Purpose / Big Picture

Two records this repository keeps about itself are wrong, and the release that carries the
sibling plans' fixes needs preparing.

The 0.6.1.0 changelog says every test suite pins its disposable PostgreSQL clusters under
`/tmp/ephpg-pgmq-hs-<uid>` and explains the uid key by saying "it is a fixed path created
`0700`". The four places that create that root call `createDirectoryIfMissing True root`,
which creates it with the process umask (typically `0755`); only the `ephemeral-pg` library's
registry *inside* the root is created `0700`. Nothing breaks, because the uid key already
prevents two users colliding, but the sentence describes code that does not exist. After this
plan the suites create the root owner-only, and `stat` on the directory shows `700`.

The compatibility ADR and the 0.6.0.0 release evidence record SQL acceptance on
PostgreSQL 17.10 with pg_partman 5.4.3. Since the nix-haskell-flake 0.24.0 migration
(commit `928746e`), the `partman` development shell provides PostgreSQL 18.6, which is what a
developer actually runs the suites against today. After this plan the family's acceptance has
been re-run on the server the shell provides, the version is written into a 0.7.0.0 release
evidence document and the ADR, and the ADR says which principle decides the server version.

Finally, the sibling plans append migration `0007` and add two constructors to
`pgmq-config`'s `ReconcileAction`, which is a breaking change for exhaustive matchers. This
plan prepares the 0.7.0.0 candidate: lockstep version bumps across the five published
packages, internal bounds, and changelogs that turn the siblings' "Unreleased" sections into
the release entry. Publication to Hackage and tagging are outside this plan.

You can see it working by deleting the root, running one suite, and reading the directory
mode; by reading the version line the migration suite prints; and by building the candidate
with `cabal sdist all` and finding seven SQL payloads in the migration tarball.


## Progress

- [ ] M1: `ensureOwnerOnlyRoot` (or equivalent) replaces `createDirectoryIfMissing` in the four test-support files
- [ ] M1: after `rm -rf /tmp/ephpg-pgmq-hs-$(id -u)` and one suite run, the directory mode reads `700`
- [ ] M1: `just nix-test` still passes (a different uid in the sandbox)
- [ ] M2: full acceptance run in the `partman` shell recorded, including the stock 1.12 selection and the flag-off `pgmq-config` build
- [ ] M2: `docs/releases/0.7.0.0-candidate.md` written with commands, counts, server versions, migration `0007` digest, and the consumer bound inventory
- [ ] M2: `docs/adr/pgmq-1.12-1.13-compatibility.md` verification paragraph names both server versions; `docs/adr/haskell-dependency-bounds-and-nix-pin-policy.md` states the shell's PostgreSQL follows the pinned nixpkgs
- [ ] M3: five package versions and internal bounds at 0.7.0.0; changelogs converted from "Unreleased" to the release entry; root changelog aggregated with the exhaustive-matcher note
- [ ] M3: `cabal sdist all` produces five 0.7.0.0 tarballs; extracted migration tarball lists seven SQL files
- [ ] `nix fmt`, `git diff --check`, `just docs-check` clean


## Surprises & Discoveries

Document unexpected behaviors, bugs, optimizations, or insights discovered during
implementation. Provide concise evidence.

(None yet.)


## Decision Log

- Decision: Make the code match the changelog (create the root `0700`) rather than soften the
  changelog. Rationale: the intent stated in 0.6.1.0 is right (a fixed, per-user path should
  not be world-readable), the change is four lines in test support, and `unix` is already a
  test dependency of every suite.
  Date: 2026-09-16
- Decision: Do not `chmod` an existing root; only create a missing one owner-only.
  Rationale: the path is keyed by uid, so an existing directory is this user's; changing the
  mode of something a previous run created is harmless but unnecessary, and never touching
  permissions of an existing path keeps the helper obviously safe.
  Date: 2026-09-16
- Decision: Record the server version the shell provides instead of pinning the shell back to
  17.10, and keep the historical 17.10 sentence in the ADR beside the new one.
  Rationale: the dependency-bounds ADR's principle is that the pin states what we test; the
  PostgreSQL the shell provides is nixpkgs' default `pkgs.postgresql` under the flake's locked
  nixpkgs, so the evidence must name the version at run time rather than assume one.
  Date: 2026-09-16
- Decision: Consumer validation for 0.7.0.0 is an inventory of retained bounds, not a rollout
  into each consumer. Rationale: the 0.6.0.0 candidate's rollouts existed because
  `QueueMetrics` and `PartitionConfig` changed shape for every consumer; 0.7.0.0 changes only
  exhaustive matchers over `ReconcileAction` and adds a migration, so the evidence a consumer
  needs is the changelog's migration note. Say so explicitly in the evidence document.
  Date: 2026-09-16


## Outcomes & Retrospective

Summarize outcomes, gaps, and lessons learned at major milestones or at completion.
Compare the result against the original purpose. Before marking the plan complete,
distill durable project context from the Decision Log, Surprises & Discoveries, and
this section into docs/adr/. Keep task-local execution details here.

(To be filled during and after implementation.)


## Context and Orientation

### The repository in one paragraph

`pgmq-hs` is a multi-package Cabal project: `pgmq-core`, `pgmq-hasql`, `pgmq-effectful`,
`pgmq-config`, `pgmq-migration` (the five published libraries, released in lockstep at one
version) and `pgmq-bench` (an internal benchmark that keeps its own `0.1.0.0`). The toolchain
comes from Nix: `nix develop` gives GHC 9.12 and cabal; `nix develop .#partman` is the same
shell with the `pg_partman` extension available to test servers and `PGMQ_REQUIRE_PARTMAN=1`
exported so partition tests fail instead of skipping. The shell's PostgreSQL is
`pkgs.postgresql` from the flake's locked nixpkgs (`flake.module.nix` builds
`partitionPostgres = pkgs.postgresql.withPackages (ps: [ ps.pg_partman ])`); it is currently
18.6. Tests start disposable servers through the `ephemeral-pg` library. `just` recipes wrap
the common commands (`just test`, `just nix-test`, `just docs-check`, `just effectful-floor`).
Run `nix fmt` before committing and `git diff --check` for whitespace.

### The ephemeral root

Four files create the shared root: `pgmq-hasql/test/EphemeralDb.hs`,
`pgmq-effectful/test/EphemeralDb.hs`, `pgmq-config/test/EphemeralDb.hs` (three near-identical
copies), and `pgmq-migration/test/Main.hs`. Each has:

```haskell
ephemeralRoot :: IO FilePath
ephemeralRoot = do
  uid <- getEffectiveUserID
  pure ("/tmp/ephpg-pgmq-hs-" <> show uid)

ephemeralConfig :: IO Config
ephemeralConfig = do
  root <- ephemeralRoot
  createDirectoryIfMissing True root
  pure defaultConfig {temporaryRoot = Last (Just root)}
```

with the comment "It is a fixed location created `0700`". `createDirectoryIfMissing` is from
`System.Directory` and applies the umask. `getEffectiveUserID` is from `System.Posix.User` in
the `unix` package, which every suite already lists in `build-depends`. `ephemeral-pg`
(`mori://shinzui/ephemeral-pg`, source at `/Users/shinzui/Keikaku/bokuno/ephemeral-pg-project/ephemeral-pg`)
creates its registry under the root with `System.Posix.Directory.createDirectory registry 0o700`
in `src/EphemeralPg/Internal/Instance.hs` and sweeps abandoned clusters under
`temporaryRoot` at start; that is why the root must be a stable path across shells. The
0.6.1.0 entry in the root `CHANGELOG.md` (the paragraph beginning "The root is keyed by
effective uid because it is a fixed path created `0700`") is the sentence this plan makes
true; the package changelogs repeat the uid rationale without the mode claim.

### The recorded evidence

`docs/adr/pgmq-1.12-1.13-compatibility.md` says, in its "Consequences and verification"
section: "SQL acceptance was verified with PostgreSQL 17.10 and pg_partman 5.4.3. The
project-specific `partman` Nix shell supplies the extension and sets `PGMQ_REQUIRE_PARTMAN=1`."
`docs/releases/0.6.0.0-candidate.md` is the evidence document for the last breaking release:
it records the candidate commit, upstream identities, a consumer inventory built from
`mori registry dependents shinzui/pgmq-hs --packages`, package integrity from `cabal sdist`,
and the validation commands with their counts, and says "the migration test reports
PostgreSQL 17.10 / pg_partman 5.4.3". The migration suite reads that string in
`testPartitions` (`pgmq-migration/test/Main.hs`) with
`SELECT current_setting('server_version') || ' / pg_partman ' || extversion FROM pg_extension WHERE extname='pg_partman'`
and prints it. `docs/adr/haskell-dependency-bounds-and-nix-pin-policy.md` holds the principle
that Cabal bounds state compatibility while the Nix pin states what we test.

### Releases

Every published package shares one version (`0.6.1.0` today) in its `.cabal` file, and the
test suites and internal `build-depends` carry bounds such as `pgmq-migration >=0.6 && <0.7`.
Each package has a `CHANGELOG.md`; the root `CHANGELOG.md` aggregates the family entry. The
sibling plans leave an "## Unreleased" section at the top of the root changelog and of the
`pgmq-migration`, `pgmq-core`, `pgmq-hasql`, and `pgmq-config` changelogs. Releases are
tagged `v<version>`; `automation/release.dhall` reacts to the tag by running
`scripts/record-release.sh`, which records the release fact in the Mori registry. Tagging and
Hackage publication are deliberately outside this plan.

### ADR context

[docs/adr/pgmq-1.12-1.13-compatibility.md](../adr/pgmq-1.12-1.13-compatibility.md) (server
version evidence, run the migration suite serially with `-j1`, give metrics cases their own
database) and
[docs/adr/haskell-dependency-bounds-and-nix-pin-policy.md](../adr/haskell-dependency-bounds-and-nix-pin-policy.md)
(the pin states what we test) are the two records this plan edits. The FIFO override ADR is
not affected. The parent MasterPlan is
[docs/masterplans/6-close-the-notification-reconciler-and-evidence-gaps-surfaced-by-the-0-6-1-0-review.md](../masterplans/6-close-the-notification-reconciler-and-evidence-gaps-surfaced-by-the-0-6-1-0-review.md);
this plan starts only after its EP-1 (`docs/plans/23-...`) and EP-2 (`docs/plans/24-...`) are
Complete, because the evidence run must include migration `0007` and the new
`ReconcileAction` constructors.


## Plan of Work

### Milestone 1: an owner-only root

Scope: the four files create the root with mode `0700` when it is missing and leave it alone
when it exists. At the end, a fresh run produces a `700` directory.

In each of the four files, replace the `createDirectoryIfMissing True root` line with a call
to a small helper defined in the same module (the three `EphemeralDb.hs` copies are kept in
sync by hand; copy the helper verbatim into each and into `pgmq-migration/test/Main.hs`):

```haskell
import Control.Exception (throwIO, try)
import System.IO.Error (IOError, isAlreadyExistsError)
import System.Posix.Directory qualified as Posix

-- | Create the shared root owner-only. An existing directory is this user's
-- (the path is keyed by uid) and is left exactly as it is.
ensureOwnerOnlyRoot :: FilePath -> IO ()
ensureOwnerOnlyRoot root = do
  result <- try (Posix.createDirectory root 0o700)
  case result of
    Right () -> pure ()
    Left err
      | isAlreadyExistsError (err :: IOError) -> pure ()
      | otherwise -> throwIO err
```

`System.Posix.Directory.createDirectory` calls `mkdir(2)` with the given mode (the umask still
applies, and `0700 & ~umask` is `0700` for any sane umask) and raises an `IOError` whose kind
is "already exists" on `EEXIST`. Remove the now-unused `createDirectoryIfMissing` import where
nothing else uses it (`pgmq-hasql/test/EphemeralDb.hs` imports `doesFileExist` from the same
module; keep that). Update the comment above `ephemeralRoot` to say the directory is created
`0700` by this helper.

Verify on macOS with `stat -f '%Lp'` (Linux: `stat -c '%a'`) after removing the root and
running one suite. Then run `just nix-test`, which builds the test checks inside the Nix
sandbox under a different uid; it must still pass, which proves the uid key and the new mode
coexist.

### Milestone 2: acceptance on the current server

Scope: run the family's suites in the `partman` shell after EP-1 and EP-2 are complete,
capture the server version, and record everything.

Run, from the repository root, in the order below (the migration suite shares one connection
and resets one database, so it runs serially; the client suites tolerate `-j2`):

```bash
nix develop .#partman --command cabal build all
nix develop .#partman --command cabal test pgmq-migration:pgmq-migration-test --test-show-details=direct -j1
nix develop .#partman --command cabal test pgmq-core pgmq-hasql pgmq-effectful pgmq-config --test-show-details=direct -j2
PGMQ_TEST_SCHEMA_VERSION=1.12.0 nix develop .#partman --command cabal test pgmq-hasql pgmq-effectful pgmq-config --test-show-details=direct -j2
nix develop .#partman --command cabal build pgmq-config -f-effectful
nix develop .#partman --command cabal test pgmq-config -f-effectful --test-show-details=direct
just effectful-floor
just nix-test
```

Capture the line the migration suite prints of the form `PostgreSQL 18.6 / pg_partman <x.y.z>`.
If the harness does not show it, read the versions the shell provides directly:

```bash
nix develop .#partman --command postgres --version
nix develop .#partman --command sh -c 'ls "$(pg_config --sharedir)/extension" | grep "^pg_partman--" | sort -V | tail -1'
```

Write `docs/releases/0.7.0.0-candidate.md` in the shape of `docs/releases/0.6.0.0-candidate.md`:
a heading naming this plan; the candidate commit; the upstream identity (still pgmq
`v1.13.0`, `32c075bb6dbed66a303d1a792393c93e36c09a97`); a "Consumer inventory" section built
by re-running `mori registry dependents shinzui/pgmq-hs --packages` and reading each consumer's
retained bounds, with an explicit sentence that no rollout was performed and why (Decision
Log); a "Package integrity" section from `cabal sdist all` (five `0.7.0.0` tarballs, the
migration tarball containing the manifest and seven SQL payloads, the MD5 of
`0007-notify-only-registered-queues.sql`); and a "Validation commands and results" section with
the commands above, their pass counts, and the server line.

Edit `docs/adr/pgmq-1.12-1.13-compatibility.md`: after the sentence recording 17.10 / 5.4.3,
add "Re-verified on <date> with PostgreSQL 18.6 and pg_partman <version>, the server the
`partman` shell has provided since the nix-haskell-flake 0.24.0 migration; see
[the 0.7.0.0 release evidence](../releases/0.7.0.0-candidate.md)." Edit
`docs/adr/haskell-dependency-bounds-and-nix-pin-policy.md`: add one paragraph under its
Decision stating that the same principle covers PostgreSQL: the `partman` shell's server is
nixpkgs' default `pkgs.postgresql` under the flake's locked nixpkgs, so release evidence names
the version observed at run time rather than a version assumed from an earlier record.

### Milestone 3: the 0.7.0.0 candidate

Scope: versions, bounds, and changelogs are ready for a release commit; nothing is tagged.

Set `version: 0.7.0.0` in `pgmq-core/pgmq-core.cabal`, `pgmq-hasql/pgmq-hasql.cabal`,
`pgmq-effectful/pgmq-effectful.cabal`, `pgmq-config/pgmq-config.cabal`, and
`pgmq-migration/pgmq-migration.cabal`; leave `pgmq-bench` at `0.1.0.0`. Find every internal
bound with `grep -rn 'pgmq-[a-z]* *[>^]' --include='*.cabal' .` and move `>=0.6 && <0.7` or
`^>=0.6` to the 0.7 equivalents (the `pgmq-bench` package's bounds on the libraries included).
Confirm with `nix develop --command cabal build all` that the solver still finds a plan.

In each package changelog, rename the sibling plans' "## Unreleased" heading to
`## 0.7.0.0 -- <date>` and keep their text; add a "Coordinated family version bump" paragraph
to `pgmq-effectful/CHANGELOG.md` and to any package with no other change (the pattern used in
0.6.1.0). In the root `CHANGELOG.md`, write the `## 0.7.0.0 -- <date>` entry: a summary
paragraph (migration `0007` restores upstream's silence on partitioned queues; the reconciler
reports name collisions and partitioned notifications; the drift wording; the ephemeral root
mode; the re-recorded server evidence), a "Breaking Changes" paragraph for `pgmq-config`
with the exhaustive-matcher note and a three-line example of handling the two constructors,
a "Bug Fixes" list carried from the package entries, and a "Documentation" list. Never edit a
published section.

Run `cabal sdist all --output-directory=dist-sdist-0.7.0.0` and list the migration tarball's
`migrations/` entries to confirm seven SQL files and the manifest are present.


## Concrete Steps

All commands run from the repository root
`/Users/shinzui/Keikaku/bokuno/libraries/pgmq-hs-project/pgmq-hs`.

Milestone 1:

```bash
rm -rf "/tmp/ephpg-pgmq-hs-$(id -u)"
nix develop --command cabal test pgmq-core pgmq-hasql --test-show-details=direct
stat -f '%Lp %N' "/tmp/ephpg-pgmq-hs-$(id -u)"
just nix-test
```

Expected: the suites pass, and the `stat` line reads `700 /tmp/ephpg-pgmq-hs-<uid>` (on
Linux, `stat -c '%a %n'` reads `700 ...`). Before the change the same line reads `755` (or the
umask's value).

Milestone 2: the command list in Plan of Work. Expected: every suite green with the counts
recorded in the evidence document; the migration suite prints `PostgreSQL 18.6 / pg_partman <version>`;
`just effectful-floor` builds against `effectful-core <2.7`; `just nix-test` passes.

Milestone 3:

```bash
grep -rn 'version: *0\.' --include='*.cabal' . | grep -v pgmq-bench
grep -rn 'pgmq-[a-z]* *[>^]' --include='*.cabal' .
nix develop --command cabal build all
nix develop --command cabal sdist all --output-directory=dist-sdist-0.7.0.0
tar tzf dist-sdist-0.7.0.0/pgmq-migration-0.7.0.0.tar.gz | grep 'migrations/'
nix fmt
git diff --check
just docs-check
```

Expected: five `0.7.0.0` versions; no bound still naming `0.6`; the tarball listing shows
`migrations/manifest` and `0001` through `0007`; formatting and whitespace clean; docs valid.

Commit after each milestone with the trailers

```text
MasterPlan: docs/masterplans/6-close-the-notification-reconciler-and-evidence-gaps-surfaced-by-the-0-6-1-0-review.md
ExecPlan: docs/plans/25-create-the-ephemeral-root-with-owner-only-permissions-and-record-acceptance-on-the-current-postgresql.md
Intention: intention_01m2nz9a82ejh91yg9sk7t6a7a
```

using Conventional Commits (`test: ...` for the root mode, `docs(release): ...` for evidence,
`chore(release): prepare 0.7.0.0` for the bump).


## Validation and Acceptance

The root: after deleting `/tmp/ephpg-pgmq-hs-<uid>` and running any one suite, the directory
exists with mode `700` and the suite passed; running a second suite reuses it without error;
`just nix-test` passes under the sandbox uid.

The evidence: `docs/releases/0.7.0.0-candidate.md` exists, names the candidate commit, the
server and extension versions actually printed, pass counts for every command, the migration
`0007` digest, and a consumer inventory with the explicit no-rollout statement; the
compatibility ADR names both 17.10 and 18.6 with dates; the dependency-bounds ADR states the
PostgreSQL principle.

The candidate: five `.cabal` files at `0.7.0.0`, internal bounds at 0.7, `cabal build all`
resolves, `cabal sdist all` yields five library tarballs whose migration archive holds seven
SQL payloads, and every changelog's top entry is `0.7.0.0` with the published `0.6.1.0` and
older sections byte-identical to before (check with `git diff --stat -- '*/CHANGELOG.md' CHANGELOG.md`
and read the diff: only additions above the `0.6.1.0` heading).


## Idempotence and Recovery

Every step repeats safely. Deleting the ephemeral root discards only disposable clusters;
`ephemeral-pg` recreates what it needs. If a suite fails because a stale postmaster holds a
port, delete the root and re-run; the sweep at start reaps registered clusters, and a fresh
root has none. If `just nix-test` fails on permissions, the helper threw on an error other
than "already exists" inside the sandbox; the sandbox uid differs from yours, so its root is a
different path and must be creatable under `/tmp`, which Nix provides. If a version bump leaves
the solver without a plan, the missing edit is an internal bound; the `grep` in Milestone 3
finds it. The evidence document and ADR edits are plain text and can be corrected in place
before the release commit. Nothing here touches a production database or publishes anything.


## Interfaces and Dependencies

No library API changes. Test support gains `ensureOwnerOnlyRoot :: FilePath -> IO ()` in four
test modules, using `System.Posix.Directory.createDirectory` from `unix` (already a test
dependency of all four suites) and `System.IO.Error.isAlreadyExistsError` from `base`.
Package versions become `0.7.0.0` for `pgmq-core`, `pgmq-hasql`, `pgmq-effectful`,
`pgmq-config`, and `pgmq-migration`; internal bounds move to `>=0.7 && <0.8` (or `^>=0.7`)
wherever they are written today. New documents: `docs/releases/0.7.0.0-candidate.md`. Edited
ADRs: `docs/adr/pgmq-1.12-1.13-compatibility.md`, `docs/adr/haskell-dependency-bounds-and-nix-pin-policy.md`.
