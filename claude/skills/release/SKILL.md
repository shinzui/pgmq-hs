---
name: release
description: Release all packages to Hackage following PVP
argument-hint: "[major|minor|patch]"
disable-model-invocation: true
allowed-tools: Read, Bash, Edit, Glob, Grep, Write, AskUserQuestion
---

# Multi-Package Release Skill

Release all packages from this multi-package repository to Hackage using a single shared version.

## Versioning Strategy

All packages share the **same version number** and are released together. A single git tag `v<version>` marks each release.

## Packages (in dependency order)

The packages MUST be published in this order due to inter-package dependencies:

1. **pgmq-core** — core types (no internal deps)
2. **pgmq-hasql** — hasql implementation (depends on pgmq-core)
3. **pgmq-migration** — schema migrations (no internal deps, but released after core by convention)
4. **pgmq-effectful** — effectful effects (depends on pgmq-core and pgmq-hasql)

**pgmq-bench** is NOT released to Hackage (it is a benchmark suite).

## Arguments

`$ARGUMENTS` is optional:
- `major`, `minor`, or `patch` — specifies the bump level
- If omitted, determine the bump level from the changes (see step 2).

## Steps

### 1. Determine what changed since the last release

- Read the current version from any package's `.cabal` file (they all share the same version).
- Find the latest git tag matching `v*` to identify the last release point.
- Run `git log --oneline <last-tag>..HEAD` to list commits since the last release.
- If there are no commits since the last tag, inform the user there is nothing to release and stop.

Present a summary showing:
- Current version
- Last release tag (or "none")
- Number of commits since last release
- Which package directories have changes

### 2. Determine the next version using PVP

The Haskell PVP version format is `A.B.C.D`:
- `A.B` is the **major** version — bump for breaking API changes (removed/renamed exports, changed types, changed semantics)
- `C` is the **minor** version — bump for backwards-compatible API additions (new exports, new modules, new type class instances)
- `D` is the **patch** version — bump for bug fixes, documentation, internal-only changes, performance improvements

Rules:
- If `$ARGUMENTS` is `major`, `minor`, or `patch`, use that bump level.
- Otherwise, analyze the commits to determine the appropriate bump:
  - Look for keywords like "breaking", "remove", "rename", "change type" → major
  - Look for keywords like "add", "new", "feature", "export" → minor
  - Look for keywords like "fix", "suppress", "docs", "refactor", "internal" → patch
- Present the proposed bump to the user and ask for confirmation before proceeding.

Increment the version:
- **major**: increment `B`, reset `C` and `D` to 0 (e.g., `0.2.0.1` → `0.3.0.0`)
- **minor**: increment `C`, reset `D` to 0 (e.g., `0.2.0.1` → `0.2.1.0`)
- **patch**: increment `D` (e.g., `0.2.0.1` → `0.2.0.2`)

### 3. Update versions and changelogs

#### Version update
- Edit ALL four package cabal files to set the new version:
  - `pgmq-core/pgmq-core.cabal`
  - `pgmq-hasql/pgmq-hasql.cabal`
  - `pgmq-migration/pgmq-migration.cabal`
  - `pgmq-effectful/pgmq-effectful.cabal`

#### Dependency bounds update
- Update the `pgmq-core` dependency bound in `pgmq-hasql/pgmq-hasql.cabal` and `pgmq-effectful/pgmq-effectful.cabal`.
- Update the `pgmq-hasql` dependency bound in `pgmq-effectful/pgmq-effectful.cabal`.
- Use PVP-compatible bounds matching the existing style in the cabal files.

#### Changelog update
- For each package that has a `CHANGELOG.md`, add a new section for the new version above any previous entries. Use today's date in `YYYY-MM-DD` format.
- Move content from "Unreleased" section (if any) into the new version section.
- Summarize commits since last release, grouped by:
  - **Breaking Changes** (if major)
  - **New Features** (if minor or major)
  - **Bug Fixes** (if any)
  - **Other Changes** (docs, refactoring, etc.)
  - Only include categories that have entries.
- Also update the root `CHANGELOG.md`.

Show the user ALL changes (version bumps, dependency bounds, changelog entries) for review before committing.

### 4. Verify builds

- Run `nix fmt` to ensure code is properly formatted.
- Run `cabal build all` to verify cabal build succeeds.
- Run `nix build` to verify nix build succeeds.
  - Note: newly created files must be `git add`-ed before `nix build` will see them, since nix uses the git tree.
  - If the nix build fails, fix the issue before proceeding.

### 5. Commit, tag, and push

- Stage all modified `.cabal` and `CHANGELOG.md` files.
- Create a single commit with message: `Release <new-version>`
- Create a single annotated git tag: `git tag -a v<version> -m "Release <version>"`
- Push the commit and tag: `git push && git push --tags`

### 6. Publish to Hackage (in dependency order)

For EACH package, in dependency order (pgmq-core → pgmq-hasql → pgmq-migration → pgmq-effectful):

1. `cd <pkg-dir>`
2. Run `cabal check` to verify no packaging issues.
3. Run `cabal test <pkg>` to ensure tests pass (skip for packages without test suites).
4. Run `cabal sdist` and then `cabal upload --publish <tarball-path>` to publish the source distribution.
5. Run `cabal haddock --haddock-for-hackage --haddock-hyperlink-source --haddock-quickjump` and then `cabal upload --publish --documentation <docs-tarball-path>` to publish documentation.
6. Report the Hackage URL for each package.

The Hackage URLs follow the pattern: `https://hackage.haskell.org/package/<pkg>-<version>`

After all packages are published, present a summary:

| Package | Version | Hackage URL |
|---------|---------|-------------|
| pgmq-core | X.Y.Z.W | https://hackage.haskell.org/package/pgmq-core-X.Y.Z.W |
| pgmq-hasql | X.Y.Z.W | https://hackage.haskell.org/package/pgmq-hasql-X.Y.Z.W |
| pgmq-migration | X.Y.Z.W | https://hackage.haskell.org/package/pgmq-migration-X.Y.Z.W |
| pgmq-effectful | X.Y.Z.W | https://hackage.haskell.org/package/pgmq-effectful-X.Y.Z.W |

## Important

- Always ask the user to confirm the version bump and changelogs before committing.
- Always publish in dependency order: pgmq-core → pgmq-hasql → pgmq-migration → pgmq-effectful.
- Never skip `cabal check`, tests, or `nix build`.
- If any step fails (including `nix build`), stop and report the error rather than continuing.
- If a Hackage upload fails for one package, do NOT continue uploading subsequent packages that depend on it.
- Run `nix fmt` before committing to ensure proper formatting.
- The commit and tag should only be created AFTER user approval of all changes.
