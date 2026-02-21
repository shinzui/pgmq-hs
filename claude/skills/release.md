---
name: release
description: Release changed packages to Hackage following PVP
argument-hint: "[major|minor|patch] [--all] [--package=PKG]"
disable-model-invocation: true
allowed-tools: Read, Bash, Edit, Glob, Grep, Write, AskUserQuestion
---

# Multi-Package Release Skill

Release changed packages from this multi-package repository to Hackage.

## Packages (in dependency order)

The packages MUST be released in this order due to inter-package dependencies:

1. **pgmq-core** — core types (no internal deps)
2. **pgmq-hasql** — hasql implementation (depends on pgmq-core)
3. **pgmq-migration** — schema migrations (no internal deps, but released after core by convention)
4. **pgmq-effectful** — effectful effects (depends on pgmq-core and pgmq-hasql)

**pgmq-bench** is NOT released to Hackage (it is a benchmark suite).

## Arguments

`$ARGUMENTS` is optional and supports:
- `major`, `minor`, or `patch` — applies the same bump level to ALL changed packages
- `--all` — release all packages regardless of changes
- `--package=PKG` — release only the specified package (e.g., `--package=pgmq-core`)
- Combinations are allowed, e.g., `patch --package=pgmq-core`

If no bump level is given, determine it from the changes (see step 2).

## Steps

### 1. Identify which packages have changes

For EACH of the four packages (`pgmq-core`, `pgmq-hasql`, `pgmq-effectful`, `pgmq-migration`):

- Look for the latest git tag matching `<package-name>-v*` (e.g., `pgmq-core-v*`).
- If a tag exists, run `git log --oneline <tag>..HEAD -- <package-dir>/` to find commits touching that package since its last release.
- If NO tag exists for a package, treat it as a first release — ALL commits touching `<package-dir>/` are considered changes.
- A package is considered "changed" if it has commits in its directory since its last tag.
- Also check: if a dependency package is being released (e.g., pgmq-core), mark dependent packages (pgmq-hasql, pgmq-effectful) as needing at minimum a patch bump to update their dependency bounds, even if they have no direct changes. Ask the user whether they want to include these transitive releases.

If `--all` was passed, mark all packages for release regardless.
If `--package=PKG` was passed, only consider that single package.

If NO packages have changes and `--all` was not passed, inform the user there is nothing to release and stop.

Present a summary table to the user showing:
- Package name
- Current version (from `<pkg>/<pkg>.cabal`)
- Last release tag (or "none")
- Number of commits since last release
- Whether it will be released

### 2. Determine the next version for each changed package using PVP

The Haskell PVP version format is `A.B.C.D`:
- `A.B` is the **major** version — bump for breaking API changes (removed/renamed exports, changed types, changed semantics)
- `C` is the **minor** version — bump for backwards-compatible API additions (new exports, new modules, new type class instances)
- `D` is the **patch** version — bump for bug fixes, documentation, internal-only changes, performance improvements

For EACH package being released:

- If `$ARGUMENTS` specifies a bump level (`major`, `minor`, `patch`), use that for all packages.
- Otherwise, analyze that package's commits to determine the appropriate bump:
  - Look for keywords like "breaking", "remove", "rename", "change type" → major
  - Look for keywords like "add", "new", "feature", "export" → minor
  - Look for keywords like "fix", "suppress", "docs", "refactor", "internal" → patch
  - If the package is only being bumped due to a dependency update, default to patch.
- Present the proposed bumps for ALL packages in a single table and ask for confirmation.

Increment versions:
- **major**: increment `B`, reset `C` and `D` to 0 (e.g., `0.2.0.1` → `0.3.0.0`)
- **minor**: increment `C`, reset `D` to 0 (e.g., `0.2.0.1` → `0.2.1.0`)
- **patch**: increment `D` (e.g., `0.2.0.1` → `0.2.0.2`)

### 3. Update versions, dependency bounds, and changelogs

For EACH package being released (in dependency order):

#### Version update
- Edit `<pkg>/<pkg>.cabal` to set the new version.

#### Dependency bounds update
- If pgmq-core is being released, update the `pgmq-core` dependency bound in:
  - `pgmq-hasql/pgmq-hasql.cabal`
  - `pgmq-effectful/pgmq-effectful.cabal`
- If pgmq-hasql is being released, update the `pgmq-hasql` dependency bound in:
  - `pgmq-effectful/pgmq-effectful.cabal`
- Use PVP-compatible bounds: `>= X.Y.Z.W && < X.(Y+1)` for major version ranges, or match the existing bound style in the cabal files.

#### Changelog update
- If the package does not have a `CHANGELOG.md`, create one with the standard format.
- Add a new section for the new version above any previous entries. Use today's date in `YYYY-MM-DD` format.
- Move content from "Unreleased" section (if any) into the new version section.
- Summarize commits since last release, grouped by:
  - **Breaking Changes** (if major)
  - **New Features** (if minor or major)
  - **Bug Fixes** (if any)
  - **Other Changes** (docs, refactoring, etc.)
  - Only include categories that have entries.

#### Root CHANGELOG.md
- Also update the root `CHANGELOG.md` to reflect the releases. Replace "Unreleased" with the version numbers and dates for each released package.

Show the user ALL changes (version bumps, dependency bounds, changelog entries) for review before committing.

### 4. Verify builds

- Run `nix fmt` to ensure code is properly formatted.
- Run `cabal build all` to verify cabal build succeeds.
- Run `nix build` to verify nix build succeeds.
  - Note: newly created files (e.g., `CHANGELOG.md`) must be `git add`-ed before `nix build` will see them, since nix uses the git tree.
  - If the nix build fails, fix the issue before proceeding.

### 5. Commit, tag, and push

- Stage all modified `.cabal` and `CHANGELOG.md` files.
- Create a single commit with message:

  ```
  Release: <pkg1> <ver1>, <pkg2> <ver2>, ...

  Released packages:
  - <pkg1> <ver1>
  - <pkg2> <ver2>
  ...
  ```

- Create annotated git tags for EACH released package:
  - `git tag -a <pkg>-v<version> -m "Release <pkg> <version>"`
  - e.g., `git tag -a pgmq-core-v0.2.0.0 -m "Release pgmq-core 0.2.0.0"`

- Push the commit and all tags: `git push && git push --tags`

### 6. Publish to Hackage (in dependency order)

For EACH package being released, in dependency order:

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
| ... | ... | ... |

## Important

- Always ask the user to confirm the version bumps and changelogs before committing.
- Always release in dependency order: pgmq-core → pgmq-hasql → pgmq-migration → pgmq-effectful.
- Never skip `cabal check`, tests, or `nix build`.
- If any step fails (including `nix build`), stop and report the error rather than continuing.
- If a Hackage upload fails for one package, do NOT continue uploading subsequent packages that depend on it.
- Run `nix fmt` before committing to ensure proper formatting.
- The commit and tags should only be created AFTER user approval of all changes.
