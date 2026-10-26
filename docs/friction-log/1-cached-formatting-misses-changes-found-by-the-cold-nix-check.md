---
type: Friction Entry
title: "Cached formatting misses changes found by the cold Nix check"
description: "Cached nix fmt reported no changes while the cold flake formatting check rejected two test files."
generated:
  by: "codex-cli/gpt-6.1-sol"
  at: "2026-10-26T04:23:32Z"
frictionId: FRIC-1
status: "accepted"
impact: detour
kind: tooling
origin: "mori://shinzui/pgmq-hs"
affects: "https://github.com/numtide/treefmt"
affectedVersion: "unknown"
plan: "docs/plans/25-create-the-ephemeral-root-with-owner-only-permissions-and-record-acceptance-on-the-current-postgresql.md"
workaround: "Run nix fmt -- --no-cache, regenerate candidate archives, then rerun the cold check and affected validation."
resolution: "Candidate preparation uses nix fmt -- --no-cache before source archives; the cold flake check remains the acceptance gate."
amended:
  - by: "codex-cli/gpt-6.1-sol"
    at: "2026-10-26T04:24:07Z"
    change: "Retain the cold-format workaround; no unverified claim about the cache cause is promoted."
---
# Cached formatting misses changes found by the cold Nix check

## Attempted

Ran nix fmt before creating candidate archives, then nix flake check.

## Encountered

nix fmt reported eight formatted files and zero changes; treefmt-check failed on test formatting. A no-cache pass processed 142 files and changed two.

```text
nix fmt: formatted 8 files (0 changed); nix fmt -- --no-cache: formatted 142 files (2 changed)
```

## Workaround

Run nix fmt -- --no-cache, regenerate candidate archives, then rerun the cold check and affected validation.
