# Shared ephemeral PostgreSQL roots validate ownership and preserve lifecycle state

## Status

Accepted, 2026-10-26, implemented by
[ExecPlan 25](../plans/25-create-the-ephemeral-root-with-owner-only-permissions-and-record-acceptance-on-the-current-postgresql.md)
under [MasterPlan 6](../masterplans/6-close-the-notification-reconciler-and-evidence-gaps-surfaced-by-the-0-6-1-0-review.md).

## Context

Test suites share `/tmp/ephpg-pgmq-hs-<effective uid>` so abandoned clusters and their process
registry stay discoverable across development shells. A uid embedded in a path does not prove
ownership. Earlier helpers used the process umask despite the documented `0700` guarantee;
changing only the creation mode would leave existing installations permissive.

## Decision

All five local creators create with POSIX mode `0700`, then inspect the root without
following a symlink. Require a real directory owned by the effective uid before chmod.
Tighten existing owned directories to `0700` and verify permissions and identity afterward.
Reject files, symlinks and foreign ownership before database startup; propagate other errors.
Keep the existing per-suite helpers and `unix` dependency rather than adding a shared library.

Preserve root contents, including process registrations. Test unsafe paths only within
private disposable parents. Never delete the shared root as permission repair or server
recovery: that can remove live clusters and the registry needed to reclaim them.

## Consequences and verification

Each creator has focused fresh/existing/repeated/file/symlink tests. The existing-directory
and repeated-call cases preserve a sentinel; the symlink case verifies its target's mode
and contents are unchanged. A foreign-owned fixture requires privileges unavailable in the
normal developer run, so acceptance records that limitation rather than touching a real
foreign-owned shared directory. Normal multi-suite acceptance observes the same root reused
with mode `0700` and its registry retained.

## Alternatives

Relying on umask fails the guarantee. Setting mode only at creation does not repair earlier
roots. Unconditional chmod follows unsafe paths. Recursive deletion loses lifecycle state.
