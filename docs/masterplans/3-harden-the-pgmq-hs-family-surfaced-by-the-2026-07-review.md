---
id: 3
slug: harden-the-pgmq-hs-family-surfaced-by-the-2026-07-review
title: "Harden the pgmq-hs family surfaced by the 2026-07 review"
kind: master-plan
created_at: 2026-07-23T23:12:20Z
---

# Harden the pgmq-hs family surfaced by the 2026-07 review

This MasterPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.
If durable project context changes, update or create a note under `docs/design/` in the same
change.


## Vision & Scope

The pgmq-hs family in this repository consists of pgmq-core, pgmq-hasql, pgmq-effectful,
pgmq-config, and pgmq-migration. It is the queue library stack under keiro-pgmq and
shibuya-pgmq-adapter. The July 2026 keiro-pgmq review covered only the seams keiro exercises;
the July 2026 pgmq-hs review deep-read the rest — the full 2,075-line install SQL, every
statement, encoder, and decoder, both effectful interpreters, and the config reconciler — and
verified its serious findings with live reproductions on PostgreSQL 18.4 using this
repository's migration. The library's core is otherwise sound: bind parameterization prevents
an injection path, read/pop/archive are single-statement atomic operations, decoder fidelity
is property-tested, and the traced interpreter is faithful. The immutable install baseline is
upstream pgmq 1.11.0; `docs/masterplans/2-support-pgmq-1-12-0-grouped-head-reads.md` owns the
audited 1.11.0-to-1.12.0 vendor and migration upgrade, including upstream 1.11.1.

The confirmed defects are a family of NULL-parameter traps plus notify-machinery fragility, all currently latent (no registered consumer hits them today) but armed for the 10-15-service adoption where teams will call these APIs directly. Live-verified: `pop` with `qty = Nothing` — documented "default 1" — passes SQL NULL to `LIMIT`, which PostgreSQL reads as `LIMIT ALL`, deleting and returning the entire queue in one statement with no visibility-timeout safety net (PGH-1); `read` with `batchSize = Nothing` leases the whole queue the same way, and `readWithPoll` shares the hazard (PGH-3); `ReadMessage.conditional` is silently dead — never encoded, and the `readMessageConditional` its docs point to does not exist, while the comment justifying the 3-arg form was refuted live (PGH-2); `enable_notify_insert` with `throttleMs = Nothing` — documented "pgmq default 250ms" — deterministically fails with a NOT NULL violation (the destroys-existing-trigger half was refuted: statement atomicity rolls the drop back) (PGH-4); and the notify throttle table is UNLOGGED, so a crash/immediate-shutdown recovery truncates it and the insert trigger silently never notifies again until an application-side re-enable — demonstrated with a full live crash cycle (PGH-6). Also confirmed by review: `setVisibilityTimeoutAt` throws on a raced-away row instead of returning `Nothing` (PGH-5); uppercase queue names silently alias two logical queues onto one physical table and break notify, and `FromJSON QueueName` bypasses validation entirely (PGH-7); the reconciler races concurrent replica startups on notify-trigger creation and partitioned re-entry (PGH-8); the documented NOTIFY channel name is wrong on every component (PGH-9); `isTransient` classifies deadlock/serialization SQLSTATEs as permanent (PGH-10); and a NULL message body inserted by any non-Haskell producer poisons every read batch at decode (PGH-11).

After this initiative: no `Maybe` parameter can silently mean "unbounded" or "always fails" —
each defaults as documented via `COALESCE` or has an explicit type; notifications continue
after crash recovery even though throttle state is temporarily lost, and the channel contract
is exported as code; queue names are rejected consistently across every entry path; transient
SQLSTATEs classify as transient; and the new behavior is pinned by tests, including the
previously-untested `Nothing` cases. In scope are PGH-1 through PGH-11 and their pgmq-hs tests
and documentation. The coordinated 0.5.0.0 version bump, changelog integration, and consumer
rollout are owned by
`docs/plans/12-expose-grouped-reads-on-the-umbrella-api-and-release-0-5-0-0.md`.

Out of scope are the grouped-head feature owned by MasterPlan 2; the FIFO ordering, index, and
partition-retention findings owned by keiro MasterPlan 17 plans 116 and 118; and new queue
features.


## Decomposition Strategy

Three child plans divide the work by defect family. EP-13 owns NULL-parameter semantics
(PGH-1 through PGH-5): the fix pattern is uniform (`COALESCE($n, default)` in client statement
text), the false comments fall together, and one test module covers the `Nothing` cases.
EP-14 owns notification reliability (PGH-6, PGH-8, and PGH-9): add a fail-open trigger
fallback after crash recovery, serialize notification and partitioned-creation SQL, export the
channel-name helper, and correct the Haddock. EP-15 owns input validation and classification
(PGH-7, PGH-10, and PGH-11): validating `QueueName` JSON decoding, the transient SQLSTATE
whitelist, and nullable message-body decoding.

Alternatives considered. One mega-plan was rejected because the families have disjoint test
surfaces: statement semantics, crash/notification cycles, and validation/classification.
Folding PGH-8 into EP-15 was rejected because its fix is SQL and advisory-lock work in the
same functions EP-14 touches.

Durable library decisions belong under this repository's `docs/design/` directory. Keiro's
`docs/adr/0001-keiro-pgmq-job-processing-telemetry-contract.md` remains an external consumer
constraint: none of these plans may change the traced interpreter's span semantics. Candidate
design notes are the NULL-parameter rule ("no optional parameter may widen scope") and the
notification-channel and crash-fallback contract.


## Exec-Plan Registry

| # | Title | Path | Hard Deps | Soft Deps | Status |
|---|-------|------|-----------|-----------|--------|
| 13 | Fix NULL parameter semantics across pop read and notify statements | docs/plans/13-fix-null-parameter-semantics-across-pop-read-and-notify-statements.md | None | None | Not Started |
| 14 | Make insert notifications survive crashes and document the channel contract | docs/plans/14-make-insert-notifications-survive-crashes-and-document-the-channel-contract.md | None | None | Not Started |
| 15 | Validate queue names and classify transient errors across the pgmq layers | docs/plans/15-validate-queue-names-and-classify-transient-errors-across-the-pgmq-layers.md | None | None | Not Started |


## Dependency Graph

There are no hard dependencies among EP-13, EP-14, and EP-15. They have integration
dependencies on shared files described below. EP-14 should define `notifyChannelName` before
the final reconciliation of EP-15's `Pgmq.Types` edits, but either can begin first.

The release dependency points outward: MasterPlan 2's EP-12 must not cut 0.5.0.0 until EP-13,
EP-14, and EP-15 are complete. The migration ledger is shared by MasterPlan 2 EP-9, keiro
MasterPlan 17 plans 116 and 118, and EP-14. No plan reserves a numeric migration filename in
advance; each takes the next free manifest number when it lands.


## Integration Points

`pgmq-migration/migrations/` is owned by EP-14 within this MasterPlan. EP-13 has no migration:
its `enable_notify_insert` client statement coalesces its bound value, while EP-14's migration
adds the complete server-side guard for non-Haskell callers. EP-14 must take the next free
manifest number after every migration already present; it must never edit the immutable
`0001-install-v1.11.0.sql`.

The migration directory is also shared with MasterPlan 2 EP-9, and that coupling goes beyond
numbering. EP-9 adds a schema-convergence test asserting that every `pgmq` function body after
the full ledger matches a fresh install of the vendored upstream `pgmq.sql`. EP-14's three
`CREATE OR REPLACE FUNCTION` statements deliberately diverge from upstream, so if EP-9 has
already landed, EP-14 must add those three signatures to that test's deliberate-deviation
allowlist in the same commit, each with a comment naming the decision that authorises it.
Repairing the failure by removing the body comparison is forbidden — it would discard the
guarantee for every function this repository does not own. See Integration Point 7 of
`docs/masterplans/2-support-pgmq-1-12-0-grouped-head-reads.md`.

`pgmq-core/src/Pgmq/Types.hs` is shared by EP-14 and EP-15. EP-14 defines and exports
`notifyChannelName`; EP-15 replaces derived `FromJSON QueueName` with validation. The final
lander must preserve both changes and run `pgmq-core-test`.

`pgmq-hasql/pgmq-hasql.cabal`, `pgmq-hasql/test/Main.hs`, and the package test tree are shared
by all three plans. Each plan owns registration of its own test module. The final lander runs
`cabal test all` and reconciles `other-modules` without dropping siblings.

`CHANGELOG.md` and package changelogs are release-owned artifacts. Each hardening plan records
the exact entry it needs; MasterPlan 2 EP-12 consolidates those entries, bumps every internal
bound, performs the full consumer rollout, and cuts 0.5.0.0.


## Progress

- [ ] EP-13: `pop`/`read`/`readWithPoll`/`set_vt`/`enable_notify_insert` NULL semantics fixed; false comments corrected; existing visibility-timeout tests updated for `Maybe`; `Nothing`-case tests pass.
- [ ] EP-13: `changeVisibilityTimeout` and `setVisibilityTimeoutAt` return `Maybe Message` instead of throwing on a raced row.
- [ ] EP-14: notification delivery survives crash recovery through the deliberate fail-open path; the crash-cycle listener reconnects after restart and proves delivery.
- [ ] EP-14: `notifyChannelName` exported; Haddock corrected; SQL mutations advisory-locked; concurrent-startup test passes.
- [ ] EP-15: Queue names rejected consistently; `FromJSON` validates; mixed-case remediation preserves topic bindings and notification configuration.
- [ ] EP-15: `isTransient` whitelists 40001/40P01/55P03/57P01/57P02/57P03/53xxx; nullable-body decode decision implemented and documented.
- [ ] EP-12 in MasterPlan 2: all hardening changes consolidated into 0.5.0.0; every in-scope consumer bound and component updated and validated.


## Surprises & Discoveries

- Verification (2026-07-23): all serious findings reproduced live on PostgreSQL 18.4 with the repo's own migration — including the full PGH-6 crash cycle (immediate shutdown truncates the throttle table; clean restart preserves it; notify demonstrably stalls; re-enable heals).
- Verification (2026-07-23): PGH-4's "destroys the existing trigger" half is refuted — the function call is one statement, so the 23502 rolls back the internal trigger drop atomically; both trigger and throttle row verified intact after the failure.
- Verification (2026-07-23): exposure of every finding is currently latent — no registered consumer calls `pop`, passes `Nothing` for the affected parameters, or uses notify. The hardening is for direct-API use by the incoming service fleet.
- Plan authoring (2026-07-23): the shared family release must be 0.5.0.0, not 0.4.x — EP-13's `Maybe Message` result-type change is PVP-major and consumer bounds (`<0.5`, `^>=0.4`) would otherwise silently break solves; MasterPlan 17's plans 116/118 claimed 0.4.1.0/0.4.2.0 for SQL-only changes and are superseded if they land on this train (noted there too).
- Plan authoring (2026-07-23): EP-13 needs no migration — all its fixes are client-side statement text (the `$n` binds exist only client-side; SQL functions use named args); the sole server-side NULL guard (`enable_notify_insert` COALESCE-250) folds into EP-14's migration, which re-creates that function anyway.
- Plan authoring (2026-07-23): PGH-10's exposure is sharper than believed — shibuya-pgmq-adapter calls `isTransient` on every ack/poll retry gate (`Internal.hs:62,515`) and consumes `setVisibilityTimeoutAt`'s result (`Internal.hs:199-206`), so EP-15's classification fix and EP-13's type change both need adapter-side follow-through in release plan 12. Also: `acquire_queue_lock` hashes the raw (non-lowercased) name — a fourth aliasing artifact removed by EP-15's uppercase rejection.
- Validation (2026-07-23): changing visibility-timeout results to `Maybe Message` also requires updates to three existing pgmq-hasql tests that dereference the result directly; EP-13 now names them explicitly.
- Validation (2026-07-23): `pgmq.topic_bindings` has a foreign key to `pgmq.meta` with no `ON UPDATE` and with `ON DELETE CASCADE`; EP-15's mixed-case remediation must preserve bindings transactionally.
- Validation (2026-07-23): the raw libpq listener in the crash test must be opened after restart, because immediate shutdown kills the pre-crash connection; EP-14 now specifies reconnect, re-LISTEN, and UTF-8 encoding.
- Validation (2026-07-23): Mori found pgmq family pins in more components than the old release step listed, including shibuya tests, examples, benchmarks, and rei. MasterPlan 2 EP-12 owns a complete rollout matrix rather than a library-only bound edit.
- Coordination (2026-07-23): official MasterPlan 2 already audits upstream 1.11.1 as part of its commit-pinned 1.12.0 upgrade. EP-14 must compose its migration after whatever ledger entries have landed rather than copying a stale reserved number.


## Decision Log

- Decision: Fix NULL semantics in the SQL statements (COALESCE) and/or Haskell types per parameter, not by documenting the current behavior.
  Rationale: "Nothing = unbounded destructive operation" is indefensible as a contract regardless of documentation; the live repros show the blast radius.
  Date: 2026-07-23

- Decision: MasterPlan 2 EP-12 is the single owner of the coordinated 0.5.0.0 repository release and consumer rollout.
  Rationale: The repository already had one release plan. Giving EP-15 a second "last lander" created competing version, changelog, migration, and consumer-bound ownership. EP-12 now depends on all three hardening plans and consolidates their release work once.
  Date: 2026-07-23

- Decision: Keep the three defect-family plans independently implementable and model their shared files as integration dependencies rather than inventing hard dependencies.
  Rationale: Their behavior and tests remain independently verifiable, while explicit ownership of `Pgmq.Types`, test registries, the migration ledger, and release artifacts prevents silent clobbering.
  Date: 2026-07-23


## Outcomes & Retrospective

(To be filled during and after implementation.)


## Revision Note

2026-08-05: Recorded the schema-convergence coupling with MasterPlan 2 EP-9 in Integration
Points, and required EP-14 to allowlist its three deliberately-diverging function bodies in
that test when EP-9 has already landed. Found while validating
`docs/masterplans/2-support-pgmq-1-12-0-grouped-head-reads.md`; both MasterPlans had recorded
the manifest-numbering half of the shared-ledger coupling but neither had recorded this half.

2026-07-23: Relocated this MasterPlan and its child plans from keiro into the authoritative
pgmq-hs repository. Incorporated the validation findings for existing `Maybe Message` tests,
crash-listener lifecycle and libpq types, mixed-case topic-binding preservation, transient
shutdown SQLSTATEs, complete consumer bounds, shared-file ownership, migration numbering, and
upstream-version reconciliation. Assigned the single 0.5.0.0 release to MasterPlan 2 EP-12.
