---
id: 3
slug: harden-the-pgmq-hs-family-surfaced-by-the-2026-07-review
title: "Harden the pgmq-hs family surfaced by the 2026-07 review"
kind: master-plan
created_at: 2026-07-23T23:12:20Z
intention: intention_01kz9yszpmejztjbet6k4bvcf7
provenance:
  revisions:
    - model: "gpt-6-astra"
      harness: "codex-cli"
      at: 2026-09-10T19:03:46Z
      mode: "update"
      note: "Reconcile shipped hardening with PGMQ 1.12/1.13, six-entry migrations, and the 0.6.0.0 handoff."
---

# Harden the pgmq-hs family surfaced by the 2026-07 review

This MasterPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.
Durable hardening contracts live under `docs/design/`; the subsequent version-compatibility
decision lives in `docs/adr/pgmq-1.12-1.13-compatibility.md`.


## Vision & Scope

The pgmq-hs family in this repository consists of pgmq-core, pgmq-hasql, pgmq-effectful,
pgmq-config, and pgmq-migration. It is the queue library stack under keiro-pgmq and
shibuya-pgmq-adapter. The July 2026 keiro-pgmq review covered only the seams keiro exercises;
the July 2026 pgmq-hs review deep-read the rest — the full 2,075-line install SQL, every
statement, encoder, and decoder, both effectful interpreters, and the config reconciler — and
verified its serious findings with live reproductions on PostgreSQL 18.4 using this
repository's migration. The library's core is otherwise sound: bind parameterization prevents
an injection path, read/pop/archive are single-statement atomic operations, decoder fidelity
is property-tested, and the traced interpreter is faithful. The immutable install baseline remains
upstream pgmq 1.11.0. The current vendor is 1.13.0, and the six-entry native ledger upgrades
through 1.12.0 to 1.13.0 while preserving this hardening.
`docs/masterplans/2-support-pgmq-1-12-0-grouped-head-reads.md` owns those completed upgrades
and the remaining 0.6.0.0 release preparation. This MasterPlan and EP-13/14/15 are complete;
their changes shipped in 0.5.0.0 on 2026-08-06. The findings below describe the July baseline,
not outstanding defects in the current tree.

The review confirmed a family of NULL-parameter traps plus notify-machinery fragility, latent at review time (no registered consumer exercised those NULL/notify paths then) but armed for the 10-15-service adoption where teams will call these APIs directly. Live-verified: `pop` with `qty = Nothing` — documented "default 1" — passes SQL NULL to `LIMIT`, which PostgreSQL reads as `LIMIT ALL`, deleting and returning the entire queue in one statement with no visibility-timeout safety net (PGH-1); `read` with `batchSize = Nothing` leases the whole queue the same way, and `readWithPoll` shares the hazard (PGH-3); `ReadMessage.conditional` is silently dead — never encoded, and the `readMessageConditional` its docs point to does not exist, while the comment justifying the 3-arg form was refuted live (PGH-2); `enable_notify_insert` with `throttleMs = Nothing` — documented "pgmq default 250ms" — deterministically fails with a NOT NULL violation (the destroys-existing-trigger half was refuted: statement atomicity rolls the drop back) (PGH-4); and the notify throttle table is UNLOGGED, so a crash/immediate-shutdown recovery truncates it and the insert trigger silently never notifies again until an application-side re-enable — demonstrated with a full live crash cycle (PGH-6). Also confirmed by review: `setVisibilityTimeoutAt` throws on a raced-away row instead of returning `Nothing` (PGH-5); uppercase queue names silently alias two logical queues onto one physical table and break notify, and `FromJSON QueueName` bypasses validation entirely (PGH-7); the reconciler races concurrent replica startups on notify-trigger creation and partitioned re-entry (PGH-8); the documented NOTIFY channel name is wrong on every component (PGH-9); `isTransient` classifies deadlock/serialization SQLSTATEs as permanent (PGH-10); and a NULL message body inserted by any non-Haskell producer poisons every read batch at decode (PGH-11).

The completed hardening guarantees: no `Maybe` parameter can silently mean "unbounded" or "always fails" —
each defaults as documented via `COALESCE` or has an explicit type; notifications continue
after crash recovery even though throttle state is temporarily lost, and the channel contract
is exported as code; queue names are rejected consistently across every entry path; transient
SQLSTATEs classify as transient; and the new behavior is pinned by tests, including the
previously-untested `Nothing` cases. In scope are PGH-1 through PGH-11 and their pgmq-hs tests
and documentation. The 0.5.0.0 release is published. Subsequent 0.6.0.0 changelog integration and consumer
validation are owned by
`docs/plans/12-expose-grouped-reads-on-the-umbrella-api-and-release-0-5-0-0.md`.

Out of scope are grouped heads, explicit partition premake, and default-partition metrics
owned by MasterPlan 2; the FIFO ordering, index, and
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

Durable hardening decisions remain in `docs/design/014-null-parameter-contract.md`,
`docs/design/015-notification-delivery-contract.md`,
`docs/design/016-queue-name-validation.md`, and
`docs/design/017-transient-error-classification.md`. The accepted
[compatibility ADR](../adr/pgmq-1.12-1.13-compatibility.md) explains how later upgrades preserve
those contracts: immutable history, a separate four-argument partition override, and
version-specific convergence tests. This update records that existing decision; it does not
change architecture or reopen the completed children.


## Exec-Plan Registry

| # | Title | Path | Hard Deps | Soft Deps | Status |
|---|-------|------|-----------|-----------|--------|
| 13 | Fix NULL parameter semantics across pop read and notify statements | docs/plans/13-fix-null-parameter-semantics-across-pop-read-and-notify-statements.md | None | None | Complete |
| 14 | Make insert notifications survive crashes and document the channel contract | docs/plans/14-make-insert-notifications-survive-crashes-and-document-the-channel-contract.md | None | None | Complete |
| 15 | Validate queue names and classify transient errors across the pgmq layers | docs/plans/15-validate-queue-names-and-classify-transient-errors-across-the-pgmq-layers.md | None | None | Complete |


## Dependency Graph

EP-13, EP-14, and EP-15 completed independently, with integration dependencies on the
shared artifacts below. Their old gate on a future 0.5.0.0 release is satisfied and obsolete.
MasterPlan 2 EP-9/10/11 are now Complete, and EP-12 remains In Progress for 0.6.0.0 release
preparation and consumer validation. Hardening is a protected regression baseline, not an
unimplemented prerequisite. Follow EP-12 and its release evidence for current release status.


## Integration Points

`pgmq-migration/migrations/` was owned by EP-14 within this MasterPlan. It landed
`0003-notify-crash-safety-and-locking.sql`; EP-13 and EP-15 needed no migrations. MasterPlan 2
EP-9 subsequently appended `0004-upgrade-v1.12.0.sql`, `0005-upgrade-v1.13.0.sql`, and
`0006-preserve-partitioned-reentry-v1.13.0.sql`. Entries 0001–0003 remain immutable, including
`0001-install-v1.11.0.sql`. Future authors must read the manifest at implementation time
rather than reuse the historical 0004 allocation advice below.

The 1.13 upstream upgrade drops `create_partitioned(text,text,text)` and replaces it with
`create_partitioned(text,text,text,integer)`. Migration 0006 restores EP-14's queue/archive
parent-registration guards and retains the advisory lock, while preserving upstream premake
forwarding and `GENERATED BY DEFAULT` identities. Do not replay 0003 or recreate the old
overload. Queue-creation traffic must wait for the entire migration suffix, including 0006.
The notification fail-open function and NULL throttle guard from 0003 remain in force.

EP-9 owns `testConvergence` in `pgmq-migration/test/Main.hs`. It compares the four-entry 1.12
checkpoint against `pgmq-migration/test/fixtures/pgmq-1.12.0.sql` and the full six-entry 1.13
ledger against `vendor/pgmq/pgmq-extension/sql/pgmq.sql`. Each comparison permits exactly
three local function-body deviations: `notify_queue_listeners()`,
`enable_notify_insert(text,integer)`, and the version-appropriate three- or four-argument
`create_partitioned`. Retain the body comparison and behavioral re-entry/crash tests; an
allowlist alone does not prove the hardening survives. Ledger expectations derive from
`nativeMigrationNames` and `testNativeComponent`.

`pgmq-core/src/Pgmq/Types.hs` contains both EP-14's `notifyChannelName` and EP-15's validating
`FromJSON QueueName`; both are complete and must survive later edits. MasterPlan 2 added
`QueueMetrics.defaultPartitionLength :: Maybe Int64`: missing on 1.12 or inapplicable on
ordinary queues is `Nothing`, not zero. This is distinct from EP-15 decoding a SQL NULL
message body as JSON `null`. Grouped heads and existing partition creation work on 1.12 and
1.13; explicit premake requires 1.13. Configuration premake is creation-only, as documented
in the compatibility ADR and `docs/design/018-reconciliation-contract.md`.

All three hardening plans registered their test modules. Later validation must retain those
modules alongside the version matrix and real pg_partman tests. The acceptance commands are:

```bash
PGMQ_REQUIRE_PARTMAN=1 PGMQ_TEST_SCHEMA_VERSION=1.13.0 cabal test all --test-show-details=direct
PGMQ_TEST_SCHEMA_VERSION=1.12.0 cabal test pgmq-hasql --test-options='--pattern "GroupedHead"'
PGMQ_TEST_SCHEMA_VERSION=1.12.0 cabal test pgmq-hasql --test-options='--pattern "Metrics"'
```

Run in the project partman development environment; migration tests must remain serial.
These are ongoing regression requirements, not tests rerun for this documentation update.
See `docs/releases/0.6.0.0-candidate.md` for recorded execution and consumer evidence.

`CHANGELOG.md` and package changelogs already contain the published 0.5.0.0 hardening entries.
MasterPlan 2 EP-12 owns the new 0.6.0.0 entries, consistent family bounds, and consumer
validation. Preserve published history and avoid presenting the hardening as newly released
in 0.6.0.0. Publication of that candidate is separate from this plan update.


## Progress

- [x] EP-13 (2026-08-05): `pop`/`read`/`readWithPoll`/`set_vt`/`enable_notify_insert` NULL semantics fixed; false comments corrected; existing visibility-timeout tests updated for `Maybe`; `Nothing`-case tests pass.
- [x] EP-13 (2026-08-05): `changeVisibilityTimeout` and `setVisibilityTimeoutAt` return `Maybe Message` instead of throwing on a raced row.
- [x] EP-14 (2026-08-05): notification delivery survives crash recovery through the deliberate fail-open path; the crash-cycle listener reconnects after restart and proves delivery.
- [x] EP-14 (2026-08-05): `notifyChannelName` exported from `Pgmq.Types` and the `Pgmq` umbrella; Haddock and design note 006 corrected; `enable_notify_insert` and `create_partitioned` advisory-locked and re-entrant; the concurrent-startup test goes from ~28% failures to zero.
- [x] EP-15 (2026-08-05): Queue names rejected consistently at both entry paths; `FromJSON` validates via `parseQueueName`; mixed-case remediation (design note 016) preserves topic bindings and notification configuration, proven for the twin and no-twin cases with rerun idempotence by `MixedCaseRemediationSpec`.
- [x] EP-15 (2026-08-05): `isTransient` whitelists 40001/40P01/55P03/57P01/57P02/57P03/53xxx, pinned in both directions (design note 017); SQL NULL bodies decode as JSON `null`, un-poisoning read batches (design note 014's NULL-cell section).
- [x] (2026-09-10 reconciliation): hardening shipped in the 2026-08-06 family release 0.5.0.0, as recorded in the root and package changelogs.
- [x] (2026-09-10 reconciliation): MasterPlan 2 EP-9/10/11 delivered PGMQ 1.12/1.13 support and preserved the hardening through migration 0006 and version-specific regression coverage.
- [ ] External follow-through, MasterPlan 2 EP-12: finish the 0.6.0.0 candidate and in-scope consumer validation; this does not reopen EP-13/14/15.


## Surprises & Discoveries

The dated July/August entries below retain execution-time observations. Their pending-release,
vendor-version, and next-migration-number statements are historical, superseded by this refresh.

- Reconciliation (2026-09-10): 0.5.0.0 shipped separately from grouped heads. Current source is the 0.6.0.0 candidate, and the native ledger reaches PGMQ 1.13 through six migrations.
- Reconciliation (2026-09-10): upstream 1.13 replaces the partition function signature and loses the local parent guards unless migration 0006 runs. Convergence now checks both server versions with the corresponding function identity.

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
- EP-13 implementation (2026-08-05): a pre-existing design note, `docs/design/010-read-conditional-null-handling.md`, had recorded as settled the very decision EP-13 reverses, and recorded it with a root cause that does not hold against the vendored SQL — it claims two overloaded `pgmq.read` functions (there is one, with `conditional JSONB DEFAULT '{}'`) and argues that `coalesce($4,'{}')` is "not viable" (the function's `CASE` gives `'{}'` the meaning "no filter", so it is exactly the right fix). That note is why `ReadMessage.conditional` stayed dead through a release. It is now marked Superseded with a Correction section, and the durable rule lives in `docs/design/014-null-parameter-contract.md`. **Bearing on EP-14 and EP-15**: check `docs/design/` for a note covering the behavior you are about to change before assuming the current code reflects a considered decision, and correct the note in the same change.
- EP-13 implementation (2026-08-05): this repository has no `pgmq-core-test` suite. The Integration Points section below tells the second lander of the shared `pgmq-core/src/Pgmq/Types.hs` edits (EP-14's `notifyChannelName`, EP-15's validating `FromJSON`) to "run `pgmq-core-test`"; the reconciliation check is `cabal test all`, which runs four suites (pgmq-hasql 61, pgmq-effectful 17, pgmq-config 11, pgmq-migration 9).
- EP-13 implementation (2026-08-05): migration ledger unchanged. `pgmq-migration/migrations/manifest` still holds only `0001-install-v1.11.0.sql` and `0002-schema-management-comment.sql`, so EP-14 takes `0003` unless MasterPlan 2 EP-9 or keiro MasterPlan 17 plans 116/118 land first. EP-13's `enable_notify_insert` fix is client-side statement text only, as designed; the server-side `COALESCE(throttle_interval_ms, 250)` guard for non-Haskell callers is still owed by EP-14's migration.
- EP-14 implementation (2026-08-05): **the notify SQL is upstream, not pgmq-hs-local.** EP-14's plan claimed the whole notify family is repository-local SQL with no upstream-parity cost. It is not: `pgmq.notify_queue_listeners`, `pgmq.enable_notify_insert`, and `pgmq.notify_insert_throttle` are all in `vendor/pgmq/pgmq-extension/sql/pgmq.sql`, byte-identical to the install migration. **Bearing on MasterPlan 2 EP-9**: its schema-convergence allowlist must seed **three** deliberately-diverging signatures — `notify_queue_listeners()`, `enable_notify_insert(TEXT, INTEGER)`, and `create_partitioned(TEXT, TEXT, TEXT)` — not just the partitioned one, each pointing at EP-14's Decision Log.
- EP-14 implementation (2026-08-05): migration `0003-notify-crash-safety-and-locking.sql` claimed and landed; the manifest is now `0001`, `0002`, `0003`. The next plan to append takes `0004`. EP-9 had not landed, so there was no convergence test to allowlist and no newer upstream body to reconcile against (`vendor/pgmq/pgmq-extension/pgmq.control` still declares `default_version = '1.11.0'`).
- EP-14 implementation (2026-08-05): PGH-8 is confirmed, not merely plausible — 400 concurrent `enable_notify_insert` calls collided on SQLSTATE 42710 at roughly a 28% rate, reliably across runs. Two replicas starting together would have hit it within days.
- EP-14 implementation (2026-08-05): **`pgmq-migration/test/Main.hs` enumerated the migration ledger positionally**, so appending `0003` turned five of its nine tests red on a correct change. Those expectations now derive from the plan via `nativeMigrationNames`, and the ledger is spelled out in exactly one place, `testNativeComponent`. **Bearing on MasterPlan 2 EP-9 and keiro MasterPlan 17 plans 116/118**: adding a migration now means adding one line to that list, not editing five expectations.
- EP-14 implementation (2026-08-05): a crash test against `ephemeral-pg` must issue `CHECKPOINT` before the immediate shutdown. Its `defaultPostgresSettings` turn off `fsync`, `synchronous_commit`, and `full_page_writes`, so SIGQUIT otherwise discards the schema install itself. `CHECKPOINT` does not make unlogged tables crash-safe, so it preserves the behavior under test.
- EP-14 implementation (2026-08-05): the wrong channel name lived in `docs/design/006-queue-notifications.md` as well as the Haddock — confirming EP-13's lesson from the other direction. **Bearing on EP-15**: grep `docs/design/` for the behavior you are changing before assuming the code is the only place the claim is recorded.
- EP-13 implementation (2026-08-05): both effectful interpreters passed the changed result type through without edits — `withTracedOp config pool (...) $ Sessions.changeVisibilityTimeout query` is polymorphic in the session's result — so keiro's ADR 0001 telemetry contract is preserved by construction. EP-14 and EP-15 can expect the same of any result-type change that does not touch `withTracedOp`'s `OpInfo`.
- EP-15 implementation (2026-08-05): **EP-14's fail-open trigger reshaped PGH-7's notify consequence between authoring and implementation.** With migration `0003`, a mixed-case throttle row no longer silences notifications — the trigger fails open on the missing (lowercased) key and notifies unthrottled while `last_notified_at` stays frozen at the epoch. The defect is the same (the configured throttle is dead on arrival) but the observable evidence changed; the frozen epoch timestamp is what `AliasingSpec` pins. Lesson for any plan whose evidence predates a sibling's landing: re-derive the observable before writing the assertion.
- EP-15 implementation (2026-08-05): **tests that construct invalid states need instance-level isolation once validation tightens.** A mixed-case `pgmq.meta` row — however short-lived — makes every concurrent `listQueues` decode fail under the stricter parser, because `queueDecoder` re-validates via `D.refine` and tasty runs specs in parallel. `AliasingSpec` and `MixedCaseRemediationSpec` therefore each provision a dedicated PostgreSQL instance (NotifyCrashSpec's pattern), separately from each other because the remediation sweeps every mixed-case row in its database. This is the template for future poisonous-state tests.
- EP-15 implementation (2026-08-05): `pgmq.notify_insert_throttle` carries the same `ON DELETE CASCADE` / no-`ON UPDATE` foreign key onto `pgmq.meta` as `pgmq.topic_bindings`, so any future metadata surgery must treat BOTH child kinds; a naive parent update fails and a naive delete silently destroys notification configuration as well as routing. The remediation itself simplified to insert-canonical-parent, repoint children by `UPDATE`, delete mixed parent (EP-15's Decision Log).
- EP-15 implementation (2026-08-05): migration ledger unchanged — EP-15 landed no migration, so the next free manifest number remains `0004` for MasterPlan 2 EP-9 or keiro MasterPlan 17 plans 116/118.
- EP-15 implementation (2026-08-05): plan 12's Milestone 5 still claimed the design directory "runs to 013, so 014 is next"; 014–017 now exist (two from EP-13/14, two from EP-15). Corrected in plan 12 to "take the next free number" (018 at time of correction) — the third instance in this MasterPlan of a stale numbering claim in a sibling plan, after the migration-ledger and ledger-expectation cases.
- Post-completion review (2026-08-05): the documented mixed-case remediation had a resurrection defect — its no-twin branch canonicalized a `pgmq.meta` row whose physical table was already destroyed (the exact orphan state `AliasingSpec`'s drop test demonstrates), turning a loud decode failure into a phantom queue that lists cleanly and fails every send with 42P01. The remediation now probes `pg_class` first and deletes orphans, pinned by `MixedCaseRemediationSpec`'s orphan case. Fixing it exposed a latent race in the spec itself: its cases ran in parallel on the shared dedicated instance while each executes the database-global sweep, so a sibling's sweep could delete a parent row between a test's `create` and its `bind_topic` (23503) — the two original cases passed only by scheduling luck. The module now uses `sequentialTestGroup`; the dedicated-instance isolation template gains the corollary "a test that runs a global sweep must also serialize against its own siblings". The same review found design note 015 silent on a fail-open consequence: a foreign caller's mixed-case throttle key is permanently invisible to the lowercased trigger lookup, so migration `0003` converts that mismatch from "never notify" into "permanently unthrottled" — strictly better, unreachable through pgmq-hs's validated API, now documented in 015.


## Decision Log

- Decision: Treat EP-13/14/15 as shipped hardening, preserve their contracts under PGMQ 1.12/1.13, and leave 0.6.0.0 release ownership with MasterPlan 2 EP-12.
  Rationale: Published 0.5.0.0 changelogs and the completed upgrade work supersede the original shared-release schedule. Follow the accepted compatibility ADR and version-specific convergence tests; no new child or ADR is needed.
  Date: 2026-09-10

Earlier entries preserve the original scheduling decisions; the September decision supersedes
their future-0.5.0.0 release assumptions.

- Decision: Fix NULL semantics in the SQL statements (COALESCE) and/or Haskell types per parameter, not by documenting the current behavior.
  Rationale: "Nothing = unbounded destructive operation" is indefensible as a contract regardless of documentation; the live repros show the blast radius.
  Date: 2026-07-23

- Decision: MasterPlan 2 EP-12 is the single owner of the coordinated 0.5.0.0 repository release and consumer rollout.
  Rationale: The repository already had one release plan. Giving EP-15 a second "last lander" created competing version, changelog, migration, and consumer-bound ownership. EP-12 now depends on all three hardening plans and consolidates their release work once.
  Date: 2026-07-23

- Decision: Implement this MasterPlan before `docs/masterplans/2-support-pgmq-1-12-0-grouped-head-reads.md`.
  Rationale: The user's call, and nothing blocks it — EP-13/14/15 have no dependency on
  MasterPlan 2, and MasterPlan 2 EP-12's dependency on all three is satisfied earlier rather
  than violated. EP-14 therefore lands its migration first and will find no schema-convergence
  test to allowlist (that test arrives with MasterPlan 2 EP-9, which now seeds the allowlist
  with EP-14's three functions). Note the scheduling consequence: EP-12 remains the sole
  release owner, so finishing this MasterPlan ships nothing to consumers until MasterPlan 2
  completes. Revisit release ownership if the hardening needs to reach the incoming service
  fleet before the grouped-head work is ready.
  Date: 2026-08-05

- Decision: Keep the three defect-family plans independently implementable and model their shared files as integration dependencies rather than inventing hard dependencies.
  Rationale: Their behavior and tests remain independently verifiable, while explicit ownership of `Pgmq.Types`, test registries, the migration ledger, and release artifacts prevents silent clobbering.
  Date: 2026-07-23


## Outcomes & Retrospective

All three child plans completed 2026-08-05, in authoring order EP-13 → EP-14 → EP-15,
with no reordering, splitting, or cancellation. Every one of PGH-1 through PGH-11 is
fixed and pinned by tests:

- No `Maybe` parameter silently means "unbounded" or "always fails": `pop`, `read`,
  `readWithPoll`, and `enable_notify_insert` coalesce to their documented defaults
  client-side (EP-13), with the server-side `enable_notify_insert` guard in migration
  `0003` for non-Haskell callers (EP-14). `ReadMessage.conditional` is a real, encoded
  parameter. The visibility-timeout operations return `Maybe Message` for raced-away
  rows.
- Notifications survive crash recovery (fail-open trigger), concurrent enables converge
  under the per-queue advisory lock, partitioned creation is re-entrant, and the channel
  name is exported code (`notifyChannelName`) rather than incorrect documentation
  (EP-14).
- Queue names are rejected consistently at both entry paths, transient SQLSTATEs
  classify as transient, and NULL message bodies decode instead of poisoning batches
  (EP-15).

The durable rules live in design notes 014 (NULL-parameter contract, absence-is-not-
failure, NULL-cell-is-data), 015 (notification delivery contract), 016 (queue-name
validation and the mixed-case remediation), and 017 (transient classification). Release
material for all three plans is published in the 0.5.0.0 root and package changelogs dated
2026-08-06. The original deferred-release account is superseded: hardening shipped separately
from grouped heads.

As of 2026-09-10, the current 0.6.0.0 candidate adds PGMQ 1.12/1.13 support while retaining
these guarantees. MasterPlan 2 EP-9/10/11 are Complete; EP-12 still owns remaining candidate
and consumer validation. Its [release evidence](../releases/0.6.0.0-candidate.md) distinguishes
completed checks from remaining work. This documentation reconciliation does not claim a
0.6.0.0 publication or a newly executed test run.

Retrospective, at the initiative level. The decomposition by defect family held: no plan
blocked another, and the three integration points that needed active management
(`Pgmq.Types`, the migration ledger, the test registries) were all handled by the
"second lander reconciles" rule without a single conflict. The recurring failure mode
across all three plans was stale recorded knowledge, not code: a design note that
entrenched a defect (EP-13), documentation wrong on every component plus positional test
expectations (EP-14), and a sibling plan's stale numbering plus a consequence reshaped
by an earlier lander (EP-15). The working countermeasures, now standing guidance: grep
`docs/design/` before assuming code reflects a considered decision, re-derive
observables before writing assertions, and never let a plan hard-code a number another
plan can move.


## Revision Note

2026-08-05 (sixth): pre-release correctness review of all three landed plans (verified
against the vendored SQL and the pre-fix commits; all suites green). Three amendments:
the mixed-case remediation in design note 016 now deletes orphaned rows instead of
resurrecting phantom queues (mirrored in `MixedCaseRemediationSpec` with a new orphan
case), design note 015 now records that fail-open turns an aliased mixed-case throttle
key into permanent unthrottled notification, and the remediation spec's cases are now
sequential because each runs the database-global sweep. No library code changed.

2026-08-05 (fifth): EP-15 implemented and marked Complete, closing the MasterPlan's own
scope (the release remains with MasterPlan 2 EP-12). Recorded five cross-plan
discoveries — the EP-14 fail-open interaction that reshaped PGH-7's notify evidence, the
dedicated-instance isolation template for poisonous-state tests, the second cascading
foreign key on `pgmq.meta`, the unchanged ledger (next free number still `0004`), and a
third stale-numbering correction cascaded into plan 12. Filled in Outcomes &
Retrospective.

2026-08-05 (fourth): EP-14 implemented and marked Complete. Recorded six cross-plan
discoveries, two of which change what MasterPlan 2 EP-9 must do: the notify functions are
upstream-vendored, so EP-9's convergence allowlist needs three signatures rather than one;
and `pgmq-migration`'s ledger expectations no longer need editing per migration. Also
recorded the claimed migration number (`0003`, next free is `0004`), the live confirmation
of PGH-8, the `CHECKPOINT` requirement for crash tests, and a second instance of a false
claim living in `docs/design/` as well as in code.

2026-08-05 (third): EP-13 implemented and marked Complete. Recorded four cross-plan
discoveries: the superseded design note that had entrenched one of the defects, the
absence of a `pgmq-core-test` suite named in Integration Points, the unchanged migration
ledger (EP-14 still takes `0003`), and the confirmation that result-type changes flow
through both effectful interpreters without touching span semantics.

2026-08-05 (second): Recorded the decision to implement this MasterPlan first. EP-14 therefore
expects to claim `0003` and to find no convergence test to allowlist; MasterPlan 2 EP-9 was
made order-independent to match.

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

2026-09-10: Reconciled the completed hardening with published 0.5.0.0 and the current
PGMQ 1.12/1.13 work. Updated release ownership, six-entry migration history, the four-argument
partition override, convergence checkpoints, regression requirements, and child-plan handoffs.
Retained dated implementation evidence as history; no implementation or release was performed.
