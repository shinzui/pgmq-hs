---
id: 6
slug: close-the-notification-reconciler-and-evidence-gaps-surfaced-by-the-0-6-1-0-review
title: "Close the notification, reconciler, and evidence gaps surfaced by the 0.6.1.0 review"
kind: master-plan
created_at: 2026-09-16T20:42:28Z
intention: "intention_01m2nz9a82ejh91yg9sk7t6a7a"
provenance:
  created_by:
    model: "claude-fable-5-1"
    harness: "claude-code"
    at: 2026-09-16T20:42:28Z
---

# Close the notification, reconciler, and evidence gaps surfaced by the 0.6.1.0 review

This MasterPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.
If durable project context changes, update or create ADRs in docs/adr/ in the same change.


## Vision & Scope

A review of every change between tag `v0.4.0.1` (`7535d4915d4ccaa717d860cb0aa54b142d3e2bf8`)
and tag `v0.6.1.0` (`163413b3ccc85ce6a517646da6ff8969c2dd7482`), recorded as `REV-1` through
`REV-5` in `docs/reviews/`, found one behavior regression that no test or retrospective had
captured, and a small set of places where the code, its documentation, and its recorded
evidence disagree. This initiative closes all of them. Nothing here changes the client API
of `pgmq-core`, `pgmq-hasql`, or `pgmq-effectful`.

When the initiative is complete, three things are true that are not true today.

First, enabling insert notifications on a partitioned queue no longer turns every insert into
a `NOTIFY`. Migration `0003` taught the insert trigger to "fail open" when its throttle row is
missing, so that a crash that truncates the `UNLOGGED` throttle table cannot silence delivery.
PostgreSQL clones a row trigger onto every leaf partition and, inside the clone, `TG_TABLE_NAME`
is the partition (`q_pq_p0`), not the queue table (`q_pq`). The name the trigger extracts
(`pq_p0`) can never have a throttle row, so on a partitioned queue the fail-open branch fires on
every insert and publishes on a per-partition channel that `notifyChannelName` never names. The
reviewer reproduced this on the native ledger `0001` through `0006`: three sends produced three
notifications on `pgmq.q_pq_p0.INSERT` and `last_notified_at` never moved, while the same
sequence on an ordinary queue produced one throttled notification. After this initiative an
append-only migration `0007` gates the fail-open on the extracted name having a `pgmq.meta`
row, which restores upstream's behavior for partitioned queues (no notifications at all) while
keeping crash recovery for ordinary queues; a test proves zero notifications arrive on any
channel for a partitioned queue on both the native and the stock 1.12 schema; and design note
015, the `CAP-4` capability record, and the `notifyChannelName` and `enableNotifyInsert`
Haddocks all say that partitioned queues receive no insert notifications and that the
post-crash unthrottled state lasts until an application restart runs the reconciler.

Second, the `pgmq-config` reconciler reports what it cannot truthfully do instead of acting. A
declared lowercase name whose only match in `pgmq.meta` is a foreign mixed-case row (`Foo` when
`foo` is declared) is reported as `DetectedQueueNameCollision` and is not "created", because
`pgmq.create('foo')` would insert a second metadata row over the same physical table, which is
exactly the aliasing hazard design note 016 exists to remediate. A notification declared on a
partitioned queue is reported as `UnsupportedNotifyOnPartitionedQueue` and no trigger is
installed. The `DetectedQueueTypeDrift` Haddock, which today says "nothing was mutated and
nothing will be", states precisely which sub-resources are still reconciled on a drifted queue.
The user guide's `ReconcileAction` listing, which still lacks the two constructors 0.5.0.0
added, and its "every operation is idempotent" sentence match the shipped contract.

Third, the test infrastructure and the recorded evidence match reality. The 0.6.1.0 changelog
says the shared ephemeral PostgreSQL root is "a fixed path created `0700`"; the four test
suites create it with `createDirectoryIfMissing`, which uses the process umask, and only
ephemeral-pg's registry subdirectory is `0700`. The suites will create the root owner-only. The
compatibility ADR and the 0.6.0.0 release evidence record acceptance on PostgreSQL 17.10 with
pg_partman 5.4.3, but the `partman` development shell has shipped PostgreSQL 18.6 since the
nix-haskell-flake 0.24.0 migration. The family's acceptance is re-run on the server the shell
actually provides, the version is recorded, and a 0.7.0.0 release candidate carries migration
`0007` and the breaking `ReconcileAction` additions.

Explicitly excluded: making partitioned queues actually deliver notifications by resolving the
parent table through `pg_inherits` inside the trigger (that is a feature upstream's trigger
lacks and belongs in `mori://pgmq/pgmq`, not in a local override); any FIFO ordering or index
work (owned by
[MasterPlan 5](5-correct-the-fifo-grouped-read-ordering-index-and-partition-retention-contracts.md));
auditing the `enable_notify_insert` and `create_partitioned` overrides in migrations `0003`
and `0006` beyond what the trigger fix touches; automatic remediation of mixed-case aliasing
(design note 016 stays an operator procedure); Hackage publication; and any change to the
pinned upstream 1.13.0 source.


## Decomposition Strategy

The work splits into three streams by functional concern, each producing a behavior that can
be verified without the others.

The first stream is the server-side notification contract: one migration, the tests that pin
it, and the documents that state it. It lives in `pgmq-migration` (SQL and ledger tests), in
`pgmq-hasql` (the notification test that needs `LISTEN` through libpq and a partitioned
queue), and in the design notes, capability record, and Haddocks that describe delivery.

The second stream is reconciler truthfulness, entirely inside `pgmq-config`: the shared core in
`pgmq-config/src/Pgmq/Config/Reconcile.hs`, the report type in
`pgmq-config/src/Pgmq/Config/Types.hs`, the contract Haddock in `pgmq-config/src/Pgmq/Config.hs`,
design note 018, the user guide, and the config test suites. It is a separate stream because it
is a separate package with a separate release consequence: adding constructors to
`ReconcileAction` is a breaking change under the family's Package Versioning Policy practice,
which the first stream is not.

The third stream is infrastructure and evidence: the four test-support files that create the
ephemeral root, the acceptance run on the current PostgreSQL, the release evidence document,
the compatibility ADR's verification paragraph, and the 0.7.0.0 candidate's versions and
changelogs. It comes last because its evidence run must include the first two streams.

Alternatives considered. A single ExecPlan was rejected: the work spans three packages and a
migration, has a real ordering constraint (evidence after fixes), and the three concerns are
verified by different suites under different shells. Fixing the notification regression only
in the Haskell client (a guard inside `enableNotifyInsert`) was rejected: it would need a
queue-shape query on every call, would leave non-Haskell callers and every existing native
install regressed, and would not touch the trigger that actually misbehaves. Editing migration
`0003` was rejected: the ledger is immutable and the test suite pins its MD5. Folding the
reconciler report into the first stream was rejected for the release reason above.

ADRs consulted, all local and cited by repository-relative path:

- [docs/adr/fifo-native-overrides-and-index-upgrade-boundary.md](../adr/fifo-native-overrides-and-index-upgrade-boundary.md)
  prohibits new extension-SQL overrides *for FIFO work* and says in so many words that
  "historical notification/partition overrides are not removed or extended by this
  initiative". This MasterPlan is that separate work. It does not add a new override lineage;
  it corrects a defect inside the lineage migration `0003` already opened, by an append-only
  migration, and records the boundary in a new ADR.
- [docs/adr/pgmq-1.12-1.13-compatibility.md](../adr/pgmq-1.12-1.13-compatibility.md) fixes the
  rules every stream obeys: existing migration bytes and order never change, local deviations
  are appended and tested for behavior while their bodies are excepted from catalog
  convergence, the SQL runner applies one transaction per migration, and the recorded
  acceptance server is PostgreSQL 17.10 with pg_partman 5.4.3 (which the third stream updates).
- [docs/adr/haskell-dependency-bounds-and-nix-pin-policy.md](../adr/haskell-dependency-bounds-and-nix-pin-policy.md)
  states that the Nix pin says what we test. The third stream applies the same principle to
  the PostgreSQL the `partman` shell provides.

No cross-repository ADR applies. The upstream source is `mori://pgmq/pgmq` at tag `v1.13.0`
(`32c075bb6dbed66a303d1a792393c93e36c09a97`); its trigger function is unchanged between 1.11.0
and 1.13.0, which is why migration `0003`'s override survives `0004` and `0005`. The design
notes that carry the affected contracts are
[docs/design/015-notification-delivery-contract.md](../design/015-notification-delivery-contract.md),
[docs/design/016-queue-name-validation.md](../design/016-queue-name-validation.md),
[docs/design/018-reconciliation-contract.md](../design/018-reconciliation-contract.md), and
[docs/design/012-vendor-upstream-pgmq-sql.md](../design/012-vendor-upstream-pgmq-sql.md).


## Exec-Plan Registry

| # | Title | Path | Hard Deps | Soft Deps | Status |
|---|-------|------|-----------|-----------|--------|
| 1 | Gate the notification fail-open on a real queue row and state the partitioned-queue contract | docs/plans/23-gate-the-notification-fail-open-on-a-real-queue-row-and-state-the-partitioned-queue-contract.md | None | None | Not Started |
| 2 | Report name collisions and unsupported notifications instead of acting on them | docs/plans/24-report-name-collisions-and-unsupported-notifications-instead-of-acting-on-them.md | None | EP-1 | Not Started |
| 3 | Create the ephemeral root with owner-only permissions and record acceptance on the current PostgreSQL | docs/plans/25-create-the-ephemeral-root-with-owner-only-permissions-and-record-acceptance-on-the-current-postgresql.md | EP-1, EP-2 | None | Not Started |

Status values: Not Started, In Progress, Complete, Cancelled.
Hard Deps and Soft Deps reference other rows by their # prefix (e.g., EP-1, EP-3).


## Dependency Graph

EP-1 and EP-2 share no code and can be implemented in parallel. EP-2 has a soft dependency on
EP-1 only because the Haddock of its new `UnsupportedNotifyOnPartitionedQueue` constructor
cites the statement "partitioned queues receive no insert notifications" that EP-1 writes into
design note 015; if EP-2 lands first, it writes that sentence itself and EP-1 keeps it.

EP-3 hard-depends on both. Its acceptance run exists to record the family's behavior on the
current PostgreSQL *after* the migration and the reconciler change, its release evidence
document must list migration `0007`'s digest and the new constructors, and its 0.7.0.0
changelog entry aggregates the "Unreleased" sections the first two plans write. Its first
milestone (the owner-only root) has no real dependency and may be done at any time, but the
plan is sequenced as a whole so that one evidence run covers everything.


## Integration Points

Design note 015 (`docs/design/015-notification-delivery-contract.md`) is owned by EP-1, which
adds the partitioned-queue statement and the "until an application restart" duration. EP-2
links to that statement from its constructor Haddock and from the user guide; it does not
edit the note.

`ReconcileAction` in `pgmq-config/src/Pgmq/Config/Types.hs` is owned by EP-2, which adds
`DetectedQueueNameCollision` and `UnsupportedNotifyOnPartitionedQueue`. EP-3 lists both in the
0.7.0.0 changelog as breaking for exhaustive matchers; nothing else touches the type.

`docs/user/queue-configuration.md` is owned by EP-2 for its Notify Insert, Reconciliation
Functions, and Idempotency Guarantees sections. EP-1 does not edit the guide; the partitioned
notification limitation appears there in EP-2's wording, which must agree with design note
015's.

The migration ledger (`pgmq-migration/migrations/manifest`, the new `0007` file, and the
ledger-derived expectations in `pgmq-migration/test/Main.hs`) is owned by EP-1. EP-3 records
the new file's MD5 in the release evidence and does not change the ledger.

Changelogs: EP-1 and EP-2 each append an "Unreleased" section to the root `CHANGELOG.md` and
to the changelog of every package they change. EP-3 converts those sections into the 0.7.0.0
entries and bumps versions. Published sections are never edited.

ADRs: EP-1 creates `docs/adr/notification-override-lineage-and-partitioned-queue-boundary.md`
in the repository's plain-Markdown convention (there is no profiled `docs/adr` bundle). EP-3
updates the verification paragraph of `docs/adr/pgmq-1.12-1.13-compatibility.md` with the
re-run server version. EP-2 updates design note 018 rather than an ADR, because the
report-not-repair boundary is already recorded there.

The four ephemeral-root creation sites (`pgmq-hasql/test/EphemeralDb.hs`,
`pgmq-effectful/test/EphemeralDb.hs`, `pgmq-config/test/EphemeralDb.hs`, and
`pgmq-migration/test/Main.hs`) are owned by EP-3 only.

The `partman` development shell (`nix develop .#partman`, which sets
`PGMQ_REQUIRE_PARTMAN=1` and provides PostgreSQL with pg_partman) is used by EP-1's tests and
EP-3's evidence run. The migration suite prints the server and extension versions from
`SELECT current_setting('server_version') || ' / pg_partman ' || extversion ...`; that line
is the evidence EP-3 records.

Cross-plan decisions that deserve ADR records: the override-lineage boundary (EP-1's new ADR),
and the report-not-repair extension to name collisions and unsupported notifications (EP-2's
design note 018 update, promoted to an ADR only if the retrospective finds it durable beyond
that note).


## Progress

- [ ] EP-1 M1: a red test on the native ledger shows a partitioned queue notifying on every insert on a partition channel
- [ ] EP-1 M2: migration `0007` appended, ledger tests updated, the test green on native and on the stock 1.12 schema, crash and channel tests unchanged
- [ ] EP-1 M3: design notes, capability record, Haddocks, new ADR, and changelog sections written
- [ ] EP-2 M1: red tests for the name collision (dedicated instance) and the partitioned notification (pg_partman) on both backends
- [ ] EP-2 M2: reconciler reports `DetectedQueueNameCollision` and `UnsupportedNotifyOnPartitionedQueue`; both backends and the `-f-effectful` build green
- [ ] EP-2 M3: drift Haddock, contract Haddock, design note 018, user guide, capability record, and changelog sections written
- [ ] EP-3 M1: the four suites create the ephemeral root owner-only; the mode is observed as `700`
- [ ] EP-3 M2: acceptance re-run in the `partman` shell, server version recorded in release evidence and the compatibility ADR
- [ ] EP-3 M3: 0.7.0.0 candidate versions, internal bounds, and changelogs prepared


## Surprises & Discoveries

Document cross-plan insights, dependency changes, scope adjustments, or unexpected
interactions between child plans. Provide concise evidence.

(None yet.)


## Decision Log

- Decision: Fix the partitioned-queue regression inside the override lineage that migration
  `0003` opened, by an append-only migration `0007` that re-creates
  `pgmq.notify_queue_listeners()` with a `pgmq.meta` gate on the fail-open branch. Do not edit
  `0003`, do not add a client-side guard as the fix, and do not resolve the parent partition.
  Rationale: `0003` introduced the defect and its bytes are immutable and MD5-pinned; the FIFO
  override ADR scopes its prohibition to FIFO work and explicitly leaves notification overrides
  to separate work; a client-only guard would not repair the trigger that non-Haskell callers
  and existing native installs run; resolving the parent would add a feature upstream's
  trigger lacks, which belongs upstream. The gate also bounds the mixed-case hazard design
  note 015 already describes (a row keyed `MyQueue` is invisible to the trigger), returning it
  to upstream's "never notify" instead of "notify unthrottled forever".
  Date: 2026-09-16
- Decision: Partitioned queues are documented as receiving no insert notifications, on native
  and stock installs alike, and the reconciler reports a notification declared on one as
  unsupported rather than installing a trigger that cannot deliver on the documented channel.
  Rationale: this is upstream's actual behavior (the throttle `UPDATE` matches zero rows on a
  leaf partition); pretending otherwise is the documentation defect the review found.
  Date: 2026-09-16
- Decision: A declared name that collides with a foreign mixed-case row is reported and not
  created; the collision is reported by one action carrying every colliding row name, and when
  the exact row is missing no sub-resource action runs for that config. Rationale: design note
  018's rule is "reconcile automatically only where the repair is non-destructive; otherwise
  report and stop"; creating over an alias silently reproduces the data hazard of design note
  016. Adding constructors is a breaking change for exhaustive matchers, accepted as 0.5.0.0
  already did for `UpdatedNotifyThrottle` and `DetectedQueueTypeDrift`.
  Date: 2026-09-16
- Decision: The `DetectedQueueTypeDrift` overclaim is fixed by rewording, not by skipping the
  notify, FIFO, and binding steps on a drifted queue. Rationale: those steps are additive,
  each reports its own truthful action, and skipping them would leave a drifted queue's
  declared bindings silently unapplied; the defect is the sentence, not the behavior.
  Date: 2026-09-16
- Decision: Acceptance evidence is re-run on the PostgreSQL the `partman` shell provides today
  (18.6) rather than pinning the shell back to 17.10, and the compatibility ADR names both.
  Rationale: the dependency-bounds ADR's principle is that the pin states what we test; the
  recorded 17.10 evidence predates the toolchain bump and is no longer what a developer runs.
  Date: 2026-09-16
- Decision: Release preparation for 0.7.0.0 is EP-3's last milestone; publication is
  excluded. Rationale: prior initiatives (EP-12) prepared the candidate inside the initiative
  that made the breaking change; the family releases in lockstep and `0007` plus the new
  constructors are the reason for the bump.
  Date: 2026-09-16
- Decision: The five review records `REV-1` through `REV-5` in `docs/reviews/` are the source
  of the findings; this MasterPlan is what they `produced`. The review's own evidence (the
  psql transcript) is reproduced inside EP-1 so the child plan stays self-contained.
  Date: 2026-09-16


## Outcomes & Retrospective

Summarize outcomes, gaps, and lessons learned at major milestones or at completion.
Compare the result against the original vision. Before marking the MasterPlan complete,
distill durable project context from this MasterPlan and its child ExecPlans into
docs/adr/. Keep task-local execution and coordination details here.

(To be filled during and after implementation.)
