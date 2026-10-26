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
  revisions:
    - model: "gpt-6-astra"
      harness: "codex-cli"
      at: 2026-10-25T20:33:49Z
      mode: "update"
      note: "Corrected current-tree release scope, shared ownership, dependencies, and integration acceptance across all three children."
    - model: "gpt-6-astra"
      harness: "codex-cli"
      at: 2026-10-26T03:51:08Z
      mode: "update"
      note: "Refresh existing-trigger upgrade acceptance, schema evidence boundaries, and archive ordering before implementation."
  reviews:
    - model: "gpt-6-astra"
      harness: "codex-cli"
      at: 2026-10-25T20:33:49Z
      verdict: "approved"
      note: "Reviewed revised planning document; implementation evidence remains pending. Corrected current-tree release scope, shared ownership, dependencies, and integration acceptance across all three children."
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
evidence disagree. This initiative closes all of them. This initiative adds no new client API
to `pgmq-core`, `pgmq-hasql`, or `pgmq-effectful`; release preparation also carries their
already implemented Unreleased changes and the inspection release handoff.

When the initiative is complete, three things are true that are not true today.

First, enabling insert notifications on a partitioned queue no longer turns every insert into
a `NOTIFY`. Migration `0003` taught the insert trigger to "fail open" when its throttle row is
missing, so that a crash that truncates the `UNLOGGED` throttle table cannot silence delivery.
PostgreSQL clones a row trigger onto every leaf partition and, inside the clone, `TG_TABLE_NAME`
is the partition (`q_pq_p0`), not the queue table (`q_pq`). The name the trigger extracts
(`pq_p0`) normally has no throttle row, so on a partitioned queue the fail-open branch fires on
every insert and publishes on a per-partition channel that `notifyChannelName` never names. The
reviewer reproduced this on the native ledger `0001` through `0006`: three sends produced three
notifications on `pgmq.q_pq_p0.INSERT` and `last_notified_at` never moved, while the same
sequence on an ordinary queue produced one throttled notification. After this initiative an
append-only migration `0007` skips actual partition relations before touching throttle state,
then gates ordinary-queue fail-open on the extracted name having a `pgmq.meta` row. This
restores silence for partitioned queues, including metadata aliases of leaf names, while
keeping crash recovery for ordinary queues; a test proves zero notifications arrive on any
channel for a partitioned queue on both the native and the stock 1.12 schema; and design note
015, the `CAP-4` capability record, and the `notifyChannelName` and `enableNotifyInsert`
Haddocks all say that partitioned queues receive no insert notifications and that the
post-crash unthrottled state lasts until notify is explicitly re-enabled or reconciliation
restores the row, typically at application startup; restart is not required by the API.

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
says the shared ephemeral PostgreSQL root is "a fixed path created `0700`"; the original four test
suites create it with `createDirectoryIfMissing`, which uses the process umask, and only
ephemeral-pg's registry subdirectory is `0700`. The five current creation sites, including `pgmq-inspect`, will create and validate the
root owner-only and tighten an existing directory owned by the effective uid. The
compatibility ADR and the 0.6.0.0 release evidence record acceptance on PostgreSQL 17.10 with
pg_partman 5.4.3, but the `partman` development shell was observed with PostgreSQL 18.6 after the
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

The work splits into three streams by functional concern. The first two are independently
verifiable; the third prepares and accepts their combined release.

The first stream is the server-side notification contract: one migration, the tests that pin
it, and the documents that state it. It lives in `pgmq-migration` (SQL and ledger tests), in `pgmq-config` (crash regression), in
`pgmq-hasql` (the notification test that needs `LISTEN` through libpq and a partitioned
queue), and in the design notes, capability record, and Haddocks that describe delivery.

The second stream is reconciler truthfulness, entirely inside `pgmq-config`: the shared core in
`pgmq-config/src/Pgmq/Config/Reconcile.hs`, the report type in
`pgmq-config/src/Pgmq/Config/Types.hs`, the contract Haddock in `pgmq-config/src/Pgmq/Config.hs`,
design note 018, the user guide, and the config test suites. It is a separate stream because it
is a separate package with a separate release consequence: adding constructors to
`ReconcileAction` is a breaking change under the family's Package Versioning Policy practice,
which the first stream is not.

The third stream is infrastructure and evidence: the five test-support files that create the
ephemeral root, the acceptance run on the current PostgreSQL, the release evidence document,
the compatibility ADR's verification paragraph, and the 0.7.0.0 candidate's versions and
changelogs. The current handoff in `CHANGELOG.md` also requires the initial `pgmq-inspect`
release: six libraries participate, while `pgmq-bench` keeps its own version. Preserve all
existing Unreleased features and announce the already-added effect constructors and JSON
instances alongside this initiative's changes. It comes last because its evidence run must include the first two streams.

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

The local [inspection boundary ADR](../adr/queue-inspection-surface-boundary-and-wire-contract.md)
and [JSON contract](../design/020-json-wire-encodings.md) also apply: EP-2 must encode both
new report constructors and add golden/parity tests; EP-3 includes the inspection release
handoff. No new cross-repository architectural decision is required. The upstream source is `mori://pgmq/pgmq` at tag `v1.13.0`
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

EP-1 and EP-2 have no hard dependency on one another. EP-2 has a soft dependency on
EP-1 only because the Haddock of its new `UnsupportedNotifyOnPartitionedQueue` constructor
cites the statement "partitioned queues receive no insert notifications" that EP-1 writes into
design note 015; if EP-2 lands first, it states the limitation in its own Haddock and guide and links to
design note 015 without editing that EP-1-owned file. Default execution is EP-1, EP-2, EP-3;
shared-document edits are serialized as described below.

EP-3 hard-depends on both. Its acceptance run exists to record the family's behavior on the
current PostgreSQL *after* the migration and the reconciler change, its release evidence
document must list migration `0007`'s digest and the new constructors, and its 0.7.0.0
changelog entry aggregates the "Unreleased" sections the first two plans write. Its owner-only-root milestone could be independent in principle, but this plan deliberately
uses a whole-child dependency: do not start EP-3 until EP-1 and EP-2 are Complete. EP-3
prepares versions and archives before running and recording final candidate acceptance.


## Integration Points

Design note 015 (`docs/design/015-notification-delivery-contract.md`) is owned by EP-1, which
adds the partitioned-queue statement and the explicit re-enable/reconcile recovery boundary. EP-2
links to that statement from its constructor Haddock and from the user guide; it does not
edit the note.

`ReconcileAction` in `pgmq-config/src/Pgmq/Config/Types.hs` is owned by EP-2, which adds
`DetectedQueueNameCollision` and `UnsupportedNotifyOnPartitionedQueue`. EP-3 lists both in the
0.7.0.0 changelog as breaking for exhaustive matchers; nothing else touches the type.

`docs/user/queue-configuration.md` is owned by EP-2 for its Notify Insert, Reconciliation
Functions, and Idempotency Guarantees sections. EP-1 does not edit the guide; the partitioned
notification limitation appears there in EP-2's wording, which must agree with design note
015's.

EP-1 also owns the ordinary throttle/channel regression checks and the nonzero-interval
crash-recovery assertions in `pgmq-config/test/NotifyCrashSpec.hs`; EP-2 runs those assertions
as part of its unchanged-behavior gate.

The migration ledger (`pgmq-migration/migrations/manifest`, the new `0007` file, and the
ledger-derived expectations in `pgmq-migration/test/Main.hs`) is owned by EP-1. EP-3 records
the new file's MD5 in the release evidence and does not change the ledger. EP-1 also owns
the populated six-entry-to-seven-entry upgrade case in the serial migration suite: existing
triggers must pick up the fix without enable/reconcile calls. EP-3 retains that result in
candidate evidence alongside fresh-install acceptance.

Changelogs: EP-1 owns the root `CHANGELOG.md` notification entry and EP-2 appends its
reconciler entry to the same existing Unreleased section, preserving unrelated entries.
Serialize these edits; do not create duplicate headings. Each owns its affected package
entries, including EP-1's `pgmq-effectful` Haddock note. EP-3 converts those sections into the 0.7.0.0
entries and bumps versions. Published sections are never edited.

ADRs: this review establishes, and EP-1 maintains and adds implementation evidence to, `docs/adr/notification-override-lineage-and-partitioned-queue-boundary.md`
in the repository's plain-Markdown convention (there is no profiled `docs/adr` bundle). EP-3
updates the verification paragraph of `docs/adr/pgmq-1.12-1.13-compatibility.md` with the
re-run server version. EP-2 updates design note 018 rather than an ADR, because the
report-not-repair boundary is already recorded there.

The five ephemeral-root creation sites (`pgmq-hasql/test/EphemeralDb.hs`,
`pgmq-effectful/test/EphemeralDb.hs`, `pgmq-config/test/EphemeralDb.hs`, `pgmq-inspect/test/EphemeralDb.hs`, and
`pgmq-migration/test/Main.hs`) are owned by EP-3 only.

The `partman` development shell (`nix develop .#partman`, which sets
`PGMQ_REQUIRE_PARTMAN=1` and provides PostgreSQL with pg_partman) is used by EP-1's tests and
EP-3's evidence run. The migration suite prints the server and extension versions from
`SELECT current_setting('server_version') || ' / pg_partman ' || extversion ...`; that line
is the evidence EP-3 records.

EP-2 owns `reconcileActionFields` and the new golden fixtures in `pgmq-config`, plus the
new rows in design note 020. Existing wire shapes remain unchanged. EP-3 packages and tests
these fixtures. EP-1 and EP-2 both update `docs/capabilities/log.md` and may regenerate its
index: serialize those updates and preserve each other's entries. EP-3 owns all six package
versions, internal bounds (including `pgmq-bench`), final changelogs, and candidate evidence.

Cross-plan decisions that deserve ADR records: the override-lineage boundary (the ADR established by this review and maintained by EP-1),
and the report-not-repair extension to name collisions and unsupported notifications (EP-2's
design note 018 update, promoted to an ADR only if the retrospective finds it durable beyond
that note).


## Progress

All three children are Not Started. EP-1 and EP-2 are eligible; EP-3 is blocked until both
are Complete. Implementation milestones live only in the child plans. Final integration
requires the six-package candidate to pass EP-3's native/stock schema matrix after version
and bound changes, with the new migration (including existing-trigger upgrade acceptance),
report JSON cases, and owner-only root behavior. The pre-implementation refresh was checked
against commit `5cb33cf`; no implementation or runtime acceptance is claimed by this update.
Start with EP-1 M1; retain EP-1 → EP-2 → EP-3 as the default sequence.


## Surprises & Discoveries

Document cross-plan insights, dependency changes, scope adjustments, or unexpected
interactions between child plans. Provide concise evidence.

Review on 2026-10-25 found that the working tree has advanced to 0.6.1.1, contains a fifth
shared-root creator in `pgmq-inspect/test/EphemeralDb.hs`, and requires `pgmq-inspect` in the
next lockstep release. `ReconcileAction` now has a hand-written JSON encoder, so adding only
the constructors would leave an incomplete encoder. The original membership-only SQL guard
also misses leaf-name metadata aliases; EP-1 now checks relation identity before either
notification branch. These findings are reflected in all affected children.

The pre-implementation refresh on 2026-10-25 found that `testPartitions` deliberately stops
at migration `0006`, while the new notification cases primarily described fresh installs.
EP-1 now proves the populated upgrade on existing triggers. Stock-selected config runs also
contain dedicated native-only fixtures, which EP-2 and EP-3 must label accurately. EP-3
formats before archiving and regenerates archives after any packaged-file change.


## Decision Log

- Decision: Fix the partitioned-queue regression inside the override lineage that migration
  `0003` opened, by an append-only migration `0007` that re-creates
  `pgmq.notify_queue_listeners()` with a partition-relation guard before the throttle UPDATE and a `pgmq.meta` gate on
  the fail-open branch (strengthened by the 2026-10-25 review). Do not edit
  `0003`, do not add a client-side guard as the fix, and do not resolve the parent partition.
  Rationale: `0003` introduced the defect and its bytes are immutable and MD5-pinned; the FIFO
  override ADR scopes its prohibition to FIFO work and explicitly leaves notification overrides
  to separate work; a client-only guard would not repair the trigger that non-Haskell callers
  and existing native installs run; resolving the parent would add a feature upstream's
  trigger lacks, which belongs upstream. The gate also bounds the mixed-case hazard design
  note 015 already describes (a row keyed `MyQueue` is invisible to the trigger), returning it
  to upstream's "never notify" instead of "notify unthrottled forever".
  Date: 2026-09-16
- Decision: Normal partitioned queues are documented as receiving no insert notifications, on native
  and stock installs alike (native 0007 additionally rejects leaf-name aliases), and the reconciler reports a notification declared on one as
  unsupported rather than installing a trigger that cannot deliver on the documented channel.
  Rationale: this is upstream's normal-queue behavior (the throttle `UPDATE` matches zero rows on a
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
  (recorded from the running server) rather than pinning the shell back to 17.10, and the compatibility ADR names both.
  Rationale: the dependency-bounds ADR's principle is that the pin states what we test; the
  recorded 17.10 evidence predates the toolchain bump and is no longer what a developer runs.
  Date: 2026-09-16
- Decision: Release preparation precedes final 0.7.0.0 acceptance in EP-3; publication is
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


Revision note (2026-10-25): reviewed against the current tree and corrected the partition
guard, report-codec coverage, shared-file ownership, whole-child ordering, root-upgrade
behavior, six-package release handoff, and candidate-before-evidence sequence. No child has
been implemented by this update.

Pre-implementation refresh (2026-10-25): Added existing-trigger upgrade acceptance, explicit
schema evidence boundaries, and format-before-archive ordering; retained all three children and
their dependencies.
