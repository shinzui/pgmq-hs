# Notification override lineage and partitioned-queue boundary

## Status

Accepted, 2026-10-25; implemented and verified 2026-10-26 in
[ExecPlan 23](../plans/23-gate-the-notification-fail-open-on-a-real-queue-row-and-state-the-partitioned-queue-contract.md)
under [MasterPlan 6](../masterplans/6-close-the-notification-reconciler-and-evidence-gaps-surfaced-by-the-0-6-1-0-review.md).
This record uses the repository's plain-Markdown ADR convention.

## Context

Migration 0003 overrides notification delivery to fail open when a crash truncates the
unlogged throttle table. The trigger derives its queue name from the physical relation;
on a partitioned queue that is a leaf, so the ordinary missing-throttle path can emit
unthrottled notifications on a partition channel instead of the documented parent channel.
The original review evidence is in
[REV-1](../reviews/pgmq-migration-v0-4-0-1-to-v0-6-1-0.md).

A metadata-membership check alone does not establish that the relation is an ordinary
queue. The checked-in `vendor/pgmq/pgmq-extension/sql/pgmq.sql` creates tables with
`IF NOT EXISTS` and inserts caller-supplied queue names into `pgmq.meta`. Regression acceptance
also demonstrates a counterexample: registering a name matching an existing leaf can
give that leaf metadata without changing its physical shape. A throttle row keyed to that
alias can also reach the notification function's UPDATE branch, bypassing a fail-open-only
guard. ExecPlan 23 reproduced both cases before applying the fix.

PostgreSQL exposes physical partition membership as `pg_class.relispartition`; this lets
the trigger inspect the relation identified by `TG_RELID` without finding its parent or
inventing a parent-channel delivery feature. See the
[PostgreSQL catalog reference](https://www.postgresql.org/docs/18/catalog-pg-class.html).

## Decision

Keep migration history immutable. Append 0007 within the notification override lineage:
return immediately for an actual partition relation before reading/updating throttle state,
then retain ordinary-queue throttling and permit missing-row fail-open only for a registered
queue name. Never resolve the parent to deliver partition notifications. Only
`notify_queue_listeners()` is replaced; catalog convergence retains its existing exceptions.

Native partitioned inserts must remain silent even when leaf-name metadata/throttle aliases
exist. Stock supported SQL is silent for normal partitioned queues, but no claim is made
that it rejects manually aliased leaves. The portable consumer contract is that notifications
on a partitioned queue's documented parent channel are unsupported. Reconciliation reports
that limitation and does not enable or update that queue's notification configuration.

Ordinary-queue crash recovery remains best-effort and unthrottled until an explicit
enable/reconcile call restores the lost row. Application startup is a common invocation
point, not an API restriction. A restart is neither necessary nor an automatic repair
if the application does not reconcile the declared notification configuration.

## Consequences and verification

EP-1 owns the migration, native alias cases, normal native/stock partition cases, listener
liveness controls, and ordinary throttle/crash regression checks. Live acceptance on PostgreSQL 18.6 / pg_partman 5.5.0 passed the 12-case serial migration
suite, including a populated six-entry upgrade with unchanged trigger identities and data.
Native hasql/config suites passed 116/70 cases; stock 1.12 normal partition/throttle cases
passed with three explicit native-only hardening skips.
The relation check adds a catalog lookup to each trigger invocation; retain that cost in
the delivery design note instead of describing the new guard as only a zero-update probe.

This does not repair queue-name aliases or change the creation functions. Operators retain
the procedure in [design note 016](../design/016-queue-name-validation.md).
The [compatibility ADR](pgmq-1.12-1.13-compatibility.md) governs immutable payloads and
transactional migration application. The
[FIFO boundary ADR](fifo-native-overrides-and-index-upgrade-boundary.md) prohibits new
overrides for FIFO work; this corrects the separately established notification lineage.

## Alternatives

A membership-only fail-open check misses alias metadata and the successful UPDATE branch.
A client-only check leaves existing native triggers and other clients affected. Rewriting
0003 breaks its published digest. Resolving a partition's parent would add notification
delivery functionality absent from the pinned upstream behavior; that is separate upstream
work for `mori://pgmq/pgmq`, not this correction.
