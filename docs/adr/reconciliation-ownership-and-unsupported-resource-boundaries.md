# Reconciliation reports ownership ambiguity and unsupported resources before acting

## Status

Accepted, 2026-10-26, implemented by
[ExecPlan 24](../plans/24-report-name-collisions-and-unsupported-notifications-instead-of-acting-on-them.md)
under [MasterPlan 6](../masterplans/6-close-the-notification-reconciler-and-evidence-gaps-surfaced-by-the-0-6-1-0-review.md).

## Context

pgmq stores original registry spelling while lowercasing physical table names. Creating a
lowercase name over a foreign mixed-case row can silently register two owners of the same
messages. Separately, the server accepts notification enabling on partitioned queues even
though the documented parent channel cannot deliver. Additive reconciliation must account
for both ownership ambiguity and effective resource capability before calling a mutator.

## Decision

Use the existing unvalidated queue snapshot to detect every differently cased alias.
Without an exact declared row, report `DetectedQueueNameCollision` and stop that config's
queue creation and sub-resources. With an exact row, report its normal existence/drift action,
then one collision action containing all aliases in ascending lexical order, and reconcile
its declared sub-resources. Other configs proceed. This is snapshot detection, not a lock
against foreign clients; resolving aliases remains an operator procedure.

For notification declarations, use observed shape when a queue exists and declared shape
when creating it. Report `UnsupportedNotifyOnPartitionedQueue` for a partitioned effective
shape without enabling, updating or re-enabling anything. Preserve any foreign throttle row.
Queue-type drift never converts the queue; supported sub-resources still reconcile against
its observed shape. Both adapters share these decisions in the existing core, without new
backend operations or server capability queries.

Reports and their JSON shapes are one interface: every new action receives an explicit
encoder and golden/parity case in the same change. Existing wire shapes remain unchanged.

## Consequences and verification

Exhaustive report matchers must handle two new constructors in the 0.7 family. `NonEmpty Text`
expresses that a collision report always has evidence. Schema-selectable isolated tests
exercise aliases, repeated runs, exact ownership, unrelated queues and both drift directions
through direct and effectful adapters. Stock-selected runs retain dedicated native-only
crash/foreign fixtures; those are not stock-schema evidence.

## Alternatives

Automatically creating an alias reproduces the ownership hazard. Automatically deleting or
renaming foreign rows exceeds startup reconciliation's authority. Checking only declared
shape installs unsupported notifications on observed partitions and suppresses supported
ones on observed ordinary queues. Stopping every sub-resource on type drift would discard
valid additive declarations.

See [the reconciliation contract](../design/018-reconciliation-contract.md),
[operator alias remediation](../design/016-queue-name-validation.md),
[notification boundary](notification-override-lineage-and-partitioned-queue-boundary.md), and
[JSON contract](../design/020-json-wire-encodings.md).
