---
type: Bug Report
title: Partitioned queue notifications bypass the throttle and documented channel
description: A partitioned queue emits a notification per insert on leaf-partition channels instead of the advertised throttled queue channel.
generated:
  by: process:codex
  at: "2026-09-25T04:34:56Z"
bugId: BUG-3
status: reported
severity: degraded
origin: mori://shinzui/keiro-runtime-kenshou/masterplans/1-build-an-extensive-verification-suite-for-the-keiro-runtime
affects: mori://shinzui/pgmq-hs/packages/pgmq-migration
capability: mori://shinzui/pgmq-hs/okf/capabilities/concepts/CAP-4
affectedVersion: "0.6.1.0"
environment: pgmq-migration schema with pg_partman; PostgreSQL 17.11 and 18.6; one thousand separate inserts paced over five seconds.
observed: The enabled partitioned queue emitted one thousand notifications on leaf-partition channels, none on notifyChannelName's parent-queue channel, despite the default 250 ms throttle; disabling notifications silenced the same listener.
expected: The shipped insert-notification capability advertises a configurable throttle and an exact channel computed by notifyChannelName, so enabling a partitioned queue must not create an unthrottled notification storm on other channels.
reproduction:
  - Build the released cohort of mori://shinzui/keiro-runtime-kenshou with pgmq-migration 0.6.1.0 and pg_partman available.
  - Run `cabal run kenshou -- run pgmq/notify/concurrency/partitioned-notify-storm --out runs --dim pg.version=18` with the durable PostgreSQL fixture.
  - Inspect run `01a0d6d6-268b-73bc-b9fe-40b6045586bd`; it records one thousand partition notifications over 4.996 seconds versus an allowance of twenty-one at a 250 ms throttle, and zero after disabling notifications.
  - Repeat on PostgreSQL 17; run `01a0d6d7-5dab-76ea-8315-00fcc28919d3` records the same counts.
workaround: Keep a polling fallback, and do not enable insert notifications on partitioned queues until their trigger behavior is corrected.
reviews:
  - kind: model
    reviewer: process:codex
    reviewed_at: "2026-09-25T04:34:56Z"
    document_timestamp: "2026-09-25T04:34:56Z"
    scope: content-and-metadata
    outcome: commented
    provider: OpenAI
    model: gpt-6-sol
    effort: medium
    context: Reproduced the notification storm on PostgreSQL 17 and 18 and checked count, channel, disable control and the shipped notification capability.
---

# Partitioned queue notifications bypass the throttle and documented channel

The released migration's fail-open trigger sees each partition name rather than the parent queue name. It cannot find that name in the throttle table, so it notifies on every insert using the partition's channel. The source of truth is `mori://shinzui/keiro-runtime-kenshou` at `runs/01a0d6d6-268b-73bc-b9fe-40b6045586bd/` and `runs/01a0d6d7-5dab-76ea-8315-00fcc28919d3/`; artifact-level run URIs are pending. The planned fix is `mori://shinzui/pgmq-hs/plans/23-gate-the-notification-fail-open-on-a-real-queue-row-and-state-the-partitioned-queue-contract`.
