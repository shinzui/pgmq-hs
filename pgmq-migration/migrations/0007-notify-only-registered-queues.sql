-- Notification fail-open only for a registered queue.
--
-- Migration 0003 made pgmq.notify_queue_listeners() fail open when its throttle
-- row is missing, so crash recovery truncating the UNLOGGED throttle table
-- cannot silently stop delivery. That branch could not tell a row lost to a
-- crash from a missing leaf-name row. PostgreSQL clones a row trigger onto
-- every leaf partition, and inside the clone TG_TABLE_NAME is the partition
-- (q_pq_p0), so the extracted name (pq_p0) normally has no throttle row. On a
-- partitioned queue the fail-open therefore fired on every insert, unthrottled,
-- on a per-partition channel no listener on the documented channel receives.
--
-- The fail-open now requires the extracted name to be a queue registered in
-- pgmq.meta. A relation guard before UPDATE rejects leaf partitions even
-- when their extracted names have alias metadata or throttle rows. Ordinary
-- queues keep crash recovery; a leaf partition, or a
-- mixed-case throttle row the trigger cannot match, behaves as upstream's
-- trigger does: no notification. Partitioned queues receive no insert
-- notifications on the hardened native install. Stock SQL is silent for
-- normal partitioned queues but does not guard manually aliased leaves.
--
-- See docs/design/015-notification-delivery-contract.md and
-- docs/adr/notification-override-lineage-and-partitioned-queue-boundary.md.

CREATE OR REPLACE FUNCTION pgmq.notify_queue_listeners()
RETURNS TRIGGER AS $$
DECLARE
  queue_name_extracted TEXT; -- Queue name extracted from trigger table name
  updated_count        INTEGER; -- Number of rows updated (0 or 1)
BEGIN
  -- Metadata names do not prove that this is an ordinary queue relation.
  IF EXISTS (
    SELECT 1 FROM pg_catalog.pg_class c
    WHERE c.oid = TG_RELID AND c.relispartition
  ) THEN
    RETURN NEW;
  END IF;

  queue_name_extracted := substring(TG_TABLE_NAME from 3);

  UPDATE pgmq.notify_insert_throttle
  SET last_notified_at = clock_timestamp()
  WHERE queue_name = queue_name_extracted
    AND (
      throttle_interval_ms = 0 -- No throttling configured
          OR clock_timestamp() - last_notified_at >=
             (throttle_interval_ms * INTERVAL '1 millisecond') -- Throttle interval has elapsed
    );

  -- Check how many rows were updated (will be 0 or 1)
  GET DIAGNOSTICS updated_count = ROW_COUNT;

  IF updated_count > 0 THEN
    PERFORM PG_NOTIFY('pgmq.' || TG_TABLE_NAME || '.' || TG_OP, NULL);
  ELSIF EXISTS (
      SELECT 1 FROM pgmq.meta m
      WHERE m.queue_name = queue_name_extracted
    )
    AND NOT EXISTS (
      SELECT 1 FROM pgmq.notify_insert_throttle nit
      WHERE nit.queue_name = queue_name_extracted
    ) THEN
    -- Fail open only for a queue that could have had a throttle row: the
    -- extracted name is registered in pgmq.meta, so a missing throttle row
    -- can be crash truncation. Both
    -- probes are unique-index lookups and run only when the UPDATE matched
    -- nothing. The row is deliberately NOT re-inserted here (see 0003): the
    -- configured interval is the crash's data loss, and the reconciler
    -- restores it when explicitly invoked, typically at application startup.
    PERFORM PG_NOTIFY('pgmq.' || TG_TABLE_NAME || '.' || TG_OP, NULL);
  END IF;

RETURN NEW;
END;
$$ LANGUAGE plpgsql;
