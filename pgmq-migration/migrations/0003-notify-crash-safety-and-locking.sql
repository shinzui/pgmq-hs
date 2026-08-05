-- Notification crash safety, reconcile locking, and partitioned re-entry.
--
-- Three deliberate divergences from upstream pgmq 1.11.0, all re-created with
-- their original signatures so this file is a drop-in replacement:
--
--   1. pgmq.notify_queue_listeners() fails open when its throttle row is
--      missing. pgmq.notify_insert_throttle is UNLOGGED, so crash recovery
--      truncates it; the trigger's UPDATE then matched zero rows and the
--      updated_count > 0 gate suppressed PG_NOTIFY forever, silently starving
--      every listener until an application re-enabled notify. Losing the
--      throttle interval in a crash is acceptable; losing deliveries is not.
--
--   2. pgmq.enable_notify_insert() takes the per-queue advisory lock and
--      coalesces a NULL throttle to the documented 250 ms. Without the lock,
--      two replicas reconciling the same queue at startup both pass the
--      internal DROP TRIGGER IF EXISTS, and the loser then fails with SQLSTATE
--      42710 when it creates a trigger the winner just committed, taking down
--      that replica's whole reconcile. Without the COALESCE, a bound SQL NULL
--      raises 23502: a plpgsql parameter DEFAULT applies only to an omitted
--      argument, never to an explicit NULL.
--
--   3. pgmq.create_partitioned() skips partman.create_parent when the table is
--      already registered in part_config. The advisory lock serializes
--      concurrent creators, but the second replica still calls create_parent on
--      a parent the first just registered — its CREATE TABLE IF NOT EXISTS is a
--      no-op, while create_parent rejects an already-managed parent. Idempotence
--      is the missing property, not locking.
--
-- See docs/plans/14-make-insert-notifications-survive-crashes-and-document-the-channel-contract.md.

CREATE OR REPLACE FUNCTION pgmq.notify_queue_listeners()
RETURNS TRIGGER AS $$
DECLARE
  queue_name_extracted TEXT; -- Queue name extracted from trigger table name
  updated_count        INTEGER; -- Number of rows updated (0 or 1)
BEGIN
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
  ELSIF NOT EXISTS (
    SELECT 1 FROM pgmq.notify_insert_throttle nit
    WHERE nit.queue_name = queue_name_extracted
  ) THEN
    -- Fail open: the trigger exists but its throttle row does not. Zero updated
    -- rows has two causes -- throttled (suppress, correct) and row absent -- and
    -- only this probe tells them apart. The row lives in an UNLOGGED table that
    -- crash recovery truncates, so losing it must not silently stop delivery.
    -- Notify unthrottled until pgmq.enable_notify_insert restores the row.
    --
    -- The row is deliberately NOT re-inserted here: the configured interval is
    -- the crash's data loss, and inventing one in the trigger's hot path would
    -- silently change throttling. The reconciler restores the configured value.
    PERFORM PG_NOTIFY('pgmq.' || TG_TABLE_NAME || '.' || TG_OP, NULL);
  END IF;

RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION pgmq.enable_notify_insert(queue_name TEXT, throttle_interval_ms INTEGER DEFAULT 250)
RETURNS void AS $$
DECLARE
  qtable TEXT := pgmq.format_table_name(queue_name, 'q');
  v_queue_name TEXT := queue_name;
  -- A bound SQL NULL never triggers the parameter DEFAULT above, so the
  -- documented 250 ms has to be applied here for non-Haskell callers.
  v_throttle_interval_ms INTEGER := COALESCE(throttle_interval_ms, 250);
BEGIN
  -- Serialize the whole disable-then-create sequence per queue, exactly as
  -- pgmq.create and pgmq.create_partitioned already do. That makes concurrent
  -- callers convergent: the second one drops and re-creates the first one's
  -- identical trigger, an idempotent no-op in effect, with no error-code
  -- matching anywhere. Accepted side effect: a concurrent second enable resets
  -- last_notified_at to the epoch, exactly as a sequential re-enable does.
  PERFORM pgmq.acquire_queue_lock(queue_name);

  -- Validate that throttle_interval_ms is non-negative
  IF v_throttle_interval_ms < 0 THEN
    RAISE EXCEPTION 'throttle_interval_ms must be non-negative';
  END IF;

  -- Validate that the queue table exists
  IF NOT EXISTS (SELECT 1 FROM information_schema.tables WHERE table_schema = 'pgmq' AND table_name = qtable) THEN
    RAISE EXCEPTION 'Queue "%" does not exist. Create it first using pgmq.create()', v_queue_name;
  END IF;

  PERFORM pgmq.disable_notify_insert(v_queue_name);

  INSERT INTO pgmq.notify_insert_throttle (queue_name, throttle_interval_ms)
  VALUES (v_queue_name, v_throttle_interval_ms)
  ON CONFLICT ON CONSTRAINT notify_insert_throttle_queue_name_key DO UPDATE
      SET throttle_interval_ms = EXCLUDED.throttle_interval_ms,
          last_notified_at = to_timestamp(0);

  EXECUTE FORMAT(
    $QUERY$
    CREATE CONSTRAINT TRIGGER trigger_notify_queue_insert_listeners
    AFTER INSERT ON pgmq.%I
    DEFERRABLE FOR EACH ROW
    EXECUTE PROCEDURE pgmq.notify_queue_listeners()
    $QUERY$,
    qtable
  );
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION pgmq.create_partitioned(
  queue_name TEXT,
  partition_interval TEXT DEFAULT '10000',
  retention_interval TEXT DEFAULT '100000'
)
RETURNS void AS $$
DECLARE
  partition_col TEXT;
  a_partition_col TEXT;
  qtable TEXT := pgmq.format_table_name(queue_name, 'q');
  qtable_seq TEXT := qtable || '_msg_id_seq';
  atable TEXT := pgmq.format_table_name(queue_name, 'a');
  fq_qtable TEXT := 'pgmq.' || qtable;
  fq_atable TEXT := 'pgmq.' || atable;
  l_already_managed BOOLEAN; -- Parent already registered in pg_partman's part_config
BEGIN
  PERFORM pgmq.validate_queue_name(queue_name);
  PERFORM pgmq.acquire_queue_lock(queue_name);
  PERFORM pgmq._ensure_pg_partman_installed();
  SELECT pgmq._get_partition_col(partition_interval) INTO partition_col;

  EXECUTE FORMAT(
    $QUERY$
    CREATE TABLE IF NOT EXISTS pgmq.%I (
        msg_id BIGINT GENERATED ALWAYS AS IDENTITY,
        read_ct INT DEFAULT 0 NOT NULL,
        enqueued_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
        last_read_at TIMESTAMP WITH TIME ZONE,
        vt TIMESTAMP WITH TIME ZONE NOT NULL,
        message JSONB,
        headers JSONB
    ) PARTITION BY RANGE (%I)
    $QUERY$,
    qtable, partition_col
  );

  -- Re-entry guard: the CREATE TABLE above is IF NOT EXISTS, but create_parent
  -- rejects a parent that is already registered, so a second caller would fail
  -- on a queue the first one finished creating.
  EXECUTE FORMAT(
    $QUERY$
    SELECT EXISTS (SELECT 1 FROM %I.part_config WHERE parent_table = %L)
    $QUERY$,
    pgmq._get_pg_partman_schema(),
    fq_qtable
  ) INTO l_already_managed;

  IF NOT l_already_managed THEN
    -- https://github.com/pgpartman/pg_partman/blob/master/doc/pg_partman.md
    -- p_parent_table - the existing parent table. MUST be schema qualified, even if in public schema.
    EXECUTE FORMAT(
      $QUERY$
      SELECT %I.create_parent(
        p_parent_table := %L,
        p_control := %L,
        p_interval := %L,
        p_type := case
          when pgmq._get_pg_partman_major_version() = 5 then 'range'
          else 'native'
        end
      )
      $QUERY$,
      pgmq._get_pg_partman_schema(),
      fq_qtable,
      partition_col,
      partition_interval
    );
  END IF;

  EXECUTE FORMAT(
    $QUERY$
    CREATE INDEX IF NOT EXISTS %I ON pgmq.%I (%I);
    $QUERY$,
    qtable || '_part_idx', qtable, partition_col
  );

  EXECUTE FORMAT(
    $QUERY$
    UPDATE %I.part_config
    SET
        retention = %L,
        retention_keep_table = false,
        retention_keep_index = true,
        automatic_maintenance = 'on'
    WHERE parent_table = %L;
    $QUERY$,
    pgmq._get_pg_partman_schema(),
    retention_interval,
    'pgmq.' || qtable
  );

  EXECUTE FORMAT(
    $QUERY$
    INSERT INTO pgmq.meta (queue_name, is_partitioned, is_unlogged)
    VALUES (%L, true, false)
    ON CONFLICT
    DO NOTHING;
    $QUERY$,
    queue_name
  );

  IF partition_col = 'enqueued_at' THEN
    a_partition_col := 'archived_at';
  ELSE
    a_partition_col := partition_col;
  END IF;

  EXECUTE FORMAT(
    $QUERY$
    CREATE TABLE IF NOT EXISTS pgmq.%I (
      msg_id BIGINT NOT NULL,
      read_ct INT DEFAULT 0 NOT NULL,
      enqueued_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
      last_read_at TIMESTAMP WITH TIME ZONE,
      archived_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
      vt TIMESTAMP WITH TIME ZONE NOT NULL,
      message JSONB,
      headers JSONB
    ) PARTITION BY RANGE (%I);
    $QUERY$,
    atable, a_partition_col
  );

  -- Same re-entry guard for the archive table.
  EXECUTE FORMAT(
    $QUERY$
    SELECT EXISTS (SELECT 1 FROM %I.part_config WHERE parent_table = %L)
    $QUERY$,
    pgmq._get_pg_partman_schema(),
    fq_atable
  ) INTO l_already_managed;

  IF NOT l_already_managed THEN
    -- https://github.com/pgpartman/pg_partman/blob/master/doc/pg_partman.md
    -- p_parent_table - the existing parent table. MUST be schema qualified, even if in public schema.
    EXECUTE FORMAT(
      $QUERY$
      SELECT %I.create_parent(
        p_parent_table := %L,
        p_control := %L,
        p_interval := %L,
        p_type := case
          when pgmq._get_pg_partman_major_version() = 5 then 'range'
          else 'native'
        end
      )
      $QUERY$,
      pgmq._get_pg_partman_schema(),
      fq_atable,
      a_partition_col,
      partition_interval
    );
  END IF;

  EXECUTE FORMAT(
    $QUERY$
    UPDATE %I.part_config
    SET
        retention = %L,
        retention_keep_table = false,
        retention_keep_index = true,
        automatic_maintenance = 'on'
    WHERE parent_table = %L;
    $QUERY$,
    pgmq._get_pg_partman_schema(),
    retention_interval,
    'pgmq.' || atable
  );

  EXECUTE FORMAT(
    $QUERY$
    CREATE INDEX IF NOT EXISTS %I ON pgmq.%I (archived_at);
    $QUERY$,
    'archived_at_idx_' || queue_name, atable
  );

END;
$$ LANGUAGE plpgsql;
