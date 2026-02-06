------------------------------------------------------------
-- Partitioning support functions
------------------------------------------------------------

CREATE FUNCTION pgmq._get_pg_partman_schema()
RETURNS TEXT AS $$
    SELECT extnamespace::regnamespace::text
    FROM pg_extension
    WHERE extname = 'pg_partman';
$$ LANGUAGE sql STABLE;

CREATE FUNCTION pgmq._get_partition_col(queue_name TEXT)
RETURNS TEXT AS $$
DECLARE
    _partition_col TEXT;
    _table_name TEXT := pgmq.format_table_name(queue_name, 'q');
BEGIN
    SELECT part_config.partition_interval INTO _partition_col
    FROM pg_class
    JOIN pg_namespace ON pg_class.relnamespace = pg_namespace.oid
    JOIN (SELECT parent_table, partition_interval FROM pgmq._get_partman_config()) AS part_config
      ON part_config.parent_table = pg_namespace.nspname || '.' || pg_class.relname
    WHERE pg_class.relname = _table_name
      AND pg_namespace.nspname = 'pgmq';

    RETURN _partition_col;
END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION pgmq._get_partman_config()
RETURNS TABLE (parent_table text, partition_interval text) AS $$
DECLARE
    schema_name TEXT;
BEGIN
    SELECT pgmq._get_pg_partman_schema() INTO schema_name;
    IF schema_name IS NULL THEN
        RETURN QUERY SELECT NULL::text, NULL::text WHERE FALSE;
        RETURN;
    END IF;
    RETURN QUERY EXECUTE FORMAT(
        $QUERY$
        SELECT parent_table, partition_interval
        FROM %I.part_config
        $QUERY$,
        schema_name
    );
END;
$$ LANGUAGE plpgsql;

-- partitioned, with partition interval
-- Note: last_read_at column added (pgmq 1.10.0+)
CREATE FUNCTION pgmq.create_partitioned(
  queue_name TEXT,
  partition_interval TEXT DEFAULT '10000',
  retention_interval TEXT DEFAULT '100000'
)
RETURNS void AS $$
DECLARE
  qtable TEXT := pgmq.format_table_name(queue_name, 'q');
  qtable_seq TEXT := qtable || '_msg_id_seq';
  atable TEXT := pgmq.format_table_name(queue_name, 'a');
  atable_seq TEXT := atable || '_msg_id_seq';
BEGIN
  PERFORM pgmq.validate_queue_name(queue_name);
  PERFORM pgmq.acquire_queue_lock(queue_name);

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
    ) PARTITION BY RANGE (msg_id)
    $QUERY$,
    qtable
  );

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
    ) PARTITION BY RANGE (msg_id);
    $QUERY$,
    atable
  );

  IF NOT pgmq._extension_exists('pg_partman') THEN
    RAISE EXCEPTION 'pg_partman is required for partitioned queues';
  END IF;

  EXECUTE FORMAT(
    $QUERY$
    SELECT %I.create_parent(
        p_parent_table := 'pgmq.%I',
        p_control := 'msg_id',
        p_type := 'range',
        p_interval := %L,
        p_premake := 1
    )
    $QUERY$,
    pgmq._get_pg_partman_schema(), qtable, partition_interval
  );

  EXECUTE FORMAT(
    $QUERY$
    SELECT %I.create_parent(
        p_parent_table := 'pgmq.%I',
        p_control := 'msg_id',
        p_type := 'range',
        p_interval := %L,
        p_premake := 1
    )
    $QUERY$,
    pgmq._get_pg_partman_schema(), atable, partition_interval
  );

  EXECUTE FORMAT(
    $QUERY$
    UPDATE %I.part_config
    SET
        retention = %L,
        retention_keep_table = false,
        retention_keep_index = true,
        automatic_maintenance = 'on'
    WHERE parent_table = 'pgmq.%I';
    $QUERY$,
    pgmq._get_pg_partman_schema(), retention_interval, qtable
  );

  EXECUTE FORMAT(
    $QUERY$
    UPDATE %I.part_config
    SET
        retention = %L,
        retention_keep_table = false,
        retention_keep_index = true,
        automatic_maintenance = 'on'
    WHERE parent_table = 'pgmq.%I';
    $QUERY$,
    pgmq._get_pg_partman_schema(), retention_interval, atable
  );

  EXECUTE FORMAT(
    $QUERY$
    CREATE INDEX IF NOT EXISTS %I ON pgmq.%I (vt ASC);
    $QUERY$,
    qtable || '_vt_idx', qtable
  );

  EXECUTE FORMAT(
    $QUERY$
    CREATE INDEX IF NOT EXISTS %I ON pgmq.%I (archived_at);
    $QUERY$,
    'archived_at_idx_' || queue_name, atable
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

END;
$$ LANGUAGE plpgsql;

-- convert_archive_partitioned
-- Note: last_read_at column added (pgmq 1.10.0+)
CREATE FUNCTION pgmq.convert_archive_partitioned(
  queue_name TEXT,
  partition_interval TEXT DEFAULT '10000',
  retention_interval TEXT DEFAULT '100000',
  leading_partition INT DEFAULT 10
)
RETURNS void AS $$
DECLARE
  atable TEXT := pgmq.format_table_name(queue_name, 'a');
BEGIN
  EXECUTE FORMAT(
    $QUERY$
    ALTER TABLE IF EXISTS pgmq.%I RENAME TO %I
    $QUERY$,
    atable, atable || '_old'
  );

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
    ) PARTITION BY RANGE (msg_id);
    $QUERY$,
    atable
  );

  IF NOT pgmq._extension_exists('pg_partman') THEN
    RAISE EXCEPTION 'pg_partman is required for partitioned queues';
  END IF;

  EXECUTE FORMAT(
    $QUERY$
    SELECT %I.create_parent(
        p_parent_table := 'pgmq.%I',
        p_control := 'msg_id',
        p_type := 'range',
        p_interval := %L,
        p_premake := %L
    )
    $QUERY$,
    pgmq._get_pg_partman_schema(), atable, partition_interval, leading_partition
  );

  EXECUTE FORMAT(
    $QUERY$
    UPDATE %I.part_config
    SET
        retention = %L,
        retention_keep_table = false,
        retention_keep_index = true,
        automatic_maintenance = 'on'
    WHERE parent_table = 'pgmq.%I';
    $QUERY$,
    pgmq._get_pg_partman_schema(), retention_interval, atable
  );

  EXECUTE FORMAT(
    $QUERY$
    INSERT INTO pgmq.%I (msg_id, read_ct, enqueued_at, last_read_at, archived_at, vt, message, headers)
    SELECT msg_id, read_ct, enqueued_at, last_read_at, archived_at, vt, message, headers FROM pgmq.%I
    $QUERY$,
    atable, atable || '_old'
  );

  EXECUTE FORMAT(
    $QUERY$
    CREATE INDEX IF NOT EXISTS %I ON pgmq.%I (archived_at);
    $QUERY$,
    'archived_at_idx_' || queue_name, atable
  );

  EXECUTE FORMAT(
    $QUERY$
    DROP TABLE IF EXISTS pgmq.%I
    $QUERY$,
    atable || '_old'
  );
END;
$$ LANGUAGE plpgsql;
