------------------------------------------------------------
-- FIFO read functions (for pgmq 1.8.0+ and 1.9.0+)
-- Note: last_read_at handling added in pgmq 1.10.0
------------------------------------------------------------

-- read_fifo: reads messages in strict FIFO order
-- This function ensures messages are read in strict FIFO order by:
-- 1. Ordering by msg_id ASC (not just using SKIP LOCKED)
-- 2. Only returning messages that don't skip any earlier pending messages
CREATE FUNCTION pgmq.read_fifo(
    queue_name TEXT,
    vt INTEGER,
    qty INTEGER
)
RETURNS SETOF pgmq.message_record AS $$
DECLARE
    sql TEXT;
    qtable TEXT := pgmq.format_table_name(queue_name, 'q');
BEGIN
    sql := FORMAT(
        $QUERY$
        WITH cte AS
        (
            SELECT msg_id, vt AS current_vt
            FROM pgmq.%I
            ORDER BY msg_id ASC
            LIMIT $1
            FOR UPDATE SKIP LOCKED
        ),
        eligible AS (
            SELECT c.msg_id
            FROM cte c
            WHERE c.msg_id = (
                SELECT MIN(msg_id)
                FROM pgmq.%I
                WHERE vt <= clock_timestamp()
            )
            OR NOT EXISTS (
                SELECT 1
                FROM pgmq.%I q
                WHERE q.msg_id < c.msg_id
                AND q.vt <= clock_timestamp()
                AND q.msg_id NOT IN (SELECT msg_id FROM cte)
            )
        )
        UPDATE pgmq.%I m
        SET
            vt = clock_timestamp() + %L,
            read_ct = read_ct + 1,
            last_read_at = clock_timestamp()
        FROM eligible e
        WHERE m.msg_id = e.msg_id
        AND m.vt <= clock_timestamp()
        RETURNING m.msg_id, m.read_ct, m.enqueued_at, m.last_read_at, m.vt, m.message, m.headers;
        $QUERY$,
        qtable, qtable, qtable, qtable, make_interval(secs => vt)
    );
    RETURN QUERY EXECUTE sql USING qty;
END;
$$ LANGUAGE plpgsql;

-- read_fifo with conditional filter
CREATE FUNCTION pgmq.read_fifo(
    queue_name TEXT,
    vt INTEGER,
    qty INTEGER,
    conditional JSONB
)
RETURNS SETOF pgmq.message_record AS $$
DECLARE
    sql TEXT;
    qtable TEXT := pgmq.format_table_name(queue_name, 'q');
BEGIN
    sql := FORMAT(
        $QUERY$
        WITH cte AS
        (
            SELECT msg_id, vt AS current_vt
            FROM pgmq.%I
            WHERE message @> $2
            ORDER BY msg_id ASC
            LIMIT $1
            FOR UPDATE SKIP LOCKED
        ),
        eligible AS (
            SELECT c.msg_id
            FROM cte c
            WHERE c.msg_id = (
                SELECT MIN(msg_id)
                FROM pgmq.%I
                WHERE vt <= clock_timestamp()
                AND message @> $2
            )
            OR NOT EXISTS (
                SELECT 1
                FROM pgmq.%I q
                WHERE q.msg_id < c.msg_id
                AND q.vt <= clock_timestamp()
                AND q.message @> $2
                AND q.msg_id NOT IN (SELECT msg_id FROM cte)
            )
        )
        UPDATE pgmq.%I m
        SET
            vt = clock_timestamp() + %L,
            read_ct = read_ct + 1,
            last_read_at = clock_timestamp()
        FROM eligible e
        WHERE m.msg_id = e.msg_id
        AND m.vt <= clock_timestamp()
        RETURNING m.msg_id, m.read_ct, m.enqueued_at, m.last_read_at, m.vt, m.message, m.headers;
        $QUERY$,
        qtable, qtable, qtable, qtable, make_interval(secs => vt)
    );
    RETURN QUERY EXECUTE sql USING qty, conditional;
END;
$$ LANGUAGE plpgsql;

-- read_fifo_with_poll: reads messages in strict FIFO order with polling
CREATE FUNCTION pgmq.read_fifo_with_poll(
    queue_name TEXT,
    vt INTEGER,
    qty INTEGER,
    max_poll_seconds INTEGER DEFAULT 5,
    poll_interval_ms INTEGER DEFAULT 100
)
RETURNS SETOF pgmq.message_record AS $$
DECLARE
    r pgmq.message_record;
    stop_at TIMESTAMP;
    sql TEXT;
    qtable TEXT := pgmq.format_table_name(queue_name, 'q');
BEGIN
    stop_at := clock_timestamp() + make_interval(secs => max_poll_seconds);
    LOOP
      IF clock_timestamp() >= stop_at THEN
        RETURN;
      END IF;

      sql := FORMAT(
          $QUERY$
          WITH cte AS
          (
              SELECT msg_id, vt AS current_vt
              FROM pgmq.%I
              ORDER BY msg_id ASC
              LIMIT $1
              FOR UPDATE SKIP LOCKED
          ),
          eligible AS (
              SELECT c.msg_id
              FROM cte c
              WHERE c.msg_id = (
                  SELECT MIN(msg_id)
                  FROM pgmq.%I
                  WHERE vt <= clock_timestamp()
              )
              OR NOT EXISTS (
                  SELECT 1
                  FROM pgmq.%I q
                  WHERE q.msg_id < c.msg_id
                  AND q.vt <= clock_timestamp()
                  AND q.msg_id NOT IN (SELECT msg_id FROM cte)
              )
          )
          UPDATE pgmq.%I m
          SET
              vt = clock_timestamp() + %L,
              read_ct = read_ct + 1,
              last_read_at = clock_timestamp()
          FROM eligible e
          WHERE m.msg_id = e.msg_id
          AND m.vt <= clock_timestamp()
          RETURNING m.msg_id, m.read_ct, m.enqueued_at, m.last_read_at, m.vt, m.message, m.headers;
          $QUERY$,
          qtable, qtable, qtable, qtable, make_interval(secs => vt)
      );

      FOR r IN
        EXECUTE sql USING qty
      LOOP
        RETURN NEXT r;
      END LOOP;
      IF FOUND THEN
        RETURN;
      ELSE
        PERFORM pg_sleep(poll_interval_ms::numeric / 1000);
      END IF;
    END LOOP;
END;
$$ LANGUAGE plpgsql;

-- read_fifo_with_poll with conditional filter
CREATE FUNCTION pgmq.read_fifo_with_poll(
    queue_name TEXT,
    vt INTEGER,
    qty INTEGER,
    conditional JSONB,
    max_poll_seconds INTEGER DEFAULT 5,
    poll_interval_ms INTEGER DEFAULT 100
)
RETURNS SETOF pgmq.message_record AS $$
DECLARE
    r pgmq.message_record;
    stop_at TIMESTAMP;
    sql TEXT;
    qtable TEXT := pgmq.format_table_name(queue_name, 'q');
BEGIN
    stop_at := clock_timestamp() + make_interval(secs => max_poll_seconds);
    LOOP
      IF clock_timestamp() >= stop_at THEN
        RETURN;
      END IF;

      sql := FORMAT(
          $QUERY$
          WITH cte AS
          (
              SELECT msg_id, vt AS current_vt
              FROM pgmq.%I
              WHERE message @> $2
              ORDER BY msg_id ASC
              LIMIT $1
              FOR UPDATE SKIP LOCKED
          ),
          eligible AS (
              SELECT c.msg_id
              FROM cte c
              WHERE c.msg_id = (
                  SELECT MIN(msg_id)
                  FROM pgmq.%I
                  WHERE vt <= clock_timestamp()
                  AND message @> $2
              )
              OR NOT EXISTS (
                  SELECT 1
                  FROM pgmq.%I q
                  WHERE q.msg_id < c.msg_id
                  AND q.vt <= clock_timestamp()
                  AND q.message @> $2
                  AND q.msg_id NOT IN (SELECT msg_id FROM cte)
              )
          )
          UPDATE pgmq.%I m
          SET
              vt = clock_timestamp() + %L,
              read_ct = read_ct + 1,
              last_read_at = clock_timestamp()
          FROM eligible e
          WHERE m.msg_id = e.msg_id
          AND m.vt <= clock_timestamp()
          RETURNING m.msg_id, m.read_ct, m.enqueued_at, m.last_read_at, m.vt, m.message, m.headers;
          $QUERY$,
          qtable, qtable, qtable, qtable, make_interval(secs => vt)
      );

      FOR r IN
        EXECUTE sql USING qty, conditional
      LOOP
        RETURN NEXT r;
      END LOOP;
      IF FOUND THEN
        RETURN;
      ELSE
        PERFORM pg_sleep(poll_interval_ms::numeric / 1000);
      END IF;
    END LOOP;
END;
$$ LANGUAGE plpgsql;

------------------------------------------------------------
-- FIFO index functions (pgmq 1.8.0+)
------------------------------------------------------------

-- _create_fifo_index_if_not_exists
-- internal function to create GIN index on headers for better FIFO performance
CREATE OR REPLACE FUNCTION pgmq._create_fifo_index_if_not_exists(queue_name TEXT)
RETURNS void AS $$
DECLARE
    qtable TEXT := pgmq.format_table_name(queue_name, 'q');
    index_name TEXT := qtable || '_fifo_idx';
BEGIN
    -- Create GIN index on headers for efficient FIFO key lookups
    EXECUTE FORMAT(
        $QUERY$
        CREATE INDEX IF NOT EXISTS %I ON pgmq.%I USING GIN (headers);
        $QUERY$,
        index_name, qtable
    );
END;
$$ LANGUAGE plpgsql;

-- create_fifo_index
-- creates a GIN index on the headers column to improve FIFO read performance
CREATE FUNCTION pgmq.create_fifo_index(queue_name TEXT)
RETURNS void AS $$
BEGIN
    PERFORM pgmq._create_fifo_index_if_not_exists(queue_name);
END;
$$ LANGUAGE plpgsql;

-- create_fifo_indexes_all
-- creates FIFO indexes on all existing queues
CREATE FUNCTION pgmq.create_fifo_indexes_all()
RETURNS void AS $$
DECLARE
    queue_record RECORD;
BEGIN
    FOR queue_record IN SELECT queue_name FROM pgmq.meta LOOP
        PERFORM pgmq.create_fifo_index(queue_record.queue_name);
    END LOOP;
END;
$$ LANGUAGE plpgsql;

------------------------------------------------------------
-- read_grouped functions (pgmq 1.8.0+)
-- AWS SQS FIFO-style batch retrieval behavior
------------------------------------------------------------

-- read_grouped
-- reads messages with AWS SQS FIFO-style batch retrieval behavior
-- attempts to return as many messages as possible from the same message group
CREATE FUNCTION pgmq.read_grouped(
    queue_name TEXT,
    vt INTEGER,
    qty INTEGER
)
RETURNS SETOF pgmq.message_record AS $$
DECLARE
    sql TEXT;
    qtable TEXT := pgmq.format_table_name(queue_name, 'q');
BEGIN
    sql := FORMAT(
        $QUERY$
        WITH fifo_groups AS (
            -- Find the minimum msg_id for each FIFO group that's ready to be processed
            SELECT
                COALESCE(headers->>'x-pgmq-group', '_default_fifo_group') as fifo_key,
                MIN(msg_id) as min_msg_id
            FROM pgmq.%I
            WHERE vt <= clock_timestamp()
            GROUP BY COALESCE(headers->>'x-pgmq-group', '_default_fifo_group')
        ),
        locked_groups AS (
            -- Lock the first available message in each FIFO group
            SELECT
                m.msg_id,
                fg.fifo_key
            FROM pgmq.%I m
            INNER JOIN fifo_groups fg ON
                COALESCE(m.headers->>'x-pgmq-group', '_default_fifo_group') = fg.fifo_key
                AND m.msg_id = fg.min_msg_id
            WHERE m.vt <= clock_timestamp()
            ORDER BY m.msg_id ASC
            FOR UPDATE SKIP LOCKED
        ),
        group_priorities AS (
            -- Assign priority to groups based on their oldest message
            SELECT
                fifo_key,
                msg_id as min_msg_id,
                ROW_NUMBER() OVER (ORDER BY msg_id) as group_priority
            FROM locked_groups
        ),
        available_messages AS (
            -- Get messages prioritizing filling batch from earliest group first
            SELECT
                m.msg_id,
                gp.group_priority,
                ROW_NUMBER() OVER (PARTITION BY gp.fifo_key ORDER BY m.msg_id) as msg_rank_in_group
            FROM pgmq.%I m
            INNER JOIN group_priorities gp ON
                COALESCE(m.headers->>'x-pgmq-group', '_default_fifo_group') = gp.fifo_key
            WHERE m.vt <= clock_timestamp()
            AND m.msg_id >= gp.min_msg_id  -- Only messages from min_msg_id onwards in each group
            AND NOT EXISTS (
                -- Ensure no earlier message in this group is currently being processed
                SELECT 1
                FROM pgmq.%I m2
                WHERE COALESCE(m2.headers->>'x-pgmq-group', '_default_fifo_group') =
                      COALESCE(m.headers->>'x-pgmq-group', '_default_fifo_group')
                AND m2.vt > clock_timestamp()
                AND m2.msg_id < m.msg_id
            )
        ),
        batch_selection AS (
            -- Select messages to fill batch, prioritizing earliest group
            SELECT
                msg_id,
                ROW_NUMBER() OVER (ORDER BY group_priority, msg_rank_in_group) as overall_rank
            FROM available_messages
        ),
        selected_messages AS (
            -- Limit to requested quantity
            SELECT msg_id
            FROM batch_selection
            WHERE overall_rank <= $1
            ORDER BY msg_id
            FOR UPDATE SKIP LOCKED
        )
        UPDATE pgmq.%I m
        SET
            vt = clock_timestamp() + %L,
            read_ct = read_ct + 1,
            last_read_at = clock_timestamp()
        FROM selected_messages sm
        WHERE m.msg_id = sm.msg_id
        RETURNING m.msg_id, m.read_ct, m.enqueued_at, m.last_read_at, m.vt, m.message, m.headers;
        $QUERY$,
        qtable, qtable, qtable, qtable, qtable, make_interval(secs => vt)
    );
    RETURN QUERY EXECUTE sql USING qty;
END;
$$ LANGUAGE plpgsql;

-- read_grouped_with_poll
-- reads messages with AWS SQS FIFO-style batch retrieval behavior, with polling support
CREATE FUNCTION pgmq.read_grouped_with_poll(
    queue_name TEXT,
    vt INTEGER,
    qty INTEGER,
    max_poll_seconds INTEGER DEFAULT 5,
    poll_interval_ms INTEGER DEFAULT 100
)
RETURNS SETOF pgmq.message_record AS $$
DECLARE
    r pgmq.message_record;
    stop_at TIMESTAMP;
BEGIN
    stop_at := clock_timestamp() + make_interval(secs => max_poll_seconds);
    LOOP
      IF (SELECT clock_timestamp() >= stop_at) THEN
        RETURN;
      END IF;

      FOR r IN
        SELECT * FROM pgmq.read_grouped(queue_name, vt, qty)
      LOOP
        RETURN NEXT r;
      END LOOP;
      IF FOUND THEN
        RETURN;
      ELSE
        PERFORM pg_sleep(poll_interval_ms::numeric / 1000);
      END IF;
    END LOOP;
END;
$$ LANGUAGE plpgsql;

------------------------------------------------------------
-- Round-robin FIFO functions (pgmq 1.9.0+)
------------------------------------------------------------

-- read_grouped_rr (round-robin)
-- reads messages while preserving FIFO within groups and interleaving across groups (layered round-robin)
CREATE FUNCTION pgmq.read_grouped_rr(
    queue_name TEXT,
    vt INTEGER,
    qty INTEGER
)
RETURNS SETOF pgmq.message_record AS $$
DECLARE
    sql TEXT;
    qtable TEXT := pgmq.format_table_name(queue_name, 'q');
BEGIN
    sql := FORMAT(
        $QUERY$
        WITH fifo_groups AS (
            -- Determine the absolute head (oldest) message id per FIFO group, regardless of visibility
            SELECT
                COALESCE(headers->>'x-pgmq-group', '_default_fifo_group') AS fifo_key,
                MIN(msg_id) AS head_msg_id
            FROM pgmq.%1$I
            GROUP BY COALESCE(headers->>'x-pgmq-group', '_default_fifo_group')
        ),
        eligible_groups AS (
            -- Only groups whose head message is currently visible
            -- Acquire a transaction-level advisory lock per group to prevent concurrent selection
            SELECT
                g.fifo_key,
                g.head_msg_id,
                ROW_NUMBER() OVER (ORDER BY g.head_msg_id) AS group_priority
            FROM fifo_groups g
            JOIN pgmq.%2$I h ON h.msg_id = g.head_msg_id
            WHERE h.vt <= clock_timestamp()
              AND pg_try_advisory_xact_lock(pg_catalog.hashtextextended(g.fifo_key, 0))
        ),
        available_messages AS (
            -- All currently visible messages starting at the head for each eligible group
            SELECT
                m.msg_id,
                eg.group_priority,
                ROW_NUMBER() OVER (
                    PARTITION BY eg.fifo_key
                    ORDER BY m.msg_id
                ) AS msg_rank_in_group
            FROM pgmq.%3$I m
            JOIN eligible_groups eg
              ON COALESCE(m.headers->>'x-pgmq-group', '_default_fifo_group') = eg.fifo_key
            WHERE m.vt <= clock_timestamp()
              AND m.msg_id >= eg.head_msg_id
        ),
        ordered_messages AS (
            -- Layered round-robin: take rank 1 of all groups by group_priority, then rank 2, etc.
            -- Assign selection order before locking
            SELECT msg_id, ROW_NUMBER() OVER (ORDER BY msg_rank_in_group, group_priority) as selection_order
            FROM available_messages
        ),
        selected_messages AS (
            -- Lock the messages in the correct order, preserving selection_order
            SELECT om.msg_id, om.selection_order
            FROM ordered_messages om
            JOIN pgmq.%4$I m ON m.msg_id = om.msg_id
            WHERE om.selection_order <= $1
            ORDER BY om.selection_order
            FOR UPDATE OF m SKIP LOCKED
        ),
        updated_messages AS (
            UPDATE pgmq.%5$I m
            SET
                vt = clock_timestamp() + %6$L,
                read_ct = read_ct + 1,
                last_read_at = clock_timestamp()
            FROM selected_messages sm
            WHERE m.msg_id = sm.msg_id
              AND m.vt <= clock_timestamp() -- final guard to avoid duplicate reads under races
            RETURNING m.msg_id, m.read_ct, m.enqueued_at, m.last_read_at, m.vt, m.message, m.headers, sm.selection_order
        )
        SELECT msg_id, read_ct, enqueued_at, last_read_at, vt, message, headers
        FROM updated_messages
        ORDER BY selection_order;
        $QUERY$,
        qtable, qtable, qtable, qtable, qtable, make_interval(secs => vt)
    );
    RETURN QUERY EXECUTE sql USING qty;
END;
$$ LANGUAGE plpgsql;

-- read_grouped_rr_with_poll
-- reads messages using round-robin layering across groups, with polling support
CREATE FUNCTION pgmq.read_grouped_rr_with_poll(
    queue_name TEXT,
    vt INTEGER,
    qty INTEGER,
    max_poll_seconds INTEGER DEFAULT 5,
    poll_interval_ms INTEGER DEFAULT 100
)
RETURNS SETOF pgmq.message_record AS $$
DECLARE
    r pgmq.message_record;
    stop_at TIMESTAMP;
BEGIN
    stop_at := clock_timestamp() + make_interval(secs => max_poll_seconds);
    LOOP
      IF (SELECT clock_timestamp() >= stop_at) THEN
        RETURN;
      END IF;

      FOR r IN
        SELECT * FROM pgmq.read_grouped_rr(queue_name, vt, qty)
      LOOP
        RETURN NEXT r;
      END LOOP;
      IF FOUND THEN
        RETURN;
      ELSE
        PERFORM pg_sleep(poll_interval_ms::numeric / 1000);
      END IF;
    END LOOP;
END;
$$ LANGUAGE plpgsql;
