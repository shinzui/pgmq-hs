------------------------------------------------------------
-- FIFO read functions (for pgmq 1.8.0+ and 1.9.0+)
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
            read_ct = read_ct + 1
        FROM eligible e
        WHERE m.msg_id = e.msg_id
        AND m.vt <= clock_timestamp()
        RETURNING m.msg_id, m.read_ct, m.enqueued_at, m.vt, m.message, m.headers;
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
            read_ct = read_ct + 1
        FROM eligible e
        WHERE m.msg_id = e.msg_id
        AND m.vt <= clock_timestamp()
        RETURNING m.msg_id, m.read_ct, m.enqueued_at, m.vt, m.message, m.headers;
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
              read_ct = read_ct + 1
          FROM eligible e
          WHERE m.msg_id = e.msg_id
          AND m.vt <= clock_timestamp()
          RETURNING m.msg_id, m.read_ct, m.enqueued_at, m.vt, m.message, m.headers;
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
              read_ct = read_ct + 1
          FROM eligible e
          WHERE m.msg_id = e.msg_id
          AND m.vt <= clock_timestamp()
          RETURNING m.msg_id, m.read_ct, m.enqueued_at, m.vt, m.message, m.headers;
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
