------------------------------------------------------------
-- Message operations: send, read, delete, archive, pop, set_vt
------------------------------------------------------------

-- send: 2 args, no delay
CREATE FUNCTION pgmq.send(
    queue_name TEXT,
    msg JSONB
)
RETURNS SETOF BIGINT AS $$
BEGIN
    RETURN QUERY SELECT * FROM pgmq.send(queue_name, msg, NULL, clock_timestamp());
END;
$$ LANGUAGE plpgsql;

-- send: 3 args with headers
CREATE FUNCTION pgmq.send(
    queue_name TEXT,
    msg JSONB,
    headers JSONB
)
RETURNS SETOF BIGINT AS $$
BEGIN
    RETURN QUERY SELECT * FROM pgmq.send(queue_name, msg, headers, clock_timestamp());
END;
$$ LANGUAGE plpgsql;

-- send: 3 args with delay
CREATE FUNCTION pgmq.send(
    queue_name TEXT,
    msg JSONB,
    delay INTEGER
)
RETURNS SETOF BIGINT AS $$
BEGIN
    RETURN QUERY SELECT * FROM pgmq.send(queue_name, msg, NULL, clock_timestamp() + make_interval(secs => delay));
END;
$$ LANGUAGE plpgsql;

-- send: 4 args with headers and delay
CREATE FUNCTION pgmq.send(
    queue_name TEXT,
    msg JSONB,
    headers JSONB,
    delay INTEGER
)
RETURNS SETOF BIGINT AS $$
BEGIN
    RETURN QUERY SELECT * FROM pgmq.send(queue_name, msg, headers, clock_timestamp() + make_interval(secs => delay));
END;
$$ LANGUAGE plpgsql;

-- send: actual implementation
CREATE FUNCTION pgmq.send(
    queue_name TEXT,
    msg JSONB,
    headers JSONB,
    vt TIMESTAMP WITH TIME ZONE
)
RETURNS SETOF BIGINT AS $$
DECLARE
    sql TEXT;
    qtable TEXT := pgmq.format_table_name(queue_name, 'q');
BEGIN
    sql := FORMAT(
        $QUERY$
        INSERT INTO pgmq.%I (vt, message, headers)
        VALUES ($1, $2, $3)
        RETURNING msg_id;
        $QUERY$,
        qtable
    );
    RETURN QUERY EXECUTE sql USING vt, msg, headers;
END;
$$ LANGUAGE plpgsql;

-- send_batch: no delay
CREATE FUNCTION pgmq.send_batch(
    queue_name TEXT,
    msgs JSONB[]
)
RETURNS SETOF BIGINT AS $$
BEGIN
    RETURN QUERY SELECT * FROM pgmq.send_batch(queue_name, msgs, NULL, clock_timestamp());
END;
$$ LANGUAGE plpgsql;

-- send_batch: with delay
CREATE FUNCTION pgmq.send_batch(
    queue_name TEXT,
    msgs JSONB[],
    delay INTEGER
)
RETURNS SETOF BIGINT AS $$
BEGIN
    RETURN QUERY SELECT * FROM pgmq.send_batch(queue_name, msgs, NULL, clock_timestamp() + make_interval(secs => delay));
END;
$$ LANGUAGE plpgsql;

-- send_batch: with headers
CREATE FUNCTION pgmq.send_batch(
    queue_name TEXT,
    msgs JSONB[],
    headers JSONB[]
)
RETURNS SETOF BIGINT AS $$
BEGIN
    RETURN QUERY SELECT * FROM pgmq.send_batch(queue_name, msgs, headers, clock_timestamp());
END;
$$ LANGUAGE plpgsql;

-- send_batch: with headers and delay
CREATE FUNCTION pgmq.send_batch(
    queue_name TEXT,
    msgs JSONB[],
    headers JSONB[],
    delay INTEGER
)
RETURNS SETOF BIGINT AS $$
BEGIN
    RETURN QUERY SELECT * FROM pgmq.send_batch(queue_name, msgs, headers, clock_timestamp() + make_interval(secs => delay));
END;
$$ LANGUAGE plpgsql;

-- send_batch: actual implementation
CREATE FUNCTION pgmq.send_batch(
    queue_name TEXT,
    msgs JSONB[],
    headers JSONB[],
    vt TIMESTAMP WITH TIME ZONE
)
RETURNS SETOF BIGINT AS $$
DECLARE
    sql TEXT;
    qtable TEXT := pgmq.format_table_name(queue_name, 'q');
BEGIN
    sql := FORMAT(
        $QUERY$
        INSERT INTO pgmq.%I (vt, message, headers)
        SELECT $1, unnest($2), unnest(coalesce($3, array_fill(NULL::jsonb, array[cardinality($2)])))
        RETURNING msg_id;
        $QUERY$,
        qtable
    );
    RETURN QUERY EXECUTE sql USING vt, msgs, headers;
END;
$$ LANGUAGE plpgsql;

---- read
---- reads a number of messages from a queue, setting a visibility timeout on them
CREATE FUNCTION pgmq.read(
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
            SELECT msg_id
            FROM pgmq.%I
            WHERE vt <= clock_timestamp()
            ORDER BY msg_id ASC
            LIMIT $1
            FOR UPDATE SKIP LOCKED
        )
        UPDATE pgmq.%I m
        SET
            vt = clock_timestamp() + %L,
            read_ct = read_ct + 1
        FROM cte
        WHERE m.msg_id = cte.msg_id
        RETURNING m.msg_id, m.read_ct, m.enqueued_at, m.vt, m.message, m.headers;
        $QUERY$,
        qtable, qtable, make_interval(secs => vt)
    );
    RETURN QUERY EXECUTE sql USING qty;
END;
$$ LANGUAGE plpgsql;

---- read
---- reads a number of messages from a queue, setting a visibility timeout on them
CREATE FUNCTION pgmq.read(
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
            SELECT msg_id
            FROM pgmq.%I
            WHERE vt <= clock_timestamp() AND message @> $2
            ORDER BY msg_id ASC
            LIMIT $1
            FOR UPDATE SKIP LOCKED
        )
        UPDATE pgmq.%I m
        SET
            vt = clock_timestamp() + %L,
            read_ct = read_ct + 1
        FROM cte
        WHERE m.msg_id = cte.msg_id
        RETURNING m.msg_id, m.read_ct, m.enqueued_at, m.vt, m.message, m.headers;
        $QUERY$,
        qtable, qtable, make_interval(secs => vt)
    );
    RETURN QUERY EXECUTE sql USING qty, conditional;
END;
$$ LANGUAGE plpgsql;

---- read_with_poll
---- reads a number of messages from a queue, setting a visibility timeout on them
CREATE FUNCTION pgmq.read_with_poll(
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
              SELECT msg_id
              FROM pgmq.%I
              WHERE vt <= clock_timestamp()
              ORDER BY msg_id ASC
              LIMIT $1
              FOR UPDATE SKIP LOCKED
          )
          UPDATE pgmq.%I m
          SET
              vt = clock_timestamp() + %L,
              read_ct = read_ct + 1
          FROM cte
          WHERE m.msg_id = cte.msg_id
          RETURNING m.msg_id, m.read_ct, m.enqueued_at, m.vt, m.message, m.headers;
          $QUERY$,
          qtable, qtable, make_interval(secs => vt)
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

---- read_with_poll
---- reads a number of messages from a queue, setting a visibility timeout on them
CREATE FUNCTION pgmq.read_with_poll(
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
              SELECT msg_id
              FROM pgmq.%I
              WHERE vt <= clock_timestamp() AND message @> $2
              ORDER BY msg_id ASC
              LIMIT $1
              FOR UPDATE SKIP LOCKED
          )
          UPDATE pgmq.%I m
          SET
              vt = clock_timestamp() + %L,
              read_ct = read_ct + 1
          FROM cte
          WHERE m.msg_id = cte.msg_id
          RETURNING m.msg_id, m.read_ct, m.enqueued_at, m.vt, m.message, m.headers;
          $QUERY$,
          qtable, qtable, make_interval(secs => vt)
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

-- delete
-- deletes a message id from the queue permanently
CREATE FUNCTION pgmq.delete(
    queue_name TEXT,
    msg_id BIGINT
)
RETURNS BOOLEAN AS $$
DECLARE
    sql TEXT;
    result BIGINT;
    qtable TEXT := pgmq.format_table_name(queue_name, 'q');
BEGIN
    sql := FORMAT(
        $QUERY$
        DELETE FROM pgmq.%I
        WHERE msg_id = $1
        RETURNING msg_id
        $QUERY$,
        qtable
    );
    EXECUTE sql USING msg_id INTO result;
    RETURN NOT (result IS NULL);
END;
$$ LANGUAGE plpgsql;

-- delete
-- deletes an array of message ids from the queue permanently
CREATE FUNCTION pgmq.delete(
    queue_name TEXT,
    msg_ids BIGINT[]
)
RETURNS SETOF BIGINT AS $$
DECLARE
    sql TEXT;
    qtable TEXT := pgmq.format_table_name(queue_name, 'q');
BEGIN
    sql := FORMAT(
        $QUERY$
        DELETE FROM pgmq.%I
        WHERE msg_id = ANY($1)
        RETURNING msg_id
        $QUERY$,
        qtable
    );
    RETURN QUERY EXECUTE sql USING msg_ids;
END;
$$ LANGUAGE plpgsql;

-- archive
-- removes a message from the queue, and sends it to the archive, where its
-- temporary durability is lost and will be deleted on the next archive expiration
-- job.
CREATE FUNCTION pgmq.archive(
    queue_name TEXT,
    msg_id BIGINT
)
RETURNS BOOLEAN AS $$
DECLARE
    sql TEXT;
    result BIGINT;
    qtable TEXT := pgmq.format_table_name(queue_name, 'q');
    atable TEXT := pgmq.format_table_name(queue_name, 'a');
BEGIN
    sql := FORMAT(
        $QUERY$
        WITH archived AS (
            DELETE FROM pgmq.%I
            WHERE msg_id = $1
            RETURNING msg_id, vt, read_ct, enqueued_at, message, headers
        )
        INSERT INTO pgmq.%I (msg_id, vt, read_ct, enqueued_at, message, headers)
        SELECT msg_id, vt, read_ct, enqueued_at, message, headers
        FROM archived
        RETURNING msg_id;
        $QUERY$,
        qtable, atable
    );
    EXECUTE sql USING msg_id INTO result;
    RETURN NOT (result IS NULL);
END;
$$ LANGUAGE plpgsql;

-- archive
-- removes an array of message ids from the queue, and sends it to the archive,
-- where its temporary durability is lost and will be deleted on the next archive
-- expiration job.
CREATE FUNCTION pgmq.archive(
    queue_name TEXT,
    msg_ids BIGINT[]
)
RETURNS SETOF BIGINT AS $$
DECLARE
    sql TEXT;
    qtable TEXT := pgmq.format_table_name(queue_name, 'q');
    atable TEXT := pgmq.format_table_name(queue_name, 'a');
BEGIN
    sql := FORMAT(
        $QUERY$
        WITH archived AS (
            DELETE FROM pgmq.%I
            WHERE msg_id = ANY($1)
            RETURNING msg_id, vt, read_ct, enqueued_at, message, headers
        )
        INSERT INTO pgmq.%I (msg_id, vt, read_ct, enqueued_at, message, headers)
        SELECT msg_id, vt, read_ct, enqueued_at, message, headers
        FROM archived
        RETURNING msg_id;
        $QUERY$,
        qtable, atable
    );
    RETURN QUERY EXECUTE sql USING msg_ids;
END;
$$ LANGUAGE plpgsql;

-- pop a single message
CREATE FUNCTION pgmq.pop(
    queue_name TEXT
)
RETURNS SETOF pgmq.message_record AS $$
DECLARE
    sql TEXT;
    result pgmq.message_record;
    qtable TEXT := pgmq.format_table_name(queue_name, 'q');
BEGIN
    sql := FORMAT(
        $QUERY$
        WITH cte AS
            (
                SELECT msg_id
                FROM pgmq.%I
                WHERE vt <= clock_timestamp()
                ORDER BY msg_id ASC
                LIMIT 1
                FOR UPDATE SKIP LOCKED
            )
        DELETE from pgmq.%I
        WHERE msg_id = (select msg_id from cte)
        RETURNING *;
        $QUERY$,
        qtable, qtable
    );
    RETURN QUERY EXECUTE sql;
END;
$$ LANGUAGE plpgsql;

-- Sets vt of a message, returns it
CREATE FUNCTION pgmq.set_vt(
    queue_name TEXT,
    msg_id BIGINT,
    vt_offset INTEGER
)
RETURNS SETOF pgmq.message_record AS $$
DECLARE
    sql TEXT;
    result pgmq.message_record;
    qtable TEXT := pgmq.format_table_name(queue_name, 'q');
BEGIN
    sql := FORMAT(
        $QUERY$
        UPDATE pgmq.%I
        SET vt = (clock_timestamp() + %L)
        WHERE msg_id = %L
        RETURNING *;
        $QUERY$,
        qtable, make_interval(secs => vt_offset), msg_id
    );
    RETURN QUERY EXECUTE sql;
END;
$$ LANGUAGE plpgsql;
