------------------------------------------------------------
-- Upgrade migration: pgmq v1.10.0 -> v1.11.0
-- Combines changes from v1.10.0->v1.10.1 and v1.10.1->v1.11.0
------------------------------------------------------------

-- 1. Topic bindings table
CREATE TABLE IF NOT EXISTS pgmq.topic_bindings
(
    pattern        text NOT NULL,
    queue_name     text NOT NULL
        CONSTRAINT topic_bindings_meta_queue_name_fk
            REFERENCES pgmq.meta (queue_name)
            ON DELETE CASCADE,
    bound_at       TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
    compiled_regex text GENERATED ALWAYS AS (
        '^' ||
        replace(
                replace(
                        regexp_replace(pattern, '([.+?{}()|\[\]\\^$])', '\\\1', 'g'),
                        '*', '[^.]+'
                ),
                '#', '.*'
        ) || '$'
        ) STORED,
    CONSTRAINT topic_bindings_unique_pattern_queue UNIQUE (pattern, queue_name)
);

CREATE INDEX IF NOT EXISTS idx_topic_bindings_covering ON pgmq.topic_bindings (pattern) INCLUDE (queue_name, compiled_regex);

-- 2. Topic validation functions
CREATE OR REPLACE FUNCTION pgmq.validate_routing_key(routing_key text)
    RETURNS boolean
    LANGUAGE plpgsql
    IMMUTABLE
AS
$$
BEGIN
    IF routing_key IS NULL OR routing_key = '' THEN
        RAISE EXCEPTION 'routing_key cannot be NULL or empty';
    END IF;

    IF length(routing_key) > 255 THEN
        RAISE EXCEPTION 'routing_key length cannot exceed 255 characters, got % characters', length(routing_key);
    END IF;

    IF routing_key !~ '^[a-zA-Z0-9._-]+$' THEN
        RAISE EXCEPTION 'routing_key contains invalid characters. Only alphanumeric, dots, hyphens, and underscores are allowed. Got: %', routing_key;
    END IF;

    IF routing_key ~ '^\.' THEN
        RAISE EXCEPTION 'routing_key cannot start with a dot. Got: %', routing_key;
    END IF;

    IF routing_key ~ '\.$' THEN
        RAISE EXCEPTION 'routing_key cannot end with a dot. Got: %', routing_key;
    END IF;

    IF routing_key ~ '\.\.' THEN
        RAISE EXCEPTION 'routing_key cannot contain consecutive dots. Got: %', routing_key;
    END IF;

    RETURN true;
END;
$$;

CREATE OR REPLACE FUNCTION pgmq.validate_topic_pattern(pattern text)
    RETURNS boolean
    LANGUAGE plpgsql
    IMMUTABLE
AS
$$
BEGIN
    IF pattern IS NULL OR pattern = '' THEN
        RAISE EXCEPTION 'pattern cannot be NULL or empty';
    END IF;

    IF length(pattern) > 255 THEN
        RAISE EXCEPTION 'pattern length cannot exceed 255 characters, got % characters', length(pattern);
    END IF;

    IF pattern !~ '^[a-zA-Z0-9._\-*#]+$' THEN
        RAISE EXCEPTION 'pattern contains invalid characters. Only alphanumeric, dots, hyphens, underscores, *, and # are allowed. Got: %', pattern;
    END IF;

    IF pattern ~ '^\.' THEN
        RAISE EXCEPTION 'pattern cannot start with a dot. Got: %', pattern;
    END IF;

    IF pattern ~ '\.$' THEN
        RAISE EXCEPTION 'pattern cannot end with a dot. Got: %', pattern;
    END IF;

    IF pattern ~ '\.\.' THEN
        RAISE EXCEPTION 'pattern cannot contain consecutive dots. Got: %', pattern;
    END IF;

    IF pattern ~ '\*\*' THEN
        RAISE EXCEPTION 'pattern cannot contain consecutive stars (**). Use # for multi-segment matching. Got: %', pattern;
    END IF;

    IF pattern ~ '##' THEN
        RAISE EXCEPTION 'pattern cannot contain consecutive hashes (##). A single # already matches zero or more segments. Got: %', pattern;
    END IF;

    IF pattern ~ '\*#' OR pattern ~ '#\*' THEN
        RAISE EXCEPTION 'pattern cannot contain adjacent wildcards (*# or #*). Separate wildcards with dots. Got: %', pattern;
    END IF;

    RETURN true;
END;
$$;

-- 3. Topic binding management functions
CREATE OR REPLACE FUNCTION pgmq.bind_topic(pattern text, queue_name text)
    RETURNS void
    LANGUAGE plpgsql
AS
$$
BEGIN
    PERFORM pgmq.validate_topic_pattern(pattern);
    IF queue_name IS NULL OR queue_name = '' THEN
        RAISE EXCEPTION 'queue_name cannot be NULL or empty';
    END IF;

    IF NOT EXISTS (SELECT 1 FROM pgmq.meta WHERE meta.queue_name = bind_topic.queue_name) THEN
        RAISE EXCEPTION 'Queue "%" does not exist. Create the queue first using pgmq.create()', queue_name;
    END IF;

    INSERT INTO pgmq.topic_bindings (pattern, queue_name)
    VALUES (pattern, queue_name)
    ON CONFLICT ON CONSTRAINT topic_bindings_unique_pattern_queue DO NOTHING;
END;
$$;

CREATE OR REPLACE FUNCTION pgmq.unbind_topic(pattern text, queue_name text)
    RETURNS boolean
    LANGUAGE plpgsql
AS
$$
DECLARE
    rows_deleted integer;
BEGIN
    IF pattern IS NULL OR pattern = '' THEN
        RAISE EXCEPTION 'pattern cannot be NULL or empty';
    END IF;

    IF queue_name IS NULL OR queue_name = '' THEN
        RAISE EXCEPTION 'queue_name cannot be NULL or empty';
    END IF;

    DELETE
    FROM pgmq.topic_bindings
    WHERE topic_bindings.pattern = unbind_topic.pattern
      AND topic_bindings.queue_name = unbind_topic.queue_name;

    GET DIAGNOSTICS rows_deleted = ROW_COUNT;

    IF rows_deleted > 0 THEN
        RETURN true;
    ELSE
        RETURN false;
    END IF;
END;
$$;

CREATE OR REPLACE FUNCTION pgmq.test_routing(routing_key text)
    RETURNS TABLE
            (
                pattern        text,
                queue_name     text,
                compiled_regex text
            )
    LANGUAGE plpgsql
    STABLE
AS
$$
BEGIN
    PERFORM pgmq.validate_routing_key(routing_key);
    RETURN QUERY
        SELECT b.pattern,
               b.queue_name,
               b.compiled_regex
        FROM pgmq.topic_bindings b
        WHERE routing_key ~ b.compiled_regex
        ORDER BY b.pattern;
END;
$$;

-- 4. Topic send functions
CREATE OR REPLACE FUNCTION pgmq.send_topic(routing_key text, msg jsonb, headers jsonb, delay integer)
    RETURNS integer
    LANGUAGE plpgsql
    VOLATILE
AS
$$
DECLARE
    b             RECORD;
    matched_count integer := 0;
BEGIN
    PERFORM pgmq.validate_routing_key(routing_key);

    IF msg IS NULL THEN
        RAISE EXCEPTION 'msg cannot be NULL';
    END IF;

    IF delay < 0 THEN
        RAISE EXCEPTION 'delay cannot be negative, got: %', delay;
    END IF;

    FOR b IN
        SELECT DISTINCT tb.queue_name
        FROM pgmq.topic_bindings tb
        WHERE routing_key ~ tb.compiled_regex
        ORDER BY tb.queue_name
        LOOP
            PERFORM pgmq.send(b.queue_name, msg, headers, delay);
            matched_count := matched_count + 1;
        END LOOP;

    RETURN matched_count;
END;
$$;

CREATE OR REPLACE FUNCTION pgmq.send_topic(routing_key text, msg jsonb)
    RETURNS integer
    LANGUAGE plpgsql
    VOLATILE
AS
$$
BEGIN
    RETURN pgmq.send_topic(routing_key, msg, NULL, 0);
END;
$$;

CREATE OR REPLACE FUNCTION pgmq.send_topic(routing_key text, msg jsonb, delay integer)
    RETURNS integer
    LANGUAGE plpgsql
    VOLATILE
AS
$$
BEGIN
    RETURN pgmq.send_topic(routing_key, msg, NULL, delay);
END;
$$;

-- 5. Topic listing functions
CREATE OR REPLACE FUNCTION pgmq.list_topic_bindings()
    RETURNS TABLE
            (
                pattern        text,
                queue_name     text,
                bound_at       TIMESTAMP WITH TIME ZONE,
                compiled_regex text
            )
    LANGUAGE sql
    STABLE
AS
$$
    SELECT pattern, queue_name, bound_at, compiled_regex
    FROM pgmq.topic_bindings
    ORDER BY bound_at DESC, pattern, queue_name;
$$;

CREATE OR REPLACE FUNCTION pgmq.list_topic_bindings(queue_name text)
    RETURNS TABLE
            (
                pattern        text,
                queue_name     text,
                bound_at       TIMESTAMP WITH TIME ZONE,
                compiled_regex text
            )
    LANGUAGE sql
    STABLE
AS
$$
    SELECT pattern, tb.queue_name, bound_at, compiled_regex
    FROM pgmq.topic_bindings tb
    WHERE tb.queue_name = list_topic_bindings.queue_name
    ORDER BY bound_at DESC, pattern;
$$;

-- 6. Notification management functions
CREATE OR REPLACE FUNCTION pgmq.list_notify_insert_throttles()
    RETURNS TABLE
            (
                queue_name           text,
                throttle_interval_ms integer,
                last_notified_at     TIMESTAMP WITH TIME ZONE
            )
    LANGUAGE sql
    STABLE
AS
$$
    SELECT queue_name, throttle_interval_ms, last_notified_at
    FROM pgmq.notify_insert_throttle
    ORDER BY queue_name;
$$;

CREATE OR REPLACE FUNCTION pgmq.update_notify_insert(queue_name text, throttle_interval_ms integer)
    RETURNS void
    LANGUAGE plpgsql
AS
$$
BEGIN
    IF throttle_interval_ms < 0 THEN
        RAISE EXCEPTION 'throttle_interval_ms must be non-negative, got: %', throttle_interval_ms;
    END IF;

    IF NOT EXISTS (SELECT 1 FROM pgmq.meta WHERE meta.queue_name = update_notify_insert.queue_name) THEN
        RAISE EXCEPTION 'Queue "%" does not exist. Create the queue first using pgmq.create()', queue_name;
    END IF;

    IF NOT EXISTS (SELECT 1 FROM pgmq.notify_insert_throttle WHERE notify_insert_throttle.queue_name = update_notify_insert.queue_name) THEN
        RAISE EXCEPTION 'Queue "%" does not have notify_insert enabled. Enable it first using pgmq.enable_notify_insert()', queue_name;
    END IF;

    UPDATE pgmq.notify_insert_throttle
    SET throttle_interval_ms = update_notify_insert.throttle_interval_ms,
        last_notified_at = to_timestamp(0)
    WHERE notify_insert_throttle.queue_name = update_notify_insert.queue_name;
END;
$$;

-- 7. Batch validation helpers
CREATE OR REPLACE FUNCTION pgmq._validate_batch_params(
    msgs JSONB[],
    headers JSONB[]
) RETURNS void AS $$
BEGIN
    IF msgs IS NULL OR array_length(msgs, 1) IS NULL THEN
        RAISE EXCEPTION 'msgs cannot be NULL or empty';
    END IF;

    IF headers IS NOT NULL AND COALESCE(array_length(headers, 1), 0) != COALESCE(array_length(msgs, 1), 0) THEN
        RAISE EXCEPTION 'headers array length (%) must match msgs array length (%)',
            COALESCE(array_length(headers, 1), 0), COALESCE(array_length(msgs, 1), 0);
    END IF;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION pgmq._send_batch(
    queue_name TEXT,
    msgs JSONB[],
    headers JSONB[],
    delay TIMESTAMP WITH TIME ZONE
) RETURNS SETOF BIGINT AS $$
DECLARE
    sql TEXT;
    qtable TEXT := pgmq.format_table_name(queue_name, 'q');
BEGIN
    sql := FORMAT(
            $QUERY$
        INSERT INTO pgmq.%I (vt, message, headers)
        SELECT $2, unnest($1), unnest(coalesce($3, ARRAY[]::jsonb[]))
        RETURNING msg_id;
        $QUERY$,
            qtable
           );
    RETURN QUERY EXECUTE sql USING msgs, delay, headers;
END;
$$ LANGUAGE plpgsql;

-- Update send_batch to use validation
-- Must drop first because parameter name changed from 'vt' to 'delay'
DROP FUNCTION IF EXISTS pgmq.send_batch(text, jsonb[], jsonb[], timestamp with time zone);
CREATE OR REPLACE FUNCTION pgmq.send_batch(
    queue_name TEXT,
    msgs JSONB[],
    headers JSONB[],
    delay TIMESTAMP WITH TIME ZONE
) RETURNS SETOF BIGINT AS $$
BEGIN
    PERFORM pgmq._validate_batch_params(msgs, headers);
    RETURN QUERY SELECT * FROM pgmq._send_batch(queue_name, msgs, headers, delay);
END;
$$ LANGUAGE plpgsql;

-- 8. Batch topic send functions
CREATE OR REPLACE FUNCTION pgmq.send_batch_topic(
    routing_key text,
    msgs jsonb[],
    headers jsonb[],
    delay TIMESTAMP WITH TIME ZONE
)
    RETURNS TABLE(queue_name text, msg_id bigint)
    LANGUAGE plpgsql
    VOLATILE
AS
$$
DECLARE
    b RECORD;
BEGIN
    PERFORM pgmq.validate_routing_key(routing_key);
    PERFORM pgmq._validate_batch_params(msgs, headers);

    FOR b IN
        SELECT DISTINCT tb.queue_name
        FROM pgmq.topic_bindings tb
        WHERE routing_key ~ tb.compiled_regex
        ORDER BY tb.queue_name
        LOOP
            RETURN QUERY
            SELECT b.queue_name, batch_result.msg_id
            FROM pgmq._send_batch(b.queue_name, msgs, headers, delay) AS batch_result(msg_id);
        END LOOP;

    RETURN;
END;
$$;

CREATE OR REPLACE FUNCTION pgmq.send_batch_topic(
    routing_key text,
    msgs jsonb[]
)
    RETURNS TABLE(queue_name text, msg_id bigint)
    LANGUAGE sql
    VOLATILE
AS
$$
    SELECT * FROM pgmq.send_batch_topic(routing_key, msgs, NULL, clock_timestamp());
$$;

CREATE OR REPLACE FUNCTION pgmq.send_batch_topic(
    routing_key text,
    msgs jsonb[],
    headers jsonb[]
)
    RETURNS TABLE(queue_name text, msg_id bigint)
    LANGUAGE sql
    VOLATILE
AS
$$
    SELECT * FROM pgmq.send_batch_topic(routing_key, msgs, headers, clock_timestamp());
$$;

CREATE OR REPLACE FUNCTION pgmq.send_batch_topic(
    routing_key text,
    msgs jsonb[],
    delay integer
)
    RETURNS TABLE(queue_name text, msg_id bigint)
    LANGUAGE sql
    VOLATILE
AS
$$
    SELECT * FROM pgmq.send_batch_topic(routing_key, msgs, NULL, clock_timestamp() + make_interval(secs => delay));
$$;

CREATE OR REPLACE FUNCTION pgmq.send_batch_topic(
    routing_key text,
    msgs jsonb[],
    delay TIMESTAMP WITH TIME ZONE
)
    RETURNS TABLE(queue_name text, msg_id bigint)
    LANGUAGE sql
    VOLATILE
AS
$$
    SELECT * FROM pgmq.send_batch_topic(routing_key, msgs, NULL, delay);
$$;

CREATE OR REPLACE FUNCTION pgmq.send_batch_topic(
    routing_key text,
    msgs jsonb[],
    headers jsonb[],
    delay integer
)
    RETURNS TABLE(queue_name text, msg_id bigint)
    LANGUAGE sql
    VOLATILE
AS
$$
    SELECT * FROM pgmq.send_batch_topic(routing_key, msgs, headers, clock_timestamp() + make_interval(secs => delay));
$$;
