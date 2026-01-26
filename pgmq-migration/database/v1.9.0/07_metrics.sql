------------------------------------------------------------
-- Metrics functions
------------------------------------------------------------

CREATE FUNCTION pgmq.metrics(queue_name TEXT)
RETURNS pgmq.metrics_result AS $$
DECLARE
    result_row pgmq.metrics_result;
    query TEXT;
    qtable TEXT := pgmq.format_table_name(queue_name, 'q');
BEGIN
    query := FORMAT(
        $QUERY$
        WITH q_summary AS (
            SELECT
                count(*) as queue_length,
                EXTRACT(epoch FROM (NOW() - max(enqueued_at)))::int as newest_msg_age_sec,
                EXTRACT(epoch FROM (NOW() - min(enqueued_at)))::int as oldest_msg_age_sec,
                NOW() as scrape_time,
                count(*) FILTER (WHERE vt <= NOW()) AS queue_visible_length
            FROM pgmq.%I
        ),
        all_metrics AS (
            SELECT CASE
                WHEN is_partitioned THEN
                    %L || '_part_' || pgmq._get_partition_col(%L)
                ELSE
                    %L
            END as queue_name,
            *
            FROM pgmq.meta
            WHERE queue_name = %L
        )
        SELECT
            m.queue_name,
            q_summary.queue_length,
            q_summary.newest_msg_age_sec,
            q_summary.oldest_msg_age_sec,
            pgmq._get_pg_stat_get_xact_tuples_inserted(%L)::bigint,
            q_summary.scrape_time,
            q_summary.queue_visible_length
        FROM q_summary
        CROSS JOIN all_metrics m
        $QUERY$,
        qtable, queue_name, queue_name, queue_name, queue_name, 'pgmq.' || qtable
    );
    EXECUTE query INTO result_row;
    RETURN result_row;
END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION pgmq.metrics_all()
RETURNS SETOF pgmq.metrics_result AS $$
DECLARE
    row_name RECORD;
    result_row pgmq.metrics_result;
BEGIN
    FOR row_name IN SELECT queue_name FROM pgmq.meta LOOP
        result_row := pgmq.metrics(row_name.queue_name);
        RETURN NEXT result_row;
    END LOOP;
END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION pgmq._get_pg_stat_get_xact_tuples_inserted(table_name TEXT)
RETURNS bigint AS $$
DECLARE
    result bigint;
BEGIN
    SELECT pg_stat_get_xact_tuples_inserted(oid)
    INTO result
    FROM pg_class
    WHERE relname = table_name;

    IF result IS NULL THEN
        SELECT pg_stat_get_xact_tuples_inserted(oid)
        INTO result
        FROM pg_class
        WHERE relname = SUBSTRING(table_name FROM 6);  -- Remove 'pgmq.' prefix (5 chars + 1 for the dot)
    END IF;

    RETURN COALESCE(result, 0);
END;
$$ LANGUAGE plpgsql;
