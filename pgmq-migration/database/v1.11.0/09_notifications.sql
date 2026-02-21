------------------------------------------------------------
-- Notification functions
------------------------------------------------------------

CREATE FUNCTION pgmq._should_notify(queue_name TEXT)
RETURNS BOOLEAN AS $$
DECLARE
    should_notify BOOLEAN;
    throttle_ms INTEGER;
    last_notified TIMESTAMP WITH TIME ZONE;
BEGIN
    -- Try to get throttle settings
    SELECT throttle_interval_ms, last_notified_at
    INTO throttle_ms, last_notified
    FROM pgmq.notify_insert_throttle
    WHERE pgmq.notify_insert_throttle.queue_name = _should_notify.queue_name
    FOR UPDATE SKIP LOCKED;

    -- If no row found, assume notifications are enabled without throttling
    IF throttle_ms IS NULL THEN
        RETURN TRUE;
    END IF;

    -- Check if enough time has passed since last notification
    IF clock_timestamp() >= last_notified + make_interval(secs => throttle_ms::numeric / 1000) THEN
        -- Update the last notification time
        UPDATE pgmq.notify_insert_throttle
        SET last_notified_at = clock_timestamp()
        WHERE pgmq.notify_insert_throttle.queue_name = _should_notify.queue_name;
        RETURN TRUE;
    END IF;

    RETURN FALSE;
END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION pgmq._notify_on_insert()
RETURNS TRIGGER AS $$
BEGIN
    IF pgmq._should_notify(TG_ARGV[0]) THEN
        PERFORM pg_notify(TG_ARGV[0], NEW.msg_id::text);
    END IF;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Add trigger to queue table to notify on insert
CREATE FUNCTION pgmq.enable_queue_notifications(queue_name TEXT)
RETURNS void AS $$
#variable_conflict use_column
DECLARE
    qtable TEXT := pgmq.format_table_name(enable_queue_notifications.queue_name, 'q');
    channel TEXT := enable_queue_notifications.queue_name;
    trigger_name TEXT := 'notify_insert_' || qtable;
BEGIN
    -- Create trigger on the queue table
    EXECUTE FORMAT(
        $QUERY$
        CREATE TRIGGER %I
        AFTER INSERT ON pgmq.%I
        FOR EACH ROW
        EXECUTE FUNCTION pgmq._notify_on_insert(%L)
        $QUERY$,
        trigger_name, qtable, channel
    );

    -- Insert throttle tracking row if it doesn't exist
    INSERT INTO pgmq.notify_insert_throttle (queue_name, throttle_interval_ms, last_notified_at)
    VALUES (enable_queue_notifications.queue_name, 0, to_timestamp(0))
    ON CONFLICT (queue_name) DO NOTHING;
END;
$$ LANGUAGE plpgsql;

-- Remove trigger from queue table
CREATE FUNCTION pgmq.disable_queue_notifications(queue_name TEXT)
RETURNS void AS $$
DECLARE
    qtable TEXT := pgmq.format_table_name(queue_name, 'q');
    trigger_name TEXT := 'notify_insert_' || qtable;
BEGIN
    -- Drop trigger if it exists
    EXECUTE FORMAT(
        $QUERY$
        DROP TRIGGER IF EXISTS %I ON pgmq.%I
        $QUERY$,
        trigger_name, qtable
    );

    -- Remove throttle tracking row
    DELETE FROM pgmq.notify_insert_throttle
    WHERE pgmq.notify_insert_throttle.queue_name = disable_queue_notifications.queue_name;
END;
$$ LANGUAGE plpgsql;

-- Set throttle interval for queue notifications
CREATE FUNCTION pgmq.set_notification_throttle(queue_name TEXT, throttle_interval_ms INTEGER)
RETURNS void AS $$
#variable_conflict use_column
BEGIN
    INSERT INTO pgmq.notify_insert_throttle (queue_name, throttle_interval_ms, last_notified_at)
    VALUES (set_notification_throttle.queue_name, set_notification_throttle.throttle_interval_ms, to_timestamp(0))
    ON CONFLICT (queue_name) DO UPDATE
    SET throttle_interval_ms = EXCLUDED.throttle_interval_ms;
END;
$$ LANGUAGE plpgsql;

-- Enable insert notifications (upstream-compatible name)
CREATE FUNCTION pgmq.enable_notify_insert(queue_name TEXT, throttle_interval_ms INTEGER DEFAULT NULL)
RETURNS void AS $$
BEGIN
    PERFORM pgmq.enable_queue_notifications(queue_name);
    IF throttle_interval_ms IS NOT NULL THEN
        PERFORM pgmq.set_notification_throttle(queue_name, throttle_interval_ms);
    END IF;
END;
$$ LANGUAGE plpgsql;

-- Disable insert notifications (upstream-compatible name)
CREATE FUNCTION pgmq.disable_notify_insert(queue_name TEXT)
RETURNS void AS $$
BEGIN
    PERFORM pgmq.disable_queue_notifications(queue_name);
END;
$$ LANGUAGE plpgsql;

-- List all notification insert throttle settings (pgmq 1.11.0+)
CREATE FUNCTION pgmq.list_notify_insert_throttles()
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

-- Update the throttle interval for a queue's insert notifications (pgmq 1.11.0+)
CREATE FUNCTION pgmq.update_notify_insert(queue_name text, throttle_interval_ms integer)
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
