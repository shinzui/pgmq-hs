------------------------------------------------------------
-- Tables: meta and notify_insert_throttle
------------------------------------------------------------

-- Table where queues and metadata about them is stored
CREATE TABLE IF NOT EXISTS pgmq.meta (
    queue_name VARCHAR UNIQUE NOT NULL,
    is_partitioned BOOLEAN NOT NULL,
    is_unlogged BOOLEAN NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
);

-- Table to track notification throttling for queues
CREATE UNLOGGED TABLE IF NOT EXISTS pgmq.notify_insert_throttle (
    queue_name           VARCHAR UNIQUE NOT NULL -- Queue name (without 'q_' prefix)
       CONSTRAINT notify_insert_throttle_meta_queue_name_fk
            REFERENCES pgmq.meta (queue_name)
            ON DELETE CASCADE,
    throttle_interval_ms INTEGER NOT NULL DEFAULT 0, -- Min milliseconds between notifications (0 = no throttling)
    last_notified_at     TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT to_timestamp(0) -- Timestamp of last sent notification
);

CREATE INDEX IF NOT EXISTS idx_notify_throttle_active
    ON pgmq.notify_insert_throttle (queue_name, last_notified_at)
    WHERE throttle_interval_ms > 0;
