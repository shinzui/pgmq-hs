-- Run as the actual queue-creating owner, in the database containing pgmq.
-- Review existing worker grants BEFORE removing PUBLIC function execution.
CREATE ROLE pgmq_inspector LOGIN NOSUPERUSER NOCREATEDB NOCREATEROLE NOINHERIT NOREPLICATION NOBYPASSRLS;
ALTER ROLE pgmq_inspector SET default_transaction_read_only = on;
ALTER ROLE pgmq_inspector SET statement_timeout = '2s';
ALTER ROLE pgmq_inspector SET lock_timeout = '250ms';
GRANT USAGE ON SCHEMA pgmq TO pgmq_inspector;
GRANT SELECT ON ALL TABLES IN SCHEMA pgmq TO pgmq_inspector;
GRANT SELECT ON ALL SEQUENCES IN SCHEMA pgmq TO pgmq_inspector;
ALTER DEFAULT PRIVILEGES IN SCHEMA pgmq GRANT SELECT ON TABLES TO pgmq_inspector;
ALTER DEFAULT PRIVILEGES IN SCHEMA pgmq GRANT SELECT ON SEQUENCES TO pgmq_inspector;
REVOKE EXECUTE ON ALL FUNCTIONS IN SCHEMA pgmq FROM PUBLIC;
-- This is owner-wide, across schemas: schema-local REVOKE cannot undo the
-- global default PUBLIC EXECUTE grant. Coordinate this with the owner's apps.
ALTER DEFAULT PRIVILEGES REVOKE EXECUTE ON FUNCTIONS FROM PUBLIC;
GRANT EXECUTE ON FUNCTION
  pgmq.list_queues(), pgmq.format_table_name(text,text),
  pgmq.metrics(text), pgmq.metrics_all(),
  pgmq.list_topic_bindings(), pgmq.list_topic_bindings(text),
  pgmq.test_routing(text), pgmq.validate_routing_key(text),
  pgmq.list_notify_insert_throttles()
TO pgmq_inspector;
