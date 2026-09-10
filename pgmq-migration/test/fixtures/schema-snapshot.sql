-- Database-independent catalog keys. Bodies remain separate so only explicit
-- local overrides can be exempted; signatures and defaults are always compared.
SELECT key, value FROM (
  SELECT 'function:' || p.proname || '(' || pg_get_function_identity_arguments(p.oid) || ')' AS key,
    json_build_array(pg_get_function_result(p.oid), pg_get_function_arguments(p.oid),
      l.lanname, p.provolatile, p.proisstrict, p.prosecdef, p.proparallel, p.proconfig)::text AS value
  FROM pg_proc p JOIN pg_namespace n ON n.oid=p.pronamespace
  JOIN pg_language l ON l.oid=p.prolang WHERE n.nspname='pgmq'
  UNION ALL
  SELECT 'body:' || p.proname || '(' || oidvectortypes(p.proargtypes) || ')', p.prosrc
  FROM pg_proc p JOIN pg_namespace n ON n.oid=p.pronamespace WHERE n.nspname='pgmq'
  UNION ALL
  SELECT 'column:' || c.relname || ':' || a.attnum,
    json_build_array(a.attname, format_type(a.atttypid,a.atttypmod), a.attnotnull,
      a.attidentity, a.attgenerated, pg_get_expr(d.adbin,d.adrelid))::text
  FROM pg_class c JOIN pg_namespace n ON n.oid=c.relnamespace
  JOIN pg_attribute a ON a.attrelid=c.oid
  LEFT JOIN pg_attrdef d ON d.adrelid=c.oid AND d.adnum=a.attnum
  WHERE n.nspname='pgmq' AND a.attnum>0 AND NOT a.attisdropped
  UNION ALL
  SELECT 'relation:' || c.relname, json_build_array(c.relkind,c.relpersistence)::text
  FROM pg_class c JOIN pg_namespace n ON n.oid=c.relnamespace WHERE n.nspname='pgmq'
  UNION ALL
  SELECT 'constraint:' || c.relname || ':' || con.conname, pg_get_constraintdef(con.oid)
  FROM pg_constraint con JOIN pg_class c ON c.oid=con.conrelid
  JOIN pg_namespace n ON n.oid=c.relnamespace WHERE n.nspname='pgmq'
) snapshot ORDER BY key
