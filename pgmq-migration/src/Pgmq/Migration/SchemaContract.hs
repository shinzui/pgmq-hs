{-# LANGUAGE LambdaCase #-}

-- | Read-only validation of the PGMQ 1.11 database objects consumed by pgmq-hs.
module Pgmq.Migration.SchemaContract
  ( pgmqV1_11StateValidator,
    pgmqV1_11StateEvidenceKey,
  )
where

import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as Text
import Database.PostgreSQL.Migrate
  ( EvidenceKey,
    StateValidator,
    evidenceKey,
    stateValidationError,
    stateValidator,
  )
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Statement (Statement)
import Hasql.Statement qualified as Statement
import Hasql.Transaction qualified as Transaction

-- | Stable evidence key required by the equivalent two-step import route.
pgmqV1_11StateEvidenceKey :: EvidenceKey
pgmqV1_11StateEvidenceKey =
  requireDefinition "PGMQ 1.11 state evidence key" (evidenceKey "pgmq_schema_contract_v1.11")

-- | Validate the PGMQ 1.11 objects used by pgmq-hs without changing database state.
pgmqV1_11StateValidator :: StateValidator
pgmqV1_11StateValidator =
  stateValidator pgmqV1_11StateEvidenceKey $ do
    missing <- Transaction.statement () missingContractObjectsStatement
    pure $ case missing of
      [] ->
        Right
          ( Aeson.object
              [ "contract" Aeson..= ("pgmq-1.11" :: Text),
                "checked_objects" Aeson..= length requiredContractObjects
              ]
          )
      _ ->
        Left
          ( requireDefinition
              "PGMQ 1.11 state validation diagnostic"
              ( stateValidationError
                  ("PGMQ 1.11 schema contract is missing: " <> Text.intercalate ", " missing)
              )
          )

data RequiredObject
  = RequiredSchema !Text
  | RequiredTable !Text
  | RequiredColumn !Text !Text !Text !Bool
  | RequiredConstraint !Text !Text !Char
  | RequiredType !Text
  | RequiredFunction !Text

-- Reviewed against vendor/pgmq/pgmq-extension/sql/pgmq.sql at PGMQ v1.11.0.
-- Function identities use PostgreSQL's regprocedure input notation.
requiredContractObjects :: [RequiredObject]
requiredContractObjects =
  [ RequiredSchema "pgmq",
    RequiredTable "meta",
    RequiredColumn "meta" "queue_name" "character varying" True,
    RequiredColumn "meta" "is_partitioned" "boolean" True,
    RequiredColumn "meta" "is_unlogged" "boolean" True,
    RequiredColumn "meta" "created_at" "timestamp with time zone" True,
    RequiredConstraint "meta" "meta_queue_name_key" 'u',
    RequiredTable "topic_bindings",
    RequiredColumn "topic_bindings" "pattern" "text" True,
    RequiredColumn "topic_bindings" "queue_name" "text" True,
    RequiredColumn "topic_bindings" "bound_at" "timestamp with time zone" True,
    RequiredColumn "topic_bindings" "compiled_regex" "text" False,
    RequiredConstraint "topic_bindings" "topic_bindings_meta_queue_name_fk" 'f',
    RequiredConstraint "topic_bindings" "topic_bindings_unique_pattern_queue" 'u',
    RequiredType "message_record",
    RequiredType "queue_record",
    RequiredType "metrics_result"
  ]
    <> fmap RequiredFunction requiredFunctions

requiredFunctions :: [Text]
requiredFunctions =
  [ "pgmq.archive(text,bigint)",
    "pgmq.archive(text,bigint[])",
    "pgmq.bind_topic(text,text)",
    "pgmq.create(text)",
    "pgmq.create_fifo_index(text)",
    "pgmq.create_fifo_indexes_all()",
    "pgmq.create_partitioned(text,text,text)",
    "pgmq.create_unlogged(text)",
    "pgmq.delete(text,bigint)",
    "pgmq.delete(text,bigint[])",
    "pgmq.detach_archive(text)",
    "pgmq.disable_notify_insert(text)",
    "pgmq.drop_queue(text)",
    "pgmq.enable_notify_insert(text,integer)",
    "pgmq.list_notify_insert_throttles()",
    "pgmq.list_queues()",
    "pgmq.list_topic_bindings()",
    "pgmq.list_topic_bindings(text)",
    "pgmq.metrics(text)",
    "pgmq.metrics_all()",
    "pgmq.pop(text,integer)",
    "pgmq.purge_queue(text)",
    "pgmq.read(text,integer,integer,jsonb)",
    "pgmq.read_grouped(text,integer,integer)",
    "pgmq.read_grouped_rr(text,integer,integer)",
    "pgmq.read_grouped_rr_with_poll(text,integer,integer,integer,integer)",
    "pgmq.read_grouped_with_poll(text,integer,integer,integer,integer)",
    "pgmq.read_with_poll(text,integer,integer,integer,integer,jsonb)",
    "pgmq.send(text,jsonb)",
    "pgmq.send(text,jsonb,integer)",
    "pgmq.send(text,jsonb,timestamp with time zone)",
    "pgmq.send(text,jsonb,jsonb)",
    "pgmq.send(text,jsonb,jsonb,integer)",
    "pgmq.send(text,jsonb,jsonb,timestamp with time zone)",
    "pgmq.send_batch(text,jsonb[])",
    "pgmq.send_batch(text,jsonb[],integer)",
    "pgmq.send_batch(text,jsonb[],timestamp with time zone)",
    "pgmq.send_batch(text,jsonb[],jsonb[])",
    "pgmq.send_batch(text,jsonb[],jsonb[],integer)",
    "pgmq.send_batch(text,jsonb[],jsonb[],timestamp with time zone)",
    "pgmq.send_batch_topic(text,jsonb[])",
    "pgmq.send_batch_topic(text,jsonb[],integer)",
    "pgmq.send_batch_topic(text,jsonb[],timestamp with time zone)",
    "pgmq.send_batch_topic(text,jsonb[],jsonb[])",
    "pgmq.send_batch_topic(text,jsonb[],jsonb[],integer)",
    "pgmq.send_batch_topic(text,jsonb[],jsonb[],timestamp with time zone)",
    "pgmq.send_topic(text,jsonb)",
    "pgmq.send_topic(text,jsonb,integer)",
    "pgmq.send_topic(text,jsonb,jsonb,integer)",
    "pgmq.set_vt(text,bigint,integer)",
    "pgmq.set_vt(text,bigint,timestamp with time zone)",
    "pgmq.set_vt(text,bigint[],integer)",
    "pgmq.set_vt(text,bigint[],timestamp with time zone)",
    "pgmq.test_routing(text)",
    "pgmq.unbind_topic(text,text)",
    "pgmq.update_notify_insert(text,integer)",
    "pgmq.validate_routing_key(text)",
    "pgmq.validate_topic_pattern(text)"
  ]

missingContractObjectsStatement :: Statement () [Text]
missingContractObjectsStatement =
  Statement.preparable
    contractQuery
    Encoders.noParams
    (Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.text)))

contractQuery :: Text
contractQuery =
  "SELECT required.identity\n"
    <> "FROM (VALUES\n  "
    <> Text.intercalate ",\n  " (renderRequiredObject <$> requiredContractObjects)
    <> "\n) AS required(identity, present)\n"
    <> "WHERE NOT required.present\n"
    <> "ORDER BY required.identity"

renderRequiredObject :: RequiredObject -> Text
renderRequiredObject = \case
  RequiredSchema schemaName ->
    row ("schema:" <> schemaName) ("pg_catalog.to_regnamespace(" <> literal schemaName <> ") IS NOT NULL")
  RequiredTable tableName ->
    row
      ("table:pgmq." <> tableName)
      ("pg_catalog.to_regclass(" <> literal ("pgmq." <> tableName) <> ") IS NOT NULL")
  RequiredColumn tableName columnName typeName requiredNotNull ->
    row
      ("column:pgmq." <> tableName <> "." <> columnName)
      ( "EXISTS (SELECT 1 FROM pg_catalog.pg_attribute AS attribute "
          <> "JOIN pg_catalog.pg_class AS relation ON relation.oid = attribute.attrelid "
          <> "JOIN pg_catalog.pg_namespace AS namespace ON namespace.oid = relation.relnamespace "
          <> "WHERE namespace.nspname = 'pgmq' AND relation.relname = "
          <> literal tableName
          <> " AND attribute.attname = "
          <> literal columnName
          <> " AND attribute.attnum > 0 AND NOT attribute.attisdropped "
          <> "AND pg_catalog.format_type(attribute.atttypid, attribute.atttypmod) = "
          <> literal typeName
          <> if requiredNotNull then " AND attribute.attnotnull)" else ")"
      )
  RequiredConstraint tableName constraintName constraintType ->
    row
      ("constraint:pgmq." <> tableName <> "." <> constraintName)
      ( "EXISTS (SELECT 1 FROM pg_catalog.pg_constraint AS constraint_record "
          <> "JOIN pg_catalog.pg_class AS relation ON relation.oid = constraint_record.conrelid "
          <> "JOIN pg_catalog.pg_namespace AS namespace ON namespace.oid = relation.relnamespace "
          <> "WHERE namespace.nspname = 'pgmq' AND relation.relname = "
          <> literal tableName
          <> " AND constraint_record.conname = "
          <> literal constraintName
          <> " AND constraint_record.contype = "
          <> literal (Text.singleton constraintType)
          <> ")"
      )
  RequiredType typeName ->
    row
      ("type:pgmq." <> typeName)
      ("pg_catalog.to_regtype(" <> literal ("pgmq." <> typeName) <> ") IS NOT NULL")
  RequiredFunction functionIdentity ->
    row
      ("function:" <> functionIdentity)
      ("pg_catalog.to_regprocedure(" <> literal functionIdentity <> ") IS NOT NULL")

row :: Text -> Text -> Text
row identity expression = "(" <> literal identity <> ", " <> expression <> ")"

literal :: Text -> Text
literal value = "'" <> Text.replace "'" "''" value <> "'"

requireDefinition :: String -> Either error value -> value
requireDefinition label = either (const (error (label <> " is invalid"))) id
