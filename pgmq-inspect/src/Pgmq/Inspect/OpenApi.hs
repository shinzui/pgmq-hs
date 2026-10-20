-- | OpenAPI 3 HTTP contract. Route coverage and real-response validation live in OpenApiSpec.
module Pgmq.Inspect.OpenApi
  ( openApiDocument,
    openApiJson,
    documentedPaths,
  )
where

import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict.InsOrd.Compat qualified as InsOrd
import Data.OpenApi (AdditionalProperties (..), Components (..), Info (..), MediaTypeObject (..), OpenApi (..), OpenApiItems (..), OpenApiType (..), Operation (..), Param (..), ParamLocation (..), PathItem (..), Reference (..), Referenced (..), Response (..), Responses (..), Schema (..), Server (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Version (showVersion)
import Paths_pgmq_inspect (version)

openApiDocument :: OpenApi
openApiDocument =
  mempty
    { _openApiInfo =
        mempty
          { _infoTitle = "pgmq-inspect",
            _infoVersion = T.pack (showVersion version),
            _infoDescription =
              Just
                "Read-only inspection of pgmq queues. Every path is relative to wherever the \
                \application is mounted. Field names are pgmq's own SQL column names in snake_case; \
                \once published they are never removed or re-typed. The WebSocket feed at ws is \
                \described in docs/user/queue-inspection.md, not here."
          },
      _openApiServers = [Server "." Nothing mempty],
      _openApiPaths = InsOrd.fromList paths,
      _openApiComponents = mempty {_componentsSchemas = InsOrd.fromList schemas}
    }

openApiJson :: LBS.ByteString
openApiJson = encode openApiDocument

documentedPaths :: [FilePath]
documentedPaths = map fst paths

paths :: [(FilePath, PathItem)]
paths =
  [ ("/", getOp "serviceDescriptor" "Service descriptor" [] (ok "ServiceDescriptor") []),
    ("/queues", getOp "listQueues" "List every queue, including names this library would reject" [] (okArray "Queue") []),
    ("/queues/{queue}", getOp "getQueue" "One queue" [queueParam] (ok "Queue") [(404, "queue_not_found")]),
    ("/queues/{queue}/metrics", getOp "getQueueMetrics" "Metrics for one queue" [queueParam] (ok "QueueMetrics") [(404, "queue_not_found")]),
    ("/metrics", getOp "getAllMetrics" "Metrics for every queue; scans rows across queues, subject to the inspection budget" [] (okArray "QueueMetrics") []),
    ("/queues/{queue}/messages", getOp "peekMessages" "Non-destructive keyset page of a queue" [queueParam, fromParam, limitParam] (ok "MessagePage") [(400, "invalid_cursor, invalid_limit"), (404, "queue_not_found")]),
    ("/queues/{queue}/messages/{msg_id}", getOp "lookupMessage" "One message by id" [queueParam, msgIdParam] (ok "Message") [(400, "invalid_message_id"), (404, "queue_not_found, message_not_found")]),
    ("/queues/{queue}/archive", getOp "peekArchive" "Keyset page of a queue's archive" [queueParam, fromParam, limitParam] (ok "ArchivedMessagePage") [(400, "invalid_cursor, invalid_limit"), (404, "queue_not_found")]),
    ("/queues/{queue}/archive/{msg_id}", getOp "lookupArchivedMessage" "One archived message by id" [queueParam, msgIdParam] (ok "ArchivedMessage") [(400, "invalid_message_id"), (404, "queue_not_found, message_not_found")]),
    ("/queues/{queue}/bindings", getOp "listQueueBindings" "Topic bindings of one queue" [queueParam] (okArray "TopicBinding") [(404, "queue_not_found")]),
    ("/bindings", getOp "listBindings" "Every topic binding" [] (okArray "TopicBinding") []),
    ("/routing/test", getOp "testRouting" "Queues a routing key would reach" [routingKeyParam] (okArray "RoutingMatch") [(400, "invalid_routing_key")]),
    ("/notify/throttles", getOp "listNotifyThrottles" "Insert-notification throttle rows" [] (okArray "NotifyInsertThrottle") []),
    ("/health/live", getOp "liveness" "Process liveness" [] (ok "Liveness") []),
    ("/health/ready", readinessOp),
    ("/openapi.json", getOp "openApi" "This document" [] (okAny "The OpenAPI 3 document") [])
  ]

getOp :: Text -> Text -> [Referenced Param] -> Referenced Response -> [(Int, Text)] -> PathItem
getOp opId summary params success errors =
  mempty
    { _pathItemGet =
        Just
          mempty
            { _operationOperationId = Just opId,
              _operationSummary = Just summary,
              _operationParameters = params,
              _operationResponses =
                mempty
                  { _responsesResponses =
                      InsOrd.fromList
                        ( (200, success)
                            : [(status, errorResponse codes) | (status, codes) <- errors]
                              <> [ (405, errorResponse "method_not_allowed"),
                                   (500, errorResponse "database_error"),
                                   (503, errorResponse "database_unavailable, inspection_busy, inspection_timeout")
                                 ]
                        )
                  }
            }
    }

-- Override after getOp so its common 503 cannot overwrite the readiness body.
readinessOp :: PathItem
readinessOp =
  let base = getOp "readiness" "Database readiness" [] (ok "Readiness") []
      patch op =
        op
          { _operationResponses =
              (_operationResponses op)
                { _responsesResponses =
                    InsOrd.insert 503 (ok "Readiness") (_responsesResponses (_operationResponses op))
                }
          }
   in base {_pathItemGet = fmap patch (_pathItemGet base)}

errorResponse :: Text -> Referenced Response
errorResponse codes =
  Inline
    mempty
      { _responseDescription = "Error envelope; code is one of: " <> codes,
        _responseContent = InsOrd.singleton "application/json" (jsonOf (Ref (Reference "ErrorEnvelope")))
      }

ok :: Text -> Referenced Response
ok name = Inline mempty {_responseDescription = "OK", _responseContent = InsOrd.singleton "application/json" (jsonOf (Ref (Reference name)))}

okArray :: Text -> Referenced Response
okArray name = Inline mempty {_responseDescription = "OK", _responseContent = InsOrd.singleton "application/json" (jsonOf (Inline (arrayOf (Ref (Reference name)))))}

okAny :: Text -> Referenced Response
okAny description = Inline mempty {_responseDescription = description, _responseContent = InsOrd.singleton "application/json" (jsonOf (Inline (typed OpenApiObject)))}

jsonOf :: Referenced Schema -> MediaTypeObject
jsonOf s = mempty {_mediaTypeObjectSchema = Just s}

queueParam, msgIdParam, fromParam, limitParam, routingKeyParam :: Referenced Param
queueParam = Inline mempty {_paramName = "queue", _paramIn = ParamPath, _paramRequired = Just True, _paramDescription = Just "Queue name as stored in pgmq.meta; any server-accepted name, not only names this library validates", _paramSchema = Just (Inline (typed OpenApiString))}
msgIdParam = Inline mempty {_paramName = "msg_id", _paramDescription = Just "Non-negative int64 ID, at most 128 decimal digits; browser clients must preserve exact integer tokens", _paramIn = ParamPath, _paramRequired = Just True, _paramSchema = Just (Inline int64)}
fromParam = Inline mempty {_paramName = "from", _paramIn = ParamQuery, _paramRequired = Just False, _paramDescription = Just "Exclusive msg_id cursor, 0..9223372036854775807, at most 128 decimal digits. Echo next_cursor here; moving-view pages are not a transaction snapshot.", _paramSchema = Just (Inline int64)}
limitParam = Inline mempty {_paramName = "limit", _paramIn = ParamQuery, _paramRequired = Just False, _paramDescription = Just "Positive decimal page size (at most 128 digits), capped before narrowing; default 50, cap 500, both host-configurable. Larger values are accepted and capped", _paramSchema = Just (Inline (typed OpenApiInteger) {_schemaMinimum = Just 1})}
routingKeyParam = Inline mempty {_paramName = "routing_key", _paramIn = ParamQuery, _paramRequired = Just True, _paramSchema = Just (Inline (typed OpenApiString))}

typed :: OpenApiType -> Schema
typed t = mempty {_schemaType = Just t, _schemaAdditionalProperties = if t == OpenApiObject then Just (AdditionalPropertiesAllowed True) else Nothing}

nullable :: Schema -> Schema
nullable s = s {_schemaNullable = Just True}

dateTime :: Schema
dateTime = (typed OpenApiString) {_schemaFormat = Just "date-time"}

int64, int32 :: Schema
int64 = (typed OpenApiInteger) {_schemaFormat = Just "int64", _schemaMinimum = Just 0, _schemaMaximum = Just 9223372036854775807}
int32 = (typed OpenApiInteger) {_schemaFormat = Just "int32"}

arrayOf :: Referenced Schema -> Schema
arrayOf item = (typed OpenApiArray) {_schemaItems = Just (OpenApiItemsObject item)}

objectOf :: [(Text, Schema)] -> Schema
objectOf fields =
  (typed OpenApiObject)
    { _schemaProperties = InsOrd.fromList [(name, Inline s) | (name, s) <- fields],
      _schemaRequired = map fst fields -- every key is always present; nullable keys carry null
    }

messageFields :: [(Text, Schema)]
messageFields =
  [ ("msg_id", int64),
    ("read_ct", int64),
    ("enqueued_at", dateTime),
    ("last_read_at", nullable dateTime),
    ("vt", dateTime),
    ("message", mempty {_schemaAdditionalProperties = Just (AdditionalPropertiesAllowed True), _schemaDescription = Just "Any JSON value; a SQL NULL body is JSON null"}),
    ("headers", mempty {_schemaAdditionalProperties = Just (AdditionalPropertiesAllowed True), _schemaDescription = Just "Any JSON value, including null; absent and JSON-null optional headers encode as null"})
  ]

schemas :: [(Text, Schema)]
schemas =
  [ ("Queue", objectOf [("queue_name", typed OpenApiString), ("is_partitioned", typed OpenApiBoolean), ("is_unlogged", typed OpenApiBoolean), ("created_at", dateTime)]),
    ("Message", objectOf messageFields),
    ("ArchivedMessage", objectOf (messageFields <> [("archived_at", dateTime)])),
    ("QueueMetrics", objectOf [("queue_name", typed OpenApiString), ("queue_length", int64 {_schemaDescription = Just "Every message in the queue table, visible or not"}), ("queue_visible_length", int64 {_schemaDescription = Just "Messages whose visibility timeout has expired, i.e. available to consumers now"}), ("newest_msg_age_sec", nullable int32), ("oldest_msg_age_sec", nullable int32), ("total_messages", int64), ("scrape_time", dateTime), ("default_partition_length", nullable int64 {_schemaDescription = Just "PGMQ 1.13 planner estimate of rows in the queue and archive default partitions; null on 1.12 and for ordinary queues"})]),
    ("TopicBinding", objectOf [("pattern", typed OpenApiString), ("queue_name", typed OpenApiString), ("bound_at", dateTime), ("compiled_regex", typed OpenApiString)]),
    ("RoutingMatch", objectOf [("pattern", typed OpenApiString), ("queue_name", typed OpenApiString), ("compiled_regex", typed OpenApiString)]),
    ("NotifyInsertThrottle", objectOf [("queue_name", typed OpenApiString), ("throttle_interval_ms", int32), ("last_notified_at", dateTime)]),
    ("MessagePage", page "Message"),
    ("ArchivedMessagePage", page "ArchivedMessage"),
    ("ErrorBody", (objectOf [("code", typed OpenApiString), ("message", typed OpenApiString)]) {_schemaProperties = InsOrd.fromList [("code", Inline (typed OpenApiString)), ("message", Inline (typed OpenApiString)), ("details", Inline (typed OpenApiObject))], _schemaRequired = ["code", "message"]}),
    ("ErrorEnvelope", (typed OpenApiObject) {_schemaProperties = InsOrd.singleton "error" (Ref (Reference "ErrorBody")), _schemaRequired = ["error"]}),
    ("ServiceDescriptor", objectOf [("service", typed OpenApiString), ("version", typed OpenApiString), ("websocket_path", typed OpenApiString), ("openapi_path", typed OpenApiString)]),
    ("Liveness", objectOf [("alive", typed OpenApiBoolean)]),
    ("Readiness", objectOf [("ready", typed OpenApiBoolean), ("checks", arrayOf (Ref (Reference "DependencyCheck")))]),
    ("DependencyCheck", objectOf [("name", typed OpenApiString), ("healthy", typed OpenApiBoolean), ("latency_ms", nullable int64), ("error", nullable (typed OpenApiString))])
  ]
  where
    page item =
      (typed OpenApiObject)
        { _schemaProperties = InsOrd.fromList [("items", Inline (arrayOf (Ref (Reference item)))), ("next_cursor", Inline int64 {_schemaDescription = Just "msg_id of the last item; absent on the last page"})],
          _schemaRequired = ["items"]
        }
