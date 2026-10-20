-- | The error envelope and the mapping from the library's typed runtime error
-- onto HTTP statuses and codes.
--
-- Every error body is @{"error":{"code":…,"message":…,"details":…}}@. @code@
-- is a stable snake_case identifier a client may switch on; @message@ is a
-- human sentence safe to show verbatim and free to change; @details@ is
-- optional structured context. The vocabulary is listed in
-- @docs/design/021-inspection-surface-wire-contract.md@.
module Pgmq.Inspect.Error
  ( ErrorBody (..),
    inspectionBusy,
    errorResponse,
    jsonResponse,
    notFound,
    methodNotAllowed,
    queueNotFound,
    messageNotFound,
    invalidCursor,
    invalidLimit,
    invalidMessageId,
    invalidRoutingKey,
    websocketUpgradeRequired,
    websocketUnavailable,
    sqlState,
    describeRuntimeError,
    classifyRuntimeError,
  )
where

import Data.Aeson (ToJSON (..), Value, encode, object, (.=))
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hasql.Errors qualified as HasqlErrors
import Network.HTTP.Types (Status, hContentType, status500, status503)
import Network.Wai (Response, responseLBS)
import Pgmq.Effectful (PgmqRuntimeError (..), isTransient)

data ErrorBody = ErrorBody
  { errorCode :: !Text,
    errorMessage :: !Text,
    errorDetails :: !(Maybe Value)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ErrorBody where
  toJSON e =
    object
      [ "error"
          .= object
            ( ["code" .= errorCode e, "message" .= errorMessage e]
                <> maybe [] (\d -> ["details" .= d]) (errorDetails e)
            )
      ]

jsonResponse :: Status -> ByteString -> Response
jsonResponse status = responseLBS status [(hContentType, "application/json")]

errorResponse :: Status -> ErrorBody -> Response
errorResponse status = jsonResponse status . encode

notFound, methodNotAllowed, websocketUpgradeRequired, websocketUnavailable :: ErrorBody
notFound = ErrorBody "not_found" "No route matches this path." Nothing
methodNotAllowed = ErrorBody "method_not_allowed" "Only GET is served." Nothing
websocketUpgradeRequired =
  ErrorBody "websocket_upgrade_required" "This path serves WebSocket upgrades only." Nothing
websocketUnavailable =
  ErrorBody "websocket_unavailable" "The WebSocket feed is not available on this server." Nothing

queueNotFound :: Text -> ErrorBody
queueNotFound name =
  ErrorBody "queue_not_found" "No queue with that name exists." (Just (object ["queue_name" .= name]))

messageNotFound :: Text -> Text -> ErrorBody
messageNotFound name raw =
  ErrorBody
    "message_not_found"
    "No message with that id exists in that table."
    (Just (object ["queue_name" .= name, "msg_id" .= raw]))

invalidCursor, invalidLimit, invalidMessageId, invalidRoutingKey :: Text -> ErrorBody
invalidCursor raw =
  ErrorBody "invalid_cursor" "from must be a non-negative integer msg_id." (Just (object ["from" .= raw]))
invalidLimit raw =
  ErrorBody "invalid_limit" "limit must be a positive integer." (Just (object ["limit" .= raw]))
invalidMessageId raw =
  ErrorBody "invalid_message_id" "msg_id must be a non-negative integer." (Just (object ["msg_id" .= raw]))
invalidRoutingKey raw =
  ErrorBody "invalid_routing_key" "routing_key is not a valid pgmq routing key." (Just (object ["routing_key" .= raw]))

-- | The SQLSTATE the server reported, when the error is a server-reported
-- statement error. Mirrors the pattern 'isTransient' reads.
sqlState :: PgmqRuntimeError -> Maybe Text
sqlState = \case
  PgmqSessionError
    ( HasqlErrors.StatementSessionError
        _
        _
        _
        _
        _
        (HasqlErrors.ServerStatementError (HasqlErrors.ServerError code _ _ _ _))
      ) -> Just code
  PgmqSessionError (HasqlErrors.ScriptSessionError _ (HasqlErrors.ServerError code _ _ _ _)) -> Just code
  _ -> Nothing

-- | Display-safe database failure. Raw driver diagnostics belong in host-side logs.
describeRuntimeError :: PgmqRuntimeError -> (Text, Maybe Value)
describeRuntimeError err =
  ( "The database could not complete the inspection request.",
    fmap (\code -> object ["sqlstate" .= code]) (sqlState err)
  )

-- | Queue identity is established in the handler, not inferred from 42P01 here.
classifyRuntimeError :: PgmqRuntimeError -> (Status, ErrorBody)
classifyRuntimeError err
  | sqlState err == Just "57014" = (status503, ErrorBody "inspection_timeout" "The inspection query exceeded its configured database deadline." details)
  | isTransient err = (status503, ErrorBody "database_unavailable" "The database is temporarily unavailable; retry the request." details)
  | otherwise = (status500, ErrorBody "database_error" message details)
  where
    (message, details) = describeRuntimeError err

-- | The runtime's shared read capacity is occupied; retry later.
inspectionBusy :: ErrorBody
inspectionBusy = ErrorBody "inspection_busy" "Inspection capacity is busy; retry the request." Nothing
