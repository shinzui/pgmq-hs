-- | Internal protocol representation. Public constructors are re-exported by WebSocket.
module Pgmq.Inspect.Frames (ClientFrame (..), ServerFrame (..), UpdateSource (..)) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Pgmq.Hasql.Statements.Types (QueueMetrics)

data ClientFrame = Subscribe !Text | Unsubscribe !Text | Ping
  deriving stock (Eq, Show)

data UpdateSource = NotifySource | PollSource
  deriving stock (Eq, Show)

data ServerFrame
  = Snapshot !Text !QueueMetrics !Bool
  | Update !Text !QueueMetrics !UpdateSource
  | Pong
  | ErrorFrame !Text !Text !(Maybe Text)
  | Goodbye
  deriving stock (Show)

instance FromJSON ClientFrame where
  parseJSON = withObject "ClientFrame" $ \o -> do
    tag <- o .: "type" :: Parser Text
    case tag of
      "subscribe" -> Subscribe <$> o .: "queue"
      "unsubscribe" -> Unsubscribe <$> o .: "queue"
      "ping" -> pure Ping
      _ -> fail "unknown frame type"

instance ToJSON ClientFrame where
  toJSON = object . clientFields
  toEncoding = pairs . mconcat . clientFields

clientFields :: (KeyValue e kv) => ClientFrame -> [kv]
clientFields (Subscribe q) = ["type" .= ("subscribe" :: Text), "queue" .= q]
clientFields (Unsubscribe q) = ["type" .= ("unsubscribe" :: Text), "queue" .= q]
clientFields Ping = ["type" .= ("ping" :: Text)]

instance ToJSON UpdateSource where
  toJSON = toJSON . sourceText
  toEncoding = toEncoding . sourceText

sourceText :: UpdateSource -> Text
sourceText NotifySource = "notify"
sourceText PollSource = "poll"

instance ToJSON ServerFrame where
  toJSON = object . serverFields
  toEncoding = pairs . mconcat . serverFields

serverFields :: (KeyValue e kv) => ServerFrame -> [kv]
serverFields (Snapshot q metrics push) = ["type" .= ("snapshot" :: Text), "queue" .= q, "metrics" .= metrics, "push" .= push]
serverFields (Update q metrics source) = ["type" .= ("update" :: Text), "queue" .= q, "metrics" .= metrics, "source" .= source]
serverFields Pong = ["type" .= ("pong" :: Text)]
serverFields (ErrorFrame code message queue) = ["type" .= ("error" :: Text), "code" .= code, "message" .= message, "queue" .= queue]
serverFields Goodbye = ["type" .= ("goodbye" :: Text)]
