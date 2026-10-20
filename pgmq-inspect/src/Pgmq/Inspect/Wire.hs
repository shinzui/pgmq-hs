-- | Wire types that are the surface's own (not records of pgmq-core or
-- pgmq-hasql, which carry their own instances). Every encoding here follows
-- the policy in @docs/design/020-json-wire-encodings.md@: snake_case keys,
-- hand-written instances, published shapes frozen. 'Page' is the one place a
-- key may be absent: @next_cursor@ is omitted on the last page by convention.
module Pgmq.Inspect.Wire
  ( Page (..),
    ServiceDescriptor (..),
    LivenessBody (..),
    DependencyCheck (..),
    ReadinessBody (..),
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Pgmq.Types (MessageId)

-- | One page of a keyset-paged read.
data Page a = Page
  { items :: ![a],
    -- | The @msg_id@ to pass back as @from@ for the next page; 'Nothing' on the
    -- last page, in which case the key is omitted from the JSON.
    nextCursor :: !(Maybe MessageId)
  }
  deriving stock (Eq, Show, Generic)

instance (ToJSON a) => ToJSON (Page a) where
  toJSON page =
    object (("items" .= items page) : maybe [] (\c -> ["next_cursor" .= c]) (nextCursor page))

-- | What @GET /@ returns: enough for a client to find the rest.
data ServiceDescriptor = ServiceDescriptor
  { serviceName :: !Text,
    serviceVersion :: !Text,
    -- | Relative to the mount root, so it is right under any prefix.
    websocketPath :: !Text,
    openapiPath :: !Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ServiceDescriptor where
  toJSON d =
    object
      [ "service" .= serviceName d,
        "version" .= serviceVersion d,
        "websocket_path" .= websocketPath d,
        "openapi_path" .= openapiPath d
      ]

newtype LivenessBody = LivenessBody {alive :: Bool}
  deriving stock (Eq, Show, Generic)

instance ToJSON LivenessBody where
  toJSON b = object ["alive" .= alive b]

-- | One dependency the readiness probe checked.
data DependencyCheck = DependencyCheck
  { checkName :: !Text,
    checkHealthy :: !Bool,
    checkLatencyMs :: !(Maybe Int64),
    checkError :: !(Maybe Text)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON DependencyCheck where
  toJSON c =
    object
      [ "name" .= checkName c,
        "healthy" .= checkHealthy c,
        "latency_ms" .= checkLatencyMs c,
        "error" .= checkError c
      ]

data ReadinessBody = ReadinessBody
  { ready :: !Bool,
    checks :: ![DependencyCheck]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ReadinessBody where
  toJSON b = object ["ready" .= ready b, "checks" .= checks b]
