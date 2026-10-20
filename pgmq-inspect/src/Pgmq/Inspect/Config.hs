-- | Configuration for the inspection surface and for the convenience runner.
--
-- The surface itself is configured by 'InspectConfig'; where it is served
-- from is 'InspectServerConfig'. A host that mounts the 'Network.Wai.Application'
-- into its own server needs only the former.
module Pgmq.Inspect.Config
  ( CorsPolicy (..),
    InspectConfig (..),
    defaultInspectConfig,
    validateInspectConfig,
    InspectServerConfig (..),
    defaultInspectServerConfig,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.URI (URI (..), URIAuth (..), parseURI)
import Numeric.Natural (Natural)

-- | Which browser origins may call the surface cross-origin.
--
-- There is deliberately no \"any origin\" constructor and credentials are
-- never allowed: the browser forbids a wildcard origin together with
-- credentials, and an inspection surface that assumes a trusted network has
-- no business opting into either. List the origins you serve a browser UI
-- from, for example @CorsAllowOrigins ("http://localhost:5173" :| [])@.
data CorsPolicy
  = -- | Send no CORS headers at all. The default.
    CorsDisabled
  | -- | Answer requests whose @Origin@ header is exactly one of these with the
    -- CORS headers that let the page read the response; serve any other origin
    -- normally but without those headers.
    CorsAllowOrigins !(NonEmpty ByteString)
  deriving stock (Eq, Show, Generic)

-- | Behaviour of the surface. See 'defaultInspectConfig' for the defaults.
data InspectConfig = InspectConfig
  { corsPolicy :: !CorsPolicy,
    -- | Page size when a request omits @limit@.
    defaultPageLimit :: !Int32,
    -- | Largest page a request may ask for; larger values are capped to this.
    maxPageLimit :: !Int32,
    -- | Budget for the readiness probe's round trip, in microseconds.
    readinessTimeoutUs :: !Int,
    -- | WebSocket feed (see "Pgmq.Inspect.WebSocket"): how often each subscribed
    -- queue is polled for metrics, in microseconds.
    pollIntervalUs :: !Int,
    -- | WebSocket feed: how long to coalesce insert notifications for one
    -- queue before reading its metrics, in microseconds.
    notifyDebounceUs :: !Int,
    -- | WebSocket feed: concurrent connections accepted before rejecting.
    wsMaxConnections :: !Int,
    -- | WebSocket feed: queues one connection may watch at once.
    wsMaxSubscriptions :: !Int,
    -- | WebSocket feed: frames buffered per connection before the oldest are
    -- dropped and an in-band overflow error is sent.
    wsQueueCapacity :: !Natural,
    maxConcurrentReads :: !Int,
    metricsMinRefreshUs :: !Int,
    wsMaxQueues :: !Int,
    wsMaxFrameBytes :: !Int
  }
  deriving stock (Eq, Show, Generic)

-- | CORS disabled, pages of 50 capped at 500, a one-second readiness budget,
-- a five-second poll, a 100 ms notify debounce, 100 connections, 100
-- subscriptions per connection, 256 buffered frames.
defaultInspectConfig :: InspectConfig
defaultInspectConfig =
  InspectConfig
    { corsPolicy = CorsDisabled,
      defaultPageLimit = 50,
      maxPageLimit = 500,
      readinessTimeoutUs = 1_000_000,
      pollIntervalUs = 5_000_000,
      notifyDebounceUs = 100_000,
      wsMaxConnections = 100,
      wsMaxSubscriptions = 100,
      wsQueueCapacity = 256,
      maxConcurrentReads = 2,
      metricsMinRefreshUs = 1_000_000,
      wsMaxQueues = 100,
      wsMaxFrameBytes = 16_384
    }

-- | Where the convenience runner listens.
data InspectServerConfig = InspectServerConfig
  { -- | Interface to bind. Loopback by default; bind @*@ only behind an
    -- authenticating reverse proxy or on a trusted network.
    bindHost :: !String,
    -- | Port to bind. @0@ asks for a free port on the configured bindHost.
    bindPort :: !Int,
    inspect :: !InspectConfig
  }
  deriving stock (Eq, Show, Generic)

-- | @127.0.0.1:9092@ with 'defaultInspectConfig'. shibuya-metrics defaults to
-- 9090 and kiroku-metrics to 9091, so the three surfaces coexist on one host.
defaultInspectServerConfig :: InspectServerConfig
defaultInspectServerConfig =
  InspectServerConfig {bindHost = "127.0.0.1", bindPort = 9092, inspect = defaultInspectConfig}

-- | Check limits before allocating sockets, queues or threads.
validateInspectConfig :: InspectConfig -> Either Text InspectConfig
validateInspectConfig cfg
  | defaultPageLimit cfg < 1 || defaultPageLimit cfg > maxPageLimit cfg = Left "defaultPageLimit must be positive and no greater than maxPageLimit"
  | maxPageLimit cfg == maxBound = Left "maxPageLimit must leave room for one lookahead row"
  | any (<= 0) [readinessTimeoutUs cfg, pollIntervalUs cfg, notifyDebounceUs cfg, wsMaxConnections cfg, wsMaxSubscriptions cfg, maxConcurrentReads cfg, metricsMinRefreshUs cfg, wsMaxQueues cfg, wsMaxFrameBytes cfg] = Left "intervals and capacity limits must be positive"
  | wsQueueCapacity cfg == 0 = Left "wsQueueCapacity must be positive"
  | CorsAllowOrigins origins <- corsPolicy cfg, not (all validOrigin origins) = Left "CORS origins must be explicit HTTP(S) origins without credentials, path, query or fragment"
  | otherwise = Right cfg
  where
    validOrigin raw = case parseURI (BS.unpack raw) of
      Just URI {uriScheme, uriAuthority = Just URIAuth {uriUserInfo, uriRegName, uriPort}, uriPath, uriQuery, uriFragment} ->
        uriScheme `elem` ["http:", "https:"] && null uriUserInfo && not (null uriRegName) && uriRegName /= "*" && null uriPath && null uriQuery && null uriFragment && validPort uriPort
      _ -> False
    validPort "" = True
    validPort (':' : digits) = not (null digits) && length digits <= 5 && all (\c -> c >= '0' && c <= '9') digits && (read digits :: Int) <= 65535
    validPort _ = False
