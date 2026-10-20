-- | CORS (Cross-Origin Resource Sharing) support: the browser mechanism that
-- lets a page served from one origin read responses from another. Off by
-- default; on only for an explicit list of origins; never with credentials.
module Pgmq.Inspect.Cors (corsMiddleware) where

import Data.List.NonEmpty qualified as NE
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors
  ( CorsResourcePolicy (..),
    cors,
    simpleCorsResourcePolicy,
  )
import Pgmq.Inspect.Config (CorsPolicy (..))

-- | 'CorsDisabled' adds no headers and intercepts nothing. 'CorsAllowOrigins'
-- answers preflight @OPTIONS@ requests from a listed origin and adds
-- @Access-Control-Allow-Origin@ (echoing the origin, with @Vary: Origin@) to
-- actual responses; a request whose @Origin@ is not listed is served normally
-- with no CORS headers, which the browser then refuses to expose to the page.
corsMiddleware :: CorsPolicy -> Middleware
corsMiddleware CorsDisabled = id
corsMiddleware (CorsAllowOrigins origins) = cors (const (Just policy))
  where
    policy =
      simpleCorsResourcePolicy
        { corsOrigins = Just (NE.toList origins, False),
          corsMethods = ["GET", "OPTIONS"],
          corsRequestHeaders = ["Content-Type"],
          corsVaryOrigin = True,
          corsIgnoreFailures = True
        }
