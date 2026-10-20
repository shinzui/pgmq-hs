-- | Compiled guide example: the host owns inspection capacity and its server.
module EmbeddingExample (mountUnder, withEmbeddedInspection) where

import Data.List (stripPrefix)
import Data.Text (Text)
import Network.HTTP.Types (status404)
import Network.Wai (Application, pathInfo, responseLBS)
import Pgmq.Inspect

mountUnder :: [Text] -> Application -> Application
mountUnder prefix app req respond =
  case stripPrefix prefix (pathInfo req) of
    Just rest -> app req {pathInfo = rest} respond
    Nothing -> respond (responseLBS status404 [] "Not found")

withEmbeddedInspection :: InspectConfig -> InspectEnv -> (Application -> IO a) -> IO a
withEmbeddedInspection config env k =
  withInspectApplication config env (k . mountUnder ["pgmq"])
