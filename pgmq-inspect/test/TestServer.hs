-- | Start the surface on an OS-assigned loopback port and talk to it with
-- http-client.
module TestServer
  ( withTestServer,
    withTestRuntime,
    Reply (..),
    get,
    getWith,
    decodeReply,
  )
where

import Control.Exception (finally)
import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive (CI)
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types (Method, methodGet)
import Network.HTTP.Types.Header (HeaderName)
import Network.Wai.Handler.Warp qualified as Warp
import Pgmq.Inspect.Config (InspectConfig, InspectServerConfig (..))
import Pgmq.Inspect.Env (InspectEnv)
import Pgmq.Inspect.Server (InspectRuntime (..), RunningInspectServer (..), withInspectRuntime, withInspectServer)
import Test.Tasty.HUnit (assertFailure)

withTestServer :: InspectConfig -> InspectEnv -> (Int -> IO a) -> IO a
withTestServer cfg env k =
  withInspectServer InspectServerConfig {bindHost = "127.0.0.1", bindPort = 0, inspect = cfg} env $ \server ->
    k (serverPort server)

data Reply = Reply
  { replyStatus :: !Int,
    replyHeaders :: ![(CI ByteString, ByteString)],
    replyBody :: !LBS.ByteString
  }

get :: Int -> String -> IO Reply
get port = getWith port methodGet []

getWith :: Int -> Method -> [(HeaderName, ByteString)] -> String -> IO Reply
getWith port method headers path = do
  manager <- Http.newManager Http.defaultManagerSettings
  request <- Http.parseRequest ("http://127.0.0.1:" <> show port <> path)
  response <- Http.httpLbs request {Http.method = method, Http.requestHeaders = headers} manager
  pure
    Reply
      { replyStatus = fromEnum (Http.responseStatus response),
        replyHeaders = Http.responseHeaders response,
        replyBody = Http.responseBody response
      }

decodeReply :: (FromJSON a) => Reply -> IO a
decodeReply reply = either (assertFailure . ("undecodable body: " <>)) pure (eitherDecode (replyBody reply))

-- | Feed shutdown happens while the host-owned Warp server is still alive.
withTestRuntime :: InspectConfig -> InspectEnv -> (Int -> InspectRuntime -> IO a) -> IO a
withTestRuntime cfg env k = withInspectRuntime cfg env $ \runtime ->
  Warp.testWithApplication (pure (runtimeApplication runtime)) $ \port ->
    k port runtime `finally` (runtimeRequestShutdown runtime >> runtimeAwaitDrained runtime 1000000)
