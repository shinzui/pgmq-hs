-- | The mountable application and the convenience runner.
--
-- 'withInspectApplication' hands the host a WAI 'Application' to mount
-- anywhere; the host that mounts it under a prefix must strip the prefix
-- from 'Network.Wai.pathInfo' before delegating, as every WAI mounting helper
-- does. 'withInspectServer' and 'runInspectServer' serve it with Warp.
module Pgmq.Inspect.Server
  ( withInspectApplication,
    withInspectApplicationWith,
    InspectRuntime (..),
    withInspectRuntime,
    rejectingWebSocketApp,
    RunningInspectServer (..),
    withInspectServer,
    runInspectServer,
  )
where

import Control.Concurrent.Async (Async, wait)
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (void)
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import GHC.Clock (getMonotonicTimeNSec)
import Hasql.Connection.Settings qualified
import Network.Wai (Application)
import Network.WebSockets qualified as WS
import Pgmq.Inspect.Budget (withInspectionBudget)
import Pgmq.Inspect.Config (InspectConfig (..), InspectServerConfig (..), validateInspectConfig)
import Pgmq.Inspect.Cors (corsMiddleware)
import Pgmq.Inspect.Env (InspectEnv (..))
import Pgmq.Inspect.Error (websocketUnavailable)
import Pgmq.Inspect.Http (httpApp)
import Pgmq.Inspect.Lifecycle qualified as Lifecycle
import Pgmq.Inspect.Listener qualified as Listener
import Pgmq.Inspect.Refresh (PushHooks (..), stopCoordinator, withCoordinator)
import Pgmq.Inspect.WebSocket qualified as Feed

-- | The mountable HTTP application and shared metrics feed. Its optional LISTEN
-- connection, scheduler and transports are released when the continuation exits.
withInspectApplication :: InspectConfig -> InspectEnv -> (Application -> IO a) -> IO a
withInspectApplication cfg env k = withInspectRuntime cfg env (k . runtimeApplication)

-- | Shared lifetime of HTTP admission and the metrics feed. Listener health is
-- independent of readiness; a reconnecting listener never disables polling.
data InspectRuntime = InspectRuntime
  { runtimeApplication :: !Application,
    runtimeRequestShutdown :: !(IO ()),
    runtimeAwaitDrained :: !(Int -> IO ()),
    -- | Current-session LISTEN acknowledgement for a channel. Resolve channel
    -- names with notifyChannelName; health alone does not acknowledge a channel.
    runtimeListenerAcknowledged :: !(Text -> STM Bool),
    -- | Connection health, independent of HTTP readiness and trigger existence.
    runtimeListenerHealthy :: !(STM Bool)
  }

withInspectRuntime :: InspectConfig -> InspectEnv -> (InspectRuntime -> IO a) -> IO a
withInspectRuntime cfg env k = do
  checked cfg
  optionalListener (listenerSettings env) $ \listener ->
    withInspectionBudget cfg $ \budget ->
      withCoordinator cfg env budget (pushHooks <$> listener) $ \coordinator -> do
        state <- Feed.newWebSocketState (wsMaxConnections cfg)
        shutdownStarted <- newTVarIO Nothing
        let shutdown = do
              now <- getMonotonicTimeNSec
              atomically $ do
                stopCoordinator coordinator
                Feed.requestShutdown state
                started <- readTVar shutdownStarted
                case started of
                  Nothing -> writeTVar shutdownStarted (Just now)
                  Just _ -> pure ()
            runtime =
              InspectRuntime
                { runtimeApplication = corsMiddleware (corsPolicy cfg) (httpApp budget cfg env (Feed.websocketApp cfg coordinator state)),
                  runtimeRequestShutdown = shutdown,
                  runtimeAwaitDrained = Feed.awaitDrained state,
                  runtimeListenerAcknowledged = maybe (const (pure False)) Listener.listenerAcknowledged listener,
                  runtimeListenerHealthy = maybe (pure False) Listener.listenerHealthy listener
                }
            cleanup = do
              shutdown
              now <- getMonotonicTimeNSec
              started <- readTVarIO shutdownStarted
              let elapsedUs = maybe 0 (\start -> min 1000000 ((now - start) `div` 1000)) started
              Feed.awaitDrained state (1000000 - fromIntegral elapsedUs)
              Feed.closeConnections state
        k runtime `finally` cleanup

-- | The application with an explicit WebSocket seam.
withInspectApplicationWith :: InspectConfig -> InspectEnv -> WS.ServerApp -> (Application -> IO a) -> IO a
withInspectApplicationWith cfg env wsApp k =
  checked cfg
    >> withInspectionBudget
      cfg
      ( \budget ->
          k (corsMiddleware (corsPolicy cfg) (httpApp budget cfg env wsApp))
      )

-- | Answer every upgrade with an ordinary 503 carrying the JSON error envelope.
rejectingWebSocketApp :: WS.ServerApp
rejectingWebSocketApp pending =
  WS.rejectRequestWith
    pending
    WS.defaultRejectRequest
      { WS.rejectCode = 503,
        WS.rejectMessage = "Service Unavailable",
        WS.rejectHeaders = [("Content-Type", "application/json")],
        WS.rejectBody = LBS.toStrict (encode websocketUnavailable)
      }

data RunningInspectServer = RunningInspectServer
  { -- | The bound port; informative when 'bindPort' was @0@.
    serverPort :: !Int,
    serverThread :: !(Async ())
  }

-- | Bind, acknowledge startup, and serve within the continuation’s scope.
withInspectServer :: InspectServerConfig -> InspectEnv -> (RunningInspectServer -> IO a) -> IO a
withInspectServer scfg env k = do
  checked (inspect scfg)
  Lifecycle.withBoundSocket scfg $ \socket ->
    withInspectRuntime (inspect scfg) env $ \runtime ->
      Lifecycle.withRunningSocket
        scfg
        socket
        (runtimeApplication runtime)
        (runtimeRequestShutdown runtime >> runtimeAwaitDrained runtime 1000000)
        (\port worker -> k (RunningInspectServer port worker))

runInspectServer :: InspectServerConfig -> InspectEnv -> IO ()
runInspectServer scfg env = withInspectServer scfg env (void . wait . serverThread)

checked :: InspectConfig -> IO ()
checked = either (ioError . userError . show) (const (pure ())) . validateInspectConfig

optionalListener :: Maybe Hasql.Connection.Settings.Settings -> (Maybe Listener.Listener -> IO a) -> IO a
optionalListener Nothing k = k Nothing
optionalListener (Just settings) k = Listener.withListener settings (k . Just)

pushHooks :: Listener.Listener -> PushHooks
pushHooks listener = PushHooks $ \channel wake -> do
  key <- Listener.subscribeChannel listener channel wake
  pure (Listener.unsubscribeChannel listener channel key)
