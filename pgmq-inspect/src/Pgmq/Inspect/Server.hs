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

import Control.Concurrent.Async (Async, wait, waitSTM, withAsync)
import Control.Concurrent.STM
import Control.Exception (bracket, finally)
import Control.Monad (void)
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Streaming.Network (bindPortTCP)
import Data.String (fromString)
import Network.Socket qualified as Socket
import Network.Wai (Application)
import Network.Wai.Handler.Warp qualified as Warp
import Network.WebSockets qualified as WS
import Pgmq.Inspect.Budget (withInspectionBudget)
import Pgmq.Inspect.Config (InspectConfig (..), InspectServerConfig (..), validateInspectConfig)
import Pgmq.Inspect.Cors (corsMiddleware)
import Pgmq.Inspect.Env (InspectEnv)
import Pgmq.Inspect.Error (websocketUnavailable)
import Pgmq.Inspect.Http (httpApp)
import Pgmq.Inspect.Refresh (stopCoordinator, withCoordinator)
import Pgmq.Inspect.WebSocket qualified as Feed

-- | The application with the WebSocket feed unavailable (every upgrade is
-- answered 503 @websocket_unavailable@). Continuation-shaped because the
-- feed, once wired, allocates a LISTEN connection and releases it when the
-- continuation returns.
withInspectApplication :: InspectConfig -> InspectEnv -> (Application -> IO a) -> IO a
withInspectApplication cfg env k = withInspectRuntime cfg env (k . runtimeApplication)

-- | Shared lifetime of HTTP admission and the metrics feed. Listener health is
-- independent of readiness; a reconnecting listener never disables polling.
data InspectRuntime = InspectRuntime
  { runtimeApplication :: !Application,
    runtimeRequestShutdown :: !(IO ()),
    runtimeAwaitDrained :: !(Int -> IO ()),
    runtimeListenerHealthy :: !(STM Bool)
  }

withInspectRuntime :: InspectConfig -> InspectEnv -> (InspectRuntime -> IO a) -> IO a
withInspectRuntime cfg env k = do
  checked cfg
  withInspectionBudget cfg $ \budget ->
    withCoordinator cfg env budget Nothing $ \coordinator -> do
      state <- Feed.newWebSocketState (wsMaxConnections cfg)
      let shutdown = atomically (stopCoordinator coordinator >> Feed.requestShutdown state)
          runtime =
            InspectRuntime
              { runtimeApplication = corsMiddleware (corsPolicy cfg) (httpApp budget cfg env (Feed.websocketApp cfg coordinator state)),
                runtimeRequestShutdown = shutdown,
                runtimeAwaitDrained = Feed.awaitDrained state,
                runtimeListenerHealthy = pure False
              }
          cleanup = shutdown >> Feed.awaitDrained state 1000000 >> Feed.closeConnections state
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
  if bindPort scfg < 0 || bindPort scfg > 65535 then ioError (userError "bindPort must be between 0 and 65535") else pure ()
  bracket (bindPortTCP (bindPort scfg) (fromString (bindHost scfg))) Socket.close $ \sock ->
    withInspectRuntime (inspect scfg) env $ \runtime -> do
      let app = runtimeApplication runtime
      actualPort <- fromIntegral <$> Socket.socketPort sock
      started <- newEmptyTMVarIO
      let settings = Warp.setBeforeMainLoop (atomically (putTMVar started ())) (Warp.setPort actualPort (settingsFor scfg))
      withAsync (Warp.runSettingsSocket settings sock app) $ \worker -> do
        -- Prefer a failure if the worker already exited when startup is observed.
        atomically ((waitSTM worker >> throwSTM (userError "Inspection server exited during startup")) `orElse` readTMVar started)
        ( withAsync (k (RunningInspectServer actualPort worker)) $ \continuation -> do
            outcome <- atomically ((Left <$> waitSTM worker) `orElse` (Right <$> waitSTM continuation))
            either (const (ioError (userError "Inspection server exited"))) pure outcome
          )
          `finally` (runtimeRequestShutdown runtime >> runtimeAwaitDrained runtime 1000000)

runInspectServer :: InspectServerConfig -> InspectEnv -> IO ()
runInspectServer scfg env = withInspectServer scfg env (void . wait . serverThread)

checked :: InspectConfig -> IO ()
checked = either (ioError . userError . show) (const (pure ())) . validateInspectConfig

settingsFor :: InspectServerConfig -> Warp.Settings
settingsFor scfg =
  Warp.setHost (fromString (bindHost scfg)) (Warp.setPort (bindPort scfg) Warp.defaultSettings)
