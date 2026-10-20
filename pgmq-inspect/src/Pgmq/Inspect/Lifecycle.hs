-- | Internal, observable socket/thread ownership used by the public runner and
-- deterministic lifecycle tests. A released ephemeral port may be reused by the
-- OS, so tests inspect the owned Socket rather than racing to bind that port again.
module Pgmq.Inspect.Lifecycle (withBoundSocket, withRunningSocket) where

import Control.Concurrent.Async (Async, waitSTM, withAsync)
import Control.Concurrent.STM
import Control.Exception (bracket, finally)
import Data.Streaming.Network (bindPortTCP)
import Data.String (fromString)
import Network.Socket qualified as Socket
import Network.Wai (Application)
import Network.Wai.Handler.Warp qualified as Warp
import Pgmq.Inspect.Config

withBoundSocket :: InspectServerConfig -> (Socket.Socket -> IO a) -> IO a
withBoundSocket cfg k = do
  if bindPort cfg < 0 || bindPort cfg > 65535 then ioError (userError "bindPort must be between 0 and 65535") else pure ()
  bracket (bindPortTCP (bindPort cfg) (fromString (bindHost cfg))) Socket.close k

-- | The caller owns the socket. Startup is acknowledged or fails synchronously;
-- graceful application shutdown runs before Warp is cancelled and joined.
withRunningSocket :: InspectServerConfig -> Socket.Socket -> Application -> IO () -> (Int -> Async () -> IO a) -> IO a
withRunningSocket cfg socket application shutdown k = do
  port <- fromIntegral <$> Socket.socketPort socket
  started <- newEmptyTMVarIO
  let settings =
        Warp.setBeforeMainLoop
          (atomically (putTMVar started ()))
          (Warp.setHost (fromString (bindHost cfg)) (Warp.setPort port Warp.defaultSettings))
  withAsync (Warp.runSettingsSocket settings socket application) $ \worker -> do
    atomically ((waitSTM worker >> throwSTM (userError "Inspection server exited during startup")) `orElse` readTMVar started)
    ( withAsync (k port worker) $ \continuation -> do
        outcome <- atomically ((Left <$> waitSTM worker) `orElse` (Right <$> waitSTM continuation))
        either (const (ioError (userError "Inspection server exited"))) pure outcome
      )
      `finally` shutdown
