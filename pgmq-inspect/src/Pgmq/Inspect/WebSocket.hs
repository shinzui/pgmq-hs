-- | Queue metrics feed at @/ws@. Client frames are
-- @{"type":"subscribe","queue":"orders"}@, @unsubscribe@ with the same queue,
-- and @{"type":"ping"}@. A successful subscribe publishes a @snapshot@ before
-- any @update@; snapshots carry @queue@, @metrics@ and @push@, updates carry
-- @queue@, @metrics@ and @source@ (@poll@ or @notify@). No messages are streamed.
--
-- @push@ means notification interest was registered for a validated,
-- non-partitioned queue, not that a listener or trigger is healthy. Polling is
-- authoritative even when every notification is lost. A runtime shares one
-- rate-limited metrics read per distinct queue across connections.
--
-- @pong@ answers a JSON ping; WebSocket control pings run every 30 seconds.
-- @error@ carries @code@, safe @message@ and an always-present nullable @queue@.
-- Bounded outboxes drop the oldest frame and report @overflow@ before the next
-- retained frame; re-read HTTP or re-subscribe after loss. Unsubscribe invalidates
-- queued generations. Bytes already handed to the socket writer cannot be recalled.
-- @goodbye@ precedes a server close, with a bounded best-effort shutdown drain.
--
-- Server JSON examples (QueueMetrics uses its owning domain encoding):
--
-- > {"type":"snapshot","queue":"orders","metrics":{"queue_name":"orders","queue_length":3,"newest_msg_age_sec":1,"oldest_msg_age_sec":9,"total_messages":12,"scrape_time":"2026-09-30T12:00:00Z","queue_visible_length":2,"default_partition_length":null},"push":true}
--
-- > {"type":"update","queue":"orders","metrics":{"queue_name":"orders","queue_length":3,"newest_msg_age_sec":1,"oldest_msg_age_sec":9,"total_messages":12,"scrape_time":"2026-09-30T12:00:00Z","queue_visible_length":2,"default_partition_length":null},"source":"notify"}
--
-- > {"type":"pong"}
--
-- > {"type":"error","code":"overflow","message":"The server dropped frames because this connection was not reading them; re-subscribe or re-read the queue over HTTP.","queue":null}
--
-- > {"type":"goodbye"}
module Pgmq.Inspect.WebSocket
  ( module Pgmq.Inspect.Frames,
    WebSocketState,
    newWebSocketState,
    requestShutdown,
    activeConnections,
    awaitDrained,
    closeConnections,
    websocketApp,
  )
where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forever, unless, when)
import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Unique (Unique, newUnique)
import Network.WebSockets qualified as WS
import Pgmq.Inspect.Config
import Pgmq.Inspect.Error
import Pgmq.Inspect.Frames
import Pgmq.Inspect.Outbox
import Pgmq.Inspect.Refresh
import System.Timeout (timeout)

-- | Connection count includes handshakes. Tasks are registered before serving.
data WebSocketState = WebSocketState
  { stateLimit :: !Int,
    stateCount :: !(TVar Int),
    stateShutdown :: !(TVar Bool),
    stateTasks :: !(TVar (Map.Map Unique (Async ())))
  }

newWebSocketState :: Int -> IO WebSocketState
newWebSocketState limit = WebSocketState limit <$> newTVarIO 0 <*> newTVarIO False <*> newTVarIO Map.empty

requestShutdown :: WebSocketState -> STM ()
requestShutdown = flip writeTVar True . stateShutdown

activeConnections :: WebSocketState -> STM Int
activeConnections = readTVar . stateCount

awaitDrained :: WebSocketState -> Int -> IO ()
awaitDrained state budget = do
  _ <- timeout budget (atomically (activeConnections state >>= check . (== 0)))
  pure ()

-- | Stop and join remaining transport/handshake tasks, also for host-owned Warp.
closeConnections :: WebSocketState -> IO ()
closeConnections state = readTVarIO (stateTasks state) >>= mapM_ cancel . Map.elems

websocketApp :: InspectConfig -> MetricsCoordinator -> WebSocketState -> WS.ServerApp
websocketApp cfg coordinator state pending = mask $ \restore -> do
  slot <- atomically $ do
    count <- readTVar (stateCount state)
    stopped <- readTVar (stateShutdown state)
    if stopped || count >= stateLimit state
      then pure False
      else writeTVar (stateCount state) (count + 1) >> pure True
  if not slot
    then
      restore $
        WS.rejectRequestWith
          pending
          WS.defaultRejectRequest
            { WS.rejectCode = 429,
              WS.rejectMessage = "Too Many Requests",
              WS.rejectHeaders = [("Content-Type", "application/json")],
              WS.rejectBody = LBS.toStrict (encode inspectionBusy)
            }
    else flip finally (atomically (modifyTVar' (stateCount state) (subtract 1))) $ do
      key <- newUnique
      start <- newEmptyTMVarIO
      withAsync (atomically (takeTMVar start) >> restore serve) $ \task -> do
        registered <- atomically $ do
          stopped <- readTVar (stateShutdown state)
          if stopped
            then pure False
            else do
              modifyTVar' (stateTasks state) (Map.insert key task)
              putTMVar start ()
              pure True
        when registered $ restore (wait task) `finally` atomically (modifyTVar' (stateTasks state) (Map.delete key))
  where
    serve =
      ( do
          connection <- WS.acceptRequest pending
          subscriber <- newSubscriber (wsQueueCapacity cfg)
          transport connection subscriber `finally` releaseSubscriber coordinator subscriber
      )
        `catch` \(_ :: WS.ConnectionException) -> pure ()
    transport connection subscriber = WS.withPingThread connection 30 (pure ()) $ do
      let outbox = subscriberOutbox subscriber
          writer = do
            (overflow, frame) <- atomically (takeOutgoing outbox)
            when overflow (WS.sendTextData connection (encode overflowFrame))
            WS.sendTextData connection (encode frame)
            case frame of
              Goodbye -> WS.sendClose connection ("shutdown" :: LBS.ByteString)
              _ -> writer
          finishWithError code message = do
            atomically (enqueue outbox (ErrorFrame code message Nothing))
            -- Capacity one must still deliver the error before goodbye.
            atomically (queuedFrames outbox >>= check . (== 0))
            atomically (enqueue outbox Goodbye)
            atomically retry
          reader =
            ( forever $ do
                bytes <- WS.receiveData connection :: IO LBS.ByteString
                if LBS.length bytes > fromIntegral (wsMaxFrameBytes cfg)
                  then finishWithError "frame_too_large" "The client frame exceeded the configured size limit."
                  else case eitherDecode bytes of
                    Left _ -> atomically (enqueue outbox (ErrorFrame "invalid_frame" "Expected a subscribe, unsubscribe or ping JSON frame." Nothing))
                    Right frame -> do
                      stopped <- readTVarIO (stateShutdown state)
                      unless stopped $ case frame of
                        Subscribe name -> subscribeQueue coordinator subscriber name
                        Unsubscribe name -> unsubscribeQueue coordinator subscriber name
                        Ping -> atomically (enqueue outbox Pong)
            )
              `catch` \err -> case err of
                WS.ParseException message | "limit" `isInfixOf` message -> finishWithError "frame_too_large" "The client frame exceeded the configured size limit."
                WS.ParseException _ -> finishWithError "invalid_frame" "The WebSocket frame was invalid."
                _ -> throwIO (err :: WS.ConnectionException)
          shutdown = do
            atomically (readTVar (stateShutdown state) >>= check)
            atomically (enqueue outbox Goodbye)
            atomically retry
      withAsync writer $ \writerTask -> race_ (wait writerTask) (race_ reader shutdown)
