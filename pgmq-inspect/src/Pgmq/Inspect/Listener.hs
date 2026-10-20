-- | One runtime-wide LISTEN connection with refcounted channels. Registry changes
-- and socket readiness share an interruptible STM wait whose fd registration is
-- always released. Reconnect reissues every desired LISTEN; polling is independent.
-- The backend application_name is @pgmq-inspect-listener@. Health and channel
-- acknowledgement are separate observations; neither promises installed triggers.
module Pgmq.Inspect.Listener
  ( Listener,
    SubscriberKey,
    withListener,
    subscribeChannel,
    unsubscribeChannel,
    listenerHealthy,
    listenerAcknowledged,
    listenerChannelCount,
  )
where

import Control.Concurrent (threadDelay, threadWaitReadSTM, yield)
import Control.Concurrent.Async (link, withAsync)
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forM_, unless, when)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Unique (Unique, newUnique)
import Database.PostgreSQL.LibPQ qualified as PQ
import Hasql.Connection qualified as Connection
import Hasql.Connection.Settings (Settings)
import Hasql.Connection.Settings qualified as Settings
import Hasql.Session qualified as Session

newtype SubscriberKey = SubscriberKey Unique
  deriving newtype (Eq, Ord)

data Listener = Listener
  { listenerRegistry :: !(TVar (Map Text (Map SubscriberKey (STM ())))),
    registryChanged :: !(TVar Bool),
    healthy :: !(TVar Bool),
    acknowledged :: !(TVar (Set Text))
  }

withListener :: Settings -> (Listener -> IO a) -> IO a
withListener settings k = do
  listener <- Listener <$> newTVarIO Map.empty <*> newTVarIO False <*> newTVarIO False <*> newTVarIO Set.empty
  let cleanup = atomically $ do
        writeTVar (healthy listener) False
        writeTVar (acknowledged listener) Set.empty
        writeTVar (listenerRegistry listener) Map.empty
  (withAsync (reconnect settings listener 100000) $ \task -> link task >> k listener) `finally` cleanup

subscribeChannel :: Listener -> Text -> STM () -> IO SubscriberKey
subscribeChannel listener channel wake = mask_ $ do
  key <- SubscriberKey <$> newUnique
  atomically $ do
    registry <- readTVar (listenerRegistry listener)
    let callbacks = Map.findWithDefault Map.empty channel registry
    writeTVar (listenerRegistry listener) (Map.insert channel (Map.insert key wake callbacks) registry)
    when (Map.null callbacks) (writeTVar (registryChanged listener) True)
  pure key

unsubscribeChannel :: Listener -> Text -> SubscriberKey -> IO ()
unsubscribeChannel listener channel key = atomically $ do
  registry <- readTVar (listenerRegistry listener)
  case Map.lookup channel registry of
    Nothing -> pure ()
    Just callbacks -> do
      let remaining = Map.delete key callbacks
      if Map.null remaining
        then do
          writeTVar (listenerRegistry listener) (Map.delete channel registry)
          writeTVar (registryChanged listener) True
        else writeTVar (listenerRegistry listener) (Map.insert channel remaining registry)

listenerHealthy :: Listener -> STM Bool
listenerHealthy = readTVar . healthy

listenerAcknowledged :: Listener -> Text -> STM Bool
listenerAcknowledged listener channel = Set.member channel <$> readTVar (acknowledged listener)

listenerChannelCount :: Listener -> STM Int
listenerChannelCount listener = Map.size <$> readTVar (listenerRegistry listener)

reconnect :: Settings -> Listener -> Int -> IO ()
reconnect settings listener delay = do
  wasHealthy <- newIORef False
  outcome <- try @SomeException $ bracket acquire Connection.release (serve listener wasHealthy)
  atomically $ writeTVar (healthy listener) False >> writeTVar (acknowledged listener) Set.empty
  case outcome of
    Left err | Just (_ :: SomeAsyncException) <- fromException err -> throwIO err
    _ -> pure ()
  succeeded <- readIORef wasHealthy
  let pause = if succeeded then 100000 else delay
  threadDelay pause
  reconnect settings listener (if succeeded then 100000 else min 5000000 (delay * 2))
  where
    acquire = Connection.acquire (settings <> Settings.applicationName "pgmq-inspect-listener") >>= either (const (ioError (userError "Listener connection failed"))) pure

serve :: Listener -> IORef Bool -> Connection.Connection -> IO ()
serve listener wasHealthy connection = loop Set.empty
  where
    loop listening = do
      desired <- atomically $ do
        registry <- readTVar (listenerRegistry listener)
        writeTVar (registryChanged listener) False
        pure (Map.keysSet registry)
      forM_ (Set.toList (listening Set.\\ desired)) $ \channel -> command "UNLISTEN" channel
      forM_ (Set.toList (desired Set.\\ listening)) $ \channel -> command "LISTEN" channel
      atomically $ writeTVar (acknowledged listener) desired >> writeTVar (healthy listener) True
      writeIORef wasHealthy True
      full <-
        Connection.use
          connection
          ( Session.onLibpqConnection
              ( \raw -> do
                  count <- drain raw (0 :: Int)
                  if count == 64
                    then pure (Right True, raw)
                    else do
                      descriptor <- PQ.socket raw >>= maybe (ioError (userError "Listener has no socket")) pure
                      -- Always unregister, including registry changes and async cancellation.
                      bracket (threadWaitReadSTM descriptor) snd $ \(readable, _) -> do
                        inputReady <-
                          atomically $
                            (readTVar (registryChanged listener) >>= check >> pure False)
                              `orElse` (readable >> pure True)
                        when inputReady $ do
                          consumed <- PQ.consumeInput raw
                          status <- PQ.status raw
                          unless (consumed && status == PQ.ConnectionOk) (ioError (userError "Listener connection dropped"))
                      pure (Right False, raw)
              )
          )
          >>= either (const (ioError (userError "Listener receive failed"))) pure
      when full yield
      loop desired
    command operation channel = do
      let quoted = "\"" <> T.replace "\"" "\"\"" channel <> "\""
      Connection.use connection (Session.script (operation <> " " <> quoted)) >>= either (const (ioError (userError "Listener channel synchronization failed"))) pure
    drain raw count
      | count >= 64 = pure count
      | otherwise =
          PQ.notifies raw >>= \case
            Nothing -> pure count
            Just notification -> do
              let channel = TE.decodeUtf8Lenient (PQ.notifyRelname notification)
              atomically $ do
                registry <- readTVar (listenerRegistry listener)
                sequence_ (Map.findWithDefault Map.empty channel registry)
              drain raw (count + 1)
