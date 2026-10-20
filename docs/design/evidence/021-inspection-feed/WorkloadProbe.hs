{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- Diagnostic workload, not a timing gate. Build/run commands are in README.md.
module Main (main) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (bracket)
import Control.Monad (forM, forM_, forever, void)
import Data.Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.List (sort)
import Data.Text qualified as T
import Data.Vector qualified as V
import Effectful (Eff, IOE, liftIO)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import EphemeralDb (withPgmqDb)
import EphemeralPg qualified as Pg
import GHC.Clock (getMonotonicTimeNSec)
import Hasql.Connection.Settings qualified as Settings
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as PC
import Hasql.Session qualified as Session
import Network.Wai.Handler.Warp qualified as Warp
import Network.WebSockets qualified as WS
import Pgmq.Effectful (PgmqRuntimeError, fromUsageError)
import Pgmq.Effectful.Effect qualified as E
import Pgmq.Hasql.Sessions qualified as S
import Pgmq.Hasql.Statements.Types qualified as H
import Pgmq.Inspect
import Pgmq.Types qualified as Core
import System.Environment (getArgs)
import System.Timeout (timeout)
import Text.Read (readMaybe)

runSession :: Pool.Pool -> Session.Session a -> IO a
runSession pool action = Pool.use pool action >>= either (error . show) pure

liftSession :: Pool.Pool -> Session.Session a -> Eff '[Error PgmqRuntimeError, IOE] a
liftSession pool action = liftIO (Pool.use pool action) >>= either (throwError . fromUsageError) pure

countingEnv :: Pool.Pool -> TVar Int -> TVar Int -> InspectEnv
countingEnv pool metricQueries metadataQueries =
  InspectEnv
    ( runnerFromInterpreter
        ( interpret
            ( \_ -> \case
                E.ListQueuesUnvalidated -> liftIO (atomically (modifyTVar' metadataQueries (+ 1))) >> liftSession pool S.listQueuesUnvalidated
                E.QueueMetricsUnvalidated name -> liftIO (atomically (modifyTVar' metricQueries (+ 1))) >> liftSession pool (S.queueMetricsUnvalidated name)
                _ -> error "Probe only runs inspection metadata and metrics"
            )
        )
    )
    Nothing

main :: IO ()
main = do
  args <- getArgs
  interval <- case args of
    [] -> pure 50000
    [raw] | Just n <- readMaybe raw, n > 0 -> pure n
    _ -> error "usage: WorkloadProbe [positive refresh interval in microseconds]"
  result <- withPgmqDb $ \workers database ->
    forM_ (zip [1 .. 4 :: Int] [False, True, True, False]) $ \(trial, inspected) -> do
      queue <- either (error . show) pure (Core.parseQueueName ("inspection_probe_" <> T.pack (show trial)))
      bracket (queue <$ runSession workers (S.createQueue queue)) (\q -> void (runSession workers (S.dropQueue q))) $ \q -> do
        let payload = Core.MessageBody (String (T.replicate 128 "x"))
        void (runSession workers (S.batchSendMessage (H.BatchSendMessage q (replicate 100000 payload) Nothing)))
        bracket
          ( Pool.acquire
              ( PC.settings
                  [ PC.size 2,
                    PC.acquisitionTimeout 1,
                    PC.staticConnectionSettings (Pg.connectionSettings database <> Settings.other "connect_timeout" "2"),
                    PC.initSession (Session.script "SET statement_timeout = '2000ms'; SET lock_timeout = '250ms'")
                  ]
              )
          )
          Pool.release
          $ \inspection -> do
            metricQueries <- newTVarIO 0
            metadataQueries <- newTVarIO 0
            let env = countingEnv inspection metricQueries metadataQueries
                config = defaultInspectConfig {pollIntervalUs = interval, metricsMinRefreshUs = interval}
                measure = do
                  initial <- readTVarIO metricQueries
                  initialMeta <- readTVarIO metadataQueries
                  started <- getMonotonicTimeNSec
                  latencies <- concat <$> mapConcurrently (const (consume workers q)) [1, 2 :: Int]
                  finished <- getMonotonicTimeNSec
                  final <- readTVarIO metricQueries
                  finalMeta <- readTVarIO metadataQueries
                  let seconds = fromIntegral (finished - started) / 1e9 :: Double
                      ordered = sort latencies
                      p95 = ordered !! (ceiling (fromIntegral (length ordered) * (0.95 :: Double)) - 1)
                  LBS.putStr (encode (object ["trial" .= trial, "inspection" .= inspected, "preloaded_rows" .= (100000 :: Int), "consumed_messages" .= (40000 :: Int), "subscribers" .= (if inspected then 100 else 0 :: Int), "elapsed_seconds" .= seconds, "messages_per_second" .= (40000 / seconds), "batch_p95_ms" .= p95, "metrics_queries" .= (final - initial), "metadata_queries" .= (finalMeta - initialMeta)]))
                  putStrLn ""
            if not inspected
              then measure
              else withInspectRuntime config env $ \runtime -> Warp.testWithApplication (pure (runtimeApplication runtime)) $ \port -> do
                ready <- newTVarIO (0 :: Int)
                let client = WS.runClient "127.0.0.1" port "/ws" $ \connection -> do
                      WS.sendTextData connection (encode (Subscribe (Core.queueNameToText q)))
                      _ <- WS.receiveData connection :: IO LBS.ByteString
                      atomically (modifyTVar' ready (+ 1))
                      forever (void (WS.receiveData connection :: IO LBS.ByteString))
                withAsync (mapConcurrently_ (const client) [1 .. 100 :: Int]) $ \clients -> do
                  link clients
                  timeout 10000000 (atomically (readTVar ready >>= check . (== 100))) >>= maybe (error "snapshot startup timed out") pure
                  measure
  either (error . show) pure result

consume :: Pool.Pool -> Core.QueueName -> IO [Double]
consume workers queue = forM [1 .. 1000 :: Int] $ \_ -> do
  started <- getMonotonicTimeNSec
  count <- runSession workers $ do
    rows <- S.readMessage (H.ReadMessage queue 30 (Just 20) Nothing)
    deleted <- S.batchDeleteMessages (H.BatchMessageQuery queue (map Core.messageId (V.toList rows)))
    pure (length deleted)
  if count /= 20 then error "consumer batch was not complete" else pure ()
  finished <- getMonotonicTimeNSec
  pure (fromIntegral (finished - started) / 1e6)
