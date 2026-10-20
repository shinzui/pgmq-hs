module IsolationSpec (tests) where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (bracket, finally, uninterruptibleMask_)
import Control.Monad (replicateM_, void)
import EphemeralPg qualified as Pg
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as PC
import Hasql.Session qualified as Session
import HttpSpec (session, withQueue)
import Network.Wai.Handler.Warp qualified as Warp
import Pgmq.Effectful (fromUsageError)
import Pgmq.Inspect
import Pgmq.Types (queueNameToText)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import TestServer
import WebSocketSpec (sendClient, withWs)

tests :: IO (Pool.Pool, Pg.Database) -> TestTree
tests database =
  testGroup
    "WebSocket inspection isolation"
    [ testCase "uncancellable driver work stays bounded while worker capacity progresses" $ do
        (workers, db) <- database
        withQueue workers $ \queue ->
          bracket (Pool.acquire (PC.settings [PC.size 1, PC.acquisitionTimeout 1, PC.staticConnectionSettings (Pg.connectionSettings db), PC.initSession (Session.script "SET statement_timeout = '2000ms'; SET lock_timeout = '250ms'")])) Pool.release $ \inspection -> do
            entered <- newEmptyMVar
            release <- newEmptyMVar
            ready <- newEmptyMVar
            parked <- newEmptyMVar
            starts <- newTVarIO (0 :: Int)
            let base = poolEnv inspection Nothing
                env =
                  base
                    { runInspection = \program -> do
                        atomically (modifyTVar' starts (+ 1))
                        -- Model IR-6's uncancellable reply wait while holding a real,
                        -- dedicated pool connection. Always release this gate in finally.
                        outcome <-
                          Pool.use
                            inspection
                            ( Session.onLibpqConnection
                                ( \raw -> do
                                    uninterruptibleMask_ (putMVar entered () >> takeMVar release)
                                    pure (Right (), raw)
                                )
                            )
                        case outcome of
                          Left err -> pure (Left (fromUsageError err))
                          Right () -> runInspection base program
                    }
                config = defaultInspectConfig {maxConcurrentReads = 1}
            withAsync (withInspectRuntime config env (\runtime -> putMVar ready runtime >> takeMVar parked)) $ \owner -> do
              runtime <- takeMVar ready
              Warp.testWithApplication (pure (runtimeApplication runtime)) $ \port -> withWs port $ \connection -> do
                sendClient connection (Subscribe (queueNameToText queue))
                takeMVar entered
                withAsync (cancel owner) $ \canceller ->
                  ( do
                      pending <- timeout 100000 (wait canceller)
                      pending @?= Nothing
                      -- HTTP and background reads share the held permit. Rejections
                      -- must not create retries, threads or worker-pool acquisitions.
                      replicateM_ 10 $ do
                        reply <- get port "/queues"
                        replyStatus reply @?= 503
                      readTVarIO starts >>= (@?= 1)
                      progressed <- timeout 1000000 (session workers (Session.script "SELECT 1"))
                      progressed @?= Just ()
                  )
                    `finally` void (tryPutMVar release ())
              -- The released driver slice lets cancellation join every owned task.
              waitCatch owner >>= (\_ -> pure ())
            reused <- timeout 1000000 (session inspection (Session.script "SELECT 1"))
            reused @?= Just ()
    ]
