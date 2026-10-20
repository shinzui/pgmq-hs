module ServerSpec (tests) where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception
import Data.Aeson (Value (..))
import Data.IORef
import Data.List (isInfixOf)
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types (statusCode)
import Network.Socket qualified as Socket
import Network.Wai (pathInfo)
import Network.Wai.Handler.Warp qualified as Warp
import Pgmq.Inspect
import Pgmq.Inspect.Lifecycle qualified as Lifecycle
import RouterSpec (errorCodeOf, mockEnv)
import System.IO.Error (isUserError)
import Test.Tasty
import Test.Tasty.HUnit
import TestServer

tests :: TestTree
tests =
  testGroup
    "Server lifecycle and mounting"
    [ testCase "a plain GET /ws is 426" $ withTestServer defaultInspectConfig mockEnv $ \port -> do
        r <- get port "/ws"
        replyStatus r @?= 426
        errorCodeOf r >>= (@?= String "websocket_upgrade_required"),
      testCase "upgrade seam returns JSON 503" $
        withInspectApplicationWith defaultInspectConfig mockEnv rejectingWebSocketApp $ \app ->
          Warp.testWithApplication (pure app) $ \port -> do
            r <- getWith port "GET" upgradeHeaders "/ws"
            replyStatus r @?= 503
            errorCodeOf r >>= (@?= String "websocket_unavailable"),
      testCase "HTTP, encoded queue names and upgrades use stripped pathInfo under a prefix" $
        withInspectApplicationWith defaultInspectConfig mockEnv rejectingWebSocketApp $ \app ->
          Warp.testWithApplication (pure (\req respond -> app req {pathInfo = drop 1 (pathInfo req)} respond)) $ \port -> do
            r <- get port "/pgmq/queues/%6frders"
            replyStatus r @?= 200
            upgrade <- getWith port "GET" upgradeHeaders "/pgmq/ws"
            replyStatus upgrade @?= 503,
      testCase "occupied port fails before continuation" $
        withInspectServer config mockEnv $ \server -> do
          ran <- newIORef False
          result <- try @IOException (withInspectServer config {bindPort = serverPort server} mockEnv (\_ -> writeIORef ran True))
          assertBool "bind failure propagated" (either (const True) (const False) result)
          readIORef ran >>= (@?= False),
      testCase "invalid host fails before continuation" $ do
        result <- try @IOException (withInspectServer config {bindHost = "invalid host !"} mockEnv (const (assertFailure "continued after bad host")))
        assertBool "host failure" (either (const True) (const False) result),
      testCase "invalid port and config are rejected" $
        mapM_
          ( \cfg -> do
              result <- try @IOException (withInspectServer cfg mockEnv (const (assertFailure "continued after invalid configuration")))
              assertBool "invalid configuration failed" (either (const True) (const False) result)
          )
          [config {bindPort = -1}, config {bindPort = 65536}, config {inspect = defaultInspectConfig {maxPageLimit = maxBound}}],
      testCase "normal and exceptional exits join thread and close the actual owned socket" $ do
        saved <- newEmptyMVar
        observedServer (\socket server -> putMVar saved (socket, server))
        (socket, server) <- takeMVar saved
        closedSocket socket
        poll (serverThread server) >>= (\state -> assertBool "server joined" (maybe False (const True) state))
        result <- try @IOException $ observedServer (\owned _ -> putMVar saved (owned, server) >> throwIO (userError "intentional callback failure"))
        case result of
          Left err -> assertBool "callback error propagated" (isUserError err && "intentional callback failure" `isInfixOf` show err)
          Right () -> assertFailure "callback error was lost"
        (failedSocket, _) <- takeMVar saved
        closedSocket failedSocket,
      testCase "cancellation immediately after bind closes the actual startup socket" $ do
        bound <- newEmptyMVar
        withAsync (Lifecycle.withBoundSocket config (\socket -> putMVar bound socket >> threadBlock)) $ \owner -> do
          socket <- takeMVar bound
          cancel owner
          closedSocket socket,
      testCase "cancellation in a running callback joins thread and closes owned socket" $ do
        started <- newEmptyMVar
        withAsync (observedServer (\socket server -> putMVar started (socket, server) >> threadBlock)) $ \owner -> do
          (socket, server) <- takeMVar started
          cancel owner
          closedSocket socket
          poll (serverThread server) >>= (\state -> assertBool "server joined" (maybe False (const True) state)),
      testCase "port 0 honors configured loopback host" $
        withInspectServer config {bindHost = "::1"} mockEnv $ \server -> do
          assertBool "OS assigned port" (serverPort server > 0)
          manager <- Http.newManager Http.defaultManagerSettings
          req <- Http.parseRequest ("http://[::1]:" <> show (serverPort server) <> "/health/live")
          response <- Http.httpLbs req manager
          statusCode (Http.responseStatus response) @?= 200
    ]
  where
    -- These are the same private resource combinators used by withInspectServer.
    observedServer action = Lifecycle.withBoundSocket config $ \socket ->
      withInspectRuntime defaultInspectConfig mockEnv $ \runtime ->
        Lifecycle.withRunningSocket
          config
          socket
          (runtimeApplication runtime)
          (runtimeRequestShutdown runtime >> runtimeAwaitDrained runtime 1000000)
          (\port worker -> action socket (RunningInspectServer port worker))
    closedSocket socket = Socket.withFdSocket socket (\descriptor -> descriptor @?= -1)
    config = defaultInspectServerConfig {bindPort = 0}
    threadBlock = newEmptyMVar >>= takeMVar :: IO ()
    upgradeHeaders = [("Connection", "Upgrade"), ("Upgrade", "websocket"), ("Sec-WebSocket-Key", "dGhlIHNhbXBsZSBub25jZQ=="), ("Sec-WebSocket-Version", "13")]
