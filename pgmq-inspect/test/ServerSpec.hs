module ServerSpec (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (replicateM_)
import Data.Aeson (Value (..))
import Data.IORef
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types (statusCode)
import Network.Wai (pathInfo)
import Network.Wai.Handler.Warp qualified as Warp
import Pgmq.Inspect
import RouterSpec (errorCodeOf, mockEnv)
import System.IO.Error (isAlreadyInUseError)
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
      testCase "normal and exceptional exits join thread and release bound socket" $ do
        saved <- newEmptyMVar
        withInspectServer config mockEnv $ \server -> putMVar saved server
        server <- takeMVar saved
        poll (serverThread server) >>= (\state -> assertBool "server joined" (maybe False (const True) state))
        probeRebind (serverPort server)
        result <- try @IOException $ withInspectServer config {bindPort = serverPort server} mockEnv (const (throwIO (userError "intentional")))
        assertBool "callback exception propagated" (either (const True) (const False) result)
        probeRebind (serverPort server),
      testCase "cancellation during startup or callback releases all scoped server resources" $ do
        replicateM_ 10 $ do
          worker <- async (withInspectServer config mockEnv (const (threadBlock)))
          cancel worker
          waitCatch worker >>= (\_ -> pure ())
        started <- newEmptyMVar
        withAsync (withInspectServer config mockEnv (\s -> putMVar started s >> threadBlock)) $ \worker -> do
          server <- takeMVar started
          cancel worker
          probeRebind (serverPort server),
      testCase "port 0 honors configured loopback host" $
        withInspectServer config {bindHost = "::1"} mockEnv $ \server -> do
          assertBool "OS assigned port" (serverPort server > 0)
          manager <- Http.newManager Http.defaultManagerSettings
          req <- Http.parseRequest ("http://[::1]:" <> show (serverPort server) <> "/health/live")
          response <- Http.httpLbs req manager
          statusCode (Http.responseStatus response) @?= 200
    ]
  where
    -- Once released, the OS may immediately assign the port to another test's
    -- outbound socket or listener. A joined server must permit a rebind within
    -- this short bound; a persistent leak still fails.
    probeRebind port = retryBind (5 :: Int)
      where
        retryBind remaining =
          withInspectServer config {bindPort = port} mockEnv (const (pure ())) `catch` \e ->
            if isAlreadyInUseError e && remaining > 0
              then threadDelay 20000 >> retryBind (remaining - 1)
              else throwIO e
    config = defaultInspectServerConfig {bindPort = 0}
    threadBlock = newEmptyMVar >>= takeMVar :: IO ()
    upgradeHeaders = [("Connection", "Upgrade"), ("Upgrade", "websocket"), ("Sec-WebSocket-Key", "dGhlIHNhbXBsZSBub25jZQ=="), ("Sec-WebSocket-Version", "13")]
