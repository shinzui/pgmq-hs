-- | Reproducible guide transcript, using only a disposable database. Set
-- PGMQ_INSPECT_RECORD_TRANSCRIPT to retain the successful curl/WS conversation.
module SmokeSpec (tests) where

import Control.Monad (forM)
import Data.Aeson (Value (..), eitherDecode)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import EphemeralDb
import EphemeralPg qualified as Pg
import Hasql.Session qualified as Session
import HttpSpec (session)
import Network.WebSockets qualified as WS
import RouterSpec (lookupKey)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO (hGetLine)
import System.Process
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testCase "standalone guide curl/WS transcript and Ctrl-C cleanup" $ do
  result <- withPgmqDb $ \owner db -> do
    session owner (Session.script "SELECT pgmq.create('demo'); SELECT pgmq.send('demo','{\"hello\":\"world\"}'); SELECT pgmq.send('demo','{\"hello\":\"world\"}'); SELECT pgmq.send('demo','{\"hello\":\"world\"}'); SELECT pgmq.archive('demo',1); SELECT pgmq.bind_topic('demo.*','demo'); SELECT pgmq.enable_notify_insert('demo',250)")
    let p =
          (proc "pgmq-inspect" ["--database-url", T.unpack (Pg.connectionString db), "--port", "0", "--no-listener", "--cors-origin", "http://localhost:5173"])
            { std_out = CreatePipe,
              create_group = True
            }
    withCreateProcess p $ \_ output _ child -> do
      startup <- maybe (assertFailure "no stdout pipe") (\h -> timeout 10000000 (hGetLine h) >>= maybe (assertFailure "startup timeout") pure) output
      let port = read (reverse (takeWhile (/= ':') (reverse startup)))
          base = "http://127.0.0.1:" <> show (port :: Int)
      curls <- forM ["/", "/queues", "/queues/demo", "/queues/demo/metrics", "/metrics", "/queues/demo/messages?limit=1", "/queues/demo/messages?from=2&limit=1", "/queues/demo/messages/2", "/queues/demo/archive", "/queues/demo/archive/1", "/queues/demo/bindings", "/bindings", "/routing/test?routing_key=demo.created", "/notify/throttles", "/health/live", "/health/ready", "/openapi.json", "/queues/demo/messages?from=bad"] $ \path -> do
        (code, body, err) <- readProcessWithExitCode "curl" ["-sS", base <> path] ""
        assertEqual err ExitSuccess code
        case eitherDecode (LBS.pack body) :: Either String Value of
          Left e -> assertFailure e
          Right _ -> pure ()
        pure ("$ curl -sS \"$base" <> path <> "\"\n" <> body <> "\n")
      (corsCode, cors, corsErr) <- readProcessWithExitCode "curl" ["-sS", "-i", "-H", "Origin: http://localhost:5173", base <> "/health/live"] ""
      assertEqual corsErr ExitSuccess corsCode
      frames <- WS.runClient "127.0.0.1" port "/ws" $ \connection -> do
        WS.sendTextData connection ("{\"type\":\"subscribe\",\"queue\":\"demo\"}" :: Text)
        snapshot <- WS.receiveData connection
        row <- either assertFailure pure (eitherDecode snapshot :: Either String Value)
        lookupKey "type" row @?= Just (String "snapshot")
        lookupKey "push" row @?= Just (Bool False)
        WS.sendTextData connection ("{\"type\":\"ping\"}" :: Text)
        pong <- WS.receiveData connection
        interruptProcessGroupOf child
        goodbye <- timeout 3000000 (WS.receiveData connection) >>= maybe (assertFailure "no Ctrl-C goodbye") pure
        final <- either assertFailure pure (eitherDecode goodbye :: Either String Value)
        lookupKey "type" final @?= Just (String "goodbye")
        pure (unlines ["> {\"type\":\"subscribe\",\"queue\":\"demo\"}", LBS.unpack snapshot, "> {\"type\":\"ping\"}", LBS.unpack pong, "> Ctrl-C to server", LBS.unpack goodbye])
      exited <- timeout 5000000 (waitForProcess child)
      assertBool "process exited" (maybe False (const True) exited)
      lookupEnv "PGMQ_INSPECT_RECORD_TRANSCRIPT" >>= mapM_ (\path -> writeFile path (startup <> "\n" <> concat curls <> "$ curl -sS -i -H 'Origin: http://localhost:5173' \"$base/health/live\"\n" <> filter (/= '\r') cors <> "\nWebSocket /ws:\n" <> frames))
  either (assertFailure . show) pure result
