module Main (main) where

import ConfigSpec qualified
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import CorsSpec qualified
import EphemeralDb
import ErrorSpec qualified
import HttpSpec qualified
import IsolationSpec qualified
import ListenerSpec qualified
import LiveFeedSpec qualified
import OutboxSpec qualified
import RawWebSocketSpec qualified
import RefreshSpec qualified
import RouterSpec qualified
import ServerSpec qualified
import Test.Tasty
import WebSocketSpec qualified
import WireGoldenSpec qualified

main :: IO ()
main =
  defaultMain $
    testGroup
      "pgmq-inspect"
      [ ConfigSpec.tests,
        CorsSpec.tests,
        ErrorSpec.tests,
        RouterSpec.tests,
        WireGoldenSpec.tests,
        WebSocketSpec.tests,
        ListenerSpec.tests,
        OutboxSpec.tests,
        RawWebSocketSpec.tests,
        RefreshSpec.tests,
        ServerSpec.tests,
        withResource acquire release (\resource -> testGroup "Database" [HttpSpec.tests (snd <$> resource), WebSocketSpec.databaseTests (snd <$> resource), LiveFeedSpec.tests (snd <$> resource), IsolationSpec.tests (snd <$> resource)])
      ]
  where
    acquire = mask $ \restore -> do
      started <- newEmptyTMVarIO
      stop <- newEmptyTMVarIO
      worker <- async $ restore $ do
        result <- try @SomeException $ withPgmqDb $ \pool db -> do
          atomically (putTMVar started (Right (pool, db)))
          atomically (takeTMVar stop)
        case result of
          Left e -> atomically (voidTryPut started (Left e))
          Right (Left e) -> atomically (voidTryPut started (Left (toException (userError (show e)))))
          Right (Right ()) -> pure ()
      pair <- restore (atomically (takeTMVar started) >>= either throwIO pure) `onException` cancel worker
      pure ((worker, stop), pair)
    release ((worker, stop), _) = atomically (putTMVar stop ()) >> wait worker
    voidTryPut var value = () <$ tryPutTMVar var value
