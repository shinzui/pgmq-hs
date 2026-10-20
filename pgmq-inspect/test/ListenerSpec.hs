module ListenerSpec (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (cancel, withAsync)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad (forM_, replicateM, void)
import EphemeralDb (withPgmqDb)
import EphemeralPg qualified as Pg
import Hasql.Decoders qualified as D
import Hasql.Pool qualified as Pool
import Hasql.Session qualified as Session
import Hasql.Statement (unpreparable)
import HttpSpec (session)
import Pgmq.Inspect.Listener
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

waitSTM :: STM a -> IO a
waitSTM action = timeout 2000000 (atomically action) >>= maybe (assertFailure "listener acknowledgement timed out") pure

backendCount :: Pool.Pool -> IO Int
backendCount pool = fromIntegral <$> session pool (Session.statement () (unpreparable "select count(*)::int from pg_stat_activity where application_name = 'pgmq-inspect-listener'" mempty (D.singleRow (D.column (D.nonNullable D.int4)))))

waitNoBackend :: Pool.Pool -> IO ()
waitNoBackend pool = timeout 2000000 loop >>= maybe (assertFailure "listener backend leaked") pure
  where
    loop = backendCount pool >>= \n -> if n == 0 then pure () else threadDelay 10000 >> loop

tests :: TestTree
tests =
  testGroup
    "WebSocket listener ownership"
    [ testCase "refcounted channels use one backend and UNLISTEN after last subscriber" $ do
        result <- withPgmqDb $ \pool database -> withListener (Pg.connectionSettings database) $ \listener -> do
          delivered <- newTVarIO (0 :: Int)
          keys <- replicateM 10 (subscribeChannel listener "inspect_test" (modifyTVar' delivered (+ 1)))
          waitSTM (listenerAcknowledged listener "inspect_test" >>= check)
          backendCount pool >>= (@?= 1)
          atomically (listenerChannelCount listener) >>= (@?= 1)
          unsubscribeChannel listener "inspect_test" (keys !! 0)
          session pool (Session.script "NOTIFY inspect_test")
          waitSTM (readTVar delivered >>= check . (== 9))
          forM_ (drop 1 keys) (unsubscribeChannel listener "inspect_test")
          waitSTM (listenerAcknowledged listener "inspect_test" >>= check . not)
          atomically (listenerChannelCount listener) >>= (@?= 0)
          session pool (Session.script "NOTIFY inspect_test")
          threadDelay 20000
          readTVarIO delivered >>= (@?= 9)
        either (assertFailure . show) pure result,
      testCase "cancellation during fd wait clears health, registrations and backend" $ do
        result <- withPgmqDb $ \pool database -> forM_ [1 .. 10 :: Int] $ \_ -> do
          ready <- newEmptyMVar
          withAsync (withListener (Pg.connectionSettings database) (\listener -> putMVar ready listener >> (newEmptyMVar >>= takeMVar))) $ \owner -> do
            listener <- takeMVar ready
            void (subscribeChannel listener "inspect_cancel" (pure ()))
            waitSTM (listenerAcknowledged listener "inspect_cancel" >>= check)
            cancel owner
            atomically (listenerHealthy listener) >>= (@?= False)
            atomically (listenerChannelCount listener) >>= (@?= 0)
            waitNoBackend pool
        either (assertFailure . show) pure result,
      testCase "buffered notification batches do not starve new channel acknowledgement" $ do
        result <- withPgmqDb $ \pool database -> withListener (Pg.connectionSettings database) $ \listener -> do
          delivered <- newTVarIO (0 :: Int)
          void (subscribeChannel listener "inspect_flood" (modifyTVar' delivered (+ 1)))
          waitSTM (listenerAcknowledged listener "inspect_flood" >>= check)
          session pool (Session.script "DO $$ BEGIN FOR i IN 1..300 LOOP PERFORM pg_notify('inspect_flood', i::text); END LOOP; END $$")
          void (subscribeChannel listener "inspect_new" (pure ()))
          waitSTM (listenerAcknowledged listener "inspect_new" >>= check)
          waitSTM (readTVar delivered >>= check . (== 300))
        either (assertFailure . show) pure result
    ]
