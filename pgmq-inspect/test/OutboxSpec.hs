module OutboxSpec (tests) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad (forM_)
import Data.Aeson (encode)
import Data.Map.Strict qualified as Map
import Data.Unique (newUnique)
import Pgmq.Inspect.Frames
import Pgmq.Inspect.Outbox
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "WebSocket outbox"
    [ testGroup
        "bounded outbox"
        [ testCase ("capacity " <> show capacity <> " drops oldest and signals before retained frames") $ do
            box <- newOutbox capacity
            writerBarrier <- newEmptyTMVarIO
            withAsync (atomically (takeTMVar writerBarrier) >> atomically (takeOutgoing box)) $ \writer -> do
              atomically $ forM_ ["first", "second", "third"] $ \code -> enqueue box (ErrorFrame code "test" Nothing)
              atomically (queuedFrames box) >>= (@?= capacity)
              atomically (putTMVar writerBarrier ())
              (lost, first) <- wait writer
              lost @?= True
              let expected = if capacity == 1 then "third" else "second"
              encode first @?= encode (ErrorFrame expected "test" Nothing)
              if capacity == 2
                then do
                  (again, retained) <- atomically (takeOutgoing box)
                  again @?= False
                  encode retained @?= encode (ErrorFrame "third" "test" Nothing)
                else pure ()
              atomically (queuedFrames box) >>= (@?= 0)
        | capacity <- [1, 2]
        ],
      testGroup
        "generation tags"
        [ testCase "unsubscribed generations are discarded by the writer" $ do
            box <- newOutbox 2
            old <- newUnique
            generations <- newTVarIO (Map.singleton "orders" old)
            atomically $ do
              enqueueTagged box (Just (generations, "orders", old)) Pong
              writeTVar generations Map.empty
              enqueue box Goodbye
            (_, frame) <- atomically (takeOutgoing box)
            encode frame @?= encode Goodbye
        ]
    ]
