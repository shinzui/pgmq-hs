{-# LANGUAGE OverloadedStrings #-}

-- | Schema-selectable isolated foreign-name fixtures for each backend.
module CollisionSpec (tests) where

import Control.Monad (forM_)
import Data.Text (Text)
import Hasql.Pool qualified as Pool
import Hasql.Session qualified as Session
import PartitionSpec (backends, isolated, reconcile, right, session)
import Pgmq.Config
import Pgmq.Types (parseQueueName, parseTopicPattern)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests =
  testGroup
    "CollisionSpec"
    [ testGroup
        (if backend then "effectful" else "direct")
        [ testCase "single alias blocks every declared sub-resource, including repeat" (collision backend ["Billing_Events"]),
          testCase "multiple aliases block every declared sub-resource, including repeat" (collision backend ["Billing_Events", "BILLING_EVENTS"])
        ]
    | backend <- backends
    ]

collision :: Bool -> [Text] -> IO ()
collision backend names = isolated $ \pool -> do
  forM_ names $ \name -> sql pool ("SELECT pgmq.create('" <> name <> "')")
  q <- right (parseQueueName "billing_events")
  other <- right (parseQueueName "other")
  pat <- right (parseTopicPattern "billing.*")
  let config = withTopicBinding pat (withFifoIndex (withNotifyInsert (Just 0) (standardQueue q)))
  forM_ [1 :: Int, 2] $ \_ -> do
    _ <- right =<< reconcile backend pool [config, standardQueue other]
    sql pool ("DO $$ BEGIN IF (SELECT count(*) FROM pgmq.meta WHERE lower(queue_name)='billing_events') <> " <> (if length names == 1 then "1" else "2") <> " THEN RAISE EXCEPTION 'collision inserted metadata alias'; END IF; END $$")
    sql pool "DO $$ BEGIN IF EXISTS (SELECT FROM pgmq.notify_insert_throttle WHERE queue_name='billing_events') OR to_regclass('pgmq.q_billing_events_fifo_idx') IS NOT NULL OR EXISTS (SELECT FROM pgmq.topic_bindings WHERE queue_name='billing_events') THEN RAISE EXCEPTION 'collision mutated sub-resources'; END IF; IF NOT EXISTS (SELECT FROM pgmq.meta WHERE queue_name='other') THEN RAISE EXCEPTION 'unrelated queue blocked'; END IF; END $$"

sql :: Pool.Pool -> Text -> IO ()
sql pool command = session pool (Session.script command)
