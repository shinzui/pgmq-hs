{-# LANGUAGE OverloadedStrings #-}

-- | Partition silence, including native metadata aliases, with a live control.
module PartitionedNotifySpec (tests) where

import Control.Exception (finally)
import Control.Monad (forM_, unless)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Database.PostgreSQL.LibPQ qualified as PQ
import EphemeralDb (withPgmqDb)
import Hasql.Pool qualified as Pool
import Hasql.Session qualified as Session
import NotifyChannelSpec (awaitNotify, withListener)
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "PartitionedNotifySpec"
    [ testCase "partition inserts are silent and leave throttle at epoch" (partitionCase False False),
      testCase "native leaf metadata alias stays silent" (nativeOnly (partitionCase True False)),
      testCase "native leaf throttle alias stays silent and unchanged" (nativeOnly (partitionCase True True)),
      testCase "ordinary nonzero throttle suppresses second committed insert" ordinaryCase,
      testCase "native mixed-case missing lowercase metadata stays silent" (nativeOnly mixedCase)
    ]

nativeOnly :: IO () -> IO ()
nativeOnly action = do
  stock <- (== Just "1.12.0") <$> lookupEnv "PGMQ_TEST_SCHEMA_VERSION"
  if stock then putStrLn "SKIPPED: native alias hardening" else action

isolated :: (Pool.Pool -> (Text -> (PQ.Connection -> IO ()) -> IO ()) -> IO ()) -> IO ()
isolated action = do
  result <- withPgmqDb $ \pool db -> action pool (withListener db) `finally` Pool.release pool
  either (assertFailure . show) pure result

sql :: Pool.Pool -> Text -> IO ()
sql pool command = Pool.use pool (Session.script command) >>= either (assertFailure . show) pure

-- A separate listener subscription for each leaf, on the same live connection.
listen :: PQ.Connection -> Text -> IO ()
listen conn channel = do
  result <- PQ.exec conn (TE.encodeUtf8 ("LISTEN \"" <> T.replace "\"" "\"\"" channel <> "\""))
  case result of
    Nothing -> assertFailure "LISTEN returned no result"
    Just res -> PQ.resultStatus res >>= (@?= PQ.CommandOk)

partitionCase :: Bool -> Bool -> IO ()
partitionCase alias throttle = isolated $ \pool listener -> do
  available <- Pool.use pool (Session.script "DO $$ BEGIN IF NOT EXISTS (SELECT FROM pg_extension WHERE extname='pg_partman') THEN RAISE EXCEPTION 'pg_partman unavailable'; END IF; END $$")
  required <- (== Just "1") <$> lookupEnv "PGMQ_REQUIRE_PARTMAN"
  case available of
    Left err -> if required then assertFailure (show err) else putStrLn "SKIPPED: pg_partman unavailable"
    Right () -> do
      sql pool "SELECT pgmq.create_partitioned('pq','10','100'); SELECT pgmq.enable_notify_insert('pq',0); SELECT pgmq.create('control'); SELECT pgmq.enable_notify_insert('control',0)"
      if alias then sql pool "SELECT pgmq.create('pq_p0'); DO $$ BEGIN IF NOT (SELECT relispartition FROM pg_class WHERE oid='pgmq.q_pq_p0'::regclass) THEN RAISE EXCEPTION 'alias changed relation'; END IF; END $$" else pure ()
      if throttle then sql pool "INSERT INTO pgmq.notify_insert_throttle(queue_name,throttle_interval_ms,last_notified_at) VALUES ('pq_p0',0,to_timestamp(0))" else pure ()
      listener "pgmq.q_control.INSERT" $ \conn -> do
        result <- PQ.exec conn "SELECT 'pgmq.' || c.relname || '.INSERT' FROM pg_inherits i JOIN pg_class c ON c.oid=i.inhrelid WHERE i.inhparent='pgmq.q_pq'::regclass"
        case result of
          Nothing -> assertFailure "partition query failed"
          Just res -> do
            PQ.resultStatus res >>= (@?= PQ.TuplesOk)
            rows <- PQ.ntuples res
            forM_ [0 .. rows - 1] $ \row -> do
              channel <- PQ.getvalue res row 0
              maybe (assertFailure "null partition channel") (listen conn . TE.decodeUtf8) channel
        listen conn "pgmq.q_pq.INSERT"
        sql pool "SELECT pgmq.send('control','{}')"
        control <- awaitNotify conn 20
        fmap PQ.notifyRelname control @?= Just "pgmq.q_control.INSERT"
        forM_ [1 :: Int .. 3] $ \_ -> sql pool "SELECT pgmq.send('pq','{}')"
        received <- awaitNotify conn 10
        fmap PQ.notifyRelname received @?= Nothing
      sql pool "DO $$ BEGIN IF (SELECT count(*) FROM pgmq.q_pq) <> 3 THEN RAISE EXCEPTION 'messages not stored'; END IF; IF EXISTS (SELECT FROM pgmq.notify_insert_throttle WHERE queue_name IN ('pq','pq_p0') AND last_notified_at <> to_timestamp(0)) THEN RAISE EXCEPTION 'partition throttle advanced'; END IF; END $$"

ordinaryCase :: IO ()
ordinaryCase = isolated $ \pool listener -> do
  sql pool "SELECT pgmq.create('ordinary'); SELECT pgmq.enable_notify_insert('ordinary',3600000)"
  listener "pgmq.q_ordinary.INSERT" $ \conn -> do
    sql pool "SELECT pgmq.send('ordinary','{}')"
    first <- awaitNotify conn 20
    fmap PQ.notifyRelname first @?= Just "pgmq.q_ordinary.INSERT"
    sql pool "CREATE TABLE public.throttle_before AS SELECT last_notified_at FROM pgmq.notify_insert_throttle WHERE queue_name='ordinary'"
    sql pool "SELECT pgmq.send('ordinary','{}')"
    second <- awaitNotify conn 10
    fmap PQ.notifyRelname second @?= Nothing
    sql pool "DO $$ BEGIN IF (SELECT last_notified_at FROM pgmq.notify_insert_throttle WHERE queue_name='ordinary') <> (SELECT last_notified_at FROM public.throttle_before) THEN RAISE EXCEPTION 'suppressed insert advanced timestamp'; END IF; END $$"
    sql pool "DO $$ BEGIN IF (SELECT count(*) FROM pgmq.q_ordinary) <> 2 THEN RAISE EXCEPTION 'messages not stored'; END IF; END $$"

mixedCase :: IO ()
mixedCase = isolated $ \pool listener -> do
  sql pool "SELECT pgmq.create('MixedNotify'); SELECT pgmq.enable_notify_insert('MixedNotify',0)"
  listener "pgmq.q_mixednotify.INSERT" $ \conn -> do
    sql pool "SELECT pgmq.send('MixedNotify','{}')"
    received <- awaitNotify conn 10
    unless (fmap PQ.notifyRelname received == Nothing) (assertFailure "mixed-case queue notified without matching metadata")
