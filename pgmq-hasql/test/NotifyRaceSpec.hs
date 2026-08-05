{-# LANGUAGE OverloadedStrings #-}

-- | PGH-8: two replicas reconciling the same configuration concurrently must not
-- collide on notification setup.
--
-- @pgmq.enable_notify_insert@ takes no lock and its @CREATE CONSTRAINT TRIGGER@
-- has no @IF NOT EXISTS@. Two callers can both pass the internal
-- @DROP TRIGGER IF EXISTS@ (a no-op on a fresh queue, so neither locks the
-- table); the second then blocks on the throttle-row unique constraint until the
-- first commits, resumes, and creates a trigger that now already exists —
-- SQLSTATE 42710, failing that replica's whole startup reconcile.
module NotifyRaceSpec (tests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Exception (SomeException, try)
import Data.Text qualified as T
import Data.Word (Word32)
import Hasql.Pool qualified as Pool
import Hasql.Session (Session)
import Pgmq.Hasql.Sessions qualified as Sessions
import Pgmq.Hasql.Statements.Types qualified as StmtTypes
import Pgmq.Types (QueueName, parseQueueName)
import System.Random (randomRIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

-- | How many fresh queues to race over. The losing caller only errors when it
-- passes the internal DROP before the winner commits its CREATE, which is a
-- narrow window — measured at roughly one collision per hundred calls — so the
-- count is deliberately generous.
iterations :: Int
iterations = 200

tests :: Pool.Pool -> TestTree
tests pool =
  testGroup
    "Notification Enable Race"
    [ testCase ("concurrent enable_notify_insert never raises 42710 (n=" <> show iterations <> ")") $ do
        failures <- concat <$> traverse (const (raceOnce pool)) [1 .. iterations]
        case filter isDuplicateObject failures of
          [] -> pure ()
          duplicates@(firstDuplicate : _) ->
            assertFailure $
              show (length duplicates)
                <> " of "
                <> show (2 * iterations)
                <> " concurrent enable_notify_insert calls failed with duplicate_object (42710). First: "
                <> firstDuplicate
    ]

-- | Create a fresh queue, enable notify on it from two connections at once, and
-- return whatever went wrong.
raceOnce :: Pool.Pool -> IO [String]
raceOnce pool = do
  qn <- genQueueName
  created <- runSession pool (Sessions.createQueue qn)
  case created of
    Left err -> pure [err]
    Right () -> do
      gate <- newEmptyMVar
      leftSlot <- newEmptyMVar
      rightSlot <- newEmptyMVar
      let enable slot = do
            () <- readMVar gate
            result <- runSession pool (enableSession qn)
            putMVar slot result
      _ <- forkIO (enable leftSlot)
      _ <- forkIO (enable rightSlot)
      putMVar gate ()
      leftResult <- takeMVar leftSlot
      rightResult <- takeMVar rightSlot
      _ <- runSession pool (() <$ Sessions.dropQueue qn)
      pure [err | Left err <- [leftResult, rightResult]]

enableSession :: QueueName -> Session ()
enableSession qn =
  Sessions.enableNotifyInsert
    StmtTypes.EnableNotifyInsert
      { StmtTypes.queueName = qn,
        StmtTypes.throttleIntervalMs = Just 0
      }

-- | Run a session, flattening both pool errors and thrown exceptions into a
-- printable failure so a forked thread can never leave its 'MVar' empty.
runSession :: Pool.Pool -> Session a -> IO (Either String a)
runSession pool session = do
  outcome <- try (Pool.use pool session)
  pure $ case outcome of
    Left (e :: SomeException) -> Left (show e)
    Right (Left usageError) -> Left (show usageError)
    Right (Right a) -> Right a

-- | hasql renders the SQLSTATE into the shown 'Pool.UsageError'.
isDuplicateObject :: String -> Bool
isDuplicateObject = T.isInfixOf "42710" . T.pack

genQueueName :: IO QueueName
genQueueName = do
  suffix <- randomRIO (10000 :: Word32, 99999)
  case parseQueueName ("race_test_" <> T.pack (show suffix)) of
    Left err -> error $ "Failed to generate queue name: " <> show err
    Right qn -> pure qn
