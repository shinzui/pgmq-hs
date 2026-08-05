{-# LANGUAGE OverloadedStrings #-}

-- | PGH-9: the documented LISTEN\/NOTIFY channel name was wrong everywhere.
--
-- The Haddock and the design note both claimed @pgmq_\<queue_name\>@, so anyone
-- following them listened on a channel that never receives anything. The real
-- channel is @pgmq.q_\<lowercased queue name\>.INSERT@, now computed by
-- 'notifyChannelName'.
--
-- This module pins the contract from both sides: a real notification arrives on
-- exactly the channel the helper computes, and a listener on the old documented
-- name receives nothing.
module NotifyChannelSpec (tests) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Database.PostgreSQL.LibPQ qualified as LibPQ
import EphemeralDb (Database, TestFixture (..), withTestFixture)
import EphemeralPg qualified as Pg
import Hasql.Pool qualified as Pool
import Pgmq.Hasql.Sessions qualified as Sessions
import Pgmq.Hasql.Statements.Types qualified as StmtTypes
import Pgmq.Types (MessageBody (..), QueueName, notifyChannelName, queueNameToText)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestUtils (assertSession, cleanupQueue)

tests :: Pool.Pool -> Database -> TestTree
tests p db =
  testGroup
    "Notification Channel Contract"
    [ testCase "a notification arrives on exactly notifyChannelName" $
        withTestFixture p $ \TestFixture {pool, queueName} -> do
          enableNotify pool queueName
          received <- withListener db (notifyChannelName queueName) $ \conn -> do
            sendProbe pool queueName
            awaitNotify conn 20
          case received of
            Nothing ->
              assertFailure $
                "expected a notification on " <> show (notifyChannelName queueName) <> " within 2s, got none"
            Just notification ->
              LibPQ.notifyRelname notification @?= TE.encodeUtf8 (notifyChannelName queueName)
          cleanupQueue pool queueName,
      testCase "nothing arrives on the old documented channel name" $
        withTestFixture p $ \TestFixture {pool, queueName} -> do
          enableNotify pool queueName
          received <- withListener db (legacyChannelName queueName) $ \conn -> do
            sendProbe pool queueName
            awaitNotify conn 10
          case received of
            Nothing -> pure ()
            Just notification ->
              assertFailure $
                "the old documented channel "
                  <> show (legacyChannelName queueName)
                  <> " received "
                  <> show (LibPQ.notifyRelname notification)
          cleanupQueue pool queueName
    ]

-- | The channel name this library's documentation claimed until 2026-08-05.
legacyChannelName :: QueueName -> Text
legacyChannelName qn = "pgmq_" <> queueNameToText qn

enableNotify :: Pool.Pool -> QueueName -> IO ()
enableNotify pool qn = do
  assertSession pool (Sessions.createQueue qn)
  assertSession pool $
    Sessions.enableNotifyInsert
      StmtTypes.EnableNotifyInsert
        { StmtTypes.queueName = qn,
          StmtTypes.throttleIntervalMs = Just 0 -- 0 = never throttle
        }

sendProbe :: Pool.Pool -> QueueName -> IO ()
sendProbe pool qn =
  ()
    <$ assertSession
      pool
      ( Sessions.sendMessage
          StmtTypes.SendMessage
            { StmtTypes.queueName = qn,
              StmtTypes.messageBody = MessageBody (Aeson.String "notify-probe"),
              StmtTypes.delay = Nothing
            }
      )

-- | Open a raw libpq connection (hasql 1.10 exposes no notification API) and
-- subscribe to @channel@. ephemeral-pg hands out connection strings as 'Text'
-- while libpq consumes 'ByteString', so both the conninfo and the command are
-- encoded explicitly.
withListener :: Database -> Text -> (LibPQ.Connection -> IO a) -> IO a
withListener db channel action =
  bracket (LibPQ.connectdb (TE.encodeUtf8 (Pg.connectionString db))) LibPQ.finish $ \conn -> do
    connStatus <- LibPQ.status conn
    unless (connStatus == LibPQ.ConnectionOk) $ do
      err <- LibPQ.errorMessage conn
      assertFailure $ "libpq connection failed: " <> show err
    -- The channel contains dots, so LISTEN needs the identifier double-quoted.
    result <- LibPQ.exec conn (TE.encodeUtf8 ("LISTEN " <> quoteIdentifier channel))
    case result of
      Nothing -> assertFailure "LISTEN returned no result"
      Just res -> do
        execStatus <- LibPQ.resultStatus res
        unless (execStatus == LibPQ.CommandOk) $
          assertFailure ("LISTEN failed with " <> show execStatus)
    action conn

quoteIdentifier :: Text -> Text
quoteIdentifier ident = "\"" <> T.replace "\"" "\"\"" ident <> "\""

-- | Poll for a notification, 100 ms per attempt.
awaitNotify :: LibPQ.Connection -> Int -> IO (Maybe LibPQ.Notify)
awaitNotify conn attempts
  | attempts <= 0 = pure Nothing
  | otherwise = do
      _ <- LibPQ.consumeInput conn
      pending <- LibPQ.notifies conn
      case pending of
        Just n -> pure (Just n)
        Nothing -> do
          threadDelay 100_000
          awaitNotify conn (attempts - 1)
