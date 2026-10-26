{-# LANGUAGE OverloadedStrings #-}

-- | Checked LISTEN transport for deployed-upgrade acceptance.
module NotifyListener (withListener, awaitNotify) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Database.PostgreSQL.LibPQ qualified as LibPQ
import Test.Tasty.HUnit (assertFailure, (@?=))

withListener :: Text -> Text -> (LibPQ.Connection -> IO a) -> IO a
withListener conninfo channel action =
  bracket (LibPQ.connectdb (TE.encodeUtf8 conninfo)) LibPQ.finish $ \conn -> do
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
      ok <- LibPQ.consumeInput conn
      unless ok (assertFailure "libpq consumeInput failed")
      LibPQ.status conn >>= (@?= LibPQ.ConnectionOk)
      pending <- LibPQ.notifies conn
      case pending of
        Just n -> pure (Just n)
        Nothing -> do
          threadDelay 100_000
          awaitNotify conn (attempts - 1)
