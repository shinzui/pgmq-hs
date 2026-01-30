{-# LANGUAGE OverloadedStrings #-}

-- | Common test utilities for pgmq-hasql tests
module TestUtils
  ( -- * Session helpers
    runSession,
    assertSession,
    assertSessionFails,

    -- * Queue helpers
    cleanupQueue,

    -- * Assertion helpers
    assertRight,
    assertJust,
  )
where

import Hasql.Pool qualified as Pool
import Hasql.Session (Session)
import Pgmq.Hasql.Sessions qualified as Sessions
import Pgmq.Types (QueueName)
import Test.Tasty.HUnit (assertFailure)

-- | Run a session against a pool
runSession :: Pool.Pool -> Session a -> IO (Either Pool.UsageError a)
runSession = Pool.use

-- | Run a session and fail the test if it errors
assertSession :: Pool.Pool -> Session a -> IO a
assertSession p session = do
  result <- runSession p session
  case result of
    Left err -> assertFailure $ "Session failed: " <> show err
    Right a -> pure a

-- | Assert that a session fails
assertSessionFails :: Pool.Pool -> Session a -> IO ()
assertSessionFails p session = do
  result <- runSession p session
  case result of
    Left _ -> pure ()
    Right _ -> assertFailure "Expected session to fail but it succeeded"

-- | Clean up a queue by dropping it (ignores errors)
cleanupQueue :: Pool.Pool -> QueueName -> IO ()
cleanupQueue p qName = do
  _ <- runSession p (Sessions.dropQueue qName)
  pure ()

-- | Assert that an Either is Right and return the value
assertRight :: (Show e) => Either e a -> IO a
assertRight (Left err) = assertFailure $ "Expected Right but got Left: " <> show err
assertRight (Right a) = pure a

-- | Assert that a Maybe is Just and return the value
assertJust :: Maybe a -> IO a
assertJust Nothing = assertFailure "Expected Just but got Nothing"
assertJust (Just a) = pure a
