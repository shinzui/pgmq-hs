{-# LANGUAGE OverloadedStrings #-}

module PlainInterpreterSpec (tests) where

import Effectful (runEff)
import Effectful.Error.Static (runError)
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as PoolConfig
import Pgmq.Effectful
  ( MessageId (..),
    MessageQuery (..),
    PgmqRuntimeError (..),
    deleteMessage,
    listQueues,
    parseQueueName,
    runPgmq,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

tests :: Pool.Pool -> TestTree
tests pool =
  testGroup
    "Plain interpreter error propagation"
    [ testCase "statement error surfaces PgmqSessionError" $ do
        bogus <- case parseQueueName "queue_that_does_not_exist_xyz" of
          Right q -> pure q
          Left err -> assertFailure ("could not build test queue name: " <> show err) >> fail ""
        result <-
          runEff
            . runError @PgmqRuntimeError
            . runPgmq pool
            $ deleteMessage
              MessageQuery
                { queueName = bogus,
                  messageId = MessageId 1
                }
        case result of
          Left (_cs, PgmqSessionError _) -> pure ()
          Left (_cs, other) ->
            assertFailure $
              "Expected PgmqSessionError, got " <> show other
          Right _ ->
            assertFailure
              "Expected an error deleting from missing queue, got success",
      testCase "connection error surfaces PgmqConnectionError" $ do
        let badCfg =
              PoolConfig.settings
                [ PoolConfig.size 1,
                  PoolConfig.staticConnectionSettings
                    "host=127.0.0.1 port=1 user=nobody dbname=nonexistent connect_timeout=1"
                ]
        badPool <- Pool.acquire badCfg
        result <-
          runEff
            . runError @PgmqRuntimeError
            . runPgmq badPool
            $ listQueues
        Pool.release badPool
        case result of
          Left (_cs, PgmqConnectionError _) -> pure ()
          Left (_cs, other) ->
            assertFailure $
              "Expected PgmqConnectionError, got " <> show other
          Right _ ->
            assertFailure
              "Expected a connection error, got success"
    ]
