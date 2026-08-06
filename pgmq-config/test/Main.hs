{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ConfigSpec qualified
import EphemeralDb (withPgmqDb)
import ForeignQueueSpec qualified
import NotifyCrashSpec qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  result <- withPgmqDb $ \pool -> do
    let tree =
          testGroup
            "pgmq-config"
            [ ConfigSpec.tests pool,
              -- NotifyCrashSpec manages its own PostgreSQL instance: it crashes
              -- the server, which the shared pool above could not survive.
              NotifyCrashSpec.tests,
              -- ForeignQueueSpec seeds a queue whose name parseQueueName
              -- rejects; on the shared pool above that row would fail every
              -- concurrent typed listQueues call, so it too runs on its own
              -- PostgreSQL instance.
              ForeignQueueSpec.tests
            ]
    defaultMain tree
  case result of
    Left err -> error $ "Failed to start temp database: " <> show err
    Right () -> pure ()
