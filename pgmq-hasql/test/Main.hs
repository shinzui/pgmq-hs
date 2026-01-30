{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import MessageSpec qualified
import QueueSpec qualified
import Test.Tasty (defaultMain, testGroup)
import TmpPostgres (withPgmqPool)

main :: IO ()
main = do
  -- Run tests with a shared temporary database
  result <- withPgmqPool $ \pool -> do
    let tree =
          testGroup
            "pgmq-hasql"
            [ QueueSpec.tests pool,
              MessageSpec.tests pool
              -- Note: MetricsSpec is temporarily disabled due to ambiguous column issue in pgmq.metrics
            ]
    defaultMain tree
  case result of
    Left err -> error $ "Failed to start temp database: " <> show err
    Right () -> pure ()
