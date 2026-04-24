{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ClassificationSpec qualified
import EphemeralDb (withPgmqPool)
import PlainInterpreterSpec qualified
import Test.Tasty (defaultMain, testGroup)
import TracedInterpreterSpec qualified

main :: IO ()
main = do
  result <- withPgmqPool $ \pool -> do
    let tree =
          testGroup
            "pgmq-effectful"
            [ ClassificationSpec.tests,
              PlainInterpreterSpec.tests pool,
              TracedInterpreterSpec.tests pool
            ]
    defaultMain tree
  case result of
    Left err -> error $ "Failed to start temp database: " <> show err
    Right () -> pure ()
