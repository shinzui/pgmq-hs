{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ConfigSpec qualified
import EphemeralDb (withPgmqDb)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  result <- withPgmqDb $ \pool -> do
    let tree =
          testGroup
            "pgmq-config"
            [ ConfigSpec.tests pool
            ]
    defaultMain tree
  case result of
    Left err -> error $ "Failed to start temp database: " <> show err
    Right () -> pure ()
