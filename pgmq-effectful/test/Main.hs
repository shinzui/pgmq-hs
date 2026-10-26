{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ClassificationSpec qualified
import DisconnectSpec qualified
import EphemeralDb (ensureOwnerOnlyRoot, withPgmqPool)
import PlainInterpreterSpec qualified
import RootPermissionsSpec qualified
import Test.Tasty (defaultMain, testGroup)
import TracedInterpreterSpec qualified
import UmbrellaExportsSpec ()

main :: IO ()
main = do
  result <- withPgmqPool $ \pool -> do
    let tree =
          testGroup
            "pgmq-effectful"
            [ RootPermissionsSpec.tests ensureOwnerOnlyRoot,
              ClassificationSpec.tests,
              PlainInterpreterSpec.tests pool,
              TracedInterpreterSpec.tests pool,
              DisconnectSpec.tests
            ]
    defaultMain tree
  case result of
    Left err -> error $ "Failed to start temp database: " <> show err
    Right () -> pure ()
