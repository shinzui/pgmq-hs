{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import AdvancedOpsSpec qualified
import AllFunctionsDecoderSpec qualified
import DecoderValidationSpec qualified
import EphemeralDb (withPgmqPool)
import MessageSpec qualified
import QueueSpec qualified
import RoundTripSpec qualified
import SchemaSpec qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  -- Run tests with a shared temporary database
  result <- withPgmqPool $ \pool -> do
    let tree =
          testGroup
            "pgmq-hasql"
            [ QueueSpec.tests pool,
              MessageSpec.tests pool,
              AdvancedOpsSpec.tests pool,
              SchemaSpec.tests pool,
              RoundTripSpec.tests pool,
              DecoderValidationSpec.tests pool,
              AllFunctionsDecoderSpec.tests pool
            ]
    defaultMain tree
  case result of
    Left err -> error $ "Failed to start temp database: " <> show err
    Right () -> pure ()
