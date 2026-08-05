{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import AdvancedOpsSpec qualified
import AliasingSpec qualified
import AllFunctionsDecoderSpec qualified
import DecoderValidationSpec qualified
import EphemeralDb (withPgmqDb)
import MessageSpec qualified
import NotifyChannelSpec qualified
import NotifyRaceSpec qualified
import NullBodySpec qualified
import NullSemanticsSpec qualified
import QueueSpec qualified
import RoundTripSpec qualified
import SchemaSpec qualified
import Test.Tasty (defaultMain, testGroup)
import TopicSpec qualified

main :: IO ()
main = do
  -- Run tests with a shared temporary database
  result <- withPgmqDb $ \pool db -> do
    let tree =
          testGroup
            "pgmq-hasql"
            [ QueueSpec.tests pool,
              MessageSpec.tests pool,
              AdvancedOpsSpec.tests pool,
              NullSemanticsSpec.tests pool,
              NullBodySpec.tests pool,
              NotifyRaceSpec.tests pool,
              -- Constructs mixed-case pgmq.meta rows, which poison listQueues
              -- decoding for every concurrent test — so it runs on its own
              -- dedicated PostgreSQL instance, never the shared pool.
              AliasingSpec.tests,
              -- Needs the Database handle: LISTEN/NOTIFY has no hasql API, so
              -- the round-trip test opens a raw libpq connection.
              NotifyChannelSpec.tests pool db,
              SchemaSpec.tests pool,
              RoundTripSpec.tests pool,
              DecoderValidationSpec.tests pool,
              AllFunctionsDecoderSpec.tests pool,
              TopicSpec.tests pool
            ]
    defaultMain tree
  case result of
    Left err -> error $ "Failed to start temp database: " <> show err
    Right () -> pure ()
