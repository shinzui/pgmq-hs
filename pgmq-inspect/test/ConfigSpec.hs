module ConfigSpec (tests) where

import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import Pgmq.Inspect.Config
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Checked configuration"
    [ testCase "defaults are valid" $ validateInspectConfig defaultInspectConfig @?= Right defaultInspectConfig,
      testGroup
        "invalid values rejected before allocation"
        [ testCase label $ assertBool label (isLeft (validateInspectConfig cfg))
        | (label, cfg) <-
            [ ("zero default", defaultInspectConfig {defaultPageLimit = 0}),
              ("negative default", defaultInspectConfig {defaultPageLimit = -1}),
              ("default above max", defaultInspectConfig {defaultPageLimit = 501}),
              ("zero max", defaultInspectConfig {maxPageLimit = 0}),
              ("lookahead overflow", defaultInspectConfig {maxPageLimit = maxBound}),
              ("zero outbox", defaultInspectConfig {wsQueueCapacity = 0}),
              ("zero reads", defaultInspectConfig {maxConcurrentReads = 0}),
              ("negative interval", defaultInspectConfig {pollIntervalUs = -1}),
              ("wildcard", defaultInspectConfig {corsPolicy = CorsAllowOrigins ("*" :| [])}),
              ("wildcard host", defaultInspectConfig {corsPolicy = CorsAllowOrigins ("http://*" :| [])}),
              ("credentials", defaultInspectConfig {corsPolicy = CorsAllowOrigins ("http://user@host" :| [])}),
              ("origin path", defaultInspectConfig {corsPolicy = CorsAllowOrigins ("http://host/path" :| [])}),
              ("bad port", defaultInspectConfig {corsPolicy = CorsAllowOrigins ("http://host:65536" :| [])})
            ]
        ]
    ]
