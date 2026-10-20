module Main (main) where

import JsonGoldenSpec qualified
import QueueNameSpec qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "pgmq-core"
      [ JsonGoldenSpec.tests,
        QueueNameSpec.tests
      ]
