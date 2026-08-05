module Main (main) where

import QueueNameSpec qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "pgmq-core"
      [ QueueNameSpec.tests
      ]
