module WireGoldenSpec (tests) where

import Data.Aeson (encode)
import Pgmq.Inspect.Error (ErrorBody (..), invalidCursor)
import Pgmq.Inspect.Wire
import Pgmq.Types (MessageId (..))
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.Golden (NoCreateFile (..), goldenVsString)

tests :: TestTree
tests =
  localOption (NoCreateFile True) $
    testGroup
      "Wire shapes (golden)"
      [ goldenVsString "page with a next cursor" "test/golden/page-more.json" $
          pure (encode (Page [1 :: Int, 2, 3] (Just (MessageId 3)))),
        goldenVsString "last page omits next_cursor" "test/golden/page-last.json" $
          pure (encode (Page [4 :: Int] Nothing)),
        goldenVsString "error envelope with details" "test/golden/error-invalid-cursor.json" $
          pure (encode (invalidCursor "x")),
        goldenVsString "error envelope without details" "test/golden/error-plain.json" $
          pure (encode (ErrorBody "not_found" "No route matches this path." Nothing)),
        goldenVsString "service descriptor" "test/golden/descriptor.json" $
          pure (encode (ServiceDescriptor "pgmq-inspect" "0.1.0.0" "ws" "openapi.json")),
        goldenVsString "liveness" "test/golden/health-live.json" $
          pure (encode (LivenessBody True)),
        goldenVsString "readiness" "test/golden/health-ready.json" $
          pure (encode (ReadinessBody False [DependencyCheck "postgres" False (Just 12) (Just "timed out")]))
      ]
