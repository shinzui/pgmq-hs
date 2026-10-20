module CorsSpec (tests) where

import Data.List.NonEmpty (NonEmpty (..))
import Pgmq.Inspect.Config
import RouterSpec (mockEnv)
import Test.Tasty
import Test.Tasty.HUnit
import TestServer

tests :: TestTree
tests =
  testGroup
    "CORS"
    [ testCase "no CORS headers when disabled" $ withTestServer defaultInspectConfig mockEnv $ \port -> do
        r <- getWith port "GET" [("Origin", "http://ui.example")] "/queues"
        replyStatus r @?= 200
        lookup "Access-Control-Allow-Origin" (replyHeaders r) @?= Nothing,
      testCase "a listed origin gets Access-Control-Allow-Origin and preflight" $ withTestServer cfg mockEnv $ \port -> do
        r <- getWith port "GET" [("Origin", "http://ui.example")] "/queues"
        lookup "Access-Control-Allow-Origin" (replyHeaders r) @?= Just "http://ui.example"
        lookup "Vary" (replyHeaders r) @?= Just "Origin"
        lookup "Access-Control-Allow-Credentials" (replyHeaders r) @?= Nothing
        preflight <- getWith port "OPTIONS" [("Origin", "http://ui.example"), ("Access-Control-Request-Method", "GET")] "/queues"
        replyStatus preflight @?= 200
        assertBool "allows GET" (lookup "Access-Control-Allow-Methods" (replyHeaders preflight) /= Nothing),
      testCase "an unlisted origin gets none" $ withTestServer cfg mockEnv $ \port -> do
        r <- getWith port "GET" [("Origin", "http://other.example")] "/queues"
        replyStatus r @?= 200
        lookup "Access-Control-Allow-Origin" (replyHeaders r) @?= Nothing
    ]
  where
    cfg = defaultInspectConfig {corsPolicy = CorsAllowOrigins ("http://ui.example" :| [])}
