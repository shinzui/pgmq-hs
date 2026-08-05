{-# LANGUAGE OverloadedStrings #-}

-- | PGH-7: queue names must be rejected consistently at every entry path.
--
-- The unexported @QueueName@ constructor leaves exactly two runtime ways to
-- build one: 'parseQueueName' and 'Data.Aeson.FromJSON'. Both must enforce the
-- same contract — non-empty, at most 47 characters, lowercase ASCII letters,
-- digits, and underscore only. Lowercase-only matters because pgmq's SQL
-- lowercases physical table names while @pgmq.meta@ stores the caller's
-- original casing: a mixed-case name aliases another queue's physical table
-- and configures notification throttles the trigger can never match. The
-- @FromJSON@ instance was previously newtype-derived and accepted anything,
-- which is the bypass these tests pin shut.
module QueueNameSpec (tests) where

import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as T
import Pgmq.Types (PgmqError (..), QueueName, parseQueueName, queueNameToText)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "QueueName validation"
    [ parseAcceptance,
      parseRejection,
      jsonPath
    ]

parseAcceptance :: TestTree
parseAcceptance =
  testGroup
    "parseQueueName accepts"
    [ testCase "a typical lowercase name" $ do
        qn <- assertParses "my_queue_123"
        assertEqual "round-trips through queueNameToText" "my_queue_123" (queueNameToText qn),
      testCase "a 47-character lowercase name (the maximum)" $ do
        qn <- assertParses (T.replicate 47 "a")
        assertEqual "length preserved" 47 (T.length (queueNameToText qn))
    ]

parseRejection :: TestTree
parseRejection =
  testGroup
    "parseQueueName rejects"
    [ testCase "an uppercase name" $
        assertRejects "MyQueue" "invalid characters",
      testCase "the empty string" $
        assertRejects "" "empty",
      testCase "a 48-character name" $
        assertRejects (T.replicate 48 "a") "too long",
      testCase "a hyphenated name" $
        assertRejects "bad-name" "invalid characters",
      testCase "a name with punctuation" $
        assertRejects "queue!" "invalid characters"
    ]

jsonPath :: TestTree
jsonPath =
  testGroup
    "FromJSON validates via parseQueueName"
    [ testCase "a lowercase name decodes and round-trips through ToJSON" $
        case Aeson.fromJSON (Aeson.String "myqueue") :: Aeson.Result QueueName of
          Aeson.Error err -> assertFailure $ "Expected Success, got Error: " <> err
          Aeson.Success qn -> assertEqual "ToJSON round-trip" (Aeson.String "myqueue") (Aeson.toJSON qn),
      testCase "an uppercase name is rejected" $ assertJsonRejects "MyQueue",
      testCase "the empty string is rejected" $ assertJsonRejects "",
      testCase "an overlong name is rejected" $ assertJsonRejects (T.replicate 60 "x"),
      testCase "a hyphenated name is rejected" $ assertJsonRejects "bad-name"
    ]

assertParses :: Text -> IO QueueName
assertParses t =
  case parseQueueName t of
    Left err -> assertFailure $ "Expected Right, got: " <> show err
    Right qn -> pure qn

-- | The rejection must be an 'InvalidQueueName' whose message names the actual
-- problem, so callers surface something diagnosable.
assertRejects :: Text -> String -> IO ()
assertRejects t expectedFragment =
  case parseQueueName t of
    Right _ -> assertFailure $ "Expected rejection of " <> show t
    Left err@(InvalidQueueName msg) ->
      assertBool
        ("Expected message mentioning " <> show expectedFragment <> ", got: " <> show err)
        (T.pack expectedFragment `T.isInfixOf` msg)
    Left err -> assertFailure $ "Expected InvalidQueueName, got: " <> show err

assertJsonRejects :: Text -> IO ()
assertJsonRejects t =
  case Aeson.fromJSON (Aeson.String t) :: Aeson.Result QueueName of
    Aeson.Error _ -> pure ()
    Aeson.Success qn ->
      assertFailure $ "Expected FromJSON rejection of " <> show t <> ", got: " <> show (queueNameToText qn)
