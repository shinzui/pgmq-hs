{-# LANGUAGE OverloadedStrings #-}

-- | Pins the JSON wire shape of every pgmq-core record that carries an aeson
-- instance. The golden files under @test/golden/@ are the human-readable
-- statement of the contract in @docs/design/020-json-wire-encodings.md@: a
-- renamed or re-typed field changes the bytes and fails here. Regenerate
-- with @--accept@ only after deciding, in the changelog, that the change is
-- additive.
module JsonGoldenSpec (tests) where

import Data.Aeson (ToJSON (..), Value (..), decode, eitherDecode, encode, object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Either (isLeft)
import Data.Int (Int64)
import Data.Time (UTCTime (..), fromGregorian)
import Pgmq.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "JSON wire encodings"
    [ goldens,
      decoding,
      testGroup "all header JSON forms" (map headerCase headerValues),
      testCase "Queue and UnvalidatedQueue share bytes" $
        assertEqual "same encoding" (encode sampleQueue) (encode (UnvalidatedQueue "orders" fractionalTime False False)),
      testGroup "exact int64 positions" (map integerCase [9007199254740993, maxBound])
    ]

-- | 2026-08-19T12:34:56.123Z: a fractional second, so the golden shows how
-- aeson renders one (trailing zeros trimmed).
fractionalTime :: UTCTime
fractionalTime = UTCTime (fromGregorian 2026 8 19) 45296.123

-- | 2026-08-19T12:35:00Z: no fraction, so the golden shows the plain form.
wholeTime :: UTCTime
wholeTime = UTCTime (fromGregorian 2026 8 19) 45300

sampleQueueName :: QueueName
sampleQueueName = either (error . show) id (parseQueueName "orders")

sampleQueue :: Queue
sampleQueue = Queue sampleQueueName fractionalTime False False

sampleUnvalidatedQueue :: UnvalidatedQueue
sampleUnvalidatedQueue = UnvalidatedQueue "MyQueue" wholeTime True False

sampleMessage :: Message
sampleMessage =
  Message
    { messageId = MessageId 42,
      visibilityTime = fractionalTime,
      enqueuedAt = wholeTime,
      lastReadAt = Nothing,
      readCount = 0,
      body = MessageBody (object ["order_id" .= (7 :: Int)]),
      headers = Nothing
    }

sampleReadMessage :: Message
sampleReadMessage =
  sampleMessage
    { lastReadAt = Just fractionalTime,
      readCount = 3,
      headers = Just (object ["x-pgmq-group" .= ("g1" :: String)])
    }

samplePattern :: TopicPattern
samplePattern = either (error . show) id (parseTopicPattern "orders.*")

goldens :: TestTree
goldens =
  testGroup
    "goldens"
    [ golden "queue" sampleQueue,
      golden "unvalidated_queue" sampleUnvalidatedQueue,
      golden "message_unread" sampleMessage,
      golden "message_read" sampleReadMessage,
      golden "archived_message" (ArchivedMessage sampleReadMessage wholeTime),
      golden "message_null_headers" (sampleMessage {headers = Just Null}),
      golden "archived_message_null_headers" (ArchivedMessage (sampleMessage {headers = Just Null}) wholeTime),
      golden "message_above_safe_integer" (sampleMessage {messageId = MessageId 9007199254740993}),
      golden "message_max_int64" (sampleMessage {messageId = MessageId maxBound}),
      golden "archived_message_above_safe_integer" (ArchivedMessage (sampleMessage {messageId = MessageId 9007199254740993}) wholeTime),
      golden "archived_message_max_int64" (ArchivedMessage (sampleMessage {messageId = MessageId maxBound}) wholeTime),
      golden "topic_binding" (TopicBinding samplePattern "orders" wholeTime "^orders\\.[^.]+$"),
      golden "routing_match" (RoutingMatch samplePattern "orders" "^orders\\.[^.]+$"),
      golden "topic_send_result" (TopicSendResult "orders" (MessageId 42)),
      golden "notify_insert_throttle" (NotifyInsertThrottle "orders" 250 wholeTime)
    ]
  where
    golden name value =
      testGroup
        name
        [ goldenVsString "bytes" ("test/golden/" <> name <> ".json") (pure (encode value <> "\n")),
          testCase "toJSON and toEncoding agree" $
            assertEqual "encoder parity" (Just (toJSON value)) (decode (encode value) :: Maybe Value)
        ]

decoding :: TestTree
decoding =
  testGroup
    "decoding"
    [ testCase "a mixed-case name fails as Queue and succeeds as UnvalidatedQueue" $ do
        let bytes = encode sampleUnvalidatedQueue
        assertBool "Queue must reject MyQueue" (isLeft (eitherDecode bytes :: Either String Queue))
        assertEqual "UnvalidatedQueue accepts it" (Just sampleUnvalidatedQueue) (decode bytes),
      testCase "ArchivedMessage round-trips" $
        assertEqual "archive round-trip" (Just (ArchivedMessage sampleReadMessage wholeTime)) (decode (encode (ArchivedMessage sampleReadMessage wholeTime))),
      testCase "absent optional fields decode in both records" $ do
        let withoutOptional = case toJSON sampleMessage of
              Object o -> Object (KM.delete "headers" (KM.delete "last_read_at" o))
              _ -> error "message encoding must be an object"
            archived = case withoutOptional of
              Object o -> Object (KM.insert "archived_at" (toJSON wholeTime) o)
              _ -> error "message encoding must be an object"
        assertEqual "missing fields normalize" (Just sampleMessage) (decode (encode withoutOptional))
        assertEqual "missing archive fields normalize" (Just (ArchivedMessage sampleMessage wholeTime)) (decode (encode archived)),
      testCase "Queue round-trips" $
        assertEqual "round-trip" (Just sampleQueue) (decode (encode sampleQueue)),
      testCase "Message round-trips with and without optional fields" $ do
        assertEqual "unread" (Just sampleMessage) (decode (encode sampleMessage))
        assertEqual "read" (Just sampleReadMessage) (decode (encode sampleReadMessage)),
      testCase "a missing last_read_at decodes as Nothing" $ do
        let bytes = encode (object ["msg_id" .= (1 :: Int), "read_ct" .= (0 :: Int), "enqueued_at" .= wholeTime, "vt" .= wholeTime, "message" .= Null])
        case decode bytes of
          Just m -> assertEqual "Nothing" Nothing (lastReadAt m)
          Nothing -> assertFailure ("Message did not decode: " <> show (LBS.unpack bytes))
    ]

-- No restriction to object headers: JSON null intentionally normalizes to Nothing.
headerValues :: [(String, Maybe Value)]
headerValues =
  [ ("absent", Nothing),
    ("null", Just Null),
    ("boolean", Just (Bool True)),
    ("number", Just (Number 7.25)),
    ("string", Just (String "trace")),
    ("array", Just (toJSON [Bool True, Null, Number 3])),
    ("object", Just (object ["trace" .= ("abc" :: String)]))
  ]

headerCase :: (String, Maybe Value) -> TestTree
headerCase (label, header) =
  testGroup
    label
    [ goldenVsString "message" ("test/golden/headers_" <> label <> ".json") (pure (encode msg <> "\n")),
      goldenVsString "archive" ("test/golden/archived_headers_" <> label <> ".json") (pure (encode archived <> "\n")),
      testCase "normalization and encoder parity" $ do
        assertEqual "Message normalized" (Just normalized) (decode (encode msg))
        assertEqual "Archive normalized" (Just (ArchivedMessage normalized wholeTime)) (decode (encode archived))
        assertEqual "normalization preserves Message bytes" (encode msg) (encode normalized)
        assertEqual "normalization preserves Archive bytes" (encode archived) (encode (ArchivedMessage normalized wholeTime))
        assertEqual "Message encoder parity" (Just (toJSON msg)) (decode (encode msg) :: Maybe Value)
        assertEqual "Archive encoder parity" (Just (toJSON archived)) (decode (encode archived) :: Maybe Value)
    ]
  where
    msg = sampleMessage {headers = header}
    archived = ArchivedMessage msg wholeTime
    normalized = msg {headers = case header of Just Null -> Nothing; other -> other}

integerCase :: Int64 -> TestTree
integerCase n = testCase (show n) $ do
  let mid = MessageId n
      msg = sampleMessage {messageId = mid}
      archived = ArchivedMessage msg wholeTime
      decimal = LBS8.pack (show n)
  assertEqual "numeric token remains exact" decimal (encode mid)
  assertEqual "MessageId decodes without rounding" (Just mid) (decode decimal)
  assertEqual "Message decodes exact id" (Just mid) (messageId <$> decode (encode msg))
  assertEqual "Archive decodes exact id" (Just mid) (messageId . archivedMessage <$> decode (encode archived))
