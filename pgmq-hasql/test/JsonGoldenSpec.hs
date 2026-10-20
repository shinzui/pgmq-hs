{-# LANGUAGE OverloadedStrings #-}

-- | Pins the wire shape of 'QueueMetrics' and proves that 'Message' survives
-- encode-then-decode for arbitrary jsonb-storable bodies and headers. The
-- property lives here rather than in pgmq-core because this suite already
-- owns the hedgehog generators PostgreSQL accepts.
module JsonGoldenSpec (tests) where

import Data.Aeson (ToJSON (..), Value (Null), decode, encode)
import Data.Time (UTCTime (..), fromGregorian)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Generators (genJsonValue)
import Hedgehog (Gen, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Pgmq.Hasql.Statements.Types (QueueMetrics (..))
import Pgmq.Types (ArchivedMessage (..), Message (..), MessageBody (..), MessageId (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "JSON wire encodings"
    [ goldenVsString "queue_metrics" "test/golden/queue_metrics.json" (pure (encode sampleMetrics <> "\n")),
      goldenVsString "queue_metrics_partitioned" "test/golden/queue_metrics_partitioned.json" (pure (encode partitionedMetrics <> "\n")),
      testProperty "Message round-trips modulo explicit-null header normalization" $ property $ do
        m <- forAll genMessage
        let normalized = m {headers = case headers m of Just Null -> Nothing; other -> other}
        decode (encode m) === Just normalized
        encode normalized === encode m
        decode (encode m) === Just (toJSON m)
        at <- forAll genTime
        let archived = ArchivedMessage m at
        decode (encode archived) === Just (ArchivedMessage normalized at)
        decode (encode archived) === Just (toJSON archived)
        encode (ArchivedMessage normalized at) === encode archived,
      testProperty "QueueMetrics encoder parity" $ property $ do
        newest <- forAll (Gen.maybe (Gen.int32 (Range.linear 0 maxBound)))
        oldest <- forAll (Gen.maybe (Gen.int32 (Range.linear 0 maxBound)))
        estimate <- forAll (Gen.maybe (Gen.int64 (Range.linear 0 maxBound)))
        let m = QueueMetrics "orders" 120 newest oldest 9000 scrape 115 estimate
        decode (encode m) === Just (toJSON m)
    ]

scrape :: UTCTime
scrape = UTCTime (fromGregorian 2026 8 19) 45296.123

-- | An empty queue: both ages are null, the partition estimate is null.
sampleMetrics :: QueueMetrics
sampleMetrics = QueueMetrics "orders" 0 Nothing Nothing 0 scrape 0 Nothing

-- | A partitioned queue on PGMQ 1.13 with a planner estimate.
partitionedMetrics :: QueueMetrics
partitionedMetrics = QueueMetrics "events" 120 (Just 2) (Just 3600) 9000 scrape 115 (Just 7)

-- | Whole milliseconds so the timestamp survives jsonb-independent rendering
-- exactly; aeson itself round-trips picoseconds, but the property is about
-- the record, not the clock.
genTime :: Gen UTCTime
genTime = do
  millis <- Gen.integral (Range.linear 0 (4_102_444_800_000 :: Integer))
  pure (posixSecondsToUTCTime (fromIntegral millis / 1000))

genMessage :: Gen Message
genMessage =
  Message
    <$> (MessageId <$> Gen.integral (Range.linear 1 maxBound))
    <*> genTime
    <*> genTime
    <*> Gen.maybe genTime
    <*> Gen.integral (Range.linear 0 1000)
    <*> (MessageBody <$> genJsonValue)
    <*> Gen.maybe genJsonValue
