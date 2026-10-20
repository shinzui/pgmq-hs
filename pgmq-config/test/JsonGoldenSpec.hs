{-# LANGUAGE OverloadedStrings #-}

-- | One golden per 'ReconcileAction' constructor. A constructor without a
-- golden is a defect: the report is a published wire shape (see
-- @docs/design/020-json-wire-encodings.md@) once a consumer logs or serves it.
module JsonGoldenSpec (tests) where

import Data.Aeson (ToJSON (..), Value, decode, encode)
import Pgmq.Config.Types
import Pgmq.Types (QueueName, TopicPattern, parseQueueName, parseTopicPattern)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "ReconcileAction wire encodings"
    [ golden "created_queue_standard" (CreatedQueue q StandardQueue),
      golden "created_queue_unlogged" (CreatedQueue q UnloggedQueue),
      golden "created_queue_partitioned" (CreatedQueue q (PartitionedQueue partition)),
      golden "created_queue_partitioned_default_premake" (CreatedQueue q (PartitionedQueue partition {premake = Nothing})),
      golden "enabled_notify" (EnabledNotify q (Just 1000)),
      golden "enabled_notify_default" (EnabledNotify q Nothing),
      golden "created_fifo_index" (CreatedFifoIndex q),
      golden "bound_topic" (BoundTopic q pat),
      golden "skipped_queue" (SkippedQueue q),
      golden "skipped_notify" (SkippedNotify q),
      golden "skipped_fifo_index" (SkippedFifoIndex q),
      golden "skipped_topic_binding" (SkippedTopicBinding q pat),
      golden "updated_notify_throttle" (UpdatedNotifyThrottle q 250 1000),
      golden "detected_queue_type_drift" (DetectedQueueTypeDrift q UnloggedQueue ObservedPartitioned),
      golden "queue_type_standard" StandardQueue,
      golden "queue_type_unlogged" UnloggedQueue,
      golden "queue_type_partitioned" (PartitionedQueue partition),
      golden "partition_config" partition,
      golden "partition_config_default" (partition {premake = Nothing}),
      golden "observed_standard" ObservedStandard,
      golden "observed_unlogged" ObservedUnlogged,
      golden "observed_partitioned" ObservedPartitioned
    ]
  where
    golden name value =
      testGroup
        name
        [ goldenVsString "bytes" ("test/golden/" <> name <> ".json") (pure (encode value <> "\n")),
          testCase "toJSON and toEncoding agree" $
            assertEqual "encoder parity" (Just (toJSON value)) (decode (encode value) :: Maybe Value)
        ]

q :: QueueName
q = either (error . show) id (parseQueueName "orders")

pat :: TopicPattern
pat = either (error . show) id (parseTopicPattern "orders.*")

partition :: PartitionConfig
partition = PartitionConfig {partitionInterval = "daily", retentionInterval = "7 days", premake = Just 2}
