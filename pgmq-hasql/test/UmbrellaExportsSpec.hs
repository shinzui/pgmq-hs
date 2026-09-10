{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- Compile witnesses: this module intentionally uses only the public umbrella.
module UmbrellaExportsSpec (grouped, polling, creation, arguments, metric) where

import Data.Int (Int32, Int64)
import Data.Vector (Vector)
import Hasql.Session (Session)
import Pgmq

grouped :: ReadGrouped -> [Session (Vector Message)]
grouped args = [readGrouped args, readGroupedRoundRobin args, readGroupedHead args]

polling :: ReadGroupedWithPoll -> [Session (Vector Message)]
polling args = [readGroupedWithPoll args, readGroupedRoundRobinWithPoll args, readGroupedHeadWithPoll args]

creation :: CreatePartitionedQueue -> Int32 -> [Session ()]
creation args n = [createPartitionedQueue args, createPartitionedQueueWithPremake args n]

arguments :: QueueName -> (ReadGrouped, ReadGroupedWithPoll, CreatePartitionedQueue)
arguments q = (ReadGrouped q 30 10, ReadGroupedWithPoll q 30 10 5 100, CreatePartitionedQueue q "100" "1000")

metric :: QueueMetrics -> Maybe Int64
metric = defaultPartitionLength
