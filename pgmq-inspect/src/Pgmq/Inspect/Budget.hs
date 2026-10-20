-- | One runtime-wide nonblocking admission gate for HTTP and feed reads.
module Pgmq.Inspect.Budget (InspectionBudget, withInspectionBudget, withInspectionPermit) where

import Control.Concurrent.STM
import Control.Exception (bracket, finally, mask)
import Pgmq.Inspect.Config

newtype InspectionBudget = InspectionBudget (TVar Int)

withInspectionBudget :: InspectConfig -> (InspectionBudget -> IO a) -> IO a
withInspectionBudget cfg k = bracket (InspectionBudget <$> newTVarIO (maxConcurrentReads cfg)) (const (pure ())) k

-- | Reject promptly when full; release on success, failure and cancellation.
-- No retries or detached work are created here.
withInspectionPermit :: InspectionBudget -> IO a -> IO (Maybe a)
withInspectionPermit (InspectionBudget slots) action = mask $ \restore -> do
  acquired <- atomically $ do
    n <- readTVar slots
    if n <= 0 then pure False else writeTVar slots (n - 1) >> pure True
  if acquired then Just <$> (restore action `finally` atomically (modifyTVar' slots (+ 1))) else pure Nothing
