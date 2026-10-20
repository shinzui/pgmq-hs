-- | Bounded transport state, with generation tags checked immediately before writing.
module Pgmq.Inspect.Outbox
  ( Outbox,
    Generation,
    SubscriptionTag,
    newOutbox,
    enqueue,
    enqueueTagged,
    takeOutgoing,
    clearQueue,
    queuedFrames,
    tagCurrent,
    overflowFrame,
  )
where

import Control.Concurrent.STM
import Control.Monad (forM_, void, when)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Unique (Unique)
import Numeric.Natural (Natural)
import Pgmq.Inspect.Frames

type Generation = Unique

type SubscriptionTag = (TVar (Map Text Generation), Text, Generation)

data Outbox = Outbox !(TBQueue (Maybe SubscriptionTag, ServerFrame)) !(TVar Bool)

newOutbox :: Natural -> IO Outbox
newOutbox capacity = Outbox <$> newTBQueueIO capacity <*> newTVarIO False

enqueue :: Outbox -> ServerFrame -> STM ()
enqueue out = enqueueTagged out Nothing

enqueueTagged :: Outbox -> Maybe SubscriptionTag -> ServerFrame -> STM ()
enqueueTagged (Outbox frames overflow) tag frame = do
  full <- isFullTBQueue frames
  when full $ void (readTBQueue frames) >> writeTVar overflow True
  writeTBQueue frames (tag, frame)

tagCurrent :: SubscriptionTag -> STM Bool
tagCurrent (generations, queue, generation) = (== Just generation) . Map.lookup queue <$> readTVar generations

takeOutgoing :: Outbox -> STM (Bool, ServerFrame)
takeOutgoing box@(Outbox frames overflow) = do
  (tag, frame) <- readTBQueue frames
  valid <- maybe (pure True) tagCurrent tag
  if not valid
    then takeOutgoing box
    else do
      dropped <- readTVar overflow
      writeTVar overflow False
      pure (dropped, frame)

queuedFrames :: Outbox -> STM Natural
queuedFrames (Outbox frames _) = lengthTBQueue frames

overflowFrame :: ServerFrame
overflowFrame = ErrorFrame "overflow" "The server dropped frames because this connection was not reading them; re-subscribe or re-read the queue over HTTP." Nothing

-- | Explicit subscribe/unsubscribe discards any frames still queued for that name.
clearQueue :: Outbox -> Text -> STM ()
clearQueue (Outbox frames _) name = do
  queued <- flushTBQueue frames
  forM_ queued $ \item@(_, frame) -> when (frameQueue frame /= Just name) (writeTBQueue frames item)
  where
    frameQueue (Snapshot q _ _) = Just q
    frameQueue (Update q _ _) = Just q
    frameQueue (ErrorFrame _ _ q) = q
    frameQueue _ = Nothing
