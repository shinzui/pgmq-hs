-- | The declarative vocabulary: what a queue topology looks like as Haskell
-- values, and what a reconciliation run reports back.
--
-- t'QueueConfig' is the declaration; 'ReconcileAction' is the report. Everything
-- here is re-exported from "Pgmq.Config", which is the module to import.
module Pgmq.Config.Types
  ( -- * Queue Configuration
    QueueConfig (..),
    QueueType (..),
    PartitionConfig (..),
    NotifyConfig (..),

    -- * Smart Constructors
    standardQueue,
    unloggedQueue,
    partitionedQueue,

    -- * Modifiers
    withNotifyInsert,
    withFifoIndex,
    withTopicBinding,

    -- * Reconciliation Report
    ReconcileAction (..),
    ObservedQueueType (..),

    -- * Defaults
    defaultThrottleMs,
  )
where

import Control.Lens ((%~), (&), (^.))
import Data.Aeson (Encoding, KeyValue, Series, ToJSON (..), object, pairs, (.=))
import Data.Generics.Labels ()
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)
import Pgmq.Types (QueueName, TopicPattern)

-- | Describes the desired state of a single pgmq queue.
data QueueConfig = QueueConfig
  { queueName :: !QueueName,
    queueType :: !QueueType,
    notifyInsert :: !(Maybe NotifyConfig),
    fifoIndex :: !Bool,
    topicBindings :: ![TopicPattern]
  }
  deriving stock (Generic, Show)

-- | The type of queue to create.
data QueueType
  = -- | A standard queue with write-ahead logging.
    StandardQueue
  | -- | An unlogged queue — faster writes, but data is lost on crash.
    UnloggedQueue
  | -- | A partitioned queue for high-throughput scenarios.
    PartitionedQueue !PartitionConfig
  deriving stock (Show)

-- | Configuration for a partitioned queue.
data PartitionConfig = PartitionConfig
  { partitionInterval :: !Text,
    retentionInterval :: !Text,
    -- | Creation only: Nothing uses the server default via the legacy call;
    -- Just requires PGMQ 1.13+ and a count of at least one. Existing queues
    -- are neither checked nor reconfigured for this setting.
    premake :: !(Maybe Int32)
  }
  deriving stock (Generic, Show)

-- | Configuration for insert notifications (LISTEN/NOTIFY).
data NotifyConfig = NotifyConfig
  { -- | Minimum milliseconds between notifications. Nothing uses the documented
    -- pgmq default (250 ms), applied via COALESCE in the pgmq-hasql statement so
    -- SQL NULL never reaches the function.
    throttleMs :: !(Maybe Int32)
  }
  deriving stock (Generic, Show)

-- | The queue shape actually observed in the database.
--
-- @pgmq.list_queues()@ reports two booleans per queue, partitioned and
-- unlogged, which describe exactly these three states. It reports nothing about
-- a partitioned queue's interval or retention settings, so those are not
-- drift-checked — see 'DetectedQueueTypeDrift'.
data ObservedQueueType
  = ObservedStandard
  | ObservedUnlogged
  | ObservedPartitioned
  deriving stock (Eq, Show)

-- | An action taken (or skipped) during queue reconciliation.
data ReconcileAction
  = -- | The queue did not exist and was created with the declared type.
    CreatedQueue !QueueName !QueueType
  | -- | No throttle row existed, so insert notifications were enabled with the
    -- declared interval ('Nothing' meaning 'defaultThrottleMs').
    EnabledNotify !QueueName !(Maybe Int32)
  | -- | The FIFO headers index did not exist and was created.
    CreatedFifoIndex !QueueName
  | -- | The topic binding did not exist and was created.
    BoundTopic !QueueName !TopicPattern
  | -- | A queue with this name already existed and its observed type matches
    -- what was declared. Nothing was issued.
    SkippedQueue !QueueName
  | -- | A throttle row already existed with the declared interval. Nothing was
    -- issued — in particular the row was /not/ re-enabled, which would reset
    -- its @last_notified_at@.
    SkippedNotify !QueueName
  | -- | The FIFO headers index already existed; nothing was issued.
    SkippedFifoIndex !QueueName
  | -- | The topic binding already existed; nothing was issued.
    SkippedTopicBinding !QueueName !TopicPattern
  | -- | The declared throttle interval differed from the database row, so the
    -- row was updated in place via @pgmq.update_notify_insert@. Fields: queue,
    -- observed interval, declared interval (in milliseconds).
    --
    -- This is the reconciler's only mutation of already-existing state. The
    -- update also resets the throttle's @last_notified_at@ to the epoch, so the
    -- next insert on that queue notifies immediately; that is a property of
    -- @pgmq.update_notify_insert@ itself, and it happens at most once per real
    -- configuration change.
    UpdatedNotifyThrottle !QueueName !Int32 !Int32
  | -- | The queue exists but its observed shape contradicts the declared one.
    -- Fields: queue, declared type, observed type.
    --
    -- The queue itself is not mutated: conversion between standard, unlogged,
    -- and partitioned requires destroying its messages. Notification, FIFO-index
    -- and topic-binding declarations are still reconciled against the observed
    -- queue and each reports its own action. Resolving drift is an operator
    -- decision. This action replaces
    -- 'SkippedQueue' for the queue it concerns, so the report still carries
    -- exactly one queue-existence action per declared config.
    --
    -- Only the three-way shape is compared. A declared 'PartitionedQueue'
    -- against an observed partitioned queue matches regardless of its interval
    -- and retention settings, because @pgmq.list_queues()@ does not report
    -- them.
    DetectedQueueTypeDrift !QueueName !QueueType !ObservedQueueType
  | -- | Foreign registry names share the declared name's lowercased physical
    -- table. Fields: declared queue, every colliding registry name in ascending
    -- lexical order. Without an exact row this replaces 'CreatedQueue' and no
    -- notification, FIFO-index or binding declarations run. With an exact row,
    -- this follows the existence action and sub-resources proceed on that row.
    -- This action mutates nothing. Detection uses the initial snapshot; a foreign
    -- client can still create an alias afterward. Resolving aliases follows
    -- @docs/design/016-queue-name-validation.md@.
    DetectedQueueNameCollision !QueueName !(NonEmpty Text)
  | -- | Insert notifications were declared on an observed or newly created
    -- partitioned queue. Its cloned leaf triggers cannot deliver on
    -- 'Pgmq.Types.notifyChannelName'; see
    -- @docs/design/015-notification-delivery-contract.md@. Nothing was enabled,
    -- updated or re-enabled; an existing foreign throttle row is left unchanged.
    UnsupportedNotifyOnPartitionedQueue !QueueName
  deriving stock (Show)

-- | The throttle interval pgmq applies when none is given: 250 milliseconds.
--
-- A t'NotifyConfig' whose @throttleMs@ is 'Nothing' means \"use this value\".
-- The pgmq-hasql enable statement supplies it with a SQL @coalesce($2, 250)@,
-- and @pgmq.enable_notify_insert@ declares the same figure as its parameter
-- default, so a 'Nothing' config and a stored 250 agree and reconciliation does
-- not flap between them.
defaultThrottleMs :: Int32
defaultThrottleMs = 250

-- | Create a standard queue configuration with no extras.
standardQueue :: QueueName -> QueueConfig
standardQueue qn =
  QueueConfig
    { queueName = qn,
      queueType = StandardQueue,
      notifyInsert = Nothing,
      fifoIndex = False,
      topicBindings = []
    }

-- | Create an unlogged queue configuration (faster, no WAL, lost on crash).
unloggedQueue :: QueueName -> QueueConfig
unloggedQueue qn =
  (standardQueue qn) {queueType = UnloggedQueue}

-- | Create a partitioned queue configuration.
partitionedQueue :: QueueName -> PartitionConfig -> QueueConfig
partitionedQueue qn pc =
  (standardQueue qn) {queueType = PartitionedQueue pc}

-- | Enable LISTEN/NOTIFY on message insert.
withNotifyInsert :: Maybe Int32 -> QueueConfig -> QueueConfig
withNotifyInsert ms cfg =
  cfg {notifyInsert = Just NotifyConfig {throttleMs = ms}}

-- | Add a FIFO index for strict message ordering.
withFifoIndex :: QueueConfig -> QueueConfig
withFifoIndex cfg = cfg {fifoIndex = True}

-- | Bind a topic pattern for AMQP-style routing.
withTopicBinding :: TopicPattern -> QueueConfig -> QueueConfig
withTopicBinding pat cfg =
  cfg & #topicBindings %~ (++ [pat])

-- JSON wire encodings ---------------------------------------------------------

encodeFields :: [Series] -> Encoding
encodeFields = pairs . mconcat

-- | Encodes as @{"type":"standard"}@, @{"type":"unlogged"}@, or
-- @{"type":"partitioned","partition_interval":…,"retention_interval":…,"premake":…}@.
-- See @docs/design/020-json-wire-encodings.md@.
queueTypeFields :: (KeyValue e kv) => QueueType -> [kv]
queueTypeFields qt = case qt of
  StandardQueue -> ["type" .= ("standard" :: Text)]
  UnloggedQueue -> ["type" .= ("unlogged" :: Text)]
  PartitionedQueue pc -> ("type" .= ("partitioned" :: Text)) : partitionConfigFields pc

-- | JSON wire contract: @docs/design/020-json-wire-encodings.md@.
instance ToJSON QueueType where
  toJSON = object . queueTypeFields
  toEncoding = encodeFields . queueTypeFields

-- | @premake@ is @null@ when creation uses the server default. See
-- @docs/design/020-json-wire-encodings.md@.
partitionConfigFields :: (KeyValue e kv) => PartitionConfig -> [kv]
partitionConfigFields pc =
  [ "partition_interval" .= (pc ^. #partitionInterval),
    "retention_interval" .= (pc ^. #retentionInterval),
    "premake" .= (pc ^. #premake)
  ]

-- | JSON wire contract: @docs/design/020-json-wire-encodings.md@.
instance ToJSON PartitionConfig where
  toJSON = object . partitionConfigFields
  toEncoding = encodeFields . partitionConfigFields

-- | Encodes as the bare strings @"standard"@, @"unlogged"@, @"partitioned"@.
observedQueueTypeText :: ObservedQueueType -> Text
observedQueueTypeText ot = case ot of
  ObservedStandard -> "standard"
  ObservedUnlogged -> "unlogged"
  ObservedPartitioned -> "partitioned"

-- | JSON wire contract: @docs/design/020-json-wire-encodings.md@.
instance ToJSON ObservedQueueType where
  toJSON = toJSON . observedQueueTypeText
  toEncoding = toEncoding . observedQueueTypeText

-- | A tagged object: @"action"@ holds the snake_case constructor name and the
-- remaining keys name the fields. @enabled_notify@'s @throttle_interval_ms@
-- is @null@ when the config declared the pgmq default ('defaultThrottleMs')
-- rather than a number; that is a different fact from declaring @250@.
-- Encode-only. The encoding is a published compatibility surface; see
-- @docs/design/020-json-wire-encodings.md@. Every constructor added later
-- must be encoded by the same rule and pinned in
-- @pgmq-config/test/JsonGoldenSpec.hs@.
reconcileActionFields :: (KeyValue e kv) => ReconcileAction -> [kv]
reconcileActionFields action = case action of
  CreatedQueue q t ->
    ["action" .= tag "created_queue", "queue" .= q, "queue_type" .= t]
  EnabledNotify q ms ->
    ["action" .= tag "enabled_notify", "queue" .= q, "throttle_interval_ms" .= ms]
  CreatedFifoIndex q ->
    ["action" .= tag "created_fifo_index", "queue" .= q]
  BoundTopic q p ->
    ["action" .= tag "bound_topic", "queue" .= q, "pattern" .= p]
  SkippedQueue q ->
    ["action" .= tag "skipped_queue", "queue" .= q]
  SkippedNotify q ->
    ["action" .= tag "skipped_notify", "queue" .= q]
  SkippedFifoIndex q ->
    ["action" .= tag "skipped_fifo_index", "queue" .= q]
  SkippedTopicBinding q p ->
    ["action" .= tag "skipped_topic_binding", "queue" .= q, "pattern" .= p]
  UpdatedNotifyThrottle q observed declared ->
    [ "action" .= tag "updated_notify_throttle",
      "queue" .= q,
      "observed_throttle_interval_ms" .= observed,
      "declared_throttle_interval_ms" .= declared
    ]
  DetectedQueueTypeDrift q declared observed ->
    [ "action" .= tag "detected_queue_type_drift",
      "queue" .= q,
      "declared" .= declared,
      "observed" .= observed
    ]
  DetectedQueueNameCollision q names ->
    ["action" .= tag "detected_queue_name_collision", "queue" .= q, "colliding_names" .= names]
  UnsupportedNotifyOnPartitionedQueue q ->
    ["action" .= tag "unsupported_notify_on_partitioned_queue", "queue" .= q]
  where
    tag :: Text -> Text
    tag = id

-- | JSON wire contract: @docs/design/020-json-wire-encodings.md@.
instance ToJSON ReconcileAction where
  toJSON = object . reconcileActionFields
  toEncoding = encodeFields . reconcileActionFields
