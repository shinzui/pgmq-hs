{-# LANGUAGE OverloadedStrings #-}

-- | PGH-6: insert notifications must survive PostgreSQL crash recovery.
--
-- @pgmq.notify_insert_throttle@ is an UNLOGGED table, so PostgreSQL truncates it
-- during crash recovery. The insert trigger only raised @PG_NOTIFY@ when it
-- successfully updated a throttle row, so after a crash the trigger fired, found
-- no row, and silently stopped notifying until an application re-enabled notify.
--
-- This module drives a real crash cycle: it starts its own PostgreSQL instance
-- (never the suite-shared one), enables notify, kills the server with an
-- immediate shutdown (SIGQUIT), restarts it on the same data directory, and then
-- asserts that a post-recovery send still reaches a LISTENing client.
module NotifyCrashSpec (tests) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Lens ((^.))
import Control.Monad (unless)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.Generics.Labels ()
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word32)
import Database.PostgreSQL.LibPQ qualified as LibPQ
import Database.PostgreSQL.Migrate
  ( defaultRunOptions,
    migrationPlan,
    runMigrationPlan,
  )
import EphemeralPg qualified as Pg
-- 'shutdownMode' names a field of both Pg.Config and Pg.Database, so the record
-- update below needs the selector from the module that defines only Database.
import EphemeralPg.Database qualified as PgDb
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as PoolConfig
import Hasql.Session (Session)
import Hasql.Session qualified as Session
import Hasql.Statement (Statement, preparable)
import Pgmq.Config (ensureQueues, standardQueue, withNotifyInsert)
import Pgmq.Hasql.Sessions qualified as Sessions
import Pgmq.Hasql.Statements.Types qualified as StmtTypes
import Pgmq.Migration qualified as Migration
import Pgmq.Types (MessageBody (..), QueueName, parseQueueName, queueNameToText)
import System.Random (randomRIO)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

-- | Everything the crash cycle observed, collected in one pass so the
-- assertions below can report each fact as its own test case.
data CrashObservations = CrashObservations
  { -- | The queue the cycle ran against.
    obsQueueName :: !QueueName,
    -- | The channel the listener subscribed to.
    obsChannel :: !Text,
    -- | Queue names present in @pgmq.list_notify_insert_throttles()@ after the crash.
    obsThrottlesAfterCrash :: ![Text],
    -- | Insert triggers on the queue table after the crash (1 = survived).
    obsTriggersAfterCrash :: !Int64,
    -- | Messages still in the queue after the crash (the queue table is logged).
    obsQueueLengthAfterCrash :: !Int64,
    -- | Channel of the notification delivered by the post-crash send, if any.
    obsNotifyAfterCrash :: !(Maybe ByteString),
    -- | Queue names in the throttle table after a reconcile ran.
    obsThrottlesAfterReconcile :: ![Text]
  }

tests :: TestTree
tests =
  withResource runCrashCycle (const (pure ())) $ \getObs ->
    testGroup
      "NotifyCrashSpec"
      [ testCase "post-crash: throttle row truncated, trigger intact" $ do
          obs <- getObs
          obsThrottlesAfterCrash obs @?= []
          obsTriggersAfterCrash obs @?= 1
          obsQueueLengthAfterCrash obs @?= 1,
        testCase "post-crash: send delivers a notification" $ do
          obs <- getObs
          case obsNotifyAfterCrash obs of
            Nothing ->
              assertFailure $
                "expected a notification on "
                  <> show (obsChannel obs)
                  <> " within 2s after crash recovery, got none"
            Just chan ->
              assertBool
                ( "notification arrived on "
                    <> show chan
                    <> " but the contract channel is "
                    <> show (obsChannel obs)
                )
                (chan == TE.encodeUtf8 (obsChannel obs)),
        testCase "post-crash: a reconcile restores the throttle row" $ do
          obs <- getObs
          obsThrottlesAfterReconcile obs @?= [queueNameToText (obsQueueName obs)]
      ]

-- | Start a dedicated PostgreSQL instance, enable notify, crash it, recover it,
-- and record what happened. Every resource is released before this returns.
runCrashCycle :: IO CrashObservations
runCrashCycle = do
  qn <- genQueueName
  db0 <- startOrFail
  ref <- newIORef db0
  bracket (pure ref) (\r -> readIORef r >>= Pg.stop) (crashCycle qn)

crashCycle :: QueueName -> IORef Pg.Database -> IO CrashObservations
crashCycle qn ref = do
  db0 <- readIORef ref
  installPgmq db0

  -- Pre-crash: create the queue, enable unthrottled notify, send one message.
  -- No listener is opened yet: an immediate shutdown kills every pre-crash
  -- connection, so a listener created here could never see the assertion's
  -- notification.
  bracket (acquirePool db0) Pool.release $ \pool -> do
    runSession pool (Sessions.createQueue qn)
    runSession pool $
      Sessions.enableNotifyInsert
        StmtTypes.EnableNotifyInsert
          { StmtTypes.queueName = qn,
            StmtTypes.throttleIntervalMs = Just 0
          }
    runSession pool (sendProbe qn "before-crash")
    -- ephemeral-pg runs PostgreSQL with fsync, synchronous_commit and
    -- full_page_writes all off, so an immediate shutdown would otherwise discard
    -- every commit still sitting in the WAL buffers — including the pgmq schema
    -- itself. CHECKPOINT flushes them. It does NOT make the unlogged throttle
    -- table crash-safe: recovery still resets unlogged relations to their init
    -- fork, which is the behavior under test.
    runSession pool (Session.script "checkpoint")

  db1 <- crashAndRecover db0
  writeIORef ref db1

  bracket (acquirePool db1) Pool.release $ \pool -> do
    throttlesAfterCrash <- listThrottleNames pool
    triggers <- runSession pool (Session.statement (queueTableName qn) insertTriggerCount)
    metrics <- runSession pool (Sessions.queueMetrics qn)

    let channel = "pgmq." <> queueTableName qn <> ".INSERT"
    notified <- withListener db1 channel $ \conn -> do
      _ <- runSession pool (sendProbe qn "after-crash")
      awaitNotify conn 20

    runSession pool (ensureQueues [withNotifyInsert (Just 0) (standardQueue qn)])
    throttlesAfterReconcile <- listThrottleNames pool

    pure
      CrashObservations
        { obsQueueName = qn,
          obsChannel = channel,
          obsThrottlesAfterCrash = throttlesAfterCrash,
          obsTriggersAfterCrash = triggers,
          obsQueueLengthAfterCrash = metrics ^. #queueLength,
          obsNotifyAfterCrash = LibPQ.notifyRelname <$> notified,
          obsThrottlesAfterReconcile = throttlesAfterReconcile
        }

-- | Stop PostgreSQL with SIGQUIT and start it again on the same data directory.
-- That is a genuine crash: the next start runs crash recovery, which truncates
-- every UNLOGGED table. Retries once, then fails loudly — the crash cycle is the
-- test, so it must never degrade into a skip.
crashAndRecover :: Pg.Database -> IO Pg.Database
crashAndRecover db = do
  let crashing = db {PgDb.shutdownMode = Pg.ShutdownImmediate}
  first <- Pg.restart crashing
  case first of
    Right db' -> pure db'
    Left _ -> do
      second <- Pg.restart crashing
      case second of
        Right db' -> pure db'
        Left err -> assertFailure $ "could not restart PostgreSQL after crash: " <> show err

-- Database plumbing -----------------------------------------------------------

startOrFail :: IO Pg.Database
startOrFail = do
  result <- Pg.startCached Pg.defaultConfig Pg.defaultCacheConfig
  case result of
    Left err -> assertFailure $ "could not start a dedicated PostgreSQL: " <> show err
    Right db -> pure db

-- | Apply the full pgmq migration ledger, exactly as @EphemeralDb@ does.
installPgmq :: Pg.Database -> IO ()
installPgmq db = do
  component <- either (error . ("Invalid PGMQ migration component: " <>) . show) pure Migration.pgmqMigrations
  plan <- either (error . ("Invalid PGMQ migration plan: " <>) . show) pure (migrationPlan (component :| []))
  installResult <- runMigrationPlan defaultRunOptions (Pg.connectionSettings db) plan
  case installResult of
    Left migrationErr -> error $ "Migration failed: " <> show migrationErr
    Right _ -> pure ()

acquirePool :: Pg.Database -> IO Pool.Pool
acquirePool db =
  Pool.acquire $
    PoolConfig.settings
      [ PoolConfig.size 2,
        PoolConfig.staticConnectionSettings (Pg.connectionSettings db)
      ]

runSession :: Pool.Pool -> Session a -> IO a
runSession pool session = do
  result <- Pool.use pool session
  case result of
    Left err -> assertFailure $ "Session failed: " <> show err
    Right a -> pure a

listThrottleNames :: Pool.Pool -> IO [Text]
listThrottleNames pool = do
  throttles <- runSession pool Sessions.listNotifyInsertThrottles
  pure (map (^. #throttleQueueName) throttles)

sendProbe :: QueueName -> Text -> Session ()
sendProbe qn label =
  ()
    <$ Sessions.sendMessage
      StmtTypes.SendMessage
        { StmtTypes.queueName = qn,
          StmtTypes.messageBody = MessageBody (Aeson.String label),
          StmtTypes.delay = Nothing
        }

-- | The physical table backing a queue: @q_@ plus the lowercased queue name.
queueTableName :: QueueName -> Text
queueTableName qn = "q_" <> T.toLower (queueNameToText qn)

-- | How many insert-notification triggers exist on the given @pgmq@ table.
insertTriggerCount :: Statement Text Int64
insertTriggerCount = preparable sql encoder decoder
  where
    sql =
      "select count(*)::int8 \
      \from pg_trigger t \
      \join pg_class c on c.oid = t.tgrelid \
      \join pg_namespace n on n.oid = c.relnamespace \
      \where n.nspname = 'pgmq' \
      \and c.relname = $1 \
      \and t.tgname = 'trigger_notify_queue_insert_listeners'"
    encoder = E.param (E.nonNullable E.text)
    decoder = D.singleRow (D.column (D.nonNullable D.int8))

genQueueName :: IO QueueName
genQueueName = do
  suffix <- randomRIO (10000 :: Word32, 99999)
  case parseQueueName ("crash_test_" <> T.pack (show suffix)) of
    Left err -> error $ "Failed to generate queue name: " <> show err
    Right qn -> pure qn

-- LISTEN plumbing -------------------------------------------------------------

-- | Open a raw libpq connection (hasql exposes no notification API), subscribe
-- to @channel@, and run the action. @ephemeral-pg@ hands out connection strings
-- as 'Text' while libpq consumes 'ByteString', so both the conninfo and the
-- command are encoded explicitly.
withListener :: Pg.Database -> Text -> (LibPQ.Connection -> IO a) -> IO a
withListener db channel action =
  bracket (LibPQ.connectdb (TE.encodeUtf8 (Pg.connectionString db))) LibPQ.finish $ \conn -> do
    connStatus <- LibPQ.status conn
    unless (connStatus == LibPQ.ConnectionOk) $ do
      err <- LibPQ.errorMessage conn
      assertFailure $ "libpq connection failed: " <> show err
    -- The channel contains dots, so LISTEN needs the identifier double-quoted.
    result <- LibPQ.exec conn (TE.encodeUtf8 ("LISTEN " <> quoteIdentifier channel))
    case result of
      Nothing -> assertFailure "LISTEN returned no result"
      Just res -> do
        execStatus <- LibPQ.resultStatus res
        unless (execStatus == LibPQ.CommandOk) $
          assertFailure ("LISTEN failed with " <> show execStatus)
    action conn

quoteIdentifier :: Text -> Text
quoteIdentifier ident = "\"" <> T.replace "\"" "\"\"" ident <> "\""

-- | Poll for a notification, 100 ms per attempt.
awaitNotify :: LibPQ.Connection -> Int -> IO (Maybe LibPQ.Notify)
awaitNotify conn attempts
  | attempts <= 0 = pure Nothing
  | otherwise = do
      _ <- LibPQ.consumeInput conn
      pending <- LibPQ.notifies conn
      case pending of
        Just n -> pure (Just n)
        Nothing -> do
          threadDelay 100_000
          awaitNotify conn (attempts - 1)
