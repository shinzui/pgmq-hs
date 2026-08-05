{-# LANGUAGE OverloadedStrings #-}

-- | PGH-7 evidence: what mixed-case queue names do to the SQL layer.
--
-- pgmq's SQL is consistent-by-lowercasing for /physical/ names —
-- @pgmq.format_table_name@ lowercases — but @pgmq.meta@ stores the caller's
-- /original/ casing, and the notify trigger extracts the /lowercased/ name from
-- the physical table it fires on. Three views of one name that only agree for
-- lowercase input. These tests drive the SQL layer directly (raw statements,
-- never the Haskell API) and document the consequences:
--
--   1. @create('MyQueue')@ then @create('myqueue')@ yields ONE physical table
--      with TWO meta rows: two logical queues silently interleaving in one
--      table.
--   2. @drop_queue('myqueue')@ destroys the mixed-case alias's messages while
--      its meta row lives on, pointing at nothing.
--   3. @enable_notify_insert('MyQueue')@ writes a throttle row the trigger's
--      lowercase lookup never matches, so the configured throttle interval is
--      silently ignored. (Since migration 0003 the trigger fails open on a
--      missing row, so notifications fire /unthrottled/; before it, they never
--      fired at all. Either way the configuration is dead on arrival.)
--
-- These are evidence tests: they pass against the current SQL layer and stay
-- green after the Haskell boundary starts rejecting mixed-case names, at which
-- point the states they construct become unreachable from validated input.
-- They run on a dedicated PostgreSQL instance, never the suite-shared one,
-- because a mixed-case @pgmq.meta@ row poisons @listQueues@ decoding for every
-- concurrent test once @parseQueueName@ rejects it.
module AliasingSpec (tests) where

import Control.Monad (void)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word32)
import Database.PostgreSQL.Migrate
  ( defaultRunOptions,
    migrationPlan,
    runMigrationPlan,
  )
import EphemeralPg qualified as Pg
import Hasql.Decoders qualified as D
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as PoolConfig
import Hasql.Session (Session, statement)
import Hasql.Statement (unpreparable)
import Pgmq.Migration qualified as Migration
import System.Random (randomRIO)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  withResource acquireDb releaseDb $ \getDb ->
    testGroup
      "Mixed-Case Queue Aliasing (PGH-7 evidence)"
      [ testOneTableTwoMetaRows getDb,
        testDropDestroysTheAlias getDb,
        testNotifyThrottleNeverMatches getDb
      ]

-- | Both casings create the same physical table, interleave their messages in
-- it, and leave two rows in @pgmq.meta@.
testOneTableTwoMetaRows :: IO (Pg.Database, Pool.Pool) -> TestTree
testOneTableTwoMetaRows getDb = testCase "create in both casings yields one physical table and two meta rows" $ do
  (_, pool) <- getDb
  (mixed, lower) <- genQueuePair
  assertSession pool (rawUnit ("select pgmq.create('" <> mixed <> "')"))
  assertSession pool (rawUnit ("select pgmq.create('" <> lower <> "')"))
  -- Exactly one physical table exists for the pair, and it is the lowercase
  -- one. The case-insensitive count would catch a hypothetical q_MyQueue_<n>.
  tables <-
    assertSession pool $
      rawCount ("select count(*) from pg_tables where schemaname = 'pgmq' and lower(tablename) = 'q_" <> lower <> "'")
  assertEqual "Exactly one physical table for both casings" 1 tables
  lowerTables <-
    assertSession pool $
      rawCount ("select count(*) from pg_tables where schemaname = 'pgmq' and tablename = 'q_" <> lower <> "'")
  assertEqual "The one physical table is the lowercased name" 1 lowerTables
  metas <-
    assertSession pool $
      rawCount ("select count(*) from pgmq.meta where lower(queue_name) = '" <> lower <> "'")
  assertEqual "Two meta rows share the one physical table" 2 metas
  -- A message sent through the mixed-case name is read back through the
  -- lowercase name: the \"two\" queues interleave in one table.
  void $ assertSession pool (rawIds ("select pgmq.send('" <> mixed <> "', '{\"via\":\"upper\"}'::jsonb)"))
  void $ assertSession pool (rawIds ("select pgmq.send('" <> lower <> "', '{\"via\":\"lower\"}'::jsonb)"))
  readBack <-
    assertSession pool $
      rawCount ("select count(*) from pgmq.read('" <> lower <> "', 0, 10)")
  assertEqual "Reading via the lowercase name returns both casings' messages" 2 readBack
  -- Cleanup: drop the lowercase queue (table and meta row), then remove the
  -- orphaned mixed-case meta row directly — drop_queue refuses once the table
  -- is gone.
  void $ assertSession pool (rawBool ("select pgmq.drop_queue('" <> lower <> "')"))
  assertSession pool (rawUnit ("delete from pgmq.meta where queue_name = '" <> mixed <> "'"))

-- | Dropping the lowercase twin destroys the mixed-case alias's messages; the
-- alias's meta row survives, pointing at a table that no longer exists.
testDropDestroysTheAlias :: IO (Pg.Database, Pool.Pool) -> TestTree
testDropDestroysTheAlias getDb = testCase "drop_queue on one casing breaks the other" $ do
  (_, pool) <- getDb
  (mixed, lower) <- genQueuePair
  assertSession pool (rawUnit ("select pgmq.create('" <> mixed <> "')"))
  assertSession pool (rawUnit ("select pgmq.create('" <> lower <> "')"))
  void $ assertSession pool (rawIds ("select pgmq.send('" <> mixed <> "', '{\"owner\":\"mixed\"}'::jsonb)"))
  dropped <- assertSession pool (rawBool ("select pgmq.drop_queue('" <> lower <> "')"))
  assertBool "drop_queue on the lowercase twin reports success" dropped
  -- The mixed-case alias is now broken: its meta row survives but every send
  -- through it fails on the missing physical table (SQLSTATE 42P01).
  sendResult <- Pool.use pool (rawIds ("select pgmq.send('" <> mixed <> "', '{\"after\":\"drop\"}'::jsonb)"))
  case sendResult of
    Right _ -> assertFailure "Sending via the mixed-case alias should fail once the twin is dropped"
    Left err ->
      assertBool
        ("Expected undefined_table (42P01), got: " <> show err)
        ("42P01" `T.isInfixOf` T.pack (show err))
  survivors <-
    assertSession pool $
      rawCount ("select count(*) from pgmq.meta where queue_name = '" <> mixed <> "'")
  assertEqual "The mixed-case meta row survives the drop" 1 survivors
  assertSession pool (rawUnit ("delete from pgmq.meta where queue_name = '" <> mixed <> "'"))

-- | @enable_notify_insert@ on a mixed-case name writes a throttle row keyed by
-- the original casing, but the trigger fires on the physical table and looks up
-- the /lowercased/ name — so the row is never matched and the configured
-- throttle never applies. Since migration 0003 the trigger fails open on the
-- missing row (notifying unthrottled); before it, the same mismatch silently
-- suppressed every notification.
testNotifyThrottleNeverMatches :: IO (Pg.Database, Pool.Pool) -> TestTree
testNotifyThrottleNeverMatches getDb = testCase "mixed-case enable_notify_insert configures a throttle the trigger never matches" $ do
  (_, pool) <- getDb
  (mixed, _) <- genQueuePair
  control <- genControlName
  assertSession pool (rawUnit ("select pgmq.create('" <> mixed <> "')"))
  assertSession pool (rawUnit ("select pgmq.enable_notify_insert('" <> mixed <> "', 60000)"))
  assertSession pool (rawUnit ("select pgmq.create('" <> control <> "')"))
  assertSession pool (rawUnit ("select pgmq.enable_notify_insert('" <> control <> "', 60000)"))
  void $ assertSession pool (rawIds ("select pgmq.send('" <> mixed <> "', '{\"probe\":\"mixed\"}'::jsonb)"))
  void $ assertSession pool (rawIds ("select pgmq.send('" <> control <> "', '{\"probe\":\"control\"}'::jsonb)"))
  mixedFrozen <-
    assertSession pool $
      rawBool
        ( "select last_notified_at = to_timestamp(0) from pgmq.notify_insert_throttle where queue_name = '"
            <> mixed
            <> "'"
        )
  assertBool
    "The mixed-case throttle row is never matched: last_notified_at stays at the epoch"
    mixedFrozen
  controlFrozen <-
    assertSession pool $
      rawBool
        ( "select last_notified_at = to_timestamp(0) from pgmq.notify_insert_throttle where queue_name = '"
            <> control
            <> "'"
        )
  assertBool
    "The lowercase control's throttle row is matched and stamped"
    (not controlFrozen)
  void $ assertSession pool (rawBool ("select pgmq.drop_queue('" <> control <> "')"))
  void $ assertSession pool (rawBool ("select pgmq.drop_queue('" <> mixed <> "')"))

-- Dedicated database plumbing -------------------------------------------------

-- | Start a dedicated PostgreSQL instance with the full pgmq migration ledger
-- installed, exactly as @EphemeralDb@ does for the shared one.
acquireDb :: IO (Pg.Database, Pool.Pool)
acquireDb = do
  started <- Pg.startCached Pg.defaultConfig Pg.defaultCacheConfig
  db <- either (\err -> error ("could not start a dedicated PostgreSQL: " <> show err)) pure started
  component <- either (error . ("Invalid PGMQ migration component: " <>) . show) pure Migration.pgmqMigrations
  plan <- either (error . ("Invalid PGMQ migration plan: " <>) . show) pure (migrationPlan (component :| []))
  installResult <- runMigrationPlan defaultRunOptions (Pg.connectionSettings db) plan
  case installResult of
    Left migrationErr -> error $ "Migration failed: " <> show migrationErr
    Right _ -> pure ()
  pool <-
    Pool.acquire $
      PoolConfig.settings
        [ PoolConfig.size 2,
          PoolConfig.staticConnectionSettings (Pg.connectionSettings db)
        ]
  pure (db, pool)

releaseDb :: (Pg.Database, Pool.Pool) -> IO ()
releaseDb (db, pool) = do
  Pool.release pool
  Pg.stop db

-- Raw statement helpers -------------------------------------------------------
--
-- Queue names are spliced into the SQL text because these tests must construct
-- names the Haskell API (rightly) refuses. Every spliced value is generated
-- below from @[A-Za-z0-9_]@, so splicing is safe here.

rawUnit :: Text -> Session ()
rawUnit sqlText = statement () (unpreparable sqlText mempty D.noResult)

rawCount :: Text -> Session Int64
rawCount sqlText = statement () (unpreparable sqlText mempty (D.singleRow (D.column (D.nonNullable D.int8))))

rawBool :: Text -> Session Bool
rawBool sqlText = statement () (unpreparable sqlText mempty (D.singleRow (D.column (D.nonNullable D.bool))))

rawIds :: Text -> Session (V.Vector Int64)
rawIds sqlText = statement () (unpreparable sqlText mempty (D.rowVector (D.column (D.nonNullable D.int8))))

assertSession :: Pool.Pool -> Session a -> IO a
assertSession pool session = do
  result <- Pool.use pool session
  case result of
    Left err -> assertFailure $ "Session failed: " <> show err
    Right a -> pure a

-- | A mixed-case name and its lowercase twin, sharing one random suffix.
genQueuePair :: IO (Text, Text)
genQueuePair = do
  suffix <- randomRIO (10000 :: Word32, 99999)
  let lower = "myqueue_" <> T.pack (show suffix)
  pure ("MyQueue_" <> T.pack (show suffix), lower)

genControlName :: IO Text
genControlName = do
  suffix <- randomRIO (10000 :: Word32, 99999)
  pure ("ctrl_" <> T.pack (show suffix))
