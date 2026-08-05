{-# LANGUAGE OverloadedStrings #-}

-- | PGH-7: the documented mixed-case remediation must preserve topic bindings
-- and notification configuration, transactionally, and must be safe to rerun.
--
-- The stricter 'Pgmq.Types.parseQueueName' makes pre-existing mixed-case
-- @pgmq.meta@ rows fail @listQueues@ decoding, so deployments must remediate
-- before upgrading. The remediation (canonical copy in
-- @docs\/design\/016-queue-name-validation.md@) cannot be a naive rename or
-- delete: both foreign keys onto @pgmq.meta (queue_name)@ — from
-- @pgmq.topic_bindings@ and @pgmq.notify_insert_throttle@ — lack @ON UPDATE@
-- and carry @ON DELETE CASCADE@, so an UPDATE of a referenced parent fails and
-- a DELETE silently destroys routing and notification configuration.
--
-- This module runs on its own dedicated PostgreSQL instance: the remediation
-- sweeps every mixed-case row in the database, so it must never share an
-- instance with other tests that construct mixed-case rows (AliasingSpec), let
-- alone the suite-shared pool.
module MixedCaseRemediationSpec (tests) where

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
      "Mixed-Case Remediation (PGH-7)"
      [ testNoTwinRename getDb,
        testTwinMerge getDb
      ]

-- | The documented remediation, verbatim from design note 016. One DO block =
-- one transaction; rerunning it after success is a no-op because the driving
-- query returns no rows.
remediationSql :: Text
remediationSql =
  T.unlines
    [ "DO $remediate$",
      "DECLARE",
      "  bad RECORD;",
      "  twin_exists BOOLEAN;",
      "BEGIN",
      "  FOR bad IN",
      "    SELECT m.queue_name AS mixed_name, lower(m.queue_name) AS canonical_name",
      "    FROM pgmq.meta m",
      "    WHERE m.queue_name <> lower(m.queue_name)",
      "  LOOP",
      "    PERFORM pgmq.acquire_queue_lock(bad.mixed_name);",
      "    PERFORM pgmq.acquire_queue_lock(bad.canonical_name);",
      "    PERFORM 1 FROM pgmq.meta",
      "      WHERE queue_name IN (bad.mixed_name, bad.canonical_name)",
      "      FOR UPDATE;",
      "",
      "    twin_exists := EXISTS (",
      "      SELECT 1 FROM pgmq.meta WHERE queue_name = bad.canonical_name",
      "    );",
      "",
      "    IF NOT twin_exists THEN",
      "      INSERT INTO pgmq.meta (queue_name, is_partitioned, is_unlogged, created_at)",
      "      SELECT bad.canonical_name, m.is_partitioned, m.is_unlogged, m.created_at",
      "      FROM pgmq.meta m WHERE m.queue_name = bad.mixed_name;",
      "    ELSE",
      "      DELETE FROM pgmq.topic_bindings b",
      "      WHERE b.queue_name = bad.mixed_name",
      "        AND EXISTS (",
      "          SELECT 1 FROM pgmq.topic_bindings t",
      "          WHERE t.queue_name = bad.canonical_name AND t.pattern = b.pattern",
      "        );",
      "      DELETE FROM pgmq.notify_insert_throttle",
      "      WHERE queue_name = bad.mixed_name",
      "        AND EXISTS (",
      "          SELECT 1 FROM pgmq.notify_insert_throttle",
      "          WHERE queue_name = bad.canonical_name",
      "        );",
      "    END IF;",
      "",
      "    UPDATE pgmq.topic_bindings SET queue_name = bad.canonical_name",
      "    WHERE queue_name = bad.mixed_name;",
      "    UPDATE pgmq.notify_insert_throttle SET queue_name = bad.canonical_name",
      "    WHERE queue_name = bad.mixed_name;",
      "",
      "    DELETE FROM pgmq.meta WHERE queue_name = bad.mixed_name;",
      "  END LOOP;",
      "END",
      "$remediate$"
    ]

-- | Detection query from design note 016, reduced to a count.
detectionCount :: Session Int64
detectionCount = rawCount "select count(*) from pgmq.meta m where m.queue_name <> lower(m.queue_name)"

-- | No lowercase twin: the mixed-case row is renamed in place; both topic
-- bindings (with their @bound_at@) and the throttle configuration survive
-- under the canonical name, and the notification trigger starts matching.
testNoTwinRename :: IO (Pg.Database, Pool.Pool) -> TestTree
testNoTwinRename getDb = testCase "no-twin rename preserves bindings and throttle configuration" $ do
  (_, pool) <- getDb
  suffix <- genSuffix
  let mixed = "Legacy_" <> suffix
      canonical = "legacy_" <> suffix
  assertSession pool (rawUnit ("select pgmq.create('" <> mixed <> "')"))
  assertSession pool (rawUnit ("select pgmq.enable_notify_insert('" <> mixed <> "', 750)"))
  assertSession pool (rawUnit ("select pgmq.bind_topic('orders.*', '" <> mixed <> "')"))
  assertSession pool (rawUnit ("select pgmq.bind_topic('audit.#', '" <> mixed <> "')"))
  bindingsBefore <- assertSession pool (bindingFingerprints mixed)
  assertEqual "Seeded two bindings on the mixed-case row" 2 (V.length bindingsBefore)

  assertSession pool (rawUnit remediationSql)

  detected <- assertSession pool detectionCount
  assertEqual "Detection query finds nothing after remediation" 0 detected
  canonicalMeta <- assertSession pool (rawCount ("select count(*) from pgmq.meta where queue_name = '" <> canonical <> "'"))
  assertEqual "The canonical meta row exists" 1 canonicalMeta
  mixedMeta <- assertSession pool (rawCount ("select count(*) from pgmq.meta where queue_name = '" <> mixed <> "'"))
  assertEqual "The mixed-case meta row is gone" 0 mixedMeta
  bindingsAfter <- assertSession pool (bindingFingerprints canonical)
  assertEqual
    "Both bindings survive under the canonical name with bound_at preserved"
    (V.toList bindingsBefore)
    (V.toList bindingsAfter)
  interval <- assertSession pool (throttleInterval canonical)
  assertEqual "The throttle configuration survives under the canonical name" 750 interval

  -- Functional proof: the trigger's lowercase lookup now matches the throttle
  -- row, so a send stamps last_notified_at off the epoch.
  void $ assertSession pool (rawIds ("select pgmq.send('" <> canonical <> "', '{\"probe\":true}'::jsonb)"))
  stamped <-
    assertSession pool $
      rawBool
        ( "select last_notified_at > to_timestamp(0) from pgmq.notify_insert_throttle where queue_name = '"
            <> canonical
            <> "'"
        )
  assertBool "After remediation the trigger matches and stamps the throttle row" stamped

  -- Rerun: the remediation must be a no-op now.
  assertSession pool (rawUnit remediationSql)
  bindingsRerun <- assertSession pool (bindingFingerprints canonical)
  assertEqual "A second run changes no bindings" (V.toList bindingsAfter) (V.toList bindingsRerun)
  intervalRerun <- assertSession pool (throttleInterval canonical)
  assertEqual "A second run changes no throttle configuration" 750 intervalRerun
  void $ assertSession pool (rawBool ("select pgmq.drop_queue('" <> canonical <> "')"))

-- | A lowercase twin exists: the two rows already alias one physical table.
-- Bindings move to the twin (duplicates deduplicate), the twin's own throttle
-- configuration wins, and the mixed-case row disappears.
testTwinMerge :: IO (Pg.Database, Pool.Pool) -> TestTree
testTwinMerge getDb = testCase "twin merge moves bindings, dedupes, and keeps the canonical throttle" $ do
  (_, pool) <- getDb
  suffix <- genSuffix
  let mixed = "Shared_" <> suffix
      canonical = "shared_" <> suffix
  assertSession pool (rawUnit ("select pgmq.create('" <> mixed <> "')"))
  assertSession pool (rawUnit ("select pgmq.create('" <> canonical <> "')"))
  assertSession pool (rawUnit ("select pgmq.bind_topic('dup.*', '" <> mixed <> "')"))
  assertSession pool (rawUnit ("select pgmq.bind_topic('dup.*', '" <> canonical <> "')"))
  assertSession pool (rawUnit ("select pgmq.bind_topic('only.*', '" <> mixed <> "')"))
  assertSession pool (rawUnit ("select pgmq.enable_notify_insert('" <> mixed <> "', 900)"))
  assertSession pool (rawUnit ("select pgmq.enable_notify_insert('" <> canonical <> "', 250)"))

  assertSession pool (rawUnit remediationSql)

  detected <- assertSession pool detectionCount
  assertEqual "Detection query finds nothing after remediation" 0 detected
  metaRows <- assertSession pool (rawCount ("select count(*) from pgmq.meta where lower(queue_name) = '" <> canonical <> "'"))
  assertEqual "One meta row remains for the pair" 1 metaRows
  patterns <- assertSession pool (bindingPatterns canonical)
  assertEqual
    "The twin holds the union of bindings, duplicates collapsed"
    ["dup.*", "only.*"]
    (V.toList patterns)
  orphanBindings <- assertSession pool (rawCount ("select count(*) from pgmq.topic_bindings where queue_name = '" <> mixed <> "'"))
  assertEqual "No bindings remain under the mixed-case name" 0 orphanBindings
  interval <- assertSession pool (throttleInterval canonical)
  assertEqual "The canonical queue's own throttle configuration wins" 250 interval
  orphanThrottles <- assertSession pool (rawCount ("select count(*) from pgmq.notify_insert_throttle where queue_name = '" <> mixed <> "'"))
  assertEqual "No throttle row remains under the mixed-case name" 0 orphanThrottles

  -- Rerun: still nothing to do.
  assertSession pool (rawUnit remediationSql)
  patternsRerun <- assertSession pool (bindingPatterns canonical)
  assertEqual "A second run changes no bindings" (V.toList patterns) (V.toList patternsRerun)
  void $ assertSession pool (rawBool ("select pgmq.drop_queue('" <> canonical <> "')"))

-- Dedicated database plumbing -------------------------------------------------

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
-- names the Haskell API (rightly) refuses; every spliced value is generated
-- below from @[A-Za-z0-9_]@.

rawUnit :: Text -> Session ()
rawUnit sqlText = statement () (unpreparable sqlText mempty D.noResult)

rawCount :: Text -> Session Int64
rawCount sqlText = statement () (unpreparable sqlText mempty (D.singleRow (D.column (D.nonNullable D.int8))))

rawBool :: Text -> Session Bool
rawBool sqlText = statement () (unpreparable sqlText mempty (D.singleRow (D.column (D.nonNullable D.bool))))

rawIds :: Text -> Session (V.Vector Int64)
rawIds sqlText = statement () (unpreparable sqlText mempty (D.rowVector (D.column (D.nonNullable D.int8))))

rawTexts :: Text -> Session (V.Vector Text)
rawTexts sqlText = statement () (unpreparable sqlText mempty (D.rowVector (D.column (D.nonNullable D.text))))

-- | Pattern plus creation timestamp, so equality across the remediation proves
-- @bound_at@ survived, not merely the pattern.
bindingFingerprints :: Text -> Session (V.Vector Text)
bindingFingerprints qname =
  rawTexts
    ( "select pattern || '|' || bound_at::text from pgmq.topic_bindings where queue_name = '"
        <> qname
        <> "' order by pattern"
    )

bindingPatterns :: Text -> Session (V.Vector Text)
bindingPatterns qname =
  rawTexts ("select pattern from pgmq.topic_bindings where queue_name = '" <> qname <> "' order by pattern")

throttleInterval :: Text -> Session Int64
throttleInterval qname =
  rawCount ("select throttle_interval_ms::int8 from pgmq.notify_insert_throttle where queue_name = '" <> qname <> "'")

assertSession :: Pool.Pool -> Session a -> IO a
assertSession pool session = do
  result <- Pool.use pool session
  case result of
    Left err -> assertFailure $ "Session failed: " <> show err
    Right a -> pure a

genSuffix :: IO Text
genSuffix = do
  suffix <- randomRIO (10000 :: Word32, 99999)
  pure (T.pack (show suffix))
