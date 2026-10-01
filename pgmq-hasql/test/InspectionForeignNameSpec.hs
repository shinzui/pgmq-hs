{-# LANGUAGE OverloadedStrings #-}

-- | IR-1 acceptance 5: every inspection read works for queues whose names
-- 'parseQueueName' rejects, because an inspection surface sees whatever exists
-- in the database, not only what this library created.
--
-- The names are created and fed through raw SQL (the Haskell API rightly
-- refuses them) and read back through the new sessions. Runs on a dedicated
-- PostgreSQL instance: a mixed-case row in pgmq.meta poisons the typed
-- listQueues decoding for every test sharing a database.
module InspectionForeignNameSpec (tests) where

import Control.Monad (void)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word32)
import Database.PostgreSQL.Migrate (defaultRunOptions, migrationPlan, runMigrationPlan)
import EphemeralDb (ephemeralConfig)
import EphemeralPg qualified as Pg
import Hasql.Decoders qualified as D
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as PoolConfig
import Hasql.Session (Session, statement)
import Hasql.Statement (unpreparable)
import Pgmq.Hasql.Sessions qualified as Sessions
import Pgmq.Hasql.Statements.Types (LookupMessage (LookupMessage), PeekMessages (PeekMessages), QueueMetrics (..))
import Pgmq.Migration qualified as Migration
import Pgmq.Types (ArchivedMessage (..), Message (..), MessageId (..), parseQueueName)
import System.Random (randomRIO)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  withResource acquireDb releaseDb $ \getDb ->
    testGroup
      "Inspection of foreign queue names"
      [ testForeignName getDb "mixed-case" ("MyQueue_" <>),
        testForeignName getDb "hyphenated" ("odd-name_" <>)
      ]

testForeignName :: IO (Pg.Database, Pool.Pool) -> String -> (Text -> Text) -> TestTree
testForeignName getDb label mkName = testCase (label <> " name: peek, lookup, archive, and metrics work") $ do
  (_, pool) <- getDb
  suffix <- T.pack . show <$> randomRIO (10000 :: Word32, 99999)
  let name = mkName suffix
  -- The whole point: this library's validator refuses the name.
  case parseQueueName name of
    Left _ -> pure ()
    Right _ -> assertFailure ("expected parseQueueName to reject " <> T.unpack name)
  assertSession pool (rawUnit ("select pgmq.create('" <> name <> "')"))
  ids <- mapM (\i -> MessageId <$> assertSession pool (rawId ("select pgmq.send('" <> name <> "', '{\"i\":" <> T.pack (show i) <> "}'::jsonb)"))) [1 .. 3 :: Int]
  firstId <- case ids of
    x : _ -> pure x
    [] -> assertFailure "expected three ids"
  page <- assertSession pool (Sessions.peekMessages (PeekMessages name Nothing 10))
  map messageId (V.toList page) @?= ids
  one <- assertSession pool (Sessions.lookupMessage (LookupMessage name firstId))
  fmap messageId one @?= Just firstId
  metrics <- assertSession pool (Sessions.queueMetricsUnvalidated name)
  queueLength metrics @?= 3
  assertEqual "metrics report the name as given" name (queueName metrics)
  void $ assertSession pool (rawBool ("select pgmq.archive('" <> name <> "', " <> T.pack (show (unMessageId firstId)) <> "::bigint)"))
  archived <- assertSession pool (Sessions.peekArchivedMessages (PeekMessages name Nothing 10))
  map (messageId . archivedMessage) (V.toList archived) @?= [firstId]
  found <- assertSession pool (Sessions.lookupArchivedMessage (LookupMessage name firstId))
  fmap (messageId . archivedMessage) found @?= Just firstId
  remaining <- assertSession pool (Sessions.peekMessages (PeekMessages name Nothing 10))
  map messageId (V.toList remaining) @?= drop 1 ids
  void $ assertSession pool (rawBool ("select pgmq.drop_queue('" <> name <> "')"))

-- Dedicated database plumbing (as in MixedCaseRemediationSpec) -----------------

acquireDb :: IO (Pg.Database, Pool.Pool)
acquireDb = do
  config <- ephemeralConfig
  started <- Pg.startCached config Pg.defaultCacheConfig
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

-- Raw statement helpers. Names are spliced into SQL text because these tests
-- must construct names the Haskell API refuses; every spliced value is built
-- above from [A-Za-z0-9_-], so splicing is safe here.

rawUnit :: Text -> Session ()
rawUnit sqlText = statement () (unpreparable sqlText mempty D.noResult)

rawBool :: Text -> Session Bool
rawBool sqlText = statement () (unpreparable sqlText mempty (D.singleRow (D.column (D.nonNullable D.bool))))

rawId :: Text -> Session Int64
rawId sqlText = statement () (unpreparable sqlText mempty (D.singleRow (D.column (D.nonNullable D.int8))))

assertSession :: Pool.Pool -> Session a -> IO a
assertSession pool session = do
  result <- Pool.use pool session
  case result of
    Left err -> assertFailure $ "Session failed: " <> show err
    Right a -> pure a
