{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A queue created by any other client must not break this application's boot.
--
-- pgmq queues are ordinary SQL tables, and the server's only queue-name check is
-- length (@pgmq.validate_queue_name@ rejects names over 47 characters and
-- nothing else). The Haskell validator 'Pgmq.Types.parseQueueName' is far
-- stricter, and the typed listing decoder re-validates every name read back from
-- the database — so one foreign queue named @billing-events@ used to make
-- @listQueues@ fail to decode, and with it the pgmq-config reconciler, whose
-- first step is that listing. Someone else's queue became your boot failure.
--
-- The reconciler now snapshots existing queues through the /unvalidated/ listing
-- and compares names as plain text. This module proves it: it seeds a
-- hyphen-named queue, shows the typed listing still rejects it while the
-- unvalidated listing reads it, and then reconciles a normal declared queue
-- successfully through both the Session and the effect backend.
--
-- The hyphen matters. A hyphenated name is rejected by every generation of
-- 'Pgmq.Types.parseQueueName' — the older @[A-Za-z0-9_]@ form and the current
-- lowercase-only one alike — so these assertions do not depend on which
-- validation rules are in force.
--
-- This module runs on its own dedicated PostgreSQL instance, never the
-- suite-shared pool: tasty runs specs concurrently, and a foreign row in the
-- shared database would make every concurrent typed-@listQueues@ call fail
-- (@ConfigSpec@ makes four of them).
module ForeignQueueSpec (tests) where

import Control.Exception (bracket)
import Control.Lens ((^.))
import Data.Generics.Labels ()
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32)
import Database.PostgreSQL.Migrate
  ( defaultRunOptions,
    migrationPlan,
    runMigrationPlan,
  )
import EphemeralPg qualified as Pg
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as PoolConfig
import Hasql.Session (Session)
import Hasql.Session qualified as Session
import Hasql.Statement (Statement, preparable)
import Pgmq.Config (ReconcileAction (..), ensureQueuesReport, standardQueue)
import Pgmq.Hasql.Sessions qualified as Sessions
import Pgmq.Migration qualified as Migration
import Pgmq.Types (QueueName, UnvalidatedQueue, parseQueueName)
import System.Random (randomRIO)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
#ifdef PGMQ_EFFECTFUL
import Effectful (runEff)
import Effectful.Error.Static (runError)
import Pgmq.Config.Effectful (ensureQueuesReportEff)
import Pgmq.Effectful.Interpreter (PgmqRuntimeError, runPgmq)
#endif

-- | The foreign queue name. Hyphens are legal server-side and rejected by every
-- generation of 'Pgmq.Types.parseQueueName'.
foreignName :: Text
foreignName = "billing-events"

-- | Everything one pass against the seeded database observed, so each fact can
-- be asserted as its own test case.
data ForeignQueueObservations = ForeignQueueObservations
  { -- | The queue declared through the Session backend.
    obsQueueName :: !QueueName,
    -- | The queue declared through the effect backend.
    obsEffQueueName :: !QueueName,
    -- | The error the typed listing failed with, if it failed at all.
    obsTypedListError :: !(Maybe String),
    -- | The foreign row as seen by the unvalidated listing.
    obsUnvalidatedForeign :: !(Maybe UnvalidatedQueue),
    -- | Report of the first Session-backed reconcile.
    obsFirstReport :: ![ReconcileAction],
    -- | Report of an immediately repeated reconcile.
    obsSecondReport :: ![ReconcileAction],
    -- | @pgmq.meta@ rows still naming the foreign queue afterwards.
    obsForeignMetaCount :: !Int64,
    -- | Report of the effect-backed reconcile.
    obsEffReport :: ![ReconcileAction]
  }

tests :: TestTree
tests =
  withResource runForeignQueueCycle (const (pure ())) $ \getObs ->
    testGroup
      "ForeignQueueSpec"
      ( [ testCase "typed listQueues rejects a foreign name (evidence)" $ do
            obs <- getObs
            case obsTypedListError obs of
              Nothing ->
                assertFailure $
                  "expected the typed listQueues to fail decoding "
                    <> show foreignName
                    <> ", but it succeeded"
              Just _ -> pure ()
            case obsUnvalidatedForeign obs of
              Nothing ->
                assertFailure $
                  "the unvalidated listing did not return a row named " <> show foreignName
              Just q -> do
                (q ^. #unvalidatedName) @?= foreignName
                (q ^. #unvalidatedIsPartitioned) @?= False
                (q ^. #unvalidatedIsUnlogged) @?= False,
          testCase "ensureQueues succeeds despite a foreign queue" $ do
            obs <- getObs
            assertBool
              ( "expected CreatedQueue "
                  <> show (obsQueueName obs)
                  <> " in the first report, got "
                  <> show (obsFirstReport obs)
              )
              (any (isCreatedQueue (obsQueueName obs)) (obsFirstReport obs))
            assertBool
              ( "expected SkippedQueue "
                  <> show (obsQueueName obs)
                  <> " in the second report, got "
                  <> show (obsSecondReport obs)
              )
              (any (isSkippedQueue (obsQueueName obs)) (obsSecondReport obs))
            obsForeignMetaCount obs @?= 1
        ]
          <> effectfulCases getObs
      )

-- | The effect-backed parity case, present only when the library's @effectful@
-- flag is on — with it off, "Pgmq.Config.Effectful" is not built at all.
effectfulCases :: IO ForeignQueueObservations -> [TestTree]
#ifdef PGMQ_EFFECTFUL
effectfulCases getObs =
  [ testCase "effectful ensureQueues matches" $ do
      obs <- getObs
      assertBool
        ( "expected CreatedQueue "
            <> show (obsEffQueueName obs)
            <> " in the effectful report, got "
            <> show (obsEffReport obs)
        )
        (any (isCreatedQueue (obsEffQueueName obs)) (obsEffReport obs))
  ]
#else
effectfulCases _ = []
#endif

isCreatedQueue :: QueueName -> ReconcileAction -> Bool
isCreatedQueue qn (CreatedQueue q _) = q == qn
isCreatedQueue _ _ = False

isSkippedQueue :: QueueName -> ReconcileAction -> Bool
isSkippedQueue qn (SkippedQueue q) = q == qn
isSkippedQueue _ _ = False

-- | Start a dedicated PostgreSQL instance, seed the foreign queue, reconcile
-- through both backends, and record what happened. The instance is stopped
-- before this returns.
runForeignQueueCycle :: IO ForeignQueueObservations
runForeignQueueCycle =
  bracket startOrFail Pg.stop $ \db -> do
    installPgmq db
    bracket (acquirePool db) Pool.release observe

observe :: Pool.Pool -> IO ForeignQueueObservations
observe pool = do
  qn <- genQueueName "foreign_test_"
  effQn <- genQueueName "foreign_eff_"

  -- Seed a queue no Haskell client could have created. `%I` quoting inside
  -- pgmq.create means the hyphen reaches the physical table name intact.
  runSession pool (Session.script "select pgmq.create('billing-events')")

  typedResult <- Pool.use pool Sessions.listQueues
  unvalidated <- runSession pool Sessions.listQueuesUnvalidated

  firstReport <- runSession pool (ensureQueuesReport [standardQueue qn])
  secondReport <- runSession pool (ensureQueuesReport [standardQueue qn])
  metaCount <- runSession pool (Session.statement foreignName metaRowCount)

  effReport <- runEffectfulReconcile pool effQn

  pure
    ForeignQueueObservations
      { obsQueueName = qn,
        obsEffQueueName = effQn,
        obsTypedListError = either (Just . show) (const Nothing) typedResult,
        obsUnvalidatedForeign =
          case filter (\q -> q ^. #unvalidatedName == foreignName) unvalidated of
            (q : _) -> Just q
            [] -> Nothing,
        obsFirstReport = firstReport,
        obsSecondReport = secondReport,
        obsForeignMetaCount = metaCount,
        obsEffReport = effReport
      }

-- | Reconcile through the @Pgmq@ effect with the plain interpreter, so the test
-- pins that /both/ backends read the lenient listing. With the @effectful@ flag
-- off this returns no actions and 'effectfulCases' emits no test case.
runEffectfulReconcile :: Pool.Pool -> QueueName -> IO [ReconcileAction]
#ifdef PGMQ_EFFECTFUL
runEffectfulReconcile pool qn = do
  result <-
    runEff . runError @PgmqRuntimeError . runPgmq pool $
      ensureQueuesReportEff [standardQueue qn]
  case result of
    Left (_cs, err) ->
      assertFailure $ "effect-backed ensureQueuesReportEff failed: " <> show err
    Right actions -> pure actions
#else
runEffectfulReconcile _pool _qn = pure []
#endif

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

-- | How many @pgmq.meta@ rows name the given queue.
metaRowCount :: Statement Text Int64
metaRowCount = preparable sql encoder decoder
  where
    sql = "select count(*)::int8 from pgmq.meta where queue_name = $1"
    encoder = E.param (E.nonNullable E.text)
    decoder = D.singleRow (D.column (D.nonNullable D.int8))

genQueueName :: Text -> IO QueueName
genQueueName prefix = do
  suffix <- randomRIO (10000 :: Word32, 99999)
  case parseQueueName (prefix <> T.pack (show suffix)) of
    Left err -> error $ "Failed to generate queue name: " <> show err
    Right qn -> pure qn
