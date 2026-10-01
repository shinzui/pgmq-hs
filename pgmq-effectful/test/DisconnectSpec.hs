{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | BUG-1: a connection lost while a statement is in flight must classify as
-- transient, whichever hasql constructor it arrives in.
--
-- Drives three real faults against a dedicated PostgreSQL (never the
-- suite-shared one) while a 'readWithPoll' is blocked server-side, records
-- what surfaced, and asserts that 'isTransient' says retry and that the same
-- pool completes a later send.
--
-- All work happens inside one 'withResource' acquisition; the test cases are
-- pure assertions over the recorded observations, because tasty runs cases
-- concurrently and a sequence of faults must not be split across them.
module DisconnectSpec (tests) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, try)
import Data.Aeson qualified as Aeson
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int32, Int64)
import Data.Text qualified as T
import Data.Word (Word32)
import Effectful (Eff, IOE, runEff)
import Effectful.Error.Static (Error, runError)
import EphemeralDb (ephemeralConfig, installPgmqNative)
import EphemeralPg qualified as Pg
-- 'shutdownMode' names a field of both Pg.Config and Pg.Database, so the record
-- update below needs the selector from the module that defines only Database.
import EphemeralPg.Database qualified as PgDb
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Errors qualified as HasqlErrors
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as PoolConfig
import Hasql.Session (Session)
import Hasql.Session qualified as Session
import Hasql.Statement (Statement, preparable)
import Pgmq.Effectful
  ( MessageBody (..),
    Pgmq,
    PgmqRuntimeError (..),
    QueueName,
    ReadWithPollMessage (..),
    SendMessage (..),
    createQueue,
    isAmbiguousReply,
    isTransient,
    parseQueueName,
    readWithPoll,
    runPgmq,
    sendMessage,
  )
import System.Posix.Signals (sigKILL, signalProcess)
import System.Random (randomRIO)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

-- | What one fault produced.
data FaultObservation = FaultObservation
  { -- | The error the in-flight poll surfaced; Nothing means it completed.
    obsError :: !(Maybe PgmqRuntimeError),
    -- | Attempts the same pool needed to complete a send afterwards.
    obsRecoveryAttempts :: !Int,
    -- | Errors seen on the way to recovery, in order.
    obsRecoveryErrors :: ![PgmqRuntimeError],
    -- | Whether the send eventually succeeded within the attempt budget.
    obsRecovered :: !Bool
  }

data DisconnectObservations = DisconnectObservations
  { obsTerminate :: !FaultObservation,
    obsKill :: !FaultObservation,
    obsRestart :: !FaultObservation
  }

tests :: TestTree
tests =
  withResource runDisconnectCycle (const (pure ())) $ \getObs ->
    testGroup
      "DisconnectSpec (BUG-1)"
      [ faultCases "backend termination" (obsTerminate <$> getObs),
        faultCases "backend SIGKILL" (obsKill <$> getObs),
        faultCases "immediate restart" (obsRestart <$> getObs),
        testCase "SIGKILL reply is ambiguous" $ do
          obs <- obsKill <$> getObs
          case obsError obs of
            Nothing -> pure ()
            Just err -> assertBool ("expected an ambiguous reply, got " <> show err) (isAmbiguousReply err)
      ]

faultCases :: String -> IO FaultObservation -> TestTree
faultCases label getObs =
  testGroup
    label
    [ testCase "surfaces an error" $ do
        obs <- getObs
        case obsError obs of
          Nothing -> assertFailure "the in-flight poll completed; the fault did not land"
          Just _ -> pure (),
      testCase "error is a disconnect shape" $ do
        obs <- getObs
        case obsError obs of
          Nothing -> pure ()
          Just err -> assertBool ("unexpected shape: " <> show err) (isDisconnectShape err),
      testCase "error is transient" $ do
        obs <- getObs
        case obsError obs of
          Nothing -> pure ()
          Just err -> assertBool ("expected transient, got " <> show err) (isTransient err),
      testCase "ambiguous replies are transient" $ do
        obs <- getObs
        case obsError obs of
          Nothing -> pure ()
          Just err ->
            assertBool
              ("ambiguous but not transient: " <> show err)
              (not (isAmbiguousReply err) || isTransient err),
      testCase "same pool recovers" $ do
        obs <- getObs
        assertBool
          ( "pool did not recover within "
              <> show (obsRecoveryAttempts obs)
              <> " attempts; errors seen: "
              <> show (obsRecoveryErrors obs)
          )
          (obsRecovered obs)
    ]

-- | The values a lost connection is known to arrive in (see design note 017).
isDisconnectShape :: PgmqRuntimeError -> Bool
isDisconnectShape = \case
  PgmqSessionError (HasqlErrors.ConnectionSessionError _) -> True
  PgmqSessionError (HasqlErrors.StatementSessionError _ _ _ _ _ statementError) ->
    case statementError of
      HasqlErrors.ServerStatementError (HasqlErrors.ServerError code _ _ _ _) ->
        code `elem` ["", "57P01", "57P02"]
      HasqlErrors.UnexpectedRowCountStatementError 1 1 1 -> True
      _ -> False
  _ -> False

-- The fault cycle -------------------------------------------------------------

-- | Start a dedicated PostgreSQL, run the three faults in order against one
-- size-1 pool, and record what each produced. Every resource is released
-- before this returns.
runDisconnectCycle :: IO DisconnectObservations
runDisconnectCycle = do
  db0 <- startOrFail
  ref <- newIORef db0
  bracket (pure ref) (\r -> readIORef r >>= Pg.stop) $ \dbRef -> do
    installPgmqNative (Pg.connectionSettings db0)
    -- The poll queue stays empty so every poll blocks server-side; recovery
    -- sends go to a separate queue so they never satisfy a later poll.
    pollQueue <- genQueueName "disconnect_poll_"
    sendQueue <- genQueueName "disconnect_send_"
    bracket (acquirePool db0) Pool.release $ \pool -> do
      runOrFail pool (createQueue pollQueue >> createQueue sendQueue)

      terminate <- faultRound "backend termination" dbRef pool pollQueue sendQueue $ \pid -> do
        db <- readIORef dbRef
        bracket (acquirePool db) Pool.release $ \control ->
          () <$ session control (Session.statement pid terminateBackend)

      kill <- faultRound "backend SIGKILL" dbRef pool pollQueue sendQueue $ \pid ->
        signalProcess sigKILL (fromIntegral pid)

      restart <- faultRound "immediate restart" dbRef pool pollQueue sendQueue $ \_pid -> do
        db <- readIORef dbRef
        db' <- crashAndRecover db
        writeIORef dbRef db'

      pure DisconnectObservations {obsTerminate = terminate, obsKill = kill, obsRestart = restart}

-- | Read the pool connection's backend pid, block it in 'readWithPoll', inject
-- the fault, collect what the poll surfaced, and then measure how many sends
-- the same pool needs to succeed again.
faultRound ::
  String ->
  IORef Pg.Database ->
  Pool.Pool ->
  QueueName ->
  QueueName ->
  (Int32 -> IO ()) ->
  IO FaultObservation
faultRound label dbRef pool pollQueue sendQueue inject = do
  -- ephemeral-pg runs PostgreSQL with fsync and synchronous_commit off, so a
  -- crash would otherwise discard the queues still sitting in the WAL buffers.
  session pool (Session.script "checkpoint")
  pid <- session pool (Session.statement () backendPid)
  outcomeVar <- newEmptyMVar
  _ <- forkIO $ do
    result <- try @SomeException (runPoll pool pollQueue)
    putMVar outcomeVar result
  db <- readIORef dbRef
  awaitPolling db pid 200
  inject pid
  outcome <- takeMVar outcomeVar
  err <- case outcome of
    Left exc -> assertFailure (label <> ": poll thread threw " <> show exc)
    Right (Left runtimeErr) -> pure (Just runtimeErr)
    Right (Right ()) -> pure Nothing
  (attempts, errors, recovered) <- recover pool sendQueue 100
  pure
    FaultObservation
      { obsError = err,
        obsRecoveryAttempts = attempts,
        obsRecoveryErrors = errors,
        obsRecovered = recovered
      }

runPoll :: Pool.Pool -> QueueName -> IO (Either PgmqRuntimeError ())
runPoll pool queue =
  runPgmqIO pool . (() <$) . readWithPoll $
    ReadWithPollMessage
      { queueName = queue,
        delay = 30,
        batchSize = Just 1,
        maxPollSeconds = 15,
        pollIntervalMs = 100,
        conditional = Nothing
      }

-- | Wait until the server shows the given backend busy in @read_with_poll@.
-- The attacked pool's only connection is busy, so this opens its own
-- short-lived control pool each time (the SIGKILL round restarts every
-- backend, so nothing is shared across rounds).
awaitPolling :: Pg.Database -> Int32 -> Int -> IO ()
awaitPolling db pid budget =
  bracket (acquirePool db) Pool.release $ \control -> go control budget
  where
    go _ 0 = assertFailure ("backend " <> show pid <> " never showed read_with_poll as active")
    go control n = do
      count <- Pool.use control (Session.statement pid activePollCount)
      case count of
        Right 1 -> pure ()
        _ -> threadDelay 50_000 >> go control (n - 1)

-- | Send one message through the attacked pool, retrying while the error is
-- transient, with a 100 ms pause, up to the attempt budget. Returns the
-- attempts used, every error seen in order, and whether a send succeeded. A
-- permanent error stops the loop.
recover :: Pool.Pool -> QueueName -> Int -> IO (Int, [PgmqRuntimeError], Bool)
recover pool queue budget = go 1 []
  where
    go attempt seen = do
      result <-
        runPgmqIO pool $
          () <$ sendMessage SendMessage {queueName = queue, messageBody = MessageBody (Aeson.String "probe"), delay = Nothing}
      case result of
        Right () -> pure (attempt, reverse seen, True)
        Left err
          | attempt >= budget || not (isTransient err) -> pure (attempt, reverse (err : seen), False)
          | otherwise -> threadDelay 100_000 >> go (attempt + 1) (err : seen)

-- | Stop PostgreSQL with SIGQUIT and start it again on the same data directory
-- and port. Retries once, then fails loudly.
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
  config <- ephemeralConfig
  result <- Pg.startCached config Pg.defaultCacheConfig
  case result of
    Left err -> assertFailure $ "could not start a dedicated PostgreSQL: " <> show err
    Right db -> pure db

-- | Size 1 is essential: it guarantees the backend pid read before the fault
-- belongs to the connection the poll uses.
acquirePool :: Pg.Database -> IO Pool.Pool
acquirePool db =
  Pool.acquire $
    PoolConfig.settings
      [ PoolConfig.size 1,
        PoolConfig.staticConnectionSettings (Pg.connectionSettings db)
      ]

runPgmqIO :: Pool.Pool -> Eff '[Pgmq, Error PgmqRuntimeError, IOE] a -> IO (Either PgmqRuntimeError a)
runPgmqIO pool action = either (Left . snd) Right <$> runEff (runError @PgmqRuntimeError (runPgmq pool action))

runOrFail :: Pool.Pool -> Eff '[Pgmq, Error PgmqRuntimeError, IOE] a -> IO a
runOrFail pool action =
  runPgmqIO pool action >>= either (\err -> assertFailure ("pgmq operation failed: " <> show err)) pure

session :: Pool.Pool -> Session a -> IO a
session pool s = do
  result <- Pool.use pool s
  case result of
    Left err -> assertFailure $ "Session failed: " <> show err
    Right a -> pure a

genQueueName :: T.Text -> IO QueueName
genQueueName prefix = do
  suffix <- randomRIO (10000 :: Word32, 99999)
  case parseQueueName (prefix <> T.pack (show suffix)) of
    Left err -> error $ "Failed to generate queue name: " <> show err
    Right qn -> pure qn

backendPid :: Statement () Int32
backendPid = preparable "select pg_backend_pid()" E.noParams (D.singleRow (D.column (D.nonNullable D.int4)))

terminateBackend :: Statement Int32 Bool
terminateBackend = preparable "select pg_terminate_backend($1)" (E.param (E.nonNullable E.int4)) (D.singleRow (D.column (D.nonNullable D.bool)))

activePollCount :: Statement Int32 Int64
activePollCount =
  preparable
    "select count(*)::int8 from pg_stat_activity where pid = $1 and state = 'active' and query like '%read_with_poll%'"
    (E.param (E.nonNullable E.int4))
    (D.singleRow (D.column (D.nonNullable D.int8)))
