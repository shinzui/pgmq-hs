module ErrorSpec (tests, serverError, failingEnv) where

import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.MVar
import Control.Exception (bracket)
import Control.Monad (void)
import Data.Aeson (Value (..))
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.IORef
import Data.List (isInfixOf)
import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian)
import Effectful (liftIO)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (throwError)
import Hasql.Errors qualified as H
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as PC
import Pgmq.Effectful (PgmqRuntimeError (..))
import Pgmq.Effectful.Effect (Pgmq (..))
import Pgmq.Inspect.Config
import Pgmq.Inspect.Env
import Pgmq.Types (UnvalidatedQueue (..))
import RouterSpec (errorCodeOf, mockEnv)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit
import TestServer

serverError :: Text -> PgmqRuntimeError
serverError code = PgmqSessionError (H.StatementSessionError 1 0 "SECRET_SQL" ["SECRET_PARAMS"] True (H.ServerStatementError (H.ServerError code "SECRET_MESSAGE" (Just "SECRET_DETAIL") (Just "SECRET_HINT") Nothing)))

failingEnv :: PgmqRuntimeError -> InspectEnv
failingEnv err = InspectEnv (runnerFromInterpreter (interpret (\_ _ -> throwError err))) Nothing

tests :: TestTree
tests =
  testGroup
    "Safe errors and shared admission"
    [ testCase "an unreachable database answers 503 database_unavailable" $
        bracket (Pool.acquire (PC.settings [PC.size 1, PC.staticConnectionSettings "host=127.0.0.1 port=1 user=nobody dbname=nonexistent connect_timeout=1"])) Pool.release $ \pool ->
          withTestServer defaultInspectConfig (poolEnv pool Nothing) $ \port -> do
            r <- get port "/queues"
            replyStatus r @?= 503
            errorCodeOf r >>= (@?= String "database_unavailable"),
      testCase "global missing schema/table remains 500; diagnostics never reach public responses" $
        mapM_
          ( \code -> withTestServer defaultInspectConfig (failingEnv (serverError code)) $ \port -> do
              r <- get port "/queues"
              replyStatus r @?= 500
              errorCodeOf r >>= (@?= String "database_error")
              assertBool "no raw diagnostics" (not ("SECRET" `isInfixOf` LBS.unpack (replyBody r)))
              ready <- get port "/health/ready"
              replyStatus ready @?= 503
              assertBool "readiness safe" (not ("SECRET" `isInfixOf` LBS.unpack (replyBody ready)))
          )
          ["42P01", "23505"],
      testCase "statement deadline maps to inspection_timeout" $
        withTestServer defaultInspectConfig (failingEnv (serverError "57014")) $ \port -> do
          r <- get port "/queues"
          replyStatus r @?= 503
          errorCodeOf r >>= (@?= String "inspection_timeout"),
      testCase "a missing queue answers 404 queue_not_found for every scoped route" $
        withTestServer defaultInspectConfig mockEnv $ \port ->
          mapM_
            ( \suffix -> do
                r <- get port ("/queues/missing" <> suffix)
                replyStatus r @?= 404
                errorCodeOf r >>= (@?= String "queue_not_found")
            )
            ["", "/metrics", "/bindings", "/messages", "/archive", "/messages/1", "/archive/1"],
      testCase "42P01 on an existing queue is database_error; a confirmed concurrent drop is queue_not_found" $
        mapM_
          ( \dropped -> do
              n <- newIORef (0 :: Int)
              let env =
                    InspectEnv
                      ( runnerFromInterpreter
                          ( interpret
                              ( \_ -> \case
                                  ListQueuesUnvalidated -> liftIO $ do
                                    calls <- atomicModifyIORef' n (\x -> (x + 1, x))
                                    pure [UnvalidatedQueue "orders" (UTCTime (fromGregorian 2026 1 1) 0) False False | calls == 0 || not dropped]
                                  QueueMetricsUnvalidated _ -> throwError (serverError "42P01")
                                  _ -> error "unexpected operation"
                              )
                          )
                      )
                      Nothing
              withTestServer defaultInspectConfig env $ \port -> do
                r <- get port "/queues/orders/metrics"
                replyStatus r @?= if dropped then 404 else 500
                errorCodeOf r >>= (@?= String (if dropped then "queue_not_found" else "database_error"))
          )
          [False, True],
      testCase "saturated admission rejects promptly and retains only bounded live work" $ do
        started <- newEmptyMVar
        release <- newEmptyMVar
        calls <- newIORef (0 :: Int)
        let env =
              InspectEnv
                ( \program -> do
                    atomicModifyIORef' calls (\n -> (n + 1, ()))
                    putMVar started ()
                    takeMVar release
                    runInspection mockEnv program
                )
                Nothing
        withTestServer defaultInspectConfig {maxConcurrentReads = 1} env $ \port ->
          withAsync (get port "/queues") $ \first -> do
            takeMVar started
            outcome <- timeout 500000 (get port "/queues")
            case outcome of
              Nothing -> assertFailure "admission queued a request"
              Just r -> (replyStatus r @?= 503) >> (errorCodeOf r >>= (@?= String "inspection_busy"))
            readIORef calls >>= (@?= 1)
            get port "/health/live" >>= (\r -> replyStatus r @?= 200)
            putMVar release ()
            void (wait first)
    ]
