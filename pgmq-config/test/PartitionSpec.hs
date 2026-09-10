{-# LANGUAGE CPP #-}

module PartitionSpec (tests) where

import Control.Exception (finally)
import Control.Monad (forM_)
import Data.Int (Int32)
import Data.List (isInfixOf)
import EphemeralDb (withPgmqDb)
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Pool qualified as Pool
import Hasql.Session qualified as Session
import Hasql.Statement (preparable)
import Pgmq.Config
import Pgmq.Types (QueueName, parseQueueName, queueNameToText)
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
#ifdef PGMQ_EFFECTFUL
import Effectful (runEff)
import Effectful.Error.Static (runError)
import Pgmq.Config.Effectful (ensureQueuesReportEff)
import Pgmq.Effectful (PgmqRuntimeError, runPgmq)
#endif

tests :: TestTree
tests = testGroup "Partition compatibility" [backendTests backend | backend <- backends]

backends :: [Bool]
#ifdef PGMQ_EFFECTFUL
backends = [False, True]
#else
backends = [False]
#endif

reconcile :: Bool -> Pool.Pool -> [QueueConfig] -> IO (Either String [ReconcileAction])
#ifdef PGMQ_EFFECTFUL
reconcile True pool configs = either (Left . show) Right <$> runEff (runError @PgmqRuntimeError (runPgmq pool (ensureQueuesReportEff configs)))
#endif
reconcile _ pool configs = either (Left . show) Right <$> Pool.use pool (ensureQueuesReport configs)

backendTests :: Bool -> TestTree
backendTests effectful =
  testGroup
    (if effectful then "effectful" else "direct")
    $ [ testCase ("creation and skip " <> show requested) $ isolated $ \pool -> withPartman pool $ do
          stock <- (== Just "1.12.0") <$> lookupEnv "PGMQ_TEST_SCHEMA_VERSION"
          qn <- right (parseQueueName "partition_config")
          let config n = partitionedQueue qn (PartitionConfig "10" "100" n)
          result <- reconcile effectful pool [config requested]
          if stock && requested /= Nothing
            then case result of
              Left err -> assertBool "explicit premake requires 1.13" ("42883" `isInfixOf` err)
              Right _ -> assertFailure "unsupported premake succeeded"
            else do
              actions <- right result
              case actions of
                [CreatedQueue q _] -> q @?= qn
                other -> assertFailure ("expected creation: " <> show other)
              let expected = maybe 4 id requested
              counts pool qn >>= (@?= [expected, expected])
              -- Turn every creation call into an error, proving the existing-queue
              -- path makes no call even when a different (invalid) count is requested.
              let signature = "queue_name text, partition_interval text DEFAULT '10000', retention_interval text DEFAULT '100000'" <> if stock then "" else ", premake integer DEFAULT 4"
              session pool (Session.script ("CREATE OR REPLACE FUNCTION pgmq.create_partitioned(" <> signature <> ") RETURNS void LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION 'unexpected partition creation'; END $$"))
              skipped <- right =<< reconcile effectful pool [config (Just 0)]
              case skipped of
                [SkippedQueue q] -> q @?= qn
                other -> assertFailure ("expected skip: " <> show other)
              counts pool qn >>= (@?= [expected, expected])
      | requested <- [Nothing, Just 2]
      ]
      ++ [ testCase "zero and negative counts fail on new queues" $ forM_ [0, -1] $ \n -> isolated $ \pool -> withPartman pool $ do
             stock <- (== Just "1.12.0") <$> lookupEnv "PGMQ_TEST_SCHEMA_VERSION"
             qn <- right (parseQueueName "invalid_partition_config")
             result <- reconcile effectful pool [partitionedQueue qn (PartitionConfig "10" "100" (Just n))]
             case result of
               Left err -> assertBool "server error retained" ((if stock then "42883" else "premake must be at least 1") `isInfixOf` err)
               Right _ -> assertFailure "invalid premake succeeded"
         ]

counts :: Pool.Pool -> QueueName -> IO [Int32]
counts pool qn =
  session pool $
    Session.statement (queueNameToText qn) $
      preparable
        "select premake from partman.part_config where parent_table in ('pgmq.q_' || $1, 'pgmq.a_' || $1) order by parent_table"
        (E.param (E.nonNullable E.text))
        (D.rowList (D.column (D.nonNullable D.int4)))

session :: Pool.Pool -> Session.Session a -> IO a
session pool action = right =<< Pool.use pool action

right :: (Show e) => Either e a -> IO a
right = either (\err -> assertFailure (show err) >> fail "unreachable") pure

isolated :: (Pool.Pool -> IO ()) -> IO ()
isolated action = right =<< withPgmqDb (\pool -> action pool `finally` Pool.release pool)

withPartman :: Pool.Pool -> IO () -> IO ()
withPartman pool action = do
  available <-
    session pool $
      Session.statement () $
        preparable
          "select exists (select from pg_extension where extname = 'pg_partman')"
          E.noParams
          (D.singleRow (D.column (D.nonNullable D.bool)))
  required <- (== Just "1") <$> lookupEnv "PGMQ_REQUIRE_PARTMAN"
  if available then action else if required then assertFailure "pg_partman required" else putStrLn "SKIPPED: pg_partman is unavailable"
