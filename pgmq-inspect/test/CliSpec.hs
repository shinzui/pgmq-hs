module CliSpec (tests, databaseTests) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.STM
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.List (isInfixOf)
import Data.Text qualified as T
import EphemeralPg qualified as Pg
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as D
import Hasql.Pool qualified as Pool
import Hasql.Session qualified as Session
import Hasql.Statement (unpreparable)
import HttpSpec (session, withQueue)
import Options.Applicative (ParserResult (..), defaultPrefs, execParserPure)
import PgmqInspectCli
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.IO (hGetLine)
import System.Process
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit
import TestServer

parse :: [String] -> IO Options
parse args = case execParserPure defaultPrefs parserInfo args of
  Success opts -> pure opts
  _ -> assertFailure "valid options did not parse"

tests :: TestTree
tests =
  testGroup
    "Standalone CLI"
    [ testCase "version/help do not need a database" $ forM_ ["--version", "--help"] $ \arg -> do
        (code, out, _) <- readProcessWithExitCode "pgmq-inspect" [arg] ""
        code @?= ExitSuccess
        assertBool "expected output" ((if arg == "--version" then "0.1.0.0" else "--database-url") `isInfixOf` out),
      testCase "URL errors are one line, exit 2, and never echo secrets" $ do
        inherited <- getEnvironment
        forM_ [[], ["--database-url", "password='sensitive url' not-valid"]] $ \args -> do
          let p = (proc "pgmq-inspect" args) {env = Just (filter ((/= "PGMQ_INSPECT_DATABASE_URL") . fst) inherited)}
          (code, _, err) <- readCreateProcessWithExitCode p ""
          code @?= ExitFailure 2
          length (lines err) @?= 1
          assertBool "secret sanitized" (not ("sensitive" `isInfixOf` err)),
      testCase "explicit URL takes precedence; absent environment is rejected" $ do
        resolveDatabaseUrl (Just "explicit") (Just "env") @?= Right "explicit"
        resolveDatabaseUrl Nothing (Just "env") @?= Right "env"
        assertBool "missing rejected" (either (const True) (const False) (resolveDatabaseUrl Nothing Nothing)),
      testCase "numeric and CORS bounds checked before connection allocation" $ do
        forM_ [["--port", "65536"], ["--pool-size", "0"], ["--poll-interval-ms", "0"], ["--poll-interval-ms", replicate 128 '9'], ["--cors-origin", "*"]] $ \args -> do
          opts <- parse args
          assertBool (show args) (either (const True) (const False) (standaloneConfig opts "dbname=unused"))
        forM_ [replicate 129 '9', "-1", "1x"] $ \raw -> case execParserPure defaultPrefs parserInfo ["--port", raw] of
          Failure _ -> pure ()
          _ -> assertFailure "unbounded/malformed option accepted"
    ]

databaseTests :: IO (Pool.Pool, Pg.Database) -> TestTree
databaseTests database =
  testGroup
    "Standalone database"
    [ testCase "actual binary uses environment URL, ephemeral port and CORS" $ do
        (pool, db) <- database
        inherited <- getEnvironment
        withQueue pool $ \_ -> do
          let p =
                (proc "pgmq-inspect" ["--port", "0", "--no-listener", "--cors-origin", "https://ui.example", "--poll-interval-ms", "1000"])
                  { env = Just (("PGMQ_INSPECT_DATABASE_URL", T.unpack (Pg.connectionString db)) : filter ((/= "PGMQ_INSPECT_DATABASE_URL") . fst) inherited),
                    std_out = CreatePipe
                  }
          withCreateProcess p $ \_ out _ _ -> case out of
            Nothing -> assertFailure "missing stdout pipe"
            Just output -> do
              line <- timeout 10000000 (hGetLine output) >>= maybe (assertFailure "startup timed out") pure
              let portNumber = read (reverse (takeWhile (/= ':') (reverse line)))
              assertBool "actual port" (portNumber > 0)
              r <- getWith portNumber "GET" [("Origin", "https://ui.example")] "/queues"
              replyStatus r @?= 200
              lookup "Access-Control-Allow-Origin" (replyHeaders r) @?= Just "https://ui.example"
              spec <- get portNumber "/openapi.json"
              replyStatus spec @?= 200,
      testCase "every pooled connection and listener settings enforce deadlines, preserve supplied GUC" $ do
        (_, db) <- database
        opts <- parse ["--pool-size", "4"]
        (_, settings) <- either (assertFailure . T.unpack) pure (standaloneConfig opts (Pg.connectionString db <> " options='-c work_mem=8MB -c statement_timeout=0'"))
        let probe = Session.statement () (unpreparable "SELECT current_setting('statement_timeout'), current_setting('lock_timeout'), current_setting('work_mem')" mempty (D.singleRow ((,,) <$> D.column (D.nonNullable D.text) <*> D.column (D.nonNullable D.text) <*> D.column (D.nonNullable D.text))))
        bracket (Pool.acquire (standalonePoolConfig opts settings)) Pool.release $ \pool -> do
          arrived <- newTVarIO (0 :: Int)
          let heldProbe = do
                row <- probe
                Session.onLibpqConnection
                  ( \raw -> do
                      atomically (modifyTVar' arrived (+ 1))
                      atomically (readTVar arrived >>= check . (== 4))
                      pure (Right (), raw)
                  )
                pure row
          values <- mapConcurrently (const (session pool heldProbe)) [1 .. 4 :: Int]
          values @?= replicate 4 ("2s", "250ms", "8MB")
        bracket (Connection.acquire settings >>= either (assertFailure . show) pure) Connection.release $ \conn -> do
          value <- Connection.use conn probe >>= either (assertFailure . show) pure
          value @?= ("2s", "250ms", "8MB")
    ]
