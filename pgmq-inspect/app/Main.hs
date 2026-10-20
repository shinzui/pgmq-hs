module Main (main) where

import Control.Concurrent.Async (wait)
import Control.Exception (bracket)
import Data.Text qualified as T
import Data.Version (showVersion)
import Hasql.Pool qualified as Pool
import Options.Applicative (execParser)
import Paths_pgmq_inspect (version)
import Pgmq.Inspect
import PgmqInspectCli
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, stderr, stdout)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  opts <- execParser parserInfo
  fallback <- lookupEnv "PGMQ_INSPECT_DATABASE_URL"
  raw <- either failWith pure (resolveDatabaseUrl (databaseUrl opts) fallback)
  (cfg, settings) <- either failWith pure (standaloneConfig opts raw)
  bracket (Pool.acquire (standalonePoolConfig opts settings)) Pool.release $ \pool ->
    withInspectServer cfg (poolEnv pool (if noListener opts then Nothing else Just settings)) $ \running -> do
      let bind = host opts
          urlHost = if ':' `elem` bind && not ('[' `elem` bind) then "[" <> bind <> "]" else bind
      putStrLn ("pgmq-inspect " <> showVersion version <> " listening on http://" <> urlHost <> ":" <> show (serverPort running))
      wait (serverThread running)

failWith :: T.Text -> IO a
failWith message = hPutStrLn stderr ("pgmq-inspect: " <> T.unpack message) >> exitWith (ExitFailure 2)
