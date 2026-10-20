-- | Private standalone configuration, shared with the executable's tests.
module PgmqInspectCli
  ( Options (..),
    parserInfo,
    resolveDatabaseUrl,
    standaloneConfig,
    standalonePoolConfig,
  )
where

import Control.Lens ((&), (.~))
import Data.Generics.Labels ()
import Data.List.NonEmpty (nonEmpty)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Data.Version (showVersion)
import Hasql.Connection.Settings qualified as Settings
import Hasql.Pool.Config qualified as PoolConfig
import Hasql.Session qualified as Session
import Options.Applicative
import Paths_pgmq_inspect (version)
import Pgmq.Inspect.Config
import PostgresqlConnectionString qualified as ConnInfo

-- Keep option arithmetic in Integer until checked conversion, including ms -> us.
data Options = Options
  { databaseUrl :: !(Maybe String),
    host :: !String,
    port :: !Integer,
    corsOrigins :: ![String],
    pollIntervalMs :: !Integer,
    poolSize :: !Integer,
    noListener :: !Bool
  }

parserInfo :: ParserInfo Options
parserInfo =
  info
    (optionsParser <**> helper <**> versionOption)
    ( fullDesc
        <> header "pgmq-inspect - read-only HTTP and WebSocket inspection of pgmq queues"
        <> progDesc "The pgmq schema must already be installed. Uses dedicated inspection connections with finite database deadlines."
        <> failureCode 2
    )
  where
    versionOption = infoOption (showVersion version) (long "version" <> help "Print the version and exit")

optionsParser :: Parser Options
optionsParser =
  Options
    <$> optional (strOption (long "database-url" <> metavar "CONNINFO" <> help "libpq connection string or URL; defaults to PGMQ_INSPECT_DATABASE_URL"))
    <*> strOption (long "host" <> metavar "HOST" <> value "127.0.0.1" <> showDefault <> help "Bind address")
    <*> option boundedInteger (long "port" <> metavar "PORT" <> value 9092 <> showDefault <> help "Bind port (0 chooses a free port)")
    <*> many (strOption (long "cors-origin" <> metavar "ORIGIN" <> help "Allowed HTTP(S) browser origin; repeatable; CORS disabled when absent"))
    <*> option boundedInteger (long "poll-interval-ms" <> metavar "MS" <> value 5000 <> showDefault <> help "Authoritative WebSocket polling interval")
    <*> option boundedInteger (long "pool-size" <> metavar "N" <> value 4 <> showDefault <> help "Dedicated inspection pool size (shared read admission defaults to 2)")
    <*> switch (long "no-listener" <> help "Disable LISTEN acceleration; keep the authoritative poll feed")

boundedInteger :: ReadM Integer
boundedInteger = eitherReader $ \raw ->
  if null raw || length raw > 128 || not (all (\c -> c >= '0' && c <= '9') raw)
    then Left "expected at most 128 decimal digits"
    else case TR.decimal (T.pack raw) of
      Right (number, rest) | T.null rest -> Right number
      _ -> Left "expected a decimal integer"

resolveDatabaseUrl :: Maybe String -> Maybe String -> Either Text Text
resolveDatabaseUrl explicit fromEnv = case explicit of
  Just raw -> Right (T.pack raw)
  Nothing -> case fromEnv of
    Just raw | not (null raw) -> Right (T.pack raw)
    _ -> Left "no database URL: pass --database-url or set PGMQ_INSPECT_DATABASE_URL"

standaloneConfig :: Options -> Text -> Either Text (InspectServerConfig, Settings.Settings)
standaloneConfig opts raw = do
  parsed <- either (const (Left badUrl)) Right (ConnInfo.parse raw)
  if parsed == mempty then Left badUrl else Right ()
  if port opts < 0 || port opts > 65535 then Left "--port must be in 0..65535" else Right ()
  if pollIntervalMs opts <= 0 || pollIntervalMs opts > toInteger (maxBound :: Int) `div` 1000 then Left "--poll-interval-ms is out of range" else Right ()
  if poolSize opts <= 0 || poolSize opts > toInteger (maxBound :: Int) then Left "--pool-size is out of range" else Right ()
  let cors = maybe CorsDisabled CorsAllowOrigins (nonEmpty (map (TE.encodeUtf8 . T.pack) (corsOrigins opts)))
      cfg = defaultInspectConfig & #corsPolicy .~ cors & #pollIntervalUs .~ fromInteger (pollIntervalMs opts * 1000)
  checked <- validateInspectConfig cfg
  let (existingOptions, withoutOptions) = maybe ("", parsed) id (ConnInfo.interceptParam "options" parsed)
      withoutConnectTimeout = maybe withoutOptions snd (ConnInfo.interceptParam "connect_timeout" withoutOptions)
      bounded =
        withoutConnectTimeout
          <> ConnInfo.param "options" (existingOptions <> " -c statement_timeout=2000 -c lock_timeout=250")
          <> ConnInfo.param "connect_timeout" "2"
      settings = Settings.connectionString (ConnInfo.toUrl bounded) <> Settings.applicationName "pgmq-inspect"
      server = defaultInspectServerConfig & #bindHost .~ host opts & #bindPort .~ fromInteger (port opts) & #inspect .~ checked
  pure (server, settings)
  where
    badUrl = "the database URL is empty or not a valid libpq connection string"

-- | Init SQL also defends the deadlines on every pooled connection. The startup
-- options above apply to the separate listener and preserve other supplied GUCs.
standalonePoolConfig :: Options -> Settings.Settings -> PoolConfig.Config
standalonePoolConfig opts settings =
  PoolConfig.settings
    [ PoolConfig.size (fromInteger (poolSize opts)),
      PoolConfig.acquisitionTimeout 1,
      PoolConfig.staticConnectionSettings settings,
      PoolConfig.initSession (Session.script "SET statement_timeout = '2000ms'; SET lock_timeout = '250ms'")
    ]
