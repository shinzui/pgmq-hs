module BenchSetup
  ( -- * Pool Management
    withBenchPool,

    -- * Session Execution
    runSession,

    -- * Schema Installation
    installPgmqSchema,

    -- * Queue Lifecycle
    createBenchQueue,
    dropBenchQueue,
    purgeQueue,
    seedQueue,
    withQueue,

    -- * Queue Names
    benchQueueName,
    uniqueQueueName,

    -- * Payload Generation
    generatePayload,
    generatePayloads,

    -- * Effectful Runner
    runEffectful,
  )
where

import BenchConfig (BenchConfig (..))
import Control.Exception (bracket)
import Data.Aeson (object, (.=))
import Data.ByteString.Char8 qualified as BS
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Word (Word32)
import Effectful (Eff, IOE, runEff)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Hasql.Connection.Setting qualified as Setting
import Hasql.Connection.Setting.Connection qualified as Connection.Setting
import Hasql.Pool (Pool, UsageError)
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as PoolConfig
import Hasql.Session (Session)
import Pgmq.Effectful.Effect (Pgmq)
import Pgmq.Effectful.Interpreter (PgmqError (..), runPgmq)
import Pgmq.Hasql.Sessions qualified as Sessions
import Pgmq.Hasql.Statements.Types (BatchSendMessage (..))
import Pgmq.Migration qualified as Migration
import Pgmq.Types (MessageBody (..), MessageId, QueueName, parseQueueName)
import System.Random (randomRIO)

-- | Create a connection pool with benchmark-optimized settings
withBenchPool :: BS.ByteString -> (Pool -> IO a) -> IO a
withBenchPool connStr action = do
  let connSettings = [Setting.connection (Connection.Setting.string (TE.decodeUtf8 connStr))]
      poolConfig =
        PoolConfig.settings
          [ PoolConfig.size 20,
            PoolConfig.acquisitionTimeout 30,
            PoolConfig.staticConnectionSettings connSettings
          ]
  bracket (Pool.acquire poolConfig) Pool.release action

-- | Execute a hasql session using the pool
runSession :: Pool -> Session a -> IO (Either UsageError a)
runSession = Pool.use

-- | Install pgmq schema via migration
installPgmqSchema :: Pool -> IO ()
installPgmqSchema pool = do
  result <- Pool.use pool Migration.migrate
  case result of
    Left poolErr -> error $ "Failed to install pgmq schema: " <> show poolErr
    Right (Left migrationErr) -> error $ "Migration failed: " <> show migrationErr
    Right (Right ()) -> pure ()

-- | Create a benchmark queue
createBenchQueue :: Pool -> QueueName -> IO ()
createBenchQueue pool qname = do
  result <- runSession pool (Sessions.createQueue qname)
  case result of
    Left err -> error $ "Failed to create queue: " <> show err
    Right () -> pure ()

-- | Drop a benchmark queue
dropBenchQueue :: Pool -> QueueName -> IO ()
dropBenchQueue pool qname = do
  result <- runSession pool (Sessions.dropQueue qname)
  case result of
    Left err -> error $ "Failed to drop queue: " <> show err
    Right _ -> pure ()

-- | Purge all messages from a queue
purgeQueue :: Pool -> QueueName -> IO ()
purgeQueue pool qname = do
  result <- runSession pool (Sessions.deleteAllMessagesFromQueue qname)
  case result of
    Left err -> error $ "Failed to purge queue: " <> show err
    Right _ -> pure ()

-- | Seed a queue with messages
seedQueue :: Pool -> QueueName -> Int -> IO [MessageId]
seedQueue pool qname count = do
  let payloads = generatePayloads count
      msg = BatchSendMessage {queueName = qname, messageBodies = payloads, delay = Nothing}
  result <- runSession pool (Sessions.batchSendMessage msg)
  case result of
    Left err -> error $ "Failed to seed queue: " <> show err
    Right ids -> pure ids

-- | Run an action with an isolated queue, cleaning up afterwards
withQueue :: Pool -> BenchConfig -> Text -> (QueueName -> IO a) -> IO a
withQueue pool config suffix action = do
  qname <- uniqueQueueName suffix
  createBenchQueue pool qname
  result <- action qname
  purgeQueue pool qname
  if config.skipCleanup
    then pure result
    else do
      dropBenchQueue pool qname
      pure result

-- | Generate a queue name with a prefix
benchQueueName :: Text -> Either Text QueueName
benchQueueName suffix =
  case parseQueueName ("bench_" <> suffix) of
    Left err -> Left $ Text.pack (show err)
    Right name -> Right name

-- | Generate a unique queue name with random suffix
uniqueQueueName :: Text -> IO QueueName
uniqueQueueName prefix = do
  suffix <- randomRIO (10000 :: Word32, 99999)
  let name = "bench_" <> prefix <> "_" <> Text.pack (show suffix)
  case parseQueueName name of
    Left err -> error $ "Failed to generate queue name: " <> show err
    Right qname -> pure qname

-- | Generate a single message payload of given size category
generatePayload :: Int -> MessageBody
generatePayload n =
  MessageBody $
    object
      [ "id" .= n,
        "data" .= ("benchmark_message_" <> show n :: String)
      ]

-- | Generate multiple payloads
generatePayloads :: Int -> [MessageBody]
generatePayloads count = map generatePayload [1 .. count]

-- | Generate a payload of approximately the given byte size
generatePayloadOfSize :: Int -> Int -> MessageBody
generatePayloadOfSize idx size =
  MessageBody $
    object
      [ "id" .= idx,
        "padding" .= replicate paddingLength 'x'
      ]
  where
    -- Approximate: {"id":1,"padding":"xxx..."} has ~30 bytes overhead
    paddingLength = max 0 (size - 30)

-- | Small payload (~100 bytes)
smallPayload :: Int -> MessageBody
smallPayload = flip generatePayloadOfSize 100

-- | Medium payload (~10KB)
mediumPayload :: Int -> MessageBody
mediumPayload = flip generatePayloadOfSize 10000

-- | Large payload (~100KB)
largePayload :: Int -> MessageBody
largePayload = flip generatePayloadOfSize 100000

-- | Run effectful action with pool
runEffectful :: Pool -> Eff '[Pgmq, Error PgmqError, IOE] a -> IO a
runEffectful pool action = do
  result <- runEff . runErrorNoCallStack @PgmqError . runPgmq pool $ action
  case result of
    Left err -> error $ "Effectful error: " <> show err
    Right a -> pure a
