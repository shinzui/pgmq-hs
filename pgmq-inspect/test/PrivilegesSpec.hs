module PrivilegesSpec (tests) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Aeson (Value (..), eitherDecode)
import Data.Text (Text)
import Data.Text.IO qualified as T
import EphemeralDb
import EphemeralPg qualified as Pg
import Hasql.Connection qualified as Connection
import Hasql.Connection.Settings qualified as Settings
import Hasql.Decoders qualified as D
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as PC
import Hasql.Session qualified as Session
import Hasql.Statement (unpreparable)
import HttpSpec (session)
import Network.WebSockets qualified as WS
import Pgmq.Effectful (PgmqRuntimeError (..))
import Pgmq.Inspect.Config
import Pgmq.Inspect.Env
import Pgmq.Inspect.Error (sqlState)
import RouterSpec (lookupKey)
import Test.Tasty
import Test.Tasty.HUnit
import TestServer

tests :: TestTree
tests =
  testGroup
    "Inspection role privileges"
    [ testCase "all reads and live snapshot work; consumption/mutation denied; future queues inherit SELECT" $ do
        result <- withPgmqDb $ \owner db -> do
          session owner (Session.script "SELECT pgmq.create('role_current'); SELECT pgmq.send('role_current','{}'); SELECT pgmq.send('role_current','{}'); SELECT pgmq.archive('role_current',1); SELECT pgmq.bind_topic('role.*','role_current'); SELECT pgmq.enable_notify_insert('role_current',250)")
          sql <- T.readFile "test/fixtures/inspection-role.sql"
          session owner (Session.script sql)
          session owner (Session.script "SELECT pgmq.create('role_future'); SELECT pgmq.send('role_future','{}')")
          let settings = Pg.connectionSettings db <> Settings.user "pgmq_inspector"
              poolConfig = PC.settings [PC.size 2, PC.acquisitionTimeout 1, PC.staticConnectionSettings settings]
          bracket (Pool.acquire poolConfig) Pool.release $ \pool ->
            withTestServer defaultInspectConfig (poolEnv pool (Just settings)) $ \port -> do
              forM_ ["/", "/openapi.json", "/queues", "/queues/role_current", "/queues/role_current/metrics", "/queues/role_current/messages", "/queues/role_current/messages/2", "/queues/role_current/archive", "/queues/role_current/archive/1", "/queues/role_current/bindings", "/metrics", "/bindings", "/routing/test?routing_key=role.created", "/notify/throttles", "/health/live", "/health/ready", "/queues/role_future/messages", "/queues/role_future/metrics"] $ \path -> do
                r <- get port path
                assertEqual (path <> " failed: " <> show (replyBody r)) 200 (replyStatus r)
              WS.runClient "127.0.0.1" port "/ws" $ \conn -> do
                WS.sendTextData conn ("{\"type\":\"subscribe\",\"queue\":\"role_current\"}" :: Text)
                bytes <- WS.receiveData conn
                row <- either assertFailure pure (eitherDecode bytes :: Either String Value)
                lookupKey "type" row @?= Just (String "snapshot")
          bracket (Connection.acquire settings >>= either (assertFailure . show) pure) Connection.release $ \conn -> do
            effective <- Connection.use conn (Session.statement () (unpreparable "SELECT current_setting('default_transaction_read_only'), current_setting('statement_timeout'), current_setting('lock_timeout')" mempty (D.singleRow ((,,) <$> D.column (D.nonNullable D.text) <*> D.column (D.nonNullable D.text) <*> D.column (D.nonNullable D.text))))) >>= either (assertFailure . show) pure
            effective @?= ("on", "2s", "250ms")
            -- Turn off the default transaction safeguard to prove the ACL itself
            -- denies mutation, including sequence advancement.
            Connection.use conn (Session.script "SET default_transaction_read_only=off") >>= either (assertFailure . show) pure
            forM_ ["SELECT pgmq.read('role_current',0,1)", "SELECT pgmq.pop('role_current')", "SELECT pgmq.send('role_current','{}')", "SELECT pgmq.archive('role_current',2)", "SELECT pgmq.delete('role_current',2)", "SELECT pgmq.purge_queue('role_current')", "SELECT pgmq.drop_queue('role_current')", "SELECT pgmq.create('role_forbidden')", "UPDATE pgmq.q_role_current SET read_ct=read_ct+1", "SELECT nextval('pgmq.q_role_current_msg_id_seq')"] $ \sql' -> do
              answer <- Connection.use conn (Session.script sql')
              case answer of
                Left err -> assertEqual ("wrong failure for " <> show sql') (Just "42501") (sqlState (PgmqSessionError err))
                Right () -> assertFailure ("write unexpectedly permitted: " <> show sql')
        either (assertFailure . show) pure result
    ]
