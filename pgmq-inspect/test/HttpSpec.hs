module HttpSpec (tests, withQueue, sendN, session) where

import Control.Exception (bracket)
import Control.Lens ((^.))
import Control.Monad (void)
import Data.Aeson
import Data.Generics.Labels ()
import Data.Int (Int32, Int64)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Vector qualified as V
import EphemeralDb
import EphemeralPg qualified as Pg
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as D
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as PC
import Hasql.Session qualified as Session
import Hasql.Statement (unpreparable)
import Pgmq.Effectful qualified
import Pgmq.Hasql.Sessions qualified as S
import Pgmq.Hasql.Statements.Types qualified as H
import Pgmq.Inspect.Config
import Pgmq.Inspect.Env
import Pgmq.Types
import RouterSpec (errorCodeOf, lookupKey)
import System.Random (randomRIO)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import TestServer

session :: Pool.Pool -> Session.Session a -> IO a
session pool action = Pool.use pool action >>= either (assertFailure . show) pure

withQueue :: Pool.Pool -> (QueueName -> IO a) -> IO a
withQueue pool k = do
  n <- randomRIO (1, 1000000000 :: Int)
  q <- either (error . show) pure (parseQueueName ("inspect_" <> T.pack (show n)))
  bracket (q <$ session pool (S.createQueue q)) (\name -> void (session pool (S.dropQueue name))) k

sendN :: Pool.Pool -> QueueName -> Int -> IO [MessageId]
sendN pool q n = session pool (S.batchSendMessage (H.BatchSendMessage q [MessageBody (object ["n" .= i]) | i <- [1 .. n]] Nothing))

baseFor :: QueueName -> String
baseFor q = "/queues/" <> T.unpack (queueNameToText q)

data PageOf a = PageOf [a] (Maybe MessageId)

instance (FromJSON a) => FromJSON (PageOf a) where
  parseJSON = withObject "PageOf" $ \o -> PageOf <$> o .: "items" <*> o .:? "next_cursor"

images :: QueueName -> Session.Session [(Int64, Int32, Maybe UTCTime, UTCTime)]
images q =
  Session.statement
    ()
    ( unpreparable
        ("select msg_id, read_ct, last_read_at, vt from pgmq.q_" <> queueNameToText q <> " order by msg_id")
        mempty
        (D.rowList ((,,,) <$> D.column (D.nonNullable D.int8) <*> D.column (D.nonNullable D.int4) <*> D.column (D.nullable D.timestamptz) <*> D.column (D.nonNullable D.timestamptz)))
    )

tests :: IO (Pool.Pool, Pg.Database) -> TestTree
tests database =
  testGroup
    "HTTP database integration"
    [ testCase "lock deadline bounds inspection while a consumer uses worker capacity" $ do
        result <- withPgmqDb $ \workers database' -> withQueue workers $ \q -> do
          void (sendN workers q 1)
          bracket (Connection.acquire (Pg.connectionSettings database') >>= either (assertFailure . show) pure) Connection.release $ \locker -> do
            Connection.use locker (Session.script "BEGIN; LOCK TABLE pgmq.meta IN ACCESS EXCLUSIVE MODE") >>= either (assertFailure . show) pure
            bracket (Pool.acquire (PC.settings [PC.size 1, PC.acquisitionTimeout 1, PC.staticConnectionSettings (Pg.connectionSettings database'), PC.initSession (Session.script "SET statement_timeout = '1s'; SET lock_timeout = '50ms'")])) Pool.release $ \inspectionPool ->
              withTestServer defaultInspectConfig (poolEnv inspectionPool Nothing) $ \port -> do
                outcome <- timeout 2000000 (get port "/queues")
                case outcome of
                  Nothing -> assertFailure "lock deadline failed"
                  Just r -> do
                    replyStatus r @?= 503
                    errorCodeOf r >>= (@?= String "database_unavailable")
                consumed <- timeout 500000 (session workers (S.readMessage (H.ReadMessage q 0 (Just 1) Nothing)))
                fmap V.length consumed @?= Just 1
        either (assertFailure . show) pure result,
      testCase "browsing over HTTP leaves read_ct at zero and every message available" $ do
        (pool, _) <- database
        withQueue pool $ \q -> do
          ids <- sendN pool q 5
          before <- session pool (images q)
          withTestServer defaultInspectConfig (poolEnv pool Nothing) $ \port -> do
            let base = baseFor q
            PageOf first cursor <- decodeReply =<< get port (base <> "/messages?limit=3") :: IO (PageOf Message)
            length first @?= 3
            cursor @?= Just (ids !! 2)
            PageOf second lastCursor <- decodeReply =<< get port (base <> "/messages?limit=3&from=" <> show (unMessageId (ids !! 2))) :: IO (PageOf Message)
            length second @?= 2
            lastCursor @?= Nothing
            r <- get port (base <> "/messages/" <> show (unMessageId (ids !! 0)))
            replyStatus r @?= 200
          after <- session pool (images q)
          before @?= after
          consumed <- session pool (S.readMessage (H.ReadMessage q 0 (Just 10) Nothing))
          V.length consumed @?= 5
          V.toList (V.map (^. #readCount) consumed) @?= [1, 1, 1, 1, 1],
      testCase "GET /queues lists the created queue and lookup returns it" $ do
        (pool, _) <- database
        withQueue pool $ \q -> withTestServer defaultInspectConfig (poolEnv pool Nothing) $ \port -> do
          rows <- decodeReply =<< get port "/queues" :: IO [Value]
          assertBool "listed" (Just (String (queueNameToText q)) `elem` map (lookupKey "queue_name") rows)
          row <- decodeReply =<< get port (baseFor q) :: IO Value
          lookupKey "queue_name" row @?= Just (String (queueNameToText q)),
      testCase "metrics distinguish total and visible depth after a lease" $ do
        (_, _) <- database
        withPgmqPool
          ( \p -> withQueue p $ \q -> do
              void (sendN p q 3)
              withTestServer defaultInspectConfig (poolEnv p Nothing) $ \port -> do
                initial <- decodeReply =<< get port (baseFor q <> "/metrics") :: IO Value
                lookupKey "queue_length" initial @?= Just (Number 3)
                lookupKey "queue_visible_length" initial @?= Just (Number 3)
                void (session p (S.readMessage (H.ReadMessage q 30 (Just 1) Nothing)))
                leased <- decodeReply =<< get port (baseFor q <> "/metrics") :: IO Value
                lookupKey "queue_length" leased @?= Just (Number 3)
                lookupKey "queue_visible_length" leased @?= Just (Number 2)
          )
          >>= either (assertFailure . show) pure,
      testCase "GET /metrics lists both queues on an isolated database" $ do
        (_, _) <- database
        withPgmqPool
          ( \p -> withQueue p $ \q -> withQueue p $ \q2 -> withTestServer defaultInspectConfig (poolEnv p Nothing) $ \port -> do
              rows <- decodeReply =<< get port "/metrics" :: IO [Value]
              let names = map (lookupKey "queue_name") rows
              assertBool "both queues" (all (\name -> Just (String (queueNameToText name)) `elem` names) [q, q2])
          )
          >>= either (assertFailure . show) pure,
      testCase "archive paging and by-id lookup retain archived_at" $ do
        (pool, _) <- database
        withQueue pool $ \q -> do
          ids <- sendN pool q 2
          mapM_ (\mid -> session pool (S.archiveMessage (H.MessageQuery q mid))) ids
          withTestServer defaultInspectConfig (poolEnv pool Nothing) $ \port -> do
            PageOf first cursor <- decodeReply =<< get port (baseFor q <> "/archive?limit=1") :: IO (PageOf ArchivedMessage)
            length first @?= 1
            cursor @?= Just (ids !! 0)
            PageOf lastPage next <- decodeReply =<< get port (baseFor q <> "/archive?limit=1&from=" <> show (unMessageId (ids !! 0))) :: IO (PageOf ArchivedMessage)
            length lastPage @?= 1
            next @?= Nothing
            row <- decodeReply =<< get port (baseFor q <> "/archive/" <> show (unMessageId (ids !! 0))) :: IO Value
            assertBool "has archived_at" (lookupKey "archived_at" row /= Nothing),
      testCase "valid missing IDs are message_not_found in either table" $ do
        (pool, _) <- database
        withQueue pool $ \q -> withTestServer defaultInspectConfig (poolEnv pool Nothing) $ \port ->
          mapM_
            ( \kind -> do
                r <- get port (baseFor q <> "/" <> kind <> "/9223372036854775807")
                replyStatus r @?= 404
                errorCodeOf r >>= (@?= String "message_not_found")
            )
            ["messages", "archive"],
      testCase "oversized page limit is capped with a lookahead cursor" $ do
        (pool, _) <- database
        withQueue pool $ \q -> do
          void (sendN pool q 4)
          withTestServer defaultInspectConfig {defaultPageLimit = 2, maxPageLimit = 3} (poolEnv pool Nothing) $ \port -> do
            PageOf rows cursor <- decodeReply =<< get port (baseFor q <> "/messages?limit=10000") :: IO (PageOf Message)
            length rows @?= 3
            assertBool "more" (cursor /= Nothing),
      testCase "bindings, routing and throttle routes use domain encodings" $ do
        (pool, _) <- database
        withQueue pool $ \q -> do
          pattern' <- either (error . show) pure (parseTopicPattern "orders.*")
          void (session pool (S.bindTopic (H.BindTopic pattern' q)))
          session pool (S.enableNotifyInsert (H.EnableNotifyInsert q (Just 250)))
          withTestServer defaultInspectConfig (poolEnv pool Nothing) $ \port -> do
            rows <- decodeReply =<< get port (baseFor q <> "/bindings") :: IO [Value]
            length rows @?= 1
            allRows <- decodeReply =<< get port "/bindings" :: IO [Value]
            assertBool "global binding" (all (`elem` allRows) rows)
            matches <- decodeReply =<< get port "/routing/test?routing_key=orders.created" :: IO [Value]
            assertBool "route match" (Just (String (queueNameToText q)) `elem` map (lookupKey "queue_name") matches)
            bad <- get port "/routing/test?routing_key=bad%20key"
            replyStatus bad @?= 400
            throttles <- decodeReply =<< get port "/notify/throttles" :: IO [Value]
            assertBool "250ms row" (any (\r -> lookupKey "queue_name" r == Just (String (queueNameToText q)) && lookupKey "throttle_interval_ms" r == Just (Number 250)) throttles),
      testCase "readiness includes a healthy postgres check and numeric latency" $ do
        (pool, _) <- database
        withTestServer defaultInspectConfig (poolEnv pool Nothing) $ \port -> do
          r <- get port "/health/ready"
          replyStatus r @?= 200
          row <- decodeReply r :: IO Value
          lookupKey "ready" row @?= Just (Bool True),
      testCase "mixed-case metadata names remain inspectable and encoded paths resolve" $ do
        (_, _) <- database
        withPgmqPool
          ( \p -> do
              session p (Session.script "select pgmq.create('MixedCase_q')")
              withTestServer defaultInspectConfig (poolEnv p Nothing) $ \port -> do
                mapM_ (\path -> get port path >>= (\r -> replyStatus r @?= 200)) ["/queues/MixedCase_q", "/queues/MixedCase_q/metrics", "/queues/MixedCase_q/messages", "/queues/%4dixedCase_q/archive"]
          )
          >>= either (assertFailure . show) pure,
      testCase "row cap does not cap payload bytes" $ do
        (pool, _) <- database
        withQueue pool $ \q -> do
          void (session pool (S.sendMessage (H.SendMessage q (MessageBody (String (T.replicate (1024 * 1024) "x"))) Nothing)))
          withTestServer defaultInspectConfig (poolEnv pool Nothing) $ \port -> do
            r <- get port (baseFor q <> "/messages?limit=1")
            replyStatus r @?= 200
            PageOf rows _ <- decodeReply r :: IO (PageOf Message)
            length rows @?= 1,
      testCase "dedicated inspection statement deadline preserves worker pool progress" $ do
        (pool, db) <- database
        bracket (Pool.acquire (PC.settings [PC.size 1, PC.acquisitionTimeout 1, PC.staticConnectionSettings (Pg.connectionSettings db), PC.initSession (Session.script "SET statement_timeout = '100ms'; SET lock_timeout = '50ms'")])) Pool.release $ \inspectionPool -> do
          -- The slow host runner uses only inspection capacity, never worker connections.
          let env =
                InspectEnv
                  ( \_ -> do
                      result <- Pool.use inspectionPool (Session.script "select pg_sleep(10)")
                      pure (either (Left . Pgmq.Effectful.fromUsageError) (const (error "slow query unexpectedly succeeded")) result)
                  )
                  Nothing
          withTestServer defaultInspectConfig env $ \port -> do
            r <- timeout 2000000 (get port "/queues")
            case r of
              Nothing -> assertFailure "statement timeout failed"
              Just reply -> errorCodeOf reply >>= (@?= String "inspection_timeout")
            worker <- timeout 500000 (session pool S.listQueuesUnvalidated)
            assertBool "worker pool progresses" (maybe False (const True) worker)
    ]
