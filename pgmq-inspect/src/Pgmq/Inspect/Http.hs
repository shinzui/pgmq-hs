-- | The HTTP router: one case per route, matching on 'pathInfo' so the
-- application behaves identically at the root and under a host's prefix.
module Pgmq.Inspect.Http
  ( PathSegment (..),
    routeTable,
    httpApp,
  )
where

import Control.Lens ((^.))
import Data.Aeson (ToJSON, encode)
import Data.Generics.Labels ()
import Data.Int (Int32, Int64)
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Version (showVersion)
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Types
  ( Method,
    methodGet,
    mkStatus,
    status200,
    status400,
    status404,
    status405,
    status503,
  )
import Network.Wai (Application, Request, Response, pathInfo, queryString, requestMethod)
import Network.Wai.Handler.WebSockets qualified as WaiWS
import Network.WebSockets qualified as WS
import Paths_pgmq_inspect (version)
import Pgmq.Effectful (allQueueMetrics, listNotifyInsertThrottles, listTopicBindings, testRouting)
import Pgmq.Effectful.Effect (listQueuesUnvalidated, lookupArchivedMessage, lookupMessage, peekArchivedMessages, peekMessages, queueMetricsUnvalidated)
import Pgmq.Hasql.Statements.Types (LookupMessage (..), PeekMessages (..))
import Pgmq.Inspect.Budget
import Pgmq.Inspect.Config (InspectConfig (..))
import Pgmq.Inspect.Env (InspectEnv (..), Inspection)
import Pgmq.Inspect.Error
import Pgmq.Inspect.Wire
import Pgmq.Types (ArchivedMessage (..), MessageId (..), TopicBinding (..), UnvalidatedQueue (..), parseRoutingKey)
import System.Timeout (timeout)

data PathSegment = Literal !Text | Placeholder !Text
  deriving stock (Eq, Show)

-- | Every route the router serves. Plan 31's OpenAPI coverage test reads it.
routeTable :: [(Method, [PathSegment])]
routeTable =
  [ (methodGet, []),
    (methodGet, [Literal "queues"]),
    (methodGet, [Literal "queues", Placeholder "queue"]),
    (methodGet, [Literal "queues", Placeholder "queue", Literal "metrics"]),
    (methodGet, [Literal "queues", Placeholder "queue", Literal "messages"]),
    (methodGet, [Literal "queues", Placeholder "queue", Literal "messages", Placeholder "msg_id"]),
    (methodGet, [Literal "queues", Placeholder "queue", Literal "archive"]),
    (methodGet, [Literal "queues", Placeholder "queue", Literal "archive", Placeholder "msg_id"]),
    (methodGet, [Literal "queues", Placeholder "queue", Literal "bindings"]),
    (methodGet, [Literal "metrics"]),
    (methodGet, [Literal "bindings"]),
    (methodGet, [Literal "routing", Literal "test"]),
    (methodGet, [Literal "notify", Literal "throttles"]),
    (methodGet, [Literal "health", Literal "live"]),
    (methodGet, [Literal "health", Literal "ready"]),
    (methodGet, [Literal "ws"])
  ]

httpApp :: InspectionBudget -> InspectConfig -> InspectEnv -> WS.ServerApp -> Application
httpApp budget cfg env wsApp req respond
  | requestMethod req /= methodGet = respond (errorResponse status405 methodNotAllowed)
  | otherwise = respond =<< admittedRoute
  where
    admittedRoute
      | pathInfo req `elem` [[], ["health", "live"], ["ws"]] = route
      | not (any (matches (pathInfo req) . snd) routeTable) = route
      | Left bad <- validateInput = pure (errorResponse status400 bad)
      | otherwise = maybe busyResponse id <$> withInspectionPermit budget route
    busyResponse
      | pathInfo req == ["health", "ready"] = jsonResponse status503 (encode (ReadinessBody False [DependencyCheck "postgres" False Nothing (Just "Inspection capacity is busy; retry the probe.")]))
      | otherwise = errorResponse status503 inspectionBusy
    validateInput = case pathInfo req of
      ["queues", _, kind] | kind `elem` ["messages", "archive"] -> () <$ parsePage cfg req
      ["queues", _, kind, mid] | kind `elem` ["messages", "archive"] -> () <$ parseMessageId mid
      ["routing", "test"] -> case queryParam "routing_key" req of
        Just raw | Right _ <- parseRoutingKey raw -> Right ()
        raw -> Left (invalidRoutingKey (maybe "" id raw))
      _ -> Right ()
    route = case pathInfo req of
      ["queues", _] -> routeSuccess
      "queues" : q : _ | any (matches (pathInfo req) . snd) routeTable -> queueScoped env q routeSuccess
      _ -> routeSuccess
    routeSuccess = case pathInfo req of
      [] -> pure descriptor
      ["queues"] -> listQueuesRoute env
      ["queues", q] -> queueRoute env q
      ["queues", q, "metrics"] -> queueMetricsRoute env q
      ["queues", q, "messages"] -> messagesPageRoute cfg env q req
      ["queues", q, "messages", mid] -> messageRoute env q mid
      ["queues", q, "archive"] -> archivePageRoute cfg env q req
      ["queues", q, "archive", mid] -> archivedMessageRoute env q mid
      ["queues", q, "bindings"] -> queueBindingsRoute env q
      ["metrics"] -> allMetricsRoute env
      ["bindings"] -> allBindingsRoute env
      ["routing", "test"] -> routingTestRoute env req
      ["notify", "throttles"] -> throttlesRoute env
      ["health", "live"] -> pure (jsonResponse status200 (encode (LivenessBody True)))
      ["health", "ready"] -> readinessRoute cfg env
      ["ws"] -> pure (maybe (errorResponse (mkStatus 426 "Upgrade Required") websocketUpgradeRequired) id (WaiWS.websocketsApp WS.defaultConnectionOptions wsApp req))
      _ -> pure (errorResponse status404 notFound)

descriptor :: Response
descriptor =
  jsonResponse status200 . encode $
    ServiceDescriptor
      { serviceName = "pgmq-inspect",
        serviceVersion = T.pack (showVersion version),
        websocketPath = "ws"
      }

-- | Run a program and encode its result, or map its failure.
runJson :: (ToJSON a) => InspectEnv -> Inspection a -> IO Response
runJson env program = runWith env program (jsonResponse status200 . encode)

runWith :: InspectEnv -> Inspection a -> (a -> Response) -> IO Response
runWith env program onOk =
  either (uncurry errorResponse . classifyRuntimeError) onOk <$> runInspection env program

listQueuesRoute :: InspectEnv -> IO Response
listQueuesRoute env = runJson env listQueuesUnvalidated

queueRoute :: InspectEnv -> Text -> IO Response
queueRoute env name =
  runWith env listQueuesUnvalidated $ \queues ->
    case find ((== name) . unvalidatedName) queues of
      Nothing -> errorResponse status404 (queueNotFound name)
      Just q -> jsonResponse status200 (encode q)

queueMetricsRoute :: InspectEnv -> Text -> IO Response
queueMetricsRoute env name = runScopedWith env name (queueMetricsUnvalidated name) (jsonResponse status200 . encode)

allMetricsRoute :: InspectEnv -> IO Response
allMetricsRoute env = runJson env allQueueMetrics

readinessRoute :: InspectConfig -> InspectEnv -> IO Response
readinessRoute cfg env = do
  started <- getMonotonicTimeNSec
  outcome <- timeout (readinessTimeoutUs cfg) (runInspection env listQueuesUnvalidated)
  finished <- getMonotonicTimeNSec
  let latency = Just (fromIntegral ((finished - started) `div` 1_000_000))
      check = case outcome of
        Nothing -> DependencyCheck "postgres" False latency (Just "readiness probe timed out")
        Just (Left err) -> DependencyCheck "postgres" False latency (Just (fst (describeRuntimeError err)))
        Just (Right _) -> DependencyCheck "postgres" True latency Nothing
      body = ReadinessBody {ready = checkHealthy check, checks = [check]}
  pure (jsonResponse (if ready body then status200 else status503) (encode body))

-- | @from@ (exclusive msg_id cursor, optional) and @limit@ (optional, defaulted,
-- capped). Malformed values are rejected before any database work.
parsePage :: InspectConfig -> Request -> Either ErrorBody (Maybe MessageId, Int32)
parsePage cfg req = do
  from <- case queryParam "from" req of
    Nothing -> Right Nothing
    Just raw -> maybe (Left (invalidCursor raw)) (Right . Just . MessageId) (parseNonNegative raw)
  limit <- case queryParam "limit" req of
    Nothing -> Right (defaultPageLimit cfg)
    Just raw -> case parseDecimalInteger raw of
      Just n | n > 0 -> Right (fromIntegral (min (fromIntegral (maxPageLimit cfg)) n))
      _ -> Left (invalidLimit raw)
  pure (from, limit)

queryParam :: Text -> Request -> Maybe Text
queryParam key req =
  case lookup (TE.encodeUtf8 key) (queryString req) of
    Just (Just raw) -> Just (TE.decodeUtf8Lenient raw)
    Just Nothing -> Just ""
    Nothing -> Nothing

-- Parse arbitrary precision before narrowing; fixed-width decimal silently wraps.
-- Bound token length so adversarial inputs do not build enormous Integers.
parseDecimalInteger :: Text -> Maybe Integer
parseDecimalInteger raw
  | T.null raw || T.length raw > 128 || not (T.all (\c -> c >= '0' && c <= '9') raw) = Nothing
  | otherwise = case TR.decimal raw of
      Right (n, rest) | T.null rest -> Just n
      _ -> Nothing

parseNonNegative :: Text -> Maybe Int64
parseNonNegative raw = do
  n <- parseDecimalInteger raw
  if n <= fromIntegral (maxBound :: Int64) then Just (fromInteger n) else Nothing

parseMessageId :: Text -> Either ErrorBody MessageId
parseMessageId raw = maybe (Left (invalidMessageId raw)) (Right . MessageId) (parseNonNegative raw)

-- | Ask for one row more than the page, return the page, and point at the
-- next one only when that extra row existed.
toPage :: Int32 -> (a -> MessageId) -> Vector a -> Page a
toPage limit cursorOf rows =
  let n = fromIntegral limit
      page = V.toList (V.take n rows)
   in Page {items = page, nextCursor = if V.length rows > n then cursorOf <$> lastOf page else Nothing}
  where
    lastOf [] = Nothing
    lastOf xs = Just (last xs)

messagesPageRoute :: InspectConfig -> InspectEnv -> Text -> Request -> IO Response
messagesPageRoute cfg env name req =
  case parsePage cfg req of
    Left bad -> pure (errorResponse status400 bad)
    Right (from, limit) ->
      runScopedWith env name (peekMessages PeekMessages {unvalidatedQueueName = name, afterMessageId = from, limit = limit + 1}) $
        jsonResponse status200 . encode . toPage limit (^. #messageId)

archivePageRoute :: InspectConfig -> InspectEnv -> Text -> Request -> IO Response
archivePageRoute cfg env name req =
  case parsePage cfg req of
    Left bad -> pure (errorResponse status400 bad)
    Right (from, limit) ->
      runScopedWith env name (peekArchivedMessages PeekMessages {unvalidatedQueueName = name, afterMessageId = from, limit = limit + 1}) $
        jsonResponse status200 . encode . toPage limit (\a -> archivedMessage a ^. #messageId)

messageRoute :: InspectEnv -> Text -> Text -> IO Response
messageRoute env name raw =
  case parseMessageId raw of
    Left bad -> pure (errorResponse status400 bad)
    Right mid ->
      runScopedWith env name (lookupMessage LookupMessage {unvalidatedQueueName = name, messageId = mid}) $
        maybe (errorResponse status404 (messageNotFound name raw)) (jsonResponse status200 . encode)

archivedMessageRoute :: InspectEnv -> Text -> Text -> IO Response
archivedMessageRoute env name raw =
  case parseMessageId raw of
    Left bad -> pure (errorResponse status400 bad)
    Right mid ->
      runScopedWith env name (lookupArchivedMessage LookupMessage {unvalidatedQueueName = name, messageId = mid}) $
        maybe (errorResponse status404 (messageNotFound name raw)) (jsonResponse status200 . encode)

queueBindingsRoute :: InspectEnv -> Text -> IO Response
queueBindingsRoute env name =
  runScopedWith env name listTopicBindings $ \bindings ->
    jsonResponse status200 (encode (filter ((== name) . bindingQueueName) bindings))

allBindingsRoute :: InspectEnv -> IO Response
allBindingsRoute env = runJson env listTopicBindings

routingTestRoute :: InspectEnv -> Request -> IO Response
routingTestRoute env req =
  case queryParam "routing_key" req of
    Nothing -> pure (errorResponse status400 (invalidRoutingKey ""))
    Just raw -> case parseRoutingKey raw of
      Left _ -> pure (errorResponse status400 (invalidRoutingKey raw))
      Right key -> runJson env (testRouting key)

throttlesRoute :: InspectEnv -> IO Response
throttlesRoute env = runJson env listNotifyInsertThrottles

-- Only metadata-confirmed absence is a queue 404. Global schema failures stay 500.
queueScoped :: InspectEnv -> Text -> IO Response -> IO Response
queueScoped env name action = do
  listed <- runInspection env listQueuesUnvalidated
  case listed of
    Left err -> pure (uncurry errorResponse (classifyRuntimeError err))
    Right queues | not (any ((== name) . unvalidatedName) queues) -> pure (errorResponse status404 (queueNotFound name))
    Right _ -> action

matches :: [Text] -> [PathSegment] -> Bool
matches [] [] = True
matches (x : xs) (Literal y : ys) = x == y && matches xs ys
matches (_ : xs) (Placeholder _ : ys) = matches xs ys
matches _ _ = False

runScopedWith :: InspectEnv -> Text -> Inspection a -> (a -> Response) -> IO Response
runScopedWith env name program onOk = do
  result <- runInspection env program
  case result of
    Left err | sqlState err == Just "42P01" -> do
      queues <- runInspection env listQueuesUnvalidated
      pure $ case queues of
        Right qs | not (any ((== name) . unvalidatedName) qs) -> errorResponse status404 (queueNotFound name)
        _ -> uncurry errorResponse (classifyRuntimeError err)
    _ -> pure (either (uncurry errorResponse . classifyRuntimeError) onOk result)
