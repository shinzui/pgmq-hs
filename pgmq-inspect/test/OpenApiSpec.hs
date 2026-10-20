module OpenApiSpec (tests, databaseTests) where

import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.MVar
import Control.Monad (forM_)
import Data.Aeson (Value (..), eitherDecode, object, (.=))
import Data.ByteString.Char8 qualified as BS
import Data.HashMap.Strict.InsOrd.Compat qualified as O
import Data.List (intercalate, sort)
import Data.OpenApi (Components (..), MediaTypeObject (..), OpenApi (..), OpenApiItems (..), OpenApiType (..), Operation (..), PathItem (..), Reference (..), Referenced (..), Response (..), Responses (..), Schema (..))
import Data.OpenApi.Schema.Validation (validateJSON)
import Data.Text qualified as T
import EmbeddingExample (mountUnder)
import EphemeralDb (withPgmqPool)
import EphemeralPg qualified as Pg
import ErrorSpec (failingEnv, serverError)
import Hasql.Pool qualified as Pool
import Hasql.Session qualified as Session
import HttpSpec (session, withQueue)
import Network.Wai.Handler.Warp qualified as Warp
import Network.WebSockets qualified as WS
import Pgmq.Inspect.Config
import Pgmq.Inspect.Env
import Pgmq.Inspect.Http
import Pgmq.Inspect.OpenApi
import Pgmq.Inspect.Server (withInspectApplication)
import Pgmq.Types (queueNameToText)
import RouterSpec (mockEnv)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.Golden (NoCreateFile (..), goldenVsString)
import Test.Tasty.HUnit
import TestServer

coverage :: [(String, String)] -> Bool
coverage served = sort served == sort documentedRoutes

servedRoutes :: [(String, String)]
servedRoutes = [(BS.unpack method, "/" <> intercalate "/" (map segment segments)) | (method, segments) <- routeTable, segments /= [Literal "ws"]]
  where
    segment (Literal t) = T.unpack t
    segment (Placeholder t) = "{" <> T.unpack t <> "}"

documentedRoutes :: [(String, String)]
documentedRoutes =
  [ (method, path)
  | (path, item) <- O.toList (_openApiPaths openApiDocument),
    (method, op) <- [("GET", _pathItemGet item), ("POST", _pathItemPost item), ("PUT", _pathItemPut item), ("DELETE", _pathItemDelete item), ("HEAD", _pathItemHead item), ("OPTIONS", _pathItemOptions item), ("PATCH", _pathItemPatch item), ("TRACE", _pathItemTrace item)],
    Just _ <- [op]
  ]

-- Resolve the actual operation/status/media schema, rather than independently
-- choosing a domain schema from a fixture's filename.
-- openapi3 3.2.5's validator ignores the OAS 3 nullable keyword (verified in
-- Internal.Schema.Validation). Translate it ONLY in the validation view to its
-- supported union/null representation. The served schema remains valid OAS 3.0.
validationSchema :: Schema -> Schema
validationSchema schema =
  let ref (Inline s) = Inline (validationSchema s)
      ref r = r
      nested =
        schema
          { _schemaProperties = fmap ref (_schemaProperties schema),
            _schemaItems = fmap (\case OpenApiItemsObject r -> OpenApiItemsObject (ref r); OpenApiItemsArray rs -> OpenApiItemsArray (map ref rs)) (_schemaItems schema),
            _schemaNullable = Nothing
          }
   in if _schemaNullable schema == Just True
        then mempty {_schemaOneOf = Just [Inline nested, Inline mempty {_schemaType = Just OpenApiNull}]}
        else nested

validateReply :: String -> Reply -> IO ()
validateReply path reply = do
  value <- decodeReply reply
  let schemas = fmap validationSchema (_componentsSchemas (_openApiComponents openApiDocument))
      resolve (Inline s) = Just s
      resolve (Ref (Reference name)) = O.lookup name schemas
      schema = do
        item <- O.lookup path (_openApiPaths openApiDocument)
        op <- _pathItemGet item
        responseRef <- O.lookup (replyStatus reply) (_responsesResponses (_operationResponses op))
        response <- case responseRef of Inline r -> Just r; _ -> Nothing
        media <- O.lookup "application/json" (_responseContent response)
        _mediaTypeObjectSchema media >>= resolve
  s <- maybe (assertFailure ("no schema for " <> path <> " " <> show (replyStatus reply))) pure schema
  assertEqual (path <> " " <> show (replyStatus reply)) [] (validateJSON schemas (validationSchema s) value)

tests :: TestTree
tests =
  testGroup
    "OpenAPI contract"
    [ localOption (NoCreateFile True) $ goldenVsString "document golden" "test/golden/openapi.json" (pure openApiJson),
      testCase "HTTP paths and methods covered; fake route proves drift detection" $ do
        assertEqual "route coverage" (sort documentedRoutes) (sort servedRoutes)
        assertBool "extra route rejected" (not (coverage (("GET", "/forgotten") : servedRoutes)))
        assertBool "extra method rejected" (not (coverage (("POST", "/queues") : servedRoutes)))
        forM_ (O.elems (_openApiPaths openApiDocument)) $ \item -> do
          assertBool "GET exists" (maybe False (const True) (_pathItemGet item))
          assertBool "POST absent" (maybe True (const False) (_pathItemPost item)),
      testCase "served document round-trips and descriptor advertises it" $
        withTestServer defaultInspectConfig mockEnv $ \port -> do
          r <- get port "/openapi.json"
          replyBody r @?= openApiJson
          case eitherDecode (replyBody r) :: Either String OpenApi of
            Left e -> assertFailure e
            Right parsed -> parsed @?= openApiDocument
          get port "/" >>= validateReply "/",
      testCase "actual failures validate by status, including readiness 503" $ do
        withTestServer defaultInspectConfig mockEnv $ \port -> do
          forM_ [("/queues/{queue}", "/queues/absent"), ("/queues/{queue}/messages/{msg_id}", "/queues/orders/messages/0"), ("/queues/{queue}/messages", "/queues/orders/messages?from=bad"), ("/routing/test", "/routing/test?routing_key=bad%20key")] $ \(template, path) -> get port path >>= validateReply template
          getWith port "POST" [] "/queues" >>= validateReply "/queues"
        forM_ ["57014", "42P01", "55P03"] $ \code ->
          withTestServer defaultInspectConfig (failingEnv (serverError code)) $ \port -> do
            get port "/queues" >>= validateReply "/queues"
            get port "/health/ready" >>= validateReply "/health/ready",
      testCase "saturated readiness has nullable latency; document bypasses database admission" $ do
        started <- newEmptyMVar
        release <- newEmptyMVar
        let env = InspectEnv (\program -> putMVar started () >> takeMVar release >> runInspection mockEnv program) Nothing
        withTestServer defaultInspectConfig {maxConcurrentReads = 1} env $ \port ->
          withAsync (get port "/queues") $ \pending -> do
            takeMVar started
            ready <- get port "/health/ready"
            replyStatus ready @?= 503
            validateReply "/health/ready" ready
            document <- get port "/openapi.json"
            replyStatus document @?= 200
            putMVar release ()
            _ <- wait pending
            pure (),
      testCase "validator detects incompatible bodies and required keys" $ do
        let schemas = fmap validationSchema (_componentsSchemas (_openApiComponents openApiDocument))
        schema <- maybe (assertFailure "Message schema missing") pure (O.lookup "Message" schemas)
        let integer = mempty {_schemaType = Just OpenApiInteger}
        assertBool "non-null rejects null" (not (null (validateJSON mempty (validationSchema integer) Null)))
        validateJSON mempty (validationSchema integer {_schemaNullable = Just True}) Null @?= []
        assertBool "nullable still rejects text" (not (null (validateJSON mempty (validationSchema integer {_schemaNullable = Just True}) (String "wrong"))))
        assertBool "required fields enforced" (not (null (validateJSON schemas schema (String "wrong"))))
    ]

databaseTests :: IO (Pool.Pool, Pg.Database) -> TestTree
databaseTests database =
  testGroup
    "OpenAPI real responses and client"
    [ testCase "nonempty domain responses, arbitrary headers and both page forms validate" $ do
        result <- withPgmqPool $ \pool -> withQueue pool $ \q -> do
          let name = queueNameToText q
              base = "/queues/" <> T.unpack name
          session pool (Session.script ("INSERT INTO pgmq.q_" <> name <> " (msg_id,vt,message,headers) OVERRIDING SYSTEM VALUE VALUES (0, now(), 'true', NULL), (1, now(), '[1]', '{}'), (2, now(), '\"body\"', 'true'), (9007199254740993, now(), 'null', '[1,true]'), (9223372036854775807, now(), '42', '\"scalar\"'); SELECT pgmq.bind_topic('inspect.*', '" <> name <> "'); SELECT pgmq.enable_notify_insert('" <> name <> "',250)"))
          withTestServer defaultInspectConfig (poolEnv pool Nothing) $ \port -> do
            forM_ [("/", "/"), ("/queues", "/queues"), ("/queues/{queue}", base), ("/queues/{queue}/metrics", base <> "/metrics"), ("/metrics", "/metrics"), ("/queues/{queue}/messages", base <> "/messages?limit=1"), ("/queues/{queue}/messages", base <> "/messages?limit=500"), ("/queues/{queue}/messages", base <> "/messages?from=9007199254740993"), ("/queues/{queue}/messages/{msg_id}", base <> "/messages/9223372036854775807"), ("/queues/{queue}/bindings", base <> "/bindings"), ("/bindings", "/bindings"), ("/routing/test", "/routing/test?routing_key=inspect.created"), ("/notify/throttles", "/notify/throttles"), ("/health/live", "/health/live"), ("/health/ready", "/health/ready"), ("/openapi.json", "/openapi.json")] $ \(template, path) -> do
              r <- get port path
              assertEqual (path <> " " <> show (replyBody r)) 200 (replyStatus r)
              validateReply template r
            session pool (Session.script ("SELECT pgmq.archive('" <> name <> "',9007199254740993); SELECT pgmq.archive('" <> name <> "',9223372036854775807)"))
            forM_ [("/queues/{queue}/archive", base <> "/archive?limit=1"), ("/queues/{queue}/archive", base <> "/archive?from=9007199254740993"), ("/queues/{queue}/archive/{msg_id}", base <> "/archive/9223372036854775807")] $ \(template, path) -> get port path >>= validateReply template
        either (assertFailure . show) pure result,
      testCase "client generated from served operations preserves prefixes, encoded names and int64 tokens" $ do
        (pool, _) <- database
        withQueue pool $ \q -> do
          -- A space is accepted by upstream and exercises percent encoding; use
          -- an isolated table/name derived from this test's random queue.
          let name = "Foreign " <> queueNameToText q
          session pool (Session.script ("SELECT pgmq.create('" <> name <> "'); INSERT INTO pgmq.\"q_" <> T.toLower name <> "\" (msg_id,vt,message) OVERRIDING SYSTEM VALUE VALUES (9007199254740993, now(), '{}'),(9223372036854775807, now(), '{}')"))
          withInspectApplication defaultInspectConfig (poolEnv pool Nothing) $ \app ->
            forM_ ["", "/pgmq"] $ \prefix ->
              Warp.testWithApplication (pure (mountUnder (if null prefix then [] else ["pgmq"]) app)) $ \port -> do
                (code, out, err) <- readProcessWithExitCode "node" ["test/client/inspection-client.mjs", "http://127.0.0.1:" <> show port <> prefix <> "/", T.unpack name] ""
                assertEqual (out <> err) ExitSuccess code
                WS.runClient "127.0.0.1" port (prefix <> "/ws") $ \connection -> do
                  WS.sendTextData connection ("{\"type\":\"ping\"}" :: T.Text)
                  pong <- WS.receiveData connection
                  (eitherDecode pong :: Either String Value) @?= Right (object ["type" .= ("pong" :: T.Text)])
          session pool (Session.script ("SELECT pgmq.drop_queue('" <> name <> "')"))
    ]
