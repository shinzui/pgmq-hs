-- | Send raw client frames so size limits are tested before a body is provided,
-- and across fragments that individually fit but collectively exceed the cap.
module RawWebSocketSpec (tests) where

import Control.Exception (bracket)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketBS
import Pgmq.Inspect.Config
import RouterSpec (mockEnv)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import TestServer

withRaw :: Int -> (Socket.Socket -> IO a) -> IO a
withRaw port k = bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close $ \socket -> do
  Socket.connect socket (Socket.SockAddrInet (fromIntegral port) (Socket.tupleToHostAddress (127, 0, 0, 1)))
  SocketBS.sendAll socket "GET /ws HTTP/1.1\r\nHost: localhost\r\nConnection: Upgrade\r\nUpgrade: websocket\r\nSec-WebSocket-Version: 13\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\r\n"
  header <- timeout 2000000 (untilBytes socket "\r\n\r\n") >>= maybe (assertFailure "handshake timed out") pure
  assertBool "upgraded" ("101" `BS.isInfixOf` header)
  k socket

untilBytes :: Socket.Socket -> BS.ByteString -> IO BS.ByteString
untilBytes socket needle = go BS.empty
  where
    go accumulated
      | needle `BS.isInfixOf` accumulated = pure accumulated
      | otherwise = do
          chunk <- SocketBS.recv socket 4096
          if BS.null chunk then assertFailure ("socket closed before " <> B8.unpack needle) else go (accumulated <> chunk)

tests :: TestTree
tests =
  testGroup
    "WebSocket raw size limits"
    [ testCase "huge declared frame is rejected without sending its payload" $
        withTestRuntime config mockEnv $ \port _ -> withRaw port $ \socket -> do
          -- Masked text, uint64 length = 1 GiB, followed by the four-byte mask.
          SocketBS.sendAll socket (BS.pack [0x81, 0xff, 0, 0, 0, 0, 0x40, 0, 0, 0, 0, 0, 0, 0])
          result <- timeout 2000000 (untilBytes socket "frame_too_large")
          assertBool "header-only size rejection" (maybe False (const True) result),
      testCase "fragmented message enforces assembled cap before unbounded accumulation" $
        withTestRuntime config mockEnv $ \port _ -> withRaw port $ \socket -> do
          let payload = BS.replicate 40 120
          SocketBS.sendAll socket (BS.pack [0x01, 0xa8, 0, 0, 0, 0] <> payload)
          SocketBS.sendAll socket (BS.pack [0x80, 0xa8, 0, 0, 0, 0] <> payload)
          result <- timeout 2000000 (untilBytes socket "frame_too_large")
          assertBool "assembled size rejection" (maybe False (const True) result)
    ]
  where
    config = defaultInspectConfig {wsMaxFrameBytes = 64}
