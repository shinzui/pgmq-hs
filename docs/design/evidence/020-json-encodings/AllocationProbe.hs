{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.Aeson
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Aeson.Types (Pair)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Time (UTCTime)
import GHC.Stats
import Pgmq.Types
import System.Environment (getArgs)
import System.Mem (performGC)

fields :: Message -> [Pair]
fields m = ["msg_id" .= messageId m, "read_ct" .= readCount m, "enqueued_at" .= enqueuedAt m, "last_read_at" .= lastReadAt m, "vt" .= visibilityTime m, "message" .= body m, "headers" .= headers m]

viaPairs :: Message -> LBS.ByteString
viaPairs = encodingToLazyByteString . pairs . foldMap (uncurry (.=)) . fields

direct :: Message -> LBS.ByteString
direct m = encodingToLazyByteString $ pairs ("msg_id" .= messageId m <> "read_ct" .= readCount m <> "enqueued_at" .= enqueuedAt m <> "last_read_at" .= lastReadAt m <> "vt" .= visibilityTime m <> "message" .= body m <> "headers" .= headers m)

main :: IO ()
main = do
  [mode, width, count] <- getArgs
  let stamp = read "2026-08-19 12:35:00 UTC" :: UTCTime
      message = Message (MessageId 1) stamp stamp Nothing 0 (MessageBody (object ["payload" .= T.replicate (read width) "x"])) Nothing
      encoder = case mode of "pairs" -> viaPairs; "direct" -> direct; _ -> encode
  _ <- evaluate (LBS.length (encode message))
  performGC
  before <- getRTSStats
  forM_ [1 .. read count] $ \n -> evaluate (LBS.length (encoder (message {messageId = MessageId n})))
  performGC
  after <- getRTSStats
  print (allocated_bytes after - allocated_bytes before)
