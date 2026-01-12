{-# LANGUAGE TemplateHaskellQuotes #-}

module Pgmq.Hasql.Quasi (pgmq) where

import Data.Text qualified as T
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Pgmq.Types (parseQueueName)

pgmq :: QuasiQuoter
pgmq = QuasiQuoter {quoteExp = mkQueueName, quotePat = undefined, quoteType = undefined, quoteDec = undefined}

mkQueueName :: String -> Q Exp
mkQueueName str = case parseQueueName (T.pack str) of
  Right q -> [|q|]
  Left e -> fail $ show e
