{-# LANGUAGE QuasiQuotes #-}

-- | Search path management statements for migrations
module Pgmq.Migration.Statements
  ( getSearchPath,
    setSearchPath,
  )
where

import Data.Text (Text)
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Statement (Statement)
import Hasql.Statement qualified as Statement

-- | Get the current search path
getSearchPath :: Statement () Text
getSearchPath =
  Statement.Statement
    "SHOW search_path"
    Encoders.noParams
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))
    True

-- | Set the search path
setSearchPath :: Statement Text ()
setSearchPath =
  Statement.Statement
    "SELECT set_config('search_path', $1, true)"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    Decoders.noResult
    False
