module Pgmq.Db.Sessions (readMessage) where

import Hasql.Session (Session, statement)
import Pgmq.Db.Statements qualified as Db
import Pgmq.Db.Statements.Types (ReadMessage)
import Pgmq.Prelude
import Pgmq.Types (Message)

readMessage :: ReadMessage -> Session (Vector Message)
readMessage query = statement query Db.readMessage
