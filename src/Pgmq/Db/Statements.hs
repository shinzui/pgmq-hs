module Pgmq.Db.Statements (createQueue, dropQueue, readMessage) where

import Pgmq.Db.Statements.Message (readMessage)
import Pgmq.Db.Statements.QueueManagement (createQueue, dropQueue)
