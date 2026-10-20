-- | Umbrella re-export: everything a host needs to configure, build, and run
-- the inspection surface. See @docs/user/queue-inspection.md@ for the guide.
module Pgmq.Inspect
  ( module Pgmq.Inspect.Config,
    module Pgmq.Inspect.Env,
    module Pgmq.Inspect.Server,
    module Pgmq.Inspect.Error,
    module Pgmq.Inspect.Wire,
    module Pgmq.Inspect.WebSocket,
    routeTable,
    PathSegment (..),
  )
where

import Pgmq.Inspect.Config
import Pgmq.Inspect.Env
import Pgmq.Inspect.Error
import Pgmq.Inspect.Http (PathSegment (..), routeTable)
import Pgmq.Inspect.Server
import Pgmq.Inspect.WebSocket
import Pgmq.Inspect.Wire
