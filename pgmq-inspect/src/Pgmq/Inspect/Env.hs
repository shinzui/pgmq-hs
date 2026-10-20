-- | How the surface reaches the database: a runner for the @Pgmq@ effect,
-- supplied by the host, plus the connection settings the WebSocket feed's
-- listener uses.
module Pgmq.Inspect.Env
  ( Inspection,
    InspectEnv (..),
    runnerFromInterpreter,
    poolEnv,
    tracedPoolEnv,
  )
where

import Effectful (Eff, IOE, runEff)
import Effectful.Error.Static (Error, runError)
import Hasql.Connection.Settings (Settings)
import Hasql.Pool (Pool)
import OpenTelemetry.Trace.Core qualified as OTel
import Pgmq.Effectful (Pgmq, PgmqRuntimeError, runPgmq, runPgmqTraced)

-- | A database program the surface wants run: every handler is one of these.
type Inspection a = Eff '[Pgmq, Error PgmqRuntimeError, IOE] a

-- | The host's side of the contract.
data InspectEnv = InspectEnv
  { -- | Run one program. Which interpreter does so is the host's choice
    -- ('poolEnv' for the plain one, 'tracedPoolEnv' for the OpenTelemetry one,
    -- or anything else, including a mock for tests).
    runInspection :: forall a. Inspection a -> IO (Either PgmqRuntimeError a),
    -- | Settings for the dedicated connection the WebSocket feed LISTENs on
    -- (see "Pgmq.Inspect.WebSocket"). 'Nothing' makes the feed poll-only.
    listenerSettings :: !(Maybe Settings)
  }

-- | Turn an interpreter of the @Pgmq@ effect into a runner, discarding the
-- call stack that 'runError' pairs with the error.
runnerFromInterpreter ::
  (forall a. Eff '[Pgmq, Error PgmqRuntimeError, IOE] a -> Eff '[Error PgmqRuntimeError, IOE] a) ->
  (forall a. Inspection a -> IO (Either PgmqRuntimeError a))
runnerFromInterpreter interpretPgmq action =
  either (Left . snd) Right <$> runEff (runError @PgmqRuntimeError (interpretPgmq action))

-- | The plain interpreter over a dedicated inspection pool. The host must set
-- finite PostgreSQL statement_timeout and lock_timeout, plus acquisition/connect
-- budgets. Never borrow worker capacity. The admission cap limits live work,
-- but hasql's blackholed-reply limitation (IR-6) still applies.
poolEnv :: Pool -> Maybe Settings -> InspectEnv
poolEnv pool settings =
  InspectEnv
    { runInspection = runnerFromInterpreter (runPgmq pool),
      listenerSettings = settings
    }

-- | The OpenTelemetry-traced interpreter over a connection pool: every
-- handler's database work becomes a span under the host's tracer.
tracedPoolEnv :: Pool -> OTel.Tracer -> Maybe Settings -> InspectEnv
tracedPoolEnv pool tracer settings =
  InspectEnv
    { runInspection = runnerFromInterpreter (runPgmqTraced pool tracer),
      listenerSettings = settings
    }
