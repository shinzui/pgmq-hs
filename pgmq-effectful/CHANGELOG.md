# Revision history for pgmq-effectful

## 0.2.0.0 -- 2026-04-23

### Breaking Changes

* Renamed the interpreter error type from `PgmqError` to
  `PgmqRuntimeError` and replaced its opaque `PgmqPoolError UsageError`
  constructor with three structured constructors:

      data PgmqRuntimeError
        = PgmqAcquisitionTimeout
        | PgmqConnectionError Hasql.Errors.ConnectionError
        | PgmqSessionError Hasql.Errors.SessionError

  The old `PgmqError`/`PgmqPoolError` names are retained with a
  DEPRECATED pragma and will be removed in 0.3.0.0.

* `runPgmq`'s error constraint changed from `Error PgmqError :> es` to
  `Error PgmqRuntimeError :> es`. Update any
  `runError @PgmqError` annotation to `runError @PgmqRuntimeError`.

* `runPgmqTraced` and `runPgmqTracedWith` now require
  `Error PgmqRuntimeError :> es`. Previously they had *no* error
  constraint and threw a `fail`-derived `IOError` outside the Error
  effect channel, which meant any `runError` wrapper around a traced
  program was a no-op. Code that relied on that silent swallowing now
  receives typed errors; update call sites to wrap with
  `runError @PgmqRuntimeError`.

### New Features

* `fromUsageError :: Hasql.Pool.UsageError -> PgmqRuntimeError` —
  convert raw hasql-pool errors into the pgmq-effectful error type.
  Useful when layering `pgmq-effectful` over code that already calls
  `Pool.use` directly.

* `isTransient :: PgmqRuntimeError -> Bool` — classification helper for
  retry logic. Returns True for acquisition timeouts, networking
  connection errors, unrecognized libpq connection errors, and
  session-level connection drops; False for authentication,
  compatibility, missing-types, statement, script, and driver errors.

* New test suite `pgmq-effectful-test` asserts that both interpreters
  surface typed `PgmqRuntimeError` values through the Error channel.

### Migration Guide

Before:

    import Pgmq.Effectful (PgmqError (..), runPgmq)

    handler =
      runEff . runError @PgmqError . runPgmq pool $ action

After:

    import Pgmq.Effectful (PgmqRuntimeError (..), runPgmq)

    handler =
      runEff . runError @PgmqRuntimeError . runPgmq pool $ action

For retry logic:

    import Pgmq.Effectful (isTransient)

    retryIfTransient action = do
      result <- runError @PgmqRuntimeError action
      case result of
        Right a -> pure (Right a)
        Left (_, err)
          | isTransient err -> retryIfTransient action
          | otherwise -> pure (Left err)

## 0.1.3.0 -- 2026-03-12

### Other Changes

* Update repository homepage URL to shinzui/pgmq-hs

## 0.1.2.0 -- 2026-03-03

* Version bump only (no changes)

## 0.1.1.0 -- 2026-02-23

### New Features

* Effectful effects and interpreters for pgmq 1.11.0 topic routing operations
* Topic management: `bindTopic`, `unbindTopic`, `validateRoutingKey`, `validateTopicPattern`, `testRouting`, `listTopicBindings`, `listTopicBindingsForQueue`
* Topic sending: `sendTopic`, `sendTopicWithHeaders`, `batchSendTopic`, `batchSendTopicForLater`, `batchSendTopicWithHeaders`, `batchSendTopicWithHeadersForLater`
* Notification management: `listNotifyInsertThrottles`, `updateNotifyInsert`

## 0.1.0.0 -- 2026-02-21

* Initial release
* Effectful effects and interpreters for all pgmq operations
* OpenTelemetry instrumentation via traced interpreter
* Support for pgmq 1.5.0 through 1.10.0 features
