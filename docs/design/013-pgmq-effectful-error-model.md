# 013 — pgmq-effectful Error Model

Status: accepted
Date: 2026-04-23
See also: [docs/masterplans/1-pgmq-effectful-error-propagation.md](../masterplans/1-pgmq-effectful-error-propagation.md)

## Context

Prior to 0.2.0.0, `pgmq-effectful` exposed a single opaque error constructor
`PgmqPoolError UsageError`. Callers who wanted to branch on the specific
failure mode (SQL state, connection-error kind, acquisition timeout) had to
import `hasql-pool` internals and pattern-match three levels deep through a
type the effectful layer neither documented nor curated.

The traced interpreter made things worse: `runPgmqTraced` called `fail` on
hasql-pool errors, which in `IO` becomes an `IOError`. That escaped the
`Effectful.Error` channel entirely — `runError @PgmqError` around a traced
program caught nothing, because `Error`-effect throws and synchronous
`IOException`s are distinct propagation paths. The runtime happened to
record a status on the active OpenTelemetry span because the span bracket
(`inSpan'`) catches `IO` exceptions; in effect, observability was the only
surviving evidence of an error that the effect layer silently dropped.

Additionally, the effectful package exported `PgmqError`, colliding with
`pgmq-core`'s `Pgmq.Types.PgmqError` (a validation error sum used by
`parseQueueName`, `parseRoutingKey`, `parseTopicPattern`). Users importing
both packages unqualified hit an ambiguous-name error.

This document records the decisions made while fixing those issues.

## Design

The interpreter error type is now:

    data PgmqRuntimeError
      = PgmqAcquisitionTimeout
      | PgmqConnectionError Hasql.Errors.ConnectionError
      | PgmqSessionError Hasql.Errors.SessionError

The three constructors mirror `Hasql.Pool.UsageError` 1-to-1, so the
conversion (`fromUsageError`) is total and obvious. Reusing hasql's own
`ConnectionError` and `SessionError` (rather than wrapping them in
pgmq-effectful-specific types) means users who already know hasql don't
learn a new vocabulary, and we don't need to keep two sets of haddocks in
sync with hasql's behavior.

Both interpreters — plain (`runPgmq`) and traced (`runPgmqTraced`,
`runPgmqTracedWith`) — now carry `Error PgmqRuntimeError :> es`. The traced
interpreter records the error on the active span (status + exception
event, subject to `TracingConfig.recordExceptions`) *before* throwing via
the `Error` effect. Observability and recoverability are independent
channels; neither hides the other.

One classification helper, `isTransient :: PgmqRuntimeError -> Bool`, is
exported. Its truth table is a direct encoding of hasql's own transience
annotations, with one deviation: `OtherConnectionError` is treated as
transient despite hasql documenting it "not transient by default", because
it is the catch-all for unrecognized libpq errors and classing the unknown
as transient errs toward letting retries happen for unfamiliar modes.

## Alternatives considered

**Keep `PgmqPoolError UsageError` and re-export `UsageError` from
`Pgmq.Effectful`.** Rejected: the wrapper adds no value, and users would
still need to understand hasql-pool's type to pattern-match. The structured
constructors cost nothing and document the three cases explicitly.

**Define a new pgmq-specific error vocabulary — `QueueNotFound`,
`UniqueViolation`, etc. — translated from SQL states.** Rejected:
classification by SQL state is application-specific; what counts as "queue
not found" depends on which pgmq SQL function raised the exception and how
the caller intends to respond. The library exposes the raw structure and
leaves that translation to applications. The `isTransient` helper is the
only classification we commit to.

**Move the error type to `pgmq-core`.** Rejected: `pgmq-core` has no hasql
dependency and deliberately keeps its dep closure small
(aeson/base/template-haskell/text/time). Pulling hasql types into the core
would cascade transitively and break the package's role as the hasql-agnostic
type layer; it would also create a dependency inversion for any future
non-hasql backend.

**Rename `pgmq-core`'s `PgmqError` instead of the effectful one.**
Rejected: that type is public API used by `parseQueueName`,
`parseRoutingKey`, `parseTopicPattern`. Renaming it cascades to every
downstream parser user. The effectful-side rename affects only code that
explicitly catches interpreter errors — a much smaller blast radius.

**Unify the plain and traced interpreters via middleware so the
error-handling path is shared.** Rejected: the traced interpreter does
meaningful work (span creation with semantic conventions) that would be
awkward to layer after a plain interpreter. The two share a conversion
helper (`fromUsageError`) and the same `Error PgmqRuntimeError` constraint
shape; that is the right level of sharing.

## Consequences

- Users of `runPgmq` with existing code using `runError @PgmqError` must
  change the type annotation to `@PgmqRuntimeError`. A `DEPRECATED` pragma
  retains the old name for one release cycle with a compile-time warning.
- Users of `runPgmqTraced` who relied on errors escaping as `IO`
  exceptions — or who wrote `runError @PgmqError` around a traced program
  hoping to catch them — now receive typed errors through the `Error`
  effect. This is a bug fix, not a regression.
- `Hasql.Errors.ConnectionError` and `Hasql.Errors.SessionError` are now
  part of `pgmq-effectful`'s public surface (they appear inside the
  exported `PgmqRuntimeError` constructors). Upgrading hasql to a major
  version that changes these types is a breaking change for
  `pgmq-effectful`.
- The new `pgmq-effectful-test` suite (see
  `pgmq-effectful/test/`) asserts at CI time that both interpreters still
  propagate typed errors, preventing a future refactor from quietly
  reintroducing the `fail` path.
