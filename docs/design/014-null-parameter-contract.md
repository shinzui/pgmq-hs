# Design Document 014: The NULL-parameter contract

## Status

**Adopted (2026-08-05)**. Supersedes design note 010. Applies to every package in this
repository that binds an optional value into a SQL statement.


## The rule

**No optional parameter may widen the scope of an operation.**

Concretely: when a Haskell field is `Maybe a` and `Nothing` is documented to mean "use the
default", the statement must make that default real. `Nothing` must never reach the
database as a value that removes a bound, disables a filter that was requested, or
violates a constraint. If a sensible narrow default does not exist, the parameter must not
be `Maybe` — give it a concrete type and force the caller to choose.

The failure mode this rule exists to prevent is not "an error occurs". It is "an operation
silently affects more rows than the caller asked for". `pop` with no quantity deleting an
entire queue is the worst case, because `pop` deletes: there is no visibility timeout to
recover from it, and the caller receives a large successful result rather than an error.


## Why the obvious implementation is wrong

Two PostgreSQL behaviours combine to make the natural encoding unsafe. Both are
long-standing PostgreSQL semantics, not pgmq-specific, and both were verified by live
reproduction against this repository's own migration.

**A plpgsql parameter DEFAULT applies only to an omitted argument, never to SQL NULL.**
`pgmq.pop(queue_name TEXT, qty INTEGER DEFAULT 1)` uses 1 when called as `pop('q')`. Called
as `pop('q', NULL)` it binds `qty = NULL` — NULL is a supplied argument, so the DEFAULT is
never consulted. A hasql encoder built from `E.param (E.nullable E.int4)` always sends the
parameter, so it can never trigger a DEFAULT. The same holds for a column DEFAULT and an
explicit NULL in an `INSERT`.

**NULL in a LIMIT clause means LIMIT ALL.** A NULL flowing into `LIMIT` does not error and
does not select zero rows; it removes the bound entirely.

So the natural-looking pair — a `Maybe Int32` field, a nullable encoder, a SQL function
with a `DEFAULT` — produces exactly the opposite of the documented behaviour, silently.


## How to satisfy the rule

Prefer `COALESCE` in the client statement text, keeping the `Maybe` field and the nullable
encoder:

```haskell
pop :: Statement PopMessage (Vector Message)
pop = preparable sql popMessageEncoder decoder
  where
    sql = "select * from pgmq.pop($1,coalesce($2,1))"
    decoder = D.rowVector messageDecoder
```

This is preferred over the alternatives for a specific reason: the SQL functions in
`pgmq-migration/migrations/` are byte-for-byte upstream pgmq parity code. Diverging them
complicates the audited upstream upgrade. A client-side `COALESCE` fixes every Haskell
caller without touching the vendored schema, and it fixes them immediately, independent of
whether any migration has been applied.

Two consequences follow.

A server-side guard is still worth adding for functions this repository owns rather than
vendors, because non-Haskell producers call them too — but it belongs in whichever
migration re-creates that function anyway, not in a migration of its own.

The `COALESCE` value must be the *documented* default, and the documentation must be
checked against the SQL rather than trusted. Where a neutral value exists, use it: in
`pgmq.read` the conditional filter's `CASE` gives `'{}'::jsonb` the meaning "no filter", so
`coalesce($4,'{}'::jsonb)` is the correct neutralization of an absent filter. See design
note 010's Correction for why the previous reading of that `CASE` was wrong.

Where no narrow default exists, use a non-`Maybe` field instead. `UpdateNotifyInsert` does
this: its `throttleIntervalMs` is a plain `Int32`, because "update the throttle to
unspecified" has no meaning.


## Absence is not failure

A related contract, adopted in the same change. When a statement's zero-row result means
"the row is gone", decode it as `Maybe`, not as a single row.

`pgmq.set_vt` is `RETURNS SETOF` and returns zero rows for a message id that no longer
exists. Decoding with `D.singleRow` turns that into hasql's
`UnexpectedRowCountStatementError` wrapped in a `StatementSessionError` — structurally
indistinguishable from a real infrastructure error. A consumer extending a lease cannot
then tell "another worker already handled this message" from "the database is
unreachable", and will either retry a permanent condition or alert on a routine one.

So: `D.rowMaybe` where a raced-away row is an expected outcome, and `D.singleRow` only
where a missing row genuinely indicates corruption. Vector-returning batch statements
already satisfy this — zero rows decode to an empty vector.


## Where this is enforced

`pgmq-hasql/test/NullSemanticsSpec.hs` carries one scope-widening guard per readable
statement plus the notify and `set_vt` cases. Every case fails against an implementation
that binds a bare NULL. When adding a statement with an optional parameter, add its guard
there.

`pgmq-config/test/ConfigSpec.hs` covers the reconciler path, where the consequence is
sharper than a single failed call: `ensureQueues` is a plain `Session` in which each
statement autocommits, so a queue created in one statement stays created when a later
statement fails. A NULL-parameter failure there is not transient — it recurs on every
application startup, forever.


## Related documents

- Design note 009: the first instance found of a bound SQL NULL producing behaviour the
  caller did not ask for. The mechanism there was different — a NULL third argument made
  PostgreSQL resolve `pgmq.send` to the headers overload rather than the delay one — but
  the fix was the same client-side `coalesce($3, 0)`, and it is why `sendMessage`,
  `batchSendMessage`, and the topic send statements already carry coalesces.
- Design note 010: superseded. Its Correction section explains the `pgmq.read` conditional
  `CASE` in detail.
