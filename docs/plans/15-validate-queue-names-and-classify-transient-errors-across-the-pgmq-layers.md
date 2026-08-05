---
id: 15
slug: validate-queue-names-and-classify-transient-errors-across-the-pgmq-layers
title: "Validate queue names and classify transient errors across the pgmq layers"
kind: exec-plan
created_at: 2026-07-23T23:12:20Z
intention: intention_01kz9yszpmejztjbet6k4bvcf7
master_plan: "docs/masterplans/3-harden-the-pgmq-hs-family-surfaced-by-the-2026-07-review.md"
---

# Validate queue names and classify transient errors across the pgmq layers

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.
If durable project context changes, update or create a note under `docs/design/` in the same
change.


## Purpose / Big Picture

Three input/classification defects in the pgmq-hs family, the Haskell client stack for PGMQ,
let bad inputs and
mis-labeled errors slip through the typed surface. First (PGH-7): the Haskell validator
`parseQueueName` accepts uppercase letters while the SQL layer lowercases physical table
names and stores the original casing in metadata — so `MyQueue` and `myqueue` are two
logical queues sharing ONE physical table (interleaved messages; dropping one destroys the
other's data), and enabling insert notifications on `MyQueue` writes a throttle row the
trigger can never match, silently killing notify. Worse, `QueueName`'s `FromJSON` instance
is newtype-derived, so config-loaded names bypass validation entirely (any string of any
length). Second (PGH-10): `isTransient` — the retry-classification predicate that
shibuya-pgmq-adapter's retry loops call on every failed ack/poll — classifies every
server-reported statement error as permanent, including serialization failures (40001),
deadlocks (40P01), lock timeouts (55P03), administrator shutdown (57P01), crash shutdown
(57P02), temporary refusal during startup/recovery (57P03), and resource exhaustion (53xxx),
all of which arrive as `StatementSessionError` and all of which are precisely the errors
retries exist for. Third (PGH-11): the message decoder requires a non-null body, but
the `message` column is nullable and `pgmq.send(queue, NULL::jsonb)` is legal SQL — one
NULL-bodied row inserted by any non-Haskell producer makes EVERY read batch containing it
fail at decode, after the read statement has already bumped `vt`/`read_ct` for the whole
batch: an unidentifiable poison row that cannot even be seen from the Haskell side.

After this plan: uppercase (and empty) queue names are rejected at both entry paths
(`parseQueueName` and `FromJSON`); the transient SQLSTATEs classify as transient and are
pinned by tests; and a NULL body decodes as JSON `null`, so the poison row is readable,
identifiable, and archivable through the normal API. It also leaves a complete changelog and
consumer-impact handoff for the single release owner, plan 12.


## Progress

- [x] M1 (2026-08-05, repro/evidence): raw-SQL aliasing tests demonstrate consequences
      (a) and (b) live on a dedicated instance (throttle row never matched for `MyQueue`;
      one physical table, two meta rows, interleaving, cross-destruction on drop);
      `parseQueueName`/`FromJSON` red state recorded via `cabal repl pgmq-core`
      (rejection tests land with the M2 suite); NullBodySpec written — two target tests
      red (whole-batch decode failure) and the `read_ct`-bump evidence green;
      ClassificationSpec extended — all nine transient-SQLSTATE assertions red.
      Transcripts recorded in Surprises & Discoveries.
- [x] M2 (2026-08-05, fix): `parseQueueName` rejects uppercase and empty; `FromJSON
      QueueName` validates via `parseQueueName`; new `pgmq-core-test` suite green;
      `isTransient` whitelists 40001/40P01/55P03/57P01/57P02/57P03/53xxx inside
      `StatementSessionError`; `messageDecoder` maps SQL NULL to JSON null; all M1 red
      tests green; mixed-case remediation documented in design note 016 and proven by
      `MixedCaseRemediationSpec` for both the twin and no-twin cases, including rerun
      idempotence, `bound_at` preservation, and the trigger matching the throttle row
      after remediation. Design notes 016 and 017 written; 014 extended with the
      NULL-cell rule. `cabal test all` green across all five suites.
- [ ] M3 (release handoff): full pgmq-hs suite green; exact package and root CHANGELOG
      material written; plan 12's consumer rollout checklist verified to include all
      shibuya components and every Mori-discovered direct consumer; no version bumped here.
- [ ] Living sections updated; design distillation pass completed under `docs/design/`
      for the queue-name validation and transient-classification contracts.


## Surprises & Discoveries

Seeded from the 2026-07 pgmq-hs review (2026-07-23; PGH-7 and PGH-10 confirmed by code
reading; PGH-11 decode-throw certain from the decoder, reachability via any non-Haskell
producer):

- The SQL layer itself is consistent-by-lowercasing (`format_table_name` lowercases,
  install SQL line 244) but `pgmq.meta` stores the caller's original casing (lines
  1145-1151, `INSERT ... VALUES (%L, ...)` with the raw name) and the notify trigger
  extracts the LOWERCASED name from the physical table (line 1572) — three views of one
  name that only agree for lowercase input.
- The per-queue advisory lock hashes the RAW name (`hashtext('pgmq.queue_' ||
  queue_name)`, line 113), so `MyQueue` and `myqueue` do not even serialize against each
  other while mutating the same physical table — one more aliasing artifact that
  rejection at the source removes.
- hasql (pinned 1.10.3.5 via cabal.project source-repository-package) names the
  row-count decode failure `UnexpectedRowCountStatementError` (older hasql called it
  "UnexpectedAmountOfRows"); the SQLSTATE for a server error is the first field of
  `ServerError` inside `ServerStatementError` inside the six-field
  `StatementSessionError`.

(Add new discoveries below as work proceeds.)

- M1 (2026-08-05): plan 14's migration `0003-notify-crash-safety-and-locking.sql` changed
  the observable shape of aliasing consequence (a). The trigger now fails open when its
  throttle row is absent, and a mixed-case throttle row IS absent from the trigger's
  point of view (it looks up the lowercased name). So `enable_notify_insert('MyQueue')`
  no longer silently kills notification — it silently /ignores the configured throttle/:
  notifications fire unthrottled on every insert while `last_notified_at` stays frozen at
  the epoch. The frozen epoch timestamp is the durable evidence either way, and is what
  `AliasingSpec` asserts, against a lowercase control queue whose row does get stamped.
- M1 (2026-08-05): `pgmq.notify_insert_throttle` carries the same
  `REFERENCES pgmq.meta (queue_name) ON DELETE CASCADE` foreign key as
  `pgmq.topic_bindings` (install SQL lines 25-31), and neither has `ON UPDATE`, so a
  naive `UPDATE pgmq.meta SET queue_name = lower(queue_name)` fails outright against
  either child. The M2 remediation must snapshot and re-create BOTH child kinds, not just
  topic bindings.
- M1 (2026-08-05): the mixed-case rows these tests construct are themselves poison for
  every concurrent test that calls `listQueues` once `parseQueueName` tightens, because
  `queueDecoder` re-validates names via `D.refine` and tasty runs specs in parallel
  (`QueueSpec` alone calls `listQueues` in four places against the shared pool). See the
  Decision Log entry on the dedicated instance.
- M1 red transcripts (2026-08-05). pgmq-effectful: 9 of 30 failed — exactly the nine
  transient-SQLSTATE assertions (`40001`, `40P01`, `55P03`, `57P01`, `57P02`, `57P03`,
  `53100`, `53200`, `53300` each "expected transient"); the permanent and row-count
  assertions passed. pgmq-hasql: 2 of 71 failed, both NullBodySpec targets, with the
  decode failure landing on the poison row and column exactly as predicted:

  ```text
  NULL Message Body (PGH-11)
    a batch containing a NULL body reads fully:                 FAIL
      Session failed: SessionUsageError (StatementSessionError 1 0
        "select * from pgmq.read($1,$2,coalesce($3,1),coalesce($4,'{}'::jsonb))"
        ["\"test_queue_18270\"","30","10","null"] True
        (RowStatementError 2 (CellRowError 5 3802 UnexpectedNullCellError)))
    the NULL-bodied row can be archived through the normal API: FAIL (same shape)
    read_ct is bumped for the whole batch even when decode fails: OK   (evidence)
  Mixed-Case Queue Aliasing (PGH-7 evidence): all 3 OK          (evidence)
  ```

  pgmq-core red state via `cabal repl pgmq-core` — the parser accepts what it must
  reject, and the derived `FromJSON` bypasses even the checks the parser does have:

  ```text
  ghci> parseQueueName "MyQueue"
  Right (QueueName "MyQueue")
  ghci> parseQueueName ""
  Right (QueueName "")
  ghci> fromJSON (String "MyQueue") :: Result QueueName
  Success (QueueName "MyQueue")
  ghci> fromJSON (String "bad-name!") :: Result QueueName
  Success (QueueName "bad-name!")
  ghci> fromJSON (String (T.replicate 60 "x")) :: Result QueueName
  Success (QueueName "xxxx…60 chars…")
  ```

- M2 (2026-08-05): the planned remediation procedure (snapshot children, delete them,
  rename the parent, reinsert) was replaced by a simpler shape with the same guarantees:
  insert the canonical parent first, `UPDATE` the children onto it, then delete the
  mixed-case parent. See the Decision Log. The functional proof that remediation heals
  notification — a post-remediation send stamps `last_notified_at` off the epoch because
  the trigger's lowercase lookup finally matches — passed on the first green run.
- M2 (2026-08-05): no caller anywhere in the repository constructs a name the stricter
  parser rejects — every fixture generator and literal is lowercase (`test_queue_`,
  `cfg_test_`, `crash_test_`, `race_test_`, `bench_`, …), verified by grep across all
  packages including `pgmq-bench`. The tightening breaks no in-repo code, and
  `cabal test all` stayed green across all five suites on the first post-fix run
  (pgmq-hasql 73 including the three NullBodySpec and two MixedCaseRemediationSpec
  tests, pgmq-effectful 30 with all thirteen new classification cases, the new
  pgmq-core-test 12, pgmq-config, pgmq-migration).


## Decision Log

- Decision: REJECT uppercase queue names in `parseQueueName` (lowercase ASCII letters,
  digits, underscore only) rather than silently normalizing to lowercase.
  Rationale: Normalization re-introduces aliasing against pre-existing mixed-case meta
  rows (a normalized `MyQueue` would silently join a previously-created `myqueue`'s
  physical table while a `MyQueue` meta row still exists), and it makes
  `queueNameToText` disagree with what the caller wrote — invisible behavior. Rejection
  is loud, at the boundary, and matches the smart-constructor design the type already
  has (the `QueueName` constructor is unexported). A migration note for existing
  uppercase meta rows is mandatory (see M2) because the stricter parser makes such rows
  fail `listQueues` decoding (`queueDecoder` runs `parseQueueName` via `D.refine`,
  `pgmq-hasql/src/Pgmq/Hasql/Decoders.hs` line 62).
  Date: 2026-07-23

- Decision: Also reject the empty string in `parseQueueName` (it passes both current
  checks and would produce the physical table `q_`).
  Rationale: Same boundary, same fix, zero legitimate use; verified no caller constructs
  an empty name.
  Date: 2026-07-23

- Decision: `FromJSON QueueName` becomes a hand-written instance that runs
  `parseQueueName` (via `Aeson.withText`), replacing the newtype-derived instance.
  `ToJSON` stays derived. The same derived-`FromJSON` bypass exists on `RoutingKey` and
  `TopicPattern` (`pgmq-core/src/Pgmq/Types.hs` lines 111 and 129) — noted as an
  adjacent hazard, deliberately OUT of this plan's scope (no finding filed; record for a
  follow-up).
  Date: 2026-07-23

- Decision: Transient SQLSTATE whitelist inside `StatementSessionError`: exactly
  `40001` (serialization_failure), `40P01` (deadlock_detected), `55P03`
  (lock_not_available), `57P01` (admin_shutdown), `57P02` (crash_shutdown), `57P03`
  (cannot_connect_now), and the `53` class prefix
  (insufficient resources: 53000/53100/53200/53300/53400). Everything else in
  `StatementSessionError` — other server errors, row-count/column/decode errors —
  remains permanent.
  Rationale: These states are canonical retry-worthy failures; 57P02 and 57P03 are the
  crash-recovery and temporarily-unavailable siblings of 57P01 and must not be classified
  as permanent. 40P01 is genuinely
  reachable here (overlapping batch delete/archive statements lock message rows in
  statement-internal order, so two sessions finalizing overlapping id sets can
  deadlock). The existing `ClassificationSpec` deliberately did not pin the
  `StatementSessionError` case; now it pins both directions.
  Date: 2026-07-23

- Decision: Decode a NULL `message` column as JSON `null` (`MessageBody
  Data.Aeson.Null`) rather than changing `Message.body` to `Maybe MessageBody` or
  documenting single-client ownership.
  Rationale: The mapping is API-compatible (no consumer breakage beyond plan 13's
  already-breaking release), and it surfaces the poison row to the consumer, who can
  see it (`body == MessageBody Null`), route it, and dead-letter/archive it through the
  normal API. Accepted conflation, recorded here: after this change a SQL NULL body and
  an explicitly-sent JSON `null` body are indistinguishable on read — which is the
  honest merged semantics, since both mean "no usable payload". "Document single-client
  ownership" was rejected because the queue tables are plain SQL any producer can
  write to; a doc cannot un-arm the trap.
  Date: 2026-07-23

- Decision: Plan 12, not this plan, owns version bumps, final changelogs, consumer source
  changes, bounds, and consumer test runs.
  Rationale: The official repository already had a release plan. A conditional second release
  owner made execution order ambiguous and missed non-library shibuya bounds.
  Date: 2026-07-23

- Decision: Mixed-case remediation must preserve `pgmq.topic_bindings` as well as
  notification configuration and must execute transactionally.
  Rationale: `pgmq.topic_bindings.queue_name` references `pgmq.meta.queue_name` without
  `ON UPDATE` and with `ON DELETE CASCADE`. Updating a parent with bindings fails; deleting
  it silently deletes those bindings. The remediation must snapshot, repoint or restore the
  child rows before changing metadata.
  Date: 2026-07-23

- Decision: `AliasingSpec` (and the M2 mixed-case remediation test) run on a dedicated
  PostgreSQL instance provisioned per-module via `EphemeralPg.startCached`, mirroring
  `pgmq-config/test/NotifyCrashSpec.hs`, instead of the suite-shared pool.
  Rationale: these tests must create mixed-case `pgmq.meta` rows, and after M2 any such
  row — however short-lived — makes concurrent `listQueues` decoding fail
  (`queueDecoder` re-validates via `parseQueueName`); `QueueSpec` calls `listQueues` on
  the shared pool in four tests and tasty runs specs in parallel. Isolation by instance
  removes the race instead of narrowing it.
  Date: 2026-08-05

(Record further decisions as they are made, with dates.)


## Outcomes & Retrospective

(To be filled during and after implementation. Before completion, record the queue-name
validation contract and transient-classification whitelist under `docs/design/`.)


## Context and Orientation

Work happens in this Cabal multi-package repository; ignore `dist-newstyle/` and run commands
from the repository root inside `nix develop`, which provides GHC 9.12.4, Cabal, and the
PostgreSQL binaries the tests need. This plan does not edit consumer repositories; plan 12
owns that rollout.

Relevant ADR: keiro's `docs/adr/0001-keiro-pgmq-job-processing-telemetry-contract.md` —
tangentially relevant. It pins the traced pgmq-effectful interpreter's span semantics;
this plan changes error *classification* (`isTransient`) but must not change what the
traced interpreter emits. That holds by construction: `isTransient` is a pure predicate
consumers call on an already-surfaced error; the traced interpreter's error labeling
(`errorStatusDescription`, `pgmq-effectful/src/Pgmq/Effectful/Interpreter/Traced.hs`
lines 498-516) is untouched. Both interpreters surface the same `PgmqRuntimeError` via
`fromUsageError`, and `isTransient` is a single shared function — so plain/traced parity
is structural, not something to re-implement. Durable decisions for this repository belong
under `docs/design/`.

Exact locations of the three defects (verify each before editing; if a line drifted,
find the construct by name and update this plan):

**PGH-7 — queue-name aliasing.** `parseQueueName`
(`pgmq-core/src/Pgmq/Types.hs` lines 91-99) accepts any ASCII alphanumeric plus
underscore (`isValidChar c = (isAscii c && isAlphaNum c) || c == '_'`, line 99), max
length 47 (63-character PostgreSQL identifier limit minus the longest prefix
`archived_at_idx_`, lines 101-105). The SQL side lowercases physical names
(`pgmq.format_table_name`, install SQL
`pgmq-migration/migrations/0001-install-v1.11.0.sql` line 244: `RETURN lower(prefix ||
'_' || queue_name)`) but `pgmq.create` stores the ORIGINAL casing in `pgmq.meta` (lines
1145-1151), and the notify trigger extracts the lowercased name from the table name
(line 1572). Consequences: (a) `enable_notify_insert('MyQueue')` inserts throttle row
`'MyQueue'` but the trigger looks up `'myqueue'` — the throttle UPDATE matches nothing
and no notification ever fires, silently; (b) `create('MyQueue')` then
`create('myqueue')` yields ONE physical table `q_myqueue` (both creates are `CREATE
TABLE IF NOT EXISTS`) with TWO meta rows — sends and reads interleave between "both"
queues, and `drop_queue('myqueue')` destroys the other's messages; (c) `QueueName`
derives `FromJSON` newtype-style (`Types.hs` lines 73-74), so JSON/config-loaded names
skip length and charset checks entirely. The `QueueName` data constructor is not
exported (export list lines 3-25 export only the type, `parseQueueName`,
`queueNameToText`), so `parseQueueName`, `FromJSON`, and the `Lift` instance (compile
time only) are the complete set of entry paths. Also relevant: `queueDecoder`
re-validates names read back from the database through `parseQueueName`
(`pgmq-hasql/src/Pgmq/Hasql/Decoders.hs` line 62, `D.refine`), which is what makes the
stricter parser a migration concern for existing uppercase meta rows (see M2).

**PGH-10 — transient errors classified permanent.** `isTransient`
(`pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs` lines 65-78) maps
`HasqlErrors.StatementSessionError {} -> False` unconditionally (line 75). But
serialization failures, deadlocks, lock timeouts, shutdowns, and resource exhaustion
all arrive as server errors inside `StatementSessionError`. The hasql error shape
(pinned hasql 1.10.3.5; source
`/Users/shinzui/Keikaku/hub/haskell/hasql-project/hasql/src/library/Hasql/Errors.hs`,
which re-exports `Hasql.Engine.Errors`): `StatementSessionError Int Int Text [Text]
Bool StatementError` (total statements, index, SQL, params, prepared, error), where
`StatementError` includes `ServerStatementError ServerError` and `ServerError`'s first
field is the five-character SQLSTATE code as `Text`. All constructors are exported from
`Hasql.Errors`. The existing `pgmq-effectful/test/ClassificationSpec.hs` pins every
case EXCEPT `StatementSessionError` — deliberately, awaiting this decision. Consumer
reality (verified 2026-07-23): keiro-pgmq does NOT call `isTransient` anywhere (grep:
zero hits) — this fix is for direct consumers; shibuya-pgmq-adapter IS one — it imports
`isTransient` (`shibuya-pgmq-adapter/src/Shibuya/Adapter/Pgmq/Internal.hs` line 62) and
gates every ack/poll retry on it (`retryingTransient`, lines 506-515), so after the
family bump its retry loops start actually retrying deadlocks and serialization
failures instead of failing fast.

**PGH-11 — NULL body poisons every batch.** `messageDecoder` requires a non-null body
(`pgmq-hasql/src/Pgmq/Hasql/Decoders.hs` line 51: `MessageBody <$> D.column
(D.nonNullable D.jsonb)`), but the queue table's `message` column is nullable (install
SQL line 1106) and `SELECT pgmq.send('q', NULL::jsonb)` is legal. Any non-Haskell
producer (psql, another language's client, a trigger) can insert a NULL body; every
subsequent `read`/`readWithPoll`/`pop` batch containing that row fails at decode —
AFTER the read's UPDATE already bumped `vt` and `read_ct` for the entire batch (the
statement succeeded; decoding its result failed). The row cannot be seen, read, or
archived through the Haskell client, and it re-poisons every batch each time its
visibility timeout lapses.

Test infrastructure (same as the sibling plans): every pgmq-hs suite self-provisions
PostgreSQL via the `ephemeral-pg` library — each package's `test/EphemeralDb.hs` starts
a cached temp server, applies the full migration ledger through pg-migrate, and hands
the suite a hasql-pool `Pool`. No external database or env vars. `pgmq-core` currently
has NO test suite (its `.cabal` defines only the library); M2 adds one. Suites are
tasty-based; DB-backed specs take the shared `Pool` (see
`pgmq-hasql/test/Main.hs` and helpers in `pgmq-hasql/test/TestUtils.hs`:
`assertSession`, `runSession`, `assertSessionFails`, `withTestFixture`).


## Plan of Work

### Milestone 1 — evidence and red tests

Scope: demonstrate all three defects inside this repo's test harness before fixing
anything. Nothing under any `src/` changes. What exists at the end: three new/extended
test modules whose failures (and raw-SQL evidence) are captured in Surprises &
Discoveries.

`pgmq-hasql/test/AliasingSpec.hs` (register in `pgmq-hasql.cabal` `other-modules` and
`test/Main.hs`): drive the SQL layer directly with `Hasql.Session.sql` /
`Hasql.Session.statement` raw statements so the tests remain valid after the Haskell
parser tightens (uppercase names will then be unconstructible through the API — which
is the point). Use a random suffix as the existing fixtures do, e.g. `MyQueue_<n>` /
`myqueue_<n>`. Cases, in prose: create both casings via `select pgmq.create(...)`;
assert exactly ONE physical table exists
(`select count(*) from information_schema.tables where table_schema='pgmq' and
table_name='q_myqueue_<n>'` equals 1 and no `q_MyQueue_<n>` variant) while `pgmq.meta`
holds TWO rows for the pair; send via the uppercase name and read via the lowercase one
to show interleaving; `select pgmq.drop_queue('myqueue_<n>')` and assert the uppercase
alias is now broken (a subsequent send through it errors — its meta row survives but
the table is gone). For consequence (a): create + `select
pgmq.enable_notify_insert('MyQueue_<n>', 0)`, send a message, and assert the throttle
row's `last_notified_at` is still the epoch (`to_timestamp(0)`) — the trigger looked up
the lowercase name, found nothing, and (today) suppressed the notification silently.
These are evidence tests: they PASS against current code and stay green after M2 (the
SQL layer is out of scope; the Haskell boundary is the fix). Label them clearly as
documenting why rejection matters.

`pgmq-core` rejection tests: because pgmq-core has no suite yet, write these as part of
the new suite you will register in M2 but run them now against unfixed code to record
the red state — or simply note that `parseQueueName "MyQueue"` currently returns
`Right` by evaluating it in `cabal repl pgmq-core`. Target assertions (red today):
`parseQueueName "MyQueue"` is `Left (InvalidQueueName ...)`; `parseQueueName ""` is
`Left`; `Aeson.fromJSON (Aeson.String "MyQueue") :: Result QueueName` is `Error`;
same for a 60-character and a hyphenated string via FromJSON (today ALL of these
succeed through FromJSON — the bypass).

`pgmq-hasql/test/NullBodySpec.hs`: create a queue, send two well-formed messages via
`Sessions.sendMessage`, then insert the poison via raw SQL (`Hasql.Session.sql` with
the queue name spliced — safe here, names are `[a-z0-9_]`):
`select pgmq.send('<qname>', null::jsonb)`. Red assertions against current code:
`Sessions.readMessage` with `batchSize = Just 10` fails the session (decode error on
the NULL cell), and — the poison property — a raw
`select read_ct from pgmq.q_<qname>` shows `read_ct` bumped to 1 on ALL THREE rows even
though the Haskell call failed. Write the M2 target assertions alongside, commented or
behind the fix.

`pgmq-effectful/test/ClassificationSpec.hs`: extend with a helper that builds
`PgmqSessionError (StatementSessionError 1 0 "select 1" [] True (ServerStatementError
(ServerError code "boom" Nothing Nothing Nothing)))` for a given `code`, then assert
transient for `"40001"`, `"40P01"`, `"55P03"`, `"57P01"`, `"57P02"`, `"57P03"`,
`"53100"`, `"53200"`, `"53300"`, and permanent for `"23505"` (unique violation),
`"42P01"` (undefined table),
`"22P02"` (bad text representation), plus permanent for a non-server statement error
(`UnexpectedRowCountStatementError 1 1 0`). All red today (every one currently
classifies permanent, so the transient assertions fail).

Acceptance: `cabal test pgmq-hasql-test pgmq-effectful-test --test-show-details=direct`
shows the predicted failures; AliasingSpec passes (it is evidence, not a fix gate);
transcripts pasted into Surprises & Discoveries.

### Milestone 2 — the fixes and the new pgmq-core suite

Scope: three small source changes plus a new test suite; every M1 red test goes green.

`pgmq-core/src/Pgmq/Types.hs`:

- `parseQueueName` (lines 91-99): reject empty and uppercase. Replace the guards with
  an added emptiness check and change the character predicate to lowercase-only:

```haskell
parseQueueName :: Text -> Either PgmqError QueueName
parseQueueName t
  | T.null t = Left $ InvalidQueueName "The queue name is empty."
  | not isShortEnough = Left $ InvalidQueueName "The queue name is too long."
  | not hasValidCharacters =
      Left $
        InvalidQueueName
          "The queue name contains invalid characters (allowed: lowercase ASCII letters, digits, underscore)."
  | otherwise = Right $ QueueName t
  where
    isShortEnough = T.length t <= maxQueueNameLength
    hasValidCharacters = T.all isValidChar t
    isValidChar c = (isAscii c && (isLower c || isDigit c)) || c == '_'
```

  Adjust the `Data.Char` import (line 29) to `(isAscii, isDigit, isLower)`. Keep the
  length machinery (lines 101-105) untouched. Update the haddock to state the contract
  and WHY: SQL `format_table_name` lowercases physical names while `pgmq.meta` stores
  the original casing, so mixed-case names alias one physical table under two
  identities; lowercase-only input makes all three representations agree.

- `FromJSON QueueName`: remove `FromJSON` from the deriving-newtype list (line 74,
  keeping `Eq, Ord, ToJSON`) and add:

```haskell
instance FromJSON QueueName where
  parseJSON = Aeson.withText "QueueName" $ \t ->
    either (fail . show) pure (parseQueueName t)
```

  with `import Data.Aeson qualified as Aeson` added (the module currently imports only
  the classes). `PgmqError` already derives `Show`.

New suite: add to `pgmq-core/pgmq-core.cabal` a `test-suite pgmq-core-test`
(exitcode-stdio-1.0, `hs-source-dirs: test`, `main-is: Main.hs`; build-depends: base,
aeson, pgmq-core, tasty ^>=1.5, tasty-hunit ^>=0.10, text — mirror the warnings import
and GHC2024 defaults the other suites use). `pgmq-core/test/Main.hs` runs a
`QueueNameSpec` covering: acceptance (`"my_queue_123"`, a 47-char lowercase name);
rejection with the right `InvalidQueueName` message for `"MyQueue"`, `""`, a 48-char
name, `"bad-name"`, `"queue!"`; and the FromJSON path — `Aeson.fromJSON (Aeson.String
"myqueue")` succeeds and round-trips through `ToJSON`, while `"MyQueue"`, `""`, and an
overlong string produce `Aeson.Error`. Also add the wired-in cabal.project entry: none
needed — `pgmq-core` is already a project package, and `cabal test all` picks up the
new suite automatically.

The operational migration note belongs in the CHANGELOG entry, the `parseQueueName` Haddock,
and a `docs/design/` note. After upgrading, a database that still contains mixed-case rows in
`pgmq.meta` will fail `listQueues` decoding, and therefore pgmq-config reconciliation,
because `queueDecoder` re-validates names. Detect every affected parent and child row before
the package upgrade:

```sql
SELECT m.queue_name,
       lower(m.queue_name) AS canonical_name,
       EXISTS (
         SELECT 1 FROM pgmq.meta lower_meta
         WHERE lower_meta.queue_name = lower(m.queue_name)
       ) AS has_lowercase_twin,
       (SELECT count(*) FROM pgmq.topic_bindings b
        WHERE b.queue_name = m.queue_name) AS binding_count,
       (SELECT throttle_interval_ms FROM pgmq.notify_insert_throttle n
        WHERE n.queue_name = m.queue_name) AS throttle_interval_ms
FROM pgmq.meta m
WHERE m.queue_name <> lower(m.queue_name);
```

Back up the returned metadata and the complete binding rows before changing anything. For
each mixed-case row, perform one transaction and lock both the mixed-case row and any
lowercase twin with `SELECT ... FOR UPDATE`.

If a lowercase twin exists, the two metadata rows already alias one physical queue table.
First update every `pgmq.topic_bindings` child from the mixed-case name to the lowercase name;
the target parent already exists, so the foreign key remains valid. Record and delete any
mixed-case `pgmq.notify_insert_throttle` row. Keep an existing lowercase throttle
configuration; otherwise re-enable notification for the lowercase name with the recorded
interval. Only after the children are safe may the transaction delete the mixed-case
`pgmq.meta` row.

If no lowercase twin exists, snapshot the row's topic bindings, including `pattern` and
`bound_at`, and its optional throttle interval inside the transaction. Delete those child
rows, update `pgmq.meta.queue_name` to lowercase, reinsert the bindings under the lowercase
name while preserving `bound_at`, and re-enable notification with the saved interval when a
throttle row existed. The child deletion is mandatory: both foreign keys lack `ON UPDATE`;
`topic_bindings` additionally has `ON DELETE CASCADE`, so a naive parent delete would silently
lose routing configuration.

Add an integration test that seeds a mixed-case metadata row with both a notification
throttle and at least two topic bindings, runs the documented remediation in a transaction,
and proves that the lowercase row, bindings, and throttle configuration survive. Also cover
the lowercase-twin case. The script must be safe to rerun: after success the detection query
returns no rows and the second run makes no change. As of 2026-07-23 no registered consumer
creates mixed-case names; verify deployed databases independently before releasing.

`pgmq-effectful/src/Pgmq/Effectful/Interpreter.hs`:

- Rewrite the `PgmqSessionError` branch of `isTransient` (lines 65-78):

```haskell
  PgmqSessionError e -> case e of
    HasqlErrors.ConnectionSessionError _ -> True
    HasqlErrors.StatementSessionError _ _ _ _ _ statementError ->
      case statementError of
        HasqlErrors.ServerStatementError (HasqlErrors.ServerError code _ _ _ _) ->
          isTransientSqlState code
        _ -> False
    HasqlErrors.ScriptSessionError {} -> False
    HasqlErrors.MissingTypesSessionError _ -> False
    HasqlErrors.DriverSessionError _ -> False

-- | SQLSTATEs that indicate a transient, retry-worthy condition:
-- 40001 serialization_failure, 40P01 deadlock_detected, 55P03
-- lock_not_available, 57P01 admin_shutdown, 57P02 crash_shutdown,
-- 57P03 cannot_connect_now, and class 53 (insufficient resources).
-- Everything else reported by the server is permanent for
-- retry purposes.
isTransientSqlState :: Text -> Bool
isTransientSqlState code =
  code `elem` ["40001", "40P01", "55P03", "57P01", "57P02", "57P03"]
    || "53" `T.isPrefixOf` code
```

  Add `import Data.Text (Text)` / `import Data.Text qualified as T` as needed (the
  module has no text import today; `text` is already a pgmq-effectful dependency).
  Update the `isTransient` haddock (lines 53-60 region) to name the whitelist. Do not
  export `isTransientSqlState` unless a test needs it — the ClassificationSpec tests go
  through `isTransient` itself. No traced-interpreter change: parity is structural (one
  shared predicate over one shared error type; see Context).

`pgmq-hasql/src/Pgmq/Hasql/Decoders.hs`:

- `messageDecoder` line 51: change the body column to nullable-with-null-mapping:

```haskell
    <*> (MessageBody . fromMaybe Aeson.Null <$> D.column (D.nullable D.jsonb)) -- message
```

  Add `import Data.Aeson qualified as Aeson` and `import Data.Maybe (fromMaybe)`
  (aeson is already a pgmq-hasql dependency). Haddock the mapping and the conflation:
  SQL NULL and JSON null both surface as `MessageBody Aeson.Null`.

Then finalize the M1 tests: `NullBodySpec`'s target assertions — `readMessage` with
`Just 10` now returns all three rows; exactly one has `body == MessageBody Aeson.Null`;
`Sessions.archiveMessage` succeeds on it; a follow-up read (after vt expiry or with a
fresh send) contains no poison. ClassificationSpec's transient assertions now pass.
pgmq-core-test passes.

Acceptance: `cabal test all --test-show-details=direct` green from the pgmq-hs root,
including the brand-new `pgmq-core-test`. AliasingSpec still green (unchanged SQL-layer
behavior, now unreachable from validated Haskell input).

### Milestone 3 — family validation and release handoff

Scope: close this child plan inside pgmq-hs without competing with the single release owner.
Run `cabal build all` and `cabal test all --test-show-details=direct`, write the package and
root CHANGELOG material for queue-name validation, the expanded transient whitelist, and NULL
body decoding, and update this plan's living sections.

Then inspect
`docs/plans/12-expose-grouped-reads-on-the-umbrella-api-and-release-0-5-0-0.md`. Its consumer
rollout must contain all of the following before this plan is marked complete:

- both keiro-pgmq library and test-component bounds;
- every shibuya adapter library and test-component bound, plus the example and benchmark
  package bounds rather than only the adapter library stanza;
- the shibuya `setVisibilityTimeoutAt` source adjustment required by plan 13;
- a Mori-generated direct-consumer inventory, including rei's current `^>=0.4` pins, with
  each consumer either upgraded and tested or explicitly recorded as remaining on the
  compatible 0.4 line.

Do not change package versions or consumer repositories here. Acceptance is the full pgmq-hs
suite green, release-note material present, the mixed-case remediation test preserving both
topic bindings and notification configuration, and plan 12 carrying an explicit, complete
consumer rollout.


## Concrete Steps

Run pgmq-hs work from the repository root inside `nix develop`:

```bash
cd .

# M1: add AliasingSpec + NullBodySpec (pgmq-hasql), extend ClassificationSpec
cabal test pgmq-hasql-test --test-show-details=direct
cabal test pgmq-effectful-test --test-show-details=direct

# M2: edit pgmq-core/src/Pgmq/Types.hs, pgmq-effectful Interpreter.hs,
#     pgmq-hasql Decoders.hs; add pgmq-core test suite
cabal build all
cabal test all --test-show-details=direct
```

Expected transcript shapes:

```text
# M1 (red)
ClassificationSpec
  40P01 deadlock is transient:            FAIL (expected transient)
NullBodySpec
  batch containing NULL body reads fully: FAIL
    Session failed: ... StatementSessionError ... Unexpected null value ...
  poison row bumped read_ct anyway:       OK   (evidence)

# M2 (green)
pgmq-core-test
  parseQueueName rejects "MyQueue":       OK
  FromJSON rejects unvalidated names:     OK
ClassificationSpec:                       all OK
NullBodySpec:                             all OK
```

M3 uses `cabal build all` and `cabal test all`; plan 12 contains all consumer commands.


## Validation and Acceptance

A novice can verify each half independently. Queue names: `cabal test pgmq-core-test`
shows `parseQueueName` and `FromJSON` rejecting `"MyQueue"`, `""`, overlong and
bad-charset names while accepting lowercase names, and `cabal repl pgmq-core` confirms
`parseQueueName "MyQueue"` is a `Left`; the pgmq-hasql `AliasingSpec` documents, live
against PostgreSQL, exactly what those rejections prevent (one physical table behind
two meta identities; the silent notify miss; drop destroying the alias's messages).
Classification: `cabal test pgmq-effectful-test` pins
40001/40P01/55P03/57P01/57P02/57P03/53xxx as transient and
unique-violation/undefined-table/decode failures as permanent, all
through the public `isTransient`. Poison row: `cabal test pgmq-hasql-test` shows a
SQL-inserted NULL-body message being read (as JSON null) and archived through the
Haskell client — a sequence that failed at the read step before the fix, with the
before-state (whole-batch decode failure after `read_ct` was bumped) preserved in the
M1 transcript. Release handoff: plan 12 contains the complete bounds, source-change, and
consumer-test matrix before it cuts 0.5.0.0.


## Idempotence and Recovery

All pgmq-hs steps are re-runnable: test modules are additive; the three source edits
are idempotent replacements; no migration file and no persistent database state are involved
(ephemeral databases are per-run). The documented mixed-case remediation is transactional,
backs up child rows first, and becomes a no-op after names are lowercase; its integration test
must prove a second run makes no change. If the stricter `parseQueueName` must be rolled back after release
(e.g. an unanticipated mixed-case deployment surfaces), the safe path is the documented
meta-row remediation, not a parser revert — record any such event in the Decision Log.


## Interfaces and Dependencies

End-state interfaces (full module paths; unchanged items not listed):

```haskell
-- pgmq-core, Pgmq.Types
parseQueueName :: Text -> Either PgmqError QueueName
  -- now rejects: empty, length > 47, any char outside [a-z0-9_]
instance FromJSON QueueName  -- hand-written, validates via parseQueueName
instance ToJSON QueueName    -- unchanged (derived)

-- pgmq-effectful, Pgmq.Effectful.Interpreter (re-exported by Pgmq.Effectful)
isTransient :: PgmqRuntimeError -> Bool
  -- StatementSessionError/ServerStatementError with SQLSTATE 40001, 40P01,
  -- 55P03, 57P01, 57P02, 57P03, or class 53 now classifies transient

-- pgmq-hasql, Pgmq.Hasql.Decoders
messageDecoder :: D.Row Message
  -- message column decoded nullable; SQL NULL surfaces as MessageBody Aeson.Null
```

Dependencies: no new library dependencies anywhere. New test suite `pgmq-core-test`
(base, aeson, pgmq-core, tasty, tasty-hunit, text). The hasql error constructors used
in tests (`StatementSessionError`, `ServerStatementError`, `ServerError`,
`UnexpectedRowCountStatementError`) are all exported from `Hasql.Errors` in the pinned
hasql 1.10.3.5. Coordination: plan 13 owns all statement/encoder changes and the
`Maybe Message` break; plan 14 owns the notify SQL and `notifyChannelName` (whose `toLower`
becomes purely defensive once this plan lands); plan 12 owns the resulting shibuya source
fix, every consumer bound, and the release.


## Revision Note

2026-07-23: Relocated from keiro plan 131 into the authoritative pgmq-hs repository.
Expanded transient classification to 57P02/57P03, replaced the unsafe mixed-case procedure
with a transactional remediation that preserves `topic_bindings` and notification
configuration, removed conditional last-lander duties, and handed the complete consumer
rollout to plan 12.
