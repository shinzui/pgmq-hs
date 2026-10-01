---
id: 28
slug: provide-stable-json-codecs-for-the-inspection-facing-records
title: "Provide stable JSON codecs for the inspection-facing records"
kind: exec-plan
created_at: 2026-10-01T00:15:41Z
intention: "intention_01m3tcw9vmeeftdtqj53d1d6nb"
master_plan: "docs/masterplans/7-expose-non-destructive-queue-inspection-through-typed-reads-json-codecs-and-the-pgmq-inspect-sister-package.md"
provenance:
  created_by:
    model: "claude-fable-5-1"
    harness: "claude-code"
    at: 2026-10-01T00:15:41Z
---

# Provide stable JSON codecs for the inspection-facing records

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.
If durable project context changes, update or create ADRs in docs/adr/ in the same change.


## Purpose / Big Picture

pgmq-hs is a Haskell client for pgmq, the PostgreSQL message queue. Its domain records
describe queues, messages, metrics, topic bindings, routing matches, and notification
throttles. Today only the small newtypes (`QueueName`, `MessageId`, `RoutingKey`,
`TopicPattern`, `MessageBody`, `MessageHeaders`) can be turned into JSON; every record that
an inspection surface, a dashboard, or a script would actually want to serialize derives
only `Eq`, `Generic`, and `Show`. Improvement request `IR-2`
(`docs/improvement-requests/provide-json-codecs-for-inspection-facing-records.md`) asks for
`ToJSON` instances on those records, `FromJSON` where decoding is meaningful, and a
documented field-naming policy, so that two consumers putting the same record on a wire
produce the same bytes instead of two incompatible dialects.

After this plan, a user of `pgmq-core`, `pgmq-hasql`, or `pgmq-config` can write
`Data.Aeson.encode` against a `Queue`, `UnvalidatedQueue`, `Message`, `ArchivedMessage`,
`TopicBinding`, `RoutingMatch`, `TopicSendResult`, `NotifyInsertThrottle`, `QueueMetrics`,
or `ReconcileAction` and get a JSON object whose keys are pgmq's own SQL column names in
snake_case, whose optional fields are always present (as `null` when absent), and whose
shape is pinned by golden tests so that any accidental rename breaks the build. A design
note states that these encodings are a published compatibility surface: once released, a
field is never removed or re-typed. The HTTP and WebSocket surface planned in
`docs/plans/29-create-the-pgmq-inspect-sister-package-with-the-http-inspection-surface.md`
serves these exact encodings and writes none of its own for these records.

You can see it working by running the three test suites named in Validation and Acceptance
and by opening the committed golden files, which are the human-readable statement of every
wire shape. For example, after this plan:

```haskell
import Data.Aeson (encode)
import Pgmq.Types (Queue (..), parseQueueName)

-- encode (Queue q createdAt False False) produces
-- {"queue_name":"orders","is_partitioned":false,"is_unlogged":false,"created_at":"2026-08-19T12:34:56.123Z"}
```


## Progress

- [ ] M1: `aeson` added to `pgmq-config`'s library `build-depends`; `tasty-golden` and `bytestring` added to the three test suites; golden directories created and registered in `extra-source-files`
- [ ] M1: hand-written `ToJSON`/`FromJSON` for `Queue`, `UnvalidatedQueue`, `Message` and `ToJSON` for `TopicBinding`, `RoutingMatch`, `TopicSendResult`, `NotifyInsertThrottle` in `pgmq-core/src/Pgmq/Types.hs`
- [ ] M1: `ArchivedMessage` instances added here if the type exists in the working tree (see the integration rule in Context and Orientation); otherwise recorded as deferred to `docs/plans/27-add-non-destructive-peek-archive-and-lookup-reads-across-the-pgmq-layers.md`
- [ ] M1: `ToJSON QueueMetrics` in `pgmq-hasql/src/Pgmq/Hasql/Statements/Types.hs`
- [ ] M1: `ToJSON` for `ReconcileAction`, `QueueType`, `PartitionConfig`, `ObservedQueueType` in `pgmq-config/src/Pgmq/Config/Types.hs`
- [ ] M1: `pgmq-core/test/JsonGoldenSpec.hs` with one golden file per record, a decode test for the validated-versus-lenient queue split, and a round-trip test; wired into `pgmq-core/test/Main.hs`
- [ ] M1: `pgmq-hasql/test/JsonGoldenSpec.hs` with the `QueueMetrics` goldens and the hedgehog round-trip property for `Message`; wired into `pgmq-hasql/test/Main.hs`
- [ ] M1: `pgmq-config/test/JsonGoldenSpec.hs` with one golden per `ReconcileAction` constructor; wired into `pgmq-config/test/Main.hs`
- [ ] M1: all three suites green under `nix develop --command cabal test`; `nix fmt` clean; M1 committed
- [ ] M2: `docs/design/020-json-wire-encodings.md` written; Haddock on every instance points at it
- [ ] M2: capability record created with the handle `okf id next` returned; `docs/capabilities/index.md` and `log.md` updated; `just docs-check` green
- [ ] M2: `Unreleased` sections in `CHANGELOG.md`, `pgmq-core/CHANGELOG.md`, `pgmq-hasql/CHANGELOG.md`, `pgmq-config/CHANGELOG.md`
- [ ] M2: `IR-2` set to `completed` with `completedAt` and `resolution`; improvement-requests log appended; M2 committed


## Surprises & Discoveries

Document unexpected behaviors, bugs, optimizations, or insights discovered during
implementation. Provide concise evidence.

(None yet.)


## Decision Log

- Decision: Every instance is hand-written with `object`/`pairs` and `withObject`; no
  instance is Generic-derived, even where the derived output would coincidentally match.
  Rationale: a derived instance's field names depend on `Options` values that a future edit
  can change silently; a hand-written field list is the policy made executable, and the
  golden tests pin it. Date: 2026-09-30
- Decision: Encoders define both `toJSON` (via `object`) and `toEncoding` (via `pairs`) from
  one shared field list per record. Rationale: `encode` uses `toEncoding`, and `pairs`
  preserves the written key order, so the golden bytes are deterministic; a `Value` built by
  `object` is backed by a hash map whose iteration order is not a promise across `aeson` or
  `hashable` versions, so pinning `encode (toJSON x)` alone would make the goldens fragile.
  Date: 2026-09-30
- Decision: Optional fields are always emitted, as `null` when the Haskell value is
  `Nothing`; decoders accept a missing key as `Nothing` via `.:?`. Rationale: a constant key
  set is what a browser client wants to switch on; lenient decoding costs nothing and tolerates
  a producer that predates a field. Date: 2026-09-30
- Decision: `Queue` and `UnvalidatedQueue` share one wire shape; only decoding differs.
  Rationale: they are the same `pgmq.list_queues()` row; the distinction is whether the name
  passed `parseQueueName`, which belongs in the decoder, not on the wire. Date: 2026-09-30
- Decision: `FromJSON` exists only for `Queue`, `UnvalidatedQueue`, `Message`, and
  `ArchivedMessage`. Rationale: `IR-2` names exactly these as round-trip candidates (fixtures
  and tooling); the metrics, bindings, matches, throttles, and reconciliation report have no
  decode consumer, and an unused decoder is a compatibility promise nobody asked for.
  Date: 2026-09-30
- Decision: `ReconcileAction` encodes as a tagged object with an `"action"` key holding the
  snake_case constructor name; `EnabledNotify`'s `Maybe Int32` encodes as `null` when the
  config declared the pgmq default rather than as the resolved `250`. Rationale: the report
  describes what the reconciler decided, and "declared the default" is a different fact from
  "declared 250"; `defaultThrottleMs` is exported for a consumer who wants the number.
  Date: 2026-09-30
- Decision: The hedgehog round-trip property for `Message` lives in `pgmq-hasql`'s suite, not
  `pgmq-core`'s. Rationale: `pgmq-hasql/test/Generators.hs` already generates JSON bodies and
  headers that PostgreSQL's `jsonb` can store, `pgmq-core`'s suite has no hedgehog dependency,
  and adding one to the family's dependency floor for a single property is not worth it.
  Date: 2026-09-30
- Decision: Golden files are generated once from the real encoder with `--accept`, reviewed
  by eye against the shapes written in this plan, and committed; they are never hand-typed.
  Rationale: `aeson`'s timestamp rendering trims fractional seconds in a specific way, and the
  golden's job is to pin what the encoder emits, not what a human predicted it would emit.
  Date: 2026-09-30


## Outcomes & Retrospective

Summarize outcomes, gaps, and lessons learned at major milestones or at completion.
Compare the result against the original purpose. Before marking the plan complete,
distill durable project context from the Decision Log, Surprises & Discoveries, and
this section into docs/adr/. Keep task-local execution details here.

(To be filled during and after implementation.)


## Context and Orientation

### Terms

**aeson** is the Haskell JSON library (`Data.Aeson`). A `ToJSON` instance turns a value into
JSON; a `FromJSON` instance parses JSON back. `encode :: ToJSON a => a -> ByteString`
produces compact JSON bytes using the instance's `toEncoding` method; `toJSON` produces an
intermediate `Value`. `object :: [Pair] -> Value` builds an object from key-value pairs, and
`pairs :: Series -> Encoding` builds the streaming equivalent; the operator `(.=)` makes a
pair in either world. `withObject` is the standard way to write a parser over an object, and
`(.:)` / `(.:?)` read a required or optional key.

**A golden test** compares a program's output against a file committed to the repository
("the golden file"). The `tasty-golden` library provides
`goldenVsString :: TestName -> FilePath -> IO ByteString -> TestTree`; when the output
differs from the file the test fails and prints a diff, and running the suite with
`--accept` overwrites the file with the new output. Golden tests are how this plan pins every
encoding: a renamed field changes the bytes and fails the build.

**snake_case** means lowercase words joined by underscores (`queue_name`). pgmq's SQL
columns are already snake_case, so the policy below is "use the column name".

**PVP** is the Haskell Package Versioning Policy. Adding a type-class instance is a change
that needs at least a minor version bump (the third component), because a downstream package
that defined its own orphan instance for the same type would stop compiling. No plan in this
initiative bumps a version; the family's next lockstep release does.

**OKF** is the Open Knowledge Format: directories of Markdown files with YAML frontmatter,
validated by the `okf` command against a profile. This repository keeps improvement requests
in `docs/improvement-requests/` and capability records in `docs/capabilities/`. A capability
record is one Markdown file per adoptable capability carrying a stable `CAP-N` handle.

### The repository

pgmq-hs is a multi-package Cabal project. `cabal.project` lists `pgmq-core`, `pgmq-hasql`,
`pgmq-effectful`, `pgmq-migration`, `pgmq-config`, and `pgmq-bench`. The toolchain comes from
`nix develop` (GHC 9.12, cabal, a PostgreSQL binary, HLS); run every `cabal` command through
it. `nix fmt` runs fourmolu and cabal-gild; the pre-commit hook rejects unformatted files, so
run `nix fmt` before every commit. One environment quirk: GNU `sed` shadows BSD `sed` on
`PATH`, so an in-place edit is `sed -i -e 's/a/b/' file`, never `sed -i '' ...`.

The layering that matters here is the type layer. `pgmq-core/src/Pgmq/Types.hs` holds the
domain records and depends only on `aeson`, `base`, `template-haskell`, `text`, and `time`.
`pgmq-hasql/src/Pgmq/Hasql/Statements/Types.hs` holds the argument records for SQL statements
and, among them, `QueueMetrics`, which is a result record that happens to live there for
historical reasons; `pgmq-hasql` depends on `aeson` and its `Pgmq.Hasql.Prelude` re-exports
`FromJSON`, `ToJSON`, `toJSON`, `toEncoding`, and friends but not `object`, `pairs`, or
`(.=)`. `pgmq-config/src/Pgmq/Config/Types.hs` holds the declarative queue vocabulary and the
`ReconcileAction` report; `pgmq-config`'s library does **not** depend on `aeson` today (only
its test suite does), so this plan adds it.

### The records today

Only the newtypes carry JSON instances. In `pgmq-core/src/Pgmq/Types.hs`:

```haskell
newtype MessageBody = MessageBody {unMessageBody :: Value}
  deriving newtype (Eq, Ord, FromJSON, ToJSON)

newtype MessageHeaders = MessageHeaders {unMessageHeaders :: Value}
  deriving newtype (Eq, Ord, FromJSON, ToJSON)

newtype MessageId = MessageId {unMessageId :: Int64}
  deriving newtype (Eq, Ord, FromJSON, ToJSON)

newtype QueueName = QueueName Text
  deriving newtype (Eq, Ord, ToJSON)
-- FromJSON QueueName is hand-written and validates through parseQueueName.

newtype RoutingKey = RoutingKey Text
  deriving newtype (Eq, Ord, FromJSON, ToJSON)

newtype TopicPattern = TopicPattern Text
  deriving newtype (Eq, Ord, FromJSON, ToJSON)
```

These stay exactly as they are (`IR-2` acceptance 3). The records this plan gives instances
to, quoted from the same file:

```haskell
data Queue = Queue
  { name :: !QueueName,
    createdAt :: !UTCTime,
    isPartitioned :: !Bool,
    isUnlogged :: !Bool
  }
  deriving stock (Eq, Generic, Show)

data UnvalidatedQueue = UnvalidatedQueue
  { unvalidatedName :: !Text,
    unvalidatedCreatedAt :: !UTCTime,
    unvalidatedIsPartitioned :: !Bool,
    unvalidatedIsUnlogged :: !Bool
  }
  deriving stock (Eq, Generic, Show)

data Message = Message
  { messageId :: !MessageId,
    visibilityTime :: !UTCTime,
    enqueuedAt :: !UTCTime,
    lastReadAt :: !(Maybe UTCTime),
    readCount :: !Int64,
    body :: !MessageBody,
    headers :: !(Maybe Value)
  }
  deriving stock (Eq, Generic, Show)

data TopicBinding = TopicBinding
  { bindingPattern :: !TopicPattern,
    bindingQueueName :: !Text,
    bindingBoundAt :: !UTCTime,
    bindingCompiledRegex :: !Text
  }
  deriving stock (Eq, Generic, Show)

data RoutingMatch = RoutingMatch
  { matchPattern :: !TopicPattern,
    matchQueueName :: !Text,
    matchCompiledRegex :: !Text
  }
  deriving stock (Eq, Generic, Show)

data TopicSendResult = TopicSendResult
  { sentToQueue :: !Text,
    sentMessageId :: !MessageId
  }
  deriving stock (Eq, Generic, Show)

data NotifyInsertThrottle = NotifyInsertThrottle
  { throttleQueueName :: !Text,
    throttleIntervalMs :: !Int32,
    throttleLastNotifiedAt :: !UTCTime
  }
  deriving stock (Eq, Generic, Show)
```

`pgmq-core` does not enable `DuplicateRecordFields`, which is why its field names carry
prefixes (`unvalidatedName`, `bindingPattern`); plain selector functions work there.

In `pgmq-hasql/src/Pgmq/Hasql/Statements/Types.hs`, which does enable
`DuplicateRecordFields`:

```haskell
data QueueMetrics = QueueMetrics
  { queueName :: !Text,
    queueLength :: !Int64,
    newestMsgAgeSec :: !(Maybe Int32),
    oldestMsgAgeSec :: !(Maybe Int32),
    totalMessages :: !Int64,
    scrapeTime :: !UTCTime,
    queueVisibleLength :: !Int64,
    defaultPartitionLength :: !(Maybe Int64)
  }
  deriving stock (Generic, Show)
```

Because `queueName` is also a field of a dozen other records in that module, the selector
function `queueName` is ambiguous there under GHC 9.12; the instance below pattern-matches
positionally instead. `queueLength` is the total number of rows in the queue table and
`queueVisibleLength` is the subset whose visibility timeout has expired, meaning they are
available to a reader; `docs/design/002-queue-visible-length.md` explains the two numbers.
`defaultPartitionLength` is `Nothing` on PGMQ 1.12 and for non-partitioned queues on 1.13,
and `docs/adr/pgmq-1.12-1.13-compatibility.md` is explicit that `Nothing` must never be read
as zero.

In `pgmq-config/src/Pgmq/Config/Types.hs` (also `DuplicateRecordFields`, with
`OverloadedLabels`, `generic-lens`, and `lens` available for `^. #field` access):

```haskell
data QueueType
  = StandardQueue
  | UnloggedQueue
  | PartitionedQueue !PartitionConfig

data PartitionConfig = PartitionConfig
  { partitionInterval :: !Text,
    retentionInterval :: !Text,
    premake :: !(Maybe Int32)
  }

data ObservedQueueType = ObservedStandard | ObservedUnlogged | ObservedPartitioned

data ReconcileAction
  = CreatedQueue !QueueName !QueueType
  | EnabledNotify !QueueName !(Maybe Int32)
  | CreatedFifoIndex !QueueName
  | BoundTopic !QueueName !TopicPattern
  | SkippedQueue !QueueName
  | SkippedNotify !QueueName
  | SkippedFifoIndex !QueueName
  | SkippedTopicBinding !QueueName !TopicPattern
  | UpdatedNotifyThrottle !QueueName !Int32 !Int32   -- queue, observed ms, declared ms
  | DetectedQueueTypeDrift !QueueName !QueueType !ObservedQueueType
```

Those ten constructors are what the tree holds today. MasterPlan 6's
`docs/plans/24-report-name-collisions-and-unsupported-notifications-instead-of-acting-on-them.md`
plans two more (`DetectedQueueNameCollision` and `UnsupportedNotifyOnPartitionedQueue`). If
they exist when you implement this plan, encode them by the same rule (snake_case constructor
name under `"action"`, snake_case field names) and add a golden for each; if they land later,
the plan that adds them owns their encoding and golden, following
`docs/design/020-json-wire-encodings.md`. The golden suite covers whatever constructors are
in the tree; a constructor without a golden is a defect.

### The integration rule for `ArchivedMessage`

`docs/plans/27-add-non-destructive-peek-archive-and-lookup-reads-across-the-pgmq-layers.md`
adds a new record to `pgmq-core/src/Pgmq/Types.hs`:

```haskell
data ArchivedMessage = ArchivedMessage
  { archivedMessage :: !Message,
    archivedAt :: !UTCTime
  }
  deriving stock (Eq, Generic, Show)
```

Its JSON shape is the `Message` object with one more key, `archived_at`, flattened into the
same object (not nested). The two plans may run in either order. The rule, which both plans
carry: **if `ArchivedMessage` is in `Pgmq.Types` when you implement this plan, add its
`ToJSON`/`FromJSON` instances and golden file here; if it is not, do nothing for it and
record that in Progress, and plan 27 adds the instances following this plan's policy.** The
exact instance plan 27's implementer should copy is in Concrete Steps below, so whichever
plan runs second has no judgment call to make.

### Test harness

`pgmq-core/test/Main.hs` runs a single `testGroup "pgmq-core" [QueueNameSpec.tests]` with
`tasty` and `tasty-hunit` and needs no database. `pgmq-hasql/test/Main.hs` starts one
ephemeral PostgreSQL through `EphemeralDb.withPgmqDb` and passes a pool to every spec; the
golden spec added here takes no pool because it never touches the database.
`pgmq-hasql/test/Generators.hs` exports hedgehog generators `genMessageBody`,
`genMessageHeaders`, and `genJsonValue` whose output PostgreSQL `jsonb` accepts (no NUL
characters), used by `RoundTripSpec`. `pgmq-config/test/Main.hs` likewise starts a database
for its specs; the golden spec takes no pool.

`cabal test` runs each test executable with the package directory as its working directory,
so a golden path like `test/golden/queue.json` resolves inside `pgmq-core/`. The Nix build
(`nix/haskell-overlay.nix`) wraps `pgmq-hasql` and `pgmq-config` in `dontCheck`, but
`pgmq-core` is built **with** its tests under `nix flake check`
(`pgmq-core = doJailbreak (final.callCabal2nix "pgmq-core" ../pgmq-core { })`), so
`tasty-golden` must resolve in the `ghc9124` package set. It does: version 2.3.6 is present
and not marked broken. Golden files must also be listed in `extra-source-files` so an sdist
and the Nix source copy carry them.

### ADRs and design notes

`docs/adr/queue-inspection-surface-boundary-and-wire-contract.md` records, for the whole
initiative, that JSON encodings of domain records are a published compatibility surface with
pgmq's snake_case column names, hand-written instances, explicit nulls, and golden pins. This
plan is that decision's implementation for the records; the design note it writes is where
the rule is stated for implementers. `docs/adr/pgmq-1.12-1.13-compatibility.md` fixes the
meaning of `defaultPartitionLength`'s `Nothing`. `docs/adr/haskell-dependency-bounds-and-nix-pin-policy.md`
governs the `aeson ^>=2.2` bound this plan adds to `pgmq-config` (the same bound the other
packages already declare and the Nix pin already exercises). The design notes that define
the fields' meanings are `docs/design/002-queue-visible-length.md` (total versus visible
depth) and `docs/design/016-queue-name-validation.md` (why a validated `Queue` and a lenient
`UnvalidatedQueue` both exist, and why decoding one differs from the other).

Across repositories, `mori://shinzui/keiro-ui/okf/adrs/concepts/ADR-5` records that the
keiro runtime console's hand-written clients are regenerated from machine-readable specs once
they exist; stable encodings here are what makes that regeneration meaningful. The
cross-project wire conventions are `mori://shinzui/keiro-ui`, path
`docs/architecture/inspection-api-conventions.md` (artifact-level URI pending); their field
rule is "new wire fields are snake_case", which this plan satisfies by construction.


## Plan of Work

### Milestone 1: the instances and the golden tests

Scope: give every listed record its instances, in `pgmq-core`, `pgmq-hasql`, and
`pgmq-config`, and pin every encoding with a committed golden file. At the end of this
milestone the three test suites pass, each golden file exists and reads as the shape this
plan describes, a decode test proves that a mixed-case name fails as `Queue` and succeeds as
`UnvalidatedQueue`, and a hedgehog property proves `Message` survives `encode` then `decode`
for arbitrary bodies and headers. Commands: the three `cabal test` invocations in Concrete
Steps. Acceptance: `IR-2` items 1, 3, and 4.

Start with the build files. Add `aeson ^>=2.2` to the `library` `build-depends` of
`pgmq-config/pgmq-config.cabal` (the test suite already has it). Add `tasty-golden ^>=2.3`
and `bytestring` to the `build-depends` of `pgmq-core-test`, `pgmq-hasql-test`, and
`pgmq-config-test`; `pgmq-hasql-test` and `pgmq-config-test` already have `bytestring` or
`aeson` where noted in the cabal files, so add only what is missing. Add `JsonGoldenSpec` to
each suite's `other-modules`. Add `extra-source-files: test/golden/*.json` to
`pgmq-core.cabal`, and extend the existing `extra-source-files` lines of `pgmq-hasql.cabal`
and `pgmq-config.cabal` (they currently list only the 1.12 fixture) with the same pattern.
`cabal-gild` reformats these files under `nix fmt`; let it.

Then write the instances, package by package, in the order given in Concrete Steps. In
`pgmq-core/src/Pgmq/Types.hs` extend the aeson import to bring in `ToJSON (..)`, `object`,
`pairs`, `withObject`, `(.:)`, `(.:?)`, `(.=)`, `Encoding`, and `Pair`, add one private
helper that turns a field list into an `Encoding`, and then add, directly below each record,
its field-list function and its instances. In `pgmq-hasql/src/Pgmq/Hasql/Statements/Types.hs`
import the same handful of aeson names (the module's prelude supplies `ToJSON` and
`toEncoding` already) and add the `QueueMetrics` instance with a positional pattern. In
`pgmq-config/src/Pgmq/Config/Types.hs` import aeson, add the four instances, and export
nothing new (instances are not exported by name). Put a Haddock comment on every instance
that names the design note; the note does not exist until Milestone 2, so write the comment
now with the path it will have.

Then write the three golden specs. Each one builds fixed sample values (one `UTCTime` with
a fractional second, one with none, every `Maybe` exercised in both states), calls
`encode`, appends a newline, and hands the bytes to `goldenVsString` with a path under
`test/golden/`. On the first run the files do not exist and `tasty-golden` reports them as
missing; run the suite once with `--accept` to create them, open each file and compare it to
the shape written in Concrete Steps, and only then commit. `pgmq-core`'s spec also contains
a decode test (`{"queue_name":"MyQueue",…}` fails as `Queue` because `parseQueueName` rejects
uppercase, and succeeds as `UnvalidatedQueue`), a round-trip test for each record that has
`FromJSON`, and a null-tolerance test (`"last_read_at": null` and a missing `last_read_at`
both decode to `Nothing`). `pgmq-hasql`'s spec adds the hedgehog property. `pgmq-config`'s
spec has one golden per constructor.

Finish by running `nix fmt`, the three suites, and committing.

### Milestone 2: the policy, the records, and the request closure

Scope: state the policy where implementers will see it, record the capability, write the
changelogs, and close `IR-2`. At the end of this milestone `docs/design/020-json-wire-encodings.md`
exists, every instance's Haddock points at it, a capability record with a freshly allocated
handle describes the encodings with the golden specs as evidence, `just docs-check` passes,
four changelogs carry an `Unreleased` section, and the request's frontmatter says
`status: completed`. Commands: `just docs-check` and the `okf` invocations in Concrete Steps.
Acceptance: `IR-2` item 2.

Write the design note first, following the house style of
`docs/design/017-transient-error-classification.md` (Status, the contract, why, where it is
enforced, related documents). It must state: the key naming rule (snake_case; where a pgmq
SQL column exists, the column's name); the freeze rule (a shipped encoding never loses or
re-types a field, additions are allowed, anything incompatible is a new field or a new
shape); optional fields as explicit `null` on encode and `.:?` on decode; timestamps as
aeson's default ISO-8601 UTC rendering with fractional seconds trimmed; that instances are
hand-written and why; which records decode and why the rest do not; the `Queue` versus
`UnvalidatedQueue` split; the `ReconcileAction` tagging rule and the meaning of `null` in
`enabled_notify`; the procedure for adding a field (add it to the field list, regenerate and
review the golden, note it in the changelog); and that the wire types of the `pgmq-inspect`
package follow the same rules.

Allocate the capability handle with `okf id next` (never type a number from memory; the
parallel plan 27 may have taken the next one), write the record in the style of
`docs/capabilities/message-queue-client.md` with `since: "unreleased"`, add its row to
`docs/capabilities/index.md`, and append to `docs/capabilities/log.md` under today's date.
Write the four `Unreleased` changelog sections. Close the request: set `status: completed`,
add `completedAt` (the UTC time the suites went green, RFC 3339) and a one-sentence
`resolution`, and append to the bundle log with `okf log add`. Run `just docs-check`,
`nix fmt`, and commit.


## Concrete Steps

All commands run from the repository root
`/Users/shinzui/Keikaku/bokuno/libraries/pgmq-hs-project/pgmq-hs` unless stated otherwise.
Prefix every `cabal` command with `nix develop --command`.

### Step 1: build files

In `pgmq-config/pgmq-config.cabal`, inside the `library` stanza's `build-depends`, add:

```cabal
    aeson ^>=2.2,
```

In `pgmq-core/pgmq-core.cabal` add a top-level line and extend the test suite:

```cabal
extra-source-files: test/golden/*.json
```

```cabal
test-suite pgmq-core-test
  other-modules:
    JsonGoldenSpec
    QueueNameSpec
  build-depends:
    aeson ^>=2.2,
    base >=4.18 && <5,
    bytestring >=0.11 && <0.13,
    pgmq-core >=0.6 && <0.7,
    tasty ^>=1.5,
    tasty-golden ^>=2.3,
    tasty-hunit ^>=0.10,
    text ^>=2.1,
    time ^>=1.14,
```

In `pgmq-hasql/pgmq-hasql.cabal` change `extra-source-files` to list both patterns and add
`JsonGoldenSpec` to `other-modules` and `tasty-golden ^>=2.3` plus `bytestring` to the test
suite's `build-depends`:

```cabal
extra-source-files:
  test/fixtures/pgmq-1.12.0.sql
  test/golden/*.json
```

In `pgmq-config/pgmq-config.cabal` do the same for its test suite (it already depends on
`aeson` and `bytestring`; add `tasty-golden ^>=2.3` and `time` is already there).

Create the directories:

```bash
mkdir -p pgmq-core/test/golden pgmq-hasql/test/golden pgmq-config/test/golden
```

### Step 2: `pgmq-core/src/Pgmq/Types.hs`

Replace the aeson import line with:

```haskell
import Data.Aeson (Encoding, FromJSON (..), Pair, ToJSON (..), Value, object, pairs, withObject, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
```

Add `ArchivedMessage (..)` to the export list only if the type exists (plan 27 exports it).
Add one private helper near the top of the definitions:

```haskell
-- | Encode a field list in its written order. 'pairs' preserves order, which
-- is what makes the golden tests in @pgmq-core/test/JsonGoldenSpec.hs@
-- deterministic; an 'object' alone would be a hash map with no order promise.
encodeFields :: [Pair] -> Encoding
encodeFields = pairs . foldMap (uncurry (.=))
```

Directly below `data Queue`:

```haskell
-- | Wire keys are the @pgmq.list_queues()@ column names. The encoding is a
-- published compatibility surface; see @docs/design/020-json-wire-encodings.md@.
-- 'Queue' and 'UnvalidatedQueue' encode identically; only decoding differs.
queueFields :: Queue -> [Pair]
queueFields q =
  [ "queue_name" .= name q,
    "is_partitioned" .= isPartitioned q,
    "is_unlogged" .= isUnlogged q,
    "created_at" .= createdAt q
  ]

instance ToJSON Queue where
  toJSON = object . queueFields
  toEncoding = encodeFields . queueFields

-- | Decoding validates @queue_name@ through 'parseQueueName' (the 'FromJSON'
-- 'QueueName' instance), so a foreign or mixed-case name fails here and must
-- be decoded as 'UnvalidatedQueue' instead.
instance FromJSON Queue where
  parseJSON = withObject "Queue" $ \o ->
    Queue
      <$> o .: "queue_name"
      <*> o .: "created_at"
      <*> o .: "is_partitioned"
      <*> o .: "is_unlogged"
```

Directly below `data UnvalidatedQueue`:

```haskell
-- | Same wire shape as 'Queue'; see @docs/design/020-json-wire-encodings.md@.
unvalidatedQueueFields :: UnvalidatedQueue -> [Pair]
unvalidatedQueueFields q =
  [ "queue_name" .= unvalidatedName q,
    "is_partitioned" .= unvalidatedIsPartitioned q,
    "is_unlogged" .= unvalidatedIsUnlogged q,
    "created_at" .= unvalidatedCreatedAt q
  ]

instance ToJSON UnvalidatedQueue where
  toJSON = object . unvalidatedQueueFields
  toEncoding = encodeFields . unvalidatedQueueFields

-- | Decodes any server-accepted name; nothing is validated.
instance FromJSON UnvalidatedQueue where
  parseJSON = withObject "UnvalidatedQueue" $ \o ->
    UnvalidatedQueue
      <$> o .: "queue_name"
      <*> o .: "created_at"
      <*> o .: "is_partitioned"
      <*> o .: "is_unlogged"
```

Directly below `data Message`:

```haskell
-- | Wire keys are the @pgmq.message_record@ column names: @msg_id@, @read_ct@,
-- @enqueued_at@, @last_read_at@, @vt@, @message@, @headers@. Optional columns
-- are always present and encode as @null@. The encoding is a published
-- compatibility surface; see @docs/design/020-json-wire-encodings.md@.
messageFields :: Message -> [Pair]
messageFields m =
  [ "msg_id" .= messageId m,
    "read_ct" .= readCount m,
    "enqueued_at" .= enqueuedAt m,
    "last_read_at" .= lastReadAt m,
    "vt" .= visibilityTime m,
    "message" .= body m,
    "headers" .= headers m
  ]

instance ToJSON Message where
  toJSON = object . messageFields
  toEncoding = encodeFields . messageFields

-- | A missing @last_read_at@ or @headers@ key decodes as 'Nothing', as does
-- an explicit @null@.
instance FromJSON Message where
  parseJSON = withObject "Message" $ \o ->
    Message
      <$> o .: "msg_id"
      <*> o .: "vt"
      <*> o .: "enqueued_at"
      <*> o .:? "last_read_at"
      <*> o .: "read_ct"
      <*> o .: "message"
      <*> o .:? "headers"
```

Note the constructor's field order is `messageId, visibilityTime, enqueuedAt, lastReadAt,
readCount, body, headers`; the parser above follows it. The wire order follows the SQL
column order instead; the two are deliberately different and the golden pins the wire one.

If `ArchivedMessage` exists, directly below it (this is the exact text plan 27's implementer
copies when they run second):

```haskell
-- | The 'Message' encoding with one more key, @archived_at@, flattened into
-- the same object. See @docs/design/020-json-wire-encodings.md@.
archivedMessageFields :: ArchivedMessage -> [Pair]
archivedMessageFields a =
  messageFields (archivedMessage a) <> ["archived_at" .= archivedAt a]

instance ToJSON ArchivedMessage where
  toJSON = object . archivedMessageFields
  toEncoding = encodeFields . archivedMessageFields

instance FromJSON ArchivedMessage where
  parseJSON = withObject "ArchivedMessage" $ \o ->
    ArchivedMessage
      <$> parseJSON (Aeson.Object o)
      <*> o .: "archived_at"
```

Directly below `data TopicBinding`, `data RoutingMatch`, `data TopicSendResult`, and
`data NotifyInsertThrottle` respectively (encode-only; no `FromJSON`):

```haskell
-- | Wire keys are the @pgmq.list_topic_bindings()@ columns. Encode-only; see
-- @docs/design/020-json-wire-encodings.md@.
topicBindingFields :: TopicBinding -> [Pair]
topicBindingFields b =
  [ "pattern" .= bindingPattern b,
    "queue_name" .= bindingQueueName b,
    "bound_at" .= bindingBoundAt b,
    "compiled_regex" .= bindingCompiledRegex b
  ]

instance ToJSON TopicBinding where
  toJSON = object . topicBindingFields
  toEncoding = encodeFields . topicBindingFields

-- | Wire keys are the @pgmq.test_routing()@ columns. Encode-only.
routingMatchFields :: RoutingMatch -> [Pair]
routingMatchFields m =
  [ "pattern" .= matchPattern m,
    "queue_name" .= matchQueueName m,
    "compiled_regex" .= matchCompiledRegex m
  ]

instance ToJSON RoutingMatch where
  toJSON = object . routingMatchFields
  toEncoding = encodeFields . routingMatchFields

-- | Wire keys are the @pgmq.send_batch_topic()@ result columns. Encode-only.
topicSendResultFields :: TopicSendResult -> [Pair]
topicSendResultFields r =
  [ "queue_name" .= sentToQueue r,
    "msg_id" .= sentMessageId r
  ]

instance ToJSON TopicSendResult where
  toJSON = object . topicSendResultFields
  toEncoding = encodeFields . topicSendResultFields

-- | Wire keys are the @pgmq.list_notify_insert_throttles()@ columns. Encode-only.
notifyInsertThrottleFields :: NotifyInsertThrottle -> [Pair]
notifyInsertThrottleFields t =
  [ "queue_name" .= throttleQueueName t,
    "throttle_interval_ms" .= throttleIntervalMs t,
    "last_notified_at" .= throttleLastNotifiedAt t
  ]

instance ToJSON NotifyInsertThrottle where
  toJSON = object . notifyInsertThrottleFields
  toEncoding = encodeFields . notifyInsertThrottleFields
```

`pgmq-core` compiles with `-Wall -Wmissing-export-lists`; the field-list functions are
private and used, so no warning arises. Check with:

```bash
nix develop --command cabal build pgmq-core
```

### Step 3: `pgmq-hasql/src/Pgmq/Hasql/Statements/Types.hs`

Add to the imports (the module's `Pgmq.Hasql.Prelude` already brings `ToJSON`, `toJSON`,
`toEncoding`, `Text`, `Int32`, `Int64`, `UTCTime`):

```haskell
import Data.Aeson (Encoding, Pair, object, pairs, (.=))
```

Directly below `data QueueMetrics`:

```haskell
-- | Wire keys are the @pgmq.metrics_result@ columns. Two of them are easy to
-- confuse and must not be: @queue_length@ is the total number of rows in the
-- queue table, and @queue_visible_length@ is the subset whose visibility
-- timeout has expired and are therefore available to a reader (see
-- @docs/design/002-queue-visible-length.md@). @default_partition_length@ is
-- @null@ when the metric is unavailable (PGMQ 1.12) or inapplicable (a
-- non-partitioned queue); @null@ never means zero. Encode-only. The encoding is
-- a published compatibility surface; see
-- @docs/design/020-json-wire-encodings.md@.
--
-- The pattern is positional because @queueName@ is a duplicate record field in
-- this module and its selector is ambiguous under GHC 9.12.
queueMetricsFields :: QueueMetrics -> [Pair]
queueMetricsFields (QueueMetrics qn len newest oldest total scrape visible dpl) =
  [ "queue_name" .= qn,
    "queue_length" .= len,
    "newest_msg_age_sec" .= newest,
    "oldest_msg_age_sec" .= oldest,
    "total_messages" .= total,
    "scrape_time" .= scrape,
    "queue_visible_length" .= visible,
    "default_partition_length" .= dpl
  ]

instance ToJSON QueueMetrics where
  toJSON = object . queueMetricsFields
  toEncoding = pairs . foldMap (uncurry (.=)) . queueMetricsFields
```

The positional pattern must list exactly eight fields in declaration order; if a later plan
adds a field to `QueueMetrics`, the pattern fails to compile, which is the intended reminder
to extend the encoding and the golden.

### Step 4: `pgmq-config/src/Pgmq/Config/Types.hs`

Add imports:

```haskell
import Control.Lens ((%~), (&), (^.))
import Data.Aeson (Encoding, Pair, ToJSON (..), object, pairs, (.=))
```

(`Control.Lens` is already imported for `(%~)` and `(&)`; extend that line.) Then add, below
the `ReconcileAction` definition:

```haskell
encodeFields :: [Pair] -> Encoding
encodeFields = pairs . foldMap (uncurry (.=))

-- | Encodes as @{"type":"standard"}@, @{"type":"unlogged"}@, or
-- @{"type":"partitioned","partition_interval":…,"retention_interval":…,"premake":…}@.
-- See @docs/design/020-json-wire-encodings.md@.
queueTypeFields :: QueueType -> [Pair]
queueTypeFields qt = case qt of
  StandardQueue -> ["type" .= ("standard" :: Text)]
  UnloggedQueue -> ["type" .= ("unlogged" :: Text)]
  PartitionedQueue pc -> ("type" .= ("partitioned" :: Text)) : partitionConfigFields pc

instance ToJSON QueueType where
  toJSON = object . queueTypeFields
  toEncoding = encodeFields . queueTypeFields

-- | @premake@ is @null@ when creation uses the server default. See
-- @docs/design/020-json-wire-encodings.md@.
partitionConfigFields :: PartitionConfig -> [Pair]
partitionConfigFields pc =
  [ "partition_interval" .= (pc ^. #partitionInterval),
    "retention_interval" .= (pc ^. #retentionInterval),
    "premake" .= (pc ^. #premake)
  ]

instance ToJSON PartitionConfig where
  toJSON = object . partitionConfigFields
  toEncoding = encodeFields . partitionConfigFields

-- | Encodes as the bare strings @"standard"@, @"unlogged"@, @"partitioned"@.
observedQueueTypeText :: ObservedQueueType -> Text
observedQueueTypeText ot = case ot of
  ObservedStandard -> "standard"
  ObservedUnlogged -> "unlogged"
  ObservedPartitioned -> "partitioned"

instance ToJSON ObservedQueueType where
  toJSON = toJSON . observedQueueTypeText
  toEncoding = toEncoding . observedQueueTypeText

-- | A tagged object: @"action"@ holds the snake_case constructor name and the
-- remaining keys name the fields. @enabled_notify@'s @throttle_interval_ms@
-- is @null@ when the config declared the pgmq default ('defaultThrottleMs')
-- rather than a number; that is a different fact from declaring @250@.
-- Encode-only. The encoding is a published compatibility surface; see
-- @docs/design/020-json-wire-encodings.md@. Every constructor added later
-- must be encoded by the same rule and pinned in
-- @pgmq-config/test/JsonGoldenSpec.hs@.
reconcileActionFields :: ReconcileAction -> [Pair]
reconcileActionFields action = case action of
  CreatedQueue q t ->
    ["action" .= tag "created_queue", "queue" .= q, "queue_type" .= t]
  EnabledNotify q ms ->
    ["action" .= tag "enabled_notify", "queue" .= q, "throttle_interval_ms" .= ms]
  CreatedFifoIndex q ->
    ["action" .= tag "created_fifo_index", "queue" .= q]
  BoundTopic q p ->
    ["action" .= tag "bound_topic", "queue" .= q, "pattern" .= p]
  SkippedQueue q ->
    ["action" .= tag "skipped_queue", "queue" .= q]
  SkippedNotify q ->
    ["action" .= tag "skipped_notify", "queue" .= q]
  SkippedFifoIndex q ->
    ["action" .= tag "skipped_fifo_index", "queue" .= q]
  SkippedTopicBinding q p ->
    ["action" .= tag "skipped_topic_binding", "queue" .= q, "pattern" .= p]
  UpdatedNotifyThrottle q observed declared ->
    [ "action" .= tag "updated_notify_throttle",
      "queue" .= q,
      "observed_throttle_interval_ms" .= observed,
      "declared_throttle_interval_ms" .= declared
    ]
  DetectedQueueTypeDrift q declared observed ->
    [ "action" .= tag "detected_queue_type_drift",
      "queue" .= q,
      "declared" .= declared,
      "observed" .= observed
    ]
  where
    tag :: Text -> Text
    tag = id

instance ToJSON ReconcileAction where
  toJSON = object . reconcileActionFields
  toEncoding = encodeFields . reconcileActionFields
```

If `DetectedQueueNameCollision` or `UnsupportedNotifyOnPartitionedQueue` exist in the tree,
add a case for each with `"action"` set to `detected_queue_name_collision` (fields `queue`
and `colliding_names`) and `unsupported_notify_on_partitioned_queue` (field `queue`), or
whatever their actual fields are; the compiler's incomplete-pattern warning under `-Wall`
tells you if a constructor is missing. `pgmq-config` does not enable `LambdaCase`, hence the
explicit `case`. Build:

```bash
nix develop --command cabal build pgmq-hasql pgmq-config
```

### Step 5: `pgmq-core/test/JsonGoldenSpec.hs`

```haskell
{-# LANGUAGE OverloadedStrings #-}

-- | Pins the JSON wire shape of every pgmq-core record that carries an aeson
-- instance. The golden files under @test/golden/@ are the human-readable
-- statement of the contract in @docs/design/020-json-wire-encodings.md@: a
-- renamed or re-typed field changes the bytes and fails here. Regenerate
-- with @--accept@ only after deciding, in the changelog, that the change is
-- additive.
module JsonGoldenSpec (tests) where

import Data.Aeson (Value (..), decode, eitherDecode, encode, object, (.=))
import Data.ByteString.Lazy qualified as LBS
import Data.Either (isLeft)
import Data.Time (UTCTime (..), fromGregorian)
import Pgmq.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "JSON wire encodings"
    [ goldens,
      decoding
    ]

-- | 2026-08-19T12:34:56.123Z: a fractional second, so the golden shows how
-- aeson renders one (trailing zeros trimmed).
fractionalTime :: UTCTime
fractionalTime = UTCTime (fromGregorian 2026 8 19) 45296.123

-- | 2026-08-19T12:35:00Z: no fraction, so the golden shows the plain form.
wholeTime :: UTCTime
wholeTime = UTCTime (fromGregorian 2026 8 19) 45300

sampleQueueName :: QueueName
sampleQueueName = either (error . show) id (parseQueueName "orders")

sampleQueue :: Queue
sampleQueue = Queue sampleQueueName fractionalTime False False

sampleUnvalidatedQueue :: UnvalidatedQueue
sampleUnvalidatedQueue = UnvalidatedQueue "MyQueue" wholeTime True False

sampleMessage :: Message
sampleMessage =
  Message
    { messageId = MessageId 42,
      visibilityTime = fractionalTime,
      enqueuedAt = wholeTime,
      lastReadAt = Nothing,
      readCount = 0,
      body = MessageBody (object ["order_id" .= (7 :: Int)]),
      headers = Nothing
    }

sampleReadMessage :: Message
sampleReadMessage =
  sampleMessage
    { lastReadAt = Just fractionalTime,
      readCount = 3,
      headers = Just (object ["x-pgmq-group" .= ("g1" :: String)])
    }

samplePattern :: TopicPattern
samplePattern = either (error . show) id (parseTopicPattern "orders.*")

goldens :: TestTree
goldens =
  testGroup
    "goldens"
    [ golden "queue" sampleQueue,
      golden "unvalidated_queue" sampleUnvalidatedQueue,
      golden "message_unread" sampleMessage,
      golden "message_read" sampleReadMessage,
      golden "topic_binding" (TopicBinding samplePattern "orders" wholeTime "^orders\\.[^.]+$"),
      golden "routing_match" (RoutingMatch samplePattern "orders" "^orders\\.[^.]+$"),
      golden "topic_send_result" (TopicSendResult "orders" (MessageId 42)),
      golden "notify_insert_throttle" (NotifyInsertThrottle "orders" 250 wholeTime)
    ]
  where
    golden name value =
      goldenVsString name ("test/golden/" <> name <> ".json") (pure (encode value <> "\n"))

decoding :: TestTree
decoding =
  testGroup
    "decoding"
    [ testCase "a mixed-case name fails as Queue and succeeds as UnvalidatedQueue" $ do
        let bytes = encode sampleUnvalidatedQueue
        assertBool "Queue must reject MyQueue" (isLeft (eitherDecode bytes :: Either String Queue))
        assertEqual "UnvalidatedQueue accepts it" (Just sampleUnvalidatedQueue) (decode bytes),
      testCase "Queue round-trips" $
        assertEqual "round-trip" (Just sampleQueue) (decode (encode sampleQueue)),
      testCase "Message round-trips with and without optional fields" $ do
        assertEqual "unread" (Just sampleMessage) (decode (encode sampleMessage))
        assertEqual "read" (Just sampleReadMessage) (decode (encode sampleReadMessage)),
      testCase "a missing last_read_at decodes as Nothing" $ do
        let bytes = encode (object ["msg_id" .= (1 :: Int), "read_ct" .= (0 :: Int), "enqueued_at" .= wholeTime, "vt" .= wholeTime, "message" .= Null])
        case decode bytes of
          Just m -> assertEqual "Nothing" Nothing (lastReadAt m)
          Nothing -> assertFailure ("Message did not decode: " <> show (LBS.unpack bytes))
    ]
```

If `ArchivedMessage` exists, add `golden "archived_message" (ArchivedMessage sampleReadMessage wholeTime)`
to `goldens` and an `ArchivedMessage` round-trip case to `decoding`. Wire the module into
`pgmq-core/test/Main.hs`:

```haskell
import JsonGoldenSpec qualified
import QueueNameSpec qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "pgmq-core"
      [ QueueNameSpec.tests,
        JsonGoldenSpec.tests
      ]
```

Create the goldens, then inspect them:

```bash
nix develop --command cabal test pgmq-core:pgmq-core-test --test-options=--accept
cat pgmq-core/test/golden/*.json
```

Expected contents (one object per file, one line each, trailing newline):

```json
{"queue_name":"orders","is_partitioned":false,"is_unlogged":false,"created_at":"2026-08-19T12:34:56.123Z"}
{"queue_name":"MyQueue","is_partitioned":true,"is_unlogged":false,"created_at":"2026-08-19T12:35:00Z"}
{"msg_id":42,"read_ct":0,"enqueued_at":"2026-08-19T12:35:00Z","last_read_at":null,"vt":"2026-08-19T12:34:56.123Z","message":{"order_id":7},"headers":null}
{"msg_id":42,"read_ct":3,"enqueued_at":"2026-08-19T12:35:00Z","last_read_at":"2026-08-19T12:34:56.123Z","vt":"2026-08-19T12:34:56.123Z","message":{"order_id":7},"headers":{"x-pgmq-group":"g1"}}
{"pattern":"orders.*","queue_name":"orders","bound_at":"2026-08-19T12:35:00Z","compiled_regex":"^orders\\.[^.]+$"}
{"pattern":"orders.*","queue_name":"orders","compiled_regex":"^orders\\.[^.]+$"}
{"queue_name":"orders","msg_id":42}
{"queue_name":"orders","throttle_interval_ms":250,"last_notified_at":"2026-08-19T12:35:00Z"}
```

The key order must be exactly as above; the timestamp rendering comes from aeson, which omits
the fraction when it is zero and otherwise trims trailing zeros (`.123` stays `.123`, half
a second would be `.5`). If a file differs from this shape in anything other than the
timestamp detail, the field list is wrong; fix the instance, not the golden. Then run the
suite again without `--accept` and expect every test to pass:

```bash
nix develop --command cabal test pgmq-core:pgmq-core-test
```

```text
pgmq-core
  QueueName validation
    ...                                                       OK
  JSON wire encodings
    goldens
      queue:                                                  OK
      unvalidated_queue:                                      OK
      message_unread:                                         OK
      message_read:                                           OK
      topic_binding:                                          OK
      routing_match:                                          OK
      topic_send_result:                                      OK
      notify_insert_throttle:                                 OK
    decoding
      a mixed-case name fails as Queue and succeeds as UnvalidatedQueue: OK
      a missing last_read_at decodes as Nothing:              OK
      ...

All N tests passed (0.02s)
```

### Step 6: `pgmq-hasql/test/JsonGoldenSpec.hs`

```haskell
{-# LANGUAGE OverloadedStrings #-}

-- | Pins the wire shape of 'QueueMetrics' and proves that 'Message' survives
-- encode-then-decode for arbitrary jsonb-storable bodies and headers. The
-- property lives here rather than in pgmq-core because this suite already
-- owns the hedgehog generators PostgreSQL accepts.
module JsonGoldenSpec (tests) where

import Data.Aeson (decode, encode)
import Data.Time (UTCTime (..), fromGregorian)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Generators (genJsonValue, genMessageBody)
import Hedgehog (Gen, forAll, property, tripping)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Pgmq.Hasql.Statements.Types (QueueMetrics (..))
import Pgmq.Types (Message (..), MessageId (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "JSON wire encodings"
    [ goldenVsString "queue_metrics" "test/golden/queue_metrics.json" (pure (encode sampleMetrics <> "\n")),
      goldenVsString "queue_metrics_partitioned" "test/golden/queue_metrics_partitioned.json" (pure (encode partitionedMetrics <> "\n")),
      testProperty "Message round-trips through JSON" $ property $ do
        m <- forAll genMessage
        tripping m encode decode
    ]

scrape :: UTCTime
scrape = UTCTime (fromGregorian 2026 8 19) 45296.123

-- | An empty queue: both ages are null, the partition estimate is null.
sampleMetrics :: QueueMetrics
sampleMetrics = QueueMetrics "orders" 0 Nothing Nothing 0 scrape 0 Nothing

-- | A partitioned queue on PGMQ 1.13 with a planner estimate.
partitionedMetrics :: QueueMetrics
partitionedMetrics = QueueMetrics "events" 120 (Just 2) (Just 3600) 9000 scrape 115 (Just 7)

-- | Whole milliseconds so the timestamp survives jsonb-independent rendering
-- exactly; aeson itself round-trips picoseconds, but the property is about
-- the record, not the clock.
genTime :: Gen UTCTime
genTime = do
  millis <- Gen.integral (Range.linear 0 (4_102_444_800_000 :: Integer))
  pure (posixSecondsToUTCTime (fromIntegral millis / 1000))

genMessage :: Gen Message
genMessage =
  Message
    <$> (MessageId <$> Gen.integral (Range.linear 1 maxBound))
    <*> genTime
    <*> genTime
    <*> Gen.maybe genTime
    <*> Gen.integral (Range.linear 0 1000)
    <*> genMessageBody
    <*> Gen.maybe genJsonValue
```

`Gen.integral (Range.linear 1 maxBound)` for `MessageId` needs the `Int64` type; write
`(Range.linear 1 (maxBound :: Int64))` with `Data.Int (Int64)` imported if inference
complains. Add `import JsonGoldenSpec qualified` and `JsonGoldenSpec.tests` to the list in
`pgmq-hasql/test/Main.hs` (it takes no pool). Create and inspect the goldens:

```bash
nix develop --command cabal test pgmq-hasql:pgmq-hasql-test --test-options='--accept -p "JSON wire encodings"'
cat pgmq-hasql/test/golden/*.json
```

```json
{"queue_name":"orders","queue_length":0,"newest_msg_age_sec":null,"oldest_msg_age_sec":null,"total_messages":0,"scrape_time":"2026-08-19T12:34:56.123Z","queue_visible_length":0,"default_partition_length":null}
{"queue_name":"events","queue_length":120,"newest_msg_age_sec":2,"oldest_msg_age_sec":3600,"total_messages":9000,"scrape_time":"2026-08-19T12:34:56.123Z","queue_visible_length":115,"default_partition_length":7}
```

The `-p` pattern keeps the first run to this spec; the full suite starts an ephemeral
PostgreSQL and takes longer. Then run the full suite once without `--accept`.

### Step 7: `pgmq-config/test/JsonGoldenSpec.hs`

```haskell
{-# LANGUAGE OverloadedStrings #-}

-- | One golden per 'ReconcileAction' constructor. A constructor without a
-- golden is a defect: the report is a published wire shape (see
-- @docs/design/020-json-wire-encodings.md@) once a consumer logs or serves it.
module JsonGoldenSpec (tests) where

import Data.Aeson (encode)
import Pgmq.Config.Types
import Pgmq.Types (QueueName, TopicPattern, parseQueueName, parseTopicPattern)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

tests :: TestTree
tests =
  testGroup
    "ReconcileAction wire encodings"
    [ golden "created_queue_standard" (CreatedQueue q StandardQueue),
      golden "created_queue_unlogged" (CreatedQueue q UnloggedQueue),
      golden "created_queue_partitioned" (CreatedQueue q (PartitionedQueue partition)),
      golden "created_queue_partitioned_default_premake" (CreatedQueue q (PartitionedQueue partition {premake = Nothing})),
      golden "enabled_notify" (EnabledNotify q (Just 1000)),
      golden "enabled_notify_default" (EnabledNotify q Nothing),
      golden "created_fifo_index" (CreatedFifoIndex q),
      golden "bound_topic" (BoundTopic q pat),
      golden "skipped_queue" (SkippedQueue q),
      golden "skipped_notify" (SkippedNotify q),
      golden "skipped_fifo_index" (SkippedFifoIndex q),
      golden "skipped_topic_binding" (SkippedTopicBinding q pat),
      golden "updated_notify_throttle" (UpdatedNotifyThrottle q 250 1000),
      golden "detected_queue_type_drift" (DetectedQueueTypeDrift q UnloggedQueue ObservedPartitioned)
    ]
  where
    golden name value =
      goldenVsString name ("test/golden/" <> name <> ".json") (pure (encode value <> "\n"))

q :: QueueName
q = either (error . show) id (parseQueueName "orders")

pat :: TopicPattern
pat = either (error . show) id (parseTopicPattern "orders.*")

partition :: PartitionConfig
partition = PartitionConfig {partitionInterval = "daily", retentionInterval = "7 days", premake = Just 2}
```

The record update `partition {premake = Nothing}` is unambiguous under
`DuplicateRecordFields` because `premake` exists in only one record of the module. Wire the
spec into `pgmq-config/test/Main.hs` as the first entry of the group (it takes no pool).
Generate and inspect:

```bash
nix develop --command cabal test pgmq-config:pgmq-config-test --test-options='--accept -p "ReconcileAction wire encodings"'
cat pgmq-config/test/golden/*.json
```

Expected, in filename order:

```json
{"action":"bound_topic","queue":"orders","pattern":"orders.*"}
{"action":"created_fifo_index","queue":"orders"}
{"action":"created_queue","queue":"orders","queue_type":{"type":"partitioned","partition_interval":"daily","retention_interval":"7 days","premake":2}}
{"action":"created_queue","queue":"orders","queue_type":{"type":"partitioned","partition_interval":"daily","retention_interval":"7 days","premake":null}}
{"action":"created_queue","queue":"orders","queue_type":{"type":"standard"}}
{"action":"created_queue","queue":"orders","queue_type":{"type":"unlogged"}}
{"action":"detected_queue_type_drift","queue":"orders","declared":{"type":"unlogged"},"observed":"partitioned"}
{"action":"enabled_notify","queue":"orders","throttle_interval_ms":1000}
{"action":"enabled_notify","queue":"orders","throttle_interval_ms":null}
{"action":"skipped_fifo_index","queue":"orders"}
{"action":"skipped_notify","queue":"orders"}
{"action":"skipped_queue","queue":"orders"}
{"action":"skipped_topic_binding","queue":"orders","pattern":"orders.*"}
{"action":"updated_notify_throttle","queue":"orders","observed_throttle_interval_ms":250,"declared_throttle_interval_ms":1000}
```

Add a golden for every extra constructor present in the tree. Run the full `pgmq-config`
suite once without `--accept`.

### Step 8: format, verify, commit Milestone 1

```bash
nix fmt
nix develop --command cabal test pgmq-core:pgmq-core-test
nix develop --command cabal test pgmq-hasql:pgmq-hasql-test
nix develop --command cabal test pgmq-config:pgmq-config-test
git add pgmq-core pgmq-hasql pgmq-config
git commit
```

Commit message:

```text
feat(json): add stable JSON codecs for the inspection-facing records

Hand-written aeson instances for Queue, UnvalidatedQueue, Message,
TopicBinding, RoutingMatch, TopicSendResult, NotifyInsertThrottle,
QueueMetrics, ReconcileAction, QueueType, PartitionConfig, and
ObservedQueueType, keyed by pgmq's snake_case column names with optional
fields always present as null. Golden tests in all three packages pin
every encoding; a property proves Message round-trips.

MasterPlan: docs/masterplans/7-expose-non-destructive-queue-inspection-through-typed-reads-json-codecs-and-the-pgmq-inspect-sister-package.md
ExecPlan: docs/plans/28-provide-stable-json-codecs-for-the-inspection-facing-records.md
Intention: intention_01m3tcw9vmeeftdtqj53d1d6nb
```

If `ArchivedMessage` was covered, say so in the body. Update Progress in this plan at this
point (check the M1 items, timestamped) and include the plan file in the commit.

### Step 9: `docs/design/020-json-wire-encodings.md`

Write the note with these headings and this content, in the house prose style:

```markdown
# Design Document 020: JSON wire encodings of the domain records

## Status

**Adopted (2026-MM-DD)**, as part of
`docs/plans/28-provide-stable-json-codecs-for-the-inspection-facing-records.md` under
`docs/masterplans/7-expose-non-destructive-queue-inspection-through-typed-reads-json-codecs-and-the-pgmq-inspect-sister-package.md`.
Records the decision in `docs/adr/queue-inspection-surface-boundary-and-wire-contract.md`
for implementers.


## The contract

(Which records carry instances, in which package. Keys are pgmq's SQL column names in
snake_case; the full key list per record. Optional fields are always present and encode as
`null`; decoders accept an absent key as `Nothing`. Timestamps use aeson's default ISO-8601
UTC rendering: `2026-08-19T12:35:00Z`, with fractional seconds when the value has them and
trailing zeros trimmed. `Queue` and `UnvalidatedQueue` share one shape; `Queue` decoding
validates through `parseQueueName`. `FromJSON` exists for `Queue`, `UnvalidatedQueue`,
`Message`, `ArchivedMessage`; everything else is encode-only. `ReconcileAction` is a tagged
object with `"action"`; `enabled_notify`'s `throttle_interval_ms` is `null` for "declared
the default".)


## Published is frozen

(Once a release carries an encoding, no key is removed or re-typed and no meaning changes.
Adding a key is allowed. Anything incompatible is a new key or a new shape, never an in-place
change. The golden tests are the enforcement: a diff in a golden file is a compatibility
event that needs a changelog entry, not an `--accept`.)


## Why hand-written

(Derived instances depend on `Options`; a one-line edit moves every key. Hand-written field
lists are the policy as code. `toEncoding` via `pairs` keeps key order deterministic;
`object` alone does not promise one.)


## Adding a field

(Add it to the record and to the field list in declaration order of the wire, regenerate the
golden with `--accept`, read the diff, and write the changelog entry that says the addition
is additive. A new `ReconcileAction` constructor gets a case and a golden.)


## Where this is enforced

(`pgmq-core/test/JsonGoldenSpec.hs`, `pgmq-hasql/test/JsonGoldenSpec.hs`,
`pgmq-config/test/JsonGoldenSpec.hs`, and their `test/golden/` directories.)


## Related documents

(002 for the two depth figures, 016 for the validated/lenient split, the surface design note
`docs/design/021-inspection-surface-wire-contract.md` whose wire types obey the same rules,
and the cross-project conventions cited as `mori://shinzui/keiro-ui`, path
`docs/architecture/inspection-api-conventions.md`, artifact-level URI pending.)
```

Replace every parenthesised summary with full prose; the note must stand alone.

### Step 10: the capability record

Allocate the handle and inspect what exists:

```bash
okf id list docs/capabilities --profile docs/capabilities/profile.dhall
okf id next docs/capabilities --profile docs/capabilities/profile.dhall CAP
```

The second command prints the next free handle (`CAP-10` as of this writing; `CAP-11` if
plan 27 landed first). Create `docs/capabilities/json-wire-encodings.md` with that handle,
modelled on `docs/capabilities/message-queue-client.md`:

```yaml
---
title: "JSON wire encodings of the domain records"
type: Capability
description: "Stable, golden-pinned aeson encodings for queues, messages, metrics, bindings, routing matches, throttles, and the reconciliation report, keyed by pgmq's snake_case column names."
generated:
  by: anthropic/claude-fable-5-1
  at: "2026-MM-DDTHH:MM:SSZ"
capabilityId: CAP-N
provider: mori://shinzui/pgmq-hs
status: shipped
stability: experimental
since: "unreleased"
packages:
  - pgmq-core
  - pgmq-hasql
  - pgmq-config
requires:
  - CAP-1
interface:
  - Pgmq.Types
  - Pgmq.Hasql.Statements.Types
  - Pgmq.Config.Types
evidence:
  - kind: test
    resource: pgmq-core/test/JsonGoldenSpec.hs
    proves: Every pgmq-core record's encoding is pinned; a mixed-case name fails as Queue and succeeds as UnvalidatedQueue.
  - kind: test
    resource: pgmq-hasql/test/JsonGoldenSpec.hs
    proves: QueueMetrics is pinned with total and visible depth distinct; Message round-trips for arbitrary bodies.
  - kind: test
    resource: pgmq-config/test/JsonGoldenSpec.hs
    proves: Every ReconcileAction constructor has a pinned tagged encoding.
  - kind: guide
    resource: docs/design/020-json-wire-encodings.md
    proves: The naming, null, timestamp, and freeze rules.
---
```

Follow the frontmatter with a body in the catalog's style: what it provides, a Shape block
showing `encode`, and Limits (pre-1.0 and experimental; encode-only records; the freeze
rule means fields are forever). Add the row to the table in `docs/capabilities/index.md`
and append to `docs/capabilities/log.md` under today's date with an `**Addition**` bullet.
Validate:

```bash
just docs-check
```

### Step 11: changelogs and request closure

Add an `## Unreleased` section at the top of each of `CHANGELOG.md`, `pgmq-core/CHANGELOG.md`,
`pgmq-hasql/CHANGELOG.md`, and `pgmq-config/CHANGELOG.md` (above the `0.6.1.1` entry; if an
`Unreleased` section already exists from another plan, add to it). Each says what gained
instances, that the field names are pgmq's column names, that optional fields encode as
`null`, that the encodings are a published compatibility surface per design note 020, and
that adding instances is a PVP minor change carried by the family's next lockstep release.
`pgmq-config`'s entry also notes the new `aeson` library dependency.

Close `IR-2`. In `docs/improvement-requests/provide-json-codecs-for-inspection-facing-records.md`
change `status: proposed` to `status: completed`, and add directly after it:

```yaml
completedAt: "2026-MM-DDTHH:MM:SSZ"
resolution: >-
  Hand-written instances with pgmq's snake_case column names landed in pgmq-core, pgmq-hasql,
  and pgmq-config under ExecPlan 28; golden tests pin every encoding, the policy is design
  note 020, existing newtype instances are unchanged, and QueueMetrics distinguishes
  queue_length from queue_visible_length.
```

Use the UTC time at which the three suites went green. Then:

```bash
okf log add docs/improvement-requests --kind Update -m "IR-2 completed: JSON codecs with golden tests and design note 020 (ExecPlan 28)"
just docs-check
nix fmt
git add docs CHANGELOG.md pgmq-core/CHANGELOG.md pgmq-hasql/CHANGELOG.md pgmq-config/CHANGELOG.md
git commit
```

Commit message:

```text
docs(json): record the wire-encoding policy and close IR-2

Design note 020 states the snake_case column-name rule, explicit nulls,
the timestamp form, and the freeze rule; the capability catalog gains the
encodings record; four changelogs carry Unreleased entries; IR-2 is
completed.

MasterPlan: docs/masterplans/7-expose-non-destructive-queue-inspection-through-typed-reads-json-codecs-and-the-pgmq-inspect-sister-package.md
ExecPlan: docs/plans/28-provide-stable-json-codecs-for-the-inspection-facing-records.md
Intention: intention_01m3tcw9vmeeftdtqj53d1d6nb
```

Before committing, fill in this plan's Progress, Outcomes & Retrospective, and any
Surprises & Discoveries, and include the plan file.


## Validation and Acceptance

`IR-2` lists four acceptance items; each maps to something you can run.

Item 1, golden encoding tests pin the output of every record, and a rename breaks a test:
after Milestone 1, temporarily change `"queue_name"` to `"name"` in `queueFields`, run
`nix develop --command cabal test pgmq-core:pgmq-core-test`, and observe:

```text
    goldens
      queue:                                                  FAIL
        Test output was different from 'test/golden/queue.json'. It was:
        {"name":"orders","is_partitioned":false,...}
```

Revert the change and the suite passes again. The same holds for `queue_metrics` in
`pgmq-hasql` and for any `ReconcileAction` golden in `pgmq-config`.

Item 2, the policy is documented where implementers see it and says encodings are frozen
once released: `docs/design/020-json-wire-encodings.md` exists with a "Published is frozen"
section, and every instance's Haddock names the file; `grep -rn "020-json-wire-encodings" pgmq-core/src pgmq-hasql/src pgmq-config/src`
returns at least thirteen lines (one per field-list function or instance).

Item 3, existing newtype instances are unchanged and no downstream consumer breaks:
`git diff v0.6.1.1 -- pgmq-core/src/Pgmq/Types.hs` shows no change to the `deriving newtype`
lines or to the hand-written `FromJSON QueueName`; `pgmq-core/test/QueueNameSpec.hs` still
passes; and `nix develop --command cabal build all` builds `pgmq-effectful`, `pgmq-migration`,
and `pgmq-bench` unchanged.

Item 4, `QueueMetrics` encodings distinguish total and visible depth: the two goldens in
`pgmq-hasql/test/golden/` carry both `queue_length` and `queue_visible_length`, with
different values (`120` and `115`) in the partitioned sample, and the Haddock on
`queueMetricsFields` defines each.

The full commands and their expected tails:

```bash
nix develop --command cabal test pgmq-core:pgmq-core-test
nix develop --command cabal test pgmq-hasql:pgmq-hasql-test
nix develop --command cabal test pgmq-config:pgmq-config-test
just docs-check
nix flake check
```

Each `cabal test` ends with `All N tests passed`. `just docs-check` prints the validation
summaries for the capabilities, reviews, and improvement-requests bundles with no error.
`nix flake check` builds `pgmq-core` with its tests, which proves `tasty-golden` and the
golden files resolve under Nix; it is slower than the cabal runs and may be deferred to the
end of Milestone 2, but it must pass before the plan is marked complete.


## Idempotence and Recovery

Every step is safe to repeat. Instances and field lists are plain additions; applying them
twice is a compile error that tells you where the duplicate is. Golden generation with
`--accept` overwrites files; if a golden ever looks wrong, delete it and regenerate after
fixing the instance, never edit the file by hand. The cabal edits are reformatted by
`nix fmt`; running it twice is a no-op.

If `pgmq-core`'s tests fail under `nix flake check` with an unresolved `tasty-golden`, the
`ghc9124` package set changed; add a `callHackageDirect` override for `tasty-golden` in
`nix/haskell-overlay.nix` following the `ephemeral-pg` entry's shape, and record the
surprise here.

If `tripping` in the hedgehog property reports a counterexample, the shrunk `Message` tells
you which field does not survive; the likeliest cause is a body or header the generator
produced that `encode` renders differently from how `decode` reads it (for example a
floating-point number), and the fix is in the generator or in the comparison, not in the
instance. Record the evidence in Surprises & Discoveries.

The OKF edits are plain text. If `just docs-check` rejects the capability record, the error
names the field; the frontmatter in Step 10 carries every field the profile requires for a
`shipped` record. If `okf id next` returns a handle you did not expect, use it; never reuse
a handle the listing shows.

To roll back a milestone, revert its commit; nothing outside the repository is touched.


## Interfaces and Dependencies

Libraries: `aeson ^>=2.2` (already a dependency of `pgmq-core` and `pgmq-hasql`; added to
`pgmq-config`'s library by this plan), `tasty-golden ^>=2.3` (new, test suites only; 2.3.6
in the pinned Nix set), `bytestring` (test suites), and the existing `hedgehog`,
`tasty-hedgehog`, `tasty`, `tasty-hunit`, `text`, `time`.

At the end of Milestone 1 these instances exist, all hand-written, all exported implicitly
with their types:

```haskell
-- pgmq-core/src/Pgmq/Types.hs
instance ToJSON Queue;                instance FromJSON Queue
instance ToJSON UnvalidatedQueue;     instance FromJSON UnvalidatedQueue
instance ToJSON Message;              instance FromJSON Message
instance ToJSON ArchivedMessage;      instance FromJSON ArchivedMessage   -- only if the type exists
instance ToJSON TopicBinding
instance ToJSON RoutingMatch
instance ToJSON TopicSendResult
instance ToJSON NotifyInsertThrottle

-- pgmq-hasql/src/Pgmq/Hasql/Statements/Types.hs
instance ToJSON QueueMetrics

-- pgmq-config/src/Pgmq/Config/Types.hs
instance ToJSON ReconcileAction
instance ToJSON QueueType
instance ToJSON PartitionConfig
instance ToJSON ObservedQueueType
```

No existing instance changes. No function or type is added to any export list except
`ArchivedMessage (..)` when plan 27 has introduced it. The field-list functions
(`queueFields`, `messageFields`, `queueMetricsFields`, `reconcileActionFields`, and the
rest) and the `encodeFields` helpers are private.

Test modules: `pgmq-core/test/JsonGoldenSpec.hs`, `pgmq-hasql/test/JsonGoldenSpec.hs`,
`pgmq-config/test/JsonGoldenSpec.hs`, each exporting `tests :: TestTree`, and the golden
files under each package's `test/golden/`.

Documents: `docs/design/020-json-wire-encodings.md`, the capability record
`docs/capabilities/json-wire-encodings.md` with its allocated handle, the index and log of
that bundle, four changelogs, and the completed `IR-2` record with the bundle log entry.

Consumers of this plan: `docs/plans/29-create-the-pgmq-inspect-sister-package-with-the-http-inspection-surface.md`
serves these encodings directly and writes none of its own for these records;
`docs/plans/30-add-the-notify-accelerated-poll-authoritative-websocket-live-feed-to-pgmq-inspect.md`
embeds the `QueueMetrics` encoding in its frames; and
`docs/plans/27-add-non-destructive-peek-archive-and-lookup-reads-across-the-pgmq-layers.md`
either receives the `ArchivedMessage` instances from this plan or supplies them by the
text in Step 2.
