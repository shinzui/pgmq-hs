# Design Document 020: JSON wire encodings of the domain records

## Status

**Adopted (2026-10-20 UTC)**, as part of
[ExecPlan 28](../plans/28-provide-stable-json-codecs-for-the-inspection-facing-records.md)
under [MasterPlan 7](../masterplans/7-expose-non-destructive-queue-inspection-through-typed-reads-json-codecs-and-the-pgmq-inspect-sister-package.md).
Implements the domain-record portion of the
[inspection boundary ADR](../adr/queue-inspection-surface-boundary-and-wire-contract.md).
The encodings are currently unreleased and experimental.

## The contract

The owning package defines each encoding. Keys use snake_case and, where a pgmq SQL
column exists, its exact column name. `encode` preserves the record-field order
listed below. Payload/header object ordering remains aeson's own; consumers must
identify fields by name. This byte order applies to the record encoder; encoding
a separately constructed Value preserves its meaning but may reorder its keys.
Optional fields are always emitted, with JSON null for
`Nothing`. Decoders accept missing optional keys and explicit null as `Nothing`.

| Package and record | Wire fields, in order |
|---|---|
| pgmq-core: Queue, UnvalidatedQueue | queue_name, is_partitioned, is_unlogged, created_at |
| pgmq-core: Message | msg_id, read_ct, enqueued_at, last_read_at, vt, message, headers |
| pgmq-core: ArchivedMessage | All Message fields, then archived_at, in one flat object |
| pgmq-core: TopicBinding | pattern, queue_name, bound_at, compiled_regex |
| pgmq-core: RoutingMatch | pattern, queue_name, compiled_regex |
| pgmq-core: TopicSendResult | queue_name, msg_id |
| pgmq-core: NotifyInsertThrottle | queue_name, throttle_interval_ms, last_notified_at |
| pgmq-hasql: QueueMetrics | queue_name, queue_length, newest_msg_age_sec, oldest_msg_age_sec, total_messages, scrape_time, queue_visible_length, default_partition_length |
| pgmq-config: PartitionConfig | partition_interval, retention_interval, premake |

`Queue` and `UnvalidatedQueue` share a wire shape. `Queue` decoding uses the existing
`QueueName` decoder and validates through `parseQueueName`. `UnvalidatedQueue`
decoding preserves the text as given; it also accepts mixed-case and foreign names.
Record decoders exist only for those two types, `Message` and `ArchivedMessage`.
The other records are encode-only because no decode consumer is currently promised.
All existing newtype instances, including numeric `MessageId`, remain unchanged.

Timestamps use aeson's ISO-8601 UTC representation: `2026-08-19T12:35:00Z` for a
whole second, or `2026-08-19T12:34:56.123Z` with trailing fractional zeros trimmed.
Message bodies and headers accept every JSON value, including scalars and arrays.
For `headers :: Maybe Value`, `Nothing` and `Just Null` encode identically; decoding
normalizes both to `Nothing`. Message/ArchivedMessage round-trip guarantees include
that normalization. `message = Null` remains a JSON null body, not a missing message.

Metrics keep total and visible depth distinct: `queue_length` counts all live rows,
while `queue_visible_length` counts those available to a consumer now. The sample
partitioned metric encodes 120 and 115 respectively. Missing ages remain null.
`default_partition_length = null` means unavailable on PGMQ 1.12 or inapplicable to
an ordinary queue on 1.13; it never means zero. See
[design note 002](002-queue-visible-length.md) and the
[PGMQ compatibility ADR](../adr/pgmq-1.12-1.13-compatibility.md).

## Reconciliation reports

`QueueType` encodes as `{"type":"standard"}`, `{"type":"unlogged"}`, or an object
starting with `"type":"partitioned"` followed by the PartitionConfig fields.
`premake = null` means creation uses the server default. `ObservedQueueType` encodes
as a bare string: `"standard"`, `"unlogged"` or `"partitioned"`.

Every `ReconcileAction` is an object whose first field is `action`, holding its
snake_case constructor name. Remaining fields follow this table:

| Action | Remaining fields |
|---|---|
| created_queue | queue, queue_type |
| enabled_notify | queue, throttle_interval_ms |
| created_fifo_index | queue |
| bound_topic | queue, pattern |
| skipped_queue | queue |
| skipped_notify | queue |
| skipped_fifo_index | queue |
| skipped_topic_binding | queue, pattern |
| updated_notify_throttle | queue, observed_throttle_interval_ms, declared_throttle_interval_ms |
| detected_queue_type_drift | queue, declared, observed |
| detected_queue_name_collision | queue, colliding_names |
| unsupported_notify_on_partitioned_queue | queue |

`colliding_names` is a nonempty string array retaining the report's ascending lexical order.

`enabled_notify` preserves a declared default as null, rather than replacing it
with 250. Declaring a default and declaring an explicit 250 are different facts in
the report. `defaultThrottleMs` remains available to callers needing the resolved
number. New report constructors must receive a case and a golden in the same change.

## Exact message IDs

`msg_id` remains a JSON integer. The fixtures
[above JavaScript's safe integer range](../../pgmq-core/test/golden/message_above_safe_integer.json)
and [at the signed int64 maximum](../../pgmq-core/test/golden/message_max_int64.json)
contain exact tokens 9007199254740993 and 9223372036854775807; corresponding archive
fixtures are committed too. Haskell decoding preserves both. Browser consumers
must parse integer tokens losslessly before converting any value to JavaScript
Number, and echo inspection cursors as exact decimal strings. A generated Number
type alone does not preserve those positions. The later inspection-client acceptance
must reuse these fixtures rather than replacing the numeric library encoding.

## Published is frozen

Once a release carries an encoding, keys are never removed or re-typed and their
meanings never change in place. Additive keys are allowed. An incompatible change
requires a new field or a new shape. Adding these instances is a PVP minor change
and can conflict with downstream orphan instances; the family's next lockstep
release carries it, with no version bump in this plan.

A golden diff is a compatibility event to review, not a reason to run `--accept`
without explanation. The suites use tasty-golden 2.3.6's public NoCreateFile option, so a missing
fixture fails instead of silently being created. New fixtures start as empty
placeholders and are filled only by an explicit `--accept` run; existing-file
drift also fails. Source registration and Nix/sdist checks ensure all committed
fixtures are present. A deliberate queue_name-to-name mutation was verified to
fail its golden and was reverted.

## Why hand-written

Derived instances can move field names when Options change. Hand-written lists
make the policy visible in code. Each private field list uses aeson's `KeyValue`
class: `toJSON` instantiates it as Value pairs for `object`; `toEncoding` instantiates
it as Series and combines them with `pairs`. No domain type gains an export merely
to support its codec.

The initially proposed concrete Pair conversion forced nested records through
Value, losing their own encoding order and increasing allocation. The final shared
list preserves nested encoders. The
[allocation evidence and reproducible probe](evidence/020-json-encodings/README.md)
cover small and 1 MiB message bodies. This implementation still allocates field
lists and output bytes; it promises neither allocation-free encoding nor a bound
on payload size. Tests compare decoded `toEncoding` output with `toJSON` for every
record so the two methods cannot silently acquire different meanings.

## Adding a field

Add the field to the owning record and its single field list, keeping established
wire fields in order. Update its decoder if that record has one. Generate the new
golden from the real encoder with `--accept` (create an empty file first for a
new fixture), inspect its diff and write the
changelog entry describing the additive change. Preserve missing/null normalization
for optional fields. A new ReconcileAction constructor gets an encoding case and
an explicit golden; all current twelve constructors are covered.

## Where this is enforced

`pgmq-core/test/JsonGoldenSpec.hs` pins queues, live/archive messages, every other
core record, all header JSON forms and exact int64 fixtures. It tests validated
versus lenient queue decoding, missing optional keys, normalization and encoder
parity. `pgmq-hasql/test/JsonGoldenSpec.hs` pins metrics and generates arbitrary
JSON bodies/headers for normalized live/archive round trips and parity checks.
`pgmq-config/test/JsonGoldenSpec.hs` pins every report constructor, the declared
queue shapes, partition options and all observed shapes. Their 55 golden files
live under each package's `test/golden/`, registered in `extra-source-files`.

## Related documents

[Design note 016](016-queue-name-validation.md) explains the validated/lenient name
split. The planned `docs/design/021-inspection-surface-wire-contract.md` extends
these rules to the HTTP/WebSocket surface's own wire types, while serving domain
records through these instances. Cross-project conventions are recorded at
`mori://shinzui/keiro-ui`, path `docs/architecture/inspection-api-conventions.md`
(artifact-level URI pending). pgmq-hs owns this contract and needs no keiro client
to encode or decode its records.
