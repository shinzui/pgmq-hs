# Design Document 019: Non-destructive inspection reads

## Status

**Adopted (2026-10-01)**, as part of
`docs/plans/27-add-non-destructive-peek-archive-and-lookup-reads-across-the-pgmq-layers.md`
(IR-1), the first child of MasterPlan 7
(`docs/masterplans/7-expose-non-destructive-queue-inspection-through-typed-reads-json-codecs-and-the-pgmq-inspect-sister-package.md`).


## The contract

Five reads observe a queue, its archive, or its metrics without changing anything:

| Session (`pgmq-hasql`, re-exported by `Pgmq`) | Effect operation (`pgmq-effectful`) | Returns |
|---|---|---|
| `peekMessages :: PeekMessages -> Session (Vector Message)` | `peekMessages` | a keyset page of `pgmq.q_<name>` |
| `peekArchivedMessages :: PeekMessages -> Session (Vector ArchivedMessage)` | `peekArchivedMessages` | a keyset page of `pgmq.a_<name>` with `archived_at` |
| `lookupMessage :: LookupMessage -> Session (Maybe Message)` | `lookupMessage` | one queue row by id, or `Nothing` |
| `lookupArchivedMessage :: LookupMessage -> Session (Maybe ArchivedMessage)` | `lookupArchivedMessage` | one archive row by id, or `Nothing` |
| `queueMetricsUnvalidated :: Text -> Session QueueMetrics` | `queueMetricsUnvalidated` | `pgmq.metrics` for a plain-text name |

None of them leases. `vt`, `read_ct`, and `last_read_at` are byte-identical before and
after any number of peeks and lookups, and a consumer polling the queue concurrently
receives every message exactly once. `pgmq-hasql/test/InspectionSpec.hs` pins both
properties by reading the raw cells before and after, and by racing a leasing consumer
against two hundred peeks.


## Why they exist

Every upstream read leases. `pgmq.read`, `read_with_poll`, `pop`, and the grouped reads all
`UPDATE ... SET vt = clock_timestamp() + <vt>, read_ct = read_ct + 1` as a side effect of
returning rows, so an operator who "just looks" steals the messages for the visibility
timeout and inflates the counter retry and dead-letter policies depend on. Upstream has no
read that does not lease, no read of an archive table, and no fetch by id.

Design note 012 permits hand-written statements for exactly this case: reads upstream has
no equivalent for. The four reads are plain `SELECT`s over the tables the upstream schema
defines (`pgmq.q_<name>`, `pgmq.a_<name>`); they shadow no `pgmq.*` function and add no
migration. The lenient metrics read calls `pgmq.metrics` itself with the projection
`queueMetrics` already uses, byte for byte (`queueMetricsSql` in
`Pgmq.Hasql.Statements.QueueObservability`).


## Lenient names

The argument records carry the queue name as plain `Text` (`unvalidatedQueueName`), not as
a `QueueName`. An inspection surface must show whatever exists in the database, including
queues other clients created with names `parseQueueName` rejects (mixed case, hyphens,
non-ASCII); design note 016 explains why the typed path is strict and why it stays strict
for everything that creates, configures, or leases. A caller holding a validated name passes
`queueNameToText`.

The physical table is never derived in Haskell. Each session first runs
`select pgmq.format_table_name($1, $2)` with prefix `q` or `a`, upstream's single source of
truth for the mapping (it lowercases and raises for names containing `$`, `;`, `--`, or
`'`), then splices the result into the read's SQL as a double-quoted identifier
(`quoteIdentifier`, which doubles any embedded `"`). The names are shown exactly as given;
the reads neither validate nor normalise them, and introduce no new aliasing hazard because
they create and configure nothing. `pgmq-hasql/test/InspectionForeignNameSpec.hs` proves
every read for a mixed-case and a hyphenated name on a dedicated instance (a mixed-case
`pgmq.meta` row poisons the typed `listQueues` for every test sharing a database).


## Paging

Pages are keyset pages: `msg_id > cursor` (exclusive; `Nothing` starts at the beginning),
`order by msg_id asc`, `limit $2`. They never use `OFFSET`, so a page does not shift when
rows before the cursor are consumed or archived. A page shorter than `limit` is the last
page; a caller that wants a "has next page" signal without an extra round-trip requests
`limit + 1` rows and drops the last. The cursor predicate is the explicit
`($1::bigint is null or msg_id > $1)`, which does not assume where ids start.

`limit` is passed straight to SQL `LIMIT`: it must be positive. `0` returns an empty page;
a negative value is a server error. Callers exposing paging to untrusted input (the
HTTP layer of the planned sister package) validate before calling.


## Errors

A missing queue is the server's `undefined_table` error, SQLSTATE `42P01`, raised by the
second statement; it surfaces as hasql's `SessionError` and, through the effect, as
`PgmqSessionError` under both interpreters. A name containing a character
`format_table_name` rejects surfaces as that function's raised exception. A missing message
is a typed `Nothing`, never an error. `isTransient` is unchanged: both errors are permanent.


## Why `unpreparable`

The read's SQL text embeds the table name, so it differs per queue. hasql caches prepared
statements per connection keyed by SQL text; a prepared statement per queue would grow that
cache without bound on a server that inspects many queues. The four reads therefore use
`Hasql.Statement.unpreparable`. `formatTableName` has fixed text and stays `preparable`.
The extra round-trip runs in the same session on the same pooled connection and is
acceptable on an inspection path.


## Partitioned queues

A partitioned queue's `pgmq.q_<name>` and `pgmq.a_<name>` are partitioned parents; a
`SELECT` on the parent sees every partition, so the reads need no special case.
`InspectionSpec` covers this under the partman shell.


## Tracing

No `pgmq.*` function backs the four reads, so the traced interpreter labels them with this
library's own names, as it already does for the catalog-backed `pgmq.list_fifo_indexes`:
`pgmq.peek`, `pgmq.peek_archive`, `pgmq.lookup_message`, and
`pgmq.lookup_archived_message`. Spans are `Internal`, carry the name exactly as given in
`messaging.destination.name`, and the lookups add `messaging.message.id`. The lenient
metrics read does call `pgmq.metrics` and keeps that label.


## Compatibility

Adding five constructors to the `Pgmq` effect GADT is a breaking change under the PVP: an
interpreter that matches every constructor (a mock in a consumer's tests) must add five
cases. The family's next release is therefore a major one (see the ADR below).


## Where enforced

- `pgmq-hasql/test/InspectionSpec.hs`: byte-identical rows, the concurrent consumer,
  exactly-once keyset paging (1000 ids in 143 pages of at most 7), no `OFFSET` in any
  statement's SQL, archive timestamps, lookups in both tables, limit semantics, the `42P01`
  missing-queue error, metrics parity, and a partitioned queue.
- `pgmq-hasql/test/InspectionForeignNameSpec.hs`: mixed-case and hyphenated names.
- `pgmq-effectful/test/TracedInterpreterSpec.hs`, the "non-destructive inspection reads"
  case in `featureTests`: all five operations under both interpreters and the span shapes.


## Related

- Design note 012 (`docs/design/012-vendor-upstream-pgmq-sql.md`): hand-written statements
  for reads upstream lacks.
- Design note 016 (`docs/design/016-queue-name-validation.md`): the strict typed path and
  the lenient inspection path.
- ADR `docs/adr/queue-inspection-surface-boundary-and-wire-contract.md`.
- `mori://shinzui/keiro-ui/okf/adrs/concepts/ADR-1` (queue-level views belong to pgmq-hs)
  and `mori://shinzui/keiro-ui/okf/adrs/concepts/ADR-3` (live views rest on authoritative
  reads).
