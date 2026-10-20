# Queue inspection lives in a runnable sister package with a pgmq-hs-owned wire contract

## Status

Accepted, 2026-09-30; clarified after implementation review on 2026-10-19. Implementation is tracked by
[MasterPlan 7](../masterplans/7-expose-non-destructive-queue-inspection-through-typed-reads-json-codecs-and-the-pgmq-inspect-sister-package.md).
This repository has no profiled ADR bundle; this record follows its existing plain-Markdown
decision convention and introduces no OKF metadata.

The inspection reads were implemented on 2026-10-01 by
[ExecPlan 27](../plans/27-add-non-destructive-peek-archive-and-lookup-reads-across-the-pgmq-layers.md);
their full contract is [design note 019](../design/019-non-destructive-inspection-reads.md).

## Context

Three improvement requests filed from the keiro runtime UI initiative (`IR-1`, `IR-2`, `IR-3` in
`docs/improvement-requests/`) ask pgmq-hs for non-destructive queue reads, JSON codecs for its
domain records, and an HTTP and WebSocket inspection surface. The requests cite the keiro-ui
decisions that queue-level views belong to pgmq-hs
(`mori://shinzui/keiro-ui/okf/adrs/concepts/ADR-1`), that inspection surfaces live in sister
packages exporting an embeddable WAI `Application`
(`mori://shinzui/keiro-ui/okf/adrs/concepts/ADR-4`), and that push is only a hint over an
authoritative read path (`mori://shinzui/keiro-ui/okf/adrs/concepts/ADR-3`).

pgmq-hs has a second audience that keiro-ui does not: a user of the library who runs no keiro
process and wants to see their queues from a browser, a dashboard, or a script. Nothing in the
requests prevents serving that audience, but nothing in them requires it either. A surface that
is only reachable by mounting it inside keiro, or whose contract is only written down in
keiro-ui's conventions document, would leave that audience without a way to run or build
against it.

Before the inspection reads, `pgmq.read` and `read_with_poll` leased rows, and `pop` removed them. Upstream pgmq provides no non-destructive read, no archive read, and no
fetch-by-id, and the vendoring policy in [design note 012](../design/012-vendor-upstream-pgmq-sql.md)
permits hand-written statements exactly for reads upstream has no equivalent for.

## Decision

**The surface is a sister package, `pgmq-inspect`, and it is runnable on its own.** It depends
on `pgmq-core`, `pgmq-hasql`, and `pgmq-effectful`; no library package in the family gains a
web dependency. It exports the bare WAI `Application` (prefix-agnostic, routing on `pathInfo`,
dispatching the WebSocket upgrade from the router so a host can mount it under any prefix), a
runner that binds a host-chosen address and port, and an executable that serves the surface
from a database URL. The package depends on nothing from keiro, kiroku, or shibuya.

**Handlers run through a host-supplied runner over the `Pgmq` effect.** The host chooses the
plain or the traced interpreter; the router is testable against a mock interpreter; error
mapping reuses `isTransient` (503 when it holds) rather than re-deriving a classification.

**pgmq-hs owns and documents its wire contract.** The cross-project conventions
(`mori://shinzui/keiro-ui`, path `docs/architecture/inspection-api-conventions.md`,
artifact-level URI pending) are adopted because they are sensible: snake_case fields,
exclusive-cursor pagination with `next_cursor` omitted on the last page, a structured error
envelope with machine-readable codes, type-tagged WebSocket frames with explicit subscribe and
unsubscribe, snapshot-then-delta, ping/pong, in-band `error` and `goodbye`, bounded delivery
queues with overflow signaled in-band, and CORS with an explicit allowed-origins list that is
disabled by default. But the contract is recorded in this repository's design notes and user
guide, uses pgmq's own vocabulary (queues, messages, archive, visibility timeout, `msg_id`,
`read_ct`, `vt`), is served as an OpenAPI 3 document, and is frozen on pgmq-hs's own authority:
once a field, route, or frame ships in a release it is never removed or re-typed, and anything
incompatible ships as a new path or frame type.

**JSON encodings of domain records are a published compatibility surface.** Field names are
pgmq's SQL column names in snake_case; `Maybe` fields are always present and encode as `null`;
instances are hand-written so no deriving option can move a field; golden tests pin every
encoding. `Queue` and `UnvalidatedQueue` encode identically.

The domain codecs were implemented by
[ExecPlan 28](../plans/28-provide-stable-json-codecs-for-the-inspection-facing-records.md).
[Design note 020](../design/020-json-wire-encodings.md) owns the full key and
report-tag contract, with 53 golden files across the three owning packages.
Queue/UnvalidatedQueue and live/archive Message decode; other records remain
encode-only. Reconciliation reports preserve declared defaults as null. Every
record's Value and streaming encoders have parity coverage. Golden suites reject
missing fixtures as well as byte drift, so a packaging omission cannot silently
regenerate the expected output. The shared field
lists retain nested streaming encoders; measurement details stay in the design
note rather than becoming an architectural performance promise.

**Inspection reads accept any server-accepted name and never lease.** The peek, archive, and
lookup reads take the queue name as plain `Text`, resolve the physical table through
`pgmq.format_table_name` on the server, and run an unprepared `SELECT` ordered by `msg_id` with
an exclusive cursor and `LIMIT`, never `OFFSET`. They modify no row. A missing message is a typed
`Nothing`; a missing queue is the server's `42P01`. No upstream function body is overridden and
no migration is added. Because no upstream function backs these reads, the traced interpreter
labels them with this library's own names (`pgmq.peek`, `pgmq.peek_archive`,
`pgmq.lookup_message`, `pgmq.lookup_archived_message`), never a `pgmq.<function>` name that
does not exist; any future hand-written read follows the same rule.

**Push is a hint; the poll is truth.** The WebSocket feed carries queue metrics, not message
bodies. One server-wide LISTEN connection accelerates updates for queues whose names validate;
one shared coordinator polls each distinct subscribed queue on an interval and fans out the result; the feed reconnects and re-`LISTEN`s on
its own and never stops polling. A client re-reads messages through the paged HTTP routes.

**Inspection cost is bounded independently of worker capacity.** Row limits bound result
counts, not scan cost or payload bytes. Ordinary queues and archives have `msg_id` primary
keys; time-partitioned parents and some partitioned archives have no matching index, so
keyset reads can scan/sort many rows. Do not add index DDL during inspection or override
upstream functions. Record complete-query EXPLAIN evidence for numeric and time partitions,
live and archived data, before claiming indexed behavior. Any optional index remains an
operator action with measured read benefit and write/storage/locking costs.

One runtime shares read admission across HTTP and the feed; excess work fails promptly.
Production inspection uses its own finite pool and PostgreSQL statement/lock deadlines.
The library does not reconfigure arbitrary worker connections. Existing blackholed network
I/O limitations remain explicit; an application timeout alone is not a hard deadline.
One metrics refresh per distinct queue is in flight at a time, notifications coalesce,
refresh frequency and distinct queues are capped, and scheduling is fair. Query count must
not multiply with the number of tabs watching the same queue. Metrics still cost a scan of
queue rows, and age fields change as time passes even without writes.

**The browser contract preserves exact positions and describes a moving view.** Existing
numeric `MessageId` JSON remains numeric; clients must parse 64-bit integer tokens losslessly
and echo cursors as exact decimal strings in query parameters. No `Number` conversion may
precede that parsing. Paging does not hold a multi-request snapshot: late commits and older
IDs archived after a cursor advances require a restart from the beginning. `headers` accepts
any JSON value. `Nothing` and `Just Null` encode alike and decode to `Nothing`; codec tests
state that normalization instead of promising an impossible strict round trip.

**The served schema is tested against real responses and mounted paths.** OpenAPI declares
relative server URL `.`; omitting servers would select origin root and break prefix mounts.
Readiness 503 keeps the readiness body, arbitrary JSON headers stay representable, and
response fixtures are validated against schemas rather than merely snapshotting two unrelated
encodings. Public database errors use fixed display-safe messages and allowlisted SQLSTATE;
raw backend messages, detail, hint, SQL and parameters are private. Queue-not-found is
established through metadata in the queue-scoped handler, not inferred globally from 42P01.

**Subscription ordering is atomic.** A snapshot enters the bounded outbox before its
subscription becomes eligible for updates. Generation checks discard in-flight results after
unsubscribe or replacement. `push` reports registered interest in notifications, not listener
health or installed triggers. Listener failure never disables polling; socket wait and
connection resources are released on cancellation.

**No mutations, no authentication.** The first iteration is read-only. The surface assumes a
trusted network or an authenticating reverse proxy, and its documentation says so rather than
implying safety that does not exist.

## Verification of the read boundary

ExecPlan 27 M4 verified actual upstream layouts on both PGMQ 1.12 and native 1.13,
using PostgreSQL 18.6 and pg_partman 5.5.0. With 100,000 rows per table, ordinary
and unlogged live/archive reads used msg_id indexes; numeric live reads used their
partition indexes. Numeric archives scanned the remaining default after pruning;
time live/archive parents scanned every occupied leaf for a msg_id page or lookup.
A late 50-row time page filtered 99,950 rows. The
[retained plans](../design/evidence/019-inspection-reads/README.md) establish why
row limits cannot substitute for database budgets and isolated inspection capacity.
These observations are not optimizer or latency guarantees. No new index is
recommended without measuring its write/storage costs and maintenance behavior.

Deterministic integration tests also pin the moving-view rule: a producer reserving
a smaller ID can commit after a larger cursor is returned, and archival can insert
an older ID behind an archive cursor. Restarting at the first page finds both;
continuing from that cursor does not. Exactly-once visitation is a property of a
quiescent fixture, not a replay guarantee under concurrent writes.

## HTTP runtime boundary

The implemented HTTP runtime owns one nonblocking read gate. Validate page limits
(including lookahead room), intervals, capacities and literal origins before allocation.
Parse IDs as bounded-length arbitrary-precision decimal before narrowing to int64.
Resolve queue identity from exact lenient metadata; recheck metadata after a scoped
42P01 rather than treating missing global tables as missing queues. Public errors
carry fixed messages and SQLSTATE only, never raw SQL, parameters or server diagnostics.

A host supplies a dedicated inspection runner with finite database statement/lock
budgets and separate capacity from workers. A scoped Warp runner binds the requested
host even on port 0 and races its startup acknowledgement against server failure.
Sockets, threads and application resources have bracketed ownership. Tests cover
saturated admission, deadline/worker isolation, prefix routing/upgrades and cleanup;
these do not establish a hard deadline for the separate IR-6 blackholed-reply problem.

## Shared feed scheduling and lifetime

The feed uses fixed runtime-wide workers, rather than a poller per client.
Atomic snapshot publication activates a subscription; generation tags and explicit
queue clearing invalidate queued frames after unsubscribe. A per-queue in-flight
guard and minimum start interval apply to polls, hints and repeated subscriptions.
Least-recently-started eligible work prevents a hot queue starving another.

After the last subscriber leaves, listener interest and cached metrics are removed.
A minimal cooldown lease remains through the minimum-refresh window so churn
cannot reset the floor. Active queues are capped at wsMaxQueues and total active
plus cooldown entries at twice that cap; exhausted cooldown capacity rejects with
inspection_busy. No historical queue map grows indefinitely.

Every JSON frame uses one bounded outbox/writer; overflow drops oldest frames and
signals before retained delivery. Runtime shutdown stops scheduling, attempts
Goodbye while Warp still runs, then cancels and joins transport tasks after a
bounded drain, including when the host owns Warp.

## Listener ownership and failure semantics

One configured runtime owns one dedicated LISTEN backend, with one interest per
distinct eligible queue and refcounted callbacks. It derives channels through
notifyChannelName, quotes identifiers and reconciles LISTEN/UNLISTEN from the
registry. Socket readiness and registry changes share a bracketed fd wait;
notifications are drained in batches of at most 64 before yielding/reconciling.
Synchronous connection failures reconnect with 100ms-to-5s backoff and reissue
all desired channels. Async cancellation releases fd waits and the connection.

Push means registered interest, independent of trigger existence, connection
health and channel acknowledgement. Per-channel acknowledgement is a separate
read-only runtime observable. Polls keep running after listener loss. A ready
hint and due poll share one authoritative read, tagged notify while advancing
the due poll deadline; neither driver creates a duplicate scan.

Lifecycle verification checks the actual owned socket descriptor and joined
thread, with a deterministic after-bind cancellation barrier. Rebinding a freed
ephemeral port is not reliable evidence: kernel state or another allocation can
outlive/reuse it. The scoped runner API remains unchanged.

Retained workload evidence shows that removing the per-client query multiplier
still has real cost. Connection/poll limits require worker SLO measurements;
separate pools isolate connection capacity, not database CPU/IO or process GC.
The uncancellable-driver test models bounded live work and eventual cleanup;
it does not establish a hard deadline for a blackholed network reply.

## Consequences

Adopting pgmq-hs never means adopting a web server; adopting `pgmq-inspect` never means
adopting keiro. The keiro console mounts the `Application`; a pgmq-hs user runs the executable
or mounts the same `Application` in their own server, and either can generate a client from the
served OpenAPI document. A future browser package shipped by pgmq-hs (`pgmq-ui` is the reserved
name) consumes exactly these artifacts and needs no change to the surface.

The cost is that shipping is forever: a field exposed in a release cannot be renamed, so every
new field is weighed before it ships. Adding constructors to the exported `Pgmq` effect for the
new reads is a breaking change for exhaustive interpreters and makes the family's next release
a major one.

The traced interpreter labels the new reads with this library's own `pgmq.peek`,
`pgmq.peek_archive`, `pgmq.lookup_message`, and `pgmq.lookup_archived_message` names, as it
already does for the catalog-backed `pgmq.list_fifo_indexes`.

## Alternatives

Building the HTTP surface over hasql sessions directly, as `kiroku-metrics` does, was rejected
because it would duplicate the error classification and tie the package to one interpreter.
Deriving routes and the OpenAPI document from a servant API type was rejected in favour of the
plain WAI routing both existing sister packages use, with the document built programmatically
and pinned by a golden file. A wildcard CORS origin was rejected as a representable
configuration. Carrying message bodies on the WebSocket feed was rejected because NOTIFY is
lossy and throttled, so the feed cannot be complete and must not look complete. Validated-only
names for the reads were rejected because an inspection surface must show what exists.

## Related decisions

See [design note 012](../design/012-vendor-upstream-pgmq-sql.md) (hand-written statements
are permitted for reads upstream lacks), [design note 013](../design/013-pgmq-effectful-error-model.md)
and [design note 017](../design/017-transient-error-classification.md) (the typed error model
behind the 503 mapping), [design note 015](../design/015-notification-delivery-contract.md)
(NOTIFY is a wake-up hint), [design note 016](../design/016-queue-name-validation.md) (the
lenient path for foreign names), and
[the dependency-bounds ADR](haskell-dependency-bounds-and-nix-pin-policy.md) (how the new
package declares what it is compatible with and what the Nix pin tests).
