# Queue inspection lives in a runnable sister package with a pgmq-hs-owned wire contract

## Status

Accepted, 2026-09-30. Implementation is tracked by
[MasterPlan 7](../masterplans/7-expose-non-destructive-queue-inspection-through-typed-reads-json-codecs-and-the-pgmq-inspect-sister-package.md).
This repository has no profiled ADR bundle; this record follows its existing plain-Markdown
decision convention and introduces no OKF metadata.

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

Every read the library offers today leases: `pgmq.read`, `read_with_poll`, and `pop` mutate `vt`
and `read_ct`. Upstream pgmq provides no non-destructive read, no archive read, and no
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

**Inspection reads accept any server-accepted name and never lease.** The peek, archive, and
lookup reads take the queue name as plain `Text`, resolve the physical table through
`pgmq.format_table_name` on the server, and run an unprepared `SELECT` ordered by `msg_id` with
an exclusive cursor and `LIMIT`, never `OFFSET`. They modify no row. A missing message is a typed
`Nothing`; a missing queue is the server's `42P01`. No upstream function body is overridden and
no migration is added.

**Push is a hint; the poll is truth.** The WebSocket feed carries queue metrics, not message
bodies. One server-wide LISTEN connection accelerates updates for queues whose names validate;
every subscribed queue is also polled on an interval; the feed reconnects and re-`LISTEN`s on
its own and never stops polling. A client re-reads messages through the paged HTTP routes.

**No mutations, no authentication.** The first iteration is read-only. The surface assumes a
trusted network or an authenticating reverse proxy, and its documentation says so rather than
implying safety that does not exist.

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
