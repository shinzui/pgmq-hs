# Capability catalog log

## 2026-10-26
* **Update**: CAP-4: partitioned queues receive no insert notifications; post-crash duration stated

## 2026-10-20
* **Update**: CAP-12: standalone CLI, served prefix-safe OpenAPI, exact-int64 client and response-schema acceptance, restricted-role tests and owned adoption guide (ExecPlan 31)
* **Update**: CAP-12: shared WebSocket metrics feed, reconnecting listener and concurrency/fault/workload evidence (ExecPlan 30)
* **Addition**: CAP-12: HTTP queue inspection surface in pgmq-inspect (ExecPlan 29)
* **Addition**: CAP-11: stable domain JSON codecs with normalized arbitrary headers, exact int64 fixtures and 53 reviewed golden files (ExecPlan 28)
* **Update**: CAP-10: document moving-view paging and inspection cost/isolation limits; add real partition workload evidence

## 2026-10-01
* **Addition**: CAP-10: non-destructive queue inspection reads (peek, archive peek, lookups, lenient metrics) across pgmq-hasql and pgmq-effectful, since unreleased (ExecPlan 27)
* **Revision**: CAP-5: revise the isTransient limits, add isAmbiguousReply, and add DisconnectSpec as evidence (ExecPlan 26)

## 2026-08-08
* **Adopt**: Authored the initial capability catalog for `pgmq-hs` under the shared
`coordination.capabilities` profile (okf-profiles v0.9.0). Derived nine capabilities
(CAP-1 … CAP-9) from the package set, the exported module surface, the test suites, the
user guides and design notes, and the changelogs at version 0.5.0.0 — grouped by adoption
decision rather than by module or function. Registered the bundle in `mori.dhall` under
`okfBundles`. Records are machine-authored; no `reviews` or `verified` provenance is
claimed.
* **Gaps found**: FIFO message-group reads (CAP-2) are shipped and tested but are not
re-exported by either convenience umbrella, so a consumer must import
`Pgmq.Hasql.Sessions` / `Pgmq.Hasql.Statements.Types` or `Pgmq.Effectful.Effect`
directly. `parseTopicPattern` (CAP-3) validates only length, not wildcard structure. The
`docs/user/queue-configuration.md` "every operation is idempotent" claim (CAP-9) is
imprecise against the additive-reconciliation contract. Insert notifications (CAP-4) are
crash-safe only on pgmq-migration installs, not on stock upstream extension installs. The
`pgmq-migration` schema contract (CAP-8) checks object structure, not function bodies.
