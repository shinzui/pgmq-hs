# Capability catalog log

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
