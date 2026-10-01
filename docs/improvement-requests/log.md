# Bundle Update Log

## 2026-10-01
* **Update**: IR-1, IR-2, and IR-3 accepted; implementation planned under MasterPlan 7 (docs/masterplans/7-expose-non-destructive-queue-inspection-through-typed-reads-json-codecs-and-the-pgmq-inspect-sister-package.md) as ExecPlans 27 through 31, with the sister package named pgmq-inspect

## 2026-09-22
* **Addition**: Request an effective PGMQ call deadline after a blackholed PostgreSQL response; `tcp_user_timeout=5000` did not bound the call within ten seconds
* **Revision**: Add reproduced FIFO index SQLSTATE 23505 catalog race to the concurrent reconciliation request
* **Addition**: Request truthful per-resource action reports when concurrent reconciliation callers race
* **Addition**: Request retryable classification for disconnect-shaped statement errors observed during PostgreSQL crashes, backend termination, and TCP resets

## 2026-08-19
* **Addition**: Add a pgmq-metrics sister package with HTTP and WebSocket inspection endpoints (IR-3) filed from keiro-ui
* **Addition**: Provide JSON codecs for inspection-facing records (IR-2) filed from keiro-ui
* **Addition**: Expose non-destructive queue inspection reads (IR-1) filed from keiro-ui
