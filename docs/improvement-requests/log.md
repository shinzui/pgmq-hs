# Bundle Update Log

## 2026-09-22
* **Addition**: Request an effective PGMQ call deadline after a blackholed PostgreSQL response; `tcp_user_timeout=5000` did not bound the call within ten seconds
* **Revision**: Add reproduced FIFO index SQLSTATE 23505 catalog race to the concurrent reconciliation request
* **Addition**: Request truthful per-resource action reports when concurrent reconciliation callers race
* **Addition**: Request retryable classification for disconnect-shaped statement errors observed during PostgreSQL crashes, backend termination, and TCP resets

## 2026-08-19
* **Addition**: Add a pgmq-metrics sister package with HTTP and WebSocket inspection endpoints (IR-3) filed from keiro-ui
* **Addition**: Provide JSON codecs for inspection-facing records (IR-2) filed from keiro-ui
* **Addition**: Expose non-destructive queue inspection reads (IR-1) filed from keiro-ui
