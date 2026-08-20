---
okf_version: "0.2"
---

# Files

- [profile.dhall](profile.dhall) - Pinned improvement-request profile descriptor.
- [log.md](log.md) - Bundle update log.

# Improvement Request

- [Add a pgmq-metrics sister package with HTTP and WebSocket inspection endpoints](add-a-pgmq-metrics-sister-package-with-http-and-websocket-inspection-endpoints.md) - Create a sister package exposing an embeddable WAI application with queue listing, metrics, non-destructive browsing, topic bindings, health, CORS, and a NOTIFY-accelerated but poll-authoritative WebSocket live feed.
- [Expose non-destructive queue inspection reads](expose-non-destructive-queue-inspection-reads.md) - Add bounded, keyset-paginated reads that observe queue and archive contents without mutating visibility timeouts or read counts — peek, archive browsing, and fetch-by-message-id.
- [Provide JSON codecs for inspection-facing records](provide-json-codecs-for-inspection-facing-records.md) - Add aeson instances with a documented, stable field-naming policy to the domain records an inspection wire format needs.
