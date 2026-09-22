---
type: Improvement Request
title: Bound PGMQ calls when a PostgreSQL response is blackholed
description: >-
  Document and provide an effective deadline for PGMQ calls whose request reaches a TCP proxy but whose response is discarded, including when libpq tcp_user_timeout is configured.
generated:
  by: openai/gpt-6-sol
  at: "2026-09-22T22:39:31Z"
reviews: []
requestId: IR-6
status: proposed
origin: mori://shinzui/keiro-runtime-kenshou
---

# Improvement Request: Bound PGMQ Calls When a PostgreSQL Response Is Blackholed

## Problem

A PGMQ operation can remain blocked when its PostgreSQL response is discarded by a TCP proxy. The reproducer uses a separate worker process because canceling the in-process database call waited for its non-interruptible I/O. With the default libpq connection settings, the call did not return within five seconds on PostgreSQL 17 or 18. Setting `tcp_user_timeout=5000` through Hasql's `Connection.other` still did not make it return within ten seconds on either version. This observation is consistent with the request bytes being acknowledged while the response is blackholed; it does not establish the exact driver or kernel cause. The harness killed the worker at the bound, healed the proxy, reused the pool for a later send, and verified that both confirmed sends remained durable.

The reproducer is `mori://shinzui/keiro-runtime-kenshou/plans/8-cover-pgmq-hs-in-isolation`, scenario `pgmq/effectful/concurrency/network-blackhole`. PostgreSQL 18 run `01a0cb45-01c0-722a-b3e3-bb3c899c5d80` and PostgreSQL 17 run `01a0cb45-d3e7-7182-9dc4-a03774192865` each exceeded ten seconds with `tcp_user_timeout=5000`. The five-second default-setting runs were `01a0cb43-f896-7260-bd2f-4522c619377a` and `01a0cb45-7444-7202-b320-d3f4d070cfa5`.

## Requested Change

Provide a documented, configurable per-operation or connection deadline that bounds a PGMQ call when the server's response does not arrive. Explain the relationship between that deadline, libpq's `tcp_user_timeout`, pool acquisition timeout, PostgreSQL statement timeout, and a proxy that silently discards response bytes. Make the timeout outcome identifiable and retryable where the operation's commit status is ambiguous, while leaving the retry decision to the caller.

## Acceptance

1. A fault test blackholes only the response path after a PGMQ request and proves that the call returns within its configured deadline without relying on the harness to kill its process.
2. The same pool completes a later operation after the proxy heals, and durable identifiers distinguish confirmed operations from an interrupted operation with unknown commit status.
3. Documentation states which timeout settings do and do not bound this response-blackhole case, including the observed limit of `tcp_user_timeout`.

## Non-goals

This request does not silently replay a non-idempotent operation whose reply was lost. It does not change the transient classifier defect tracked by `mori://shinzui/pgmq-hs/okf/improvement-requests/concepts/IR-4`.
