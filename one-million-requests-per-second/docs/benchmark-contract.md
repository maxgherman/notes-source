# Benchmark contract

Contract version: `6`

This document defines when two benchmark runs are comparable. Every result must
record the Git commit, container image digest, configuration, AWS instance
types, Availability Zones, and the values below.

## Workload

- Endpoint: `GET /products/{id}`
- Successful response: uncompressed JSON, approximately 1 KiB
- Product population: 10,000,000 deterministic rows for AWS runs
- Access distribution: Zipf over the product population
- Zipf exponent: `1.1`
- Random seed: `104729` for one generator. A distributed run uses
  `104729 + shard_index * 1000003`; the shard count and every derived seed are
  recorded.
- Unknown-product requests: `0.1%`
- Protocol for the first fleet benchmark: HTTP/1.1 over TLS 1.3
- Connections: persistent; no request pipelining
- Pending work: a bounded queue of 64 requests per connection. Queued work
  retains its original open-loop schedule timestamp and is included in
  schedule-to-completion latency.
- Response handling: every body is read and validated
- Compression: disabled

The load generator must use an open-loop arrival schedule and report coordinated
omission-corrected latency. If it cannot sustain the requested arrival rate, the
run fails rather than silently becoming a lower-rate closed-loop test.

The reference generator measures schedule-to-completion latency, including time
spent waiting for a connection worker. This directly exposes coordinated
omission. Its latency histogram uses logarithmic buckets with a maximum relative
width of 1%; result files must record that width.

## Cache

- Storage: serialized response bytes in each server process
- Capacity: explicitly configured byte budget
- Eviction: weighted least-recently-used approximation supplied by the cache
  implementation
- Positive TTL: 3,600 seconds with deterministic ±20% per-key jitter
- Negative TTL: 30 seconds
- Concurrent loads for one absent key: coalesced into one database query

Cache capacity and positive TTL are test parameters and must be present in every
result. Warm-up does not preload a synthetic hot set; the measured request
distribution warms the cache.

Version 2 replaces the abrupt full-rate warm-up from version 1 with a linear
open-loop ramp. The default ramp begins at the lesser of `1,000 RPS` and the
measurement rate, and reaches the measurement rate at the end of warm-up. A
different positive starting rate is allowed when it does not exceed the target,
but both endpoints must be recorded in the result. Arrival times are obtained by
inverting the cumulative integral of the linear rate, not by closing the loop on
response completion.

Version 3 adds synchronized distributed generation. Every shard finishes
readiness checks, constructs its distribution and workers, and then waits for a
common UTC epoch second before starting its ramp. After draining warm-up work,
every shard waits for a second common UTC epoch before measurement, leaving a
fixed transition interval for the slowest valid shard. The machines must use
time-synchronized clocks. A shard that misses either epoch invalidates the run.
The aggregate target rate, connections and ramp start are divided evenly among
shards; a run with indivisible values is rejected. Shard streams use the
deterministic seeds above so generators do not replay an identical request
sequence in lockstep. Distributed runs are comparable only when their shard
count and transition interval are the same.

Version 4 increases the bounded pending-work queue from 2 to 16 requests per
connection. At the reference one-million-RPS rate and 4,096 aggregate
connections, this holds approximately 65 ms of scheduled arrivals, slightly
more than the 50 ms p99 limit. This prevents rare permitted tail events from
invalidating a run through generator saturation while preserving their full
schedule-to-completion latency. The queue remains bounded, and any saturation
still invalidates the run.

Version 5 increases the bounded queue from 16 to 64 requests per connection.
At the reference rate and connection count, this holds approximately 262 ms of
scheduled arrivals. Queued requests retain their original timestamps, so queue
wait remains visible in schedule-to-completion latency and the 50 ms p99 limit
still applies. The reference generator also resolves the target once before
creating workers and reuses that socket address for reconnects while retaining
the hostname for TLS SNI and the HTTP `Host` header. This prevents reconnects
from exhausting the EC2 link-local allowance through repeated DNS queries.

Version 6 increases the positive-entry TTL from 300 to 3,600 seconds and makes
the configured TTL part of every shard result and aggregate manifest. The
five-minute TTL expired entries loaded during the reference warm-up before the
measurement began, creating a database reload wave rather than measuring the
intended cache-backed serving path. With deterministic ±20% jitter, the v6 TTL
keeps entries loaded during the warm-up alive for the complete 30-minute
measurement while continuing to bound staleness.

## Run phases

1. Readiness: database and all serving targets pass health checks.
2. Warm-up: 5-minute linear arrival-rate ramp, excluded from reported results.
3. Measurement: 30 consecutive minutes.
4. Cool-down: stop new arrivals and allow in-flight requests to finish.

Before the fleet test, one server is measured in two modes:

1. a cache-hit ceiling test that isolates HTTP, TLS, and response transmission;
2. the complete Zipf workload with PostgreSQL cache misses.

## Fleet success criteria

- At least 1,000,000 completed responses per second throughout the measured
  window
- Client-observed p99 latency below 50 ms
- Fewer than 0.1% transport errors and non-2xx responses, excluding intentional
  unknown-product requests
- Every response body read and validated
- The run remains successful after removing any one serving target

## Required observations

The load generators record offered rate, completed rate, status codes, transport
errors, connection counts, response bytes, and p50/p90/p99/p99.9/max latency.

Each server records CPU, memory, network bytes and packets, accepted connections,
TLS handshakes and resumptions, cache hits, misses, evictions, current cache
weight, coalesced loads, database query count, and database latency.

PostgreSQL records queries per second, active connections, CPU, memory, read I/O,
buffer-cache hit rate, transaction latency, and storage latency/queue depth.

## Invalid runs

A result is invalid if the workload parameters changed without a new contract
version, a generator saturated, bodies were not read, validation was sampled
instead of applied to every response, metrics collection failed, a serving
target was unhealthy before measurement, or the client and server clocks were
not synchronized.
