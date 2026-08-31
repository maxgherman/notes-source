# One million requests per second

This project contains the executable benchmark behind the blog post. Its first
goal is not one million requests per second. Its first goal is a reproducible
single-server measurement that tells us how many servers the complete system
needs.

The serving path is:

```text
client -> HTTP/TLS server -> bounded local cache -> PostgreSQL on a miss
```

The benchmark rules are frozen in
[`docs/benchmark-contract.md`](docs/benchmark-contract.md). Change that document
before changing the workload; otherwise two runs may look comparable when they
are not.

## Project status

The benchmark harness contains:

- a Rust HTTP service exposing `GET /products/{id}`
- a byte-bounded, expiring in-process cache
- coalesced concurrent cache loads
- PostgreSQL reads on cache misses
- negative caching for missing products
- Prometheus-format service metrics at `GET /metrics`
- optional TLS 1.3 termination in the Rust service
- an open-loop, deterministic HTTP/1.1 load generator
- full response-body validation and schedule-to-completion latency reporting
- machine-readable benchmark result files
- deterministic schema and data-loading scripts
- a local Docker Compose environment
- an AWS single-server environment with distributed EC2 generators and RDS

Plain HTTP remains available for local diagnostics. Comparable fleet benchmarks
use TLS and the exact workload in the benchmark contract.

## Run locally

The local workflow requires Docker with the Compose plugin:

```sh
docker compose up --build -d postgres
./scripts/load-products.sh 100000
docker compose up --build server
```

Then request a product and inspect metrics:

```sh
curl -i http://127.0.0.1:8080/products/42
curl http://127.0.0.1:8080/metrics
```

The first product request should be a cache miss and the second should be a
cache hit. An ID above the loaded range returns `404` and is negatively cached.

## Run the benchmark

The reference generator owns all benchmark phases: readiness, a five-minute
linear warm-up ramp, a thirty-minute measurement, and cool-down while queued
requests finish. The ramp starts at the lesser of `1,000 RPS` and the target,
then reaches the target as measurement begins. Its defaults otherwise match the
frozen contract, except that the target rate defaults to a safe local
`1,000 RPS`.

For a short HTTP smoke run against Compose:

```sh
./scripts/run-benchmark.sh \
  --product-count 100000 \
  --warmup-seconds 10 \
  --duration-seconds 30 \
  --rate 1000 \
  --connections 64
```

Results are written under `results/`. A process exit status of `2` means the
generator saturated or the completion-rate, error-rate, or p99 threshold was
not met. The JSON still records the failed run.

For a local TLS run, create a private seven-day test certificate for the Compose
service name and restart the server in TLS mode:

```sh
./scripts/generate-test-certs.sh server
TLS_CERT_PATH=/certs/server.crt \
TLS_KEY_PATH=/certs/server.key \
docker compose up --build -d server

BENCHMARK_TARGET=https://server:8080 \
BENCHMARK_CA_CERT=/certs/ca.crt \
./scripts/run-benchmark.sh \
  --product-count 100000 \
  --warmup-seconds 10 \
  --duration-seconds 30
```

The generator creates one sequential HTTP/1.1 request stream per connection;
it never pipelines. Arrivals are scheduled independently of completions. Each
body is read and checked against the deterministic database record, and latency
starts at the intended arrival time so connection queues remain visible.

Use `--warmup-start-rate` to override the default ramp start. Both the warm-up
start and end rates are stored in the JSON result. Comparable runs must use the
same ramp configuration.

Use `docker compose --profile benchmark run --rm loadgen --help` for every
configuration option. AWS runs should always provide the source commit, image
digest, and an instance/AZ description through the corresponding flags or the
`BENCHMARK_GIT_COMMIT`, `BENCHMARK_IMAGE_DIGEST`, and
`BENCHMARK_ENVIRONMENT` environment variables.

The reproducible AWS workflow is documented in
[`infra/aws/README.md`](infra/aws/README.md). It provisions no ECS services and
opens no SSH port: the TypeScript CDK app synthesizes CloudFormation for EC2
benchmark hosts, RDS PostgreSQL, ECR and SSM permissions, while
`scripts/aws/benchmark.py` controls the complete lifecycle.

## Configuration

| Variable | Default | Meaning |
| --- | ---: | --- |
| `LISTEN_ADDR` | `0.0.0.0:8080` | HTTP listen address |
| `DATABASE_URL` | required | PostgreSQL connection URL |
| `DATABASE_MAX_CONNECTIONS` | `32` | Maximum PostgreSQL pool size |
| `CACHE_CAPACITY_BYTES` | `536870912` | Cache body-byte budget (512 MiB) |
| `CACHE_TTL_SECONDS` | `3600` | Base positive-entry TTL |
| `CACHE_TTL_JITTER_PERCENT` | `20` | Deterministic per-key TTL spread |
| `NEGATIVE_CACHE_TTL_SECONDS` | `30` | Missing-product TTL |
| `TLS_CERT_PATH` | unset | PEM certificate chain; enables TLS when set with `TLS_KEY_PATH` |
| `TLS_KEY_PATH` | unset | PEM private key; enables TLS when set with `TLS_CERT_PATH` |

The cache capacity covers response bodies plus a small per-entry allowance.
`scripts/run-benchmark.sh` records the Compose overrides of 128 MiB and 3,600
seconds. Set `BENCHMARK_CACHE_CAPACITY_BYTES` or
`BENCHMARK_CACHE_TTL_SECONDS` when targeting a differently configured server so
the result remains self-describing.

Service metrics now include request/status/latency counters, cache hits, misses,
size evictions and coalesced loads, configured cache TTL, database query
latency, accepted and active connections, and TLS handshakes, failures, and
resumptions.
