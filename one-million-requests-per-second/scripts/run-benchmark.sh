#!/bin/sh
set -eu

benchmark_target="${BENCHMARK_TARGET:-http://server:8080}"
environment_description="${BENCHMARK_ENVIRONMENT:-local Docker Compose}"
result_file="${RESULT_FILE:-run-$(date -u +%Y%m%dT%H%M%SZ).json}"
git_commit="${BENCHMARK_GIT_COMMIT:-$(git rev-parse HEAD 2>/dev/null || echo unknown)}"
image_digest="${BENCHMARK_IMAGE_DIGEST:-unknown}"
cache_capacity_bytes="${BENCHMARK_CACHE_CAPACITY_BYTES:-134217728}"
cache_ttl_seconds="${BENCHMARK_CACHE_TTL_SECONDS:-3600}"
BENCHMARK_UID="${BENCHMARK_UID:-$(id -u)}"
BENCHMARK_GID="${BENCHMARK_GID:-$(id -g)}"
export BENCHMARK_UID BENCHMARK_GID

set -- \
    --target "$benchmark_target" \
    --git-commit "$git_commit" \
    --image-digest "$image_digest" \
    --environment "$environment_description" \
    --server-cache-capacity-bytes "$cache_capacity_bytes" \
    --server-cache-ttl-seconds "$cache_ttl_seconds" \
    --output "/results/$result_file" \
    "$@"

if [ -n "${BENCHMARK_CA_CERT:-}" ]; then
    set -- "$@" --ca-cert "$BENCHMARK_CA_CERT"
fi

mkdir -p results
docker compose --profile benchmark run --rm loadgen "$@"
