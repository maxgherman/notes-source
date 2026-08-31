#!/bin/sh
set -eu

product_count="${1:-100000}"
batch_size="${2:-100000}"

case "$product_count:$batch_size" in
    *[!0-9:]*|0:*|*:0)
        echo "usage: $0 [positive-product-count] [positive-batch-size]" >&2
        exit 2
        ;;
esac

range_start=1
while [ "$range_start" -le "$product_count" ]; do
    range_end=$((range_start + batch_size - 1))
    if [ "$range_end" -gt "$product_count" ]; then
        range_end="$product_count"
    fi

    echo "loading products $range_start through $range_end"
    docker compose exec -T postgres psql \
        --username benchmark \
        --dbname benchmark \
        --set "range_start=$range_start" \
        --set "range_end=$range_end" \
        --file /benchmark-db/load-product-range.sql

    range_start=$((range_end + 1))
done

docker compose exec -T postgres psql \
    --username benchmark \
    --dbname benchmark \
    --command 'VACUUM (ANALYZE) products;'

docker compose exec -T postgres psql \
    --username benchmark \
    --dbname benchmark \
    --tuples-only \
    --command "SELECT count(*) AS products, pg_size_pretty(pg_total_relation_size('products')) AS total_size FROM products;"

