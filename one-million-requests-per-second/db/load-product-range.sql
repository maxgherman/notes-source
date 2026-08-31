\set ON_ERROR_STOP on

INSERT INTO products (
    id,
    sku,
    name,
    category_id,
    price_cents,
    description,
    updated_at
)
SELECT
    id,
    'SKU-' || lpad(id::text, 12, '0'),
    'Product ' || id,
    1 + (id % 1000),
    100 + ((id * 37) % 100000),
    repeat(md5(id::text), 28),
    timestamptz '2026-01-01 00:00:00+00'
        + ((id % 365) * interval '1 day')
FROM generate_series(:range_start, :range_end) AS generated(id)
ON CONFLICT (id) DO NOTHING;

