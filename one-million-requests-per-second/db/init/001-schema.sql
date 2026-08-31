CREATE TABLE IF NOT EXISTS products (
    id           bigint PRIMARY KEY,
    sku          text NOT NULL,
    name         text NOT NULL,
    category_id  integer NOT NULL,
    price_cents  integer NOT NULL,
    description  text NOT NULL,
    updated_at   timestamptz NOT NULL
);

