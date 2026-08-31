use std::{env, str::FromStr};

use sqlx::{Row, postgres::PgPoolOptions};

const CREATE_SCHEMA: &str = r#"
CREATE TABLE IF NOT EXISTS products (
    id           bigint PRIMARY KEY,
    sku          text NOT NULL,
    name         text NOT NULL,
    category_id  integer NOT NULL,
    price_cents  integer NOT NULL,
    description  text NOT NULL,
    updated_at   timestamptz NOT NULL
)
"#;

const LOAD_RANGE: &str = r#"
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
FROM generate_series($1::bigint, $2::bigint) AS generated(id)
ON CONFLICT (id) DO NOTHING
"#;

#[tokio::main]
async fn main() {
    if let Err(error) = run().await {
        eprintln!("database initialization failed: {error}");
        std::process::exit(1);
    }
}

async fn run() -> Result<(), Box<dyn std::error::Error>> {
    let config = Config::parse()?;
    let pool = PgPoolOptions::new()
        .max_connections(1)
        .connect(&config.database_url)
        .await?;

    if config.stats {
        print_stats(&pool).await?;
        return Ok(());
    }

    sqlx::query(CREATE_SCHEMA).execute(&pool).await?;
    sqlx::query("TRUNCATE TABLE products").execute(&pool).await?;

    let mut range_start = 1_u64;
    while range_start <= config.product_count {
        let range_end = range_start
            .saturating_add(config.batch_size - 1)
            .min(config.product_count);
        println!("loading products {range_start} through {range_end}");
        sqlx::query(LOAD_RANGE)
            .bind(i64::try_from(range_start)?)
            .bind(i64::try_from(range_end)?)
            .execute(&pool)
            .await?;
        range_start = range_end + 1;
    }

    sqlx::query("VACUUM (ANALYZE) products")
        .execute(&pool)
        .await?;
    let (products, total_size): (i64, String) = sqlx::query_as(
        "SELECT count(*), pg_size_pretty(pg_total_relation_size('products')) FROM products",
    )
    .fetch_one(&pool)
    .await?;
    if products != i64::try_from(config.product_count)? {
        return Err(format!(
            "loaded {products} products, expected {}",
            config.product_count
        )
        .into());
    }
    println!("loaded {products} products ({total_size})");
    Ok(())
}

async fn print_stats(pool: &sqlx::PgPool) -> Result<(), Box<dyn std::error::Error>> {
    let row = sqlx::query(
        r#"
        SELECT
            datname,
            xact_commit,
            xact_rollback,
            blks_read,
            blks_hit,
            tup_returned,
            tup_fetched,
            tup_inserted,
            tup_updated,
            tup_deleted,
            conflicts,
            temp_files,
            temp_bytes,
            deadlocks,
            coalesce(stats_reset::text, '') AS stats_reset
        FROM pg_stat_database
        WHERE datname = current_database()
        "#,
    )
    .fetch_one(pool)
    .await?;
    let output = serde_json::json!({
        "database": row.try_get::<String, _>("datname")?,
        "transactions_committed": row.try_get::<i64, _>("xact_commit")?,
        "transactions_rolled_back": row.try_get::<i64, _>("xact_rollback")?,
        "blocks_read": row.try_get::<i64, _>("blks_read")?,
        "blocks_hit": row.try_get::<i64, _>("blks_hit")?,
        "tuples_returned": row.try_get::<i64, _>("tup_returned")?,
        "tuples_fetched": row.try_get::<i64, _>("tup_fetched")?,
        "tuples_inserted": row.try_get::<i64, _>("tup_inserted")?,
        "tuples_updated": row.try_get::<i64, _>("tup_updated")?,
        "tuples_deleted": row.try_get::<i64, _>("tup_deleted")?,
        "conflicts": row.try_get::<i64, _>("conflicts")?,
        "temporary_files": row.try_get::<i64, _>("temp_files")?,
        "temporary_bytes": row.try_get::<i64, _>("temp_bytes")?,
        "deadlocks": row.try_get::<i64, _>("deadlocks")?,
        "stats_reset": row.try_get::<String, _>("stats_reset")?,
    });
    println!("{}", serde_json::to_string_pretty(&output)?);
    Ok(())
}

struct Config {
    database_url: String,
    product_count: u64,
    batch_size: u64,
    stats: bool,
}

impl Config {
    fn parse() -> Result<Self, String> {
        let mut product_count = 10_000_000_u64;
        let mut batch_size = 100_000_u64;
        let mut stats = false;
        let mut args = env::args().skip(1);

        while let Some(flag) = args.next() {
            if flag == "--help" || flag == "-h" {
                println!(
                    "million-rps-dbtool\n\n  --product-count N  rows to load (default: 10000000)\n  --batch-size N     rows per statement (default: 100000)\n  --stats            print pg_stat_database counters without loading"
                );
                std::process::exit(0);
            }
            if flag == "--stats" {
                stats = true;
                continue;
            }
            let value = args
                .next()
                .ok_or_else(|| format!("missing value for {flag}"))?;
            match flag.as_str() {
                "--product-count" => product_count = parse(&flag, &value)?,
                "--batch-size" => batch_size = parse(&flag, &value)?,
                _ => return Err(format!("unknown option {flag}")),
            }
        }

        if product_count == 0 || batch_size == 0 || product_count > i64::MAX as u64 {
            return Err("product count and batch size must be positive and fit in bigint".into());
        }
        let database_url =
            env::var("DATABASE_URL").map_err(|_| "DATABASE_URL must be set".to_owned())?;

        Ok(Self {
            database_url,
            product_count,
            batch_size,
            stats,
        })
    }
}

fn parse<T>(flag: &str, value: &str) -> Result<T, String>
where
    T: FromStr,
    T::Err: std::fmt::Display,
{
    value
        .parse()
        .map_err(|error| format!("invalid {flag}: {error}"))
}
