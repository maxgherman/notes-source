mod config;
mod metrics;

use std::{fs::File, io::BufReader, sync::Arc, time::Instant};

use axum::{
    Json, Router,
    body::Body,
    extract::{Path, State},
    http::{StatusCode, header},
    response::{IntoResponse, Response},
    routing::get,
};
use bytes::Bytes;
use config::Config;
use hyper::server::conn::http1;
use hyper_util::{rt::TokioIo, service::TowerToHyperService};
use metrics::Metrics;
use moka::{Expiry, future::Cache, notification::RemovalCause};
use rustls::ServerConfig;
use serde::Serialize;
use sqlx::{FromRow, PgPool, postgres::PgPoolOptions};
use thiserror::Error;
use tokio::io::{AsyncRead, AsyncWrite};
use tokio_rustls::TlsAcceptor;

#[derive(Clone)]
struct AppState {
    pool: PgPool,
    cache: Cache<u64, Arc<CachedResponse>>,
    config: Config,
    metrics: Arc<Metrics>,
}

#[derive(Debug)]
struct CachedResponse {
    status: StatusCode,
    body: Bytes,
    expires_after: std::time::Duration,
}

struct ResponseExpiry;

impl Expiry<u64, Arc<CachedResponse>> for ResponseExpiry {
    fn expire_after_create(
        &self,
        _key: &u64,
        response: &Arc<CachedResponse>,
        _created_at: Instant,
    ) -> Option<std::time::Duration> {
        Some(response.expires_after)
    }
}

#[derive(Debug, FromRow, Serialize)]
struct Product {
    id: i64,
    sku: String,
    name: String,
    category_id: i32,
    price_cents: i32,
    description: String,
    updated_at: String,
}

#[derive(Debug, Error)]
enum LoadError {
    #[error("database query failed: {0}")]
    Database(#[from] sqlx::Error),
    #[error("response serialization failed: {0}")]
    Serialization(#[from] serde_json::Error),
}

#[tokio::main]
async fn main() {
    if let Err(error) = run().await {
        eprintln!("server failed: {error}");
        std::process::exit(1);
    }
}

async fn run() -> Result<(), Box<dyn std::error::Error>> {
    let config = Config::from_env()?;
    let pool = PgPoolOptions::new()
        .max_connections(config.database_max_connections)
        .connect(&config.database_url)
        .await?;

    let service_metrics = Arc::new(Metrics::default());
    let eviction_metrics = service_metrics.clone();
    let cache = Cache::builder()
        .max_capacity(config.cache_capacity_bytes)
        .weigher(|_id: &u64, response: &Arc<CachedResponse>| {
            response
                .body
                .len()
                .saturating_add(128)
                .min(u32::MAX as usize) as u32
        })
        .eviction_listener(move |_id, _response, cause| {
            if cause == RemovalCause::Size {
                eviction_metrics.cache_eviction();
            }
        })
        .expire_after(ResponseExpiry)
        .build();

    let state = AppState {
        pool,
        cache,
        config: config.clone(),
        metrics: service_metrics.clone(),
    };

    let app = Router::new()
        .route("/products/{id}", get(get_product))
        .route("/health/ready", get(ready))
        .route("/metrics", get(metrics))
        .with_state(state);

    let listener = tokio::net::TcpListener::bind(config.listen_addr).await?;
    let tls_config = load_tls_config(&config)?;
    let scheme = if tls_config.is_some() {
        "https"
    } else {
        "http"
    };
    println!("listening on {scheme}://{}", config.listen_addr);
    serve(listener, app, tls_config, service_metrics).await?;

    Ok(())
}

async fn get_product(
    State(state): State<AppState>,
    Path(id): Path<u64>,
) -> Result<Response, StatusCode> {
    let started = Instant::now();
    state.metrics.request();

    let result = get_product_inner(&state, id).await;
    let status = match &result {
        Ok(response) => response.status().as_u16(),
        Err(status) => status.as_u16(),
    };
    state.metrics.response(status, started.elapsed());
    result
}

async fn get_product_inner(state: &AppState, id: u64) -> Result<Response, StatusCode> {
    if let Some(response) = state.cache.get(&id).await {
        state.metrics.cache_hit();
        return Ok(to_http_response(response));
    }

    state.metrics.cache_miss();
    let load_state = state.clone();
    let response = state
        .cache
        .try_get_with(id, async move { load_product(&load_state, id).await })
        .await
        .map_err(|error| {
            eprintln!("product {id} load failed: {error}");
            StatusCode::INTERNAL_SERVER_ERROR
        })?;

    Ok(to_http_response(response))
}

fn load_tls_config(
    config: &Config,
) -> Result<Option<Arc<ServerConfig>>, Box<dyn std::error::Error>> {
    let (Some(cert_path), Some(key_path)) = (&config.tls_cert_path, &config.tls_key_path) else {
        return Ok(None);
    };

    let mut cert_reader = BufReader::new(File::open(cert_path)?);
    let certs = rustls_pemfile::certs(&mut cert_reader).collect::<Result<Vec<_>, _>>()?;
    if certs.is_empty() {
        return Err(format!("no certificates found in {}", cert_path.display()).into());
    }

    let mut key_reader = BufReader::new(File::open(key_path)?);
    let key = rustls_pemfile::private_key(&mut key_reader)?
        .ok_or_else(|| format!("no private key found in {}", key_path.display()))?;

    let provider = rustls::crypto::ring::default_provider();
    let tls_config = ServerConfig::builder_with_provider(Arc::new(provider))
        .with_protocol_versions(&[&rustls::version::TLS13])?
        .with_no_client_auth()
        .with_single_cert(certs, key)?;
    Ok(Some(Arc::new(tls_config)))
}

async fn serve(
    listener: tokio::net::TcpListener,
    app: Router,
    tls_config: Option<Arc<ServerConfig>>,
    metrics: Arc<Metrics>,
) -> Result<(), Box<dyn std::error::Error>> {
    let tls_acceptor = tls_config.map(TlsAcceptor::from);
    let mut connections = tokio::task::JoinSet::new();
    let shutdown = shutdown_signal();
    tokio::pin!(shutdown);

    loop {
        tokio::select! {
            _ = &mut shutdown => break,
            accepted = listener.accept() => {
                let (stream, peer) = accepted?;
                metrics.connection_accepted();
                let guard = ConnectionGuard(metrics.clone());
                let connection_app = app.clone();
                let acceptor = tls_acceptor.clone();
                let connection_metrics = metrics.clone();
                connections.spawn(async move {
                    let _guard = guard;
                    if let Some(acceptor) = acceptor {
                        match acceptor.accept(stream).await {
                            Ok(tls_stream) => {
                                let resumed = matches!(
                                    tls_stream.get_ref().1.handshake_kind(),
                                    Some(rustls::HandshakeKind::Resumed)
                                );
                                connection_metrics.tls_handshake(resumed);
                                serve_http_connection(tls_stream, connection_app).await;
                            }
                            Err(error) => {
                                connection_metrics.tls_handshake_error();
                                eprintln!("TLS handshake from {peer} failed: {error}");
                            }
                        }
                    } else {
                        serve_http_connection(stream, connection_app).await;
                    }
                });
            }
        }

        while connections.try_join_next().is_some() {}
    }

    connections.shutdown().await;
    Ok(())
}

async fn serve_http_connection<S>(stream: S, app: Router)
where
    S: AsyncRead + AsyncWrite + Unpin + Send + 'static,
{
    let service = TowerToHyperService::new(app);
    if let Err(error) = http1::Builder::new()
        .keep_alive(true)
        .serve_connection(TokioIo::new(stream), service)
        .await
    {
        eprintln!("HTTP connection failed: {error}");
    }
}

struct ConnectionGuard(Arc<Metrics>);

impl Drop for ConnectionGuard {
    fn drop(&mut self) {
        self.0.connection_closed();
    }
}

async fn load_product(state: &AppState, id: u64) -> Result<Arc<CachedResponse>, LoadError> {
    let product = match i64::try_from(id) {
        Ok(database_id) => {
            state.metrics.database_query();
            let started = Instant::now();
            let product = sqlx::query_as::<_, Product>(
                r#"
                SELECT
                    id,
                    sku,
                    name,
                    category_id,
                    price_cents,
                    description,
                    to_char(updated_at AT TIME ZONE 'UTC', 'YYYY-MM-DD"T"HH24:MI:SS"Z"') AS updated_at
                FROM products
                WHERE id = $1
                "#,
            )
            .bind(database_id)
            .fetch_optional(&state.pool)
            .await;
            state.metrics.database_query_finished(started.elapsed());
            product
        }
        Err(_) => Ok(None),
    };

    let (status, body, ttl) = match product {
        Ok(Some(product)) => (
            StatusCode::OK,
            Bytes::from(serde_json::to_vec(&product)?),
            jittered_ttl(&state.config, id),
        ),
        Ok(None) => (
            StatusCode::NOT_FOUND,
            Bytes::from_static(br#"{"error":"product not found"}"#),
            state.config.negative_cache_ttl,
        ),
        Err(error) => {
            state.metrics.database_error();
            return Err(error.into());
        }
    };

    Ok(Arc::new(CachedResponse {
        status,
        body,
        expires_after: ttl,
    }))
}

fn to_http_response(response: Arc<CachedResponse>) -> Response {
    Response::builder()
        .status(response.status)
        .header(header::CONTENT_TYPE, "application/json")
        .header(header::CACHE_CONTROL, "no-store")
        .body(Body::from(response.body.clone()))
        .expect("static response headers are valid")
}

async fn ready(State(state): State<AppState>) -> impl IntoResponse {
    match sqlx::query_scalar::<_, i32>("SELECT 1")
        .fetch_one(&state.pool)
        .await
    {
        Ok(1) => (StatusCode::OK, Json(serde_json::json!({ "ready": true }))),
        _ => (
            StatusCode::SERVICE_UNAVAILABLE,
            Json(serde_json::json!({ "ready": false })),
        ),
    }
}

async fn metrics(State(state): State<AppState>) -> Response {
    // Moka updates entry count and weighted size during asynchronous maintenance.
    // Metrics are off the hot path, so make those gauges current before reading
    // them instead of forcing maintenance work into product requests.
    state.cache.run_pending_tasks().await;
    let body = state
        .metrics
        .render(
            state.cache.entry_count(),
            state.cache.weighted_size(),
            state.config.cache_ttl.as_secs(),
        );
    Response::builder()
        .status(StatusCode::OK)
        .header(header::CONTENT_TYPE, "text/plain; version=0.0.4")
        .body(Body::from(body))
        .expect("static metrics headers are valid")
}

fn jittered_ttl(config: &Config, id: u64) -> std::time::Duration {
    let spread = config.cache_ttl_jitter_percent as i128;
    if spread == 0 {
        return config.cache_ttl;
    }

    // SplitMix64 gives each key a stable pseudo-random position in the jitter
    // interval while preserving reproducibility across benchmark runs.
    let mut value = id.wrapping_add(0x9e3779b97f4a7c15);
    value = (value ^ (value >> 30)).wrapping_mul(0xbf58476d1ce4e5b9);
    value = (value ^ (value >> 27)).wrapping_mul(0x94d049bb133111eb);
    value ^= value >> 31;

    let offset = (value % (spread as u64 * 2 + 1)) as i128 - spread;
    let millis = config.cache_ttl.as_millis() as i128;
    std::time::Duration::from_millis((millis * (100 + offset) / 100).max(1) as u64)
}

async fn shutdown_signal() {
    let ctrl_c = async {
        tokio::signal::ctrl_c()
            .await
            .expect("failed to install Ctrl+C handler");
    };

    #[cfg(unix)]
    let terminate = async {
        tokio::signal::unix::signal(tokio::signal::unix::SignalKind::terminate())
            .expect("failed to install SIGTERM handler")
            .recv()
            .await;
    };

    #[cfg(not(unix))]
    let terminate = std::future::pending::<()>();

    tokio::select! {
        _ = ctrl_c => {},
        _ = terminate => {},
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::net::SocketAddr;

    fn config(jitter: u8) -> Config {
        Config {
            listen_addr: SocketAddr::from(([127, 0, 0, 1], 8080)),
            database_url: "postgres://unused".into(),
            database_max_connections: 1,
            cache_capacity_bytes: 1024,
            cache_ttl: std::time::Duration::from_secs(100),
            cache_ttl_jitter_percent: jitter,
            negative_cache_ttl: std::time::Duration::from_secs(1),
            tls_cert_path: None,
            tls_key_path: None,
        }
    }

    #[test]
    fn ttl_jitter_stays_inside_configured_range() {
        let config = config(20);
        for id in 1..10_000 {
            let ttl = jittered_ttl(&config, id).as_secs();
            assert!((80..=120).contains(&ttl));
        }
    }

    #[test]
    fn zero_jitter_preserves_base_ttl() {
        let config = config(0);
        assert_eq!(jittered_ttl(&config, 42), config.cache_ttl);
    }
}
