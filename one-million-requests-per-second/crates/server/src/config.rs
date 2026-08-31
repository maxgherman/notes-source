use std::{env, net::SocketAddr, path::PathBuf, str::FromStr, time::Duration};

#[derive(Clone, Debug)]
pub struct Config {
    pub listen_addr: SocketAddr,
    pub database_url: String,
    pub database_max_connections: u32,
    pub cache_capacity_bytes: u64,
    pub cache_ttl: Duration,
    pub cache_ttl_jitter_percent: u8,
    pub negative_cache_ttl: Duration,
    pub tls_cert_path: Option<PathBuf>,
    pub tls_key_path: Option<PathBuf>,
}

impl Config {
    pub fn from_env() -> Result<Self, String> {
        let jitter = optional("CACHE_TTL_JITTER_PERCENT", 20_u8)?;
        if jitter > 100 {
            return Err("CACHE_TTL_JITTER_PERCENT must be at most 100".into());
        }

        let tls_cert_path = optional_path("TLS_CERT_PATH")?;
        let tls_key_path = optional_path("TLS_KEY_PATH")?;
        if tls_cert_path.is_some() != tls_key_path.is_some() {
            return Err("TLS_CERT_PATH and TLS_KEY_PATH must be set together".into());
        }

        let config = Self {
            listen_addr: optional("LISTEN_ADDR", SocketAddr::from(([0, 0, 0, 0], 8080)))?,
            database_url: env::var("DATABASE_URL")
                .map_err(|_| "DATABASE_URL must be set".to_owned())?,
            database_max_connections: optional("DATABASE_MAX_CONNECTIONS", 32_u32)?,
            cache_capacity_bytes: optional("CACHE_CAPACITY_BYTES", 512 * 1024 * 1024_u64)?,
            cache_ttl: Duration::from_secs(optional("CACHE_TTL_SECONDS", 3_600_u64)?),
            cache_ttl_jitter_percent: jitter,
            negative_cache_ttl: Duration::from_secs(optional(
                "NEGATIVE_CACHE_TTL_SECONDS",
                30_u64,
            )?),
            tls_cert_path,
            tls_key_path,
        };

        if config.database_max_connections == 0 {
            return Err("DATABASE_MAX_CONNECTIONS must be greater than zero".into());
        }
        if config.cache_capacity_bytes == 0 {
            return Err("CACHE_CAPACITY_BYTES must be greater than zero".into());
        }
        if config.cache_ttl.is_zero() {
            return Err("CACHE_TTL_SECONDS must be greater than zero".into());
        }
        if config.negative_cache_ttl.is_zero() {
            return Err("NEGATIVE_CACHE_TTL_SECONDS must be greater than zero".into());
        }

        Ok(config)
    }
}

fn optional_path(name: &str) -> Result<Option<PathBuf>, String> {
    match env::var(name) {
        Ok(value) if value.trim().is_empty() => Ok(None),
        Ok(value) => Ok(Some(PathBuf::from(value))),
        Err(env::VarError::NotPresent) => Ok(None),
        Err(error) => Err(format!("cannot read {name}: {error}")),
    }
}

fn optional<T>(name: &str, default: T) -> Result<T, String>
where
    T: FromStr,
    T::Err: std::fmt::Display,
{
    match env::var(name) {
        Ok(value) => value
            .parse()
            .map_err(|error| format!("invalid {name}: {error}")),
        Err(env::VarError::NotPresent) => Ok(default),
        Err(error) => Err(format!("cannot read {name}: {error}")),
    }
}
