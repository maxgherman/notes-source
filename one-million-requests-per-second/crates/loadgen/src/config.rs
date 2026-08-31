use std::{env, path::PathBuf, str::FromStr, time::Duration};

use serde::Serialize;
use url::Url;

pub const USAGE: &str = r#"million-rps-loadgen

Open-loop benchmark client for GET /products/{id}.

Required:
  --target URL                     http:// or https:// service origin

Workload:
  --rate RPS                       offered requests/second (default: 1000)
  --connections N                  persistent connections (default: 256)
  --warmup-seconds N               warm-up duration (default: 300)
  --warmup-start-rate RPS          linear-ramp start (default: min(1000, target))
  --duration-seconds N             measured duration (default: 1800)
  --product-count N                known products (default: 10000000)
  --zipf-exponent N                Zipf exponent (default: 1.1)
  --seed N                         deterministic seed (default: 104729)
  --unknown-per-thousand N         intentional unknown requests (default: 1)
  --queue-depth-per-connection N   bounded pending work per connection (default: 64)
  --request-timeout-ms N           per-request timeout (default: 5000)
  --start-at-unix-seconds N        wait for this UTC epoch second before warm-up
  --measurement-start-at-unix-seconds N
                                   wait for this UTC epoch second before measurement
  --server-cache-capacity-bytes N  serving cache budget (default: 536870912)
  --server-cache-ttl-seconds N      positive-entry cache TTL (default: 3600)

TLS and output:
  --ca-cert PATH                   PEM CA used to validate an HTTPS target
  --output PATH                    write the JSON result to this path
  --git-commit VALUE               server source revision recorded in the result
  --image-digest VALUE             server image digest recorded in the result
  --environment VALUE              instance/AZ or local environment description

Thresholds:
  --max-p99-ms N                   schedule-to-completion p99 limit (default: 50)
  --max-error-rate N               error fraction limit (default: 0.001)
  --readiness-timeout-seconds N    readiness deadline (default: 30)
"#;

#[derive(Clone, Debug)]
pub struct Config {
    pub target: Url,
    pub rate: u64,
    pub connections: usize,
    pub warmup: Duration,
    pub warmup_start_rate: u64,
    pub duration: Duration,
    pub product_count: u64,
    pub zipf_exponent: f64,
    pub seed: u64,
    pub unknown_per_thousand: u16,
    pub queue_depth_per_connection: usize,
    pub request_timeout: Duration,
    pub readiness_timeout: Duration,
    pub start_at_unix_seconds: Option<u64>,
    pub measurement_start_at_unix_seconds: Option<u64>,
    pub server_cache_capacity_bytes: u64,
    pub server_cache_ttl: Duration,
    pub max_p99: Duration,
    pub max_error_rate: f64,
    pub ca_cert: Option<PathBuf>,
    pub output: Option<PathBuf>,
    pub git_commit: String,
    pub image_digest: String,
    pub environment: String,
}

#[derive(Serialize)]
pub struct ConfigSnapshot {
    pub target: String,
    pub rate_rps: u64,
    pub connections: usize,
    pub warmup_seconds: u64,
    pub warmup_start_rate_rps: u64,
    pub duration_seconds: u64,
    pub product_count: u64,
    pub zipf_exponent: f64,
    pub seed: u64,
    pub unknown_per_thousand: u16,
    pub queue_depth_per_connection: usize,
    pub request_timeout_ms: u128,
    pub scheduled_start_unix_seconds: Option<u64>,
    pub scheduled_measurement_start_unix_seconds: Option<u64>,
    pub server_cache_capacity_bytes: u64,
    pub server_cache_ttl_seconds: u64,
    pub max_p99_ms: u128,
    pub max_error_rate: f64,
    pub ca_cert: Option<String>,
}

impl Config {
    pub fn parse() -> Result<Option<Self>, String> {
        let mut args = env::args().skip(1);
        let mut target = None;
        let mut rate: u64 = 1_000;
        let mut connections: usize = 256;
        let mut warmup_seconds: u64 = 300;
        let mut warmup_start_rate: Option<u64> = None;
        let mut duration_seconds: u64 = 1_800;
        let mut product_count: u64 = 10_000_000;
        let mut zipf_exponent: f64 = 1.1;
        let mut seed: u64 = 104_729;
        let mut unknown_per_thousand: u16 = 1;
        let mut queue_depth_per_connection: usize = 64;
        let mut request_timeout_ms: u64 = 5_000;
        let mut readiness_timeout_seconds: u64 = 30;
        let mut start_at_unix_seconds: Option<u64> = None;
        let mut measurement_start_at_unix_seconds: Option<u64> = None;
        let mut server_cache_capacity_bytes = 536_870_912_u64;
        let mut server_cache_ttl_seconds = 3_600_u64;
        let mut max_p99_ms: u64 = 50;
        let mut max_error_rate: f64 = 0.001;
        let mut ca_cert = None;
        let mut output = None;
        let mut git_commit = "unknown".to_owned();
        let mut image_digest = "unknown".to_owned();
        let mut environment = "unspecified".to_owned();

        while let Some(flag) = args.next() {
            if flag == "--help" || flag == "-h" {
                return Ok(None);
            }
            let value = args
                .next()
                .ok_or_else(|| format!("missing value for {flag}"))?;
            match flag.as_str() {
                "--target" => target = Some(value),
                "--rate" => rate = parse(&flag, &value)?,
                "--connections" => connections = parse(&flag, &value)?,
                "--warmup-seconds" => warmup_seconds = parse(&flag, &value)?,
                "--warmup-start-rate" => warmup_start_rate = Some(parse(&flag, &value)?),
                "--duration-seconds" => duration_seconds = parse(&flag, &value)?,
                "--product-count" => product_count = parse(&flag, &value)?,
                "--zipf-exponent" => zipf_exponent = parse(&flag, &value)?,
                "--seed" => seed = parse(&flag, &value)?,
                "--unknown-per-thousand" => unknown_per_thousand = parse(&flag, &value)?,
                "--queue-depth-per-connection" => {
                    queue_depth_per_connection = parse(&flag, &value)?
                }
                "--request-timeout-ms" => request_timeout_ms = parse(&flag, &value)?,
                "--start-at-unix-seconds" => start_at_unix_seconds = Some(parse(&flag, &value)?),
                "--measurement-start-at-unix-seconds" => {
                    measurement_start_at_unix_seconds = Some(parse(&flag, &value)?)
                }
                "--server-cache-capacity-bytes" => {
                    server_cache_capacity_bytes = parse(&flag, &value)?
                }
                "--server-cache-ttl-seconds" => {
                    server_cache_ttl_seconds = parse(&flag, &value)?
                }
                "--readiness-timeout-seconds" => readiness_timeout_seconds = parse(&flag, &value)?,
                "--max-p99-ms" => max_p99_ms = parse(&flag, &value)?,
                "--max-error-rate" => max_error_rate = parse(&flag, &value)?,
                "--ca-cert" => ca_cert = Some(PathBuf::from(value)),
                "--output" => output = Some(PathBuf::from(value)),
                "--git-commit" => git_commit = value,
                "--image-digest" => image_digest = value,
                "--environment" => environment = value,
                _ => return Err(format!("unknown option {flag}")),
            }
        }

        let target = Url::parse(&target.ok_or("--target is required")?)
            .map_err(|error| format!("invalid --target: {error}"))?;
        if !matches!(target.scheme(), "http" | "https") {
            return Err("--target scheme must be http or https".into());
        }
        if target.scheme() == "https" && ca_cert.is_none() {
            return Err("--ca-cert is required for an HTTPS target".into());
        }
        if target.host_str().is_none() {
            return Err("--target must contain a host".into());
        }
        if target.path() != "/" || target.query().is_some() || target.fragment().is_some() {
            return Err("--target must be an origin without a path, query, or fragment".into());
        }
        let warmup_start_rate = warmup_start_rate.unwrap_or(rate.min(1_000));
        if rate == 0
            || warmup_start_rate == 0
            || connections == 0
            || warmup_seconds == 0
            || duration_seconds == 0
            || product_count == 0
            || queue_depth_per_connection == 0
            || request_timeout_ms == 0
            || server_cache_capacity_bytes == 0
            || server_cache_ttl_seconds == 0
        {
            return Err(
                "rates, counts, durations, queue depth, and timeouts must be positive".into(),
            );
        }
        if warmup_start_rate > rate {
            return Err("--warmup-start-rate must not exceed --rate".into());
        }
        if measurement_start_at_unix_seconds.is_some() != start_at_unix_seconds.is_some() {
            return Err(
                "--start-at-unix-seconds and --measurement-start-at-unix-seconds must be set together"
                    .into(),
            );
        }
        if let (Some(warmup_start), Some(measurement_start)) =
            (start_at_unix_seconds, measurement_start_at_unix_seconds)
        {
            let earliest_measurement = warmup_start
                .checked_add(warmup_seconds)
                .ok_or("scheduled phase timestamps overflow")?;
            if measurement_start <= earliest_measurement {
                return Err(
                    "--measurement-start-at-unix-seconds must leave time for the warm-up and its phase barrier"
                        .into(),
                );
            }
        }
        if zipf_exponent <= 0.0 || !zipf_exponent.is_finite() {
            return Err("--zipf-exponent must be finite and greater than zero".into());
        }
        if product_count > i64::MAX as u64 - 10_000 || product_count > usize::MAX as u64 {
            return Err("--product-count is too large for this platform".into());
        }
        if unknown_per_thousand > 1_000 {
            return Err("--unknown-per-thousand must be at most 1000".into());
        }
        if !(0.0..=1.0).contains(&max_error_rate) {
            return Err("--max-error-rate must be between zero and one".into());
        }
        rate.checked_mul(duration_seconds)
            .ok_or("rate multiplied by duration is too large")?;
        rate.checked_mul(warmup_seconds)
            .ok_or("rate multiplied by warm-up is too large")?;

        Ok(Some(Self {
            target,
            rate,
            connections,
            warmup: Duration::from_secs(warmup_seconds),
            warmup_start_rate,
            duration: Duration::from_secs(duration_seconds),
            product_count,
            zipf_exponent,
            seed,
            unknown_per_thousand,
            queue_depth_per_connection,
            request_timeout: Duration::from_millis(request_timeout_ms),
            readiness_timeout: Duration::from_secs(readiness_timeout_seconds),
            start_at_unix_seconds,
            measurement_start_at_unix_seconds,
            server_cache_capacity_bytes,
            server_cache_ttl: Duration::from_secs(server_cache_ttl_seconds),
            max_p99: Duration::from_millis(max_p99_ms),
            max_error_rate,
            ca_cert,
            output,
            git_commit,
            image_digest,
            environment,
        }))
    }

    pub fn snapshot(&self) -> ConfigSnapshot {
        ConfigSnapshot {
            target: self.target.to_string(),
            rate_rps: self.rate,
            connections: self.connections,
            warmup_seconds: self.warmup.as_secs(),
            warmup_start_rate_rps: self.warmup_start_rate,
            duration_seconds: self.duration.as_secs(),
            product_count: self.product_count,
            zipf_exponent: self.zipf_exponent,
            seed: self.seed,
            unknown_per_thousand: self.unknown_per_thousand,
            queue_depth_per_connection: self.queue_depth_per_connection,
            request_timeout_ms: self.request_timeout.as_millis(),
            scheduled_start_unix_seconds: self.start_at_unix_seconds,
            scheduled_measurement_start_unix_seconds: self
                .measurement_start_at_unix_seconds,
            server_cache_capacity_bytes: self.server_cache_capacity_bytes,
            server_cache_ttl_seconds: self.server_cache_ttl.as_secs(),
            max_p99_ms: self.max_p99.as_millis(),
            max_error_rate: self.max_error_rate,
            ca_cert: self.ca_cert.as_ref().map(|path| path.display().to_string()),
        }
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
