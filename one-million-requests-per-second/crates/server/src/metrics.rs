use std::{
    fmt::Write,
    sync::atomic::{AtomicU64, Ordering},
    time::Duration,
};

const DB_LATENCY_BUCKETS_US: [u64; 8] = [
    1_000,
    2_000,
    5_000,
    10_000,
    25_000,
    50_000,
    100_000,
    u64::MAX,
];

const REQUEST_LATENCY_BUCKETS_US: [u64; 13] = [
    100,
    250,
    500,
    1_000,
    2_000,
    5_000,
    10_000,
    25_000,
    50_000,
    100_000,
    250_000,
    1_000_000,
    u64::MAX,
];

#[derive(Default)]
pub struct Metrics {
    requests: AtomicU64,
    responses: AtomicU64,
    responses_2xx: AtomicU64,
    responses_4xx: AtomicU64,
    responses_5xx: AtomicU64,
    request_latency_sum_us: AtomicU64,
    request_latency_buckets: [AtomicU64; REQUEST_LATENCY_BUCKETS_US.len()],
    cache_hits: AtomicU64,
    cache_misses: AtomicU64,
    cache_evictions: AtomicU64,
    database_queries: AtomicU64,
    database_errors: AtomicU64,
    database_latency_sum_us: AtomicU64,
    database_latency_buckets: [AtomicU64; DB_LATENCY_BUCKETS_US.len()],
    accepted_connections: AtomicU64,
    active_connections: AtomicU64,
    tls_handshakes: AtomicU64,
    tls_handshake_errors: AtomicU64,
    tls_resumptions: AtomicU64,
}

impl Metrics {
    pub fn request(&self) {
        self.requests.fetch_add(1, Ordering::Relaxed);
    }

    pub fn response(&self, status: u16, elapsed: Duration) {
        self.responses.fetch_add(1, Ordering::Relaxed);
        match status {
            200..=299 => self.responses_2xx.fetch_add(1, Ordering::Relaxed),
            400..=499 => self.responses_4xx.fetch_add(1, Ordering::Relaxed),
            500..=599 => self.responses_5xx.fetch_add(1, Ordering::Relaxed),
            _ => 0,
        };
        observe(
            elapsed,
            &self.request_latency_sum_us,
            &self.request_latency_buckets,
            &REQUEST_LATENCY_BUCKETS_US,
        );
    }

    pub fn cache_hit(&self) {
        self.cache_hits.fetch_add(1, Ordering::Relaxed);
    }

    pub fn cache_miss(&self) {
        self.cache_misses.fetch_add(1, Ordering::Relaxed);
    }

    pub fn cache_eviction(&self) {
        self.cache_evictions.fetch_add(1, Ordering::Relaxed);
    }

    pub fn database_query(&self) {
        self.database_queries.fetch_add(1, Ordering::Relaxed);
    }

    pub fn database_error(&self) {
        self.database_errors.fetch_add(1, Ordering::Relaxed);
    }

    pub fn database_query_finished(&self, elapsed: Duration) {
        observe(
            elapsed,
            &self.database_latency_sum_us,
            &self.database_latency_buckets,
            &DB_LATENCY_BUCKETS_US,
        );
    }

    pub fn connection_accepted(&self) {
        self.accepted_connections.fetch_add(1, Ordering::Relaxed);
        self.active_connections.fetch_add(1, Ordering::Relaxed);
    }

    pub fn connection_closed(&self) {
        self.active_connections.fetch_sub(1, Ordering::Relaxed);
    }

    pub fn tls_handshake(&self, resumed: bool) {
        self.tls_handshakes.fetch_add(1, Ordering::Relaxed);
        if resumed {
            self.tls_resumptions.fetch_add(1, Ordering::Relaxed);
        }
    }

    pub fn tls_handshake_error(&self) {
        self.tls_handshake_errors.fetch_add(1, Ordering::Relaxed);
    }

    pub fn render(
        &self,
        cache_entries: u64,
        cache_weight_bytes: u64,
        cache_ttl_seconds: u64,
    ) -> String {
        let mut output = String::with_capacity(4_096);
        counter(
            &mut output,
            "http_requests_total",
            self.requests.load(Ordering::Relaxed),
        );
        counter(
            &mut output,
            "http_responses_total",
            self.responses.load(Ordering::Relaxed),
        );
        counter(
            &mut output,
            "http_responses_2xx_total",
            self.responses_2xx.load(Ordering::Relaxed),
        );
        counter(
            &mut output,
            "http_responses_4xx_total",
            self.responses_4xx.load(Ordering::Relaxed),
        );
        counter(
            &mut output,
            "http_responses_5xx_total",
            self.responses_5xx.load(Ordering::Relaxed),
        );
        histogram(
            &mut output,
            "http_request_duration_seconds",
            &REQUEST_LATENCY_BUCKETS_US,
            &self.request_latency_buckets,
            self.request_latency_sum_us.load(Ordering::Relaxed),
            self.responses.load(Ordering::Relaxed),
        );
        counter(
            &mut output,
            "cache_hits_total",
            self.cache_hits.load(Ordering::Relaxed),
        );
        let cache_misses = self.cache_misses.load(Ordering::Relaxed);
        let database_queries = self.database_queries.load(Ordering::Relaxed);
        counter(&mut output, "cache_misses_total", cache_misses);
        counter(
            &mut output,
            "cache_evictions_total",
            self.cache_evictions.load(Ordering::Relaxed),
        );
        counter(
            &mut output,
            "cache_coalesced_loads_total",
            cache_misses.saturating_sub(database_queries),
        );
        counter(&mut output, "database_queries_total", database_queries);
        counter(
            &mut output,
            "database_errors_total",
            self.database_errors.load(Ordering::Relaxed),
        );
        histogram(
            &mut output,
            "database_query_duration_seconds",
            &DB_LATENCY_BUCKETS_US,
            &self.database_latency_buckets,
            self.database_latency_sum_us.load(Ordering::Relaxed),
            database_queries,
        );
        counter(
            &mut output,
            "accepted_connections_total",
            self.accepted_connections.load(Ordering::Relaxed),
        );
        gauge(
            &mut output,
            "active_connections",
            self.active_connections.load(Ordering::Relaxed),
        );
        counter(
            &mut output,
            "tls_handshakes_total",
            self.tls_handshakes.load(Ordering::Relaxed),
        );
        counter(
            &mut output,
            "tls_handshake_errors_total",
            self.tls_handshake_errors.load(Ordering::Relaxed),
        );
        counter(
            &mut output,
            "tls_resumptions_total",
            self.tls_resumptions.load(Ordering::Relaxed),
        );
        gauge(&mut output, "cache_entries", cache_entries);
        gauge(&mut output, "cache_weight_bytes", cache_weight_bytes);
        gauge(&mut output, "cache_ttl_seconds", cache_ttl_seconds);
        output
    }
}

fn observe<const N: usize>(
    elapsed: Duration,
    sum_us: &AtomicU64,
    buckets: &[AtomicU64; N],
    upper_bounds: &[u64; N],
) {
    let micros = elapsed.as_micros().min(u64::MAX as u128) as u64;
    sum_us.fetch_add(micros, Ordering::Relaxed);
    for (index, upper_bound) in upper_bounds.iter().enumerate() {
        if micros <= *upper_bound {
            buckets[index].fetch_add(1, Ordering::Relaxed);
        }
    }
}

fn histogram<const N: usize>(
    output: &mut String,
    name: &str,
    upper_bounds_us: &[u64; N],
    buckets: &[AtomicU64; N],
    sum_us: u64,
    count: u64,
) {
    let _ = writeln!(output, "# TYPE {name} histogram");
    for (index, upper_bound) in upper_bounds_us.iter().enumerate() {
        let label = if *upper_bound == u64::MAX {
            "+Inf".to_owned()
        } else {
            format!("{:.6}", *upper_bound as f64 / 1_000_000.0)
        };
        let _ = writeln!(
            output,
            "{name}_bucket{{le=\"{label}\"}} {}",
            buckets[index].load(Ordering::Relaxed)
        );
    }
    let _ = writeln!(output, "{name}_sum {:.6}", sum_us as f64 / 1_000_000.0);
    let _ = writeln!(output, "{name}_count {count}");
}

fn counter(output: &mut String, name: &str, value: u64) {
    let _ = writeln!(output, "# TYPE {name} counter");
    let _ = writeln!(output, "{name} {value}");
}

fn gauge(output: &mut String, name: &str, value: u64) {
    let _ = writeln!(output, "# TYPE {name} gauge");
    let _ = writeln!(output, "{name} {value}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn histograms_are_cumulative_and_derived_metrics_are_rendered() {
        let metrics = Metrics::default();
        metrics.request();
        metrics.response(200, Duration::from_micros(1_500));
        metrics.cache_miss();
        metrics.cache_miss();
        metrics.database_query();
        metrics.database_query_finished(Duration::from_micros(1_500));
        metrics.connection_accepted();
        metrics.tls_handshake(true);
        let output = metrics.render(2, 2_048, 3_600);

        assert!(output.contains("database_query_duration_seconds_bucket{le=\"0.001000\"} 0"));
        assert!(output.contains("database_query_duration_seconds_bucket{le=\"0.002000\"} 1"));
        assert!(output.contains("database_query_duration_seconds_bucket{le=\"+Inf\"} 1"));
        assert!(output.contains("cache_coalesced_loads_total 1"));
        assert!(output.contains("active_connections 1"));
        assert!(output.contains("tls_resumptions_total 1"));
        assert!(output.contains("cache_entries 2"));
        assert!(output.contains("cache_ttl_seconds 3600"));
    }
}
