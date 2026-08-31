mod config;
mod histogram;
mod http;

use std::{
    collections::BTreeMap,
    fs,
    sync::Arc,
    time::{Duration, SystemTime, UNIX_EPOCH},
};

use config::{Config, USAGE};
use histogram::{LatencyHistogram, LatencySnapshot};
use http::{HttpConnection, HttpError, HttpResponse, Target};
use md5::{Digest, Md5};
use serde::{Deserialize, Serialize};
use tokio::{
    sync::{mpsc, oneshot},
    time::{Instant, sleep, sleep_until, timeout},
};

#[tokio::main]
async fn main() {
    match Config::parse() {
        Ok(Some(config)) => {
            if let Err(error) = run(config).await {
                eprintln!("load generator failed: {error}");
                std::process::exit(1);
            }
        }
        Ok(None) => println!("{USAGE}"),
        Err(error) => {
            eprintln!("{error}\n\n{USAGE}");
            std::process::exit(2);
        }
    }
}

async fn run(config: Config) -> Result<(), Box<dyn std::error::Error>> {
    let target = Arc::new(Target::new(&config.target, config.ca_cert.as_deref()).await?);
    wait_until_ready(&target, &config).await?;

    eprintln!(
        "building Zipf distribution for {} products (exponent {})",
        config.product_count, config.zipf_exponent
    );
    let mut sampler = ProductSampler::new(
        config.product_count,
        config.zipf_exponent,
        config.seed,
        config.unknown_per_thousand,
    );

    let mut senders = Vec::with_capacity(config.connections);
    let mut workers = Vec::with_capacity(config.connections);
    for _ in 0..config.connections {
        let (sender, receiver) = mpsc::channel(config.queue_depth_per_connection);
        senders.push(sender);
        let worker_target = target.clone();
        let request_timeout = config.request_timeout;
        workers.push(tokio::spawn(async move {
            worker(receiver, worker_target, request_timeout).await
        }));
    }

    if let Some(start_at) = config.start_at_unix_seconds {
        wait_for_scheduled_phase("warm-up", start_at).await?;
    }
    let started_unix_seconds = unix_seconds();

    let mut cursor = 0;
    eprintln!(
        "warm-up: linear ramp from {} to {} RPS over {} seconds across {} connections",
        config.warmup_start_rate,
        config.rate,
        config.warmup.as_secs(),
        config.connections
    );
    let warmup_plan = RatePlan::linear(config.warmup_start_rate, config.rate, config.warmup);
    let warmup = schedule_phase(&senders, &mut cursor, &mut sampler, warmup_plan, false).await;
    barrier(&senders).await?;

    if let Some(start_at) = config.measurement_start_at_unix_seconds {
        wait_for_scheduled_phase("measurement", start_at).await?;
    }
    let measurement_started_unix_seconds = unix_seconds();

    eprintln!(
        "measurement: {} RPS for {} seconds",
        config.rate,
        config.duration.as_secs()
    );
    let measurement = schedule_phase(
        &senders,
        &mut cursor,
        &mut sampler,
        RatePlan::constant(config.rate, config.duration),
        true,
    )
    .await;
    barrier(&senders).await?;
    drop(senders);

    let mut summary = WorkerSummary::default();
    for worker in workers {
        summary.merge(worker.await?);
    }

    let result = BenchmarkResult::new(
        &config,
        warmup,
        measurement,
        summary,
        started_unix_seconds,
        measurement_started_unix_seconds,
        target.connection_counts(),
    );
    let json = serde_json::to_string_pretty(&result)?;
    println!("{json}");
    if let Some(path) = &config.output {
        if let Some(parent) = path
            .parent()
            .filter(|parent| !parent.as_os_str().is_empty())
        {
            fs::create_dir_all(parent)?;
        }
        fs::write(path, format!("{json}\n"))?;
        eprintln!("wrote {}", path.display());
    }

    if !result.valid_run || !result.thresholds_met {
        std::process::exit(2);
    }
    Ok(())
}

async fn wait_for_scheduled_phase(phase: &str, start_at: u64) -> Result<(), String> {
    let target = UNIX_EPOCH
        .checked_add(Duration::from_secs(start_at))
        .ok_or_else(|| format!("scheduled {phase} start is outside the system clock range"))?;
    let delay = target.duration_since(SystemTime::now()).map_err(|_| {
        format!("scheduled {phase} start {start_at} is in the past; synchronized run aborted")
    })?;
    eprintln!(
        "waiting {:.3} seconds for synchronized {phase} start",
        delay.as_secs_f64()
    );
    sleep(delay).await;
    Ok(())
}

async fn wait_until_ready(target: &Target, config: &Config) -> Result<(), String> {
    let deadline = Instant::now() + config.readiness_timeout;
    let mut last_error = "no readiness response".to_owned();
    while Instant::now() < deadline {
        let attempt = timeout(config.request_timeout, async {
            let mut connection = target.connect().await?;
            connection.get("/health/ready").await
        })
        .await;
        match attempt {
            Ok(Ok(response))
                if response.status == 200
                    && serde_json::from_slice::<ReadyResponse>(&response.body)
                        .is_ok_and(|body| body.ready) =>
            {
                eprintln!("target is ready");
                return Ok(());
            }
            Ok(Ok(response)) => last_error = format!("readiness returned HTTP {}", response.status),
            Ok(Err(error)) => last_error = error.to_string(),
            Err(_) => last_error = "readiness request timed out".into(),
        }
        sleep(Duration::from_millis(250)).await;
    }
    Err(format!(
        "target did not become ready in {} seconds: {last_error}",
        config.readiness_timeout.as_secs()
    ))
}

async fn schedule_phase(
    senders: &[mpsc::Sender<Work>],
    cursor: &mut usize,
    sampler: &mut ProductSampler,
    rate_plan: RatePlan,
    measured: bool,
) -> ScheduleReport {
    let offered = rate_plan.offered_requests();
    let started = Instant::now();
    let ends = started + rate_plan.duration;
    let mut dispatched = 0;
    let mut saturated = 0;
    let mut late_by_more_than_one_ms = 0;
    let mut max_schedule_lag = Duration::ZERO;

    for sequence in 0..offered {
        let scheduled_at = started + rate_plan.arrival_offset(sequence);
        sleep_until(scheduled_at).await;
        let lag = Instant::now().saturating_duration_since(scheduled_at);
        if lag > Duration::from_millis(1) {
            late_by_more_than_one_ms += 1;
        }
        max_schedule_lag = max_schedule_lag.max(lag);

        let (id, expected_not_found) = sampler.next();
        let work = Work::Request(RequestJob {
            scheduled_at,
            id,
            expected_not_found,
            measured,
        });
        if dispatch(senders, cursor, work) {
            dispatched += 1;
        } else {
            saturated += 1;
        }
    }

    let scheduler_overrun = Instant::now().saturating_duration_since(ends);
    ScheduleReport {
        start_rate_rps: rate_plan.start_rate,
        end_rate_rps: rate_plan.end_rate,
        offered,
        dispatched,
        generator_saturation_events: saturated,
        schedules_over_one_ms_late: late_by_more_than_one_ms,
        max_schedule_lag_ms: duration_ms(max_schedule_lag),
        scheduler_overrun_ms: duration_ms(scheduler_overrun),
    }
}

#[derive(Clone, Copy)]
struct RatePlan {
    start_rate: u64,
    end_rate: u64,
    duration: Duration,
}

impl RatePlan {
    fn constant(rate: u64, duration: Duration) -> Self {
        Self {
            start_rate: rate,
            end_rate: rate,
            duration,
        }
    }

    fn linear(start_rate: u64, end_rate: u64, duration: Duration) -> Self {
        debug_assert!(start_rate > 0);
        debug_assert!(start_rate <= end_rate);
        debug_assert!(!duration.is_zero());
        Self {
            start_rate,
            end_rate,
            duration,
        }
    }

    fn offered_requests(self) -> u64 {
        let rate_sum = self.start_rate as u128 + self.end_rate as u128;
        let offered = rate_sum * self.duration.as_secs() as u128 / 2;
        offered.min(u64::MAX as u128) as u64
    }

    fn arrival_offset(self, sequence: u64) -> Duration {
        if self.start_rate == self.end_rate {
            return Duration::from_secs_f64(sequence as f64 / self.start_rate as f64);
        }

        // Invert N(t) = r0*t + (slope*t^2)/2 so each request retains an
        // absolute open-loop arrival time while the offered rate rises.
        let duration_seconds = self.duration.as_secs_f64();
        let start_rate = self.start_rate as f64;
        let slope = (self.end_rate - self.start_rate) as f64 / duration_seconds;
        let discriminant = start_rate * start_rate + 2.0 * slope * sequence as f64;
        Duration::from_secs_f64((discriminant.sqrt() - start_rate) / slope)
    }
}

fn dispatch(senders: &[mpsc::Sender<Work>], cursor: &mut usize, mut work: Work) -> bool {
    for _ in 0..senders.len() {
        let index = *cursor;
        *cursor = (*cursor + 1) % senders.len();
        match senders[index].try_send(work) {
            Ok(()) => return true,
            Err(mpsc::error::TrySendError::Full(returned))
            | Err(mpsc::error::TrySendError::Closed(returned)) => work = returned,
        }
    }
    false
}

async fn barrier(senders: &[mpsc::Sender<Work>]) -> Result<(), String> {
    let mut acknowledgements = Vec::with_capacity(senders.len());
    for sender in senders {
        let (acknowledge, acknowledged) = oneshot::channel();
        sender
            .send(Work::Barrier(acknowledge))
            .await
            .map_err(|_| "a load-generator worker stopped before the phase barrier")?;
        acknowledgements.push(acknowledged);
    }
    for acknowledged in acknowledgements {
        acknowledged
            .await
            .map_err(|_| "a load-generator worker dropped its phase barrier")?;
    }
    Ok(())
}

async fn worker(
    mut receiver: mpsc::Receiver<Work>,
    target: Arc<Target>,
    request_timeout: Duration,
) -> WorkerSummary {
    let mut connection = None;
    let mut summary = WorkerSummary::default();
    while let Some(work) = receiver.recv().await {
        match work {
            Work::Barrier(acknowledge) => {
                let _ = acknowledge.send(());
            }
            Work::Request(job) => {
                let service_started = Instant::now();
                let response =
                    timeout(request_timeout, request(&mut connection, &target, job.id)).await;
                // Cancelling an in-flight HTTP/1.1 request leaves the stream at an
                // unknown message boundary. Reusing it can associate the timed-out
                // response with the next request and turn every later validation on
                // that worker into a false body mismatch.
                if response.is_err() {
                    connection = None;
                }
                if !job.measured {
                    if !matches!(response, Ok(Ok(_))) {
                        summary.warmup_errors += 1;
                    }
                    continue;
                }

                summary.observed_requests += 1;
                summary.service_latency.record(service_started.elapsed());
                match response {
                    Ok(Ok(response)) => {
                        let latency = job.scheduled_at.elapsed();
                        summary.schedule_latency.record(latency);
                        summary.completed_responses += 1;
                        summary.response_bytes += response.body.len() as u64;
                        *summary.status_codes.entry(response.status).or_default() += 1;
                        match validate_response(&response, job.id, job.expected_not_found) {
                            Validation::Valid => summary.valid_responses += 1,
                            Validation::UnexpectedStatus => summary.unexpected_statuses += 1,
                            Validation::InvalidBody => summary.validation_errors += 1,
                        }
                    }
                    Ok(Err(_)) | Err(_) => {
                        connection = None;
                        summary.transport_errors += 1;
                    }
                }
            }
        }
    }
    summary
}

async fn request(
    connection: &mut Option<HttpConnection>,
    target: &Target,
    id: u64,
) -> Result<HttpResponse, HttpError> {
    if connection.is_none() {
        *connection = Some(target.connect().await?);
    }
    let result = connection
        .as_mut()
        .expect("connection was initialized")
        .get(&format!("/products/{id}"))
        .await;
    match result {
        Ok(response) => {
            if !response.reusable {
                *connection = None;
            }
            Ok(response)
        }
        Err(error) => {
            *connection = None;
            Err(error)
        }
    }
}

fn validate_response(response: &HttpResponse, id: u64, expected_not_found: bool) -> Validation {
    if expected_not_found {
        if response.status != 404 {
            return Validation::UnexpectedStatus;
        }
        return match serde_json::from_slice::<ErrorResponse>(&response.body) {
            Ok(error) if error.error == "product not found" => Validation::Valid,
            _ => Validation::InvalidBody,
        };
    }
    if response.status != 200 {
        return Validation::UnexpectedStatus;
    }
    let Ok(product) = serde_json::from_slice::<ProductResponse>(&response.body) else {
        return Validation::InvalidBody;
    };
    let valid = product.id == id as i64
        && product.sku == format!("SKU-{id:012}")
        && product.name == format!("Product {id}")
        && product.category_id == 1 + (id % 1_000) as i32
        && product.price_cents == 100 + ((id * 37) % 100_000) as i32
        && description_matches(id, &product.description)
        && product.updated_at == expected_updated_at(id);
    if valid {
        Validation::Valid
    } else {
        Validation::InvalidBody
    }
}

fn description_matches(id: u64, description: &str) -> bool {
    let digest = format!("{:x}", Md5::digest(id.to_string().as_bytes()));
    description.len() == digest.len() * 28
        && description
            .as_bytes()
            .chunks_exact(digest.len())
            .all(|chunk| chunk == digest.as_bytes())
}

fn expected_updated_at(id: u64) -> String {
    const MONTH_LENGTHS: [u64; 12] = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
    let mut day_of_year = id % 365;
    let mut month = 1;
    for length in MONTH_LENGTHS {
        if day_of_year < length {
            return format!("2026-{month:02}-{:02}T00:00:00Z", day_of_year + 1);
        }
        day_of_year -= length;
        month += 1;
    }
    unreachable!("id modulo 365 always names a day in 2026")
}

enum Validation {
    Valid,
    UnexpectedStatus,
    InvalidBody,
}

enum Work {
    Request(RequestJob),
    Barrier(oneshot::Sender<()>),
}

struct RequestJob {
    scheduled_at: Instant,
    id: u64,
    expected_not_found: bool,
    measured: bool,
}

#[derive(Default)]
struct WorkerSummary {
    observed_requests: u64,
    completed_responses: u64,
    valid_responses: u64,
    transport_errors: u64,
    unexpected_statuses: u64,
    validation_errors: u64,
    response_bytes: u64,
    warmup_errors: u64,
    status_codes: BTreeMap<u16, u64>,
    schedule_latency: LatencyHistogram,
    service_latency: LatencyHistogram,
}

impl WorkerSummary {
    fn merge(&mut self, other: Self) {
        self.observed_requests += other.observed_requests;
        self.completed_responses += other.completed_responses;
        self.valid_responses += other.valid_responses;
        self.transport_errors += other.transport_errors;
        self.unexpected_statuses += other.unexpected_statuses;
        self.validation_errors += other.validation_errors;
        self.response_bytes += other.response_bytes;
        self.warmup_errors += other.warmup_errors;
        for (status, count) in other.status_codes {
            *self.status_codes.entry(status).or_default() += count;
        }
        self.schedule_latency.merge(&other.schedule_latency);
        self.service_latency.merge(&other.service_latency);
    }
}

#[derive(Serialize)]
struct BenchmarkResult {
    schema_version: u32,
    contract_version: u32,
    started_unix_seconds: u64,
    measurement_started_unix_seconds: u64,
    finished_unix_seconds: u64,
    git_commit: String,
    image_digest: String,
    environment: String,
    config: config::ConfigSnapshot,
    warmup: ScheduleReport,
    measurement: ScheduleReport,
    metrics: ResultMetrics,
    valid_run: bool,
    thresholds_met: bool,
    failure_reasons: Vec<String>,
}

#[derive(Serialize)]
struct ResultMetrics {
    observed_requests: u64,
    completed_responses: u64,
    valid_responses: u64,
    achieved_valid_rps: f64,
    transport_errors: u64,
    unexpected_statuses: u64,
    validation_errors: u64,
    error_rate: f64,
    response_bytes: u64,
    client_connection_attempts: u64,
    client_connections_opened: u64,
    warmup_errors: u64,
    status_codes: BTreeMap<u16, u64>,
    schedule_to_completion_latency: LatencySnapshot,
    worker_service_latency: LatencySnapshot,
}

#[derive(Serialize)]
struct ScheduleReport {
    start_rate_rps: u64,
    end_rate_rps: u64,
    offered: u64,
    dispatched: u64,
    generator_saturation_events: u64,
    schedules_over_one_ms_late: u64,
    max_schedule_lag_ms: f64,
    scheduler_overrun_ms: f64,
}

impl BenchmarkResult {
    fn new(
        config: &Config,
        warmup: ScheduleReport,
        measurement: ScheduleReport,
        summary: WorkerSummary,
        started_unix_seconds: u64,
        measurement_started_unix_seconds: u64,
        connection_counts: (u64, u64),
    ) -> Self {
        let errors =
            summary.transport_errors + summary.unexpected_statuses + summary.validation_errors;
        let error_rate = if measurement.dispatched == 0 {
            1.0
        } else {
            errors as f64 / measurement.dispatched as f64
        };
        let achieved_valid_rps = summary.valid_responses as f64 / config.duration.as_secs_f64();
        let p99 = summary.schedule_latency.percentile_us(0.99);
        let mut failure_reasons = Vec::new();

        if warmup.generator_saturation_events > 0 {
            failure_reasons.push("the generator saturated during warm-up".into());
        }
        if measurement.generator_saturation_events > 0 {
            failure_reasons.push("the generator saturated during measurement".into());
        }
        if warmup.scheduler_overrun_ms > 10.0 {
            failure_reasons
                .push("the generator could not schedule warm-up arrivals on time".into());
        }
        if measurement.scheduler_overrun_ms > 10.0 {
            failure_reasons
                .push("the generator could not schedule measured arrivals on time".into());
        }
        if summary.observed_requests != measurement.dispatched {
            failure_reasons.push("not every dispatched request produced an observation".into());
        }
        let valid_run = failure_reasons.is_empty();
        if achieved_valid_rps < config.rate as f64 {
            failure_reasons.push(format!(
                "valid completion rate {achieved_valid_rps:.3} RPS was below offered rate {} RPS",
                config.rate
            ));
        }
        if Duration::from_micros(p99) >= config.max_p99 {
            failure_reasons.push(format!(
                "p99 {:.3} ms was not below {} ms",
                p99 as f64 / 1_000.0,
                config.max_p99.as_millis()
            ));
        }
        if error_rate >= config.max_error_rate {
            failure_reasons.push(format!(
                "error rate {error_rate:.6} was not below {:.6}",
                config.max_error_rate
            ));
        }
        let thresholds_met = failure_reasons.is_empty();

        Self {
            schema_version: 3,
            contract_version: 6,
            started_unix_seconds,
            measurement_started_unix_seconds,
            finished_unix_seconds: unix_seconds(),
            git_commit: config.git_commit.clone(),
            image_digest: config.image_digest.clone(),
            environment: config.environment.clone(),
            config: config.snapshot(),
            warmup,
            measurement,
            metrics: ResultMetrics {
                observed_requests: summary.observed_requests,
                completed_responses: summary.completed_responses,
                valid_responses: summary.valid_responses,
                achieved_valid_rps,
                transport_errors: summary.transport_errors,
                unexpected_statuses: summary.unexpected_statuses,
                validation_errors: summary.validation_errors,
                error_rate,
                response_bytes: summary.response_bytes,
                client_connection_attempts: connection_counts.0,
                client_connections_opened: connection_counts.1,
                warmup_errors: summary.warmup_errors,
                status_codes: summary.status_codes,
                schedule_to_completion_latency: summary.schedule_latency.snapshot(),
                worker_service_latency: summary.service_latency.snapshot(),
            },
            valid_run,
            thresholds_met,
            failure_reasons,
        }
    }
}

struct ProductSampler {
    cumulative_weights: Vec<f64>,
    total_weight: f64,
    product_count: u64,
    unknown_per_thousand: u16,
    sequence: u64,
    random: SplitMix64,
}

impl ProductSampler {
    fn new(product_count: u64, exponent: f64, seed: u64, unknown_per_thousand: u16) -> Self {
        let mut cumulative_weights = Vec::with_capacity(product_count as usize);
        let mut total_weight = 0.0;
        for rank in 1..=product_count {
            total_weight += 1.0 / (rank as f64).powf(exponent);
            cumulative_weights.push(total_weight);
        }
        Self {
            cumulative_weights,
            total_weight,
            product_count,
            unknown_per_thousand,
            sequence: seed % 1_000,
            random: SplitMix64(seed),
        }
    }

    fn next(&mut self) -> (u64, bool) {
        let unknown = self.sequence % 1_000 < self.unknown_per_thousand as u64;
        self.sequence = self.sequence.wrapping_add(1);
        if unknown {
            let id = self.product_count + 1 + self.random.next_u64() % 10_000;
            return (id, true);
        }
        let needle = self.random.next_f64() * self.total_weight;
        let index = self
            .cumulative_weights
            .partition_point(|weight| *weight < needle);
        (
            (index.min(self.cumulative_weights.len() - 1) + 1) as u64,
            false,
        )
    }
}

struct SplitMix64(u64);

impl SplitMix64 {
    fn next_u64(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9e3779b97f4a7c15);
        let mut value = self.0;
        value = (value ^ (value >> 30)).wrapping_mul(0xbf58476d1ce4e5b9);
        value = (value ^ (value >> 27)).wrapping_mul(0x94d049bb133111eb);
        value ^ (value >> 31)
    }

    fn next_f64(&mut self) -> f64 {
        ((self.next_u64() >> 11) as f64) * (1.0 / ((1_u64 << 53) as f64))
    }
}

#[derive(Deserialize)]
struct ReadyResponse {
    ready: bool,
}

#[derive(Deserialize)]
struct ProductResponse {
    id: i64,
    sku: String,
    name: String,
    category_id: i32,
    price_cents: i32,
    description: String,
    updated_at: String,
}

#[derive(Deserialize)]
struct ErrorResponse {
    error: String,
}

fn duration_ms(duration: Duration) -> f64 {
    duration.as_secs_f64() * 1_000.0
}

fn unix_seconds() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs()
}

#[cfg(test)]
mod tests {
    use super::*;
    use tokio::io::{AsyncReadExt, AsyncWriteExt};

    fn product_response(id: u64) -> Vec<u8> {
        let description = format!("{:x}", Md5::digest(id.to_string().as_bytes())).repeat(28);
        let body = serde_json::to_vec(&serde_json::json!({
            "id": id,
            "sku": format!("SKU-{id:012}"),
            "name": format!("Product {id}"),
            "category_id": 1 + (id % 1_000),
            "price_cents": 100 + ((id * 37) % 100_000),
            "description": description,
            "updated_at": expected_updated_at(id)
        }))
        .unwrap();
        format!(
            "HTTP/1.1 200 OK\r\nContent-Length: {}\r\nConnection: keep-alive\r\n\r\n",
            body.len()
        )
        .into_bytes()
        .into_iter()
        .chain(body)
        .collect()
    }

    #[test]
    fn sampler_is_reproducible_and_stays_in_range() {
        let mut first = ProductSampler::new(100, 1.1, 104_729, 1);
        let mut second = ProductSampler::new(100, 1.1, 104_729, 1);
        let mut unknown = 0;
        for _ in 0..10_000 {
            let a = first.next();
            let b = second.next();
            assert_eq!(a, b);
            if a.1 {
                unknown += 1;
                assert!(a.0 > 100);
            } else {
                assert!((1..=100).contains(&a.0));
            }
        }
        assert_eq!(unknown, 10);
    }

    #[test]
    fn validates_a_deterministic_product() {
        let description = format!("{:x}", Md5::digest(b"42")).repeat(28);
        let body = serde_json::to_vec(&serde_json::json!({
            "id": 42,
            "sku": "SKU-000000000042",
            "name": "Product 42",
            "category_id": 43,
            "price_cents": 1654,
            "description": description,
            "updated_at": "2026-02-12T00:00:00Z"
        }))
        .unwrap();
        let response = HttpResponse {
            status: 200,
            body,
            reusable: true,
        };
        assert!(matches!(
            validate_response(&response, 42, false),
            Validation::Valid
        ));
    }

    #[test]
    fn rejects_a_product_with_the_wrong_deterministic_description() {
        let body = serde_json::to_vec(&serde_json::json!({
            "id": 42,
            "sku": "SKU-000000000042",
            "name": "Product 42",
            "category_id": 43,
            "price_cents": 1654,
            "description": "0".repeat(896),
            "updated_at": "2026-02-12T00:00:00Z"
        }))
        .unwrap();
        let response = HttpResponse {
            status: 200,
            body,
            reusable: true,
        };
        assert!(matches!(
            validate_response(&response, 42, false),
            Validation::InvalidBody
        ));
    }

    #[tokio::test]
    async fn discards_a_connection_after_a_warmup_timeout() {
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
        let address = listener.local_addr().unwrap();
        let server = tokio::spawn(async move {
            let mut accepted = 0;
            loop {
                let (mut stream, _) = listener.accept().await.unwrap();
                accepted += 1;
                tokio::spawn(async move {
                    let mut request = [0_u8; 1024];
                    stream.read(&mut request).await.unwrap();
                    if accepted == 1 {
                        sleep(Duration::from_millis(100)).await;
                        stream.write_all(&product_response(41)).await.unwrap();
                    } else {
                        stream.write_all(&product_response(42)).await.unwrap();
                    }
                });
            }
        });

        let target = Arc::new(
            Target::new(
                &url::Url::parse(&format!("http://{address}/")).unwrap(),
                None,
            )
            .await
            .unwrap(),
        );
        let (sender, receiver) = mpsc::channel(2);
        let worker = tokio::spawn(worker(receiver, target.clone(), Duration::from_millis(10)));
        for (id, measured) in [(41, false), (42, true)] {
            sender
                .send(Work::Request(RequestJob {
                    scheduled_at: Instant::now(),
                    id,
                    expected_not_found: false,
                    measured,
                }))
                .await
                .unwrap();
        }
        drop(sender);

        let summary = worker.await.unwrap();
        server.abort();
        assert_eq!(summary.warmup_errors, 1);
        assert_eq!(summary.transport_errors, 0);
        assert_eq!(summary.valid_responses, 1);
        assert_eq!(target.connection_counts(), (2, 2));
    }

    #[test]
    fn linear_rate_plan_has_deterministic_open_loop_arrivals() {
        let plan = RatePlan::linear(1_000, 20_000, Duration::from_secs(30));

        assert_eq!(plan.offered_requests(), 315_000);
        assert_eq!(plan.arrival_offset(0), Duration::ZERO);
        assert!((plan.arrival_offset(86_250).as_secs_f64() - 15.0).abs() < 0.000_001);
        assert!(plan.arrival_offset(314_999) < Duration::from_secs(30));
        assert!(plan.arrival_offset(200_000) < plan.arrival_offset(200_001));
    }
}
