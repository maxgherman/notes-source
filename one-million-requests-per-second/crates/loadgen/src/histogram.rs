use std::time::Duration;

use serde::Serialize;

const LOG_BASE: f64 = 1.01;

#[derive(Default)]
pub struct LatencyHistogram {
    buckets: Vec<u64>,
    count: u64,
    sum_us: u128,
    max_us: u64,
}

#[derive(Serialize)]
pub struct LatencySnapshot {
    pub count: u64,
    pub mean_ms: f64,
    pub p50_ms: f64,
    pub p90_ms: f64,
    pub p99_ms: f64,
    pub p99_9_ms: f64,
    pub max_ms: f64,
    pub relative_bucket_width: f64,
}

impl LatencyHistogram {
    pub fn record(&mut self, duration: Duration) {
        let micros = duration.as_micros().min(u64::MAX as u128) as u64;
        let index = bucket_index(micros);
        if self.buckets.len() <= index {
            self.buckets.resize(index + 1, 0);
        }
        self.buckets[index] += 1;
        self.count += 1;
        self.sum_us += micros as u128;
        self.max_us = self.max_us.max(micros);
    }

    pub fn merge(&mut self, other: &Self) {
        if self.buckets.len() < other.buckets.len() {
            self.buckets.resize(other.buckets.len(), 0);
        }
        for (index, value) in other.buckets.iter().enumerate() {
            self.buckets[index] += value;
        }
        self.count += other.count;
        self.sum_us += other.sum_us;
        self.max_us = self.max_us.max(other.max_us);
    }

    pub fn percentile_us(&self, percentile: f64) -> u64 {
        if self.count == 0 {
            return 0;
        }
        let rank = ((self.count as f64 * percentile).ceil() as u64).max(1);
        let mut seen = 0;
        for (index, value) in self.buckets.iter().enumerate() {
            seen += value;
            if seen >= rank {
                return bucket_upper_bound(index).min(self.max_us.max(1));
            }
        }
        self.max_us
    }

    pub fn snapshot(&self) -> LatencySnapshot {
        LatencySnapshot {
            count: self.count,
            mean_ms: if self.count == 0 {
                0.0
            } else {
                self.sum_us as f64 / self.count as f64 / 1_000.0
            },
            p50_ms: self.percentile_us(0.50) as f64 / 1_000.0,
            p90_ms: self.percentile_us(0.90) as f64 / 1_000.0,
            p99_ms: self.percentile_us(0.99) as f64 / 1_000.0,
            p99_9_ms: self.percentile_us(0.999) as f64 / 1_000.0,
            max_ms: self.max_us as f64 / 1_000.0,
            relative_bucket_width: 0.01,
        }
    }
}

fn bucket_index(micros: u64) -> usize {
    if micros <= 1 {
        0
    } else {
        ((micros as f64).ln() / LOG_BASE.ln()).ceil() as usize
    }
}

fn bucket_upper_bound(index: usize) -> u64 {
    if index == 0 {
        1
    } else {
        LOG_BASE.powi(index.min(i32::MAX as usize) as i32).ceil() as u64
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn percentile_is_within_one_percent_bucket_width() {
        let mut histogram = LatencyHistogram::default();
        for micros in 1..=100 {
            histogram.record(Duration::from_micros(micros));
        }
        assert!((50..=51).contains(&histogram.percentile_us(0.50)));
        assert!((99..=100).contains(&histogram.percentile_us(0.99)));
        assert_eq!(histogram.snapshot().max_ms, 0.1);
    }
}
