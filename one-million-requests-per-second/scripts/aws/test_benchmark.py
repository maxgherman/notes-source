import importlib.util
import io
import pathlib
import unittest
from contextlib import redirect_stderr
from unittest.mock import patch


SCRIPT = pathlib.Path(__file__).with_name("benchmark.py")
SPEC = importlib.util.spec_from_file_location("benchmark", SCRIPT)
assert SPEC is not None and SPEC.loader is not None
benchmark = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(benchmark)


class ControllerTests(unittest.TestCase):
    def test_utc_timestamp_conversion_supports_python_3_10(self) -> None:
        self.assertEqual(
            benchmark.utc_datetime(0).isoformat(), "1970-01-01T00:00:00+00:00"
        )

    def test_reads_a_positive_integer_prometheus_gauge(self) -> None:
        self.assertEqual(
            benchmark.prometheus_integer(
                "# TYPE cache_ttl_seconds gauge\ncache_ttl_seconds 3600\n",
                "cache_ttl_seconds",
            ),
            3_600,
        )
        with self.assertRaises(benchmark.BenchmarkError):
            benchmark.prometheus_integer("", "cache_ttl_seconds")

    def shard_result(self) -> dict:
        return {
            "schema_version": 3,
            "contract_version": 6,
            "started_unix_seconds": 1_000,
            "measurement_started_unix_seconds": 1_330,
            "finished_unix_seconds": 1_390,
            "git_commit": "abc123",
            "image_digest": "repository@sha256:digest",
            "environment": "test shard=0 instance=i-test",
            "config": {
                "target": "https://server.benchmark.internal:8080/",
                "rate_rps": 250_000,
                "connections": 1_024,
                "warmup_start_rate_rps": 250,
                "warmup_seconds": 300,
                "duration_seconds": 60,
                "product_count": 10_000_000,
                "zipf_exponent": 1.1,
                "seed": 104_729,
                "unknown_per_thousand": 1,
                "queue_depth_per_connection": 64,
                "request_timeout_ms": 5_000,
                "max_p99_ms": 50,
                "max_error_rate": 0.001,
                "ca_cert": "/certs/ca.crt",
                "server_cache_capacity_bytes": 536_870_912,
                "server_cache_ttl_seconds": 3_600,
                "scheduled_start_unix_seconds": 1_000,
                "scheduled_measurement_start_unix_seconds": 1_330,
            },
            "warmup": {"start_rate_rps": 250, "end_rate_rps": 250_000},
            "measurement": {
                "start_rate_rps": 250_000,
                "end_rate_rps": 250_000,
            },
            "metrics": {
                "observed_requests": 15_000_000,
                "valid_responses": 15_000_000,
                "achieved_valid_rps": 250_000.0,
                "transport_errors": 0,
                "unexpected_statuses": 0,
                "validation_errors": 0,
                "response_bytes": 1,
                "status_codes": {"200": 15_000_000},
                "schedule_to_completion_latency": {"p99_ms": 2, "p99_9_ms": 3},
            },
            "valid_run": True,
            "thresholds_met": True,
            "failure_reasons": [],
        }

    def validate(self, result: dict) -> None:
        benchmark.validate_shard_result(
            result,
            index=0,
            rate=250_000,
            connections=1_024,
            queue_depth_per_connection=64,
            warmup_rate=250,
            warmup_seconds=300,
            duration_seconds=60,
            product_count=10_000_000,
            zipf_exponent=1.1,
            seed=104_729,
            cache_capacity_bytes=536_870_912,
            cache_ttl_seconds=3_600,
            request_timeout_ms=5_000,
            max_p99_ms=50,
            max_error_rate=0.001,
            target="https://server.benchmark.internal:8080/",
            environment="test shard=0 instance=i-test",
            start_at=1_000,
            measurement_start_at=1_330,
            commit="abc123",
            image="repository@sha256:digest",
        )

    def test_accepts_matching_shard_provenance_and_schedule(self) -> None:
        self.validate(self.shard_result())

    def test_rejects_a_measurement_that_started_at_a_different_time(self) -> None:
        result = self.shard_result()
        result["measurement_started_unix_seconds"] = 1_331
        with self.assertRaisesRegex(benchmark.BenchmarkError, "measurement start"):
            self.validate(result)

    def test_rejects_a_mismatched_image(self) -> None:
        result = self.shard_result()
        result["image_digest"] = "repository@sha256:other"
        with self.assertRaisesRegex(benchmark.BenchmarkError, "image digest"):
            self.validate(result)

    def test_splits_aggregate_values_exactly(self) -> None:
        self.assertEqual(benchmark.split_evenly(1_000_000, 4, "rate"), 250_000)
        with self.assertRaises(benchmark.BenchmarkError):
            benchmark.split_evenly(10, 4, "rate")

    def test_threshold_failure_does_not_relabel_a_structurally_valid_run(self) -> None:
        result = self.shard_result()
        result["thresholds_met"] = False
        result["failure_reasons"] = ["p99 exceeded the threshold"]
        manifest = benchmark.aggregate_results(
            [result],
            total_rate=250_000,
            total_connections=1_024,
            cache_ttl_seconds=3_600,
            start_at=1_000,
            measurement_start_at=1_330,
            stack="test",
            image="repository@sha256:digest",
            commit="abc123",
        )
        self.assertEqual(manifest["config"]["server_cache_ttl_seconds"], 3_600)
        self.assertTrue(manifest["valid_run"])
        self.assertFalse(manifest["thresholds_met"])

    def test_cli_rejects_nonpositive_durations_and_invalid_error_rates(self) -> None:
        parser = benchmark.parser()
        self.assertEqual(parser.parse_args(["status"]).region, "us-east-1")
        for arguments in (
            ["run", "--duration-seconds", "0"],
            ["run", "--queue-depth-per-connection", "0"],
            ["deploy", "--cache-ttl-seconds", "0"],
            ["run", "--max-error-rate", "0"],
            ["run", "--zipf-exponent", "nan"],
        ):
            with self.subTest(arguments=arguments), redirect_stderr(io.StringIO()):
                with self.assertRaises(SystemExit):
                    parser.parse_args(arguments)

    def test_cdk_command_propagates_region_and_profile(self) -> None:
        command = benchmark.cdk_command(
            benchmark.Aws("ap-southeast-2", "personal"), "synth"
        )
        self.assertEqual(
            command[-4:], ["--region", "ap-southeast-2", "--profile", "personal"]
        )

    def test_provision_qualifies_parameters_with_physical_stack_name(self) -> None:
        args = benchmark.parser().parse_args(
            [
                "--stack",
                "custom-stack",
                "provision",
                "--cache-capacity-bytes",
                "4294967296",
                "--yes",
            ]
        )
        aws = benchmark.Aws("us-east-1", "admin")

        with (
            patch.object(
                benchmark,
                "available_availability_zones",
                return_value=["us-east-1a", "us-east-1b"],
            ),
            patch.object(benchmark, "execute_cdk") as execute_cdk,
            patch.object(benchmark, "stack_outputs", return_value={}),
        ):
            benchmark.provision(args, aws)

        command = execute_cdk.call_args.args
        self.assertIn(
            "custom-stack:CacheCapacityBytes=4294967296",
            command,
        )
        self.assertNotIn(
            "MillionRpsBenchmark:CacheCapacityBytes=4294967296",
            command,
        )

    def test_provision_selects_a_distinct_secondary_database_zone(self) -> None:
        args = benchmark.parser().parse_args(
            ["provision", "--availability-zone", "us-east-1b", "--yes"]
        )
        aws = benchmark.Aws("us-east-1", "admin")

        with (
            patch.object(
                benchmark,
                "available_availability_zones",
                return_value=["us-east-1a", "us-east-1b", "us-east-1c"],
            ),
            patch.object(benchmark, "execute_cdk") as execute_cdk,
            patch.object(benchmark, "stack_outputs", return_value={}),
        ):
            benchmark.provision(args, aws)

        command = execute_cdk.call_args.args
        self.assertIn("million-rps-benchmark:BenchmarkAvailabilityZone=us-east-1b", command)
        self.assertIn(
            "million-rps-benchmark:DatabaseSecondaryAvailabilityZone=us-east-1a",
            command,
        )


if __name__ == "__main__":
    unittest.main()
