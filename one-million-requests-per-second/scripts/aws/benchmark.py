#!/usr/bin/env python3
"""Provision, deploy, run, collect, and destroy the AWS benchmark environment."""

from __future__ import annotations

import argparse
import datetime as dt
import json
import math
import os
import pathlib
import shlex
import subprocess
import sys
import tempfile
import time
from typing import Any


ROOT = pathlib.Path(__file__).resolve().parents[2]
INFRA_ROOT = ROOT / "infra" / "aws"
CDK_STACK = "MillionRpsBenchmark"
TERMINAL_COMMAND_STATES = {"Success", "Cancelled", "Failed", "TimedOut"}
U64_MAX = (1 << 64) - 1


class BenchmarkError(RuntimeError):
    pass


def utc_datetime(timestamp: int) -> dt.datetime:
    """Return an aware UTC datetime on every supported Python version."""
    return dt.datetime.fromtimestamp(timestamp, dt.timezone.utc)


def positive_int(value: str) -> int:
    parsed = int(value)
    if parsed <= 0:
        raise argparse.ArgumentTypeError("must be greater than zero")
    return parsed


def nonnegative_int(value: str) -> int:
    parsed = int(value)
    if parsed < 0:
        raise argparse.ArgumentTypeError("must not be negative")
    return parsed


def positive_float(value: str) -> float:
    parsed = float(value)
    if not math.isfinite(parsed) or parsed <= 0:
        raise argparse.ArgumentTypeError("must be finite and greater than zero")
    return parsed


def error_rate(value: str) -> float:
    parsed = float(value)
    if not math.isfinite(parsed) or not 0 < parsed <= 1:
        raise argparse.ArgumentTypeError("must be greater than zero and at most one")
    return parsed


def u64(value: str) -> int:
    parsed = int(value)
    if not 0 <= parsed <= U64_MAX:
        raise argparse.ArgumentTypeError("must fit in an unsigned 64-bit integer")
    return parsed


def execute(
    command: list[str],
    *,
    capture: bool = False,
    input_text: str | None = None,
    cwd: pathlib.Path = ROOT,
    environment: dict[str, str] | None = None,
) -> str:
    printable = " ".join(shlex.quote(part) for part in command)
    print(f"+ {printable}", file=sys.stderr)
    result = subprocess.run(
        command,
        cwd=cwd,
        check=True,
        text=True,
        input=input_text,
        stdout=subprocess.PIPE if capture else None,
        env=os.environ | (environment or {}),
    )
    return result.stdout.strip() if capture else ""


class Aws:
    def __init__(self, region: str, profile: str | None):
        self.region = region
        self.profile = profile

    def command(self, *arguments: str) -> list[str]:
        command = ["aws", *arguments, "--region", self.region]
        if self.profile:
            command.extend(["--profile", self.profile])
        return command

    def text(self, *arguments: str) -> str:
        return execute(self.command(*arguments), capture=True)

    def json(self, *arguments: str) -> Any:
        output = self.text(*arguments, "--output", "json")
        return json.loads(output) if output else None


def cdk_command(aws: Aws, *arguments: str) -> list[str]:
    command = ["npm", "exec", "--", "cdk", *arguments, "--region", aws.region]
    if aws.profile:
        command.extend(["--profile", aws.profile])
    return command


def execute_cdk(aws: Aws, *arguments: str) -> None:
    execute(
        cdk_command(aws, *arguments),
        cwd=INFRA_ROOT,
        environment={
            "AWS_REGION": aws.region,
            "AWS_DEFAULT_REGION": aws.region,
        },
    )


def stack_outputs(aws: Aws, stack: str) -> dict[str, str]:
    response = aws.json("cloudformation", "describe-stacks", "--stack-name", stack)
    stacks = response.get("Stacks", [])
    if len(stacks) != 1:
        raise BenchmarkError(f"expected one CloudFormation stack named {stack}")
    return {
        item["OutputKey"]: item["OutputValue"]
        for item in stacks[0].get("Outputs", [])
    }


def tagged_instances(aws: Aws, stack: str, role: str) -> list[dict[str, Any]]:
    response = aws.json(
        "ec2",
        "describe-instances",
        "--filters",
        f"Name=tag:BenchmarkStack,Values={stack}",
        f"Name=tag:BenchmarkRole,Values={role}",
        "Name=instance-state-name,Values=running",
    )
    instances = [
        instance
        for reservation in response.get("Reservations", [])
        for instance in reservation.get("Instances", [])
    ]
    return sorted(instances, key=lambda item: item["InstanceId"])


def wait_for_managed_instances(aws: Aws, instance_ids: list[str], timeout_seconds: int = 600) -> None:
    deadline = time.monotonic() + timeout_seconds
    expected = set(instance_ids)
    while time.monotonic() < deadline:
        response = aws.json("ssm", "describe-instance-information")
        managed = {
            item["InstanceId"] for item in response.get("InstanceInformationList", [])
        }
        if expected <= managed:
            return
        time.sleep(5)
    missing = ", ".join(sorted(expected - managed))
    raise BenchmarkError(f"instances did not register with SSM: {missing}")


def send_command(
    aws: Aws,
    instance_id: str,
    command: str,
    *,
    execution_timeout: int = 3600,
) -> str:
    response = aws.json(
        "ssm",
        "send-command",
        "--document-name",
        "AWS-RunShellScript",
        "--instance-ids",
        instance_id,
        "--parameters",
        json.dumps(
            {
                "commands": [command],
                "executionTimeout": [str(execution_timeout)],
            }
        ),
        "--timeout-seconds",
        "60",
    )
    return response["Command"]["CommandId"]


def command_result(
    aws: Aws,
    command_id: str,
    instance_id: str,
    *,
    timeout_seconds: int,
) -> dict[str, Any]:
    deadline = time.monotonic() + timeout_seconds
    last: dict[str, Any] = {}
    while time.monotonic() < deadline:
        try:
            last = aws.json(
                "ssm",
                "get-command-invocation",
                "--command-id",
                command_id,
                "--instance-id",
                instance_id,
            )
        except subprocess.CalledProcessError:
            time.sleep(2)
            continue
        if last.get("Status") in TERMINAL_COMMAND_STATES:
            return last
        time.sleep(5)
    raise BenchmarkError(
        f"SSM command {command_id} on {instance_id} did not finish; last status {last.get('Status')}"
    )


def require_success(result: dict[str, Any], description: str) -> None:
    if result.get("Status") != "Success":
        stderr = result.get("StandardErrorContent", "").strip()
        stdout = result.get("StandardOutputContent", "").strip()
        raise BenchmarkError(
            f"{description} failed with {result.get('Status')}\n{stderr}\n{stdout}"
        )


def image_for_commit(aws: Aws, repository_uri: str, commit: str) -> str:
    repository_name = repository_uri.rsplit("/", 1)[-1]
    response = aws.json(
        "ecr",
        "describe-images",
        "--repository-name",
        repository_name,
        "--image-ids",
        f"imageTag={commit}",
    )
    details = response.get("imageDetails", [])
    if len(details) != 1:
        raise BenchmarkError(f"no unique ECR image tagged {commit}")
    return f"{repository_uri}@{details[0]['imageDigest']}"


def current_commit() -> str:
    return execute(["git", "rev-parse", "HEAD"], capture=True)


def assert_clean_repository() -> None:
    if execute(["git", "status", "--porcelain"], capture=True):
        raise BenchmarkError("the repository is dirty; commit the exact source before building")


def bootstrap(args: argparse.Namespace, aws: Aws) -> None:
    if not args.yes:
        raise BenchmarkError("bootstrap creates persistent AWS support resources; rerun with --yes")
    account = aws.text(
        "sts", "get-caller-identity", "--query", "Account", "--output", "text"
    )
    execute_cdk(aws, "bootstrap", f"aws://{account}/{aws.region}")


def available_availability_zones(aws: Aws) -> list[str]:
    response = aws.json(
        "ec2",
        "describe-availability-zones",
        "--filters",
        "Name=state,Values=available",
    )
    return sorted(
        zone["ZoneName"]
        for zone in response.get("AvailabilityZones", [])
        if zone.get("ZoneName")
    )


def provision(args: argparse.Namespace, aws: Aws) -> None:
    if not args.yes:
        raise BenchmarkError("provision creates billable resources; rerun with --yes")
    availability_zones = available_availability_zones(aws)
    if len(availability_zones) < 2:
        raise BenchmarkError(f"region {aws.region} does not expose two available zones")
    availability_zone = args.availability_zone or availability_zones[0]
    if availability_zone not in availability_zones:
        raise BenchmarkError(
            f"availability zone {availability_zone} is not available in {aws.region}"
        )
    database_secondary_availability_zone = next(
        zone for zone in availability_zones if zone != availability_zone
    )
    parameters = {
        "BenchmarkAvailabilityZone": availability_zone,
        "DatabaseSecondaryAvailabilityZone": database_secondary_availability_zone,
        "ServerInstanceType": args.server_instance_type,
        "LoadGeneratorInstanceType": args.loadgen_instance_type,
        "LoadGeneratorCount": args.loadgen_count,
        "DatabaseInstanceClass": args.database_instance_class,
        "DatabaseEngineVersion": args.database_engine_version,
        "DatabaseStorageGiB": args.database_storage_gib,
        "CacheCapacityBytes": args.cache_capacity_bytes,
        "ProtectDatabase": "true" if args.protect_database else "false",
    }
    parameter_arguments = [
        value
        for name, parameter in parameters.items()
        for value in ("--parameters", f"{args.stack}:{name}={parameter}")
    ]
    execute_cdk(
        aws,
        "deploy",
        CDK_STACK,
        "--context",
        f"stackName={args.stack}",
        "--require-approval",
        "never",
        *parameter_arguments,
    )
    print(json.dumps(stack_outputs(aws, args.stack), indent=2))


def build(args: argparse.Namespace, aws: Aws) -> None:
    assert_clean_repository()
    outputs = stack_outputs(aws, args.stack)
    repository_uri = outputs["ImageRepositoryUri"]
    commit = current_commit()
    repository_name = repository_uri.rsplit("/", 1)[-1]
    try:
        image = image_for_commit(aws, repository_uri, commit)
    except (BenchmarkError, subprocess.CalledProcessError):
        registry = repository_uri.split("/", 1)[0]
        password = aws.text("ecr", "get-login-password")
        execute(
            ["docker", "login", "--username", "AWS", "--password-stdin", registry],
            input_text=password,
        )
        execute(
            [
                "docker",
                "buildx",
                "build",
                "--platform",
                "linux/amd64",
                "--provenance=false",
                "--tag",
                f"{repository_uri}:{commit}",
                "--push",
                ".",
            ]
        )
        image = image_for_commit(aws, repository_uri, commit)
    print(image)
    details = aws.json(
        "ecr",
        "describe-images",
        "--repository-name",
        repository_name,
        "--image-ids",
        f"imageTag={commit}",
    )
    if details["imageDetails"][0].get("imageScanStatus", {}).get("status") == "FAILED":
        raise BenchmarkError("ECR image scan failed")


def remote_registry_login(image: str, region: str) -> str:
    registry = image.split("/", 1)[0]
    return (
        f"aws ecr get-login-password --region {shlex.quote(region)} "
        f"| docker login --username AWS --password-stdin {shlex.quote(registry)}"
    )


def deploy(args: argparse.Namespace, aws: Aws) -> None:
    assert_clean_repository()
    outputs = stack_outputs(aws, args.stack)
    commit = current_commit()
    image = image_for_commit(aws, outputs["ImageRepositoryUri"], commit)
    server = tagged_instances(aws, args.stack, "server")
    loadgens = tagged_instances(aws, args.stack, "loadgen")
    if len(server) != 1 or not loadgens:
        raise BenchmarkError("expected one server and at least one load generator")
    all_ids = [server[0]["InstanceId"], *[item["InstanceId"] for item in loadgens]]
    wait_for_managed_instances(aws, all_ids)

    parameter_prefix = f"/{args.stack}/tls"
    with tempfile.TemporaryDirectory(prefix="million-rps-tls-") as directory:
        execute(
            [
                str(ROOT / "scripts" / "generate-test-certs.sh"),
                outputs["ServerDnsName"],
                directory,
            ]
        )
        for name, kind in (
            ("ca.crt", "String"),
            ("server.crt", "SecureString"),
            ("server.key", "SecureString"),
        ):
            path = pathlib.Path(directory, name)
            execute(
                aws.command(
                    "ssm",
                    "put-parameter",
                    "--name",
                    f"{parameter_prefix}/{name}",
                    "--type",
                    kind,
                    "--value",
                    f"file://{path}",
                    "--overwrite",
                )
            )

    pull = f"""set -euo pipefail
while [ ! -f /var/lib/cloud/instance/boot-finished ]; do sleep 2; done
{remote_registry_login(image, aws.region)}
docker pull {shlex.quote(image)}
"""
    pull_commands = {
        instance_id: send_command(aws, instance_id, pull)
        for instance_id in all_ids
    }
    for instance_id, command_id in pull_commands.items():
        require_success(
            command_result(aws, command_id, instance_id, timeout_seconds=900),
            f"image pull on {instance_id}",
        )

    server_id = server[0]["InstanceId"]
    server_command = f"""set -euo pipefail
install -d -m 0755 /etc/million-rps
aws ssm get-parameter --region {shlex.quote(aws.region)} --name {shlex.quote(parameter_prefix + '/server.crt')} --with-decryption --query Parameter.Value --output text > /etc/million-rps/server.crt
aws ssm get-parameter --region {shlex.quote(aws.region)} --name {shlex.quote(parameter_prefix + '/server.key')} --with-decryption --query Parameter.Value --output text > /etc/million-rps/server.key
aws ssm get-parameter --region {shlex.quote(aws.region)} --name {shlex.quote(parameter_prefix + '/ca.crt')} --query Parameter.Value --output text > /etc/million-rps/ca.crt
chown 65532:65532 /etc/million-rps/server.crt /etc/million-rps/server.key
chmod 0600 /etc/million-rps/server.crt /etc/million-rps/server.key
chmod 0644 /etc/million-rps/ca.crt
secret_json=$(aws secretsmanager get-secret-value --region {shlex.quote(aws.region)} --secret-id {shlex.quote(outputs['DatabaseSecretArn'])} --query SecretString --output text)
db_user=$(printf '%s' "$secret_json" | jq -r .username)
db_password=$(printf '%s' "$secret_json" | jq -r .password)
encoded_password=$(jq -nr --arg value "$db_password" '$value|@uri')
database_url="postgres://${{db_user}}:${{encoded_password}}@{outputs['DatabaseEndpoint']}:{outputs['DatabasePort']}/benchmark?sslmode=verify-full&sslrootcert=/certs/rds-ca.pem"
docker run --rm --network host --entrypoint million-rps-dbtool \
  --env DATABASE_URL="$database_url" \
  --volume /etc/million-rps/rds-ca.pem:/certs/rds-ca.pem:ro \
  {shlex.quote(image)} \
  --product-count {args.product_count} --batch-size {args.batch_size}
cat > /etc/million-rps/server.env <<EOF
DATABASE_URL=$database_url
DATABASE_MAX_CONNECTIONS={args.database_connections}
CACHE_CAPACITY_BYTES={outputs['CacheCapacityBytes']}
CACHE_TTL_SECONDS={args.cache_ttl_seconds}
CACHE_TTL_JITTER_PERCENT=20
NEGATIVE_CACHE_TTL_SECONDS=30
TLS_CERT_PATH=/certs/server.crt
TLS_KEY_PATH=/certs/server.key
EOF
chmod 0600 /etc/million-rps/server.env
docker rm --force million-rps-server 2>/dev/null || true
docker run --detach --name million-rps-server --restart unless-stopped \
  --network host \
  --env-file /etc/million-rps/server.env \
  --volume /etc/million-rps:/certs:ro \
  {shlex.quote(image)}
"""
    command_id = send_command(aws, server_id, server_command, execution_timeout=7200)
    require_success(
        command_result(aws, command_id, server_id, timeout_seconds=7500),
        "database initialization and server deployment",
    )

    loadgen_command = f"""set -euo pipefail
aws ssm get-parameter --region {shlex.quote(aws.region)} --name {shlex.quote(parameter_prefix + '/ca.crt')} --query Parameter.Value --output text > /etc/million-rps/ca.crt
chown 65532:65532 /etc/million-rps/ca.crt /var/lib/million-rps/results
chmod 0644 /etc/million-rps/ca.crt
"""
    commands = {
        item["InstanceId"]: send_command(aws, item["InstanceId"], loadgen_command)
        for item in loadgens
    }
    for instance_id, command_id in commands.items():
        require_success(
            command_result(aws, command_id, instance_id, timeout_seconds=300),
            f"load-generator deployment on {instance_id}",
        )

    print(f"deployed {image} to one server and {len(loadgens)} load generators")


def split_evenly(total: int, shards: int, description: str) -> int:
    if total % shards:
        raise BenchmarkError(f"{description} {total} must be divisible by {shards} load generators")
    value = total // shards
    if value <= 0:
        raise BenchmarkError(f"{description} per generator must be positive")
    return value


def collect_remote_file(aws: Aws, instance_id: str, path: str) -> str:
    command_id = send_command(aws, instance_id, f"cat {shlex.quote(path)}", execution_timeout=60)
    result = command_result(aws, command_id, instance_id, timeout_seconds=120)
    require_success(result, f"collecting {path} from {instance_id}")
    return result.get("StandardOutputContent", "")


def collect_server_metrics(aws: Aws, instance_id: str, server_name: str) -> str:
    command = (
        "curl --fail --silent "
        "--cacert /etc/million-rps/ca.crt "
        f"--resolve {shlex.quote(server_name)}:8080:127.0.0.1 "
        f"https://{shlex.quote(server_name)}:8080/metrics"
    )
    command_id = send_command(aws, instance_id, command, execution_timeout=60)
    result = command_result(aws, command_id, instance_id, timeout_seconds=120)
    require_success(result, "collecting server metrics")
    return result.get("StandardOutputContent", "")


def prometheus_integer(metrics: str, name: str) -> int:
    prefix = f"{name} "
    matches = [line.removeprefix(prefix) for line in metrics.splitlines() if line.startswith(prefix)]
    if len(matches) != 1:
        raise BenchmarkError(f"server metrics did not contain exactly one {name!r} sample")
    try:
        value = int(matches[0])
    except ValueError as error:
        raise BenchmarkError(f"server metric {name!r} was not an integer") from error
    if value <= 0:
        raise BenchmarkError(f"server metric {name!r} was not positive")
    return value


def collect_database_stats(aws: Aws, instance_id: str, image: str) -> str:
    command = f"""docker run --rm --network host \
  --env-file /etc/million-rps/server.env \
  --volume /etc/million-rps/rds-ca.pem:/certs/rds-ca.pem:ro \
  --entrypoint million-rps-dbtool \
  {shlex.quote(image)} --stats
"""
    command_id = send_command(aws, instance_id, command, execution_timeout=60)
    result = command_result(aws, command_id, instance_id, timeout_seconds=120)
    require_success(result, "collecting PostgreSQL statistics")
    return result.get("StandardOutputContent", "")


def collect_cloudwatch_metrics(
    aws: Aws,
    *,
    server_id: str,
    loadgen_ids: list[str],
    database_identifier: str,
    start_at: int,
    finished_at: int,
) -> dict[str, Any]:
    queries: list[dict[str, Any]] = []

    def metric(
        identifier: str,
        namespace: str,
        name: str,
        dimensions: list[dict[str, str]],
        stat: str,
        period: int,
    ) -> None:
        queries.append(
            {
                "Id": identifier,
                "MetricStat": {
                    "Metric": {
                        "Namespace": namespace,
                        "MetricName": name,
                        "Dimensions": dimensions,
                    },
                    "Period": period,
                    "Stat": stat,
                },
                "ReturnData": True,
            }
        )

    ec2_nodes = [
        ("server", server_id),
        *[(f"loadgen{index}", value) for index, value in enumerate(loadgen_ids)],
    ]
    for label, instance_id in ec2_nodes:
        dimensions = [{"Name": "InstanceId", "Value": instance_id}]
        metric(f"{label}_cpu", "AWS/EC2", "CPUUtilization", dimensions, "Average", 60)
        metric(f"{label}_netin", "AWS/EC2", "NetworkIn", dimensions, "Sum", 60)
        metric(f"{label}_netout", "AWS/EC2", "NetworkOut", dimensions, "Sum", 60)
        metric(f"{label}_packetsin", "AWS/EC2", "NetworkPacketsIn", dimensions, "Sum", 60)
        metric(f"{label}_packetsout", "AWS/EC2", "NetworkPacketsOut", dimensions, "Sum", 60)
        metric(f"{label}_memory", "CWAgent", "mem_used_percent", dimensions, "Average", 10)

    database_dimensions = [{"Name": "DBInstanceIdentifier", "Value": database_identifier}]
    for identifier, name, stat in (
        ("database_cpu", "CPUUtilization", "Average"),
        ("database_connections", "DatabaseConnections", "Average"),
        ("database_read_iops", "ReadIOPS", "Average"),
        ("database_write_iops", "WriteIOPS", "Average"),
        ("database_read_latency", "ReadLatency", "Average"),
        ("database_write_latency", "WriteLatency", "Average"),
        ("database_queue_depth", "DiskQueueDepth", "Average"),
        ("database_free_memory", "FreeableMemory", "Average"),
        ("database_network_in", "NetworkReceiveThroughput", "Average"),
        ("database_network_out", "NetworkTransmitThroughput", "Average"),
    ):
        metric(identifier, "AWS/RDS", name, database_dimensions, stat, 60)

    start = utc_datetime(start_at).isoformat()
    end = utc_datetime(finished_at).isoformat()
    return aws.json(
        "cloudwatch",
        "get-metric-data",
        "--metric-data-queries",
        json.dumps(queries),
        "--start-time",
        start,
        "--end-time",
        end,
        "--scan-by",
        "TimestampAscending",
    )


def aggregate_results(
    shard_results: list[dict[str, Any]],
    *,
    total_rate: int,
    total_connections: int,
    cache_ttl_seconds: int,
    start_at: int,
    measurement_start_at: int,
    stack: str,
    image: str,
    commit: str,
) -> dict[str, Any]:
    metrics = [item["metrics"] for item in shard_results]
    achieved = sum(item["achieved_valid_rps"] for item in metrics)
    status_codes: dict[str, int] = {}
    for item in metrics:
        for status, count in item["status_codes"].items():
            status_codes[status] = status_codes.get(status, 0) + count
    errors = sum(
        item["transport_errors"] + item["unexpected_statuses"] + item["validation_errors"]
        for item in metrics
    )
    observed = sum(item["observed_requests"] for item in metrics)
    failure_reasons = [
        f"shard {index}: {reason}"
        for index, shard in enumerate(shard_results)
        for reason in shard["failure_reasons"]
    ]
    if achieved < total_rate:
        failure_reasons.append(
            f"aggregate valid completion rate {achieved:.3f} RPS was below {total_rate} RPS"
        )
    valid_run = all(item["valid_run"] for item in shard_results)
    thresholds_met = valid_run and all(
        item["thresholds_met"] for item in shard_results
    ) and achieved >= total_rate
    return {
        "schema_version": 1,
        "contract_version": 6,
        "kind": "distributed-benchmark-manifest",
        "stack": stack,
        "scheduled_start_unix_seconds": start_at,
        "scheduled_measurement_start_unix_seconds": measurement_start_at,
        "git_commit": commit,
        "image_digest": image,
        "config": {
            "aggregate_rate_rps": total_rate,
            "aggregate_connections": total_connections,
            "shard_count": len(shard_results),
            "server_cache_ttl_seconds": cache_ttl_seconds,
        },
        "metrics": {
            "observed_requests": observed,
            "valid_responses": sum(item["valid_responses"] for item in metrics),
            "achieved_valid_rps": achieved,
            "error_count": errors,
            "error_rate": errors / observed if observed else 1.0,
            "response_bytes": sum(item["response_bytes"] for item in metrics),
            "status_codes": status_codes,
            "conservative_max_shard_p99_ms": max(
                item["schedule_to_completion_latency"]["p99_ms"] for item in metrics
            ),
            "conservative_max_shard_p99_9_ms": max(
                item["schedule_to_completion_latency"]["p99_9_ms"] for item in metrics
            ),
        },
        "valid_run": valid_run,
        "thresholds_met": thresholds_met,
        "failure_reasons": failure_reasons,
        "shards": shard_results,
    }


def validate_shard_result(
    result: dict[str, Any],
    *,
    index: int,
    rate: int,
    connections: int,
    queue_depth_per_connection: int,
    warmup_rate: int,
    warmup_seconds: int,
    duration_seconds: int,
    product_count: int,
    zipf_exponent: float,
    seed: int,
    cache_capacity_bytes: int,
    cache_ttl_seconds: int,
    request_timeout_ms: int,
    max_p99_ms: int,
    max_error_rate: float,
    target: str,
    environment: str,
    start_at: int,
    measurement_start_at: int,
    commit: str,
    image: str,
) -> None:
    mismatches: list[str] = []

    def expect(description: str, actual: Any, expected: Any) -> None:
        if actual != expected:
            mismatches.append(f"{description} was {actual!r}, expected {expected!r}")

    config = result.get("config")
    warmup = result.get("warmup")
    measurement = result.get("measurement")
    metrics = result.get("metrics")
    for description, value in (
        ("config", config),
        ("warm-up report", warmup),
        ("measurement report", measurement),
        ("metrics", metrics),
    ):
        if not isinstance(value, dict):
            raise BenchmarkError(f"shard {index} {description} was not a JSON object")
    expect("schema version", result.get("schema_version"), 3)
    expect("contract version", result.get("contract_version"), 6)
    expect("git commit", result.get("git_commit"), commit)
    expect("image digest", result.get("image_digest"), image)
    expect("environment", result.get("environment"), environment)
    expect("warm-up start", result.get("started_unix_seconds"), start_at)
    expect(
        "measurement start",
        result.get("measurement_started_unix_seconds"),
        measurement_start_at,
    )
    expect("configured rate", config.get("rate_rps"), rate)
    expect("configured connections", config.get("connections"), connections)
    expect("configured warm-up rate", config.get("warmup_start_rate_rps"), warmup_rate)
    expect("configured warm-up duration", config.get("warmup_seconds"), warmup_seconds)
    expect("configured measurement duration", config.get("duration_seconds"), duration_seconds)
    expect("configured product count", config.get("product_count"), product_count)
    expect("configured seed", config.get("seed"), seed)
    expect("configured target", config.get("target"), target)
    expect("configured unknown rate", config.get("unknown_per_thousand"), 1)
    expect(
        "configured queue depth",
        config.get("queue_depth_per_connection"),
        queue_depth_per_connection,
    )
    expect("configured request timeout", config.get("request_timeout_ms"), request_timeout_ms)
    expect("configured p99 threshold", config.get("max_p99_ms"), max_p99_ms)
    expect("configured error threshold", config.get("max_error_rate"), max_error_rate)
    expect("configured CA certificate", config.get("ca_cert"), "/certs/ca.crt")
    expect(
        "configured cache capacity",
        config.get("server_cache_capacity_bytes"),
        cache_capacity_bytes,
    )
    expect(
        "configured cache TTL",
        config.get("server_cache_ttl_seconds"),
        cache_ttl_seconds,
    )
    expect("configured warm-up start", config.get("scheduled_start_unix_seconds"), start_at)
    expect(
        "configured measurement start",
        config.get("scheduled_measurement_start_unix_seconds"),
        measurement_start_at,
    )
    expect("warm-up start rate", warmup.get("start_rate_rps"), warmup_rate)
    expect("warm-up end rate", warmup.get("end_rate_rps"), rate)
    expect("measurement start rate", measurement.get("start_rate_rps"), rate)
    expect("measurement end rate", measurement.get("end_rate_rps"), rate)
    for field in (
        "observed_requests",
        "valid_responses",
        "achieved_valid_rps",
        "transport_errors",
        "unexpected_statuses",
        "validation_errors",
        "response_bytes",
        "status_codes",
        "schedule_to_completion_latency",
    ):
        if field not in metrics:
            mismatches.append(f"metrics omitted {field!r}")
    latency = metrics.get("schedule_to_completion_latency")
    if not isinstance(latency, dict) or "p99_ms" not in latency or "p99_9_ms" not in latency:
        mismatches.append("schedule-to-completion latency omitted p99 or p99.9")
    if not isinstance(result.get("failure_reasons"), list):
        mismatches.append("failure reasons were not a JSON array")
    actual_zipf = config.get("zipf_exponent")
    if not isinstance(actual_zipf, (int, float)) or not math.isclose(
        actual_zipf, zipf_exponent, rel_tol=0.0, abs_tol=1e-12
    ):
        mismatches.append(
            f"configured Zipf exponent was {actual_zipf!r}, expected {zipf_exponent!r}"
        )
    if mismatches:
        details = "\n".join(f"- {mismatch}" for mismatch in mismatches)
        raise BenchmarkError(f"shard {index} did not match the distributed run contract:\n{details}")


def run_benchmark(args: argparse.Namespace, aws: Aws) -> None:
    assert_clean_repository()
    if args.warmup_start_rate > args.rate:
        raise BenchmarkError("warm-up start rate must not exceed the measurement rate")
    if args.product_count > (1 << 63) - 1 - 10_000:
        raise BenchmarkError("product count is too large for PostgreSQL bigint request IDs")
    outputs = stack_outputs(aws, args.stack)
    commit = current_commit()
    image = image_for_commit(aws, outputs["ImageRepositoryUri"], commit)
    loadgens = tagged_instances(aws, args.stack, "loadgen")
    server = tagged_instances(aws, args.stack, "server")
    if len(server) != 1 or not loadgens:
        raise BenchmarkError("the deployed stack does not have the expected instances")
    shard_count = len(loadgens)
    shard_rate = split_evenly(args.rate, shard_count, "rate")
    shard_connections = split_evenly(args.connections, shard_count, "connections")
    shard_warmup_rate = split_evenly(args.warmup_start_rate, shard_count, "warm-up rate")
    if args.seed + (shard_count - 1) * 1_000_003 > U64_MAX:
        raise BenchmarkError("the derived per-shard seed does not fit in an unsigned 64-bit integer")
    wait_for_managed_instances(
        aws,
        [server[0]["InstanceId"], *[item["InstanceId"] for item in loadgens]],
    )

    metrics_before = collect_server_metrics(
        aws, server[0]["InstanceId"], outputs["ServerDnsName"]
    )
    cache_ttl_seconds = prometheus_integer(metrics_before, "cache_ttl_seconds")
    database_stats_before = collect_database_stats(
        aws, server[0]["InstanceId"], image
    )

    start_at = int(time.time()) + args.start_delay_seconds
    measurement_start_at = start_at + args.warmup_seconds + args.phase_transition_seconds
    run_name = utc_datetime(start_at).strftime("run-%Y%m%dT%H%M%SZ")
    environment = (
        f"AWS stack={args.stack} region={aws.region} az={outputs['AvailabilityZone']} "
        f"server={outputs['ServerInstanceType']} loadgen={outputs['LoadGeneratorInstanceType']} "
        f"database={outputs['DatabaseInstanceClass']} postgres={outputs['DatabaseEngineVersion']} "
        f"distributed_generators={shard_count}"
    )
    remote_paths: list[str] = []
    commands: dict[str, str] = {}
    for index, instance in enumerate(loadgens):
        instance_id = instance["InstanceId"]
        remote_path = f"/var/lib/million-rps/results/{run_name}-shard-{index}.json"
        remote_paths.append(remote_path)
        seed = args.seed + index * 1_000_003
        command = f"""set -euo pipefail
rm -f {shlex.quote(remote_path)}
docker run --rm --network host --entrypoint million-rps-loadgen \
  --volume /etc/million-rps/ca.crt:/certs/ca.crt:ro \
  --volume /var/lib/million-rps/results:/results \
  {shlex.quote(image)} \
  --target https://{outputs['ServerDnsName']}:8080 \
  --ca-cert /certs/ca.crt \
  --rate {shard_rate} \
  --connections {shard_connections} \
  --warmup-seconds {args.warmup_seconds} \
  --warmup-start-rate {shard_warmup_rate} \
  --duration-seconds {args.duration_seconds} \
  --product-count {args.product_count} \
  --zipf-exponent {args.zipf_exponent} \
  --seed {seed} \
  --unknown-per-thousand 1 \
  --queue-depth-per-connection {args.queue_depth_per_connection} \
  --request-timeout-ms {args.request_timeout_ms} \
  --server-cache-capacity-bytes {outputs['CacheCapacityBytes']} \
  --server-cache-ttl-seconds {cache_ttl_seconds} \
  --max-p99-ms {args.max_p99_ms} \
  --max-error-rate {args.max_error_rate} \
  --start-at-unix-seconds {start_at} \
  --measurement-start-at-unix-seconds {measurement_start_at} \
  --git-commit {commit} \
  --image-digest {shlex.quote(image)} \
  --environment {shlex.quote(environment + f' shard={index} instance={instance_id}')} \
  --output /results/{pathlib.PurePosixPath(remote_path).name}
"""
        commands[instance_id] = send_command(
            aws,
            instance_id,
            command,
            execution_timeout=(
                args.warmup_seconds
                + args.phase_transition_seconds
                + args.duration_seconds
                + 900
            ),
        )

    timeout_seconds = (
        args.start_delay_seconds
        + args.warmup_seconds
        + args.phase_transition_seconds
        + args.duration_seconds
        + 1200
    )
    command_results: dict[str, dict[str, Any]] = {}
    for instance_id, command_id in commands.items():
        command_results[instance_id] = command_result(
            aws,
            command_id,
            instance_id,
            timeout_seconds=timeout_seconds,
        )
        status = command_results[instance_id].get("Status")
        print(f"{instance_id}: benchmark command {status}", file=sys.stderr)
        if status == "Failed" and command_results[instance_id].get("ResponseCode") == 2:
            continue
        require_success(
            command_results[instance_id], f"benchmark command on {instance_id}"
        )

    output_directory = ROOT / "results" / "aws" / run_name
    output_directory.mkdir(parents=True, exist_ok=False)
    shard_results: list[dict[str, Any]] = []
    for index, (instance, remote_path) in enumerate(zip(loadgens, remote_paths, strict=True)):
        contents = collect_remote_file(aws, instance["InstanceId"], remote_path)
        shard_result = json.loads(contents)
        (output_directory / f"shard-{index}.json").write_text(
            json.dumps(shard_result, indent=2) + "\n"
        )
        validate_shard_result(
            shard_result,
            index=index,
            rate=shard_rate,
            connections=shard_connections,
            queue_depth_per_connection=args.queue_depth_per_connection,
            warmup_rate=shard_warmup_rate,
            warmup_seconds=args.warmup_seconds,
            duration_seconds=args.duration_seconds,
            product_count=args.product_count,
            zipf_exponent=args.zipf_exponent,
            seed=args.seed + index * 1_000_003,
            cache_capacity_bytes=int(outputs["CacheCapacityBytes"]),
            cache_ttl_seconds=cache_ttl_seconds,
            request_timeout_ms=args.request_timeout_ms,
            max_p99_ms=args.max_p99_ms,
            max_error_rate=args.max_error_rate,
            target=f"https://{outputs['ServerDnsName']}:8080/",
            environment=environment + f" shard={index} instance={instance['InstanceId']}",
            start_at=start_at,
            measurement_start_at=measurement_start_at,
            commit=commit,
            image=image,
        )
        command_status = command_results[instance["InstanceId"]].get("Status")
        if command_status == "Success" and (
            not shard_result.get("valid_run") or not shard_result.get("thresholds_met")
        ):
            raise BenchmarkError(
                f"shard {index} returned success for a benchmark result that failed its contract"
            )
        shard_results.append(shard_result)

    manifest = aggregate_results(
        shard_results,
        total_rate=args.rate,
        total_connections=args.connections,
        cache_ttl_seconds=cache_ttl_seconds,
        start_at=start_at,
        measurement_start_at=measurement_start_at,
        stack=args.stack,
        image=image,
        commit=commit,
    )
    manifest_path = output_directory / "manifest.json"
    benchmark_finished_at = max(item["finished_unix_seconds"] for item in shard_results)

    metrics_after = collect_server_metrics(
        aws, server[0]["InstanceId"], outputs["ServerDnsName"]
    )
    database_stats_after = collect_database_stats(
        aws, server[0]["InstanceId"], image
    )
    (output_directory / "server-metrics-before.prom").write_text(metrics_before)
    (output_directory / "server-metrics-after.prom").write_text(metrics_after)
    (output_directory / "database-stats-before.json").write_text(database_stats_before)
    (output_directory / "database-stats-after.json").write_text(database_stats_after)

    if args.metrics_delay_seconds:
        print(
            f"waiting {args.metrics_delay_seconds} seconds for CloudWatch metric ingestion",
            file=sys.stderr,
        )
        time.sleep(args.metrics_delay_seconds)
    cloudwatch = collect_cloudwatch_metrics(
        aws,
        server_id=server[0]["InstanceId"],
        loadgen_ids=[item["InstanceId"] for item in loadgens],
        database_identifier=outputs["DatabaseIdentifier"],
        start_at=start_at,
        finished_at=benchmark_finished_at,
    )
    (output_directory / "cloudwatch.json").write_text(
        json.dumps(cloudwatch, indent=2) + "\n"
    )

    environment_snapshot = {
        "stack_outputs": {
            key: value
            for key, value in outputs.items()
            if key != "DatabaseSecretArn"
        },
        "server": server,
        "load_generators": loadgens,
    }
    (output_directory / "environment.json").write_text(
        json.dumps(environment_snapshot, indent=2, default=str) + "\n"
    )
    manifest_path.write_text(json.dumps(manifest, indent=2) + "\n")
    print(manifest_path)
    if not manifest["valid_run"] or not manifest["thresholds_met"]:
        raise BenchmarkError("the distributed benchmark did not meet its contract")


def status(args: argparse.Namespace, aws: Aws) -> None:
    outputs = stack_outputs(aws, args.stack)
    response = {
        "outputs": outputs,
        "server": tagged_instances(aws, args.stack, "server"),
        "load_generators": tagged_instances(aws, args.stack, "loadgen"),
    }
    print(json.dumps(response, indent=2, default=str))


def destroy(args: argparse.Namespace, aws: Aws) -> None:
    if not args.yes:
        raise BenchmarkError("destroy permanently deletes the RDS database; rerun with --yes")
    for name in ("ca.crt", "server.crt", "server.key"):
        try:
            execute(
                aws.command(
                    "ssm",
                    "delete-parameter",
                    "--name",
                    f"/{args.stack}/tls/{name}",
                )
            )
        except subprocess.CalledProcessError:
            pass
    execute_cdk(
        aws,
        "destroy",
        CDK_STACK,
        "--context",
        f"stackName={args.stack}",
        "--force",
    )


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description=__doc__)
    result.add_argument("--region", default="us-east-1")
    result.add_argument("--profile")
    result.add_argument("--stack", default="million-rps-benchmark")
    subparsers = result.add_subparsers(dest="operation", required=True)

    bootstrap_parser = subparsers.add_parser("bootstrap")
    bootstrap_parser.add_argument("--yes", action="store_true")

    provision_parser = subparsers.add_parser("provision")
    provision_parser.add_argument("--availability-zone")
    provision_parser.add_argument("--server-instance-type", default="c6in.8xlarge")
    provision_parser.add_argument("--loadgen-instance-type", default="c6in.4xlarge")
    provision_parser.add_argument("--loadgen-count", type=positive_int, default=4)
    provision_parser.add_argument("--database-instance-class", default="db.r7i.2xlarge")
    provision_parser.add_argument("--database-engine-version", default="17.10")
    provision_parser.add_argument("--database-storage-gib", type=positive_int, default=100)
    provision_parser.add_argument(
        "--cache-capacity-bytes", type=positive_int, default=536_870_912
    )
    provision_parser.add_argument("--protect-database", action="store_true")
    provision_parser.add_argument("--yes", action="store_true")

    subparsers.add_parser("build")

    deploy_parser = subparsers.add_parser("deploy")
    deploy_parser.add_argument("--product-count", type=positive_int, default=10_000_000)
    deploy_parser.add_argument("--batch-size", type=positive_int, default=100_000)
    deploy_parser.add_argument("--database-connections", type=positive_int, default=32)
    deploy_parser.add_argument("--cache-ttl-seconds", type=positive_int, default=3_600)

    run_parser = subparsers.add_parser("run")
    run_parser.add_argument("--rate", type=positive_int, default=1_000_000)
    run_parser.add_argument("--connections", type=positive_int, default=4096)
    run_parser.add_argument("--queue-depth-per-connection", type=positive_int, default=64)
    run_parser.add_argument("--warmup-start-rate", type=positive_int, default=1000)
    run_parser.add_argument("--warmup-seconds", type=positive_int, default=300)
    run_parser.add_argument("--duration-seconds", type=positive_int, default=1800)
    run_parser.add_argument("--product-count", type=positive_int, default=10_000_000)
    run_parser.add_argument("--zipf-exponent", type=positive_float, default=1.1)
    run_parser.add_argument("--seed", type=u64, default=104_729)
    run_parser.add_argument("--request-timeout-ms", type=positive_int, default=5000)
    run_parser.add_argument("--max-p99-ms", type=positive_int, default=50)
    run_parser.add_argument("--max-error-rate", type=error_rate, default=0.001)
    run_parser.add_argument("--start-delay-seconds", type=positive_int, default=120)
    run_parser.add_argument("--phase-transition-seconds", type=positive_int, default=30)
    run_parser.add_argument("--metrics-delay-seconds", type=nonnegative_int, default=90)

    subparsers.add_parser("status")
    destroy_parser = subparsers.add_parser("destroy")
    destroy_parser.add_argument("--yes", action="store_true")
    return result


def main() -> None:
    args = parser().parse_args()
    aws = Aws(args.region, args.profile)
    operations = {
        "bootstrap": bootstrap,
        "provision": provision,
        "build": build,
        "deploy": deploy,
        "run": run_benchmark,
        "status": status,
        "destroy": destroy,
    }
    try:
        operations[args.operation](args, aws)
    except (BenchmarkError, subprocess.CalledProcessError, json.JSONDecodeError) as error:
        print(f"error: {error}", file=sys.stderr)
        raise SystemExit(1) from error


if __name__ == "__main__":
    main()
