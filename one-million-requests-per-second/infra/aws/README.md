# AWS single-server benchmark

This environment measures one Rust server without making the load generator
compete for the same host. It deliberately uses plain EC2 for the server and
generators, RDS PostgreSQL for the database, ECR for immutable images, and SSM
Run Command instead of SSH.

The stack creates billable resources. Its defaults are one `c6in.8xlarge`, four
`c6in.4xlarge` instances, and one `db.r7i.2xlarge` RDS instance. Confirm current
regional availability and pricing before provisioning. The database contains
deterministic benchmark data and is deleted by the teardown command unless
database protection was explicitly enabled.

## Prerequisites

- AWS CLI v2 credentials able to manage CloudFormation, IAM, EC2, Auto Scaling,
  ECR, RDS, Route 53, SSM and Secrets Manager
- Node.js 22 and npm (the CDK CLI and libraries are pinned in `package-lock.json`)
- Docker Buildx
- Python 3.10 or newer
- OpenSSL
- a clean Git commit containing the exact source to benchmark

No inbound SSH rule is created. The instances receive public addresses only so
they can reach SSM, ECR and package repositories without a NAT gateway;
benchmark traffic uses private addresses inside one cluster placement group.
RDS has no public address.

Install the exact infrastructure dependencies and verify the synthesized stack:

```sh
cd infra/aws
npm ci
npm run build
npm test
npm run synth
cd ../..
```

## Lifecycle

Commands default to `us-east-1` and the stack name
`million-rps-benchmark`. Put global options before the operation, for example
`--profile personal provision`.

Bootstrap CDK once for each AWS account and region. This creates the
`CDKToolkit` stack and its persistent deployment roles, S3 bucket and ECR
repository; destroying the benchmark stack does not remove them:

```sh
./scripts/aws/benchmark.py \
  --region us-east-1 \
  bootstrap --yes
```

Provision the infrastructure only after reviewing the instance choices:

```sh
./scripts/aws/benchmark.py \
  --region us-east-1 \
  provision --yes
```

The stack pins PostgreSQL `17.10`, disables automatic minor upgrades and records
the selected EC2/RDS types. Override a type when it is unavailable in the
chosen region:

```sh
./scripts/aws/benchmark.py provision \
  --availability-zone us-east-1b \
  --server-instance-type c6in.8xlarge \
  --loadgen-instance-type c6in.4xlarge \
  --loadgen-count 4 \
  --database-instance-class db.r7i.2xlarge \
  --cache-capacity-bytes 536870912 \
  --yes
```

When `--availability-zone` is omitted, the controller uses the first available
zone returned by EC2. It automatically places the secondary RDS subnet in a
different available zone. Select another zone explicitly when EC2 reports
insufficient capacity for the benchmark instance type.

Build the clean commit for `linux/amd64`, push it to ECR and resolve its digest:

```sh
./scripts/aws/benchmark.py build
```

Deploy that digest, create a seven-day benchmark CA, initialize ten million RDS
rows, and start the TLS server:

```sh
./scripts/aws/benchmark.py deploy \
  --cache-ttl-seconds 3600
```

Run a short, inexpensive validation before a publication-length measurement:

```sh
./scripts/aws/benchmark.py run \
  --rate 40000 \
  --connections 512 \
  --warmup-seconds 30 \
  --duration-seconds 60
```

The full contract defaults are one million aggregate RPS, 4,096 aggregate
connections, a five-minute ramp and a thirty-minute measurement:

```sh
./scripts/aws/benchmark.py run
```

All aggregate rates, connection counts and warm-up start rates must divide
evenly by the number of generators. The controller schedules every shard for
common warm-up and measurement UTC seconds, with a 30-second transition interval
after the nominal warm-up. Override that interval with
`--phase-transition-seconds` only when recording a distinct run configuration.
The controller rejects shards whose schedule, source commit, image digest, or
workload parameters differ. It collects each complete JSON result and writes a
manifest under `results/aws/`. It also records before/after Prometheus snapshots
and CloudWatch time series for EC2 CPU, memory, network, packets, and RDS CPU,
connections, I/O, latency, memory, queue depth, and network. PostgreSQL
`pg_stat_database` counters are captured on both sides of the run for buffer-hit
and transaction deltas. The manifest uses the maximum shard p99 as a
conservative latency summary; the individual shard histograms remain the source
data.

Inspect resources and immutable identifiers at any point:

```sh
./scripts/aws/benchmark.py status
```

Delete the environment as soon as the run is collected:

```sh
./scripts/aws/benchmark.py destroy --yes
```

If `--protect-database` was used during provisioning, disable RDS deletion
protection before teardown. TLS parameters are removed by the controller. ECR
is configured to empty on stack deletion.

## Infrastructure source

The TypeScript CDK app is in `bin/benchmark.ts` and
`lib/benchmark-stack.ts`. It intentionally uses L1 CloudFormation resources so
the benchmark topology stays explicit: one public benchmark subnet, two
private RDS subnets, a cluster placement group, no NAT gateway, no inbound SSH,
and no public database. The single-AZ RDS instance is pinned to the same
Availability Zone as the server and load generators. CDK assertions in
`test/benchmark-stack.test.ts` protect that topology and the output names
consumed by the Python controller.

`provision` runs `cdk deploy`; `destroy` runs `cdk destroy`. CDK synthesizes the
stack to CloudFormation and CloudFormation remains the deployment engine.

## Scope

This is a single-server capacity experiment. It does not yet satisfy the
contract's fleet-resilience condition or measure a load balancer. After finding
the valid capacity of one isolated server, a separate fleet stack can size the
number of serving targets and test removal of one target.
