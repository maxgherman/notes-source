# Beyond the Buzzwords Remote Target

This directory contains the smallest useful AWS target for the remote publisher:

- one EC2 instance
- `k3s` installed by user data
- one `nginx` container running inside the cluster
- public HTTP on port `80` through the default k3s ingress path
- SSM enabled through the instance role
- a placeholder `WAITING` page deployed at startup

The point is to provision a real remote surface that the local publisher can update later without dragging in Kubernetes immediately.
The point is to keep Kubernetes present but small enough to understand.

## Why this shape

This keeps the first remote step:

- real AWS infrastructure
- cheap enough to test
- reachable over HTTP
- controllable through SSM
- explicit enough to show what Kubernetes is really doing

That makes it a good base for the next layer, where the local implementation sends `SUCCESS` or `FAILURE` to the instance.

## Install

From this directory:

```sh
npm install
```

If your AWS account is not bootstrapped for CDK yet:

```sh
npx cdk bootstrap
```

## Deploy

```sh
npm run deploy
```

The stack outputs:

- `InstanceId`
- `PublicIp`
- `PublicUrl`

`InstanceId` is the important one for the remote publisher path.

## Destroy

```sh
npm run destroy
```

## What the instance is prepared for

The EC2 box is ready for an SSM-based publisher that:

- updates the `result-site-content` `ConfigMap`
- restarts the `result-site` deployment
- waits for the rollout to complete

The next step on the local side will be to use the stack outputs like this:

```sh
BB_PUBLISH_MODE=ssm
BB_AWS_REGION=<region>
BB_INSTANCE_ID=<instance-id>
```

At that point, the local implementation can stop writing to `out/` and start publishing a real remote page instead.

## Redeploy note

If you already deployed the earlier Nginx-on-EC2 version, run `npm run deploy` again after these changes. The k3s bootstrap lives in EC2 user data, so the instance needs to be replaced or updated through CloudFormation before the Kubernetes-based publisher can work.
