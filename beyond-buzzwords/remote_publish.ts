import {
  GetCommandInvocationCommand,
  SSMClient,
  SendCommandCommand,
} from "@aws-sdk/client-ssm";

import { renderPage } from "./publish.js";
import type { Publisher, VerificationResult } from "./types.js";

type SsmPublisherOptions = {
  region: string;
  instanceId: string;
  publicUrl?: string;
  pollIntervalMs?: number;
  timeoutMs?: number;
};

const TERMINAL_FAILURE_STATUSES = new Set([
  "Cancelled",
  "Cancelling",
  "Delayed",
  "Failed",
  "TimedOut",
]);

const sleep = (ms: number) =>
  new Promise((resolve) => setTimeout(resolve, ms));

const encodeFile = (content: string) => Buffer.from(content, "utf8").toString("base64");

const buildCommands = (result: VerificationResult): string[] => {
  const page64 = encodeFile(renderPage(result));
  const json64 = encodeFile(JSON.stringify(result, null, 2));

  return [
    "set -euo pipefail",
    "export KUBECONFIG=/etc/rancher/k3s/k3s.yaml",
    "install -d -m 0755 /tmp/result-site",
    `printf '%s' '${page64}' | base64 -d > /tmp/result-site/index.html`,
    `printf '%s' '${json64}' | base64 -d > /tmp/result-site/result.json`,
    "kubectl -n beyond-buzzwords create configmap result-site-content --from-file=index.html=/tmp/result-site/index.html --from-file=result.json=/tmp/result-site/result.json --dry-run=client -o yaml | kubectl apply -f -",
    "kubectl -n beyond-buzzwords rollout restart deployment/result-site",
    "kubectl -n beyond-buzzwords rollout status deployment/result-site --timeout=180s",
  ];
};

export const createSsmPublisher = (
  options: SsmPublisherOptions,
): Publisher => {
  const client = new SSMClient({ region: options.region });
  const pollIntervalMs = options.pollIntervalMs ?? 2_000;
  const timeoutMs = options.timeoutMs ?? 90_000;

  return async (result: VerificationResult) => {
    const send = await client.send(new SendCommandCommand({
      DocumentName: "AWS-RunShellScript",
      InstanceIds: [options.instanceId],
      Parameters: {
        commands: buildCommands(result),
      },
      Comment: `Publish ${result.page} for Beyond the Buzzwords`,
    }));

    const commandId = send.Command?.CommandId;
    if (!commandId) {
      throw new Error("SSM did not return a command id");
    }

    const startedAt = Date.now();

    while (Date.now() - startedAt < timeoutMs) {
      try {
        const invocation = await client.send(new GetCommandInvocationCommand({
          CommandId: commandId,
          InstanceId: options.instanceId,
        }));

        if (invocation.Status === "Success") {
          return;
        }

        if (invocation.Status && TERMINAL_FAILURE_STATUSES.has(invocation.Status)) {
          throw new Error(
            `SSM publish failed with status ${invocation.Status}: ${invocation.StandardErrorContent ?? "no stderr"}`,
          );
        }
      } catch (error) {
        if (
          error instanceof Error &&
          error.name !== "InvocationDoesNotExist"
        ) {
          throw error;
        }
      }

      await sleep(pollIntervalMs);
    }

    throw new Error(
      `Timed out waiting for SSM command ${commandId} on ${options.instanceId}`,
    );
  };
};
