import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { fileURLToPath } from "node:url";
import { z } from "zod";

import { createFilePublisher } from "./publish.js";
import { createSsmPublisher } from "./remote_publish.js";
import type { VerificationResult } from "./types.js";

const outputDir = process.env.BB_OUT_DIR ??
  fileURLToPath(new URL("./out", import.meta.url));
const publishMode = process.env.BB_PUBLISH_MODE ?? "file";

const publisher = publishMode === "ssm"
  ? createSsmPublisher({
    region: process.env.BB_AWS_REGION ?? process.env.AWS_REGION ?? "",
    instanceId: process.env.BB_INSTANCE_ID ?? "",
    publicUrl: process.env.BB_PUBLIC_URL,
  })
  : createFilePublisher(outputDir);

if (publishMode === "ssm") {
  if (!process.env.BB_INSTANCE_ID) {
    throw new Error("BB_INSTANCE_ID is required when BB_PUBLISH_MODE=ssm");
  }
  if (!(process.env.BB_AWS_REGION ?? process.env.AWS_REGION)) {
    throw new Error("BB_AWS_REGION or AWS_REGION is required when BB_PUBLISH_MODE=ssm");
  }
}

const server = new McpServer({
  name: "beyond-buzzwords-publisher",
  version: "1.0.0",
});

server.tool(
  "publish_result",
  {
    prediction: z.enum(["verified", "failed"]),
    page: z.enum(["SUCCESS", "FAILURE"]),
    message: z.string(),
    numbers: z.array(z.number()),
    leadingOnes: z.number().optional(),
    leadingOneRatio: z.number(),
    threshold: z.number(),
  },
  async (input) => {
    const result: VerificationResult = {
      prediction: input.prediction,
      page: input.page,
      message: input.message,
      numbers: input.numbers,
      leadingOnes: input.leadingOnes ??
        input.numbers.filter((value) => String(Math.abs(value)).startsWith("1")).length,
      leadingOneRatio: input.leadingOneRatio,
      threshold: input.threshold,
    };

    await publisher(result);

    return {
      content: [
        {
          type: "text",
          text:
            publishMode === "ssm"
              ? `Published ${result.page} to ${process.env.BB_PUBLIC_URL ?? process.env.BB_INSTANCE_ID} with ratio ${result.leadingOneRatio.toFixed(3)}`
              : `Published ${result.page} to ${outputDir}/index.html with ratio ${result.leadingOneRatio.toFixed(3)}`,
        },
      ],
    };
  },
);

const main = async () => {
  const transport = new StdioServerTransport();
  await server.connect(transport);
};

main().catch((error) => {
  console.error("MCP server error:", error);
  process.exit(1);
});
