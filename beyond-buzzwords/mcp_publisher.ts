import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StdioClientTransport } from "@modelcontextprotocol/sdk/client/stdio.js";
import process from "node:process";

import type { Publisher, VerificationResult } from "./types.js";

type MCPPublisherOptions = {
  outputDir: string;
  cwd?: string;
};

export const createMcpPublisher = (
  options: MCPPublisherOptions,
): Publisher => {
  return async (result: VerificationResult) => {
    const client = new Client({
      name: "beyond-buzzwords-agent",
      version: "1.0.0",
    }, {
      capabilities: {},
    });

    const transport = new StdioClientTransport({
      command: process.execPath,
      args: ["--import", "tsx", "mcp_server.ts"],
      env: {
        ...process.env,
        BB_OUT_DIR: options.outputDir,
      },
      cwd: options.cwd ??
        "/home/max/projects/notes/src/content/blog/beyond-buzzwords/implementation",
    });

    try {
      await client.connect(transport);
      await client.callTool({
        name: "publish_result",
        arguments: {
          prediction: result.prediction,
          page: result.page,
          message: result.message,
          numbers: result.numbers,
          leadingOnes: result.leadingOnes,
          leadingOneRatio: result.leadingOneRatio,
          threshold: result.threshold,
        },
      });
    } finally {
      await client.close();
    }
  };
};
