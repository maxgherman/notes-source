import { readFile } from "node:fs/promises";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { runAgent } from "./agent.js";
import {
  createOllamaExtractor,
  extractNumbersDeterministic,
} from "./extract.js";
import { createFilePublisher } from "./publish.js";
import { createMcpPublisher } from "./mcp_publisher.js";

const CURRENT_DIR = fileURLToPath(new URL(".", import.meta.url));
const DEFAULT_OUTPUT_DIR = fileURLToPath(new URL("./out", import.meta.url));

const usage = `Usage:
  node --import tsx main.ts --input "12 145 18 1099 31 176 102 87"
  node --import tsx main.ts --file ./sample-input.txt

Options:
  --input <text>            Raw input string
  --file <path>             Read raw input from file
  --threshold <number>      Verification threshold, default 0.7
  --out <path>              Output directory, default ./out
  --publisher <mode>        file | mcp, default file
  --ollama-model <name>     Enable real LLM extraction via Ollama
  --ollama-url <url>        Override Ollama base URL
`;

const parseArgs = (args: string[]) => {
  const values: Record<string, string> = {};

  for (let index = 0; index < args.length; index += 1) {
    const arg = args[index];
    if (!arg.startsWith("--")) continue;
    const key = arg.slice(2);
    const value = args[index + 1];
    if (!value || value.startsWith("--")) {
      values[key] = "true";
      continue;
    }
    values[key] = value;
    index += 1;
  }

  return values;
};

const isMain = process.argv[1] != null &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url);

if (isMain) {
  const args = parseArgs(process.argv.slice(2));

  if (args.help === "true") {
    console.log(usage);
    process.exit(0);
  }

  const rawInput = args.input
    ? args.input
    : args.file
    ? await readFile(args.file, "utf8")
    : null;

  if (!rawInput) {
    console.error(usage);
    process.exit(1);
  }

  const threshold = args.threshold ? Number(args.threshold) : 0.7;
  const outputDir = args.out ?? DEFAULT_OUTPUT_DIR;
  const publisherMode = args.publisher ?? "file";

  const extractor = args["ollama-model"]
    ? createOllamaExtractor({
      model: args["ollama-model"],
      baseUrl: args["ollama-url"],
    })
    : extractNumbersDeterministic;

  const publish = publisherMode === "mcp"
    ? createMcpPublisher({ outputDir, cwd: CURRENT_DIR })
    : createFilePublisher(outputDir);
  const result = await runAgent(rawInput, extractor, publish, threshold);

  console.log(JSON.stringify(result, null, 2));

  if (publisherMode === "mcp") {
    const remoteMode = process.env.BB_PUBLISH_MODE;

    if (remoteMode === "ssm") {
      const target = process.env.BB_PUBLIC_URL ?? process.env.BB_INSTANCE_ID ?? "remote target";
      console.log(`Published result to ${target} through MCP and SSM`);
    } else {
      console.log(`Published result through MCP to ${outputDir}/index.html`);
    }
  } else {
    console.log(`Wrote output to ${outputDir}/index.html`);
  }
}
