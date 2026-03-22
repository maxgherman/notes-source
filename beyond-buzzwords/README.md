# Beyond the Buzzwords Implementation

This directory contains a minimal runnable implementation of the local side of the article.

What it does:

- reads numbers from text
- optionally uses Ollama to extract numbers from messy input
- verifies the prediction that most numbers begin with `1`
- publishes a result page and result JSON to `out/`
- can publish either directly to files or through a real MCP tool

## Requirements

Required:

- `node`
- `npm`

Optional:

- `ollama` only if you want to use real local LLM extraction instead of deterministic regex extraction

If you do not install Ollama, the deterministic mode still works and is enough to run the local flow end to end.

## Run

From this directory you can use:

```sh
npm install
npm run run
```

or:

```sh
npm run run:input
```

Failure case:

```sh
npm run run:failure
```

Local MCP mode:

```sh
npm run run:mcp
```

Failure case through MCP:

```sh
npm run run:failure:mcp
```

Deterministic mode:

```sh
node --import tsx src/content/blog/beyond-buzzwords/implementation/main.ts \
  --file src/content/blog/beyond-buzzwords/implementation/sample-input.txt
```

Inline input:

```sh
node --import tsx src/content/blog/beyond-buzzwords/implementation/main.ts \
  --input "12 145 18 1099 31 176 102 87"
```

## Ollama setup

You only need this if you want the `--ollama-model` mode.

Install Ollama:

- Linux:

```sh
curl -fsSL https://ollama.com/install.sh | sh
```

- macOS:
  Install the Ollama app from `https://ollama.com/download` and make sure the `ollama` CLI is available in your shell.

- Windows:
  Install from `https://ollama.com/download/windows` or use:

```powershell
irm https://ollama.com/install.ps1 | iex
```

Once installed, make sure Ollama is running.

On Linux or in standalone CLI mode:

```sh
ollama serve
```

Then pull a small local model:

```sh
ollama pull qwen2.5:3b
```

Ollama serves its local API on `http://localhost:11434`.

Ollama-backed extraction:

```sh
npm run run:ollama
```

or:

```sh
node --import tsx src/content/blog/beyond-buzzwords/implementation/main.ts \
  --input "The audience gave me 12, 145, 18, 1099, 31, 176, 102 and 87." \
  --ollama-model qwen2.5:3b
```

Ollama plus MCP:

```sh
npm run run:ollama:mcp
```

## Output

The run writes:

- `out/index.html`
- `out/result.json`

This is intentionally local and minimal. The next step will be swapping the file publisher for a real remote publisher through MCP and AWS.

## MCP mode

When you use `--publisher mcp`, the agent does not write the output files directly. Instead it:

1. starts a local MCP server over stdio
2. calls the `publish_result` tool
3. lets that tool write `out/index.html` and `out/result.json`

This keeps the current implementation fully local while introducing a real MCP boundary.

## Remote publish over SSM

If you have deployed the CDK stack in [remote/](/home/max/projects/notes/src/content/blog/beyond-buzzwords/implementation/remote), the same MCP tool can publish to the EC2 instance instead of local files.

Set these environment variables before running:

```sh
export BB_PUBLISH_MODE=ssm
export BB_AWS_REGION=<aws-region>
export BB_INSTANCE_ID=<instance-id>
export BB_PUBLIC_URL=<public-url>
```

Then run:

```sh
npm run run:mcp
```

To publish a deliberate `FAILURE` result instead:

```sh
npm run run:failure:mcp
```

Or with Ollama:

```sh
npm run run:ollama:mcp
```

In this mode, the MCP server sends an SSM command to the instance, writes:

- a new `result-site-content` `ConfigMap`
- restarts the `result-site` deployment in `k3s`
- waits for the rollout to complete

The remote side is now:

- one EC2 instance
- a single-node `k3s` cluster
- one `nginx` container
- one `ConfigMap` holding `index.html` and `result.json`
- one `Service`
- one `Ingress`

If you deployed the earlier plain-Nginx version, redeploy the stack before using this path.
