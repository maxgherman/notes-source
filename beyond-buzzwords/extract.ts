import type { NumberExtractor } from "./types.js";

const DEFAULT_OLLAMA_URL = "http://127.0.0.1:11434";

export const extractNumbersDeterministic: NumberExtractor = async (rawInput) => {
  const matches = rawInput.match(/-?\d+(?:\.\d+)?/g) ?? [];
  return matches.map(Number).filter((value) => Number.isFinite(value));
};

type OllamaOptions = {
  baseUrl?: string;
  model: string;
};

export const createOllamaExtractor = (
  options: OllamaOptions,
): NumberExtractor => {
  const baseUrl = options.baseUrl ?? DEFAULT_OLLAMA_URL;

  return async (rawInput: string) => {
    const response = await fetch(`${baseUrl}/api/chat`, {
      method: "POST",
      headers: {
        "content-type": "application/json",
      },
      body: JSON.stringify({
        model: options.model,
        stream: false,
        messages: [
          {
            role: "system",
            content:
              "Extract all numbers from the user input. Return JSON only in the shape {\"numbers\":[...]} with numeric values.",
          },
          {
            role: "user",
            content: rawInput,
          },
        ],
      }),
    });

    if (!response.ok) {
      const body = await response.text();
      throw new Error(`Ollama request failed: ${response.status} ${body}`);
    }

    const payload = await response.json() as {
      message?: { content?: string };
    };

    const content = payload.message?.content?.trim();

    if (!content) {
      throw new Error("Ollama response did not contain message content");
    }

    const parsed = JSON.parse(content) as { numbers?: unknown[] };
    const numbers = Array.isArray(parsed.numbers) ? parsed.numbers : [];

    return numbers
      .map((value) => Number(value))
      .filter((value) => Number.isFinite(value));
  };
};
