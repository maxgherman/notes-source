import { mkdir, writeFile } from "node:fs/promises";

import type { Publisher, VerificationResult } from "./types.js";

export const renderPage = (result: VerificationResult): string => {
  const accent = result.page === "SUCCESS" ? "#1f7a4d" : "#9e2a2b";
  const bg = result.page === "SUCCESS" ? "#eef8f1" : "#fdf0f0";

  return `<!doctype html>
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <title>${result.page}</title>
    <style>
      :root {
        color-scheme: light;
        --bg: ${bg};
        --fg: #0f172a;
        --accent: ${accent};
        --muted: #475569;
      }
      body {
        margin: 0;
        min-height: 100vh;
        display: grid;
        place-items: center;
        background: radial-gradient(circle at top, #ffffff, var(--bg));
        font-family: Georgia, "Times New Roman", serif;
        color: var(--fg);
      }
      main {
        width: min(720px, calc(100vw - 3rem));
        padding: 3rem;
        border: 1px solid color-mix(in srgb, var(--accent) 22%, white);
        background: rgba(255, 255, 255, 0.82);
        box-shadow: 0 24px 60px rgba(15, 23, 42, 0.12);
      }
      h1 {
        margin: 0 0 1rem;
        font-size: clamp(3rem, 10vw, 5rem);
        letter-spacing: 0.08em;
        color: var(--accent);
      }
      p {
        margin: 0.6rem 0;
        line-height: 1.55;
        color: var(--muted);
      }
      code {
        font-size: 0.95rem;
        color: var(--fg);
      }
    </style>
  </head>
  <body>
    <main>
      <h1>${result.page}</h1>
      <p>${result.message}</p>
      <p><strong>Leading ones:</strong> ${result.leadingOnes}/${result.numbers.length}</p>
      <p><strong>Ratio:</strong> ${result.leadingOneRatio.toFixed(3)} (threshold ${result.threshold})</p>
      <p><strong>Numbers:</strong> <code>${JSON.stringify(result.numbers)}</code></p>
    </main>
  </body>
</html>`;
};

export const createFilePublisher = (
  outputDir: string,
): Publisher => {
  return async (result: VerificationResult) => {
    await mkdir(outputDir, { recursive: true });
    await writeFile(
      `${outputDir}/index.html`,
      renderPage(result),
      "utf8",
    );
    await writeFile(
      `${outputDir}/result.json`,
      JSON.stringify(result, null, 2),
      "utf8",
    );
  };
};
