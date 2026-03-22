import type { NumberExtractor, Publisher, VerificationResult } from "./types.js";
import { verifyPrediction } from "./verify.js";

export const runAgent = async (
  rawInput: string,
  extractNumbers: NumberExtractor,
  publish: Publisher,
  threshold = 0.7,
): Promise<VerificationResult> => {
  const numbers = await extractNumbers(rawInput);

  if (numbers.length === 0) {
    throw new Error("No numeric input found");
  }

  const result = verifyPrediction(numbers, threshold);
  await publish(result);
  return result;
};
