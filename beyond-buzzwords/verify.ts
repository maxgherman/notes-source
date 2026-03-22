import type { VerificationResult } from "./types.js";

export const leadingDigit = (value: number): number => {
  const digits = String(Math.abs(value)).replace(/^0+/, "");
  return digits.length > 0 ? Number(digits[0]) : 0;
};

export const verifyPrediction = (
  numbers: number[],
  threshold = 0.7,
): VerificationResult => {
  const leadingOnes = numbers.filter((value) => leadingDigit(value) === 1).length;
  const leadingOneRatio = numbers.length === 0 ? 0 : leadingOnes / numbers.length;
  const verified = leadingOneRatio >= threshold;

  return {
    prediction: verified ? "verified" : "failed",
    page: verified ? "SUCCESS" : "FAILURE",
    message: verified
      ? "Most numbers begin with 1"
      : "Observed numbers did not match the expected pattern",
    numbers,
    leadingOnes,
    leadingOneRatio,
    threshold,
  };
};
