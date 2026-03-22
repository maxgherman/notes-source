export type PredictionStatus = "verified" | "failed";
export type ResultPage = "SUCCESS" | "FAILURE";

export type VerificationResult = {
  prediction: PredictionStatus;
  page: ResultPage;
  message: string;
  numbers: number[];
  leadingOnes: number;
  leadingOneRatio: number;
  threshold: number;
};

export type NumberExtractor = (rawInput: string) => Promise<number[]>;

export type Publisher = (result: VerificationResult) => Promise<void>;
