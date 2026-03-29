// Semigroup interface
interface Semigroup<T> {
  combine(a: T, b: T): T;
}

// Sum semigroup
const SumSemigroup: Semigroup<number> = {
  combine: (a, b) => a + b
};

// Simulate parallel processing with Promise-based chunks
async function parallelReduce<T>(
  semigroup: Semigroup<T>,
  data: T[]
): Promise<T | null> {
  if (data.length === 0) return null;
  if (data.length === 1) return data[0];

  // Split into chunks for parallel processing
  const chunkSize = Math.ceil(data.length / (navigator.hardwareConcurrency || 4));
  const chunks: T[][] = [];

  for (let i = 0; i < data.length; i += chunkSize) {
    chunks.push(data.slice(i, i + chunkSize));
  }

  // Process chunks in parallel (simulated with setTimeout)
  const chunkPromises = chunks.map(chunk =>
    new Promise<T>((resolve) => {
      setTimeout(() => {
        // Sequential reduce within each chunk
        const result = chunk.reduce(semigroup.combine);
        resolve(result);
      }, Math.random() * 100); // Simulate processing time
    })
  );

  // Wait for all chunks to complete
  const chunkResults: T[] = await Promise.all(chunkPromises);

  // Combine chunk results using a simple loop
  if (chunkResults.length === 0) return null;
  if (chunkResults.length === 1) return chunkResults[0];

  let result = chunkResults[0];
  for (let i = 1; i < chunkResults.length; i++) {
    result = semigroup.combine(result, chunkResults[i]);
  }
  return result;
}

// Map-reduce with parallel processing
async function parallelMapReduce<T, U>(
  semigroup: Semigroup<T>,
  data: U[],
  mapper: (item: U) => T
): Promise<T | null> {
  const mapped = data.map(mapper);
  return parallelReduce(semigroup, mapped);
}

// Example usage
async function demonstrateParallelSum() {
  const numbers = Array.from({length: 1000000}, (_, i) => i + 1);

  console.time('Parallel Sum');
  const parallelSum = await parallelMapReduce(SumSemigroup, numbers, x => x);
  console.timeEnd('Parallel Sum');
  console.log('Parallel sum result:', parallelSum);

  // Compare with sequential processing
  console.time('Sequential Sum');
  const sequentialSum = numbers.reduce((a, b) => a + b, 0);
  console.timeEnd('Sequential Sum');
  console.log('Sequential sum result:', sequentialSum);
}

// Run the demonstration
demonstrateParallelSum();
