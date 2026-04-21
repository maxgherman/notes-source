// Enhanced monoid interface with guaranteed identity
interface Monoid<T> {
  empty: T;
  combine: (a: T, b: T) => T;
}

// Functional helper: create chunks from array
const createChunks = <T>(data: T[], chunkSize: number): T[][] => {
  const chunks: T[][] = [];
  for (let i = 0; i < data.length; i += chunkSize) {
    chunks.push(data.slice(i, i + chunkSize));
  }
  return chunks;
};

// Functional helper: simulate parallel chunk processing
const processChunk = <T>(monoid: Monoid<T>) => (chunk: T[]): Promise<T> =>
  new Promise(resolve => {
    setTimeout(() => {
      // Empty chunks safely reduce to identity element
      const result = chunk.reduce(monoid.combine, monoid.empty);
      resolve(result);
    }, Math.random() * 50);
  });

// Safe parallel fold - always returns a value, never null/undefined
const parallelFold = <T>(monoid: Monoid<T>) => async (data: T[]): Promise<T> => {
  if (data.length === 0) return monoid.empty;  // Identity element!

  const chunkSize = Math.max(1, Math.ceil(data.length / (navigator.hardwareConcurrency || 4)));
  const chunks = createChunks(data, chunkSize);

  // Process chunks in parallel, empty chunks automatically become identity
  const chunkResults = await Promise.all(chunks.map(processChunk(monoid)));

  // Combine all results - empty chunks contribute identity element
  return chunkResults.reduce(monoid.combine, monoid.empty);
};

// Enhanced map-fold with load balancing
const parallelMapFold = <T, U>(monoid: Monoid<T>) => (mapper: (item: U) => T) =>
  async (data: U[]): Promise<T> => {
    const chunks = createChunks(data, 1000);

    const chunkResults = await Promise.all(
      chunks.map(chunk => processChunk(monoid)(chunk.map(mapper)))
    );

    return chunkResults.reduce(monoid.combine, monoid.empty);
  };

// Enhanced monoids with better parallel characteristics
const NumberAddMonoid: Monoid<number> = {
  empty: 0,
  combine: (a, b) => a + b
};

const NumberMultiplyMonoid: Monoid<number> = {
  empty: 1,
  combine: (a, b) => a * b
};

// Statistics monoid for parallel data analysis
interface Statistics {
  count: number;
  sum: number;
  sumSquares: number;
}

const StatisticsMonoid: Monoid<Statistics> = {
  empty: { count: 0, sum: 0, sumSquares: 0 },
  combine: (a, b) => ({
    count: a.count + b.count,
    sum: a.sum + b.sum,
    sumSquares: a.sumSquares + b.sumSquares
  })
};

// Usage demonstration - functional composition style
async function demonstrateParallelMonoids() {
  const numbers = Array.from({length: 1000000}, (_, i) => i + 1);
  const emptyArray: number[] = [];

  // Create specialized functions using partial application
  const parallelSum = parallelMapFold<number, number>(NumberAddMonoid)((x: number) => x);
  const parallelFoldSum = parallelFold(NumberAddMonoid);
  const parallelStats = parallelMapFold<Statistics, number>(StatisticsMonoid)(
    (x: number) => ({ count: 1, sum: x, sumSquares: x * x })
  );

  console.time('Parallel Sum');
  const sum = await parallelSum(numbers);
  console.timeEnd('Parallel Sum');
  console.log('Parallel sum:', sum);

  // Empty arrays are perfectly safe - no null checks needed!
  const emptySum = await parallelFoldSum(emptyArray);
  console.log('Empty array sum:', emptySum);  // 0, not null/undefined

  // Parallel statistics collection
  console.time('Parallel Stats');
  const stats = await parallelStats(numbers.slice(0, 10000));
  console.timeEnd('Parallel Stats');

  const mean = stats.sum / stats.count;
  const variance = (stats.sumSquares / stats.count) - (mean * mean);
  console.log(`Statistics: count=${stats.count}, mean=${mean.toFixed(2)}, variance=${variance.toFixed(2)}`);

  // Functional composition example: pipeline of operations
  const processLargeDataset = async (data: number[]) => {
    const sum = await parallelSum(data);
    const stats = await parallelStats(data);
    const product = await parallelMapFold<number, number>(NumberMultiplyMonoid)((x: number) => x)(data.slice(0, 10));

    return { sum, stats, product };
  };

  const results = await processLargeDataset([1, 2, 3, 4, 5]);
  console.log('Pipeline results:', results);
}

demonstrateParallelMonoids();
