// Async Reader: environment R -> Promise<A>
type ReaderAsync<R, A> = (r: R) => Promise<A>;

class ReaderT<R, A> {
  constructor(public readonly run: ReaderAsync<R, A>) {}

  // Functor: map
  map<B>(f: (a: A) => B): ReaderT<R, B> {
    return new ReaderT(async (r: R) => {
      const a = await this.run(r);
      return f(a);
    });
  }

  // Applicative: ap (parallel async version)
  ap<B>(rf: ReaderT<R, (a: A) => B>): ReaderT<R, B> {
    return new ReaderT(async (r: R) => {
      // Run both in parallel
      const [func, val] = await Promise.all([rf.run(r), this.run(r)]);
      return func(val);
    });
  }

  // Applicative: pure
  static of<R, A>(value: A): ReaderT<R, A> {
    return new ReaderT(() => Promise.resolve(value));
  }
}

// Environment type
type Env = { multiplier: number };

// Simulated async computation that adds to the multiplier
const readerA = new ReaderT<Env, number>(async (env) => {
  await delay(100); // simulate latency
  return 5 + env.multiplier;
});

// Simulated async computation that produces a function
const readerF = new ReaderT<Env, (x: number) => number>(async (env) => {
  await delay(100); // simulate latency
  return (x: number) => x * env.multiplier;
});

// Run both computations in parallel and apply result
const resultReader = readerA.ap(readerF);

// Run with environment
(async () => {
  const result = await resultReader.run({ multiplier: 3 });
  console.log(result); // (5 + 3) * 3 = 8 * 3 = 24
})();

// Helper delay function
function delay(ms: number): Promise<void> {
  return new Promise(res => setTimeout(res, ms));
}
