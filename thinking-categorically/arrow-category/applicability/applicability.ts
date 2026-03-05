// Arrow interface - represents computations that can be composed
interface Arrow<A, B> {
  run(input: A): B;
  compose<C>(other: Arrow<B, C>): Arrow<A, C>;
  first<D>(): Arrow<[A, D], [B, D]>;
  second<D>(): Arrow<[D, A], [D, B]>;
}

// Basic function arrow implementation
class FunctionArrow<A, B> implements Arrow<A, B> {
  constructor(private fn: (a: A) => B) {}

  run(input: A): B {
    return this.fn(input);
  }

  compose<C>(other: Arrow<B, C>): Arrow<A, C> {
    return new FunctionArrow<A, C>(a => other.run(this.run(a)));
  }

  first<D>(): Arrow<[A, D], [B, D]> {
    return new FunctionArrow<[A, D], [B, D]>(([a, d]) => [this.run(a), d]);
  }

  second<D>(): Arrow<[D, A], [D, B]> {
    return new FunctionArrow<[D, A], [D, B]>(([d, a]) => [d, this.run(a)]);
  }
}

// Helper function to lift regular functions to arrows
const arr = <A, B>(fn: (a: A) => B): Arrow<A, B> => new FunctionArrow(fn);

// Identity arrow
const identity = <A>(): Arrow<A, A> => arr(x => x);

// Basic arrow examples
const basicArrowExamples = () => {
  console.log("=== Basic Arrow Examples ===");

  const addOne = arr((x: number) => x + 1);
  const double = arr((x: number) => x * 2);
  const toString = arr((x: number) => x.toString());

  // Arrow composition
  const pipeline = addOne.compose(double).compose(toString);
  console.log(pipeline.run(5)); // "12"

  // First and second on pairs
  const pairTransform = addOne.first<number>().compose(double.second<number>());
  console.log(pairTransform.run([10, 20])); // [11, 40]
};

// Maybe arrow for handling nullable computations
class MaybeArrow<A, B> implements Arrow<A, B | null> {
  constructor(private fn: (a: A) => B | null) {}

  run(input: A): B | null {
    return this.fn(input);
  }

  compose<C>(other: Arrow<B | null, C>): Arrow<A, C> {
    return new FunctionArrow<A, C>(a => {
      const result = this.run(a);
      return result === null ? (null as unknown as C) : other.run(result);
    });
  }

  first<D>(): Arrow<[A, D], [B | null, D]> {
    return new FunctionArrow<[A, D], [B | null, D]>(([a, d]) => [this.run(a), d]);
  }

  second<D>(): Arrow<[D, A], [D, B | null]> {
    return new FunctionArrow<[D, A], [D, B | null]>(([d, a]) => [d, this.run(a)]);
  }
}

const safeDivide = new MaybeArrow<number, number>(x => x !== 0 ? 100 / x : null);
const safeSqrt = new MaybeArrow<number | null, number>(x =>
  x === null ? null : x >= 0 ? Math.sqrt(x) : null
);

const maybeArrowExamples = () => {
  console.log("\n=== Maybe Arrow Examples ===");

  const safeComputation = safeDivide.compose(safeSqrt);

  console.log(safeComputation.run(4));  // 5
  console.log(safeComputation.run(0));  // null (division by zero)
  console.log(safeComputation.run(-1)); // null (negative input to sqrt)
};

// Logging arrow that tracks computation steps
class LoggingArrow<A, B> implements Arrow<A, B> {
  constructor(
    private fn: (a: A) => B,
    private description: string
  ) {}

  run(input: A): B {
    console.log(`Executing: ${this.description} with input ${input}`);
    const result = this.fn(input);
    console.log(`Result: ${result}`);
    return result;
  }

  compose<C>(other: Arrow<B, C>): Arrow<A, C> {
    return new FunctionArrow<A, C>(a => other.run(this.run(a)));
  }

  first<D>(): Arrow<[A, D], [B, D]> {
    return new FunctionArrow<[A, D], [B, D]>(([a, d]) => [this.run(a), d]);
  }

  second<D>(): Arrow<[D, A], [D, B]> {
    return new FunctionArrow<[D, A], [D, B]>(([d, a]) => [d, this.run(a)]);
  }
}

const loggingArrowExamples = () => {
  console.log("\n=== Logging Arrow Examples ===");

  const addOneWithLog = new LoggingArrow((x: number) => x + 1, "add one");
  const doubleWithLog = new LoggingArrow((x: number) => x * 2, "double");

  const computation = addOneWithLog.compose(doubleWithLog);
  const result = computation.run(5);
  console.log(`Final result: ${result}`);
};

// State arrow for stateful computations
class StateArrow<S, A, B> implements Arrow<A, B> {
  constructor(private fn: (state: S, input: A) => [S, B], private initialState: S) {}

  run(input: A): B {
    const [_, result] = this.fn(this.initialState, input);
    return result;
  }

  runWithState(state: S, input: A): [S, B] {
    return this.fn(state, input);
  }

  compose<C>(other: Arrow<B, C>): Arrow<A, C> {
    return new FunctionArrow<A, C>(a => other.run(this.run(a)));
  }

  first<D>(): Arrow<[A, D], [B, D]> {
    return new FunctionArrow<[A, D], [B, D]>(([a, d]) => [this.run(a), d]);
  }

  second<D>(): Arrow<[D, A], [D, B]> {
    return new FunctionArrow<[D, A], [D, B]>(([d, a]) => [d, this.run(a)]);
  }
}

const stateArrowExamples = () => {
  console.log("\n=== State Arrow Examples ===");

  // Counter arrow that increments state and adds it to input
  const counter = new StateArrow<number, number, number>(
    (state, input) => [state + 1, input + state],
    0
  );

  console.log(counter.run(10)); // 10 (0 + 10)
  console.log(counter.run(10)); // 10 (still uses initial state)

  // To see stateful behavior, we need to thread state manually
  let state = 0;
  [state, ] = counter.runWithState(state, 10);
  console.log(`State after first call: ${state}`); // 1

  const [newState, result] = counter.runWithState(state, 10);
  console.log(`Result with threaded state: ${result}`); // 11 (1 + 10)
  console.log(`New state: ${newState}`); // 2
};

// Run all examples
basicArrowExamples();
maybeArrowExamples();
loggingArrowExamples();
stateArrowExamples();