// Array endofunctor - transforms within the same type system
const numbers = [1, 2, 3, 4];
const doubled = numbers.map(x => x * 2); // Still Array<number>

// Optional/Maybe endofunctor
class Optional<T> {
  constructor(private value: T | null) {}

  map<U>(f: (value: T) => U): Optional<U> {
    return this.value === null ?
      new Optional<U>(null) :
      new Optional(f(this.value));
  }

  static of<T>(value: T | null): Optional<T> {
    return new Optional(value);
  }

  toString(): string {
    return this.value === null ? "None" : `Some(${this.value})`;
  }
}

// Usage - all operations stay within the TypeScript type system
const opt1 = Optional.of(5).map(x => x * 2);        // Optional<number>
const opt2 = Optional.of("hello").map(s => s.length); // Optional<number>
const opt3 = Optional.of<number>(null).map(x => x + 1); // Optional<number>

console.log(opt1.toString()); // Some(10)
console.log(opt2.toString()); // Some(5)
console.log(opt3.toString()); // None

// Promise endofunctor - transforms within async context
const promise: Promise<number> = Promise.resolve(42);
const transformedPromise: Promise<string> = promise.then(x => x.toString());

transformedPromise.then(console.log);