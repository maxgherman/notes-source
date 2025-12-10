// Bifunctor interface
interface Bifunctor<A, B> {
  bimap<C, D>(f: (a: A) => C, g: (b: B) => D): Bifunctor<C, D>;
  first<C>(f: (a: A) => C): Bifunctor<C, B>;
  second<D>(g: (b: B) => D): Bifunctor<A, D>;
}

// Tuple/Pair bifunctor
class Tuple<A, B> implements Bifunctor<A, B> {
  constructor(private _first: A, private _second: B) {}

  bimap<C, D>(f: (a: A) => C, g: (b: B) => D): Tuple<C, D> {
    return new Tuple(f(this._first), g(this._second));
  }

  first<C>(f: (a: A) => C): Tuple<C, B> {
    return new Tuple(f(this._first), this._second);
  }

  second<D>(g: (b: B) => D): Tuple<A, D> {
    return new Tuple(this._first, g(this._second));
  }

  toString(): string {
    return `(${this._first}, ${this._second})`;
  }
}

// Either bifunctor (sum type)
abstract class Either<A, B> implements Bifunctor<A, B> {
  abstract bimap<C, D>(f: (a: A) => C, g: (b: B) => D): Either<C, D>;
  abstract first<C>(f: (a: A) => C): Either<C, B>;
  abstract second<D>(g: (b: B) => D): Either<A, D>;

  static left<A, B>(value: A): Either<A, B> {
    return new Left(value);
  }

  static right<A, B>(value: B): Either<A, B> {
    return new Right(value);
  }
}

class Left<A, B> extends Either<A, B> {
  constructor(private value: A) { super(); }

  bimap<C, D>(f: (a: A) => C, g: (b: B) => D): Either<C, D> {
    return new Left<C, D>(f(this.value));
  }

  first<C>(f: (a: A) => C): Either<C, B> {
    return new Left<C, B>(f(this.value));
  }

  second<D>(g: (b: B) => D): Either<A, D> {
    return new Left<A, D>(this.value);
  }

  toString(): string { return `Left(${this.value})`; }
}

class Right<A, B> extends Either<A, B> {
  constructor(private value: B) { super(); }

  bimap<C, D>(f: (a: A) => C, g: (b: B) => D): Either<C, D> {
    return new Right<C, D>(g(this.value));
  }

  first<C>(f: (a: A) => C): Either<C, B> {
    return new Right<C, B>(this.value);
  }

  second<D>(g: (b: B) => D): Either<A, D> {
    return new Right<A, D>(g(this.value));
  }

  toString(): string { return `Right(${this.value})`; }
}

// Usage examples
const tupleExample = () => {
  const pair = new Tuple(5, "hello");

  console.log("Original:", pair.toString());                    // (5, hello)
  console.log("Bimap:", pair.bimap(x => x * 2, s => s.length).toString()); // (10, 5)
  console.log("First:", pair.first(x => x * 3).toString());     // (15, hello)
  console.log("Second:", pair.second(s => s + "!").toString()); // (5, hello!)
};

const eitherExample = () => {
  const leftVal = Either.left<number, string>(42);
  const rightVal = Either.right<number, string>("world");

  console.log("Left bimap:", leftVal.bimap(x => x * 2, s => s.length).toString());   // Left(84)
  console.log("Right bimap:", rightVal.bimap(x => x * 2, s => s.length).toString()); // Right(5)
};

tupleExample();
eitherExample();
