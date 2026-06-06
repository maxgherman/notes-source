// Generic Monad interface
interface Monad<T> {
    bind<U>(f: (value: T) => Monad<U>): Monad<U>;
    map<U>(f: (value: T) => U): Monad<U>;
}

// Maybe Monad Implementation
abstract class Maybe<T> implements Monad<T> {
    abstract bind<U>(f: (value: T) => Maybe<U>): Maybe<U>;
    abstract map<U>(f: (value: T) => U): Maybe<U>;
    abstract isSome(): boolean;
    abstract isNone(): boolean;

    static pure<T>(value: T): Maybe<T> {
        return new Some(value);
    }

    static none<T>(): Maybe<T> {
        return new None<T>();
    }
}

class Some<T> extends Maybe<T> {
    constructor(private value: T) {
        super();
    }

    bind<U>(f: (value: T) => Maybe<U>): Maybe<U> {
        return f(this.value);
    }

    map<U>(f: (value: T) => U): Maybe<U> {
        return Maybe.pure(f(this.value));
    }

    isSome(): boolean { return true; }
    isNone(): boolean { return false; }

    getValue(): T { return this.value; }

    toString(): string { return `Some(${this.value})`; }
}

class None<T> extends Maybe<T> {
    bind<U>(f: (value: T) => Maybe<U>): Maybe<U> {
        return new None<U>();
    }

    map<U>(f: (value: T) => U): Maybe<U> {
        return new None<U>();
    }

    isSome(): boolean { return false; }
    isNone(): boolean { return true; }

    toString(): string { return "None"; }
}

// Either Monad Implementation
abstract class Either<L, R> implements Monad<R> {
    abstract bind<U>(f: (value: R) => Either<L, U>): Either<L, U>;
    abstract map<U>(f: (value: R) => U): Either<L, U>;
    abstract isLeft(): boolean;
    abstract isRight(): boolean;

    static left<L, R>(value: L): Either<L, R> {
        return new Left<L, R>(value);
    }

    static right<L, R>(value: R): Either<L, R> {
        return new Right<L, R>(value);
    }
}

class Left<L, R> extends Either<L, R> {
    constructor(private value: L) {
        super();
    }

    bind<U>(f: (value: R) => Either<L, U>): Either<L, U> {
        return new Left<L, U>(this.value);
    }

    map<U>(f: (value: R) => U): Either<L, U> {
        return new Left<L, U>(this.value);
    }

    isLeft(): boolean { return true; }
    isRight(): boolean { return false; }

    getLeft(): L { return this.value; }

    toString(): string { return `Left(${this.value})`; }
}

class Right<L, R> extends Either<L, R> {
    constructor(private value: R) {
        super();
    }

    bind<U>(f: (value: R) => Either<L, U>): Either<L, U> {
        return f(this.value);
    }

    map<U>(f: (value: R) => U): Either<L, U> {
        return Either.right<L, U>(f(this.value));
    }

    isLeft(): boolean { return false; }
    isRight(): boolean { return true; }

    getRight(): R { return this.value; }

    toString(): string { return `Right(${this.value})`; }
}

// Array Monad Implementation
class ArrayMonad<T> implements Monad<T> {
    constructor(private values: T[]) {}

    static pure<T>(value: T): ArrayMonad<T> {
        return new ArrayMonad([value]);
    }

    bind<U>(f: (value: T) => ArrayMonad<U>): ArrayMonad<U> {
        const results: U[] = [];
        for (const value of this.values) {
            results.push(...f(value).getValues());
        }
        return new ArrayMonad(results);
    }

    map<U>(f: (value: T) => U): ArrayMonad<U> {
        return new ArrayMonad(this.values.map(f));
    }

    getValues(): T[] { return this.values; }

    toString(): string { return `Array([${this.values.join(', ')}])`; }
}

// Example usage
function safeDiv(x: number, y: number): Maybe<number> {
    return y === 0 ? Maybe.none<number>() : Maybe.pure(x / y);
}

function parseNumber(str: string): Either<string, number> {
    const num = parseFloat(str);
    return isNaN(num) ?
        Either.left<string, number>(`Cannot parse '${str}' as number`) :
        Either.right<string, number>(num);
}

// Maybe monad calculation
const maybeResult = Maybe.pure(10)
    .bind(x => Maybe.pure(2).bind(y => safeDiv(x, y)))
    .map(z => z * 3);

console.log(maybeResult.toString()); // Some(15)

// Either monad calculation
const eitherResult = parseNumber("10")
    .bind(x => parseNumber("2").bind(y =>
        y === 0 ? Either.left<string, number>("Division by zero") :
                  Either.right<string, number>(x / y)))
    .map(z => z * 3);

console.log(eitherResult.toString()); // Right(15)

// Array monad (non-deterministic computation)
const arrayResult = ArrayMonad.pure(1)
    .bind(x => new ArrayMonad([x, x + 1, x + 2]))
    .bind(y => new ArrayMonad([y * 2, y * 3]))
    .map(z => z + 10);

console.log(arrayResult.toString()); // Array([12, 13, 14, 16, 16, 19])
