// Maybe type definition
type Maybe<T> = T | null;

// Helper functions for Maybe monad
function pure<T>(value: T): Maybe<T> {
    return value;
}

function flatMap<A, B>(ma: Maybe<A>, f: (a: A) => Maybe<B>): Maybe<B> {
    if (ma === null) {
        return null;
    }
    return f(ma);
}

function safeDiv(x: number, y: number): Maybe<number> {
    return y === 0 ? null : x / y;
}

// Fluent builder pattern for monadic computations
class DoBuilder<T> {
    constructor(private value: Maybe<T>) {}

    static from<T>(value: Maybe<T>): DoBuilder<T> {
        return new DoBuilder(value);
    }

    static pure<T>(value: T): DoBuilder<T> {
        return new DoBuilder(pure(value));
    }

    bind<U>(f: (value: T) => Maybe<U>): DoBuilder<U> {
        return new DoBuilder(flatMap(this.value, f));
    }

    map<U>(f: (value: T) => U): DoBuilder<U> {
        return new DoBuilder(
            this.value !== null ? f(this.value) : null
        );
    }

    run(): Maybe<T> {
        return this.value;
    }
}

// Usage examples:
const builderExample1 = DoBuilder
    .pure(20)
    .bind(x => safeDiv(x, 4))
    .bind(y => safeDiv(y, 2))
    .map(z => z * 3)
    .run(); // Result: 7.5

const builderExample2 = DoBuilder
    .pure(20)
    .bind(x => safeDiv(x, 0)) // This will fail
    .bind(y => safeDiv(y, 2))
    .map(z => z * 3)
    .run(); // Result: null

// More complex example with string parsing
function parseNumber(str: string): Maybe<number> {
    const num = parseFloat(str);
    return isNaN(num) ? null : num;
}

const complexCalculation = DoBuilder
    .from(parseNumber("10"))
    .bind(x => DoBuilder.from(parseNumber("2")).run())
    .bind(y => safeDiv(10, y)) // Using the first parsed number
    .map(result => `Result: ${result}`)
    .run(); // Result: "Result: 5"

console.log("Example 1:", builderExample1); // 7.5
console.log("Example 2:", builderExample2); // null
console.log("Complex:", complexCalculation); // "Result: 5"

// Builder pattern with error context
class DoBuilderWithError<T> {
    constructor(
        private value: Maybe<T>,
        private errorContext: string[] = []
    ) {}

    static pure<T>(value: T): DoBuilderWithError<T> {
        return new DoBuilderWithError(pure(value));
    }

    bind<U>(
        f: (value: T) => Maybe<U>,
        errorMsg?: string
    ): DoBuilderWithError<U> {
        if (this.value === null) {
            return new DoBuilderWithError<U>(null, this.errorContext);
        }

        const result = f(this.value);
        const newContext = result === null && errorMsg
            ? [...this.errorContext, errorMsg]
            : this.errorContext;

        return new DoBuilderWithError(result, newContext);
    }

    map<U>(f: (value: T) => U): DoBuilderWithError<U> {
        return new DoBuilderWithError(
            this.value !== null ? f(this.value) : null,
            this.errorContext
        );
    }

    run(): { value: Maybe<T>; errors: string[] } {
        return { value: this.value, errors: this.errorContext };
    }
}

// Usage with error tracking:
const resultWithErrors = DoBuilderWithError
    .pure(10)
    .bind(x => safeDiv(x, 0), "Division by zero in step 1")
    .bind(y => safeDiv(y, 2), "Division failed in step 2")
    .map(z => z * 3)
    .run();

console.log("Result with errors:", resultWithErrors);
// { value: null, errors: ["Division by zero in step 1"] }
