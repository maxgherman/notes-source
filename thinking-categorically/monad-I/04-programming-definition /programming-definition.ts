// TypeScript: Generic types that add context
type Maybe<A> = A | null

function pureMaybe<A>(value: A): Maybe<A> {
    return value  // In Maybe, this just returns the value
}

// For Promise:
function purePromise<A>(value: A): Promise<A> {
    return Promise.resolve(value)
}

// TypeScript: flatMap/bind operation
function flatMap<A, B>(ma: Maybe<A>, f: (a: A) => Maybe<B>): Maybe<B> {
    if (ma === null) {
        return null           // short-circuit on failure
    }
    return f(ma)             // apply function to unwrapped value
}

// Helper function that might fail
function safeDivide(x: number, y: number): number | null {
    return y === 0 ? null : x / y;
}

// Chaining operations with flatMap
const computation =
    flatMap(pureMaybe(20), x =>
        flatMap(safeDivide(x, 4), y =>
            safeDivide(y, 2)));

// More readable with helper functions
function divideBy(divisor: number): (value: number) => number | null {
    return (value: number) => safeDivide(value, divisor);
}

const result =
    flatMap(flatMap(pureMaybe(20), divideBy(4)), divideBy(2));

// Result: 2.5

// Without monads, we'd need nested null checks:
// const step1 = safeDivide(20, 4);
// if (step1 !== null) {
//     const step2 = safeDivide(step1, 2);
//     if (step2 !== null) {
//         return step2;
//     }
// }
// return null;

