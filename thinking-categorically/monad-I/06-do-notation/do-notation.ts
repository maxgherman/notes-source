// Type definition for Maybe
type Maybe<T> = T | null;

// Helper functions
function pure<T>(value: T): Maybe<T> {
    return value;
}

function safeDivide(x: number, y: number): Maybe<number> {
    return y === 0 ? null : x / y;
}

// Option 1: Generator-based do-notation simulation
function runDo<TFrom, T, TNext>(generatorFn: () => Generator<Maybe<TFrom>, T, TNext>): Maybe<T> {
    const generator = generatorFn();
    let current = generator.next();

    while (!current.done) {
        const maybeValue = current.value;
        if (maybeValue === null) {
            return null; // Short-circuit on failure
        }
        current = generator.next(maybeValue as TNext);
    }

    return current.value;
}

// Usage example:
const doExample = runDo(function*() {
    const x: number = yield safeDivide(20, 4);  // Extract value from Maybe
    const y: number = yield safeDivide(x, 2);  // Chain the next operation

    return y * 3; // Final computation
});

console.log("Generator example result:", doExample); // Output: 7.5
