// State monad implementation for the accumulator pattern
class State<S, A> {
    constructor(private runState: (state: S) => [A, S]) {}

    run(initialState: S): [A, S] {
        return this.runState(initialState);
    }

    map<B>(f: (a: A) => B): State<S, B> {
        return new State(s => {
            const [a, newS] = this.runState(s);
            return [f(a), newS];
        });
    }

    flatMap<B>(f: (a: A) => State<S, B>): State<S, B> {
        return new State(s => {
            const [a, newS] = this.runState(s);
            return f(a).run(newS);
        });
    }

    then<B>(next: State<S, B>): State<S, B> {
        return this.flatMap(_ => next);
    }

    static pure<S, A>(value: A): State<S, A> {
        return new State(s => [value, s]);
    }

    static get<S>(): State<S, S> {
        return new State(s => [s, s]);
    }

    static put<S>(newState: S): State<S, void> {
        return new State(_ => [undefined, newState]);
    }
}

// Accumulator state type
interface AccumulatorState {
    sum: number;
    count: number;
}

// Sum with count using State monad
function sumWithCount(numbers: number[]): State<AccumulatorState, number> {
    if (numbers.length === 0) {
        return State.get<AccumulatorState>().map(state => state.sum);
    }

    const [head, ...tail] = numbers;
    return State.get<AccumulatorState>()
        .flatMap(state =>
            State.put<AccumulatorState>({
                sum: state.sum + head,
                count: state.count + 1
            })
            .then(sumWithCount(tail))
        );
}

// Usage example
const numbers = [1, 2, 3, 4, 5];
const initialState: AccumulatorState = { sum: 0, count: 0 };
const [result, finalState] = sumWithCount(numbers).run(initialState);
console.log(`Sum: ${result}`);
console.log(`Final state:`, finalState);
// Output: Sum: 15, Final state: { sum: 15, count: 5 }
