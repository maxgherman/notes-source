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

// Parser state containing input and position
interface ParseState {
    input: string;
    position: number;
}

// Parse a specific character
function parseChar(expected: string): State<ParseState, string | null> {
    return State.get<ParseState>()
        .flatMap(state => {
            const remaining = state.input.slice(state.position);
            if (remaining.length > 0 && remaining[0] === expected) {
                return State.put<ParseState>({
                    input: state.input,
                    position: state.position + 1
                }).map(() => expected);
            } else {
                return State.pure<ParseState, string | null>(null);
            }
        });
}

// Parse a string by parsing each character
function parseString(str: string): State<ParseState, string | null> {
    if (str.length === 0) {
        return State.pure<ParseState, string | null>(str);
    }

    const [head, ...tail] = str;
    return parseChar(head)
        .flatMap(charResult => {
            if (charResult === null) {
                return State.pure<ParseState, string | null>(null);
            }
            return parseString(tail.join(''))
                .map(tailResult => tailResult === null ? null : str);
        });
}

// Parse a number (simplified)
function parseNumber(): State<ParseState, number | null> {
    return State.get<ParseState>()
        .flatMap(state => {
            const remaining = state.input.slice(state.position);
            const digits = remaining.match(/^\d+/);

            if (digits === null) {
                return State.pure<ParseState, number | null>(null);
            }

            const digitStr = digits[0];
            return State.put<ParseState>({
                input: state.input,
                position: state.position + digitStr.length
            }).map(() => parseInt(digitStr, 10));
        });
}

// Usage example
const initialState: ParseState = { input: "hello123world", position: 0 };

const parser = parseString("hello")
    .flatMap(result1 =>
        parseNumber()
            .flatMap(result2 =>
                parseString("world")
                    .map(result3 => ({
                        hello: result1,
                        number: result2,
                        world: result3
                    }))
            )
    );

const [results, finalState] = parser.run(initialState);
console.log("Parse results:", results);
console.log("Final state:", finalState);
// Output: Parse results: { hello: "hello", number: 123, world: "world" }
