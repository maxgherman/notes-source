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

// ID generator state
interface IdGenState {
    nextId: number;
}

// Generate a single unique ID
function generateId(): State<IdGenState, number> {
    return State.get<IdGenState>()
        .flatMap(state => {
            const currentId = state.nextId;
            return State.put<IdGenState>({ nextId: currentId + 1 })
                .map(() => currentId);
        });
}

// Generate multiple IDs
function generateIds(count: number): State<IdGenState, number[]> {
    if (count <= 0) {
        return State.pure<IdGenState, number[]>([]);
    }

    return generateId()
        .flatMap(id =>
            generateIds(count - 1)
                .map(restIds => [id, ...restIds])
        );
}

// Usage example
const initialGen: IdGenState = { nextId: 1 };
const [ids, finalGen] = generateIds(5).run(initialGen);
console.log(`Generated IDs: ${JSON.stringify(ids)}`);
console.log(`Final generator state:`, finalGen);
// Output: Generated IDs: [1,2,3,4,5], Final generator state: { nextId: 6 }
