// State monad implementation
class State<S, A> {
    constructor(private runState: (state: S) => [A, S]) {}

    // Run the state computation
    run(initialState: S): [A, S] {
        return this.runState(initialState);
    }

    // Extract just the result, discarding final state
    eval(initialState: S): A {
        return this.run(initialState)[0];
    }

    // Extract just the final state, discarding result
    exec(initialState: S): S {
        return this.run(initialState)[1];
    }

    // Functor: map over the result value
    map<B>(f: (a: A) => B): State<S, B> {
        return new State(s => {
            const [a, newS] = this.runState(s);
            return [f(a), newS];
        });
    }

    // Monad: bind/flatMap
    flatMap<B>(f: (a: A) => State<S, B>): State<S, B> {
        return new State(s => {
            const [a, newS] = this.runState(s);
            return f(a).run(newS);
        });
    }

    // Utility for chaining with ignored results
    then<B>(next: State<S, B>): State<S, B> {
        return this.flatMap(_ => next);
    }

    // Static constructor for pure values
    static pure<S, A>(value: A): State<S, A> {
        return new State(s => [value, s]);
    }

    // Static constructor for state operations
    static get<S>(): State<S, S> {
        return new State(s => [s, s]);
    }

    static put<S>(newState: S): State<S, void> {
        return new State(_ => [undefined, newState]);
    }

    static modify<S>(f: (s: S) => S): State<S, void> {
        return new State(s => [undefined, f(s)]);
    }
}

// Game state interface
interface GameState {
    health: number;
    score: number;
    inventory: string[];
    position: { x: number, y: number };
}

const initialGameState: GameState = {
    health: 100,
    score: 0,
    inventory: [],
    position: { x: 0, y: 0 }
};

// Game operations using State monad
function collectItem(item: string): State<GameState, string> {
    return State.get<GameState>()
        .flatMap(state =>
            State.put<GameState>({
                ...state,
                inventory: [...state.inventory, item],
                score: state.score + 10
            })
        )
        .map(() => `Collected ${item}`);
}

function takeDamage(damage: number): State<GameState, string> {
    return State.get<GameState>()
        .flatMap(state => {
            const newHealth = Math.max(0, state.health - damage);
            return State.put<GameState>({ ...state, health: newHealth })
                .map(() => newHealth > 0 ? "Still alive" : "Game over");
        });
}

function movePlayer(dx: number, dy: number): State<GameState, string> {
    return State.get<GameState>()
        .flatMap(state => {
            const newPosition = {
                x: state.position.x + dx,
                y: state.position.y + dy
            };
            return State.put<GameState>({ ...state, position: newPosition })
                .map(() => `Moved to (${newPosition.x}, ${newPosition.y})`);
        });
}

// Monadic game sequence
function playGame(): State<GameState, string[]> {
    return collectItem("sword")
        .flatMap(msg1 =>
            takeDamage(20)
                .flatMap(msg2 =>
                    movePlayer(5, 3)
                        .map(msg3 => [msg1, msg2, msg3])
                )
        );
}

// Alternative with async-like syntax using generators
function* playGameGenerator(): Generator<State<GameState, string>, string[], any> {
    const msg1 = yield collectItem("sword");
    const msg2 = yield takeDamage(20);
    const msg3 = yield movePlayer(5, 3);
    return [msg1, msg2, msg3];
}

// Run the game
const gameResult = playGame().run(initialGameState);
console.log("Messages:", gameResult[0]);
console.log("Final state:", gameResult[1]);
