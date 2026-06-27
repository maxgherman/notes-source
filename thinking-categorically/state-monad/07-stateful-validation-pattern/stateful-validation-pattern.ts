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

// Validation state containing errors and context
interface ValidationState {
    errors: string[];
    context: Map<string, string>;
}

// Validate that a field is required (not empty)
function validateRequired(field: string, value: string): State<ValidationState, boolean> {
    return State.get<ValidationState>()
        .flatMap(state => {
            if (!value || value.trim() === '') {
                const newState: ValidationState = {
                    errors: [...state.errors, `${field} is required`],
                    context: new Map(state.context)
                };
                return State.put<ValidationState>(newState).map(() => false);
            } else {
                const newState: ValidationState = {
                    errors: [...state.errors],
                    context: new Map(state.context).set(field, value)
                };
                return State.put<ValidationState>(newState).map(() => true);
            }
        });
}

// Validate email format
function validateEmail(email: string): State<ValidationState, boolean> {
    return State.get<ValidationState>()
        .flatMap(state => {
            const isValid = email.includes('@') && email.includes('.');
            if (!isValid) {
                const newState: ValidationState = {
                    errors: [...state.errors, 'Invalid email format'],
                    context: new Map(state.context)
                };
                return State.put<ValidationState>(newState).map(() => false);
            }
            return State.pure(true);
        });
}

// Validate a complete user
function validateUser(name: string, email: string): State<ValidationState, boolean> {
    return validateRequired("name", name)
        .flatMap(nameValid =>
            validateRequired("email", email)
                .flatMap(emailValid => {
                    if (emailValid) {
                        return validateEmail(email)
                            .map(emailFormatValid => nameValid && emailValid && emailFormatValid);
                    } else {
                        return State.pure(nameValid && emailValid);
                    }
                })
        );
}

// Usage example
const initialState: ValidationState = {
    errors: [],
    context: new Map()
};

const [isValid, finalState] = validateUser("John", "john@example.com").run(initialState);
console.log(`Is valid: ${isValid}`);
console.log(`Errors: ${JSON.stringify(finalState.errors)}`);
console.log(`Context:`, Array.from(finalState.context.entries()));
