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

// Application configuration interface
interface AppConfig {
    apiUrl: string;
    timeout: number;
    features: Set<string>;
}

// Enable a feature if not already enabled
function enableFeature(feature: string): State<AppConfig, boolean> {
    return State.get<AppConfig>()
        .flatMap(config => {
            if (config.features.has(feature)) {
                return State.pure(false); // Already enabled
            } else {
                const newFeatures = new Set(config.features);
                newFeatures.add(feature);
                return State.put<AppConfig>({
                    ...config,
                    features: newFeatures
                }).map(() => true);
            }
        });
}

// Configure multiple features
function configureApp(): State<AppConfig, string> {
    return enableFeature("darkMode")
        .flatMap(darkMode =>
            enableFeature("notifications")
                .flatMap(notifications =>
                    enableFeature("analytics")
                        .map(analytics =>
                            `Configured: darkMode=${darkMode}, notifications=${notifications}, analytics=${analytics}`
                        )
                )
        );
}

// Usage example
const initialConfig: AppConfig = {
    apiUrl: "https://api.example.com",
    timeout: 5000,
    features: new Set<string>()
};

const [result, finalConfig] = configureApp().run(initialConfig);
console.log(result);
console.log("Final config:", {
    ...finalConfig,
    features: Array.from(finalConfig.features)
});
// Output shows which features were newly enabled
