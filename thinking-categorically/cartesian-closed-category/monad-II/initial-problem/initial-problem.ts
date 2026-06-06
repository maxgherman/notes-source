// Types and interfaces for better type safety
interface User {
    id: number;
    name: string;
}

interface UserPermissions {
    canAccess: boolean;
}

interface ProcessedData {
    processed: boolean;
    data: any;
}

interface SavedData {
    saved: boolean;
    data: any;
}

interface SuccessResult {
    success: SavedData;
}

interface ErrorResult {
    error: string;
}

type Result = SuccessResult | ErrorResult;

const database = {
    getUser: (userId: number): User | null => {
        // Simulate a user fetch
        return userId > 0 ? { id: userId, name: "John Doe" } : null;
    }
};

const auth = {
    checkPermissions: (user: User): UserPermissions | null => {
        // Simulate permission check
        return user ? { canAccess: true } : null;
    }
};

const validator = {
    validate: (data: any): any | null => {
        // Simulate data validation
        return data && typeof data === 'object' ? data : null;
    }
};

const processor = {
    process: (data: any): ProcessedData | null => {
        // Simulate data processing
        return data ? { processed: true, data } : null;
    }
};

const storage = {
    save: (data: ProcessedData): SavedData | null => {
        // Simulate data storage
        return data ? { saved: true, data } : null;
    }
};

// Example of the monadic approach (as mentioned in the introduction)
class Maybe<T> {
    constructor(private value: T | null) {}

    static of<T>(value: T | null): Maybe<T> {
        return new Maybe(value);
    }

    flatMap<U>(fn: (value: T) => Maybe<U>): Maybe<U> {
        if (this.value === null) {
            return new Maybe<U>(null);
        }
        return fn(this.value);
    }

    map<U>(fn: (value: T) => U): Maybe<U> {
        if (this.value === null) {
            return new Maybe<U>(null);
        }
        return Maybe.of(fn(this.value));
    }

    getValue(): T | null {
        return this.value;
    }

    isNothing(): boolean {
        return this.value === null;
    }
}

// Monadic version of the same function
function processUserRequestMonadic(userId: number, requestData: any): Maybe<SavedData> {
    return Maybe.of(database.getUser(userId))
        .flatMap(user => Maybe.of(auth.checkPermissions(user)))
        .flatMap(permissions => Maybe.of(validator.validate(requestData)))
        .flatMap(validatedData => Maybe.of(processor.process(validatedData)))
        .flatMap(result => Maybe.of(storage.save(result)));
}

// Usage of monadic version
const monadicResult = processUserRequestMonadic(1, { key: "value" });
if (monadicResult.isNothing()) {
    console.log("Operation failed at some step");
} else {
    console.log("Success:", monadicResult.getValue());
}
