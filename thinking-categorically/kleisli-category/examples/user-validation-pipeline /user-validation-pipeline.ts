// Either monad implementation
interface Either<L, R> {
    flatMap<B>(f: (r: R) => Either<L, B>): Either<L, B>;
    map<B>(f: (r: R) => B): Either<L, B>;
}

class Left<L, R> implements Either<L, R> {
    constructor(private value: L) { }

    flatMap<B>(f: (r: R) => Either<L, B>): Either<L, B> {
        return new Left<L, B>(this.value);
    }

    map<B>(f: (r: R) => B): Either<L, B> {
        return new Left<L, B>(this.value);
    }
}

class Right<L, R> implements Either<L, R> {
    constructor(private value: R) { }

    flatMap<B>(f: (r: R) => Either<L, B>): Either<L, B> {
        return f(this.value);
    }

    map<B>(f: (r: R) => B): Either<L, B> {
        return new Right<L, B>(f(this.value));
    }
}

// Domain types
interface User {
    name: string;
    age: number;
    email: string;
}

type ValidationError = 'EmptyName' | 'InvalidAge' | 'InvalidEmail';

// Kleisli arrows for validation
const validateName = (name: string): Either<ValidationError, string> =>
    name === '' ? new Left('EmptyName') : new Right(name);

const validateAge = (ageStr: string): Either<ValidationError, number> => {
    const age = Number(ageStr);
    return (!Number.isInteger(age) || age < 0 || age > 150)
        ? new Left('InvalidAge')
        : new Right(age);
};

const validateEmail = (email: string): Either<ValidationError, string> =>
    email.includes('@') ? new Right(email) : new Left('InvalidEmail');

// Kleisli composition for user creation
const createUser = (name: string, ageStr: string, email: string): Either<ValidationError, User> =>
    validateName(name)
        .flatMap(validName =>
            validateAge(ageStr)
                .flatMap(validAge =>
                    validateEmail(email)
                        .map(validEmail => ({
                            name: validName,
                            age: validAge,
                            email: validEmail
                        }))
                )
        );

// Usage
console.log(createUser("Alice", "25", "alice@example.com")); // Right(User)
console.log(createUser("", "25", "alice@example.com"));     // Left('EmptyName')
