// Maybe monad implementation
interface Maybe<A> {
    flatMap<B>(f: (a: A) => Maybe<B>): Maybe<B>;
    map<B>(f: (a: A) => B): Maybe<B>;
}

// Kleisli composition function
const kleisliCompose = <A, B, C>(
    f: (a: A) => Maybe<B>,
    g: (b: B) => Maybe<C>
): (a: A) => Maybe<C> => {
    return (a: A) => f(a).flatMap(g);
};

class Some<A> implements Maybe<A> {
    constructor(private value: A) { }

    flatMap<B>(f: (a: A) => Maybe<B>): Maybe<B> {
        return f(this.value);
    }

    map<B>(f: (a: A) => B): Maybe<B> {
        return new Some(f(this.value));
    }
}

class None<A> implements Maybe<A> {
    flatMap<B>(f: (a: A) => Maybe<B>): Maybe<B> {
        return new None<B>();
    }

    map<B>(f: (a: A) => B): Maybe<B> {
        return new None<B>();
    }
}

// Kleisli arrows for safe division
const safeDivide = (x: number, y: number): Maybe<number> =>
    y === 0 ? new None<number>() : new Some(x / y);

const divideBy2 = (x: number): Maybe<number> => safeDivide(x, 2);
const reciprocal = (x: number): Maybe<number> => safeDivide(1, x);
const divideBy4 = (x: number): Maybe<number> => safeDivide(x, 4);

// Kleisli composition
const complexOperation = (x: number): Maybe<number> =>
    kleisliCompose(
        kleisliCompose(divideBy2, reciprocal),
        divideBy4
    )(x);

// Usage
console.log(complexOperation(2));  // Some(0.25)
console.log(complexOperation(0));  // None
