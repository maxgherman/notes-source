
type Maybe<A> = A | null

function pureMaybe<A>(value: A): Maybe<A> {
    return value  // In Maybe, this just returns the value
}

function flatMap<A, B>(ma: Maybe<A>, f: (a: A) => Maybe<B>): Maybe<B> {
    if (ma === null) {
        return null           // short-circuit on failure
    }
    return f(ma)             // apply function to unwrapped value
}

// Left Identity

// pure(a).flatMap(f) === f(a)
function leftIdentityExample<A, B>(a: A, f: (x: A) => Maybe<B>): boolean {
    const left = flatMap(pureMaybe(a), f);
    const right = f(a);
    return JSON.stringify(left) === JSON.stringify(right);
}

// Example usage:
const addTen = (x: number) => x !== null ? x + 10 : null;
console.log(leftIdentityExample(5, addTen)); // true


// Right Identity

// ma.flatMap(pure) === ma
function rightIdentityExample<A>(ma: Maybe<A>): boolean {
    const left = flatMap(ma, pureMaybe);
    const right = ma;
    return JSON.stringify(left) === JSON.stringify(right);
}

// Example usage:
console.log(rightIdentityExample(42)); // true
console.log(rightIdentityExample(null)); // true


// Associativity

// flatMap(flatMap(ma, f), g) === flatMap(ma, x => flatMap(f(x), g))
function associativityExample<A, B, C>(
    ma: Maybe<A>,
    f: (x: A) => Maybe<B>,
    g: (y: B) => Maybe<C>
): boolean {
    const left = flatMap(flatMap(ma, f), g);
    const right = flatMap(ma, x => flatMap(f(x), g));
    return JSON.stringify(left) === JSON.stringify(right);
}

// Example usage:
const double = (x: number) => x !== null ? x * 2 : null;
const toString2 = (x: number) => x !== null ? x.toString() : null;
console.log(associativityExample(5, double, toString2)); // true
