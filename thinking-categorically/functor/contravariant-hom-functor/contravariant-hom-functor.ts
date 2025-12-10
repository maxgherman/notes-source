// Contravariant Hom functor: (- -> B) for fixed B
interface Contravariant<A> {
  contramap<B>(f: (b: B) => A): Contravariant<B>;
}

// Predicate contravariant functor: (A -> boolean)
class Predicate<A> implements Contravariant<A> {
  constructor(private predicate: (a: A) => boolean) {}

  test(value: A): boolean {
    return this.predicate(value);
  }

  contramap<B>(f: (b: B) => A): Predicate<B> {
    return new Predicate<B>((b: B) => this.predicate(f(b)));
  }
}

// Examples with target type boolean
const isPositive = new Predicate<number>(x => x > 0);
const isLongString = new Predicate<string>(s => s.length > 5);

// Contravariant mapping: transform number -> boolean into string -> boolean
// by first converting string to number
const stringToPositive = isPositive.contramap((s: string) => parseInt(s));

console.log(isPositive.test(5));           // true
console.log(isPositive.test(-3));          // false
console.log(stringToPositive.test("10"));  // true
console.log(stringToPositive.test("-5"));  // false

// Another example: Person -> boolean via Person -> number
interface Person { age: number; name: string; }
const isAdult = new Predicate<number>(age => age >= 18);
const personIsAdult = isAdult.contramap((p: Person) => p.age);

const alice: Person = { age: 25, name: "Alice" };
const bob: Person = { age: 16, name: "Bob" };

console.log(personIsAdult.test(alice)); // true
console.log(personIsAdult.test(bob));   // false
