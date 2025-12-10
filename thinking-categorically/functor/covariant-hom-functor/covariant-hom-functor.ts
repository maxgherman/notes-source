// Covariant Hom functor: Hom<A, _> for fixed A
type Hom<A, B> = (a: A) => B;

// Hom(number, -) - functions from number to various types
const toStr: Hom<number, string> = (n: number) => n.toString();
const isEven: Hom<number, boolean> = (n: number) => n % 2 === 0;
const double: Hom<number, number> = (n: number) => n * 2;

// The functor action: composition
// fmap :: (b -> c) -> (a -> b) -> (a -> c)
const fmap =
  <A, B, C>(f: (b: B) => C) =>
    (g: (a: A) => B): ((a: A) => C) =>
      (a: A) => f(g(a));

// Example: transform number -> number into number -> string
const doubleToString: Hom<number, string> = fmap<number, number, string>(toStr)(double);

console.log(doubleToString(5)); // "10"

// Another example: number -> number into number -> boolean
const doubleIsEven: Hom<number, boolean> = fmap<number, number, boolean>(isEven)(double);

console.log(doubleIsEven(3)); // true (3*2=6 is even)
console.log(doubleIsEven(2)); // true (2*2=4, which is even)

// Working with arrays of functions (sets of morphisms)
const numberFunctions: Array<Hom<number, number>> = [
  (x: number) => x + 1,
  (x: number) => x * 2,
  (x: number) => x * x
];

// Apply fmap to each function in the set
const stringFunctions: Array<Hom<number, string>> =
  numberFunctions.map((f) => fmap<number, number, string>(toStr)(f));

stringFunctions.forEach((f, i) => {
  console.log(`Function ${i}(3) = "${f(3)}"`);
  // Function 0(3) = "4"
  // Function 1(3) = "6"
  // Function 2(3) = "9"
});
