// Terminal Object: void/undefined (unit type)
type Terminal = void;
const terminal: Terminal = undefined;

// Currying: ((A × B) => C) → (A => (B => C))
const curry = <A, B, C>(f: (pair: Product<A, B>) => C): (x: A) => (y: B) => C =>
  (x: A) => (y: B) => f([x, y]);

// Uncurrying: (A => (B => C)) → ((A × B) => C)
const uncurry = <A, B, C>(f: (x: A) => (y: B) => C): (pair: Product<A, B>) => C =>
  ([x, y]: Product<A, B>) => f(x)(y);

// Every type has exactly one function to terminal
const toTerminal = <A>(x: A): Terminal => undefined;

// Binary Products: Tuple types [A, B]
type Product<A, B> = [A, B];

// Product construction and projections
const makePair = <A, B>(x: A, y: B): Product<A, B> => [x, y];

const fst = <A, B>([x, _]: Product<A, B>): A => x;
const snd = <A, B>([_, y]: Product<A, B>): B => y;

// Universal property: pairing morphism
const pairing = <X, A, B>(
  f: (x: X) => A,
  g: (x: X) => B
): (x: X) => Product<A, B> =>
  (x: X) => [f(x), g(x)];

// Exponential Objects: Function types (A) => B
type Exponential<A, B> = (x: A) => B;

// Evaluation morphism
const evalMorphism = <A, B>(func: Exponential<A, B>, arg: A): B => func(arg);

// Alternative eval that takes a product
const evalProduct = <A, B>([func, arg]: Product<Exponential<A, B>, A>): B =>
  func(arg);

// Example 1: Terminal object and product projections
function example1() {
  const unitResult: Terminal = terminal;               // unique inhabitant
  const dropped: Terminal = toTerminal("anything");   // morphism to 1
  const pairAB = makePair<number, string>(42, "answer");
  const firstOfPair = fst(pairAB);                     // π₁
  const secondOfPair = snd(pairAB);                    // π₂
  console.log("Example 1 (terminal, products):", { unitResult, dropped, pairAB, firstOfPair, secondOfPair });
}

// Example 2: Universal property of products (pairing) + evaluation
function example2() {
  const build = pairing((x: number) => x + 1, (x: number) => x * 2); // ⟨f,g⟩
  const from5 = build(5); // [6,10]
  const incr: Exponential<number, number> = x => x + 1;
  const evaluated = evalMorphism(incr, 10);                 // eval(f, a)
  const evaluatedViaProduct = evalProduct([incr, 10]);      // eval ∘ pairing
  console.log("Example 2 (pairing, eval):", { from5, evaluated, evaluatedViaProduct });
}

// Example 3: Currying/Uncurrying isomorphism (exponential structure)
function example3() {
  const addPair = ([a,b]: Product<number, number>) => a + b;          // (A × B) → C
  const curried = curry(addPair);                                     // A → (B → C)
  const uncurried = uncurry(curried);                                 // round trip
  const roundTrip = uncurried([3,4]);                                 // 7
  // Partial application exhibits internal hom
  const add10 = curried(10);                                          // B → C
  const fifteen = add10(5);                                           // 15
  console.log("Example 3 (curry/uncurry):", { roundTrip, fifteen });
}

// Example 4: Higher-order functions and composition (exponential structure)
const compose = <A, B, C>(
  f: Exponential<B, C>,
  g: Exponential<A, B>
): Exponential<A, C> =>
  (x: A) => f(g(x));

function example4() {
  const processNumber: Exponential<number, string> = compose(
    compose(String, (x: number) => x * 2), // (*2) then to string
    (x: number) => x + 1                   // (+1) first
  );
  const result = processNumber(5); // "12"
  console.log("Example 4 (composition / pipeline):", { result });
}

// Example 5: Familiar higher-order operations as exponentials
function example5() {
  const numbers = [1, 2, 3, 4, 5];
  const doubled = numbers.map((x: number) => x * 2);        // map: functorial action
  const evens = numbers.filter((x: number) => x % 2 === 0); // predicate as exponential
  const sum = numbers.reduce((acc: number, x: number) => acc + x, 0); // fold uses evaluation repeatedly
  console.log("Example 5 (arrays & exponentials):", { doubled, evens, sum });
}

// Driver to run all examples in order
function runAllExamples() {
  example1();
  example2();
  example3();
  example4();
  example5();
}

runAllExamples();
