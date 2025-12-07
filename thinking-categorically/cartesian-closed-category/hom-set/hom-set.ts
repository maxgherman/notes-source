// The Hom-set Hom(A, B) corresponds to function type (A) => B
type Hom<A, B> = (a: A) => B;

// In a CCC: Hom(A, B) ≅ B^A (exponential object)
type Exponential<A, B> = Hom<A, B>;  // They're the same!

// The currying isomorphism: Hom(A × B, C) ≅ Hom(A, Hom(B, C))
type Product<A, B> = [A, B];
type CurryingIsomorphism<A, B, C> = {
  left: Hom<Product<A, B>, C>;      // Functions from pairs
  right: Hom<A, Hom<B, C>>;         // Curried functions
};

// Demonstrating the isomorphism
const curryingDemo = () => {
  // Left side: Hom([number, number], number)
  const multiply: Hom<Product<number, number>, number> =
    ([x, y]) => x * y;

  // Right side: Hom(number, Hom(number, number))
  const curryMultiply: Hom<number, Hom<number, number>> =
    (x) => (y) => x * y;

  // The isomorphism in action
  console.log(multiply([6, 7]));      // 42
  console.log(curryMultiply(6)(7));   // 42

  // curry/uncurry witness the isomorphism
  const curry = <A, B, C>(f: Hom<Product<A, B>, C>): Hom<A, Hom<B, C>> =>
    (a) => (b) => f([a, b]);

  const uncurry = <A, B, C>(f: Hom<A, Hom<B, C>>): Hom<Product<A, B>, C> =>
    ([a, b]) => f(a)(b);

  const curried = curry(multiply);
  const uncurried = uncurry(curryMultiply);

  console.log(curried(6)(7));        // 42
  console.log(uncurried([6, 7]));    // 42
};

// Higher-order functions manipulate Hom-sets
const mapHom = <A, B, C>(
  f: Hom<B, C>,
  functions: Hom<A, B>[]
): Hom<A, C>[] =>
  functions.map(g => (x: A) => f(g(x)));  // Function composition

// Example: Working with collections of functions
const functionManipulation = () => {
  // A collection of functions (elements of Hom(number, number))
  const numberFunctions: Hom<number, number>[] = [
    x => x + 1,
    x => x * 2,
    x => x * x
  ];

  // Transform them with another function (Hom(number, string))
  const toString: Hom<number, string> = x => `Result: ${x}`;

  // Get new functions (elements of Hom(number, string))
  const stringFunctions = mapHom(toString, numberFunctions);

  // Test the transformed functions
  stringFunctions.forEach((f, i) => {
    console.log(`Function ${i}(5) = ${f(5)}`);
    // "Function 0(5) = Result: 6"
    // "Function 1(5) = Result: 10"
    // "Function 2(5) = Result: 25"
  });
};

// Exponential objects as function spaces
const exponentialObjects = () => {
  // String^number represents all functions from number to string
  const stringFromNumber: Hom<number, string>[] = [
    x => x.toString(),
    x => `The number ${x}`,
    x => "*".repeat(x),
    x => x < 0 ? "negative" : x > 0 ? "positive" : "zero"
  ];

  // Functions are first-class - can be stored, passed, returned
  const selectFunction = (index: number): Hom<number, string> =>
    stringFromNumber[index] || (x => `Unknown: ${x}`);

  const selectedFunc = selectFunction(2);
  console.log(selectedFunc(5)); // "*****"

  // Function composition creates new exponential objects
  const addPrefix: Hom<string, string> = s => `Output: ${s}`;
  const composedFunctions = stringFromNumber.map(f =>
    (x: number) => addPrefix(f(x))
  );

  console.log(composedFunctions[1](42)); // "Output: The number 42"
};


// Demonstrating the category structure of Hom-sets
const homCategory = () => {
  // Objects: Types (A, B, C, ...)
  // Morphisms: Natural transformations between Hom-sets

  // Identity natural transformation: Hom(A,B) -> Hom(A,B)
  const idHom = <A, B>(f: Hom<A, B>): Hom<A, B> => f;

  // Example natural transformation: pre-composition
  // Given g: A -> B, get natural transformation Hom(B,C) -> Hom(A,C)
  const preComposition = <A, B, C>(g: Hom<A, B>) =>
    (f: Hom<B, C>): Hom<A, C> => (x: A) => f(g(x));

  // Example usage
  const double: Hom<number, number> = x => x * 2;
  const toString: Hom<number, string> = x => `Value: ${x}`;
  const preComposeWithDouble = preComposition(double);
  const composedFunction = preComposeWithDouble(toString);
  console.log(`Pre-composed function(5) = ${composedFunction(5)}`); // "Value: 10"

  console.log("Hom-sets form a category with natural transformations");
};

// The fundamental insight: Hom(A,B) ≅ B^A
const fundamentalTheorem = () => {
  console.log("=== Fundamental Theorem: Hom(A,B) ≅ B^A ===");
  console.log("In TypeScript (modeling a Cartesian Closed Category):");
  console.log("- Types are objects");
  console.log("- Function types (A) => B are morphisms in Hom(A,B)");
  console.log("- Function types (A) => B are also objects (exponentials B^A)");
  console.log("- This duality enables functional programming patterns\n");

  // Demonstrate that function types are both morphisms AND objects
  const functionAsMorphism: Hom<number, string> = x => x.toString();  // Morphism in Hom(number, string)

  // The same function type as objects that can be manipulated
  const functionsAsObjects: Hom<number, string>[] = [
    functionAsMorphism,  // Our function is now data!
    x => `Number: ${x}`,
    x => "*".repeat(Math.max(0, x)),
    x => x < 0 ? "negative" : x > 0 ? "positive" : "zero"
  ];

  console.log("Functions as objects - stored in an array:");
  functionsAsObjects.forEach((func, index) => {
    console.log(`  Function ${index}(3) = "${func(3)}"`);
  });

  // Higher-order function that takes functions as arguments (objects)
  // and returns a function (object) - this is only possible because
  // Hom(A,B) ≅ B^A makes functions first-class
  const testFunction: Hom<Hom<number, string>, string> = f => f(42);

  console.log("\nHigher-order function applying each function to 42:");
  functionsAsObjects.forEach((func, index) => {
    console.log(`  Testing function ${index}: "${testFunction(func)}"`);
  });

  // Functions can be returned from other functions (function factories)
  const createFormatter = (prefix: string): Hom<number, string> =>
    x => `${prefix}: ${x}`;

  const errorFormatter = createFormatter("ERROR");
  const infoFormatter = createFormatter("INFO");

  console.log("\nFunction factories creating new functions:");
  console.log(`  ${errorFormatter(404)}`);  // "ERROR: 404"
  console.log(`  ${infoFormatter(200)}`);   // "INFO: 200"

  // The key insight: functions are simultaneously structure and data
  console.log("\n🔑 Key Insight:");
  console.log("Functions are simultaneously:");
  console.log("- MORPHISMS: They represent computational relationships");
  console.log("- OBJECTS: They can be manipulated as first-class data");
  console.log("This dual nature is what makes functional programming powerful!");
};

curryingDemo();
functionManipulation();
exponentialObjects();
homCategory();
fundamentalTheorem();
