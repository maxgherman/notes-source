// Semigroup interface in TypeScript
interface Semigroup<T> {
  combine(a: T, b: T): T;
}

// String concatenation semigroup
const StringSemigroup: Semigroup<string> = {
  combine: (a, b) => a + b
};

// Array concatenation semigroup
const ArraySemigroup = <T>(): Semigroup<T[]> => ({
  combine: (a, b) => [...a, ...b]
});

// Number addition semigroup
const SumSemigroup: Semigroup<number> = {
  combine: (a, b) => a + b
};

// Number multiplication semigroup
const ProductSemigroup: Semigroup<number> = {
  combine: (a, b) => a * b
};

// Generic combine function (associative)
function combineAll<T>(semigroup: Semigroup<T>, items: T[]): T {
  if (items.length === 0) {
    throw new Error("Cannot combine empty array");
  }

  return items.reduce(semigroup.combine);
}

// Examples in action:
console.log(StringSemigroup.combine("Hello", " World")); // "Hello World"
console.log(combineAll(StringSemigroup, ["Hello", " ", "World"])); // "Hello World"
console.log(combineAll(ArraySemigroup<number>(), [[1,2], [3,4], [5,6]])); // [1,2,3,4,5,6]
console.log(combineAll(SumSemigroup, [1, 2, 3, 4])); // 10
console.log(combineAll(ProductSemigroup, [2, 3, 4])); // 24
