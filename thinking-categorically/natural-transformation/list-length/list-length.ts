// The Array functor (built into JavaScript/TypeScript)
// Array.map :: (a -> b) -> Array<a> -> Array<b>

// Natural transformation from Array to number
const arrayLength = <A>(arr: Array<A>): number => arr.length;

// Verification of naturality:
// For any function f: A -> B and array arr: Array<A>:
// arrayLength(arr.map(f)) === arrayLength(arr)

// Example usage:
const f = (x: number) => x * 2;
const arr = [1, 2, 3];
const natural = arrayLength(arr.map(f)) === arrayLength(arr); // true

console.log(`Naturality check: ${natural}`);