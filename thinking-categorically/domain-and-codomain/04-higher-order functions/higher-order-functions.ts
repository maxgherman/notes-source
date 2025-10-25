// Higher-order functions are everywhere in JavaScript
const numbers: number[] = [1, 2, 3, 4, 5];

// map: (a -> b) -> Array<a> -> Array<b>
const doubled: number[] = numbers.map(x => x * 2);

// filter: (a -> boolean) -> Array<a> -> Array<a>
const evens: number[] = numbers.filter(x => x % 2 === 0);

// reduce: (accumulator -> a -> accumulator) -> accumulator -> Array<a> -> accumulator
const sum: number = numbers.reduce((acc, x) => acc + x, 0);

// Function factory - returns customized functions
function createMultiplier(factor: number): (x: number) => number {
  return (x: number) => x * factor;
}

const triple = createMultiplier(3);
const result = triple(4); // 12

console.log(result);
