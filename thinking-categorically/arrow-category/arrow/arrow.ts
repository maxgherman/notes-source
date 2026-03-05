// Define the sets as arrays
const A = [1, 2];
const B = [10, 20];
const C = [3, 4];
const D = [30, 40];

// Define the functions
const f = (a: number) => a === 1 ? 10 : 20;
const g = (c: number) => c === 3 ? 30 : 40;
const s = (a: number) => a === 1 ? 3 : 4;
const t = (b: number) => b === 10 ? 30 : 40;

// Check commutativity for all elements in A
const commutes = A.every(a => t(f(a)) === g(s(a)));

console.log(commutes); // Output: true