// Natural transformation from Array<Array<A>> to Array<A>
const flatten = <A>(nested: Array<Array<A>>): Array<A> =>
    nested.reduce((acc, arr) => acc.concat(arr), [] as Array<A>);

// Verification of naturality:
const f3 = (x: number) => x * 2;
const nested = [[1, 2], [3, 4]];
const left3 = flatten(nested).map(f3);
const right3 = flatten(nested.map(arr => arr.map(f3)));

console.log(`Flatten naturality: ${JSON.stringify(left3) === JSON.stringify(right3)}`);
console.log(`Left: [${left3.join(', ')}]`);   // [2, 4, 6, 8]
console.log(`Right: [${right3.join(', ')}]`); // [2, 4, 6, 8]