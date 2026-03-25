function main() {
  console.log("=== Applicative Homomorphism Law ===\n");

  const result = listHomomorphismExample();
  console.log(`Array<T> result: ${result.join(", ")}`); // 6
}

// --------------------------------
// 1 Array<T> example
// --------------------------------
function listHomomorphismExample(): number[] {
  const times2 = (x: number): number => x * 2;

  // Left: applying array of functions to array of values
  const left = [times2].flatMap(f => [3].map(f));

  // Right: applying function directly to value, then wrapping in array
  const right = [times2(3)];

  console.log(`Left == Right? ${arraysEqual(left, right)}`); // true
  return left;
}

// Helper to check shallow array equality
function arraysEqual<T>(a: T[], b: T[]): boolean {
  return a.length === b.length && a.every((val, idx) => val === b[idx]);
}

main();
