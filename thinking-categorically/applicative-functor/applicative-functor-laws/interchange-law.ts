function main() {
  // List of functions
  const u: Array<(x: number) => number> = [
    x => x + 1,
    x => x * 2
  ];

  const y = 3;

  // Left side: u <*> pure y
  const left = u.map(f => f(y));

  // Right side: pure (f => f(y)) <*> u
  const right = [(f: (x: number) => number) => f(y)].flatMap(g => u.map(f => g(f)));

  console.log("Left:  ", left);   // [4, 6]
  console.log("Right: ", right);  // [4, 6]
  console.log("Equal? ", arraysEqual(left, right)); // true
}

function arraysEqual<T>(a: T[], b: T[]): boolean {
  return a.length === b.length && a.every((v, i) => v === b[i]);
}

main();
