function main() {
  // u, v are arrays of functions; w is a value array
  const u: Array<(x: number) => number> = [x => x + 1];
  const v: Array<(x: number) => number> = [x => x * 2];
  const w: number[] = [10];

  // Function composition: (f ∘ g)(x) = f(g(x))
  const compose = <A, B, C>(f: (b: B) => C, g: (a: A) => B) => (x: A) => f(g(x));

  // Left: pure (.) <*> u <*> v <*> w
  const left = [compose].flatMap(c =>
    u.flatMap(f =>
      v.flatMap(g =>
        w.map(x => c(f, g)(x))
      )
    )
  );

  // Right: u <*> (v <*> w)
  const right = u.flatMap(f =>
    v.flatMap(g =>
      w.map(x => f(g(x)))
    )
  );

  console.log("Left: ", left);   // [21]
  console.log("Right:", right);  // [21]
  console.log("Equal?", arraysEqual(left, right)); // true
}

function arraysEqual<T>(a: T[], b: T[]): boolean {
  return a.length === b.length && a.every((v, i) => v === b[i]);
}

main();
