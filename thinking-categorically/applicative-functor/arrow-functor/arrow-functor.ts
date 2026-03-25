type Arrow<A, B> = (a: A) => B;
type F<R, A> = Arrow<R, A>;

const f: Arrow<number, string> = x => `${x} + 2`;   // A -> B
const g: Arrow<number, number> = x => x + 1;        // R -> A

const composed: Arrow<number, string> = (r: number) => f(g(r)); // R -> B

const fmap =
  <R>() =>
  <A, B>(f: Arrow<A, B>) =>
  (g: Arrow<R, A>): Arrow<R, B> =>
    (r: R) => f(g(r));

const add1: Arrow<number, number> = x => x + 1;
const add2: Arrow<number, string> = x => `${x} + 2`;

const result = fmap<number>()(add2)(add1);

console.log(result(5)); // '6 + 2'
