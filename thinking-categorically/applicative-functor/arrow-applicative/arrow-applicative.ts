class FArrow<R, A> {
  constructor(public readonly run: (r: R) => A) {}

  static of<R, A>(a: A): FArrow<R, A> {
    return new FArrow(() => a);
  }

  map<B>(f: (a: A) => B): FArrow<R, B> {
    return new FArrow((r: R) => f(this.run(r)));
  }

  ap<B>(fab: FArrow<R, (a: A) => B>): FArrow<R, B> {
    return new FArrow((r: R) => {
      const func = fab.run(r); // function (a => b)
      const val = this.run(r); // a
      return func(val);        // b
    });
  }
}

const fa = new FArrow((env: number) => env + 1);
const ff = new FArrow((env: number) => (x: number) => x * env);

const result = fa.ap(ff);
console.log(result.run(3)); // (3 + 1) * 3 = 12
