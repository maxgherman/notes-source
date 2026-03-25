// The Reader type: a function from environment R to value A
type Reader<R, A> = (r: R) => A;

class ReaderApplicative<R, A> {
  constructor(public readonly run: Reader<R, A>) {}

  // Functor: map :: (A -> B) -> Reader<R, A> -> Reader<R, B>
  map<B>(f: (a: A) => B): ReaderApplicative<R, B> {
    return new ReaderApplicative((r: R) => f(this.run(r)));
  }

  // Applicative: ap :: Reader<R, (A -> B)> -> Reader<R, A> -> Reader<R, B>
  ap<B>(rf: ReaderApplicative<R, (a: A) => B>): ReaderApplicative<R, B> {
    return new ReaderApplicative((r: R) => {
      const func = rf.run(r);
      const val = this.run(r);
      return func(val);
    });
  }

  // Applicative: pure :: A -> Reader<R, A>
  static of<R, A>(value: A): ReaderApplicative<R, A> {
    return new ReaderApplicative(() => value);
  }
}

// Environment type
type Env = { multiplier: number };

// A Reader that uses the environment
const readerA = new ReaderApplicative<Env, number>(env => 5 + env.multiplier);

// A Reader that produces a function (Env -> A -> B)
const readerF = new ReaderApplicative<Env, (x: number) => number>(env => x => x * env.multiplier);

// Apply them together
const resultReader = readerA.ap(readerF);

// Run the computation with an environment
console.log(resultReader.run({ multiplier: 3 })); // (5 + 3) * 3 = 8 * 3 = 24
