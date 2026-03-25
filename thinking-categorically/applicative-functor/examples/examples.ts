class F<A> {
  constructor(public readonly value: A) {}

  map<B>(f: (x: A) => B): F<B> {
    return new F(f(this.value));
  }

  ap<B>(fab: F<(x: A) => B>): F<B> {
    return new F(fab.value(this.value));
  }
}

const result = new F<number>(1);

console.log(result.map(x => x + 1).map(x => `${x} + 1`)); // F { value: '2 + 1' }

console.log(result.ap(new F((x:number) => x + 1))) // F { value: 2 }
