type StrToNum<T> = T extends `${infer N extends number}` ? N : never;
type TupleIndex<T extends readonly unknown[]> = StrToNum<keyof T>;

class HList<T extends readonly unknown[]> {
  private constructor(private readonly values: T) {}

  static empty(): HList<[]> {
    return new HList([]);
  }

  add<A>(value: A): HList<[...T, A]> {
    return new HList([...this.values, value]);
  }

  get<I extends TupleIndex<T>>(index: I): T[I] {
    return this.values[index];
  }
}

const list = HList.empty()
  .add("hello")
  .add(42)
  .add(true)
  .add({ x: 10, y: 20 });

const listText: string = list.get(0);
const listNumber: number = list.get(1);
const listFlag: boolean = list.get(2);

if (false) {
  // @ts-expect-error Index 4 is outside the tuple.
  list.get(4);
}

class HArray<T extends unknown[]> {
  private constructor(private readonly values: T) {}

  static empty(): HArray<[]> {
    return new HArray([]);
  }

  add<A>(value: A): HArray<[...T, A]> {
    return new HArray([...this.values, value]);
  }

  get<I extends TupleIndex<T>>(index: I): T[I] {
    return this.values[index];
  }

  set<I extends TupleIndex<T>>(index: I, value: T[I]): void {
    this.values[index] = value;
  }

  build(): T {
    return [...this.values] as T;
  }
}

const array = HArray.empty()
  .add("hello")
  .add(42)
  .add(true);

array.set(0, "world");
array.set(1, 100);
array.set(2, false);

if (false) {
  // @ts-expect-error Position 0 contains a string.
  array.set(0, 123);

  // @ts-expect-error Position 1 contains a number.
  array.set(1, "hello");
}

const tuple = array.build();
const tupleText: string = tuple[0];
const tupleNumber: number = tuple[1];
const tupleFlag: boolean = tuple[2];

function add<T extends unknown[], A>(xs: T, value: A): [...T, A] {
  return [...xs, value];
}

const built = add(add(add([] as [], "hello"), 42), true);
built[0] = "world";
built[1] = 100;

if (false) {
  // @ts-expect-error Position 0 contains a string.
  built[0] = 100;
}

console.log({
  list: [listText, listNumber, listFlag],
  array: [array.get(0), array.get(1), array.get(2)],
  tuple: [tupleText, tupleNumber, tupleFlag],
  built,
});
