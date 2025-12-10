type Functor<T> = {
  map: <U>(f: (x: T) => U) => Functor<U>;
  toString: () => string;
};

const maybe = <T>(value: T | null | undefined): Functor<T> => ({
  map: <U>(f: (x: T) => U): Functor<U> => {
    if (value === null || value === undefined) {
      return nothing as Functor<U>;
    }
    return just(f(value as T));
  },
  toString: () =>
    value === null || value === undefined ? "Nothing" : `Just(${value})`,
});

const just = <T>(value: T): Functor<T> => maybe(value);
const nothing: Functor<never> = maybe<never>(null);

console.log(maybe(5).map(x => x + 1).toString()); // Just(6)
console.log(maybe(null).map(x => x! + 1).toString()); // Nothing
