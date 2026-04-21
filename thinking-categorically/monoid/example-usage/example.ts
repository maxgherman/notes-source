// Monoid interface
interface Monoid<T> {
  empty: T;
  combine: (a: T, b: T) => T;
}

// String concatenation monoid
const StringMonoid: Monoid<string> = {
  empty: "",
  combine: (a, b) => a + b
};

// Number addition monoid
const NumberAddMonoid: Monoid<number> = {
  empty: 0,
  combine: (a, b) => a + b
};

// Array concatenation monoid
const ArrayMonoid = <T>(): Monoid<T[]> => ({
  empty: [],
  combine: (a, b) => [...a, ...b]
});

// Generic fold function that works with any monoid
function fold<T>(monoid: Monoid<T>, values: T[]): T {
  return values.reduce(monoid.combine, monoid.empty);
}

// Usage examples
const result1 = fold(StringMonoid, ["Hello", " ", "World"]);
// Result: "Hello World"

const result2 = fold(NumberAddMonoid, [1, 2, 3, 4, 5]);
// Result: 15

const result3 = fold(ArrayMonoid<number>(), [[1, 2], [3, 4], [5]]);
// Result: [1, 2, 3, 4, 5]

// Safe operations with empty collections
const emptySum = fold(NumberAddMonoid, []);
// Result: 0 (identity element)

// Custom monoid for shopping cart
interface CartItem {
  quantity: number;
  price: number;
}

const CartMonoid: Monoid<CartItem> = {
  empty: { quantity: 0, price: 0 },
  combine: (a, b) => ({
    quantity: a.quantity + b.quantity,
    price: a.price + b.price
  })
};

const cart = fold(CartMonoid, [
  { quantity: 2, price: 10.50 },
  { quantity: 1, price: 25.00 },
  { quantity: 3, price: 7.25 }
]);
// Result: { quantity: 6, price: 42.75 }

console.log(result1, result2, result3, emptySum, cart);
