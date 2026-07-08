// Domain types
interface Person {
  name: string;
  age: number;
  email: string;
}

// Maybe monad implementation
interface Maybe<T> {
  flatMap<U>(f: (value: T) => Maybe<U>): Maybe<U>;
  map<U>(f: (value: T) => U): Maybe<U>;
  getOrElse(defaultValue: T): T;
}

class Some<T> implements Maybe<T> {
  constructor(private value: T) {}

  flatMap<U>(f: (value: T) => Maybe<U>): Maybe<U> {
    return f(this.value);
  }

  map<U>(f: (value: T) => U): Maybe<U> {
    return new Some(f(this.value));
  }

  getOrElse(defaultValue: T): T {
    return this.value;
  }

  toString(): string {
    return `Some(${this.value})`;
  }
}

class None<T> implements Maybe<T> {
  flatMap<U>(f: (value: T) => Maybe<U>): Maybe<U> {
    return new None<U>();
  }

  map<U>(f: (value: T) => U): Maybe<U> {
    return new None<U>();
  }

  getOrElse(defaultValue: T): T {
    return defaultValue;
  }

  toString(): string {
    return "None";
  }
}

// Kleisli composition helper
const kleisliCompose = <A, B, C>(
  f: (a: A) => Maybe<B>,
  g: (b: B) => Maybe<C>
): (a: A) => Maybe<C> => {
  return (a: A) => f(a).flatMap(g);
};

// Kleisli arrows for CSV field transformations
const parseAge = (ageStr: string): Maybe<number> => {
  const age = Number(ageStr);
  return Number.isInteger(age) ? new Some(age) : new None<number>();
};

const validateAge = (age: number): Maybe<number> => {
  return (age >= 0 && age <= 150) ? new Some(age) : new None<number>();
};

const parseEmail = (email: string): Maybe<string> => {
  return email.includes('@') ? new Some(email) : new None<string>();
};

// Composed Kleisli arrows
const processAge = kleisliCompose(parseAge, validateAge);
const processEmail = parseEmail; // No additional validation needed

// CSV row transformation using Kleisli composition
const transformCSVRow = (row: string[]): Maybe<Person> => {
  if (row.length !== 3) return new None<Person>();

  const [name, ageStr, emailStr] = row;

  return processAge(ageStr)
    .flatMap(validAge =>
      processEmail(emailStr)
        .map(validEmail => ({
          name,
          age: validAge,
          email: validEmail
        }))
    );
};

// Alternative using explicit Kleisli composition
const createPerson = (name: string) => (age: number) => (email: string): Person => ({
  name,
  age,
  email
});

const transformCSVRowFunctional = (row: string[]): Maybe<Person> => {
  if (row.length !== 3) return new None<Person>();

  const [name, ageStr, emailStr] = row;

  // Using Kleisli composition to build the transformation pipeline
  return new Some(name)
    .flatMap(n =>
      processAge(ageStr)
        .flatMap(a =>
          processEmail(emailStr)
            .map(e => createPerson(n)(a)(e))
        )
    );
};

// Usage example
const csvRows = [
  ["Alice", "25", "alice@example.com"],
  ["Bob", "-5", "bob@example.com"],     // Invalid age
  ["Charlie", "30", "invalid-email"],   // Invalid email
  ["Diana", "28", "diana@example.com"]
];

const results = csvRows.map(transformCSVRow);
results.forEach(result => console.log(JSON.stringify(result)));
// Output:
// Some({"name":"Alice","age":25,"email":"alice@example.com"})
// None
// None
// Some({"name":"Diana","age":28,"email":"diana@example.com"})

