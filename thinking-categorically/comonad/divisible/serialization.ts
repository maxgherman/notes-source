interface Encoder<A> {
  encode: (value: A) => readonly string[]
}

// Divisible instance for Encoder
const conquer = <A>(): Encoder<A> => ({
  encode: (_) => []
})

const divide = <A, B, C>(
  split: (a: A) => [B, C],
  encodeB: Encoder<B>,
  encodeC: Encoder<C>
): Encoder<A> => ({
  encode: (a: A) => {
    const [b, c] = split(a)
    return [...encodeB.encode(b), ...encodeC.encode(c)]
  }
})

// Usage: Encode complex objects by splitting them
interface Person {
  name: string
  age: number
}

const stringEncoder: Encoder<string> = { encode: s => [JSON.stringify(s)] }
const numberEncoder: Encoder<number> = { encode: n => [n.toString()] }

const personEncoder: Encoder<Person> = divide(
  (p: Person) => [p.name, p.age],
  stringEncoder,
  numberEncoder
)
console.log(personEncoder.encode({name: "Alice", age: 30}).join(","))
// Result: "Alice",30
