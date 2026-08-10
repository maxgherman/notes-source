interface Encoder<A> {
  encode: (value: A) => string
}

// Contravariant in the input type A
const contramap = <A, B>(f: (b: B) => A) =>
  (encoder: Encoder<A>): Encoder<B> => ({
    encode: (b: B) => encoder.encode(f(b))
  })

// Usage
const intEncoder: Encoder<number> = {
  encode: (n) => n.toString()
}

// Reuse for any type that can become a number
const lengthEncoder: Encoder<string> =
  contramap((s: string) => s.length)(intEncoder)

console.log(lengthEncoder.encode("1234567890"))