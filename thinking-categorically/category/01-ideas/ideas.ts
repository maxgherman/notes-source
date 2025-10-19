const compose = <A,B,C>(g:(_:B)=> C, f:(_:A)=> B) =>
    (x:A) => g(f(x));

const f = (x:number) => x.toString();      // Number -> String
const g = (s:string) => s.length > 2;      // String -> Boolean

const h = compose(g, f);          // Number -> Boolean

console.log(h(123)); // Output: true