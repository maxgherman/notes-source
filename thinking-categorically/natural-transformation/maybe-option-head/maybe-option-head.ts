// Option/Maybe type
type Option<A> = { tag: 'some'; value: A } | { tag: 'none' };

const some = <A>(value: A): Option<A> => ({ tag: 'some', value });
const none: Option<never> = { tag: 'none' };

// Natural transformation from Array to Option
const safeHead = <A>(arr: Array<A>): Option<A> =>
    arr.length > 0 ? some(arr[0]!) : none;

// Option functor map
const mapOption = <A, B>(f: (a: A) => B, opt: Option<A>): Option<B> =>
    opt.tag === 'some' ? some(f(opt.value)) : none;

// Verification of naturality:
// For any function f: A -> B and array arr: Array<A>:
// mapOption(f, safeHead(arr)) should equal safeHead(arr.map(f))

const f2 = (x: number) => x.toString();
const arr2 = [1, 2, 3];
const left = mapOption(f2, safeHead(arr2));
const right = safeHead(arr2.map(f2));

console.log(`Left: ${JSON.stringify(left)}`);   // {"tag":"some","value":"1"}
console.log(`Right: ${JSON.stringify(right)}`); // {"tag":"some","value":"1"}