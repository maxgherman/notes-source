const identity = <T>(x: T) => x;

const promiseValue = Promise.resolve("hello");
const promiseIdentity = Promise.resolve(identity);

// Simulate applicative ap
const result = promiseIdentity.then(f => promiseValue.then(f));
result.then(console.log); // logs: "hello"

const result2 = [(identity)].flatMap(f => [1, 2, 3].map(f));
console.log(result2);
