const isGenerator = g => g?.constructor?.name === 'GeneratorFunction';

function run(generator, ...args) {
  let iterator = generator(...args);
  let currentValue = null;
  let result = iterator.next(currentValue);
  currentValue = result.value;

  while (!result.done) {
    result = iterator.next(currentValue);

    if(isGenerator(result.value)) {
      iterator = result.value(currentValue);
      result = iterator.next(currentValue);
    }

    if(!result.done) {
      const prevValue = currentValue;
      currentValue = result.value;

      if(isGenerator(currentValue)) {
        iterator = currentValue(prevValue);
      }
    }
  }

  return currentValue;
 }

 const divide = (x, y) => function* (next) {
  if (y === 0) {
    yield new Error("Division by zero");
  } else {
    yield x / y;
  }

  yield next
}

const handler = (next) => function* (ex){
  if(ex instanceof Error){
    yield `Exception: ${ex.message}`;
  } else {
    yield next
  }
};

function* continuation(result) {
  yield `Result: ${result}`;
}

const div = divide(10, 0);
const handle = handler(continuation);
const result = run(div, handle);
console.log(result)
