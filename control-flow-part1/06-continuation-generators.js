const isGenerator = g => g?.constructor?.name === 'GeneratorFunction';

function run(generator, ...args) {
  let iterator = generator(...args);
  let currentValue = null;
  let result = iterator.next(currentValue);
  currentValue = result.value;

  while (!result.done) {
    result = iterator.next(currentValue);

    if(isGenerator(result.value))
    {
      iterator = result.value(currentValue);
      result = iterator.next(currentValue);
    }

    if(!result.done) {
      const prevValue = currentValue;
      currentValue = result.value;

      if(isGenerator(currentValue))
      {
        iterator = currentValue(prevValue);
      }
    }
  }

  return currentValue;
 }

 function* divide(x, y, next) {
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

const result = run(divide, 10, 0, handler(continuation));
console.log(result)
