const EffectEx = (message) => Object.freeze({
    isEffect: true,
    isException: true,
    handler: () => console.log(message)
  });
  
  const EffectInput = (value) => Object.freeze({
    isEffect: true,
    handler: () => value
  });
  
  const EffectOutput = (message) => Object.freeze({
    isEffect: true,
    handler: () => console.log(message)
  });
  
  const simulatedInput = (value) => function* () {
    yield EffectOutput('Enter number:');
    return yield Promise.resolve(EffectInput(value))
  }
  
  const consoleInput = function* () {
    const readline = require('readline');
    const instance = readline.createInterface({
        input: process.stdin,
        output: process.stdout
    });
  
    return yield new Promise(resolve => {
      instance.question('Enter number: ', function(value) {
        instance.close();
        resolve(EffectInput(Number(value)));
      });
    });
  }
  
  function* output(message) {
    yield EffectOutput('Result: ');
    yield EffectOutput(message);
  }
  
  function* raise(message) {
    yield EffectEx(message);
  }
  
  function* divide(x, y) {
    if(y === 0) {
      return yield* raise('Division by zero');
    }
  
    return yield (x/y);
  }
  
  const program = (input) => function* () {
    const y = yield* input();
    const result = yield* divide(10, y);
    yield* output(result);
  }
  
  async function run(generator) {
    const iterator = generator();
  
    let result = iterator.next();
  
    try {
      while(!result.done) {
        const step = await Promise.resolve(result.value);
  
        if (step?.isEffect) {
  
          if(step?.isException) {
            throw result.value;
          }
  
          const value = step.handler();
          result = iterator.next(value);
        } else {
          result = iterator.next(step);
        }
      }
    } catch(e) {
      if(e?.isEffect && e?.isException) {
        e.handler();
        return;
      }
  
      throw e;
    }
  }
  
  run(
    program(simulatedInput(1))
  );
  