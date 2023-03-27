const Coroutine = () => {
    const tasks = [];
    let promise = Promise.resolve();
  
    const coroutine = {
      add(task) {
        tasks.push(task);
      },
  
      run(value) {
        if(tasks.length <= 0){
          return;
        }
  
        const task = tasks[0];
  
        promise = promise
        .then(() => task(value))
        .catch(data => data)
        .then(data => {
          tasks.shift();
          coroutine.run(data);
        });
      }
    }
  
    return coroutine;
  };
  
  const divide = (x, y) => () => {
    if(y === 0){
      throw new Error("Division by zero");
    } else {
      return x / y;
    }
  }
  
  const handler = (continueWith) => (ex) =>
    ex instanceof Error ? `Exception: ${ex.message}` : continueWith(ex);
  
  const continuation = (result) =>`Result: ${result}`;
  
  const coroutine = Coroutine();
  coroutine.add(divide(10, 0))
  coroutine.add(handler(continuation));
  coroutine.add(console.log);
  
  coroutine.run();
  