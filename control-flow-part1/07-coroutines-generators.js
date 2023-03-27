function coroutine(f) {
    const iterator = f();
    iterator.next(); // run till first yield
  
    return function(x) {
      return iterator.next(x).value;
    }
  }
  
  function* divide() {
    let [x, y] = yield;
  
    while(y == 0){
      y = yield new Error("Division by zero");
  
    }
  
    yield (x / y);
  }
  
  function* error(){
    while(true){
      const ex = yield
  
      if(ex instanceof Error){
        yield `Exception: ${ex.message}`;
      }
    }
  };
  
  function* result() {
    while(true){
      const result = yield
  
      yield `Result: ${result}`
    }
  }
  
  const div = coroutine(divide)
  const handler = coroutine(error);
  const continuation = coroutine(result)
  
  let value = div([10,0]);
  value = handler(value)
  console.log(value)
  
  value = div(1);
  value = continuation(value)
  console.log(value)
  