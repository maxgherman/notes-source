const wrapIt = (value, symbol) => ({
    value,
    isWrapper: s => s == symbol
  });
  
  const callCC = f => {
    const id = Symbol();
  
    try {
      return f(value => { throw wrapIt(value, id); })
    }
    catch(ex){
      if(ex?.isWrapper(id)) {
        return ex.value;
      }
  
      throw ex;
    }
  }
  
  function divide(x, y, c) {
    if (y === 0) {
      c({
        success: false,
        value: new Error("Division by zero")
      });
    } else {
      c({
        success: true,
        value: result(x / y)
      });
    }
  }
  
  const handler = function (ex, c) {
    c(`Exception: ${ex.message}`);
  };
  
  const continuation = function (result, c) {
    c(`Result: ${result}`);
  };
  
  const result1 = callCC(done => divide(10, 0, done));
  const result2 = callCC(done =>
    result1.success ?
      continuation(result1.value, done) :
      handler(result1.value, done)
  );
  _ = callCC(_ => console.log(result2))
  