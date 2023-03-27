function withCatch(f, handler) {
    return function () {
      try {
        f.apply(null, arguments);
      } catch (ex) {
        handler(ex);
      }
    };
  }
  
  function divide(x, y, continuation) {
    if (y === 0) {
      throw new Error("Division by zero");
    } else {
      continuation(x / y);
    }
  }
  
  const handler = function (ex) {
    console.log(`Exception: ${ex.message}`);
  };
  
  const continuation = function (result) {
    console.log(`Result: ${result}`);
  };
  
  const divideWithCatch = withCatch(divide, handler);
  
  divideWithCatch(6, 0, continuation); // "Exception: Division by zero"
  divideWithCatch(6, 2, continuation); // "Result: 3"
  