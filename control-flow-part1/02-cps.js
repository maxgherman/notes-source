function divide(x, y, result, error) {
    if (y === 0) {
      error(new Error("Division by zero"));
    } else {
      result(x / y);
    }
  }
  
  const handler = function (ex, c) {
    c(`Exception: ${ex.message}`);
  };
  
  const continuation = function (result, c) {
    c(`Result: ${result}`);
  };
  
  const messages = [];
  
  const logger = (message, c) => {
    messages.push(message);
    c(messages);
  }
  
  divide(10, 0,
    (r) => continuation(r, (v) => logger(v, console.log)),
    (e) => handler(e, (v) => logger(v, console.log))
  );
  