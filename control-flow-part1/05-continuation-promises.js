const result = (value) => {
    if(value?.then) {
      return value;
    }
  
    return {
      then: f => result(f(value)),
      catch: _ => error(value)
    };
  }
  
  const error = (value) => {
    if(value?.catch) {
      return value;
    }
  
    return {
      then: f => result(f(value)),
      catch: f => error(f(value))
    };
  }
  
  const divide = (x, y, result, error) =>
    y === 0 ?
      error(new Error("Division by zero")) :
      result(x/y);
  
  const handler = ex => c => c(`Exception: ${ex.message}`);
  
  const continuation = result => c => c(`Result: ${result}`);
  
  divide(10, 0, result, error)
  .catch(e => handler(e)(result))
  .then(value => continuation(value)(result))
  .then(console.log)
  