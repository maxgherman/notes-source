function compose(...fns) {
    return (...args) =>
      fns.reduceRight((res, fn) => [fn.call(null, ...res)], args)[0]
  }
  
  const divide = (x, y, result, error) => {
    if (y === 0) {
      error(new Error("Division by zero"));
    } else {
      result(x / y);
    }
  }
  
  const handler = ex => c => {
    c(`Exception: ${ex.message}`);
  };
  
  const continuation = result => c => {
    c(`Result: ${result}`);
  };
  
  const flip = f => y => x => f(x)(y)
  
  compose(
    ([c, h]) => divide(10, 0, c, h),
    x => [flip(continuation)(x), flip(handler)(x)])
    (console.log)
  