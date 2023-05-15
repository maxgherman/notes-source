const EffectEx = (message) => ({
    isExEffect: true,
    message
  });
  
  const raise = (message) => { throw EffectEx(message); }
  
  const callCC = (f, handler) => {
    try {
      return f();
    }
    catch(ex){
      if(ex?.isExEffect) {
        return handler(ex.message);
      }
  
      throw ex;
    }
  }
  
  function divide(x, y) {
    return y === 0 ? raise("Division by zero") : (x / y);
  }
  
  const result = callCC(
    () => divide(10, 0),
    (s) => { console.log(s); return Infinity; }
  );
  
  console.log(result)
  