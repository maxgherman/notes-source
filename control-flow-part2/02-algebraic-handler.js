const EffectEx = (message) => ({
    isExEffect: true,
    message
  });
  
  const raise = (message) => EffectEx(message);
  
  function divide(x, y) {
    return y === 0 ? raise("Division by zero") : (x / y);
  }
  
  function catchEx(action, handler) {
    const result = action();
  
    return result.isExEffect ? handler(result.message) : result;
  }
  
  function divWithInfinity(x, y) {
    return catchEx(
      () => divide(x, y),
      (s) => { console.log(s); return Infinity}
    );
  }
  
  const result = divWithInfinity(10, 0);
  console.log(result);
  