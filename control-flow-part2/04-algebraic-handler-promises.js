const EffectEx = (message) => ({
    isExEffect: true,
    message
  });
  
  const raise = (message) => { throw Promise.reject(EffectEx(message)); }
  
  function divide(x, y) {
    if(y === 0) {
      raise("Division by zero");
    }
  
    return (x/y);
  }
  
  async function run(action, handler) {
    try {
      return action();
    } catch (e) {
      if(e instanceof Promise) {
        const inner = await e.catch(x => x);
  
        if(inner.isExEffect){
          return handler(inner.message);
        }
  
        return inner;
      }
  
      throw e;
    }
  }
  
  const div = () => {
    const result = divide(10, 0);
    return `Result: ${result}`;
  }
  
  run(
    div,
    (e) => `Exception: ${e}`
  )
  .then(console.log)  