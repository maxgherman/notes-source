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
      return handler(action());
    } catch (e) {
      if(e instanceof Promise) {
        const inner = await e.catch(x => x);
  
        if(inner.isExEffect){
          return handler();
        }
  
        throw inner;
      }
  
      throw e;
    }
  }
  
  const Nothing = () => () => {}
  const Just = (value) => () => value
  
  const $case = (caseOf) =>
    (maybe) => {
      const value = maybe();
  
      // for the lack of native pattern matching in JavaScript
      return (value == null || value === undefined) ?
        caseOf.nothing() :
        caseOf.just(value)
    }
  
  const handler = (value) => value === undefined ?
    Nothing() :
    Just(value);
  
  const div = () => {
    const result = divide(10, 0);
    return result;
  }
  
  run(div, handler)
  .then($case({
    nothing: () => "Division by zero",
    just: (value) => `Result: ${value}`
  }))
  .then(console.log);
  