function Fiber(coroutine) {
    var iterator = coroutine();
    let data;
  
    return {
      moveNext(props) {
  
        if(props){
          data = props;
        }
  
        const result = iterator.next(data);
  
        if (result.done) {
          return false;
        }
  
        if(Array.isArray(result.value) && result.value.length > 0){
          const [next, p] = result.value;
          next.moveNext(p);
        }
  
        return true;
      }
    }
  }
  
  const scheduler = () => {
  
    const fiber1 = (x,y) => (next) => Fiber(function* () {
      if(y === 0){
        yield [next, new Error("Division by zero")];
      } else {
        yield [next, (x / y)];
      }
  
      yield next;
    });
  
    const fiber2 = (next) => Fiber(function* () {
      let result;
  
      while(!result){
        result = yield []; // wait
      }
  
      if(result instanceof Error){
        yield [next, `Exception: ${result.message}`];
      } else {
        yield [next, `Result: ${result}`];
      }
    });
  
    const fiber3 = Fiber(function* () {
      let result;
  
      while(!result){
        result = yield []; // wait
      }
  
      console.log(result);
    });
  
    return {
      run(x, y) {
        const f3 = fiber3;
        const f2 = fiber2(f3);
        const f1 = fiber1(x, y)(f2);
  
        while (f1.moveNext() || f2.moveNext() || f3.moveNext()) {
          console.log("Main thread doing other work...");
        }
      }
    }
  };
  
  scheduler()
  .run(10, 0)
  