function* divide() {
    let count = 0;
  
    const queue = [];
  
    while(true) {
      const value = yield;
  
      queue.push(value);
      count++;
  
      if(count % 2 === 0) {
        const x = queue.shift();
        const y = queue.shift();
        count = 0;
  
        yield y === 0 ?
          new Error('Division by zero') :
          (x/y);
      }
    }
  }
  
  const div = divide();
  div.next();
  div.next(10);
  console.log(div.next(2).value);
  
  div.next();
  div.next(20);
  console.log(div.next(0).value);
  