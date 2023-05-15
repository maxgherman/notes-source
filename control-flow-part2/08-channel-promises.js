const Channel = () => {

    const queue = [];
    let resolver = null;
    let promise = new Promise(resolve => {
      resolver = resolve;
    });
  
    return {
      async send(value) {
        if(queue.length < 1) {
          queue.push(value);
          return;
        }
  
        const x = queue.shift();
        const y = value;
        const result = y === 0 ?
          new Error('Division by zero') :
          (x/y);
  
        const prev = resolver;
        resolver = null;
        await prev(result);
        promise = new Promise((resolve) => {
          resolver = resolve;
        });
      },
  
      async receive() {
        const value = await promise;
        promise = new Promise((resolve) => {
          resolver = resolve;
        });
  
        return value;
      }
    }
  }
  
  const divide = Channel();
  
  async function sender() {
    await divide.send(10);
    await divide.send(2);
  
    await divide.send(10);
    await divide.send(0);
  }
  
  async function receiver() {
    let value = await divide.receive();
    console.log(value);
  
    value = await divide.receive();
    console.log(value);
  }
  
  sender();
  receiver();
  