const Channel = (transform = null) => {
    const queue = [];
    let receiveResolve = null;
  
    return {
      send: (value) =>
        new Promise(resolve => {
          if(queue.length <= 0 && receiveResolve) {
            receiveResolve(transform ? transform(value) : value);
            receiveResolve = null;
          } else {
            queue.push(transform ? transform(value) : value);
          }
  
          resolve();
        }),
  
      receive: () =>
        new Promise(resolve => {
          if(queue.length > 0) {
            resolve(queue.shift());
            return;
          }
  
          receiveResolve = resolve;
        })
    }
  }
  
  async function createPipe(...channels) {
    let result = Promise.resolve();
  
    while(channels.length > 0) {
      const channel1 = channels.shift();
      const channel2 = channels[0];
  
      if(channel2) {
        result = result
        .then(() => channel1.receive())
        .then(value => channel2.send(value));
      }
    }
  
    await result;
  }
  
  async function pipe(...channels) {
    while(true){
      await createPipe(...channels);
    }
  }
  
  async function sender(channel) {
    channel.send(10);
    channel.send(0);
  }
  
  async function receiver(channel) {
    while(true){
      const value = await channel.receive();
      console.log(value);
    }
  }
  
  const channel1 = Channel();
  const channel2 = Channel((x) => [10, x]);
  const channel3 = Channel(([x, y]) =>
    y === 0 ? new Error('Division by zero') : (x/y));
  
  pipe(channel1, channel2, channel3);
  receiver(channel3);
  sender(channel1);
  