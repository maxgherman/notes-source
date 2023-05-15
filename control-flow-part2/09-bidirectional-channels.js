const Channel = () => {

    let resolver = null;
    let promise = new Promise(resolve => {
      resolver = resolve;
    });
  
    return {
      async send(value) {
        const prev = resolver;
        resolver = null;
        await prev(value);
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
  
  const channelIn = Channel();
  const channelOut = Channel();
  
  const sendMessageForward = async (x, y) => {
    await channelIn.send(x);
    await channelIn.send(y);
  
    return await channelOut.receive();
  }
  
  const sendMessageBackward = async () => {
    const x = await channelIn.receive();
    const y = await channelIn.receive();
  
    await channelOut.send(
      y === 0 ? new Error('Division by zero') : (x/y)
    );
  }
  
  async function sender() {
    let value = await sendMessageForward(10, 5);
    console.log(value);
  
    value = await sendMessageForward(10, 0);
    console.log(value);
  }
  
  async function receiver() {
    await sendMessageBackward();
    await sendMessageBackward();
  }
  
  Promise.all([sender(), receiver()]);
  