const Actor = (receiveMessage) => {
    const mailbox = [];
    let isProcessingMailbox = false;
  
    const processMailbox = async () => {
      if (isProcessingMailbox) {
        return;
      }
  
      isProcessingMailbox = true;
  
      while (mailbox.length > 0) {
        const message = mailbox.shift();
        await receiveMessage(message);
      }
  
      isProcessingMailbox = false;
    }
  
    return {
      send(message) {
        mailbox.push(message);
        processMailbox();
      },
  
      receive: receiveMessage,
    }
  }
  
  const DivideActor = () => {
  
    const receive = async (message) => {
      if(message.type !== 'divide') {
        return;
      }
  
      const {x, y} = message.data;
  
      if(y === 0) {
        await message.error
          .receive({
            type: 'error',
            data: new Error('Division by zero')
          });
  
        return;
      }
  
      const result = x/y;
      await message.sender
        .receive({
          type: 'result', data: result
        });
    }
  
    return Actor(receive);
  }
  
  const ErrorActor = () => {
  
    const receive = async (message) => {
      if(message.type !== 'error') {
        return;
      }
  
      const error = message.data;
      console.log(`Exception: ${error}`);
    }
  
    return Actor(receive);
  }
  
  const RunActor = () => {
  
    const result = Actor(receive);
  
    async function receive(message) {
  
      switch(message.type) {
        case 'result':
          console.log(`Result: ${message.data}`);
          break;
  
        case 'divide':
          message.sender.receive({...message, sender: result });
          break;
      }
    }
  
    return result;
  }
  
  const run = RunActor();
  const divide = DivideActor();
  const error = ErrorActor();
  
  run.send({
    type: 'divide',
    sender: divide,
    error,
    data: { x: 10, y: 2 }
  });
  
  run.send({
    type: 'divide',
    sender: divide,
    error,
    data: { x: 10, y: 0 }
  });
  