const Actor = (receiveMessage) => {
    const mailbox = [];
    const children = [];
    let parent = null;
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
  
      spawn(child) {
        children.push(child);
        child.setParent(this);
      },
  
      setParent(parent) {
        parent = parent;
      },
  
      async sendParent(message) {
        if (parent) {
          await parent.send(message);
        }
      },
  
      async sendChildren(message) {
        const promises = children
          .map(child => child.send(message));
  
          await Promise.all(promises);
      }
    }
  }
  
  const GetNumberActor = (number, type) => {
  
    const receive = async (message) => {
      if(message.type !== 'number') {
        return;
      }
  
      await message.sender.receive({
        type,
        data: number,
        receiver: message.receiver,
        error: message.error
      });
    }
  
    return Actor(receive);
  }
  
  const DivideActor = () => {
  
    const stackX = [];
    const stackY = [];
  
    const divide = async (message) => {
  
      if(stackX.length <= 0  || stackY.length <= 0) {
        return;
      }
  
      const x = stackX.pop();
      const y = stackY.pop();
  
      if(y === 0) {
        await message.error
          .receive({
            type: 'error',
            data: new Error('Division by zero')
          });
  
        return;
      }
  
      const result = x/y;
      await message.receiver.receive({
        type: 'result',
        data: result
      });
    }
  
    const receive = async (message) => {
  
      switch(message.type) {
          case 'x':
            stackX.push(message.data);
            break;
  
          case 'y':
            stackY.push(message.data);
            break;
      }
  
      await divide(message);
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
  
    async function receive(message) {
  
      if(message.type === 'result') {
        console.log(`Result: ${message.data}`);
      }
    }
  
    return Actor(receive);
  }
  
  const run = RunActor();
  const getX = GetNumberActor(10, 'x');
  const getY = GetNumberActor(2, 'y');
  const getX1 = GetNumberActor(10, 'x');
  const getY1 = GetNumberActor(0, 'y');
  
  const divide = DivideActor();
  const error = ErrorActor();
  
  run.spawn(getX);
  run.spawn(getY);
  run.spawn(getX1);
  run.spawn(getY1);
  
  run.sendChildren({
    type: 'number',
    sender: divide,
    receiver: run,
    error
   });
  