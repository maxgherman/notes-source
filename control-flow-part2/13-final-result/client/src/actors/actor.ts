
export type Actor<T> = {
    send: (message: T) => void
    receive: (message: T) => Promise<void>
    sendChildren: (message: T) => Promise<void>
    setParent: <R>(parent: Actor<R>) => void
    spawn: <R>(child: Actor<R>) => void
    sendParent: <R>(message: R) => Promise<void>
}

export const ActorBase = <T>(receiveMessage: (message: T) => Promise<void>): Actor<T> => {
    const mailbox: T[] = [];
    const children: Actor<unknown>[] = [];
    let parent: Actor<unknown> | undefined;
    let isProcessingMailbox = false;
  
    const processMailbox = async () => {
      if (isProcessingMailbox) {
        return;
      }
  
      isProcessingMailbox = true;
  
      while (mailbox.length > 0) {
        const message = mailbox.shift() as T;
        await receiveMessage(message);
      }
  
      isProcessingMailbox = false;
    }
  
    return {
      send(message: T) {
        mailbox.push(message);
        processMailbox();
      },
  
      receive: receiveMessage,
  
      spawn<R>(child: Actor<R>) {
        children.push(child as Actor<unknown>);
        child.setParent(this);
      },
  
      setParent<R>(value: Actor<R>) {
        parent = value as Actor<unknown>;
      },
  
      async sendParent<R>(message: R) {
        if (parent) {
          await parent.send(message);
        }
      },
  
      async sendChildren(message: T) {
        const promises = children
          .map(child => child.send(message));
  
          await Promise.all(promises);
      }  
    }
}