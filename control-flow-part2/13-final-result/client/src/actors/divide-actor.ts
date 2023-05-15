import type { Actor } from './actor';
import type {
    EnvironmentMessage,
    EnvironmentRequestMessage,
    ErrorMessage,
    WorkerMessage
} from './messages';
import { ActorBase } from './actor';

export const DivideActor = (
    errorActor: Actor<ErrorMessage>,
    envActor: Actor<EnvironmentRequestMessage>
    ): Actor<WorkerMessage> => {

    const data: { serverURL: string | undefined } = {
        serverURL: undefined
    };

    const result = ActorBase(receive);

    async function receive(message: WorkerMessage | EnvironmentMessage) {
        if(message.type === 'divide') {
            const {x,y} = (message as WorkerMessage).data;

            if(y === 0) {
                await errorActor
                .receive({
                    type: 'error',
                    data: [{
                        field: 'y',
                        error: new Error('Division by zero')
                    }]
                });
    
                return;
            }

            const response = await fetch(`${data.serverURL}/divide`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({x,y})
            });

            if(response.ok)
            {
                const data = await response.json() as { Value: number };
                await result.sendParent({
                    type: 'result',
                    data: data.Value
                });
            } else {
                await errorActor
                .receive({
                    type: 'error',
                    data: [{
                        error: new Error('Server error')
                    }]
                });
            }
        }

        if(message.type === 'environment') {
            const { serverURL } = (message as EnvironmentMessage).data;
            data.serverURL = serverURL;
        }
    }

    envActor.receive({
        type: 'environment',
        receive
    });
  
    return result;
}