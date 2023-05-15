import { Actor, ActorBase } from './actor';
import type { EnvironmentRequestMessage } from './messages';

export const EnvActor = (): Actor<EnvironmentRequestMessage> =>
    ActorBase(async message => {
        if(message.type != 'environment') {
            return;
        }

        await message.receive({
            type: 'environment',
            data: {
                serverURL: import.meta.env.VITE_SERVER_URL 
            }
        })
    });