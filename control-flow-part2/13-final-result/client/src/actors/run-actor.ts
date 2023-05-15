import type { Actor } from './actor';
import type { ErrorMessage, RunMessage } from './messages';
import { ActorBase } from './actor';
import { ValidateActor } from './validate-actor';
import { DivideActor } from './divide-actor';
import { EnvActor } from './env-actor';

export type SetResult = (data: number) => void

export const RunActor = (
  errorActor: Actor<ErrorMessage>,
  setResult: SetResult): Actor<RunMessage> => {

    const result = ActorBase(receive);
    
    const envActor = EnvActor();
    result.spawn(envActor);

    const divideActor = DivideActor(errorActor, envActor);
    result.spawn(divideActor);
    
    const validateActor = ValidateActor(errorActor, divideActor);
    result.spawn(validateActor);
    
    async function receive(message: RunMessage) {
  
      switch(message.type) {
        case 'result':
          setResult(message.data as number);
          break;
  
        case 'run':
          await validateActor.send({
            type: 'validate',
            data: message.data as {x: string, y: string}
      });
          break;
      }
    }
  
    return result;
  }