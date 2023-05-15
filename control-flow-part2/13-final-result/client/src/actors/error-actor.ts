import type { Actor } from './actor';
import type { ErrorMessage } from './messages';
import { ActorBase } from './actor';

export type SetError = (data: {field: string, error: string}[]) => void 

export const ErrorActor = (setError: SetError): Actor<ErrorMessage> => {

    const receive = async (message: ErrorMessage) => {
      if(message.type !== 'error') {
        return;
      }
  
      const errors = message
        .data
        .map(({field, error}) => ({ field, error: error.message}));
      
        setError(errors);
    }
  
    return ActorBase(receive);
  }