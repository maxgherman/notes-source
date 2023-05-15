import type { Actor } from './actor';
import type { ValidateMessage, ErrorMessage, WorkerMessage } from './messages';
import { ActorBase } from './actor';

const validateNumber = (value: string) => {
    const result = Number(value);

    return value === '' ||
        value === null ||
        value === undefined || isNaN(result) ?
        { success: false, value: null } :
        { success: true, value: result };
}

export const ValidateActor = (
    errorActor: Actor<ErrorMessage>,
    workerActor: Actor<WorkerMessage>): Actor<ValidateMessage> => {

    const result = ActorBase(receive);

    async function receive(message: ValidateMessage) {
        if (message.type !== 'validate') {
            return;
        }

        const { success: successX, value: x } = validateNumber(message.data.x);
        const { success: successY, value: y } = validateNumber(message.data.y);

        const errors: ErrorMessage['data'] = [];
        
        if (!successX) {
            errors.push({
                field: 'x',
                error: new Error('Validation error: not a number')
            });
        }

        if (!successY) {
            errors.push({
                field: 'y',
                error: new Error('Validation error: not a number')
            });
        }

        if (errors.length > 0) {
            errorActor.receive({
                type: 'error',
                data: errors
            });

            return;
        }

        await workerActor.receive({
            type: 'divide',
            data: { x: x as number, y: y as number }
        });
    }

    return result;
}