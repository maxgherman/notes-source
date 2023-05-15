import { useState, useMemo } from 'react';
import type { StartMessage } from '../actors/messages';
import { RunActor } from '../actors/run-actor'
import { ErrorActor } from '../actors/error-actor';

export const useActors = () => {
    const [value, setValue] = useState<number|undefined>();
    const [errors, setErrors] = useState<{field: string, error: string}[]>([]);

    const errorActor = useMemo(() => ErrorActor((e) => {
        setErrors(e);
        setValue(undefined);
    }), [setErrors, setValue]);
    
    const runActor = useMemo(() => RunActor(errorActor, (e) =>{
        setValue(e);
        setErrors([]);
    }), [errorActor]);

    const sendMessage = (value: StartMessage['data']) => {
        const message = {
            type: 'run',
            data: value,
            error: errorActor
        }
        runActor.send(message);
    };

    return { value, errors, sendMessage };
}