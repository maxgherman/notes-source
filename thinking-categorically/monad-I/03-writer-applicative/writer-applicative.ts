type Writer<A> = { value: A; log: string[] };

function pure<A>(value: A): Writer<A> {
    return { value, log: [] };
}

function apply<A, B>(wf: Writer<(a: A) => B>, wa: Writer<A>): Writer<B> {
    return {
        value: wf.value(wa.value),
        log: [...wf.log, ...wa.log]
    };
}

function addLog(x: number): Writer<number> {
    return { value: x, log: ["Added " + x] };
}

const w1 = addLog(3);
const w2 = addLog(5);
const wf = pure((a: number) => (b: number) => a + b);
const result = apply(apply(wf, w1), w2);
console.log(result.value); // 8
console.log(result.log);   // ["Added 3", "Added 5"]
