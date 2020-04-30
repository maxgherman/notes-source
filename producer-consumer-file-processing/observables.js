const Observables = (chunkSize = 102400, processEvent = null) => {

    const { from, operators, forkJoin, defer, EMPTY, Observable } = rxjs;
    const { reduce, map, concatAll } = operators;

    const splitChunk = (file, start) => {
        const end = file.size;

        if(start >= end) {
            return {
                success: false,
                result: null
            };
        }
        const chunkStart = start;
        const chunkEnd = Math.min(start + chunkSize, end);

        return {
            success: true,
            chunkEnd,
            result: file.slice(chunkStart, chunkEnd)
        };
    }

    const splitChunks = (file) =>
        new Observable(subscriber => {
            let start = 0;

            while(true) {
                const { success, chunkEnd } = splitChunk(file, start);

                if(success) {
                    subscriber.next({ success, chunkEnd });
                    start = chunkEnd;
                } else {
                    subscriber.complete();
                    break;
                }
            }
        });

    const process = (files) =>
        forkJoin(
            files.map(file =>
                splitChunks(file)
                .pipe(
                    map((chunk) =>
                         defer(() => from(new Promise(resolve =>
                            setTimeout(() => {

                                if(processEvent) {
                                    const progress = ((chunk.chunkEnd / file.size) * 100).toFixed(2);
                                    processEvent(file.name, progress);
                                }

                                resolve(chunk);
                            }, 200)
                        )))),
                    concatAll()
                )
            )
        );

    const group = (files) =>
        from(files)
        .pipe(
            reduce((acc, cur, index) => {

                if(index % 2 === 0) {
                    const item = [cur];
                    acc.result.push(item);
                    acc.last = item;
                } else {
                    acc.last.push(cur);
                }

                return acc;

            }, { result: [], last: null }),
            map(x => from(x.result)),
            concatAll()
        );


    return {
        run: (files) => {

            group(files)
            .pipe(
                map(process),
                concatAll()
            )
            .subscribe(EMPTY)
        }
    }
};
