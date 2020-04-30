
const Initial = (files, chunkSize = 1024, progressEvent) => {

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

    const readChunk = (fileEntry) => {
        const { file, progress } = fileEntry;
        const { success, chunkEnd } = splitChunk(file, progress.start);

        if(success) {

            if(progressEvent) {
                const progress = ((chunkEnd / file.size) * 100).toFixed(2);
                progressEvent(file.name, progress);
            }

            progress.index++;
            progress.start = chunkEnd;

            setTimeout(() => {
                readChunk(fileEntry);
            }, 100)
        } else {
            read()
        }
    }

    const read = () => {
        if(files.length <= 0) {
            return;
        }

        const file = files.pop();
        const fileEntry = {
            file,
            progress: {
                start: 0,
                index: 0
            },
        }

        readChunk(fileEntry);
    }

    return {
        read
    }

}
