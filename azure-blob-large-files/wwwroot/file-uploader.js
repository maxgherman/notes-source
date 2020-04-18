
/**
 * 
 * @param {Array.<File>} files - array of files
 * @param {number} chunkSize - chunk upload size (default 1024)
 * @param {number} delay - chunk upload delay (default 0)
 * @param {number} fileUploadCount - number of simultaneous files to upload  
 */
const FileUploader = (
    files,
    chunkSize = 1024,
    delay = 0,
    fileUploadCount = 2) => {
    
    const fileQueue = [];
    let progressEvent;

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

    const uploadChunk = (fileEntry) => {

        if(fileEntry.chunkQueue.length <= 0) {
            commitFile(fileEntry);
            return;
        }

        const chunkEntry = fileEntry.chunkQueue.pop();

        fetch('/files/chunk', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/octet-stream',
                'Content-Length': chunkEntry.data.size,
                ...(fileEntry.fileId && {'X-Content-Id': fileEntry.fileId}),
                'X-Chunk-Id': chunkEntry.index,
            },
            body: chunkEntry.data
        })
        .then(response => response.json())
        .then(data => {
            fileEntry.fileId = data.fileId;
            const chunk = splitChunk(fileEntry.file, chunkEntry.start, fileEntry.size);

            if(chunk.success) {
                fileEntry.chunkQueue.push({
                    index: chunkEntry.index + 1,
                    data: chunk.result,
                    start: chunk.chunkEnd
                })

                if(progressEvent) {
                    const progress = ((chunkEntry.start / fileEntry.file.size) * 100).toFixed(2);
                    progressEvent(fileEntry.fileId, fileEntry.file.name, progress);
                }
            }

            setTimeout(() => {
                uploadChunk(fileEntry);
            }, delay)
        })
        .catch(error => {
            console.log(error);
            // (Optional): Implement retry chunk upload logic here
            // uploadChunk(fileEntry);
        });
    }

    const uploadFile = () => {
        const entry = fileQueue.pop();

        if(!entry) {
            return;
        }

        const chunk = splitChunk(entry.file, 0);
       
        if(chunk.success) {
            entry.chunkQueue.push({
                index: 0,
                data: chunk.result,
                start: chunk.chunkEnd
            });

            uploadChunk(entry);
        }
    }

    const commitFile = (fileEntry) => {
        fetch(`/files/${fileEntry.fileId}`, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
            },
            body: JSON.stringify({
                fileName: fileEntry.file.name,
                size: fileEntry.file.size
            })
        })
        .then(() => {
            progressEvent(fileEntry.fileId, fileEntry.file.name, 100);
            launchUpload();
        })
        .catch(console.log);
    }

    const launchUpload = () => {
       
        if(fileQueue.length <= 0) {
            return;
        }

        let count = fileUploadCount;

        while(count > 0) {
            uploadFile();
            count--;
        }
    }

    return {
        upload: () => {
            const entries = files.map(file => ({
                file,
                chunkQueue: []
            }));

            fileQueue.push.apply(fileQueue, entries);

            launchUpload();
        },

        reportProgress: (handle) => {
            progressEvent = handle;
        } 
    };
}