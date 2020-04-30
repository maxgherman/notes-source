const ProducerConsumer = (files, progressEvent) => {
    const fileQueue = [];
    let stop = false;

    const produce = () => {
        if(files.length) {
            fileQueue.push(files.pop());
        }
    }

    const producer = () => {

        const interval = setInterval(() => {

            if(files.length <= 0) {
                stop = true;
                clearInterval(interval)
                return;
            }

            produce();
        }, 500);
    }

    const consume = () => {
        const file = fileQueue.pop();

        if(file && progressEvent) {
            progressEvent(file.name);
        }
    }

    const consumer = () => {
        const interval = setInterval(() => {

            if(stop && fileQueue.length <= 0) {
                clearInterval(interval)
                return;
            }

            consume();
        }, 500);
    }

    return {
        read: () => {
            producer();
            consumer();
        }
    }
}
