const fileIds = {
    id: 0,
    data: {},
    getFileId(name) {
        let entry = this.data[name];

        entry = entry === undefined ? (this.id++) : entry;
        this.data[name] = entry;

        return entry;
    }
};

const updateFileProgress = (id, name, progress) => {
    const results = document.querySelector('.octet .results');

    let parent = document.getElementById(id);

    if(!parent) {
        parent = document.createElement('div');
        parent.id = id;
        results.appendChild(parent);
    }

    if(progress === undefined) {
        parent.innerHTML = name;
        return;
    }

    if(progress >= 100) {
        parent.innerHTML = name;
        return;
    }

    progressBar = Math.floor((23 * progress) / 100);
    parent.innerHTML = `${(new Array(progressBar)).fill('=').join('')}> ${progress} %`;
}

const strategy = {
    'initial': (files) => {
        Initial(Array.from(files), 20240, (name, progress) => {
            const id = fileIds.getFileId(name);
            updateFileProgress(id, name, progress);
        }).read()
    },

    'producer-consumer': (files) => {
        ProducerConsumer(Array.from(files), (name) => {
            const id = fileIds.getFileId(name);
            updateFileProgress(id, name);
        }).read();
    },

    'observables': (files) => {
        Observables(20240, (name, progress) => {
            const id = fileIds.getFileId(name);
            updateFileProgress(id, name, progress);
        })
        .run(Array.from(files));
    }
};

document.querySelector('.octet .btn-upload')
.addEventListener('click', () => {
    const input = document.querySelector('.octet .file-input');

    input.click();
})

document.querySelector('.octet .file-input')
.addEventListener('change', (e) => {
    const results = document.querySelector('.octet .results');
    results.innerHTML = '';

    const uploadOption = document.querySelector('.octet .upload-option');
    strategy[uploadOption.value](e.target.files);

    e.target.value = [];
});
