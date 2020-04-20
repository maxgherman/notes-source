document.querySelector('.octet .btn-upload')
.addEventListener('click', () => {
    const input = document.querySelector('.octet .file-input');
    input.click();
})

document.querySelector('.octet .file-input')
.addEventListener('change', (e) => {
    const chunkSize = document.querySelector('.octet .chunk-size');
    const delay = document.querySelector('.octet .delay');
    const fileUploadCount = document.querySelector('.octet .file-upload-count')

    const fileUploader = FileUploader(
        Array.from(e.target.files),
        parseInt(chunkSize.value),
        parseInt(delay.value),
        parseInt(fileUploadCount.value)
    );

    const results = document.querySelector('.octet .results');

    fileUploader.reportProgress((id, name, progress) => {
        let parent = document.getElementById(id);

        if(!parent) {
            parent = document.createElement('div');
            parent.id = id;
            results.appendChild(parent);
        }

        if(progress >= 100) {
            const link = document.createElement('a');
            link.href = `/files/${id}`;
            link.innerHTML = name;
            parent.innerHTML = '';
            parent.appendChild(link);
        } else {
            const progressBar = Math.floor((23 * progress) / 100);
            parent.innerHTML = `${(new Array(progressBar)).fill('=').join('')}> ${progress} %`;
        }
    });

    fileUploader.upload();

    e.target.value = [];
});

const sizeRound = document.querySelector('.octet .size-round');

document.querySelector('.octet .chunk-size')
.addEventListener('keyup', (e) => {
    const value = e.target.value;

    if(!value) {
        sizeRound.innerHTML = '';
        return;
    }

    sizeRound.innerHTML = formatBytes(parseInt(value), 2);
});


function formatBytes(bytes, decimals = 2) {
    if (bytes === 0) return '0 Bytes';

    const k = 1024;
    const dm = decimals < 0 ? 0 : decimals;
    const sizes = ['Bytes', 'KB', 'MB', 'GB', 'TB', 'PB', 'EB', 'ZB', 'YB'];

    const i = Math.floor(Math.log(bytes) / Math.log(k));

    return parseFloat((bytes / Math.pow(k, i)).toFixed(dm)) + ' ' + sizes[i];
}

