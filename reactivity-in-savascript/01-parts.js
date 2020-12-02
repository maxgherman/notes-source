let state = 'state';

function getter() {
    return state;
}

function setter(value) {
    state = value;
}

function action() {
    const value = getter();
    document.getElementById('placeholder').innerHTML = value;
}