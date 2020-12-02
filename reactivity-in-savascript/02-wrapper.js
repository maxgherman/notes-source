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

const ComputeItemNaive = {
    add(action) {
        this.actions.push(action);
    },
    wrap(value) {
        return function () {
            value(...arguments);
            this.actions.forEach(item => {
                item();
            });
        }.bind(this);
    },
    create() {
        return Object.assign({}, ComputeItemNaive, { actions: [] });
    }
}

function run() {
    const computeItem = ComputeItemNaive.create();
    computeItem.add(action);

    const wrapper = computeItem.wrap(setter);

    [...Array(6).keys()].forEach(item => {
        setTimeout(() => wrapper('state ' + item), item * 1000);
    });
}

run();