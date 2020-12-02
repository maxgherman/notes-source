const ComputeItem = {
    id: 0,
    pendingComputations: [],
    currentComputation: null,

    invalidate() {
        ComputeItem.pendingComputations.push(this);

        if (this.computation) {
            this.computation();
        }

        this.requireFlush();
    },

    onInvalidate(f) {
        this.computation = f;
    },

    requireFlush() {
        setTimeout(() => {
            while (ComputeItem.pendingComputations.length) {
                ComputeItem.pendingComputations.shift().compute();
            }
        }, 0);
    },

    compute() {
        const current = ComputeItem.currentComputation;
        ComputeItem.currentComputation = this;
        this.action();
        ComputeItem.currentComputation = current;
    },

    create(execute) {
        const result = Object.assign({}, ComputeItem,
            {
                computation: null,
                id: ComputeItem.id++,
                action: execute
            });

        ComputeItem.currentComputation = result;
        return result;
    }
}

const Dependency = {
    depend() {
        const current = ComputeItem.currentComputation;

        if (!current) return;

        this.computations[current.id] = current;

        current.onInvalidate(() => {
            delete this.computations[current.id];
        });
    },

    change() {
        for (let key in this.computations) {
            this.computations[key].invalidate();
        }
    },

    action(execute) {
        const computation = ComputeItem.create(execute);
        computation.compute();
    },

    create() {
        return Object.assign({}, Dependency, { computations: {} });
    }
}

let state = 'apple';
const dependency = Dependency.create();

function getter() {
    dependency.depend();
    return state;
}

function getter2() {
    let value = getter().split('').reverse().join('');
    return value;
}

function setter(value) {
    if (state === value) return;

    state = value;
    dependency.change();
}

function action() {
    let value = getter();
    let placeholder = document.getElementById('placeholder');
    if(placeholder) {
        placeholder.innerHTML = value;
    }
}

function action2() {
    let value = getter2();
    let placeholder = document.getElementById('placeholder2');
    if(placeholder) {
        placeholder.innerHTML = value;
    }
}

Dependency.action(action2);
Dependency.action(action);

function run() {
    [...Array(6).keys()].forEach(item => {
        setTimeout(() => setter('banana ' + item), (item + 1) * 1000);
    });
}

run();