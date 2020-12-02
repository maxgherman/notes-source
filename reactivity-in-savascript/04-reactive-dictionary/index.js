import { Dependency } from './dependency';

const ReactiveDictionary = {
    get(key) {
        let dependency = this.keyDependencies[key];
        if (!dependency) {
            dependency = Dependency.create();
            this.keyDependencies[key] = dependency;
        }
        dependency.depend();
        return this.keys[key];
    },
    set(key, value) {
        this.keys[key] = value;
        let dependency = this.keyDependencies[key];
        if (dependency) {
            dependency.change();
        }
    },
    create() {
        return Object.assign({}, ReactiveDictionary, {
            keyDependencies: {},
            keys: {}
        });
    }
}

let reactiveDictionary = ReactiveDictionary.create();

function action() {
    let value = reactiveDictionary.get('test');
    let placeholder = document.getElementById('placeholder');
    if(placeholder) {
        placeholder.innerHTML = value;
    }
}

function action2() {
    let value = reactiveDictionary.get('test 2');
    let placeholder = document.getElementById('placeholder2');
    if(placeholder) {
        placeholder.innerHTML = value;
    }
}

Dependency.action(action);
Dependency.action(action2);

function run() {
    [...Array(10).keys()].forEach(item => {
        setTimeout(() => {
            if (item < 5)
                reactiveDictionary.set('test', 'RD value ' + item);
            else
                reactiveDictionary.set('test 2', 'RD value ' + item);
        }, item * 1000);
    });
}

run();