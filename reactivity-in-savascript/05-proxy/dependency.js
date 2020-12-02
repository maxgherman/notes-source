import { ComputeItem } from './compute-item';

export const Dependency = {
    depend() {
        let current = ComputeItem.currentComputation;

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