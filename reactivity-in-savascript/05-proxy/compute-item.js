export const ComputeItem = {
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
