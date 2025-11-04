// Self-hom-set Hom(number[], number[])
type ArrayTransform = (arr: number[]) => number[];

const sort: ArrayTransform = (arr) => [...arr].sort((a, b) => a - b);
const reverse: ArrayTransform = (arr) => [...arr].reverse();
const double: ArrayTransform = (arr) => arr.map(x => x * 2);
const identity: ArrayTransform = (arr) => arr;  // id_number[]

// Object state transformations - Hom(State, State)
interface State {
  count: number;
  message: string;
}

type StateTransform = (state: State) => State;

const incrementCount: StateTransform = (state) => ({
  ...state,
  count: state.count + 1
});

const updateMessage: StateTransform = (state) => ({
  ...state,
  message: "Updated!"
});

const reset: StateTransform = (state) => ({
  count: 0,
  message: ""
});

// Composition of state transformations
const incrementAndUpdate: StateTransform = (state) =>
  updateMessage(incrementCount(state));
