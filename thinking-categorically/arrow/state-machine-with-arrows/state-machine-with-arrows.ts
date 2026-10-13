// State machine type mimicking Arrow behavior
type StateMachine<S, A, B> = {
  run: (state: S, input: A) => { result: B; newState: S };
};

// Helper to create state machines
function stateMachine<S, A, B>(f: (state: S, input: A) => { result: B; newState: S }): StateMachine<S, A, B> {
  return { run: f };
}

// Identity state machine
function idStateMachine<S, A>(): StateMachine<S, A, A> {
  return stateMachine((state, input) => ({ result: input, newState: state }));
}

// Composition
function composeStateMachine<S, A, B, C>(
  first: StateMachine<S, A, B>,
  second: StateMachine<S, B, C>
): StateMachine<S, A, C> {
  return stateMachine((state, input) => {
    const { result: b, newState: s1 } = first.run(state, input);
    const { result: c, newState: s2 } = second.run(s1, b);
    return { result: c, newState: s2 };
  });
}

// Arrow-like combinators
function arrStateMachine<S, A, B>(f: (a: A) => B): StateMachine<S, A, B> {
  return stateMachine((state, input) => ({ result: f(input), newState: state }));
}

function firstStateMachine<S, A, B, C>(
  sm: StateMachine<S, A, B>
): StateMachine<S, [A, C], [B, C]> {
  return stateMachine((state, [a, c]) => {
    const { result: b, newState: s1 } = sm.run(state, a);
    return { result: [b, c], newState: s1 };
  });
}

// State machine primitives
function getState<S, A>(): StateMachine<S, A, S> {
  return stateMachine((state, _) => ({ result: state, newState: state }));
}

function setState<S>(): StateMachine<S, S, void> {
  return stateMachine((_, newState) => ({ result: undefined, newState }));
}

function modifyState<S, A>(f: (s: S) => S): StateMachine<S, A, void> {
  return stateMachine((state, _) => ({ result: undefined, newState: f(state) }));
}

// Traffic light state machine
type LightState = "Red" | "Yellow" | "Green";
type LightInput = "TimerTick" | "EmergencyStop";

function transition([state, input]: [LightState, LightInput]): LightState {
  if (input === "EmergencyStop") return "Red";
  if (state === "Red" && input === "TimerTick") return "Green";
  if (state === "Green" && input === "TimerTick") return "Yellow";
  if (state === "Yellow" && input === "TimerTick") return "Red";
  return state;
}

const trafficLight: StateMachine<LightState, LightInput, LightState> = stateMachine((state, input) => {
  const newState = transition([state, input]);
  return { result: newState, newState };
});

// Counter with reset functionality
const counterMachine: StateMachine<number, string, number> = stateMachine((state, command) => {
  let newState = state;
  if (command === "inc") newState += 1;
  else if (command === "dec") newState -= 1;
  else if (command === "reset") newState = 0;
  return { result: newState, newState };
});

// Example usage
function testStateMachines() {
  // Counter
  console.log("Counter:");
  let s = 0;
  let r = counterMachine.run(s, "inc"); console.log(r); s = r.newState;
  r = counterMachine.run(s, "dec"); console.log(r); s = r.newState;
  r = counterMachine.run(s, "reset"); console.log(r);

  // Traffic light
  console.log("\nTraffic Light:");
  let t: LightState = "Red";
  let tl = trafficLight.run(t, "TimerTick"); console.log(tl); t = tl.newState;
  tl = trafficLight.run(t, "TimerTick"); console.log(tl); t = tl.newState;
  tl = trafficLight.run(t, "EmergencyStop"); console.log(tl);
}

testStateMachines();
