// Circuit type mimicking Arrow behavior
interface Circuit<A, B> {
  run: (input: A) => B;
}

// Helper function to create circuits (similar to 'arr')
function circuit<A, B>(f: (input: A) => B): Circuit<A, B> {
  return { run: f };
}

// Function composition operator (similar to '>>>')
function compose<A, B, C>(
  first: Circuit<A, B>,
  second: Circuit<B, C>
): Circuit<A, C> {
  return circuit(input => second.run(first.run(input)));
}

// Parallel composition (similar to '***')
function parallel<A, B, C, D>(
  left: Circuit<A, B>,
  right: Circuit<C, D>
): Circuit<[A, C], [B, D]> {
  return circuit(([a, c]) => [left.run(a), right.run(c)]);
}

// Basic circuit components
const amplify = (gain: number): Circuit<number, number> =>
  circuit(input => input * gain);

const delay: Circuit<number, number> =
  circuit(input => input); // Simplified delay

const signalFilter = (cutoff: number): Circuit<number, number> =>
  circuit(input => Math.abs(input) > cutoff ? input : 0);

// Complex circuits using composition
const parallelAmplifiers: Circuit<[number, number], [number, number]> =
  parallel(amplify(2.0), amplify(3.0));

const serialProcessing: Circuit<number, number> =
  compose(
    compose(amplify(1.5), signalFilter(0.1)),
    delay
  );

// Branching and rejoining signals
const splitterMixer: Circuit<number, number> = circuit(input => {
  const amplified = amplify(2.0).run(input);
  const filtered = signalFilter(0.5).run(input);
  return amplified + filtered;
});

// Multi-stage processing pipeline
const audioProcessor: Circuit<number, number> = circuit(signal => {
  // Stage 1: Pre-amplification
  const preamp = amplify(1.2).run(signal);

  // Stage 2: Parallel processing
  const [highGain, lowGain] = parallelAmplifiers.run([preamp, preamp]);

  // Stage 3: Filtering
  const filtered = signalFilter(0.3).run(highGain);

  // Stage 4: Mix and final amplification
  const mixed = filtered + lowGain;
  const final = amplify(0.9).run(mixed);

  return final;
});

// Example usage
function testCircuits() {
  console.log("Testing audio processor:");
  console.log(audioProcessor.run(1.0));  // 4.104

  console.log("Testing serial processing:");
  console.log(serialProcessing.run(2.5)); // 3.75

  console.log("Testing parallel amplifiers:");
  console.log(parallelAmplifiers.run([1.0, 2.0])); // [2.0, 6.0]

  console.log("Testing splitter mixer:");
  console.log(splitterMixer.run(1.5)); // 4.5
}

// Advanced: Arrow-like operators using fluent interface
class CircuitBuilder<A, B> {
  constructor(private circuit: Circuit<A, B>) {}

  // Composition operator
  then<C>(next: Circuit<B, C>): CircuitBuilder<A, C> {
    return new CircuitBuilder(compose(this.circuit, next));
  }

  // Parallel composition with another circuit
  and<C, D>(other: Circuit<C, D>): CircuitBuilder<[A, C], [B, D]> {
    return new CircuitBuilder(parallel(this.circuit, other));
  }

  // Run the circuit
  run(input: A): B {
    return this.circuit.run(input);
  }
}

// Factory function for fluent interface
function from<A, B>(f: (input: A) => B): CircuitBuilder<A, B> {
  return new CircuitBuilder(circuit(f));
}

// Fluent interface example
const fluentProcessor = from((x: number) => x * 1.2)
  .then(circuit(x => x > 0.3 ? x : 0))
  .then(circuit(x => x * 0.9));

console.log("Fluent processor:", fluentProcessor.run(1.0)); // 1.08
