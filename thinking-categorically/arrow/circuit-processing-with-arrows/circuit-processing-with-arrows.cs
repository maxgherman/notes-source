using System;

// Circuit interface mimicking Arrow behavior
public interface ICircuit<TInput, TOutput>
{
    TOutput Run(TInput input);
}

// Concrete circuit implementation
public class Circuit<TInput, TOutput> : ICircuit<TInput, TOutput>
{
    private readonly Func<TInput, TOutput> function;

    public Circuit(Func<TInput, TOutput> function)
    {
        this.function = function;
    }

    public TOutput Run(TInput input) => function(input);
}

// Static factory and composition methods
public static class Circuit
{
    // Helper to create circuits (similar to 'arr')
    public static ICircuit<TInput, TOutput> From<TInput, TOutput>(
        Func<TInput, TOutput> function)
    {
        return new Circuit<TInput, TOutput>(function);
    }

    // Function composition operator (similar to '>>>')
    public static ICircuit<TInput, TOutput3> Compose<TInput, TOutput1, TOutput3>(
        ICircuit<TInput, TOutput1> first,
        ICircuit<TOutput1, TOutput3> second)
    {
        return From<TInput, TOutput3>(input => second.Run(first.Run(input)));
    }

    // Parallel composition (similar to '***')
    public static ICircuit<(TInput1, TInput2), (TOutput1, TOutput2)> Parallel<TInput1, TInput2, TOutput1, TOutput2>(
        ICircuit<TInput1, TOutput1> left,
        ICircuit<TInput2, TOutput2> right)
    {
        return From<(TInput1, TInput2), (TOutput1, TOutput2)>(
            inputs => (left.Run(inputs.Item1), right.Run(inputs.Item2)));
    }

    // Try the first circuit and use the second one if it fails.
    public static ICircuit<TInput, TOutput> Alternative<TInput, TOutput>(
        ICircuit<TInput, TOutput> first,
        ICircuit<TInput, TOutput> second)
    {
        return From<TInput, TOutput>(input =>
        {
            try
            {
                return first.Run(input);
            }
            catch
            {
                return second.Run(input);
            }
        });
    }
}

// Extension methods for fluent interface
public static class CircuitExtensions
{
    public static ICircuit<TInput, TOutput3> Then<TInput, TOutput1, TOutput3>(
        this ICircuit<TInput, TOutput1> first,
        ICircuit<TOutput1, TOutput3> second)
    {
        return Circuit.Compose(first, second);
    }

    public static ICircuit<(TInput1, TInput2), (TOutput1, TOutput2)> And<TInput1, TInput2, TOutput1, TOutput2>(
        this ICircuit<TInput1, TOutput1> left,
        ICircuit<TInput2, TOutput2> right)
    {
        return Circuit.Parallel(left, right);
    }

    public static ICircuit<TInput, TResult> Map<TInput, TOutput, TResult>(
        this ICircuit<TInput, TOutput> circuit,
        Func<TOutput, TResult> selector)
    {
        return circuit.Then(Circuit.From(selector));
    }

    public static ICircuit<TInput, TOutput> Or<TInput, TOutput>(
        this ICircuit<TInput, TOutput> first,
        ICircuit<TInput, TOutput> second)
    {
        return Circuit.Alternative(first, second);
    }
}

// Basic circuit components
public static class BasicCircuits
{
    public static ICircuit<double, double> Amplify(double gain) =>
        Circuit.From<double, double>(input => input * gain);

    public static ICircuit<double, double> Delay =>
        Circuit.From<double, double>(input => input); // Simplified delay

    public static ICircuit<double, double> SignalFilter(double cutoff) =>
        Circuit.From<double, double>(input => Math.Abs(input) > cutoff ? input : 0);
}

// Example usage class
public class CircuitProcessingExample
{
    public static void Main()
    {
        // Complex circuits using composition
        var parallelAmplifiers = BasicCircuits.Amplify(2.0)
            .And(BasicCircuits.Amplify(3.0));

        var serialProcessing = BasicCircuits.Amplify(1.5)
            .Then(BasicCircuits.SignalFilter(0.1))
            .Then(BasicCircuits.Delay);

        // Branching and rejoining signals
        var splitterMixer = Circuit.From<double, double>(input =>
        {
            var amplified = BasicCircuits.Amplify(2.0).Run(input);
            var filtered = BasicCircuits.SignalFilter(0.5).Run(input);
            return amplified + filtered;
        });

        // Multi-stage processing pipeline
        var audioProcessor = Circuit.From<double, double>(signal =>
        {
            // Stage 1: Pre-amplification
            var preamp = BasicCircuits.Amplify(1.2).Run(signal);

            // Stage 2: Parallel processing
            var (highGain, lowGain) = parallelAmplifiers.Run((preamp, preamp));

            // Stage 3: Filtering
            var filtered = BasicCircuits.SignalFilter(0.3).Run(highGain);

            // Stage 4: Mix and final amplification
            var mixed = filtered + lowGain;
            var final = BasicCircuits.Amplify(0.9).Run(mixed);

            return final;
        });

        // Test the circuits
        TestCircuits(audioProcessor, serialProcessing, parallelAmplifiers, splitterMixer);
    }

    private static void TestCircuits(
        ICircuit<double, double> audioProcessor,
        ICircuit<double, double> serialProcessing,
        ICircuit<(double, double), (double, double)> parallelAmplifiers,
        ICircuit<double, double> splitterMixer)
    {
        Console.WriteLine("Testing audio processor:");
        Console.WriteLine(audioProcessor.Run(1.0));  // 1.512

        Console.WriteLine("Testing serial processing:");
        Console.WriteLine(serialProcessing.Run(2.5)); // 3.75

        Console.WriteLine("Testing parallel amplifiers:");
        var (left, right) = parallelAmplifiers.Run((1.0, 2.0));
        Console.WriteLine($"({left}, {right})"); // (2, 6)

        Console.WriteLine("Testing splitter mixer:");
        Console.WriteLine(splitterMixer.Run(1.5)); // 3.75
    }
}

// Advanced: LINQ-style query syntax for circuits
public static class CircuitLinq
{
    public static ICircuit<TInput, TResult> Select<TInput, TOutput, TResult>(
        this ICircuit<TInput, TOutput> circuit,
        Func<TOutput, TResult> selector)
    {
        return circuit.Then(Circuit.From(selector));
    }

    public static ICircuit<TInput, TResult> SelectMany<TInput, TOutput, TResult>(
        this ICircuit<TInput, TOutput> circuit,
        Func<TOutput, ICircuit<TInput, TResult>> selector)
    {
        return Circuit.From<TInput, TResult>(input =>
        {
            var intermediate = circuit.Run(input);
            return selector(intermediate).Run(input);
        });
    }
}

// LINQ-style example
public class LinqCircuitExample
{
    public static void Example()
    {
        var linqProcessor = BasicCircuits.Amplify(2.0)
            .Select(x => x + 1)
            .Then(BasicCircuits.SignalFilter(1.0))
            .Select(x => x * 0.5);

        Console.WriteLine("LINQ-style processor:", linqProcessor.Run(1.0)); // 0.75
    }
}
