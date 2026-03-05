using System;

// Arrow interface for composable computations
public interface IArrow<A, B>
{
    B Run(A input);
    IArrow<A, C> Compose<C>(IArrow<B, C> other);
    IArrow<Tuple<A, D>, Tuple<B, D>> First<D>();
    IArrow<Tuple<D, A>, Tuple<D, B>> Second<D>();
}

// Basic function arrow implementation
public class FunctionArrow<A, B> : IArrow<A, B>
{
    private readonly Func<A, B> function;

    public FunctionArrow(Func<A, B> function)
    {
        this.function = function;
    }

    public B Run(A input)
    {
        return function(input);
    }

    public IArrow<A, C> Compose<C>(IArrow<B, C> other)
    {
        return new FunctionArrow<A, C>(a => other.Run(this.Run(a)));
    }

    public IArrow<Tuple<A, D>, Tuple<B, D>> First<D>()
    {
        return new FunctionArrow<Tuple<A, D>, Tuple<B, D>>(
            tuple => Tuple.Create(this.Run(tuple.Item1), tuple.Item2));
    }

    public IArrow<Tuple<D, A>, Tuple<D, B>> Second<D>()
    {
        return new FunctionArrow<Tuple<D, A>, Tuple<D, B>>(
            tuple => Tuple.Create(tuple.Item1, this.Run(tuple.Item2)));
    }
}

// Helper class for creating arrows
public static class Arrow
{
    public static IArrow<A, B> Lift<A, B>(Func<A, B> function)
    {
        return new FunctionArrow<A, B>(function);
    }

    public static IArrow<A, A> Identity<A>()
    {
        return new FunctionArrow<A, A>(x => x);
    }
}

// Maybe arrow for handling nullable computations
public class MaybeArrow<A, B> : IArrow<A, B>
    where B : class?
{
    private readonly Func<A, B> function;

    public MaybeArrow(Func<A, B> function)
    {
        this.function = function;
    }

    public B Run(A input)
    {
        return function(input);
    }

    public IArrow<A, C> Compose<C>(IArrow<B, C> other)
    {
        return new FunctionArrow<A, C>(a =>
        {
            var result = this.Run(a);
            return result == null ? default(C)! : other.Run(result);
        });
    }

    public IArrow<Tuple<A, D>, Tuple<B, D>> First<D>()
    {
        return new FunctionArrow<Tuple<A, D>, Tuple<B, D>>(
            tuple => Tuple.Create(this.Run(tuple.Item1), tuple.Item2));
    }

    public IArrow<Tuple<D, A>, Tuple<D, B>> Second<D>()
    {
        return new FunctionArrow<Tuple<D, A>, Tuple<D, B>>(
            tuple => Tuple.Create(tuple.Item1, this.Run(tuple.Item2)));
    }
}

// Logging arrow that tracks computation steps
public class LoggingArrow<A, B> : IArrow<A, B>
{
    private readonly Func<A, B> function;
    private readonly string description;

    public LoggingArrow(Func<A, B> function, string description)
    {
        this.function = function;
        this.description = description;
    }

    public B Run(A input)
    {
        Console.WriteLine($"Executing: {description} with input {input}");
        var result = function(input);
        Console.WriteLine($"Result: {result}");
        return result;
    }

    public IArrow<A, C> Compose<C>(IArrow<B, C> other)
    {
        return new FunctionArrow<A, C>(a => other.Run(this.Run(a)));
    }

    public IArrow<Tuple<A, D>, Tuple<B, D>> First<D>()
    {
        return new FunctionArrow<Tuple<A, D>, Tuple<B, D>>(
            tuple => Tuple.Create(this.Run(tuple.Item1), tuple.Item2));
    }

    public IArrow<Tuple<D, A>, Tuple<D, B>> Second<D>()
    {
        return new FunctionArrow<Tuple<D, A>, Tuple<D, B>>(
            tuple => Tuple.Create(tuple.Item1, this.Run(tuple.Item2)));
    }
}

// State arrow for stateful computations
public class StateArrow<S, A, B> : IArrow<A, B>
{
    private readonly Func<S, A, Tuple<S, B>> function;
    private readonly S initialState;

    public StateArrow(Func<S, A, Tuple<S, B>> function, S initialState)
    {
        this.function = function;
        this.initialState = initialState;
    }

    public B Run(A input)
    {
        return RunWithState(initialState, input).Item2;
    }

    public Tuple<S, B> RunWithState(S state, A input)
    {
        return function(state, input);
    }

    public IArrow<A, C> Compose<C>(IArrow<B, C> other)
    {
        return new FunctionArrow<A, C>(a => other.Run(this.Run(a)));
    }

    public IArrow<Tuple<A, D>, Tuple<B, D>> First<D>()
    {
        return new FunctionArrow<Tuple<A, D>, Tuple<B, D>>(
            tuple => Tuple.Create(this.Run(tuple.Item1), tuple.Item2));
    }

    public IArrow<Tuple<D, A>, Tuple<D, B>> Second<D>()
    {
        return new FunctionArrow<Tuple<D, A>, Tuple<D, B>>(
            tuple => Tuple.Create(tuple.Item1, this.Run(tuple.Item2)));
    }
}

class Program
{
    static void BasicArrowExamples()
    {
        Console.WriteLine("=== Basic Arrow Examples ===");

        var addOne = Arrow.Lift<int, int>(x => x + 1);
        var double_ = Arrow.Lift<int, int>(x => x * 2);
        var toString = Arrow.Lift<int, string>(x => x.ToString());

        // Arrow composition
        var pipeline = addOne.Compose(double_).Compose(toString);
        Console.WriteLine(pipeline.Run(5)); // "12"

        // First and second on tuples
        var pairTransform = addOne.First<int>().Compose(double_.Second<int>());
        Console.WriteLine(pairTransform.Run(Tuple.Create(10, 20))); // (11, 40)
    }

    static void MaybeArrowExamples()
    {
        Console.WriteLine("\n=== Maybe Arrow Examples ===");

        var safeDivide = new MaybeArrow<double, string?>(x =>
            x != 0 ? (100 / x).ToString() : null);

        Console.WriteLine(safeDivide.Run(4) ?? "null");  // "25"
        Console.WriteLine(safeDivide.Run(0) ?? "null");  // "null"

        // Chain operations with error handling
        var processNumber = new MaybeArrow<string?, string?>(s =>
            s is null ? null : $"Processed: {s}");

        var pipeline = safeDivide.Compose(processNumber);
        Console.WriteLine(pipeline.Run(4) ?? "null");  // "Processed: 25"
        Console.WriteLine(pipeline.Run(0) ?? "null");  // "null"
    }

    static void LoggingArrowExamples()
    {
        Console.WriteLine("\n=== Logging Arrow Examples ===");

        var addOneWithLog = new LoggingArrow<int, int>(x => x + 1, "add one");
        var doubleWithLog = new LoggingArrow<int, int>(x => x * 2, "double");

        var computation = addOneWithLog.Compose(doubleWithLog);
        var result = computation.Run(5);
        Console.WriteLine($"Final result: {result}");
    }

    static void StateArrowExamples()
    {
        Console.WriteLine("\n=== State Arrow Examples ===");

        // Counter arrow that increments state and adds it to input
        var counter = new StateArrow<int, int, int>(
            (state, input) => Tuple.Create(state + 1, input + state),
            0);

        Console.WriteLine(counter.Run(10)); // 10 (0 + 10)

        // Stateful computation example
        var state = 0;
        var result1 = counter.RunWithState(state, 10);
        Console.WriteLine($"Result: {result1.Item2}, New State: {result1.Item1}"); // Result: 10, State: 1

        var result2 = counter.RunWithState(result1.Item1, 10);
        Console.WriteLine($"Result: {result2.Item2}, New State: {result2.Item1}"); // Result: 11, State: 2
    }

    // Practical example: HTTP request pipeline with error handling
    static void PracticalExample()
    {
        Console.WriteLine("\n=== Practical HTTP Pipeline Example ===");

        var validateUrl = new MaybeArrow<string, string>(url =>
            Uri.TryCreate(url, UriKind.Absolute, out _) ? url : "Invalid URL");

        var makeRequest = new LoggingArrow<string, string>(url =>
            $"Response from {url}", "HTTP request");

        var parseJson = new LoggingArrow<string, string>(response =>
            $"Parsed: {response}", "JSON parsing");

        // Note: In a real implementation, we'd need more sophisticated error handling
        // This demonstrates the concept

        Console.WriteLine("Arrows enable:");
        Console.WriteLine("- Composable computation pipelines");
        Console.WriteLine("- Structure-preserving transformations");
        Console.WriteLine("- Systematic error handling");
        Console.WriteLine("- Reusable computation patterns");
    }

    static void Main(string[] args)
    {
        BasicArrowExamples();
        MaybeArrowExamples();
        LoggingArrowExamples();
        StateArrowExamples();
        PracticalExample();
    }
}