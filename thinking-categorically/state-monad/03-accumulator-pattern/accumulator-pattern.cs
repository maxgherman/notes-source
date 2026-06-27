using System;
using System.Collections.Generic;
using System.Linq;

// State monad implementation for the accumulator pattern
public class State<S, A>
{
    private readonly Func<S, (A Result, S NewState)> computation;

    public State(Func<S, (A, S)> computation)
    {
        this.computation = computation;
    }

    public (A Result, S NewState) Run(S initialState)
    {
        return computation(initialState);
    }

    public State<S, B> Map<B>(Func<A, B> f)
    {
        return new State<S, B>(s =>
        {
            var (result, newState) = computation(s);
            return (f(result), newState);
        });
    }

    public State<S, B> FlatMap<B>(Func<A, State<S, B>> f)
    {
        return new State<S, B>(s =>
        {
            var (result, newState) = computation(s);
            return f(result).Run(newState);
        });
    }

    // LINQ Support
    public State<S, B> SelectMany<B>(Func<A, State<S, B>> f)
    {
        return FlatMap(f);
    }

    public State<S, C> SelectMany<B, C>(Func<A, State<S, B>> f, Func<A, B, C> projection)
    {
        return FlatMap(a => f(a).Map(b => projection(a, b)));
    }

    public State<S, B> Select<B>(Func<A, B> f)
    {
        return Map(f);
    }

    public static State<S, A> Pure(A value)
    {
        return new State<S, A>(s => (value, s));
    }

    public static State<S, S> Get()
    {
        return new State<S, S>(s => (s, s));
    }

    public static State<S, Unit> Put(S newState)
    {
        return new State<S, Unit>(_ => (Unit.Default, newState));
    }
}

// Unit type for void-like operations
public struct Unit
{
    public static readonly Unit Default = new Unit();
}

// Accumulator state
public class AccumulatorState
{
    public int Sum { get; set; }
    public int Count { get; set; }

    public AccumulatorState(int sum = 0, int count = 0)
    {
        Sum = sum;
        Count = count;
    }

    public override string ToString() => $"AccumulatorState(Sum: {Sum}, Count: {Count})";
}

public static class AccumulatorExample
{
    // Sum with count using State monad
    public static State<AccumulatorState, int> SumWithCount(IEnumerable<int> numbers)
    {
        var numbersList = numbers.ToList();

        if (!numbersList.Any())
        {
            return State<AccumulatorState, AccumulatorState>.Get()
                .Select(state => state.Sum);
        }

        var head = numbersList.First();
        var tail = numbersList.Skip(1);

        return from state in State<AccumulatorState, AccumulatorState>.Get()
               from _ in State<AccumulatorState, Unit>.Put(new AccumulatorState(state.Sum + head, state.Count + 1))
               from result in SumWithCount(tail)
               select result;
    }

    // Usage example
    public static void Main()
    {
        var numbers = new[] { 1, 2, 3, 4, 5 };
        var initialState = new AccumulatorState();
        var (result, finalState) = SumWithCount(numbers).Run(initialState);

        Console.WriteLine($"Sum: {result}");
        Console.WriteLine($"Final state: {finalState}");
        // Output: Sum: 15, Final state: AccumulatorState(Sum: 15, Count: 5)
    }
}
