using System;
using System.Collections.Generic;
using System.Linq;

// Unit type for void-like operations
public struct Unit
{
    public static readonly Unit Default = new Unit();
}

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

// ID generator state
public class IdGenState
{
    public int NextId { get; set; }

    public IdGenState(int nextId = 1)
    {
        NextId = nextId;
    }

    public override string ToString() => $"IdGenState(NextId: {NextId})";
}

public static class Program
{
    // Generate a single unique ID
    public static State<IdGenState, int> GenerateId()
    {
        return from state in State<IdGenState, IdGenState>.Get()
               let currentId = state.NextId
               from _ in State<IdGenState, Unit>.Put(new IdGenState(currentId + 1))
               select currentId;
    }

    // Generate multiple IDs
    public static State<IdGenState, List<int>> GenerateIds(int count)
    {
        if (count <= 0)
        {
            return State<IdGenState, List<int>>.Pure(new List<int>());
        }

        return from id in GenerateId()
               from restIds in GenerateIds(count - 1)
               select new List<int> { id }.Concat(restIds).ToList();
    }

    // Usage example
    public static void Main()
    {
        var initialGen = new IdGenState(1);
        var (ids, finalGen) = GenerateIds(5).Run(initialGen);

        Console.WriteLine($"Generated IDs: [{string.Join(", ", ids)}]");
        Console.WriteLine($"Final generator state: {finalGen}");
        // Output: Generated IDs: [1, 2, 3, 4, 5], Final generator state: IdGenState(NextId: 6)
    }
}
