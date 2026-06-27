using System;
using System.Linq;
using System.Text.RegularExpressions;

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

// Parser state containing input and position
public class ParseState
{
    public string Input { get; set; }
    public int Position { get; set; }

    public ParseState(string input, int position = 0)
    {
        Input = input;
        Position = position;
    }

    public override string ToString() => $"ParseState(Input: \"{Input}\", Position: {Position})";
}

public static class Program
{
    // Parse a specific character
    public static State<ParseState, string?> ParseChar(char expected)
    {
        return from state in State<ParseState, ParseState>.Get()
               let remaining = state.Input.Substring(Math.Min(state.Position, state.Input.Length))
               let success = remaining.Length > 0 && remaining[0] == expected
               from _ in success ? State<ParseState, Unit>.Put(new ParseState(state.Input, state.Position + 1))
                                 : State<ParseState, Unit>.Put(state)
               select success ? expected.ToString() : null;
    }

    // Parse a string by parsing each character
    public static State<ParseState, string?> ParseString(string str)
    {
        if (string.IsNullOrEmpty(str))
        {
            return State<ParseState, string?>.Pure(str);
        }

        var head = str[0];
        var tail = str.Substring(1);

        return from charResult in ParseChar(head)
               from tailResult in charResult != null ? ParseString(tail) : State<ParseState, string?>.Pure(null)
               select charResult != null && tailResult != null ? str : null;
    }

    // Parse a number (simplified)
    public static State<ParseState, int?> ParseNumber()
    {
        return from state in State<ParseState, ParseState>.Get()
               let remaining = state.Input.Substring(Math.Min(state.Position, state.Input.Length))
               let match = Regex.Match(remaining, @"^\d+")
               let success = match.Success
               from _ in success ? State<ParseState, Unit>.Put(new ParseState(state.Input, state.Position + match.Length))
                                 : State<ParseState, Unit>.Put(state)
               select success ? (int?)int.Parse(match.Value) : null;
    }

    // Usage example
    public static void Main()
    {
        var initialState = new ParseState("hello123world");

        var parser = from hello in ParseString("hello")
                     from number in ParseNumber()
                     from world in ParseString("world")
                     select new { Hello = hello, Number = number, World = world };

        var (results, finalState) = parser.Run(initialState);

        Console.WriteLine($"Parse results: Hello={results.Hello}, Number={results.Number}, World={results.World}");
        Console.WriteLine($"Final state: {finalState}");
        // Output: Parse results: Hello=hello, Number=123, World=world
    }
}
