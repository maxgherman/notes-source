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

// Validation state containing errors and context
public class ValidationState
{
    public List<string> Errors { get; set; }
    public Dictionary<string, string> Context { get; set; }

    public ValidationState(List<string>? errors = null, Dictionary<string, string>? context = null)
    {
        Errors = errors ?? new List<string>();
        Context = context ?? new Dictionary<string, string>();
    }

    public override string ToString()
    {
        return $"ValidationState(Errors: [{string.Join(", ", Errors.Select(e => $"\"{e}\""))}], " +
               $"Context: {{{string.Join(", ", Context.Select(kv => $"{kv.Key}: \"{kv.Value}\""))}}})";
    }
}

public static class Program
{
    // Validate that a field is required (not empty)
    public static State<ValidationState, bool> ValidateRequired(string field, string value)
    {
        return from state in State<ValidationState, ValidationState>.Get()
               let isEmpty = string.IsNullOrWhiteSpace(value)
               let newState = isEmpty
                   ? new ValidationState(
                       state.Errors.Concat(new[] { $"{field} is required" }).ToList(),
                       new Dictionary<string, string>(state.Context))
                   : new ValidationState(
                       new List<string>(state.Errors),
                       new Dictionary<string, string>(state.Context) { [field] = value })
               from _ in State<ValidationState, Unit>.Put(newState)
               select !isEmpty;
    }

    // Validate email format
    public static State<ValidationState, bool> ValidateEmail(string email)
    {
        return from state in State<ValidationState, ValidationState>.Get()
               let isValid = !string.IsNullOrEmpty(email) && email.Contains("@") && email.Contains(".")
               let newState = isValid
                   ? state
                   : new ValidationState(
                       state.Errors.Concat(new[] { "Invalid email format" }).ToList(),
                       new Dictionary<string, string>(state.Context))
               from _ in State<ValidationState, Unit>.Put(newState)
               select isValid;
    }

    // Validate a complete user
    public static State<ValidationState, bool> ValidateUser(string name, string email)
    {
        return from nameValid in ValidateRequired("name", name)
               from emailValid in ValidateRequired("email", email)
               from emailFormatValid in emailValid ? ValidateEmail(email) : State<ValidationState, bool>.Pure(false)
               select nameValid && emailValid && emailFormatValid;
    }

    // Usage example
    public static void Main()
    {
        var initialState = new ValidationState();
        var (isValid, finalState) = ValidateUser("John", "john@example.com").Run(initialState);

        Console.WriteLine($"Is valid: {isValid}");
        Console.WriteLine($"Final state: {finalState}");
    }
}
