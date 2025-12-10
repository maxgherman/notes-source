using System;
using System.Collections.Generic;
using System.Linq;

// Option/Maybe endofunctor
public class Option<T>
{
    private readonly T value;
    private readonly bool hasValue;

    private Option(T value, bool hasValue)
    {
        this.value = value;
        this.hasValue = hasValue;
    }

    public static Option<T> Some(T value) => new Option<T>(value, true);
    public static Option<T> None() => new Option<T>(default(T)!, false);

    public Option<U> Map<U>(Func<T, U> f)
    {
        return hasValue ? Option<U>.Some(f(value)) : Option<U>.None();
    }

    public override string ToString() =>
        hasValue ? $"Some({value})" : "None";
}

class Program
{
    static void Main()
    {
        // IEnumerable<T> endofunctor - LINQ Select
        var numbers = new List<int> { 1, 2, 3, 4 };
        var strings = numbers.Select(x => x.ToString()); // Still IEnumerable, different element type

        // Usage - all transformations within the same type system
        var opt1 = Option<int>.Some(42).Map(x => x * 2);           // Option<int>
        var opt2 = Option<string>.Some("hello").Map(s => s.Length); // Option<int>
        var opt3 = Option<int>.None().Map(x => x.ToString());       // Option<string>

        Console.WriteLine(opt1); // Some(84)
        Console.WriteLine(opt2); // Some(5)
        Console.WriteLine(opt3); // None

        // Task<T> endofunctor - async transformations
        var task = Task.FromResult(42);
        var transformedTask = task.ContinueWith(t => t.Result.ToString()); // Task<string>
    }
}