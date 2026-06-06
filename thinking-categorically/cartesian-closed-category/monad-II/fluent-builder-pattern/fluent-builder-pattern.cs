using System;
using System.Collections.Generic;
using System.Linq;

// Maybe type definition for C#
public class Maybe<T>
{
    private readonly T? value;
    private readonly bool hasValue;

    private Maybe(T? value, bool hasValue)
    {
        this.value = value;
        this.hasValue = hasValue;
    }

    public static Maybe<T> Pure(T value) => new Maybe<T>(value, true);
    public static Maybe<T> None() => new Maybe<T>(default(T), false);

    public Maybe<U> Bind<U>(Func<T, Maybe<U>> f)
    {
        return hasValue && value != null ? f(value) : Maybe<U>.None();
    }

    public Maybe<U> Map<U>(Func<T, U> f)
    {
        return hasValue && value != null ? Maybe<U>.Pure(f(value)) : Maybe<U>.None();
    }

    // LINQ Support - Required for query syntax
    public Maybe<U> SelectMany<U>(Func<T, Maybe<U>> selector)
    {
        return Bind(selector);
    }

    public Maybe<V> SelectMany<U, V>(Func<T, Maybe<U>> selector, Func<T, U, V> resultSelector)
    {
        return Bind(x => selector(x).Bind(y => Maybe<V>.Pure(resultSelector(x, y))));
    }

    public Maybe<U> Select<U>(Func<T, U> selector)
    {
        return Map(selector);
    }

    public bool HasValue => hasValue && value != null;
    public T? Value => hasValue ? value : default(T);
    public bool IsNone => !hasValue || value == null;

    public override string ToString()
    {
        return HasValue ? $"Some({Value})" : "None";
    }

    public override bool Equals(object? obj)
    {
        if (obj is Maybe<T> other)
        {
            if (!HasValue && !other.HasValue) return true;
            if (HasValue && other.HasValue) return EqualityComparer<T>.Default.Equals(Value, other.Value);
        }
        return false;
    }

    public override int GetHashCode()
    {
        return HasValue ? EqualityComparer<T>.Default.GetHashCode(Value!) : 0;
    }
}

// Helper functions for Maybe monad
public static class MaybeHelpers
{
    public static Maybe<T> Pure<T>(T value) => Maybe<T>.Pure(value);

    public static Maybe<U> FlatMap<T, U>(Maybe<T> ma, Func<T, Maybe<U>> f)
    {
        return ma.Bind(f);
    }

    public static Maybe<double> SafeDiv(double x, double y)
    {
        return y == 0 ? Maybe<double>.None() : Maybe<double>.Pure(x / y);
    }

    public static Maybe<double> ParseNumber(string str)
    {
        return double.TryParse(str, out var result) ?
               Maybe<double>.Pure(result) :
               Maybe<double>.None();
    }
}

// Fluent builder pattern for monadic computations
public class DoBuilder<T>
{
    private readonly Maybe<T> value;

    private DoBuilder(Maybe<T> value)
    {
        this.value = value;
    }

    public static DoBuilder<T> From(Maybe<T> value)
    {
        return new DoBuilder<T>(value);
    }

    public static DoBuilder<T> Pure(T value)
    {
        return new DoBuilder<T>(Maybe<T>.Pure(value));
    }

    public DoBuilder<U> Bind<U>(Func<T, Maybe<U>> f)
    {
        return new DoBuilder<U>(value.Bind(f));
    }

    public DoBuilder<U> Map<U>(Func<T, U> f)
    {
        return new DoBuilder<U>(value.Map(f));
    }

    public Maybe<T> Run()
    {
        return value;
    }

    // Helper methods
    public bool IsNone => value.IsNone;
    public T? GetValue() => value.Value;
}

// Builder pattern with error context
public class DoBuilderWithError<T>
{
    private readonly Maybe<T> value;
    private readonly List<string> errorContext;

    private DoBuilderWithError(Maybe<T> value, List<string>? errorContext = null)
    {
        this.value = value;
        this.errorContext = errorContext ?? new List<string>();
    }

    public static DoBuilderWithError<T> Pure(T value)
    {
        return new DoBuilderWithError<T>(Maybe<T>.Pure(value));
    }

    public DoBuilderWithError<U> Bind<U>(Func<T, Maybe<U>> f, string? errorMsg = null)
    {
        if (value.IsNone)
        {
            return new DoBuilderWithError<U>(Maybe<U>.None(), errorContext);
        }

        var result = f(value.Value!);
        var newContext = new List<string>(errorContext);

        if (result.IsNone && !string.IsNullOrEmpty(errorMsg))
        {
            newContext.Add(errorMsg);
        }

        return new DoBuilderWithError<U>(result, newContext);
    }

    public DoBuilderWithError<U> Map<U>(Func<T, U> f)
    {
        var newValue = value.HasValue ? Maybe<U>.Pure(f(value.Value!)) : Maybe<U>.None();
        return new DoBuilderWithError<U>(newValue, errorContext);
    }

    public (Maybe<T> Value, List<string> Errors) Run()
    {
        return (value, new List<string>(errorContext));
    }
}

class Program
{
    static void Main()
    {
        // Usage examples:
        var builderExample1 = DoBuilder<double>
            .Pure(20.0)
            .Bind(x => MaybeHelpers.SafeDiv(x, 4))
            .Bind(y => MaybeHelpers.SafeDiv(y, 2))
            .Map(z => z * 3)
            .Run(); // Result: 7.5

        var builderExample2 = DoBuilder<double>
            .Pure(20.0)
            .Bind(x => MaybeHelpers.SafeDiv(x, 0)) // This will fail
            .Bind(y => MaybeHelpers.SafeDiv(y, 2))
            .Map(z => z * 3)
            .Run(); // Result: None

        // More complex example with string parsing
        var complexCalculation = DoBuilder<string>
            .From(MaybeHelpers.ParseNumber("10").Map(x => x.ToString()))
            .Bind(x => MaybeHelpers.ParseNumber("2").Map(y => $"Division of {x} by {y}"))
            .Bind(desc => MaybeHelpers.SafeDiv(10, 2).Map(result => $"{desc} = {result}"))
            .Run();

        Console.WriteLine($"Example 1: {builderExample1}"); // Some(7.5)
        Console.WriteLine($"Example 2: {builderExample2}"); // None
        Console.WriteLine($"Complex: {complexCalculation}"); // Some(Division of 10 by 2 = 5)

        // Usage with error tracking:
        var resultWithErrors = DoBuilderWithError<double>
            .Pure(10.0)
            .Bind(x => MaybeHelpers.SafeDiv(x, 0), "Division by zero in step 1")
            .Bind(y => MaybeHelpers.SafeDiv(y, 2), "Division failed in step 2")
            .Map(z => z * 3)
            .Run();

        Console.WriteLine($"Result with errors: Value = {resultWithErrors.Value}, " +
                         $"Errors = [{string.Join(", ", resultWithErrors.Errors.Select(e => $"\"{e}\""))}]");
        // Result with errors: Value = None, Errors = ["Division by zero in step 1"]

        // LINQ-style usage with proper SelectMany support
        var linqResult =
            from x in Maybe<double>.Pure(20.0)
            from y in MaybeHelpers.SafeDiv(x, 4)
            from z in MaybeHelpers.SafeDiv(y, 2)
            select z * 3;

        Console.WriteLine($"LINQ result: {linqResult}"); // Some(7.5)

        // Demonstrating error propagation with LINQ
        var linqErrorResult =
            from x in Maybe<double>.Pure(10.0)
            from y in MaybeHelpers.SafeDiv(x, 0) // This fails
            from z in MaybeHelpers.SafeDiv(y, 2)
            select z * 3;

        Console.WriteLine($"LINQ error result: {linqErrorResult}"); // None

        // Complex chaining example
        var chainedResult = DoBuilder<double>
            .Pure(100.0)
            .Bind(x => MaybeHelpers.SafeDiv(x, 5))    // 20
            .Bind(y => MaybeHelpers.SafeDiv(y, 4))    // 5
            .Bind(z => MaybeHelpers.SafeDiv(z, 1))    // 5
            .Map(final => final * 2)                   // 10
            .Run();

        Console.WriteLine($"Chained result: {chainedResult}"); // Some(10)
    }
}
