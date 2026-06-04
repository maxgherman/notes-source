using System;

// Type definition for Maybe
public class Maybe<T>
{
    public T? Value { get; }
    public bool HasValue { get; }

    public Maybe(T? value)
    {
        Value = value;
        HasValue = value != null;
    }

    public static Maybe<T> Pure(T value) => new Maybe<T>(value);

    public static Maybe<T> None() => new Maybe<T>(default(T));

    public Maybe<U> Bind<U>(Func<T, Maybe<U>> f)
    {
        return HasValue ? f(Value!) : Maybe<U>.None();
    }

    // Required for LINQ query syntax
    public Maybe<U> SelectMany<U>(Func<T, Maybe<U>> f)
    {
        return Bind(f);
    }

    // Required for LINQ query syntax with projection
    public Maybe<V> SelectMany<U, V>(Func<T, Maybe<U>> f, Func<T, U, V> projection)
    {
        return Bind(x => f(x).Bind(y => Maybe<V>.Pure(projection(x, y))));
    }
}

class Program
{
    static Maybe<int> SafeDivide(int x, int y)
    {
        return y == 0 ? Maybe<int>.None() : Maybe<int>.Pure(x / y);
    }

    static void Main()
    {
        // Option 1: LINQ query syntax (natural do-notation)
        var linqResult =
            from x in Maybe<int>.Pure(20)
            from y in SafeDivide(x, 4)
            from z in SafeDivide(y, 2)
            select z * 3;

        Console.WriteLine($"LINQ result: {(linqResult.HasValue ? linqResult.Value.ToString() : "null")}");
        // Output: LINQ result: 6

    }
}
