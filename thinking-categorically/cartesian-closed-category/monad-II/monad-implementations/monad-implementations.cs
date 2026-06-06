using System;
using System.Collections.Generic;
using System.Linq;

// Generic Monad interface
public interface IMonad<T>
{
    IMonad<U> Bind<U>(Func<T, IMonad<U>> f);
    IMonad<U> Map<U>(Func<T, U> f);
}

// Maybe Monad Implementation
public abstract class Maybe<T> : IMonad<T>
{
    public abstract IMonad<U> Bind<U>(Func<T, IMonad<U>> f);
    public abstract IMonad<U> Map<U>(Func<T, U> f);
    public abstract bool IsSome { get; }
    public abstract bool IsNone { get; }

    // LINQ Support
    public abstract Maybe<U> SelectMany<U>(Func<T, Maybe<U>> f);
    public abstract Maybe<V> SelectMany<U, V>(Func<T, Maybe<U>> f, Func<T, U, V> projection);
    public abstract Maybe<U> Select<U>(Func<T, U> f);

    public static Maybe<T> Pure(T value) => new Some<T>(value);
    public static Maybe<T> None() => new None<T>();
}

public class Some<T> : Maybe<T>
{
    private readonly T value;

    public Some(T value) { this.value = value; }

    public override IMonad<U> Bind<U>(Func<T, IMonad<U>> f) => f(value);
    public override IMonad<U> Map<U>(Func<T, U> f) => Maybe<U>.Pure(f(value));

    public override Maybe<U> SelectMany<U>(Func<T, Maybe<U>> f) => f(value);

    public override Maybe<V> SelectMany<U, V>(Func<T, Maybe<U>> f, Func<T, U, V> projection)
    {
        return f(value).SelectMany(u => Maybe<V>.Pure(projection(value, u)));
    }

    public override Maybe<U> Select<U>(Func<T, U> f) => Maybe<U>.Pure(f(value));

    public override bool IsSome => true;
    public override bool IsNone => false;

    public T Value => value;
    public override string ToString() => $"Some({value})";
}

public class None<T> : Maybe<T>
{
    public override IMonad<U> Bind<U>(Func<T, IMonad<U>> f) => Maybe<U>.None();
    public override IMonad<U> Map<U>(Func<T, U> f) => Maybe<U>.None();

    public override Maybe<U> SelectMany<U>(Func<T, Maybe<U>> f) => Maybe<U>.None();

    public override Maybe<V> SelectMany<U, V>(Func<T, Maybe<U>> f, Func<T, U, V> projection)
    {
        return Maybe<V>.None();
    }

    public override Maybe<U> Select<U>(Func<T, U> f) => Maybe<U>.None();

    public override bool IsSome => false;
    public override bool IsNone => true;

    public override string ToString() => "None";
}

// Either Monad Implementation
public abstract class Either<L, R> : IMonad<R>
{
    public abstract IMonad<U> Bind<U>(Func<R, IMonad<U>> f);
    public abstract IMonad<U> Map<U>(Func<R, U> f);

    // LINQ Support
    public abstract Either<L, U> SelectMany<U>(Func<R, Either<L, U>> f);
    public abstract Either<L, V> SelectMany<U, V>(Func<R, Either<L, U>> f, Func<R, U, V> projection);
    public abstract Either<L, U> Select<U>(Func<R, U> f);

    public static Either<L, R> Left(L value) => new Left<L, R>(value);
    public static Either<L, R> Right(R value) => new Right<L, R>(value);
}

public class Left<L, R> : Either<L, R>
{
    private readonly L value;

    public Left(L value) { this.value = value; }

    public override IMonad<U> Bind<U>(Func<R, IMonad<U>> f) =>
        new Left<L, U>(value) as IMonad<U>;
    public override IMonad<U> Map<U>(Func<R, U> f) =>
        new Left<L, U>(value) as IMonad<U>;

    public override Either<L, U> SelectMany<U>(Func<R, Either<L, U>> f) =>
        Either<L, U>.Left(value);

    public override Either<L, V> SelectMany<U, V>(Func<R, Either<L, U>> f, Func<R, U, V> projection) =>
        Either<L, V>.Left(value);

    public override Either<L, U> Select<U>(Func<R, U> f) => Either<L, U>.Left(value);

    public L Value => value;
    public override string ToString() => $"Left({value})";
}

public class Right<L, R> : Either<L, R>
{
    private readonly R value;

    public Right(R value) { this.value = value; }

    public override IMonad<U> Bind<U>(Func<R, IMonad<U>> f) => f(value);
    public override IMonad<U> Map<U>(Func<R, U> f) => Either<L, U>.Right(f(value)) as IMonad<U>;

    public override Either<L, U> SelectMany<U>(Func<R, Either<L, U>> f) => f(value);

    public override Either<L, V> SelectMany<U, V>(Func<R, Either<L, U>> f, Func<R, U, V> projection)
    {
        return f(value).SelectMany(u => Either<L, V>.Right(projection(value, u)));
    }

    public override Either<L, U> Select<U>(Func<R, U> f) => Either<L, U>.Right(f(value));

    public R Value => value;
    public override string ToString() => $"Right({value})";
}

// Extension methods for IEnumerable (List Monad)
public static class ListMonadExtensions
{
    public static IEnumerable<U> SelectMany<T, U>(
        this IEnumerable<T> source,
        Func<T, IEnumerable<U>> selector)
    {
        return source.SelectMany(selector);
    }
}

class Program
{
    static Maybe<double> SafeDiv(double x, double y)
    {
        return y == 0 ? Maybe<double>.None() : Maybe<double>.Pure(x / y);
    }

    static Either<string, double> ParseNumber(string str)
    {
        return double.TryParse(str, out var result) ?
            Either<string, double>.Right(result) :
            Either<string, double>.Left($"Cannot parse '{str}' as number");
    }

    static void Main()
    {
        // Maybe monad with LINQ syntax
        var maybeResult =
            from x in Maybe<double>.Pure(10)
            from y in Maybe<double>.Pure(2)
            from z in SafeDiv(x, y)
            select z * 3;

        Console.WriteLine($"Maybe result: {maybeResult}"); // Some(15)

        // Either monad with LINQ syntax
        var eitherResult =
            from x in ParseNumber("10")
            from y in ParseNumber("2")
            from z in (y == 0 ? Either<string, double>.Left("Division by zero") :
                              Either<string, double>.Right(x / y))
            select z * 3;

        Console.WriteLine($"Either result: {eitherResult}"); // Right(15)

        // List monad (built into LINQ)
        var listResult =
            from x in new[] { 1, 2, 3 }
            from y in new[] { 10, 20 }
            select x + y;

        Console.WriteLine($"List result: [{string.Join(", ", listResult)}]");
        // [11, 21, 12, 22, 13, 23]

        // Nested computation with error handling
        var complexResult =
            from a in ParseNumber("10")
            from b in ParseNumber("2")
            from c in SafeDiv(a, b).IsSome ?
                     Either<string, double>.Right(((Some<double>)SafeDiv(a, b)).Value) :
                     Either<string, double>.Left("Division failed")
            from d in ParseNumber("3")
            select c * d + 1;

        Console.WriteLine($"Complex result: {complexResult}"); // Right(16)
    }
}
