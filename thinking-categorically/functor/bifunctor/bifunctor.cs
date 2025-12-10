using System;
using System.Collections.Generic;
using System.Linq;

// Bifunctor interface
public interface IBifunctor<A, B>
{
    IBifunctor<C, D> Bimap<C, D>(Func<A, C> f, Func<B, D> g);
    IBifunctor<C, B> First<C>(Func<A, C> f);
    IBifunctor<A, D> Second<D>(Func<B, D> g);
}

// Tuple bifunctor
public class Tuple<A, B> : IBifunctor<A, B>
{
    public A FirstValue { get; }
    public B SecondValue { get; }

    public Tuple(A first, B second)
    {
        FirstValue = first;
        SecondValue = second;
    }

    public IBifunctor<C, D> Bimap<C, D>(Func<A, C> f, Func<B, D> g)
    {
        return new Tuple<C, D>(f(FirstValue), g(SecondValue));
    }

    public IBifunctor<C, B> First<C>(Func<A, C> f)
    {
        return new Tuple<C, B>(f(FirstValue), SecondValue);
    }

    public IBifunctor<A, D> Second<D>(Func<B, D> g)
    {
        return new Tuple<A, D>(FirstValue, g(SecondValue));
    }

    public override string ToString() => $"({FirstValue}, {SecondValue})";
}

// Either bifunctor (discriminated union)
public abstract class Either<A, B> : IBifunctor<A, B>
{
    public abstract IBifunctor<C, D> Bimap<C, D>(Func<A, C> f, Func<B, D> g);
    public abstract IBifunctor<C, B> First<C>(Func<A, C> f);
    public abstract IBifunctor<A, D> Second<D>(Func<B, D> g);

    public static Either<A, B> Left(A value) => new Left<A, B>(value);
    public static Either<A, B> Right(B value) => new Right<A, B>(value);
}

public class Left<A, B> : Either<A, B>
{
    public A Value { get; }

    public Left(A value) { Value = value; }

    public override IBifunctor<C, D> Bimap<C, D>(Func<A, C> f, Func<B, D> g)
    {
        return new Left<C, D>(f(Value));
    }

    public override IBifunctor<C, B> First<C>(Func<A, C> f)
    {
        return new Left<C, B>(f(Value));
    }

    public override IBifunctor<A, D> Second<D>(Func<B, D> g)
    {
        return new Left<A, D>(Value);
    }

    public override string ToString() => $"Left({Value})";
}

public class Right<A, B> : Either<A, B>
{
    public B Value { get; }

    public Right(B value) { Value = value; }

    public override IBifunctor<C, D> Bimap<C, D>(Func<A, C> f, Func<B, D> g)
    {
        return new Right<C, D>(g(Value));
    }

    public override IBifunctor<C, B> First<C>(Func<A, C> f)
    {
        return new Right<C, B>(Value);
    }

    public override IBifunctor<A, D> Second<D>(Func<B, D> g)
    {
        return new Right<A, D>(g(Value));
    }

    public override string ToString() => $"Right({Value})";
}


class Program
{
    static void Main()
    {
        // Tuple examples
        var pair = new Tuple<int, string>(5, "hello");

        Console.WriteLine($"Original: {pair}");                                           // (5, hello)
        Console.WriteLine($"Bimap: {pair.Bimap(x => x * 2, s => s.Length)}");           // (10, 5)
        Console.WriteLine($"First: {pair.First(x => x * 3)}");                          // (15, hello)
        Console.WriteLine($"Second: {pair.Second(s => s + "!")}");                      // (5, hello!)

        // Either examples
        var leftVal = Either<int, string>.Left(42);
        var rightVal = Either<int, string>.Right("world");

        Console.WriteLine($"Left bimap: {leftVal.Bimap(x => x * 2, s => s.Length)}");   // Left(84)
        Console.WriteLine($"Right bimap: {rightVal.Bimap(x => x * 2, s => s.Length)}"); // Right(5)

    }
}
