using System;

// Maybe monad implementation
public interface IMaybe<T>
{
    IMaybe<U> FlatMap<U>(Func<T, IMaybe<U>> f);
    IMaybe<U> Map<U>(Func<T, U> f);
}

// Kleisli composition helper
public static class MaybeExtensions
{
    public static Func<A, IMaybe<C>> KleisliCompose<A, B, C>(
        Func<A, IMaybe<B>> f,
        Func<B, IMaybe<C>> g)
    {
        return a => f(a).FlatMap(g);
    }
}

public class Some<T> : IMaybe<T>
{
    private readonly T value;
    public Some(T value) { this.value = value; }

    public IMaybe<U> FlatMap<U>(Func<T, IMaybe<U>> f)
    {
        return f(value);
    }

    public IMaybe<U> Map<U>(Func<T, U> f)
    {
        return new Some<U>(f(value));
    }

    public override string ToString() => $"Some({value})";
}

public class None<T> : IMaybe<T>
{
    public IMaybe<U> FlatMap<U>(Func<T, IMaybe<U>> f)
    {
        return new None<U>();
    }

    public IMaybe<U> Map<U>(Func<T, U> f)
    {
        return new None<U>();
    }

    public override string ToString() => "None";
}

public static class Program
{
    // Kleisli arrows for safe division
    public static IMaybe<double> SafeDivide(double x, double y)
    {
        return y == 0 ? new None<double>() : new Some<double>(x / y);
    }

    public static IMaybe<double> DivideBy2(double x) => SafeDivide(x, 2);
    public static IMaybe<double> Reciprocal(double x) => SafeDivide(1, x);
    public static IMaybe<double> DivideBy4(double x) => SafeDivide(x, 4);

    // Kleisli composition
    public static IMaybe<double> ComplexOperation(double x)
    {
        return MaybeExtensions.KleisliCompose(
            MaybeExtensions.KleisliCompose<double, double, double>(DivideBy2, Reciprocal),
            DivideBy4
        )(x);
    }

    public static void Main()
    {
        Console.WriteLine(ComplexOperation(2.0));  // Some(0.25)
        Console.WriteLine(ComplexOperation(0.0));  // None
    }
}
