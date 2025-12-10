using System;
using System.Collections.Generic;
using System.Linq;

public interface IFunctor<T>
{
    IFunctor<U> Map<U>(Func<T, U> f);
}

public class Maybe<T> : IFunctor<T>
{
    private readonly T value;
    private readonly bool hasValue;

    private Maybe(T value, bool hasValue)
    {
        this.value = value;
        this.hasValue = hasValue;
    }

    public static Maybe<T> Just(T value) => new Maybe<T>(value, true);
    public static Maybe<T> Nothing() => new Maybe<T>(default(T)!, false);

    public IFunctor<U> Map<U>(Func<T, U> f)
    {
        if (!hasValue) return Maybe<U>.Nothing();
        return Maybe<U>.Just(f(value));
    }

    public override string ToString()
    {
        return hasValue ? $"Just({value})" : "Nothing";
    }
}

class Program
{
    static void Main()
    {
        // Example usage:
        var justHello = Maybe<string>.Just("Hello").Map(x => x + " World"); // Just("Hello World")
        var nothing = Maybe<string>.Nothing().Map(x => x + " World");       // Nothing

        Console.WriteLine(justHello); // Just(Hello World)
        Console.WriteLine(nothing);   // Nothing
    }
}