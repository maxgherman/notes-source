using System;
using System.Collections.Generic;
using System.Linq;

// Option/Maybe type
public abstract class Option<T>
{
    public abstract Option<U> Map<U>(Func<T, U> f);
}

public class Some<T> : Option<T>
{
    public T Value { get; }
    public Some(T value) => Value = value;
    public override Option<U> Map<U>(Func<T, U> f) => new Some<U>(f(Value));
}

public class None<T> : Option<T>
{
    public override Option<U> Map<U>(Func<T, U> f) => new None<U>();
}

public static class Program
{
    // Natural transformation from IEnumerable to Option
    public static Option<T> SafeHead<T>(this IEnumerable<T> source)
    {
        using var enumerator = source.GetEnumerator();
        return enumerator.MoveNext() ? new Some<T>(enumerator.Current) : new None<T>();
    }

    public static void Main()
    {
        Func<int, string> f = x => x.ToString();
        var list = new[] { 1, 2, 3 };

        // Verify naturality: fmap f (safeHead xs) == safeHead (fmap f xs)
        var left = list.SafeHead().Map(f);
        var right = list.Select(f).SafeHead();

        Console.WriteLine($"Left: {(left is Some<string> s1 ? s1.Value : "None")}");   // "1"
        Console.WriteLine($"Right: {(right is Some<string> s2 ? s2.Value : "None")}"); // "1"
    }
}