using System;
using System.Collections.Generic;
using System.Linq;

public static class Program
{
    // Natural transformation from IEnumerable<IEnumerable<T>> to IEnumerable<T>
    public static IEnumerable<T> Flatten<T>(IEnumerable<IEnumerable<T>> nested) =>
        nested.SelectMany(x => x);

    public static void Main()
    {
        Func<int, int> f = x => x * 2;
        var nested = new[] { new[] { 1, 2 }, new[] { 3, 4 } };

        // Verify naturality: fmap f (flatten xss) == flatten (fmap (fmap f) xss)
        var left = Flatten(nested).Select(f);
        var right = Flatten(nested.Select(inner => inner.Select(f)));

        Console.WriteLine($"Flatten naturality: {left.SequenceEqual(right)}");
        Console.WriteLine($"Left: [{string.Join(", ", left)}]");   // [2, 4, 6, 8]
        Console.WriteLine($"Right: [{string.Join(", ", right)}]"); // [2, 4, 6, 8]
    }
}