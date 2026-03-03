using System;
using System.Collections.Generic;
using System.Linq;

// The List/IEnumerable functor (Select method provides fmap)
// Select :: (A -> B) -> IEnumerable<A> -> IEnumerable<B>

public static class Program
{
    // Natural transformation from IEnumerable to int
    public static int EnumerableCount<T>(IEnumerable<T> source) => source.Count();

    // Verification of naturality:
    // For any function f: A -> B and enumerable source: IEnumerable<A>:
    // EnumerableCount(source.Select(f)) == EnumerableCount(source)

    public static void Main()
    {
        Func<int, string> f = x => (x * 2).ToString();
        var list = new[] { 1, 2, 3 };

        var natural = EnumerableCount(list.Select(f)) == EnumerableCount(list);
        Console.WriteLine($"Naturality check: {natural}"); // True
    }
}