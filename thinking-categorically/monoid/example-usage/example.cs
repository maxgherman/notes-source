using System;
using System.Collections.Generic;
using System.Linq;

// Monoid interface
public interface IMonoid<T>
{
    T Identity { get; }
    T Combine(T a, T b);
}

// String concatenation monoid
public class StringConcatMonoid : IMonoid<string>
{
    public string Identity => string.Empty;

    public string Combine(string a, string b) => a + b;
}

// Integer addition monoid
public class IntAddMonoid : IMonoid<int>
{
    public int Identity => 0;

    public int Combine(int a, int b) => a + b;
}

// List concatenation monoid
public class ListConcatMonoid<T> : IMonoid<List<T>>
{
    public List<T> Identity => new List<T>();

    public List<T> Combine(List<T> a, List<T> b)
    {
        var result = new List<T>(a);
        result.AddRange(b);
        return result;
    }
}

// Extension methods for monoid operations
public static class MonoidExtensions
{
    public static T Fold<T>(this IEnumerable<T> source, IMonoid<T> monoid)
    {
        return source.Aggregate(monoid.Identity, monoid.Combine);
    }

    public static T FoldMap<TSource, T>(
        this IEnumerable<TSource> source,
        Func<TSource, T> selector,
        IMonoid<T> monoid)
    {
        return source.Select(selector).Fold(monoid);
    }
}

// Usage examples
class Program
{
    static void Main()
    {
        var stringMonoid = new StringConcatMonoid();
        var intMonoid = new IntAddMonoid();
        var listMonoid = new ListConcatMonoid<int>();

        // String concatenation
        var words = new[] { "Hello", " ", "World", "!" };
        var sentence = words.Fold(stringMonoid);
        Console.WriteLine(sentence); // "Hello World!"

        // Number addition
        var numbers = new[] { 1, 2, 3, 4, 5 };
        var sum = numbers.Fold(intMonoid);
        Console.WriteLine(sum); // 15

        // List concatenation
        var lists = new[]
        {
            new List<int> { 1, 2 },
            new List<int> { 3, 4 },
            new List<int> { 5 }
        };
        var combined = lists.Fold(listMonoid);
        Console.WriteLine(string.Join(", ", combined)); // "1, 2, 3, 4, 5"

        // Safe empty collection handling
        var emptySum = new int[0].Fold(intMonoid);
        Console.WriteLine(emptySum); // 0 (identity element)

        // Custom monoid for order totals
        var orders = new[] { 10.50m, 25.00m, 7.25m, 15.75m };
        var total = orders.FoldMap(x => x, new DecimalAddMonoid());
        Console.WriteLine($"Total: ${total}"); // "Total: $58.50"
    }
}

// Custom decimal addition monoid
public class DecimalAddMonoid : IMonoid<decimal>
{
    public decimal Identity => 0m;

    public decimal Combine(decimal a, decimal b) => a + b;
}
