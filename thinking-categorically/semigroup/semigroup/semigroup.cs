using System;
using System.Collections.Generic;
using System.Linq;

// Semigroup interface in C#
public interface ISemigroup<T>
{
    T Combine(T a, T b);
}

// String concatenation semigroup
public class StringSemigroup : ISemigroup<string>
{
    public string Combine(string a, string b) => a + b;
}

// List concatenation semigroup
public class ListSemigroup<T> : ISemigroup<List<T>>
{
    public List<T> Combine(List<T> a, List<T> b) => a.Concat(b).ToList();
}

// Number addition semigroup
public class SumSemigroup : ISemigroup<int>
{
    public int Combine(int a, int b) => a + b;
}

// Number multiplication semigroup
public class ProductSemigroup : ISemigroup<int>
{
    public int Combine(int a, int b) => a * b;
}

// Generic combine function (associative)
public static class SemigroupExtensions
{
    public static T CombineAll<T>(this ISemigroup<T> semigroup, IEnumerable<T> items)
    {
        var list = items.ToList();
        if (!list.Any())
        {
            throw new ArgumentException("Cannot combine empty collection");
        }

        return list.Aggregate(semigroup.Combine);
    }
}

// Examples in action:
class Program
{
    static void Main()
    {
        var stringSemigroup = new StringSemigroup();
        var listSemigroup = new ListSemigroup<int>();
        var sumSemigroup = new SumSemigroup();
        var productSemigroup = new ProductSemigroup();

        // String concatenation
        Console.WriteLine(stringSemigroup.Combine("Hello", " World")); // "Hello World"
        Console.WriteLine(stringSemigroup.CombineAll(new[] { "Hello", " ", "World" })); // "Hello World"

        // List concatenation
        var list1 = new List<int> { 1, 2 };
        var list2 = new List<int> { 3, 4 };
        var list3 = new List<int> { 5, 6 };
        Console.WriteLine(string.Join(",", listSemigroup.CombineAll(new[] { list1, list2, list3 }))); // "1,2,3,4,5,6"

        // Sum semigroup
        Console.WriteLine(sumSemigroup.CombineAll(new[] { 1, 2, 3, 4 })); // 10

        // Product semigroup
        Console.WriteLine(productSemigroup.CombineAll(new[] { 2, 3, 4 })); // 24
    }
}
