using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

class Program
{
    static async Task Main()
    {
        Console.WriteLine("=== Applicative Homomorphism Law ===\n");

        // 1. Task<T>
        var result1 = await TaskHomomorphismExample();
        Console.WriteLine($"Task<T> result: {result1}"); // 3

        // 2. List<T>
        var result2 = ListHomomorphismExample();
        Console.WriteLine($"List<T> result: {string.Join(", ", result2)}"); // 6
    }

    // --------------------------------
    // 1. Task<T> example (like Maybe)
    // --------------------------------
    static async Task<int> TaskHomomorphismExample()
    {
        Func<int, int> add1 = x => x + 1;
        var left = await Ap(Task.FromResult(add1), Task.FromResult(2));
        var right = await Task.FromResult(add1(2));

        Console.WriteLine($"Left == Right? {left == right}"); // True
        return left;
    }

    static async Task<TResult> Ap<T, TResult>(
        Task<Func<T, TResult>> tf,
        Task<T> tx)
    {
        var f = await tf;
        var x = await tx;
        return f(x);
    }

    // --------------------------------
    // 2. List<T> example
    // --------------------------------
    static List<int> ListHomomorphismExample()
    {
        Func<int, int> times2 = x => x * 2;

        var left = Ap(new List<Func<int, int>> { times2 }, new List<int> { 3 });
        var right = new List<int> { times2(3) };

        Console.WriteLine($"Left == Right? {left.SequenceEqual(right)}"); // True
        return left;
    }

    static List<TResult> Ap<T, TResult>(
        List<Func<T, TResult>> fs,
        List<T> xs)
    {
        return fs.SelectMany(f => xs.Select(f)).ToList();
    }
}
