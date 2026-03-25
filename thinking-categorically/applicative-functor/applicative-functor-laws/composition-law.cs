using System;
using System.Collections.Generic;
using System.Linq;

class Program
{
    static void Main()
    {
        // u = [(+1)]
        var u = new List<Func<int, int>> { x => x + 1 };

        // v = [(*2)]
        var v = new List<Func<int, int>> { x => x * 2 };

        // w = [10]
        var w = new List<int> { 10 };

        // Function composition: f ∘ g
        Func<Func<int, int>, Func<int, int>, Func<int, int>> compose =
            (f, g) => x => f(g(x));

        // Left: pure (.) <*> u <*> v <*> w
        var left = new List<Func<Func<int, int>, Func<int, int>, Func<int, int>>> { compose }
            .SelectMany(c => u
            .SelectMany(f => v
            .SelectMany(g => w
            .Select(x => c(f, g)(x))))).ToList();

        // Right: u <*> (v <*> w)
        var right = u
            .SelectMany(f => v
            .SelectMany(g => w
            .Select(x => f(g(x))))).ToList();

        Console.WriteLine($"Left:  {string.Join(", ", left)}");
        Console.WriteLine($"Right: {string.Join(", ", right)}");
        Console.WriteLine($"Equal? {left.SequenceEqual(right)}");
    }
}
