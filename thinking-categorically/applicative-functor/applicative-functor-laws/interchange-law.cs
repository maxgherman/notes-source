using System;
using System.Collections.Generic;
using System.Linq;

class Program
{
    static void Main()
    {
        // List of functions (like [(+1), (*2)])
        var u = new List<Func<int, int>> { x => x + 1, x => x * 2 };

        // A pure value
        int y = 3;

        // Left side: u <*> pure y
        var left = u.Select(f => f(y)).ToList();

        // Right side: pure (f => f y) <*> u
        var right = new List<Func<Func<int, int>, int>> { f => f(y) }
            .SelectMany(g => u.Select(f => g(f)))
            .ToList();

        Console.WriteLine($"Left:  {string.Join(", ", left)}");
        Console.WriteLine($"Right: {string.Join(", ", right)}");
        Console.WriteLine($"Equal? {left.SequenceEqual(right)}");
    }
}
