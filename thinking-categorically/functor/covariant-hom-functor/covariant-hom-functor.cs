using System;
using System.Collections.Generic;
using System.Linq;

class Program
{
    static void Main()
    {
        // Functions in Hom(int, string)
        Func<int, string> toString = x => x.ToString();
        Func<int, string> describe = x => $"The number {x}";

        // Functions in Hom(int, bool)
        Func<int, bool> isEven = x => x % 2 == 0;
        Func<int, bool> isPositive = x => x > 0;

        // Functions in Hom(int, int)
        Func<int, int> double_ = x => x * 2;
        Func<int, int> square = x => x * x;

        // The functor action: fmap for Func<A, _>
        // fmap :: (B -> C) -> (A -> B) -> (A -> C)
        Func<Func<B, C>, Func<Func<A, B>, Func<A, C>>> FMap<A, B, C>() =>
            f => g => x => f(g(x));

        var fmap = FMap<int, int, string>();

        // Transform int -> int functions into int -> string functions
        var doubleToString = fmap(toString)(double_);
        var squareToString = fmap(toString)(square);

        Console.WriteLine(doubleToString(5)); // "10"
        Console.WriteLine(squareToString(5)); // "25"

        // Working with collections of functions (representing Hom-sets)
        var intToIntFunctions = new List<Func<int, int>> { double_, square, x => x + 1 };

        // Map all functions to int -> string
        var intToStringFunctions = intToIntFunctions
            .Select(f => fmap(toString)(f))
            .ToList();

        Console.WriteLine("Transformed functions applied to 3:");
        for (int i = 0; i < intToStringFunctions.Count; i++)
        {
            Console.WriteLine($"Function {i}(3) = \"{intToStringFunctions[i](3)}\"");
            // Function 0(3) = "6"   (double)
            // Function 1(3) = "9"   (square)
            // Function 2(3) = "4"   (add one)
        }
    }
}