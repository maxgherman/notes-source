using System;

class Program
{
    // Example functions (arrows)
    static int F(int a) => a + 10; // f: A -> B
    static int G(int c) => c * 10; // g: C -> D

    // s: A -> C
    static int S(int a) => a + 1;

    // t: B -> D
    static int T(int b) => (b - 9) * 10;

    static void Main()
    {
        // Pick an element a in A
        int a = 2;

        // Compute both paths in the commutative square:
        // Path 1: t(f(a))
        int path1 = T(F(a));

        // Path 2: g(s(a))
        int path2 = G(S(a));

        Console.WriteLine($"t(f({a})) = {path1}");
        Console.WriteLine($"g(s({a})) = {path2}");
        Console.WriteLine($"Commutative? {path1 == path2}");
    }
}