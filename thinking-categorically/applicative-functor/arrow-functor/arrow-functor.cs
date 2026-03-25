using System;

class Program
{
    static void Main()
    {
        // map: (A → B) → (R → A) → (R → B)
        Func<Func<A, B>, Func<R, A>, Func<R, B>> Map<A, B, R>() =>
            (f, g) => x => f(g(x));

        // Functions
        Func<int, int> add1 = x => x + 1;
        Func<int, string> add2 = x => $"{x} + 2";

        // Compose functions using map
        var result = Map<int, string, int>()(add2, add1);

        Console.WriteLine(result(5)); // prints: "6 + 2"
    }
}
